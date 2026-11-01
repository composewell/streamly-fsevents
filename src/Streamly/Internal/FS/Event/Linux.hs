-- |
-- Module      : Streamly.Internal.FS.Event.Linux
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- =Overview
--
-- Use 'watch' or 'watchRecursive' with a list of file system paths you want
-- to watch as argument. It returns a stream of 'Event' representing the file system events
-- occurring under the watched paths.
--
-- @
-- Stream.mapM_ (putStrLn . 'showEvent') $ 'watch' [Path.fromString "dir"]
-- @
--
-- 'Event' is an opaque type. Accessor functions (e.g. 'showEvent' above)
-- provided in this module are used to determine the attributes of the event.
--
-- Identical successive events may be coalesced into a single event.
--
-- =Design notes
--
-- For reference documentation see:
--
-- * <https://man7.org/linux/man-pages/man7/inotify.7.html inotify man page>
--
-- We try to keep the macOS\/Linux/Windows event handling APIs and defaults
-- semantically and syntactically as close as possible.
--
-- =BUGs
--
-- When testing on Linux Kernel version @5.3.0-53-generic #47-Ubuntu@, the last
-- event for the root path seems to be delayed until one more event occurs.
--
-- = Differences between macOS and Linux APIs:
--
-- 1. macOS watch is based on the path provided to it, if the path is
-- deleted and recreated it will still be watched, if the path moves to another
-- path it won't be watched anymore. Whereas Linux watch is based on a handle
-- to the path, if the path is deleted and recreated it won't be watched, if
-- the path moves to another it can still be watched (though this is
-- configurable).
--
-- 2. macOS watches the directory hierarchy recursively, Linux watches only one
-- level of dir, recursive watch has to be built in user space by watching for
-- create events and adding the new directories to the watch. Not sure how this
-- will scale for too many paths.
--
-- 3. In macOS the path of the subject of the event is absolute, in Linux the
-- path is the name of the object inside the dir being watched.
--
-- 4. On Linux 'watch' fails if a path does not exist, on macOS it does
-- not fail.
--
-- 5. On Linux the reads and writes of an open file after it is unlinked
-- generate events unless 'setExcludeUnlinked' is 'True', on macOS they do
-- not generate events.
--
-- 6. On Linux metadata change events are reported only if
-- 'setAttrsModified' is 'True', on macOS they are always reported. macOS
-- has separate flags for different kinds of metadata change.
--
-- 7. On Linux a hard link generates 'isCreated', on macOS it does not.

#include "config.h"

module Streamly.Internal.FS.Event.Linux
    (
    -- * Subscribing to events

    -- ** Default configuration
      Config (..)
    , defaultConfig

    -- ** Watch Behavior
    , setRecursiveMode
    , setScanNewDirs
    , setRestartOnOverflow
    , setFollowSymLinks
    , setExcludeUnlinked
    , setUnwatchMoved
    , setOneShot
    , setOnlyDir
    , WhenExists (..)
    , setWhenExists

    -- ** Events of Interest
    -- *** Root Path Events
    , setRootDeleted
    , setRootMoved
    , setRootPathEvents

    -- *** Item Level Metadata change
    , setAttrsModified

    -- *** Item Level Access
    , setAccessed
    , setOpened
    , setWriteClosed
    , setNonWriteClosed

    -- *** Item CRUD events
    , setCreated
    , setDeleted
    , setMovedFrom
    , setMovedTo
    , setModified

    , setAllEvents

    -- ** Watch APIs
    , watch
    , watchRecursive
    , watchWith

    -- Low level watch APIs
    , Watch
    , openWatch
    , closeWatch
    , watchToStream
    , addToWatch
    , removeFromWatch

    -- * Handling Events
    , Event(..)
    , getRoot
    , getRelPath
    , getAbsPath
    , Cookie
    , getCookie

    -- ** Root Level Events
    , isRootPathEvent
    , isRootUnwatched
    , isRootDeleted
    , isRootMoved
    , isRootUnmounted

    -- ** Item Level Metadata change
    , isAttrsModified

    -- ** Item Level Access
    , isAccessed
    , isOpened
    , isWriteClosed
    , isNonWriteClosed

    -- ** Item Level CRUD events
    , isCreated
    , isDeleted
    , isMovedFrom
    , isMovedTo
    , isMoved
    , isModified

    -- ** Item Path info
    , isDir
    , isScanEvent

    -- ** Exception Conditions
    , isEventsLost

    -- * Debugging
    , showEvent
    )
where

import Control.Concurrent (rtsSupportsBoundThreads, threadWaitRead)
import Control.Concurrent.MVar
    (MVar, modifyMVar, modifyMVar_, newMVar, readMVar, withMVar)
import Control.Exception (finally, onException, try)
import Control.Monad (filterM, unless, void, when)
import Data.Bits ((.|.), (.&.), complement)
import Data.Char (ord)
import Data.IntMap.Lazy (IntMap)
import Data.IORef
    (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (isJust, isNothing)
#if !(MIN_VERSION_base(4,13,0))
import Data.Semigroup (Semigroup(..))
#endif
import Data.Set (Set)
import Data.Word (Word8, Word32)
import Foreign.C.Error
    ( Errno(..), eAGAIN, eINTR, eINVAL, eNOTDIR, eWOULDBLOCK, getErrno
    , throwErrno, throwErrnoIfMinus1, throwErrnoIfMinus1_
    )
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt(..), CSize(..), CUInt(..))
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek, peekByteOff, peekElemOff, sizeOf)
import GHC.Conc (atomically, closeFdWith, threadWaitReadSTM)
import GHC.IO.Exception (IOException(ioe_errno))
import Streamly.Data.Array (Array)
import Streamly.Data.Stream (Stream)
import Streamly.Data.Parser (Parser)
import System.IO.Error
    ( catchIOError, illegalOperationErrorType, isDoesNotExistError
    , mkIOError
    )
import System.Posix.Types (CSsize(..), Fd(..))

import Streamly.Internal.Data.Array (unsafeFreezeWithShrink)
import Streamly.Internal.FileSystem.Path (Path)

import qualified Control.Exception as Exception (mask, uninterruptibleMask_)
import qualified Data.IntMap.Lazy as Map
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Data.Array as A (createOf, reader, toList)
import qualified Streamly.Data.Stream as S
import qualified Streamly.Data.Unfold as UF
import qualified Streamly.Internal.FileSystem.Path as Path

import qualified Streamly.Internal.Data.Array as A
    ( asCStringUnsafe, breakEndBy_, unsafePinnedAsPtr
    )
import qualified Streamly.Internal.Data.MutArray as MArray
    (byteLength, unsafeCreateWithPtr')
import qualified Streamly.Internal.FileSystem.DirIO as Dir
    (ignoreMissing, readDirs, readEither)
import qualified Streamly.Internal.Data.Parser as PR
    (takeEQ, fromEffect)

-------------------------------------------------------------------------------
-- Subscription to events
-------------------------------------------------------------------------------

-- | Watch configuration, used to specify the events of interest and the
-- behavior of the watch.
data Config = Config
    { watchRec :: Bool
    , scanNewDirs :: Bool
    , restartOnOverflow :: Bool
    , createFlags :: Word32
    }

-------------------------------------------------------------------------------
-- Boolean settings
-------------------------------------------------------------------------------

setFlag :: Word32 -> Bool -> Config -> Config
setFlag mask status cfg@Config{..} =
    let flags =
            if status
            then createFlags .|. mask
            else createFlags .&. complement mask
    in cfg {createFlags = flags}

-------------------------------------------------------------------------------
-- Settings
-------------------------------------------------------------------------------

-- | Watch the whole directory tree recursively instead of watching just one
-- level of directory.
--
-- A directory created in or moved into the tree is added to the watch. A
-- directory moved out of the tree is removed from the watch, its paths
-- report 'isRootUnwatched'. A directory renamed within the tree is removed
-- and added again with the new path. A symbolic link to a directory in the
-- tree is not followed.
--
-- A new directory is added when its create or move event is read from the
-- stream, not when it is created. If the path has been reused by then, the
-- directory found at the path is watched. If an already watched directory is
-- moved to that path, its events may be reported under both the old and the
-- new path until the move event is read.
--
-- A directory created in the tree is scanned after its watch is added, see
-- 'setScanNewDirs'. A directory moved into the tree or renamed within it is
-- not reported that way, its subdirectories are added without generating
-- events and the events in it before it is added are not reported. A rename
-- within the tree is not a creation of its contents, and a directory moved
-- in has one move event on all platforms. To find the entries in such a
-- directory, scan it when its 'isMovedTo' event is received.
--
-- The scan that adds a tree does not report the open, access and close events
-- that it generates. These events generated by another process during the scan
-- are not reported either.
--
-- A directory whose create or move event is dropped by an event queue
-- overflow is added by 'setRestartOnOverflow', unless it is disabled.
--
-- Each directory in the tree uses one inotify watch. If the number of watches
-- exceeds the limit in @\/proc\/sys\/fs\/inotify\/max_user_watches@, the watch
-- API or the event stream raises an exception.
--
-- In recursive mode the kernel watch always requests the create, moved-from
-- and moved-to events, because they are needed to add and remove the
-- watches of subdirectories. These events are reported only if they are
-- enabled with 'setCreated', 'setMovedFrom' and 'setMovedTo'.
--
-- /default: False/
setRecursiveMode :: Bool -> Config -> Config
setRecursiveMode recursive cfg@Config{} = cfg {watchRec = recursive}

-- | In recursive mode, report the entries found in a directory created in the
-- tree. Applies only if 'setCreated' is enabled.
--
-- A directory is not watched until after its create event is read. Without
-- this setting the entries created in it by then are not reported. With this
-- setting the directory is scanned after its watch is added, the scan goes
-- into its subdirectories, and an event for which 'isCreated' and
-- 'isScanEvent' are true is reported for each entry found. These events follow
-- the create event of the directory. For example,
-- @mkdir -p a\/b; touch a\/b\/f@ reports @a@, @a\/b@ and @a\/b\/f@.
--
-- The scan reports the state of the directory, not its history. Every entry
-- present when the scan reads it is reported. An entry created and deleted
-- before the scan is not reported, a modification before the scan is not
-- reported separately.
--
-- An entry created after the watch is added and before the scan reads it is
-- reported twice, by the scan and by the kernel. See 'isScanEvent' for
-- removing the duplicates.
--
-- The scan runs while the stream is being read, a scan of a large tree delays
-- reading the kernel event queue. All the entries found are collected in
-- memory before they are reported.
--
-- A directory added by the scan at the start of the watch, or by
-- 'setRestartOnOverflow' after an 'isEventsLost' event, does not report its
-- entries. 'isEventsLost' requires the application to scan the tree anyway.
--
-- /default: True/
setScanNewDirs :: Bool -> Config -> Config
setScanNewDirs scan cfg@Config{} = cfg {scanNewDirs = scan}

-- | In recursive mode, add the whole tree of each watch root again after an
-- event queue overflow, the same way as the start of a watch. This is an
-- automatic restart of the watch, without closing it.
--
-- The kernel drops the events that do not fit in the event queue, whose size
-- is @\/proc\/sys\/fs\/inotify\/max_queued_events@, and reports 'isEventsLost'.
-- A directory whose create or move event is dropped is not added to the watch,
-- and a directory renamed in that period keeps its old path. When this option
-- is 'False', the watch stays that way. An application that needs a complete
-- watch must close the watch and open a new one after 'isEventsLost'.
--
-- When this option is 'True', after an overflow:
--
-- * A directory that is not watched is added and reports events from then on.
-- * A renamed directory reports its new path.
-- * A path that is no longer in the tree is removed and reports
-- 'isRootUnwatched'.
--
-- 'isEventsLost' is reported in both cases. The entries found are not reported,
-- the application has to scan the tree to know its current state.
--
-- The cost and the limits of this option:
--
-- * Each overflow lists all the directories of the tree, on the thread reading
-- the stream. The time taken is proportional to the number of directories. The
-- kernel event queue is not read during this time.
-- * Activity by other processes that generates more than @max_queued_events@
-- events during the restart causes another overflow and another restart, until
-- the activity slows down. The restart itself generates no events.
-- * The open, access and close events are not reported during the restart, see
-- 'setRecursiveMode'.
-- * A watch root that was renamed, deleted or replaced is not restarted, its
-- path no longer refers to the watched directory.
--
-- /default: True/
setRestartOnOverflow :: Bool -> Config -> Config
setRestartOnOverflow restart cfg@Config{} = cfg {restartOnOverflow = restart}

foreign import capi
    "sys/inotify.h value IN_DONT_FOLLOW" iN_DONT_FOLLOW :: Word32

-- | If the pathname to be watched is a symbolic link then watch the target of
-- the symbolic link instead of the symbolic link itself.
--
-- Note that the path location in the events is through the original symbolic
-- link path rather than the resolved path.
--
-- /default: True/
setFollowSymLinks :: Bool -> Config -> Config
setFollowSymLinks s = setFlag iN_DONT_FOLLOW (not s)

#if HAVE_DECL_IN_EXCL_UNLINK
foreign import capi
    "sys/inotify.h value IN_EXCL_UNLINK" iN_EXCL_UNLINK :: Word32
#else
#warning "IN_EXCL_UNLINK is not defined, setExcludeUnlinked has no effect."
-- Linux kernels older than 2.6.36 do not have this flag.
iN_EXCL_UNLINK :: Word32
iN_EXCL_UNLINK = 0
#endif

-- | Do not report the events of a file after it is unlinked from the watched
-- directory. A process can keep a file open after the file is deleted and
-- continue to read and write it. When this setting is 'False', those reads
-- and writes are reported as events with the deleted path. When it is
-- 'True', they are not reported.
--
-- This setting has no effect if the kernel headers do not define
-- @IN_EXCL_UNLINK@, that is, on Linux kernels older than 2.6.36.
--
-- /default: True/
setExcludeUnlinked :: Bool -> Config -> Config
setExcludeUnlinked = setFlag iN_EXCL_UNLINK

-- | Same as 'setExcludeUnlinked'.
--
-- /Pre-release/
--
{-# DEPRECATED setUnwatchMoved "Use setExcludeUnlinked" #-}
setUnwatchMoved :: Bool -> Config -> Config
setUnwatchMoved = setExcludeUnlinked

foreign import capi
    "sys/inotify.h value IN_MASK_ADD" iN_MASK_ADD :: Word32

-- | What to do when a path being added to the watch is already watched, for
-- example, when the same directory is reached via two roots. 'setWhenExists'
-- has no effect, a path that is already watched is always added with
-- 'AddIfExists'.
{-# DEPRECATED WhenExists "The setting has no effect" #-}
data WhenExists =
      AddIfExists -- ^ Add the new events to the events already watched on
                  -- the path.
    | ReplaceIfExists -- ^ Watch only the new events, events already watched
                      -- on the path but not in the new set are no longer
                      -- watched.
    | FailIfExists -- ^ Raise an exception.

-- | Has no effect. A path that is already watched is always added with
-- 'AddIfExists'.
{-# DEPRECATED setWhenExists "The setting has no effect" #-}
setWhenExists :: WhenExists -> Config -> Config
setWhenExists _ = id

foreign import capi
    "sys/inotify.h value IN_ONESHOT" iN_ONESHOT :: Word32

-- | Watch the object only for one event and then remove it from the watch.
--
-- /default: False/
setOneShot :: Bool -> Config -> Config
setOneShot = setFlag iN_ONESHOT

foreign import capi
    "sys/inotify.h value IN_ONLYDIR" iN_ONLYDIR :: Word32

-- | Watch the object only if it is a directory. This provides a race-free way
-- to ensure that the watched object is a directory.
--
-- /default: False/
setOnlyDir :: Bool -> Config -> Config
setOnlyDir = setFlag iN_ONLYDIR

-------------------------------------------------------------------------------
-- Event types that can occur
-------------------------------------------------------------------------------

foreign import capi
    "sys/inotify.h value IN_DELETE_SELF" iN_DELETE_SELF :: Word32

-- | Report when the watched path itself gets deleted.
--
-- In recursive mode, the event is reported for the watch roots only, the
-- deletion of a subdirectory is reported as 'isDeleted'.
--
-- /default: False/
setRootDeleted :: Bool -> Config -> Config
setRootDeleted = setFlag iN_DELETE_SELF

foreign import capi
    "sys/inotify.h value IN_MOVE_SELF" iN_MOVE_SELF :: Word32

-- | Report when the watched root path itself gets renamed.
--
-- In recursive mode, the event is reported for the watch roots only, the
-- rename of a subdirectory is reported as 'isMovedFrom' and 'isMovedTo'.
--
-- /default: False/
setRootMoved :: Bool -> Config -> Config
setRootMoved = setFlag iN_MOVE_SELF

-- | Report when the watched root path itself gets deleted or renamed.
--
-- In recursive mode, the events are reported for the watch roots only.
--
-- /default: False/
setRootPathEvents :: Bool -> Config -> Config
setRootPathEvents = setFlag (iN_DELETE_SELF .|. iN_MOVE_SELF)

foreign import capi
    "sys/inotify.h value IN_ATTRIB" iN_ATTRIB :: Word32

-- | Report when the metadata e.g. owner, permission modes, modifications times
-- of an object changes.
--
-- When this setting is 'True', @touch@ on an existing file generates
-- 'isAttrsModified' but not 'isModified'.
--
-- /default: True/
setAttrsModified :: Bool -> Config -> Config
setAttrsModified = setFlag iN_ATTRIB

foreign import capi
    "sys/inotify.h value IN_ACCESS" iN_ACCESS :: Word32

-- | Report when a file is accessed.
--
-- /default: False/
setAccessed :: Bool -> Config -> Config
setAccessed = setFlag iN_ACCESS

foreign import capi
    "sys/inotify.h value IN_OPEN" iN_OPEN :: Word32

-- | Report when a file is opened.
--
-- /default: False/
setOpened :: Bool -> Config -> Config
setOpened = setFlag iN_OPEN

foreign import capi
    "sys/inotify.h value IN_CLOSE_WRITE" iN_CLOSE_WRITE :: Word32

-- | Report when a file that was opened for writes is closed.
--
-- /default: False/
setWriteClosed :: Bool -> Config -> Config
setWriteClosed = setFlag iN_CLOSE_WRITE

foreign import capi
    "sys/inotify.h value IN_CLOSE_NOWRITE" iN_CLOSE_NOWRITE :: Word32

-- | Report when a file that was opened for not writing is closed.
--
-- /default: False/
setNonWriteClosed :: Bool -> Config -> Config
setNonWriteClosed = setFlag iN_CLOSE_NOWRITE

foreign import capi
    "sys/inotify.h value IN_CREATE" iN_CREATE :: Word32

-- | Report when a file is created.
--
-- /default: True/
setCreated :: Bool -> Config -> Config
setCreated = setFlag iN_CREATE

foreign import capi
    "sys/inotify.h value IN_DELETE" iN_DELETE :: Word32

-- | Report when a file is deleted.
--
-- /default: True/
setDeleted :: Bool -> Config -> Config
setDeleted = setFlag iN_DELETE

foreign import capi
    "sys/inotify.h value IN_MOVED_FROM" iN_MOVED_FROM :: Word32

-- | Report the source of a move.
--
-- /default: True/
setMovedFrom :: Bool -> Config -> Config
setMovedFrom = setFlag iN_MOVED_FROM

foreign import capi
    "sys/inotify.h value IN_MOVED_TO" iN_MOVED_TO :: Word32

-- | Report the target of a move.
--
-- /default: True/
setMovedTo :: Bool -> Config -> Config
setMovedTo = setFlag iN_MOVED_TO

foreign import capi
    "sys/inotify.h value IN_MODIFY" iN_MODIFY :: Word32

-- | Report when a file is modified.
--
-- /default: True/
setModified :: Bool -> Config -> Config
setModified = setFlag iN_MODIFY

-- | Set all tunable events 'True' or 'False'. Equivalent to setting:
--
-- * setRootDeleted
-- * setRootMoved
-- * setAttrsModified
-- * setAccessed
-- * setOpened
-- * setWriteClosed
-- * setNonWriteClosed
-- * setCreated
-- * setDeleted
-- * setMovedFrom
-- * setMovedTo
-- * setModified
setAllEvents :: Bool -> Config -> Config
setAllEvents s =
      setRootDeleted s
    . setRootMoved s
    . setAttrsModified s
    . setAccessed s
    . setOpened s
    . setWriteClosed s
    . setNonWriteClosed s
    . setCreated s
    . setDeleted s
    . setMovedFrom s
    . setMovedTo s
    . setModified s

-------------------------------------------------------------------------------
-- Default config
-------------------------------------------------------------------------------

-- The defaults are set in such a way that the behavior on macOS and Linux is
-- as much compatible as possible.
--
-- | The default configuration settings are:
--
-- * 'setScanNewDirs' 'True'
-- * 'setRestartOnOverflow' 'True'
-- * 'setFollowSymLinks' 'True'
-- * 'setExcludeUnlinked' 'True'
-- * 'setOneShot' 'False'
-- * 'setOnlyDir' 'False'
--
-- The tunable events enabled by default are:
--
-- * setCreated True
-- * setDeleted True
-- * setMovedFrom True
-- * setMovedTo True
-- * setModified True
-- * setAttrsModified True
--
-- /Pre-release/
--
defaultConfig :: Config
defaultConfig =
      setExcludeUnlinked True
    $ setCreated True
    $ setDeleted True
    $ setMovedFrom True
    $ setMovedTo True
    $ setModified True
    $ setAttrsModified True
    $ Config
        { watchRec = False
        , scanNewDirs = True
        , restartOnOverflow = True
        , createFlags = 0
        }

-- | The events that the kernel watch must generate in recursive mode, to add
-- and remove the watches of the subdirectories.
recursiveEvents :: Word32
recursiveEvents = iN_CREATE .|. iN_MOVED_FROM .|. iN_MOVED_TO

-- | The flags of the kernel watch.
--
-- Every directory is added with IN_MASK_ADD. Each call to the 'watch' API is a
-- different inotify instance, and it keeps one watch per directory in that
-- instance. A directory can have an existing watch only if it is added to the
-- same 'Watch' more than once, following are the only ways that can happen:
--
-- * a watch root is listed twice, or two watch roots are paths to the same
--   directory, for example, via a symbolic link;
-- * in recursive mode, a root is under another root, the scan of the outer
--   root adds it too;
-- * in recursive mode, a subdirectory created while its parent is scanned is
--   added by the scan and again on its create event.
--
-- For the public 'watch' API it does not make a difference even if we do not
-- specify IN_MASK_ADD, in that case the watch will be replaced and the
-- replaced watch is the same as the original because same Config is used for
-- all watches. However, if we happen to use the internal API with different
-- event masks that would result in the mask getting changed. So we use
-- IN_MASK_ADD for that case to add both the event masks.
--
watchFlags :: Config -> Word32
watchFlags Config{..} =
    if watchRec
    then flags .|. recursiveEvents
    else flags

    where

    flags = createFlags .|. iN_MASK_ADD

-- | The flags of the kernel watch while a directory and the tree under it are
-- being listed in recursive mode.
--
-- Listing a directory generates open, access and close events on its watch
-- and on the watch of its parent, 8 events for an empty directory. The stream
-- is not read during the scan, on a large tree these events overflow the
-- queue. These events are requested after the directory and the tree under it
-- have been listed, see 'addToWatch'. The same events generated by another
-- process in that period are not reported, all other events are.
--
-- IN_ONESHOT is requested at the same time, an event of the scan must not
-- remove the watch.
scanFlags :: Config -> Word32
scanFlags cfg@Config{..} =
    if watchRec
    then watchFlags cfg .&. complement scanEvents
    else watchFlags cfg

    where

    scanEvents = iN_OPEN .|. iN_ACCESS .|. iN_CLOSE_NOWRITE .|. iN_ONESHOT

-------------------------------------------------------------------------------
-- Path representation for events
-------------------------------------------------------------------------------

type PathRep =
    ( Path       -- Absolute path of the watch root
    , Maybe Path -- Path of subdir relative to watch root
    )

{-# INLINE combinePathRep #-}
combinePathRep :: PathRep -> Path
combinePathRep (root, Nothing) = root
combinePathRep (root, Just sub) = Path.join root sub

{-# INLINE addSub #-}
addSub :: PathRep -> Path -> PathRep
addSub (root, Nothing) sub = (root, Just sub)
addSub (root, Just sub0) sub1 = (root, Just (Path.join sub0 sub1))

{-# INLINE getRepRoot #-}
getRepRoot :: PathRep -> Path
getRepRoot = fst

{-# INLINE getRepSub #-}
getRepSub :: PathRep -> Maybe Path
getRepSub = snd

-- XXX Path is currently not guaranteed to be normalized.
-- See: https://github.com/composewell/streamly/issues/2888
eqPath :: Path -> Path -> Bool
eqPath p1 p2 = Path.toArray p1 == Path.toArray p2

eqPathRep :: PathRep -> PathRep -> Bool
eqPathRep (r1, s1) (r2, s2) =
    eqPath r1 r2 && fmap Path.toArray s1 == fmap Path.toArray s2

-- | @isInSubtree dir prep@ is 'True' if @prep@ is the subdirectory @dir@ of a
-- root or a path under it, via the same root.
isInSubtree :: PathRep -> PathRep -> Bool
isInSubtree (r1, Just s1) (r2, Just s2) =
    eqPath r1 r2 && (b2 == b1 || (b1 ++ [sep]) `isPrefixOf` b2)

    where

    sep = fromIntegral (ord '/')
    b1 = A.toList (Path.toArray s1)
    b2 = A.toList (Path.toArray s2)
isInSubtree _ _ = False

-------------------------------------------------------------------------------
-- Open an event stream
-------------------------------------------------------------------------------

-- | A path added to the watch. The kernel identifies a watch by inode, all
-- the paths added for an inode share the same watch descriptor. An inode can
-- be added via more than one path, for example:
--
-- * a recursive root and a root inside it
-- * two overlapping recursive roots
-- * a symlink followed to a watched directory
-- * a bind mount of a watched directory
-- * two hard links to the same file, added as roots
-- * a root added again before its removal is complete
data WatchPath = WatchPath
    { wpRep :: PathRep
    -- | 'removeFromWatch' was called for the root, the path reports the
    -- events until its 'isRootUnwatched' event.
    , wpRemoved :: Bool
    }

-- | The map entry of a watch descriptor. A user watch descriptor and a
-- marker watch descriptor are different watch descriptors, see
-- 'addRemovalMarker' for markers.
data WdEntry =
    -- | Entry for a user watch descriptor: the paths watched via it, in the
    -- order added.
      WdPaths [WatchPath]
    -- | Entry for a marker watch descriptor: the watch descriptors of older
    -- markers, and the removed paths, as (user watch descriptor, path), to
    -- be reported 'isRootUnwatched' when its IN_IGNORED event is read.
    | WdMarker [Int] [(Int, PathRep)]

-- NOTE: Waiting for events and closing the watch:
--
-- * User level: GHC Handle takes a lock during handle operations e.g. reads or
--   a read and a close in different threads. The lock protects the shared
--   Handle state changed by these operations e.g. read buffer. If a read is
--   blocked indefinitely other handle operations will block as well until a
--   read occurs. Specifically for inotify fd, a read can block indefinitely,
--   so other operations like add/remove/close watch cannot proceed from other
--   threads. Solution: We do not use a Handle. We hold the lock of the watch
--   only during non-blocking system calls, never while waiting.
-- * Kernel level: close(2) system call does not wake up a thread blocked in
--   read(2) on the fd. An inotify fd has no other way to wake up the reader,
--   unlike shutdown(2) on a socket or closing the write end of a pipe.
--   Solution: we keep the fd non-blocking, and 'readEvents' waits for it to
--   become readable via the RTS.
-- * Threaded RTS: 'closeFdWith' closes the fd and wakes up the threads
--   waiting for it in the IO manager.
-- * Non-threaded RTS: the threads wait in select, select fails with EBADF
--   if a waited fd is closed, and the RTS terminates the program.
--   Solution: 'closeWatch' does not close the fd while a thread waits, it
--   forces an IN_IGNORED event to wake up the waiting threads, the last one
--   closes the fd.

-- | The state of the fd of a watch.
data FdState = FdState
    { fsWatchClosed :: !Bool -- ^ 'closeWatch' was called
    , fsFdOpen :: !Bool -- ^ The fd is not closed yet
    -- | The threads waiting for the fd to become readable, counted in the
    -- non-threaded RTS only.
    , fsFdWaiters :: !Int
    }

-- | A handle for a watch.
--
data Watch =
    Watch
        CInt -- The inotify fd
        -- Lock: held during a system call on the fd, 'closeWatch' cannot close
        -- the fd, and the fd number cannot be reused, while the system call is
        -- in progress. The system calls made with the lock held must not
        -- block.
        (MVar FdState)
        (IORef (IntMap WdEntry)) -- Map key is the watch descriptor

-- Instead of using the watch descriptor we can provide APIs that use the path
-- itself to identify the watch. That will require us to maintain a map from wd
-- to path in the Watch handle.

newtype WD = WD CInt deriving Show

foreign import ccall unsafe
    "sys/inotify.h inotify_init1" c_inotify_init1 :: CInt -> IO CInt

foreign import capi
    "sys/inotify.h value IN_CLOEXEC" iN_CLOEXEC :: CInt

foreign import capi
    "sys/inotify.h value IN_NONBLOCK" iN_NONBLOCK :: CInt

-- | Create a 'Watch' handle. 'addToWatch' can be used to add paths being
-- monitored by this watch.
--
-- /Pre-release/
--
createWatch :: IO Watch
createWatch = do
    -- The fd must not be inherited by child processes. An inherited fd
    -- keeps the inotify instance and its watches alive after 'closeWatch',
    -- and counts against the max_user_instances limit.
    --
    -- The fd is non-blocking, 'readEvents' waits for it to become readable
    -- via the GHC IO manager.
    fd <-
        throwErrnoIfMinus1 "createWatch"
            $ c_inotify_init1 (iN_CLOEXEC .|. iN_NONBLOCK)
    lock <-
        newMVar
            FdState {fsWatchClosed = False, fsFdOpen = True, fsFdWaiters = 0}
    emptyMapRef <- newIORef Map.empty
    return $ Watch fd lock emptyMapRef

-- | Run an action on the fd of a watch, holding the lock of the watch. The
-- action must not block. Throw an exception if the watch is closed. The
-- string is the name of the operation, used in the exception message.
withWatchFd :: String -> Watch -> (CInt -> IO a) -> IO a
withWatchFd op (Watch fd lock _) action =
    withMVar lock $ \st ->
        if fsWatchClosed st
        then ioError
                $ mkIOError
                    illegalOperationErrorType
                    (op ++ ": the watch is closed")
                    Nothing
                    Nothing
        else action fd

-- | Run an action holding the lock of the watch, do nothing if the watch is
-- closed. The action must not block. See 'withWatchFd'.
whenWatchOpen :: Watch -> IO () -> IO ()
whenWatchOpen (Watch _ lock _) action =
    withMVar lock $ \st -> unless (fsWatchClosed st) action

isWatchClosed :: Watch -> IO Bool
isWatchClosed (Watch _ lock _) = fsWatchClosed <$> readMVar lock

foreign import ccall unsafe
    "sys/inotify.h inotify_add_watch" c_inotify_add_watch
        :: CInt -> CString -> CUInt -> IO CInt

-- Returns 'False' if the path is already being watched.
insertPath :: Int -> PathRep -> IntMap WdEntry -> (IntMap WdEntry, Bool)
insertPath wd prep m =
    case Map.lookup wd m of
        Just (WdPaths wps)
            | any isSame wps -> (m, False)
            | otherwise -> (Map.insert wd (WdPaths (wps ++ [new])) m, True)
        _ -> (Map.insert wd (WdPaths [new]) m, True)

    where

    new = WatchPath {wpRep = prep, wpRemoved = False}
    isSame wp = not (wpRemoved wp) && eqPathRep (wpRep wp) prep

-- | Add the watch of a path using the given flags, without scanning it.
-- Returns the watch descriptor, and 'False' if the path is already being
-- watched.
addPathWith :: Word32 -> Watch -> PathRep -> IO (Int, Bool)
addPathWith flags watch0@(Watch _ _ wdMapRef) prep = do
    let absPath = combinePathRep prep

    -- The watch descriptor is inserted in the map with the lock held, so that
    -- the watch cannot be closed in between.
    withWatchFd "addToWatch" watch0 $ \fd -> do
        wd <- A.asCStringUnsafe (Path.toArray absPath) $ \pathPtr ->
            throwErrnoIfMinus1 ("addToWatch: " ++ Path.toString absPath) $
                c_inotify_add_watch fd pathPtr (CUInt flags)

        -- We add the parent first so that we start getting events for any
        -- new creates and add the new subdirectories on creates while we are
        -- adding the children.
        --
        -- A directory created while its parent is being scanned is added
        -- twice, once by the scan and once on its create event. The second
        -- add finds the path already watched and does not scan the directory
        -- again.
        --
        -- The map update must be atomic because more than one thread
        -- modifies the map. 'addToWatch' and 'removeFromWatch' can be called
        -- from any thread while the thread reading the event stream adds
        -- subdirectories on create events and removes watch descriptors on
        -- IN_IGNORED events. A separate readIORef and writeIORef could lose
        -- an update made by another thread in between. 'insertPath' also
        -- checks whether the path is already watched and inserts it in the
        -- same step, so when a scan and a create event in different threads
        -- add the same directory, only one of them gets 'isNew' as 'True' and
        -- scans the directory.
        (,) (fromIntegral wd)
            <$> atomicModifyIORef' wdMapRef (insertPath (fromIntegral wd) prep)

-- | Add the watch of a path, without scanning it. Returns 'False' if the path
-- is already being watched.
addPath :: Config -> Watch -> PathRep -> IO Bool
addPath cfg wt prep = snd <$> addPathWith (watchFlags cfg) wt prep

foreign import ccall unsafe
    "sys/inotify.h inotify_rm_watch" c_inotify_rm_watch
        :: CInt -> CInt -> IO CInt

-- | Call inotify_add_watch with the given flags on a path that has the watch
-- descriptor @wd@. Returns 'True' if the path still refers to that directory.
--
-- inotify_add_watch takes a path, not a watch descriptor. The directory may
-- have been deleted, moved or replaced after it was added:
--
-- * If the path does not exist, or it is not a directory, there is nothing to
--   change. The directory is reported as deleted or moved by its parent, which
--   removes its watch. A watch root keeps the flags it was added with.
-- * If the path is now another directory which is not in the map, the call
--   adds a watch that nothing refers to, it is removed. The IN_IGNORED event
--   of such a watch descriptor is dropped. The move event of the path adds the
--   directory with its new path.
-- * If the path is now another directory in the map, it has the same flags.
updateWatch :: Word32 -> Watch -> Int -> PathRep -> IO Bool
updateWatch flags wt@(Watch _ _ wdMapRef) wd prep =
    withWatchFd "addToWatch" wt $ \fd -> do
        r <- A.asCStringUnsafe (Path.toArray absPath) $ \pathPtr ->
            try
                $ throwErrnoIfMinus1 ("addToWatch: " ++ Path.toString absPath)
                $ c_inotify_add_watch fd pathPtr (CUInt flags)
        case r of
            Left (e :: IOError) ->
                let notDir = fmap Errno (ioe_errno e) == Just eNOTDIR
                 in if isDoesNotExistError e || notDir
                    then return False
                    else ioError e
            Right wd1 -> do
                m <- readIORef wdMapRef
                let same = fromIntegral wd1 == wd
                when (not same && Map.notMember (fromIntegral wd1) m)
                    $ void $ c_inotify_rm_watch fd wd1
                return same

    where

    absPath = combinePathRep prep

-- | @addToWatch cfg watch root subpath@ adds @subpath@ to the list of paths
-- being monitored under @root@ via the watch handle @watch@.  @root@ must be
-- an absolute path and @subpath@ must be relative to @root@.
--
-- If the same directory is added via two different roots, for example, a
-- recursive root and a root inside it, its events are reported once for each
-- root.
--
-- Can be called from any thread, including while another thread reads the
-- events of the watch using 'watchToStream'. The call does not wait for an
-- event. Raise an exception if the watch is closed.
--
-- /Pre-release/
--
addToWatch :: Config -> Watch -> PathRep -> IO ()
addToWatch cfg@Config{..} watch0 prep = do
    --    showPathRep (root, Nothing) = "root = " ++ Path.toString root
    --    showPathRep (root, Just sub) =
    --        "root = " ++ Path.toString root ++ " sub = " ++ Path.toString sub
    -- putStrLn $ showPathRep prep ++ " absPath = " ++ Path.toString absPath

    -- A subdirectory may be deleted or moved before its watch is added,
    -- 'addSubdirToWatch' ignores the error in that case.
    --
    -- NOTE: A subdirectory is added when its IN_CREATE or IN_MOVED_TO event
    -- is read, not when it is created. inotify_add_watch resolves the path at
    -- that time, if the directory was deleted or moved and another directory
    -- was created at the same path, the new directory is watched. The later
    -- events of the parent bring the map to the final state:
    --
    -- * mkdir d, rmdir d, mkdir d: the first IN_CREATE watches the second d.
    --   IN_DELETE does not change the map. The second IN_CREATE finds the
    --   path already watched and does not scan it again.
    -- * mkdir d, mv d e, mkdir d: the first IN_CREATE watches the second d.
    --   IN_MOVED_FROM of d removes the watch of the second d, the second
    --   IN_CREATE adds it again with a new watch descriptor and scans it
    --   again. No watch covers the second d in between.
    -- * mkdir d, mv d e, mv f d, where f is watched: IN_CREATE of d finds
    --   the watch descriptor of f and adds d as its second path. Until the
    --   marker queued by IN_MOVED_FROM of d, the events in f are reported
    --   twice, under f and under d.
    --
    -- 'addNewDir' reports the entries of a created directory. This scan adds
    -- the subdirectories only, it is used for the roots, for the directories
    -- moved in and after an overflow.
    --
    -- XXX If the caller does not have permission to read the path,
    -- inotify_add_watch fails with EACCES and we throw an exception. We could
    -- add a config option to ignore such errors, like the options for
    -- ignoring errors when reading a directory.
    --
    (wd, isNew) <- addPathWith (scanFlags cfg) watch0 prep

    -- Now add the children. If we missed any creates while we were adding the
    -- parent, this will make sure they are added too.
    when (isNew && watchRec) $ addSubdirs cfg watch0 prep

    -- The directory has been listed. The watch of its parent still has
    -- 'scanFlags' until the scan of the parent ends, the directories are
    -- changed in the reverse order of adding them. An already watched path is
    -- changed too, to add the flags of this config.
    when (scanFlags cfg /= watchFlags cfg)
        $ void $ updateWatch (watchFlags cfg) watch0 wd prep

-- | Run an action on each subdirectory of a path, if the path is a directory.
forSubdirs :: PathRep -> (PathRep -> IO ()) -> IO ()
forSubdirs prep action = do
    let absPath = combinePathRep prep
    -- The errors ignored:
    --
    -- * ENOENT from lstat of a DT_UNKNOWN entry deleted after readdir
    --   returns it, 'ignoreMissing' skips the entry.
    -- * ENOENT from opendir of absPath, absPath was deleted. The watch on
    --   absPath reports the deletion.
    -- * ENOTDIR from opendir of absPath, absPath is not a directory. A root
    --   added without 'setOnlyDir' may be a file, and a directory may be
    --   replaced by a file after its watch is added.
    let f = action . addSub prep
    S.fold (FL.drainMapM f) (Dir.readDirs (Dir.ignoreMissing True) absPath)
        `catchIOError` \e ->
            let notDir = fmap Errno (ioe_errno e) == Just eNOTDIR
             in unless (isDoesNotExistError e || notDir) (ioError e)

-- | Add the subdirectories of a path to the watch, if the path is a
-- directory. The subdirectories already watched via the same root are not
-- scanned again.
addSubdirs :: Config -> Watch -> PathRep -> IO ()
addSubdirs cfg wt prep = forSubdirs prep (addSubdirToWatch cfg wt)

-- | Like 'addToWatch' but does not raise an exception if the path does not
-- exist, is not a directory, or if the watch is closed. Used to add the
-- subdirectories found by the scan or reported by IN_CREATE or IN_MOVED_TO,
-- a subdirectory may be deleted, moved or replaced by another file before its
-- watch is added. Its parent reports the deletion or the move. The watch may
-- be closed by another thread while the event stream adds a subdirectory.
--
-- The subdirectory is added with 'setOnlyDir' and without
-- 'setFollowSymLinks', the add fails with ENOTDIR if the path is now a file
-- or a symbolic link. The scan does not follow symbolic links either.
addSubdirToWatch :: Config -> Watch -> PathRep -> IO ()
addSubdirToWatch cfg wt prep =
    ignoreSubdirError wt () (addToWatch (subdirConfig cfg) wt prep)

subdirConfig :: Config -> Config
subdirConfig = setOnlyDir True . setFollowSymLinks False

-- | Return the default value if the action fails because the subdirectory
-- does not exist, is not a directory, or if the watch is closed.
ignoreSubdirError :: Watch -> a -> IO a -> IO a
ignoreSubdirError wt def action =
    action `catchIOError` \e -> do
        closed <- isWatchClosed wt
        let notDir = fmap Errno (ioe_errno e) == Just eNOTDIR
        if isDoesNotExistError e || notDir || closed
        then return def
        else ioError e

-- | An entry found by 'addNewDir'.
data ScannedEntry = ScannedEntry
    { seIsDir :: Bool
    , sePath :: PathRep
    }

-- | Add a directory reported by IN_CREATE and return the entries in it. The
-- watch is added before the directory is read, an entry created later is
-- either found by the scan or reported by the kernel, or both.
--
-- A directory already watched returns no entries, the scan that added it
-- returned them, or it is the scan of a root or of an overflow.
addNewDir :: Config -> Watch -> PathRep -> IO [ScannedEntry]
addNewDir cfg wt prep = do
    isNew <- ignoreSubdirError wt False (addPath (subdirConfig cfg) wt prep)
    if isNew
    then do
        entries <-
            S.fold FL.toList (Dir.readEither (Dir.ignoreMissing True) absPath)
                `catchIOError` \e ->
                    let notDir = fmap Errno (ioe_errno e) == Just eNOTDIR
                     in if isDoesNotExistError e || notDir
                        then return []
                        else ioError e
        concat <$> mapM addEntry entries
    else return []

    where

    absPath = combinePathRep prep

    addEntry (Left dir) = do
        let path = addSub prep dir
        rest <- addNewDir cfg wt path
        return (ScannedEntry True path : rest)
    addEntry (Right file) =
        return [ScannedEntry False (addSub prep file)]

foreign import ccall unsafe "unistd.h pipe2"
    c_pipe2 :: Ptr CInt -> CInt -> IO CInt

foreign import capi "fcntl.h value O_CLOEXEC" o_CLOEXEC :: CInt

foreign import ccall unsafe "unistd.h close" c_close :: CInt -> IO CInt

-- | Mark the paths that satisfy the predicate as removed. Returns the watch
-- descriptors whose paths are now all removed, and the removed paths of the
-- other watch descriptors, both in the ascending order of watch descriptors.
markRemoved ::
       (PathRep -> Bool)
    -> IntMap WdEntry
    -> (IntMap WdEntry, ([Int], [(Int, PathRep)]))
markRemoved p m =
    let ((wds, shared), m1) = Map.mapAccumWithKey step ([], []) m
     in (m1, (reverse wds, reverse shared))

    where

    isRemoving wp = not (wpRemoved wp) && p (wpRep wp)
    setRemoved wp = if isRemoving wp then wp {wpRemoved = True} else wp

    step acc _ e@(WdMarker _ _) = (acc, e)
    step acc@(wds, shared) wd (WdPaths wps) =
        let removing = filter isRemoving wps
            wps1 = map setRemoved wps
            shared1 = reverse [(wd, wpRep wp) | wp <- removing] ++ shared
         in if null removing
            then (acc, WdPaths wps)
            else if all wpRemoved wps1
            then ((wd : wds, shared), WdPaths wps1)
            else ((wds, shared1), WdPaths wps1)

-- | Undo 'markRemoved' for the listed paths. For each path, the first path of
-- its watch descriptor that is marked removed and equal to it is unmarked.
-- Identical paths of a watch descriptor report the same events, it does not
-- matter which one is unmarked.
unmarkRemoved :: [(Int, PathRep)] -> IntMap WdEntry -> IntMap WdEntry
unmarkRemoved paths m0 = foldl step m0 paths

    where

    step m (wd, prep) =
        let isMatch wp = wpRemoved wp && eqPathRep (wpRep wp) prep
         in case Map.lookup wd m of
                Just (WdPaths wps)
                    | (xs, wp : ys) <- break isMatch wps ->
                        let wp1 = wp {wpRemoved = False}
                         in Map.insert wd (WdPaths (xs ++ wp1 : ys)) m
                _ -> m

-- | Queue an IN_IGNORED event on the inotify fd by adding a watch and
-- removing it. The action is run with the watch descriptor after adding the
-- watch and before removing it. The string is used in the exception
-- messages.
--
-- Requires @\/proc@ to be mounted and a free fd for a pipe, throw an
-- exception if not.
queueIgnored :: String -> CInt -> (CInt -> IO ()) -> IO ()
queueIgnored err fd action =
    allocaArray 2 $ \fds -> do
        -- For the marker event we need an inode that no user path can use,
        -- otherwise it may share the watch descriptor of a user watch. A temp
        -- file could be watched by the user. We use a pipe for this purpose,
        -- no path in a mounted file system reaches a pipe, and each pipe is a
        -- new inode, so each marker gets its own watch descriptor. Closing the
        -- pipe frees the inode, there is no file to clean up.
        -- inotify_add_watch takes a path, we use the pipe path at
        -- @\/proc\/self\/fd@ to add to the watch.
        throwErrnoIfMinus1_ err $ c_pipe2 fds o_CLOEXEC
        rfd <- peekElemOff fds 0
        wfd <- peekElemOff fds 1
        let path = "/proc/self/fd/" ++ show rfd
        flip finally (c_close rfd >> c_close wfd) $ do
            wd <- withCString path $ \pathPtr ->
                throwErrnoIfMinus1 err
                    $ c_inotify_add_watch fd pathPtr (CUInt iN_ACCESS)
            action wd
            throwErrnoIfMinus1_ err $ c_inotify_rm_watch fd wd

-- | When multiple paths are being watched through the same wd and we remove
-- only one path we cannot remove the wd but we still need to identify the
-- point in the event queue up to which we need to report the events for the
-- path being removed.
--
-- To do that we force queue an IN_IGNORED event by creating and immediately
-- deleting a watch. All the watch descriptors of an inotify fd share one event
-- queue. When the IN_IGNORED event for this wd shows up in the inotify queue
-- we know that's the end point we were looking for.
--
addRemovalMarker ::
    CInt -> IORef (IntMap WdEntry) -> [Int] -> [(Int, PathRep)] -> IO ()
addRemovalMarker fd wdMapRef markers paths =
    queueIgnored err fd $ \wd ->
        -- The marker must be in the map before its IN_IGNORED is read.
        atomicModifyIORef' wdMapRef
            $ \m ->
                let e = WdMarker markers paths
                 in (Map.insert (fromIntegral wd) e m, ())

    where

    err = "unwatchPaths: marker " ++ show (map fst paths)

-- | Remove the paths that satisfy the predicate from the watch. The string
-- describes the paths in the exception message.
--
-- The events already queued for the paths are reported, followed by an
-- 'isRootUnwatched' event for each path.
--
-- Removing a path that shares the directory with another path requires
-- @\/proc@ to be mounted and a free fd for a pipe, throw an exception if
-- not. The paths of the shared watch descriptors are not removed in that
-- case, the call can be retried.
--
-- Does nothing if the watch is closed, closing the watch removes all the
-- paths.
unwatchPaths :: Watch -> String -> (PathRep -> Bool) -> IO ()
unwatchPaths wt@(Watch fd _ wdMapRef) desc p =
    whenWatchOpen wt $ do
        -- A watch descriptor can be shared by more than one path, see
        -- 'WatchPath' for the cases.
        --
        -- Only when all the paths of a watch descriptor are removed we remove
        -- the kernel watch. Otherwise, the kernel watch is still needed for
        -- the other paths.
        --
        (wds, shared) <- atomicModifyIORef' wdMapRef (markRemoved p)

        -- The kernel queues an IN_IGNORED event for a removed watch. After
        -- calling inotify_rm_watch, a path still stays in the map until an
        -- IN_IGNORED event arrives, so that the events already queued for the
        -- removed path, including IN_IGNORED, reports the path.
        mapM_ rmWatch wds

        -- If the kernel watch is still needed for the other paths, we do not
        -- remove the watch, therefore, no IN_IGNORED event will arrive for it.
        -- But we still need to read and report all the events queued on this
        -- wd till this moment, for our current that is being removed. To do so
        -- we add another watch to our inotify instance and immediately delete
        -- that watch, this will force an IN_IGNORED event for that watch which
        -- will arrive after all the events queued till now. By identifying the
        -- wd of that marker watch we can issue an end-of-watch event for our
        -- path being remove from the shared wd.
        --
        when (not (null shared))
            $ addRemovalMarker fd wdMapRef [] shared
                `onException` atomicModifyIORef' wdMapRef
                    (\m -> (unmarkRemoved shared m, ()))

    where

    rmWatch wd = do
        let err = "unwatchPaths: " ++ desc
        r <- c_inotify_rm_watch fd (fromIntegral wd)
        when (r == -1) $ do
            errno <- getErrno
            -- EINVAL means that the kernel has already removed the watch, for
            -- example, when the directory was deleted.
            when (errno /= eINVAL) $ throwErrno err

-- | Remove an absolute root path from a 'Watch', if a path was moved after
-- adding you need to provide the original path which was used to add the
-- Watch.
--
-- The events already queued for the root are reported, followed by an
-- 'isRootUnwatched' event for each path watched under the root. A directory
-- that is also watched via another root remains watched for that root.
--
-- Removing a root that shares a directory with another root requires
-- @\/proc@ to be mounted and a free fd for a pipe, throw an exception if
-- not. The shared directories remain watched for the root in that case, the
-- call can be retried.
--
-- Can be called from any thread, including while another thread reads the
-- events of the watch using 'watchToStream'. The call does not wait for an
-- event. Does nothing if the watch is closed.
--
-- /Pre-release/
--
removeFromWatch :: Watch -> Path -> IO ()
removeFromWatch wt root =
    unwatchPaths wt (show (Path.toString root)) (eqPath root . getRepRoot)

-- | Close the fd of a watch. closeFdWith wakes up the threads waiting for
-- the fd to become readable, in the threaded RTS. The kernel releases the fd
-- even if close fails.
closeInotifyFd :: CInt -> IO ()
closeInotifyFd fd = closeFdWith (\_ -> void (c_close fd)) (Fd fd)

-- | Close a 'Watch' handle. The event streams of the watch end, a thread
-- waiting for events in 'watchToStream' is woken up. Closing a closed watch
-- does nothing.
--
-- Can be called from any thread, including while another thread reads the
-- events of the watch using 'watchToStream'. The call does not wait for an
-- event.
--
-- With the non-threaded RTS, the fd is closed when no thread is waiting for
-- events. To wake up the waiting threads, 'closeWatch' queues an event, which
-- requires @\/proc@ to be mounted and a free fd for a pipe. If the event
-- cannot be queued, the waiting threads wake up when the next event arrives,
-- their streams end then.
--
-- /Pre-release/
--
closeWatch :: Watch -> IO ()
closeWatch (Watch fd lock _) =
    modifyMVar_ lock $ \st ->
        if fsWatchClosed st
        then return st
        -- The non-threaded RTS waits for the fds in select, select fails
        -- with EBADF if a waited fd is closed, and the RTS terminates the
        -- program. The last waiting thread closes the fd.
        else if rtsSupportsBoundThreads || fsFdWaiters st == 0
        then do
            closeInotifyFd fd
            return st {fsWatchClosed = True, fsFdOpen = False}
        else do
            -- The waiting threads find the watch closed and do not read the
            -- event.
            queueIgnored "closeWatch" fd (\_ -> return ())
                `catchIOError` \_ -> return ()
            return st {fsWatchClosed = True}

-- | Given a 'Config' and list of @paths@ ("/" separated byte arrays) start
-- monitoring the paths for file system events. Returns a 'Watch' handle which
-- can then be used to read the event stream or to close the watch.
--
-- If adding a path fails, the watch is closed and the exception is rethrown.
--
-- /Pre-release/
--
openWatch :: Config -> NonEmpty Path -> IO Watch
openWatch cfg paths =
    -- The handle is not returned to the caller if adding a path fails, the
    -- caller cannot close it. 'mask' ensures that an async exception cannot
    -- arrive between creating the watch and installing the handler.
    Exception.mask $ \restore -> do
        w <- createWatch
        restore (mapM_ (add w) (NonEmpty.toList paths))
            `onException` closeWatch w
        return w

    where

    add w root = addToWatch cfg w (root, Nothing)

-------------------------------------------------------------------------------
-- Raw events read from the watch file handle
-------------------------------------------------------------------------------

-- | Identifies the 'isMovedFrom' and 'isMovedTo' events of the same move
-- operation, see 'getCookie'.
newtype Cookie = Cookie Word32 deriving (Show, Eq)

-- | An Event generated by the file system. Use the accessor functions to
-- examine the event.
data Event = Event
   { eventWd :: CInt
   , eventFlags :: Word32
   , eventCookie :: Word32
   , eventRoot :: Path
   , eventRelPath :: Maybe Path
   , eventScanned :: Bool
   }

-- The inotify event struct from the man page/header file:
--
--            struct inotify_event {
--                int      wd;       /* Watch descriptor */
--                uint32_t mask;     /* Mask describing event */
--                uint32_t cookie;   /* Unique cookie associating related
--                                      events (for rename(2)) */
--                uint32_t len;      /* Size of name field */
--                char     name[];   /* Optional null-terminated name */
--            };
--
foreign import capi
    "sys/inotify.h value IN_Q_OVERFLOW" iN_Q_OVERFLOW :: Word32

foreign import capi
    "sys/inotify.h value IN_IGNORED" iN_IGNORED :: Word32

-- | Remove a marker and the removed paths listed in it from the map. Returns
-- the paths that were still in the map.
removeMarker ::
       Int
    -> [Int]
    -> [(Int, PathRep)]
    -> IntMap WdEntry
    -> (IntMap WdEntry, [(Int, PathRep)])
removeMarker marker markers paths m0 =
    let m = foldr Map.delete m0 (marker : markers)
        (m1, removed) = foldl step (m, []) paths
     in (m1, reverse removed)

    where

    step (m, acc) (wd, prep) =
        let isMatch wp = wpRemoved wp && eqPathRep (wpRep wp) prep
         in case Map.lookup wd m of
                Just (WdPaths wps)
                    | (xs, _ : ys) <- break isMatch wps ->
                        (update wd (xs ++ ys) m, (wd, prep) : acc)
                _ -> (m, acc)

    update wd [] m = Map.delete wd m
    update wd wps m = Map.insert wd (WdPaths wps) m

-- | The removed paths whose IN_IGNORED event is not read yet. Returns a pair:
--
-- * the watch descriptors of the markers
-- * the removed paths, as (user watch descriptor, path watched via it). These
--   are the paths listed in the markers, and the paths watched via a user
--   watch descriptor when every path watched via it is removed.
pendingRemovals :: IntMap WdEntry -> ([Int], [(Int, PathRep)])
pendingRemovals m =
    ( [wd | (wd, WdMarker _ _) <- entries]
    , [p | (_, WdMarker _ ps) <- entries, p <- ps]
        ++ [ (wd, wpRep wp)
           | (wd, WdPaths wps) <- entries
           , all wpRemoved wps
           , wp <- wps
           ]
    )

    where

    entries = Map.toList m

-- | The kernel drops the events that do not fit in the event queue,
-- including the IN_IGNORED events of the removed watches and of the markers,
-- and queues an IN_Q_OVERFLOW event instead. The removed paths would stay in
-- the map forever, and the removed paths of a shared watch descriptor would
-- keep reporting its events.
--
-- Queue a marker listing the removed paths and the markers whose IN_IGNORED
-- event has not been read. The IN_IGNORED events of these were generated
-- before the new marker, the queue is in order, if one of them is not read
-- before the IN_IGNORED event of the new marker, it was dropped. If the new
-- marker is dropped too, the next IN_Q_OVERFLOW event queues another one.
--
-- Failing to queue the marker, for example, when the process is out of fds,
-- is ignored, the next IN_Q_OVERFLOW event tries again.
syncRemovals :: Watch -> IO ()
syncRemovals wt@(Watch fd _ wdMapRef) =
    whenWatchOpen wt $ do
        (markers, paths) <- pendingRemovals <$> readIORef wdMapRef
        when (not (null markers && null paths))
            $ addRemovalMarker fd wdMapRef markers paths
                `catchIOError` \_ -> return ()

-- | Add a directory and the tree under it again, recording the paths found.
--
-- The flags are replaced, not added to, so that an existing watch stops
-- generating the events of a listing, see 'scanFlags'. The parent has already
-- been changed by the time a directory is listed, a listing generates no
-- events. inotify_add_watch returns the existing watch descriptor of a
-- directory, and adds a watch if it has none. 'insertPath' adds the path if the
-- watch descriptor does not have it, for example, if the directory was renamed.
restartDir :: Config -> Watch -> IORef (Set (Array Word8)) -> PathRep -> IO ()
restartDir cfg wt seen prep = do
    (wd, _) <- addPathWith (scanFlags cfg .&. complement iN_MASK_ADD) wt prep
    modifyIORef' seen (Set.insert (pathKey prep))
    forSubdirs prep restartSubdir
    when (scanFlags cfg /= watchFlags cfg)
        $ void $ updateWatch (watchFlags cfg) wt wd prep

    where

    restartSubdir p =
        ignoreSubdirError wt () (restartDir (subdirConfig cfg) wt seen p)

pathKey :: PathRep -> Array Word8
pathKey = Path.toArray . combinePathRep

-- | Add the tree of each watch root again, after an event queue overflow. A
-- subdirectory is added on its IN_CREATE or IN_MOVED_TO event, and its path is
-- changed on IN_MOVED_FROM, these events are dropped when the queue overflows.
--
-- The tree is added in place, the watch descriptors are not removed. Removing
-- a watch queues an IN_IGNORED event, the removal of a tree of more than
-- max_queued_events directories would overflow the queue again.
--
-- A path under a restarted root that is not found is no longer in the tree, it
-- is removed. A root is skipped if its path no longer refers to the watched
-- directory, a renamed root is still watched and its subdirectories cannot be
-- reached by the old path. Failing to remove a path is ignored, like in
-- 'syncRemovals', the next overflow tries again.
restartTree :: Config -> Watch -> IO ()
restartTree cfg wt@(Watch _ _ wdMapRef) = do
    m <- readIORef wdMapRef
    seen <- newIORef Set.empty
    roots <-
        filterM (restartRoot seen)
            [ (wd, wpRep wp)
            | (wd, WdPaths wps) <- Map.toList m
            , wp <- wps
            , not (wpRemoved wp)
            , isNothing (getRepSub (wpRep wp))
            ]
    found <- readIORef seen
    let isRestarted prep = any (eqPath (getRepRoot prep) . fst . snd) roots
        isStale prep =
               isJust (getRepSub prep)
            && isRestarted prep
            && Set.notMember (pathKey prep) found
    unwatchPaths wt "restartTree" isStale `catchIOError` \_ -> return ()

    where

    restartRoot seen (wd, prep) = do
        same <-
            ignoreSubdirError wt False
                $ updateWatch (watchFlags cfg) wt wd prep
        when same $ ignoreSubdirError wt () (restartDir cfg wt seen prep)
        return same

-- XXX We can perhaps use parseD monad instance for fusing with parseMany? Need
-- to measure the perf.
--
-- An event is reported once for each path of its watch descriptor. An
-- IN_IGNORED event removes its watch descriptor from the map, the event
-- itself still reports the root. Events for a watch descriptor that is not in
-- the map are dropped.
--
-- The IN_IGNORED event of a marker reports 'isRootUnwatched' for the paths
-- listed in the marker, and removes the paths and the listed markers from the
-- map. An IN_Q_OVERFLOW event queues a marker for the removals whose
-- IN_IGNORED event may have been dropped. The marker is queued after
-- 'restartTree', which can remove more paths.
--
{-# INLINE readOneEvent #-}
readOneEvent :: Config -> Watch -> Parser Word8 IO [Event]
readOneEvent cfg  wt@(Watch _ _ wdMapRef) = do
    -- XXX the header format is implicit in the code, this could be bug prone.
    let headerLen = sizeOf (undefined :: CInt) + 12
    arr <- PR.takeEQ headerLen (A.createOf headerLen)
    (ewd, eflags, cookie, pathLen) <- PR.fromEffect $ A.unsafePinnedAsPtr arr readHeader
    mpth <-
        if pathLen /= 0
        then do
            arr1 <- PR.takeEQ pathLen (A.createOf pathLen)
            -- The name is null terminated and null padded to pathLen bytes.
            let pth = fst (A.breakEndBy_ (== 0) arr1)
            return $ Just (Path.unsafeFromArray pth)
        else return Nothing
    wdm <- PR.fromEffect $ readIORef wdMapRef
    let mkEvent wd (root, rel) =
            Event
                { eventWd = wd
                , eventFlags = eflags
                , eventCookie = cookie
                , eventRoot = root
                , eventRelPath = rel
                , eventScanned = False
                }
        mkScanned ScannedEntry{..} =
            (mkEvent ewd sePath)
                { eventFlags =
                    if seIsDir then iN_CREATE .|. iN_ISDIR else iN_CREATE
                , eventCookie = 0
                , eventScanned = True
                }
        isIgnored = eflags .&. iN_IGNORED /= 0
    -- The kernel event queue overflowed and events were dropped. The overflow
    -- event is not tied to a watch, its wd is -1. Report one overflow event
    -- per watch root, so that the consumer knows that events may be missing
    -- for every root.
    -- The roots are the paths without a sub path; a path with a sub path is a
    -- subdirectory added under a recursive root. Marker entries are skipped,
    -- they are not watched paths.
    if eflags .&. iN_Q_OVERFLOW /= 0
    then do
        when (watchRec cfg && restartOnOverflow cfg)
            $ PR.fromEffect
            $ restartTree cfg wt
        PR.fromEffect $ syncRemovals wt
        return
            [ mkEvent (fromIntegral wd) (wpRep wp)
            | (wd, WdPaths wps) <- Map.toList wdm
            , wp <- wps
            , isNothing (getRepSub (wpRep wp))
            ]
    else
        case Map.lookup (fromIntegral ewd) wdm of
            Nothing -> return []
            Just (WdMarker markers paths)
                | isIgnored -> do
                    removed <-
                        PR.fromEffect
                            $ atomicModifyIORef' wdMapRef
                            $ removeMarker (fromIntegral ewd) markers paths
                    return
                        [ mkEvent (fromIntegral wd) prep
                        | (wd, prep) <- removed
                        ]
                | otherwise -> return []
            Just (WdPaths wps) -> do
                let addName wp =
                        wp {wpRep = maybe id (flip addSub) mpth (wpRep wp)}
                    wps1 = map addName wps
                    -- Do not add or remove the directories of a removed
                    -- root.
                    live = map wpRep $ filter (not . wpRemoved) wps1
                    -- Check for "ISDIR" first because it is less likely
                    isDirEv = eflags .&. iN_ISDIR /= 0
                    isDirCreated = isDirEv && eflags .&. iN_CREATE /= 0
                    isDirMovedIn = isDirEv && eflags .&. iN_MOVED_TO /= 0
                    isDirMovedOut = isDirEv && eflags .&. iN_MOVED_FROM /= 0
                    reportScan =
                           scanNewDirs cfg
                        && createFlags cfg .&. iN_CREATE /= 0
                -- A directory renamed within the tree is removed on
                -- IN_MOVED_FROM and added again on IN_MOVED_TO.
                when (watchRec cfg && isDirMovedOut)
                    $ PR.fromEffect
                    $ mapM_
                        (\prep ->
                            unwatchPaths wt
                                (show (Path.toString (combinePathRep prep)))
                                (isInSubtree prep)
                        )
                        live
                when (watchRec cfg
                        && (isDirMovedIn || (isDirCreated && not reportScan)))
                    $ PR.fromEffect
                    $ mapM_ (addSubdirToWatch cfg wt) live
                scanned <-
                    if watchRec cfg && isDirCreated && reportScan
                    then
                        PR.fromEffect
                            $ concat <$> mapM (addNewDir cfg wt) live
                    else return []
                when isIgnored
                    $ PR.fromEffect
                    $ atomicModifyIORef' wdMapRef
                    $ \m -> (Map.delete (fromIntegral ewd) m, ())
                -- The parent of a subdirectory reports its deletion and move
                -- as IN_DELETE and IN_MOVED_FROM, the IN_DELETE_SELF and
                -- IN_MOVE_SELF events are reported for the roots only.
                let isSelfEvent =
                        eflags .&. (iN_DELETE_SELF .|. iN_MOVE_SELF) /= 0
                    -- The events added by 'watchFlags' only.
                    unrequested =
                        recursiveEvents .&. complement (createFlags cfg)
                    isUnrequested = eflags .&. unrequested /= 0
                    isReported wp =
                        not isSelfEvent || isNothing (getRepSub (wpRep wp))
                if isUnrequested
                then return []
                else return
                        $ [mkEvent ewd (wpRep wp) | wp <- wps1, isReported wp]
                        ++ map mkScanned scanned

    where

    readHeader (ptr :: Ptr Word8) _ = do
        let len = sizeOf (undefined :: CInt)
        ewd :: CInt <- peek (castPtr ptr)
        eflags <- peekByteOff ptr len
        cookie <- peekByteOff ptr (len + 4)
        pathLen :: Word32 <- peekByteOff ptr (len + 8)
        return (ewd, eflags, cookie, fromIntegral pathLen)

foreign import ccall unsafe "unistd.h read"
    c_read :: CInt -> Ptr Word8 -> CSize -> IO CSsize

-- | Read the events available on the fd of a watch, wait for the events if
-- none is available. Returns 'Nothing' if the watch is closed.
readEvents :: Watch -> IO (Maybe (Array Word8))
readEvents (Watch fd lock _) = loop

    where

    -- As per the inotify man page, a buffer of size
    --
    --          sizeof(struct inotify_event) + NAME_MAX + 1
    --
    -- is sufficient to read at least one event. A smaller buffer fails with
    -- EINVAL.
    bufSize = 32 * 1024

    -- Returns 0 if no event is available.
    readFd p = do
        n <- c_read fd p (fromIntegral bufSize)
        if n >= 0
        then return (fromIntegral n)
        else do
            errno <- getErrno
            if errno == eAGAIN || errno == eWOULDBLOCK || errno == eINTR
            then return 0
            else throwErrno "watchToStream"

    -- In the threaded RTS, the wait is registered with the IO manager with
    -- the lock held, if 'closeWatch' closes the fd after that, the wait
    -- raises an exception. If the fd were closed before the registration, the
    -- registration could wait on a reused fd number.
    --
    -- In the non-threaded RTS, the waiting thread is counted with the lock
    -- held, 'closeWatch' does not close the fd while the thread waits.
    register st =
        if rtsSupportsBoundThreads
        then do
            (wait, unregister) <- threadWaitReadSTM (Fd fd)
            -- The next iteration returns 'Nothing' if the exception is due
            -- to 'closeWatch'.
            let waitTh restore =
                    restore (atomically wait) `catchIOError` (\_ -> return ())
                        `onException` unregister
            return (st, Just (Left waitTh))
        else do
            -- The lock is never held across a blocking call, taking it
            -- uninterruptibly cannot block for long.
            let waitNonTh restore =
                    restore (threadWaitRead (Fd fd))
                        `finally` Exception.uninterruptibleMask_
                            (modifyMVar_ lock unregisterNonTh)
            return
                ( st {fsFdWaiters = fsFdWaiters st + 1}
                , Just (Left waitNonTh)
                )

    unregisterNonTh st = do
        let st1 = st {fsFdWaiters = fsFdWaiters st - 1}
        if fsWatchClosed st1 && fsFdOpen st1 && fsFdWaiters st1 == 0
        then do
            closeInotifyFd fd
            return st1 {fsFdOpen = False}
        else return st1

    -- The wait must be counted or unregistered even if an async exception
    -- arrives after the registration.
    loop = do
        r <- Exception.mask $ \restore -> do
            r <- modifyMVar lock $ \st ->
                if fsWatchClosed st
                then return (st, Nothing)
                else do
                    arr <- MArray.unsafeCreateWithPtr' bufSize readFd
                    if MArray.byteLength arr /= 0
                    then return (st, Just $ Right $ unsafeFreezeWithShrink arr)
                    else register st
            case r of
                Just (Left wait) -> wait restore >> return (Just (Left ()))
                Just (Right arr) -> return (Just (Right arr))
                Nothing -> return Nothing
        case r of
            Nothing -> return Nothing
            Just (Right arr) -> return (Just arr)
            Just (Left ()) -> loop

-- | Read the events from a 'Watch' as a stream. The 'Config' must be the one
-- used to open the watch. The stream does not close the watch, use
-- 'closeWatch' to close it. The stream ends when the watch is closed.
--
-- While a thread reads the stream, other threads can use 'addToWatch',
-- 'removeFromWatch' and 'closeWatch' on the watch. An event is reported to
-- only one of the streams if more than one stream reads the same watch.
--
-- /Pre-release/
--
watchToStream :: Config -> Watch -> Stream IO Event
watchToStream cfg wt =
    S.unfoldEach UF.fromList
        $ S.catRights
        $ S.parseMany (readOneEvent cfg wt)
        $ S.unfoldEach A.reader
        $ S.unfoldrM (\() -> fmap (, ()) <$> readEvents wt) ()

-- XXX We should not go across the mount points of network file systems or file
-- systems that are known to not generate any events.

-- | Start monitoring a list of file system paths for file system events with
-- the supplied configuration operation over the 'defaultConfig'. The
-- paths could be files or directories. When recursive mode is set and the
-- path is a directory, the whole directory tree under it is watched
-- recursively. Monitoring starts from the current time onwards.
--
-- /Non-existing Paths:/ the API fails if a watch is started on a non-exsting
-- path.
--
-- /Overlapping Paths:/ a path listed more than once is watched once, its
-- events are reported once. If two different paths reach the same file or
-- directory, for example, a recursive root and a root inside it, its events
-- are reported once for each path.
--
-- /Performance:/ Note that recursive watch on a large directory tree could be
-- expensive. When starting a watch, the whole tree must be read and watches
-- are started on each directory in the tree. The initial time to start the
-- watch as well as the memory required is proportional to the number of
-- directories in the tree.
--
-- /Bugs:/ When new directories are created under the tree they are added to
-- the watch on receiving the directory create event. However, the creation of
-- a dir and adding a watch for it is not atomic.  The implementation takes
-- care of this and makes sure that watches are added for all directories.
-- However, In the mean time, the directory may have received more events which
-- may get lost.  Handling of any such lost events is yet to be implemented.
--
-- See the Linux __inotify__ man page for more details.
--
-- @
-- watchWith
--      ('setFollowSymLinks' True . 'setExcludeUnlinked' False)
--      [Path.fromString "dir"]
-- @
watchWith :: (Config -> Config) -> NonEmpty Path -> Stream IO Event
watchWith f paths = S.bracketIO before after (watchToStream cfg)

    where

    cfg = f defaultConfig
    before = openWatch cfg paths
    after = closeWatch

-- | Same as 'watchWith' using 'defaultConfig' and recursive mode.
--
-- >>> watchRecursive = watchWith (setRecursiveMode True)
--
-- See 'watchWith' for pitfalls and bugs when using recursive watch on Linux.
watchRecursive :: NonEmpty Path -> Stream IO Event
watchRecursive = watchWith (setRecursiveMode True)

-- | Same as 'watchWith' using defaultConfig and non-recursive mode.
--
-- >>> watch = watchWith id
watch :: NonEmpty Path -> Stream IO Event
watch = watchWith id

-------------------------------------------------------------------------------
-- Examine event stream
-------------------------------------------------------------------------------

-- | Get the watch root corresponding to the 'Event'.
--
-- If a directory is watched via more than one root, for example, a recursive
-- root and a root inside it, its events are reported once for each root.
--
-- For an 'isEventsLost' event, this is the watch root whose events may have
-- been lost. The kernel generates a single overflow event for all the watch
-- roots, it is reported as one 'isEventsLost' event for each watch root.
--
-- Note that if a path was moved after adding to the watch, this will give the
-- original path and not the new path after moving.
--
-- TBD: we can possibly update the watch root on a move self event.
getRoot :: Event -> Path
getRoot = eventRoot

-- getRelPath returns 'Nothing' rather than "." for the watch root because
-- the watch root may be a file, see the comment on getRelPath in
-- "Streamly.Internal.FS.Event".

-- | Get the file system object path for which the event is generated, relative
-- to the watched root. Returns 'Nothing' if the event is for the watch root
-- itself, or if it is an 'isEventsLost' event.
getRelPath :: Event -> Maybe Path
getRelPath Event{..} = eventRelPath

-- | Get the absolute file system object path for which the event is generated.
--
-- When the watch root is a symlink, the absolute path returned is via the
-- original symlink and not through the resolved path.
--
-- If the event is for the watch root this is the same as 'getRoot'.
getAbsPath :: Event -> Path
getAbsPath ev =
    case getRelPath ev of
        Just relPath -> Path.join (getRoot ev) relPath
        Nothing -> getRoot ev

-- XXX should we use a Maybe?
-- | Cookie is set when a rename occurs. The cookie value can be used to
-- connect the 'isMovedFrom' and 'isMovedTo' events, if both the events belong
-- to the same move operation then they will have the same cookie value.
getCookie :: Event -> Cookie
getCookie Event{..} = Cookie eventCookie

-------------------------------------------------------------------------------
-- Event types
-------------------------------------------------------------------------------

getFlag :: Word32 -> Event -> Bool
getFlag mask Event{..} = eventFlags .&. mask /= 0

-------------------------------------------------------------------------------
-- Error events
-------------------------------------------------------------------------------

-- XXX rename to isQOverflowed or hasOverflowed?
--
-- | Event queue overflowed and we may have lost some events. The user
-- application must scan everything under the watched paths to know the
-- current state.
--
-- In recursive mode, the watched directories are scanned again on an
-- overflow, the subdirectories whose create or move events were lost are
-- added to the watch. A subdirectory moved out of the tree, whose move event
-- was lost, remains watched with its old path.
isEventsLost :: Event -> Bool
isEventsLost = getFlag iN_Q_OVERFLOW

-------------------------------------------------------------------------------
-- Events affecting the watched path only
-------------------------------------------------------------------------------

-- Compare with isRootChanged on macOS. isRootChanged includes all these cases.
--
-- | A path was removed from the watch explicitly using 'removeFromWatch' or
-- automatically (file was deleted, or filesystem was unmounted).
--
-- Note that in recursive watch mode all the subdirectories are watch roots,
-- therefore, they will all generate this event.
--
-- /Occurs only for a watched path/
isRootUnwatched :: Event -> Bool
isRootUnwatched = getFlag iN_IGNORED

-- | Watched file/directory was itself deleted.  (This event also occurs if an
-- object is moved to another filesystem, since mv(1) in effect copies the file
-- to the other filesystem and then deletes it from the original filesystem.)
-- In addition, an 'isRootUnwatched' event will subsequently be generated
-- for the watch descriptor.
--
-- In recursive mode, the event is reported for the watch roots only, the
-- deletion of a subdirectory is reported as 'isDeleted'.
--
-- /Occurs only for a watched path/
isRootDeleted :: Event -> Bool
isRootDeleted = getFlag iN_DELETE_SELF

-- | Watched file/directory was itself moved within the file system.
--
-- In recursive mode, the event is reported for the watch roots only, the
-- move of a subdirectory is reported as 'isMovedFrom' and 'isMovedTo'.
--
-- /Occurs only for a watched path/
isRootMoved :: Event -> Bool
isRootMoved = getFlag iN_MOVE_SELF

foreign import capi
    "sys/inotify.h value IN_UNMOUNT" iN_UNMOUNT :: Word32

-- | Filesystem containing watched object was unmounted.  In addition, an
-- 'isRootUnwatched' event will subsequently be generated for the watch
-- descriptor.
--
-- /Occurs only for a watched path/
isRootUnmounted :: Event -> Bool
isRootUnmounted = getFlag iN_UNMOUNT

-- | Determine whether the event indicates a change of path of the monitored
-- object itself. Note that the object may become unreachable or deleted after
-- a change of path.
--
-- /Occurs only for a watched path/
isRootPathEvent :: Event -> Bool
isRootPathEvent = getFlag (iN_DELETE_SELF .|. iN_MOVE_SELF .|. iN_UNMOUNT)

-------------------------------------------------------------------------------
-- Metadata change Events
-------------------------------------------------------------------------------

-- | Determine whether the event indicates inode metadata change for an object
-- contained within the monitored path.
--
-- Metadata change may include, permissions (e.g., chmod(2)), timestamps
-- (e.g., utimensat(2)), extended attributes (setxattr(2)), link count (since
-- Linux 2.6.25; e.g., for the target of link(2) and for unlink(2)), and
-- user/group ID (e.g., chown(2)).
--
-- /Can occur for watched path or a file inside it/
isAttrsModified :: Event -> Bool
isAttrsModified = getFlag iN_ATTRIB

-------------------------------------------------------------------------------
-- Access
-------------------------------------------------------------------------------

-- | File was accessed (e.g. read, execve).
--
-- /Occurs only for a file inside the watched directory/
isAccessed :: Event -> Bool
isAccessed = getFlag iN_ACCESS

-- | File or directory was opened.
--
-- /Occurs only for a file inside the watched directory/
isOpened :: Event -> Bool
isOpened = getFlag iN_OPEN

-- | File opened for writing was closed.
--
-- /Occurs only for a file inside the watched directory/
isWriteClosed :: Event -> Bool
isWriteClosed = getFlag iN_CLOSE_WRITE

-- XXX what if it was opened for append? Does NOWRITE mean all cases where the
-- mode was not write? A dir open comes in this category?
--
-- | File or directory opened for read but not write was closed.
--
-- /Can occur for watched path or a file inside it/
isNonWriteClosed :: Event -> Bool
isNonWriteClosed = getFlag iN_CLOSE_NOWRITE

-------------------------------------------------------------------------------
-- CRUD Events
-------------------------------------------------------------------------------

-- | File/directory created in watched directory (e.g., open(2) O_CREAT,
-- mkdir(2), link(2), symlink(2), bind(2) on a UNIX domain socket).
--
-- /Occurs only for an object inside the watched directory/
isCreated :: Event -> Bool
isCreated = getFlag iN_CREATE

-- | File/directory deleted from watched directory.
--
-- /Occurs only for an object inside the watched directory/
isDeleted :: Event -> Bool
isDeleted = getFlag iN_DELETE

-- XXX what if an object is moved in from outside or moved out of the monitored
-- dir?
--
-- | Generated for the original path when an object is moved from under a
-- monitored directory.
--
-- /Occurs only for an object inside the watched directory/
isMovedFrom :: Event -> Bool
isMovedFrom = getFlag iN_MOVED_FROM

-- | Generated for the new path when an object is moved under a monitored
-- directory.
--
-- /Occurs only for an object inside the watched directory/
isMovedTo :: Event -> Bool
isMovedTo = getFlag iN_MOVED_TO

-- | Generated for a path that is moved from or moved to the monitored
-- directory.
--
-- >>> isMoved ev = isMovedFrom ev || isMovedTo ev
--
-- /Occurs only for an object inside the watched directory/
isMoved :: Event -> Bool
isMoved ev = isMovedFrom ev || isMovedTo ev

-- | Determine whether the event indicates modification of an object within the
-- monitored path. This event is generated only for files and not directories.
--
-- /Occurs only for an object inside the watched directory/
isModified :: Event -> Bool
isModified = getFlag iN_MODIFY

-------------------------------------------------------------------------------
-- Information about path type (applicable only when 'setFileEvents' is 'True')
-------------------------------------------------------------------------------

foreign import capi
    "sys/inotify.h value IN_ISDIR" iN_ISDIR :: Word32

-- | Determine whether the event is for a directory path.
isDir :: Event -> Bool
isDir = getFlag iN_ISDIR

-- | Determine whether the event was generated by a scan of a directory
-- instead of the kernel, see 'setScanNewDirs'. 'isCreated' is true for such an
-- event. Such an event says that the entry exists and was not reported by the
-- kernel before the scan.
--
-- An entry created while its directory is being scanned is reported by the
-- scan and by the kernel. The event from the scan comes first. The duplicates
-- can be removed by a scan of the output stream: remember the paths of the
-- 'isScanEvent' events, drop an 'isCreated' event for a remembered path, and
-- forget a path on its 'isDeleted' or 'isMovedFrom' event.
isScanEvent :: Event -> Bool
isScanEvent = eventScanned

-------------------------------------------------------------------------------
-- Debugging
-------------------------------------------------------------------------------

-- | Convert an 'Event' record to a String representation.
showEvent :: Event -> String
showEvent ev@Event{..} =
       "--------------------------"
    ++ "\nWd = " ++ show eventWd
    ++ "\nRoot = " ++ show (Path.toString $ getRoot ev)
    ++ "\nPath = " ++ maybe "" (show . Path.toString) (getRelPath ev)
    ++ "\nCookie = " ++ show (getCookie ev)
    ++ "\nFlags " ++ show eventFlags

    ++ showev isEventsLost "Overflow"

    ++ showev isRootUnwatched "RootUnwatched"
    ++ showev isRootDeleted "RootDeleted"
    ++ showev isRootMoved "RootMoved"
    ++ showev isRootUnmounted "RootUnmounted"

    ++ showev isAttrsModified "AttrsModified"

    ++ showev isAccessed "Accessed"
    ++ showev isOpened "Opened"
    ++ showev isWriteClosed "WriteClosed"
    ++ showev isNonWriteClosed "NonWriteClosed"

    ++ showev isCreated "Created"
    ++ showev isDeleted "Deleted"
    ++ showev isModified "Modified"
    ++ showev isMovedFrom "MovedFrom"
    ++ showev isMovedTo "MovedTo"

    ++ showev isDir "Dir"
    ++ showev isScanEvent "Scanned"
    ++ "\n"

        where showev f str = if f ev then "\n" ++ str else ""
