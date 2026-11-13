-- |
-- Module      : Streamly.Internal.FS.Event.Linux
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

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
    , readEvent
    , eventStream
    , addRoot
    , removeRoot

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
    , isAccessDenied

    -- * Debugging
    , showEvent
    )
where

import Control.Concurrent (rtsSupportsBoundThreads, threadWaitRead)
import Control.Concurrent.MVar
    ( MVar, modifyMVar, modifyMVar_, newMVar, putMVar, readMVar, takeMVar
    , withMVar
    )
import Control.Exception
    (SomeException, bracket, finally, onException, throwIO, try)
import Control.Monad (mapAndUnzipM, void, when)
import Data.Bits ((.|.), (.&.), complement)
import Data.Char (ord)
import Data.Functor.Identity (runIdentity)
import Data.IntMap.Lazy (IntMap)
import Data.IntSet (IntSet)
import Data.IORef
    (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.List (find, isPrefixOf)
#if !(MIN_VERSION_base(4,20,0))
import Data.List (foldl')
#endif
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe, isJust, isNothing)
#if !(MIN_VERSION_base(4,13,0))
import Data.Semigroup (Semigroup(..))
#endif
import Data.Word (Word8, Word32)
import Foreign.C.Error
    ( Errno(..), eACCES, eAGAIN, eINTR, eINVAL, eLOOP, eNOENT, eNOTDIR, ePERM
    , eWOULDBLOCK, errnoToIOError, getErrno, throwErrno, throwErrnoIfMinus1
    , throwErrnoIfMinus1_
    )
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt(..), CSize(..), CUInt(..))
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek, peekByteOff, peekElemOff, sizeOf)
import GHC.Conc
    ( TVar, atomically, closeFdWith, newTVarIO, orElse, readTVar
    , retry, threadWaitReadSTM, writeTVar
    )
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

import qualified Control.Exception as Exception
    (interruptible, mask, mask_, uninterruptibleMask_)
import qualified Data.IntMap.Lazy as Map
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as OrdMap
import qualified Streamly.Data.Array as A
    (createOf, fromList, read, toList)
import qualified Streamly.Data.Stream as S
import qualified Streamly.Data.StreamK as StreamK
    (append, fromStream, toStream)
import qualified Streamly.Internal.FileSystem.Path as Path

import qualified Streamly.Internal.Data.Array as A
    ( asCStringUnsafe, breakEndBy_, unsafePinnedAsPtr
    )
import qualified Streamly.Internal.Data.MutArray as MArray
    (byteLength, unsafeCreateWithPtr')
import qualified Streamly.Internal.FileSystem.DirIO as Dir (ignoreMissing)
import qualified Streamly.Internal.FileSystem.Posix.ReadDir as ReadDir
    (DirStream(..), closeDirStream, readDirStreamEither)
import qualified Streamly.Internal.Data.Parser as PR
    (takeEQ, fromEffect)
import qualified Streamly.Internal.Data.StreamK as StreamK
    (concatIterateWith)

-------------------------------------------------------------------------------
-- Subscription to events
-------------------------------------------------------------------------------

-- | Watch configuration, used to specify the events of interest and the
-- behavior of the watch.
data Config = Config
    { watchRec :: Bool
    , scanNewDirs :: Bool
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

-- | Watch the whole directory tree recursively instead of watching just the
-- immediate children of the directory.
--
-- A symbolic link inside the tree is not followed, whatever the value of
-- 'setFollowSymLinks'.
--
-- /Created in the tree/: A directory created in the tree is added to the watch
-- when its create event is processed, which is some time after the directory
-- was created. The directory is then scanned for entries created in that gap,
-- and each entry found under it is reported as an 'isCreated' event with
-- 'isScanEvent' set, after the 'isCreated' event of the directory. The scan
-- is controlled by the 'setScanNewDirs' config option.
--
-- /Moved into the tree/: A directory moved into the tree from outside
-- reports 'isMovedTo'. It is added to the watch and scanned in the same way
-- as a directory created in the tree, the entries found by the scan are
-- reported after the 'isMovedTo' event.
--
-- /Moved out of the tree/: A directory moved out of the tree reports
-- 'isMovedFrom'. The directory and the subdirectories under it are removed
-- from the watch, and each of them reports 'isRootUnwatched' under its old
-- path. Until then, the events of the moved out tree are reported under the
-- old path. The kernel queues an @IN_IGNORED@ event for each removed watch,
-- so moving out a tree of more than
-- @\/proc\/sys\/fs\/inotify\/max_queued_events@ directories, 16384 by
-- default, overflows the queue and reports 'isEventsLost'.
--
-- /Renames within the tree/: A directory renamed within the tree reports
-- 'isMovedFrom' for the old path and 'isMovedTo' for the new path, and the
-- new path is scanned in the same way as a directory moved in: the scan
-- reports all the entries under the new path, and adds to the watch any
-- subdirectory that is not watched yet. A consumer that removes the old paths
-- on 'isMovedFrom' and adds the newly reported entries gets the tree at its
-- new path, as for a move out followed by a move in.
--
-- The watches of the renamed tree are kept if the new path still refers to the
-- watched directory when the 'isMovedTo' event is processed, and
-- 'isRootUnwatched' is not reported. The events generated before the rename
-- report the old path, the events generated after the rename report the new
-- path. No event is missed, except the open, access and close events
-- blocked during the scan.
--
-- If the new path no longer refers to the watched directory when the
-- 'isMovedTo' event is processed, for example, if the directory was renamed
-- again, or if the new path now refers to another directory, the watches
-- under the old path are removed and report 'isRootUnwatched'. The new path
-- is then added in the same way as a directory moved in. A deletion or a
-- modification made in the directory before the new watch is added is not
-- reported, or is reported under the old path.
--
-- /Races in create processing/: If a newly created directory was replaced by
-- the time its create or move event is processed, the directory that replaced
-- it is added to the watch. If the replacing directory was already watched,
-- its events may be reported under both the old and the new path until the
-- move event is processed. For example, in @mkdir d; mv d e; mv f d@ with
-- @f@ watched, the events of the directory @f@ are reported under @f@ and @d@
-- until the 'isMovedTo' event of @e@ is processed.
--
-- /Reporting during the scan/: The open, access and close events of the
-- directories being scanned are not reported until the directory and the
-- tree under it have been processed. Otherwise the events generated by the
-- directory reads could overflow the kernel event queue.
--
-- The tree is watched in this way whether or not 'setCreated', 'setMovedFrom',
-- 'setMovedTo' and 'setRootMoved' are enabled, these settings control only the
-- report of their events.
--
-- Each directory in the tree uses one inotify watch. If the number of watches
-- exceeds the limit in @\/proc\/sys\/fs\/inotify\/max_user_watches@, the watch
-- API or the event stream raises an exception.
--
-- /default: False/
setRecursiveMode :: Bool -> Config -> Config
setRecursiveMode recursive cfg@Config{} = cfg {watchRec = recursive}

-- | In recursive mode, a scan reports (1) the create events missed in the gap
-- after a directory is created and before its watch is added, (2) the
-- pre-existing entries in a directory moved into the watched tree, (3) the
-- entries that may be missed when a directory is renamed, by reporting all
-- the entries under its new path.
--
-- /Newly created directories/: A directory created in the watched tree is not
-- watched until after its create event is processed. Without a scan the
-- entries created in it by then are not reported. With the scan enabled the
-- directory tree is scanned right after its watch is added. Each entry found
-- in it is emitted as an 'isCreated' event with 'isScanEvent' true. These
-- events are emitted after the create event of the directory. For example,
-- @mkdir -p a\/b; touch a\/b\/f@ reports @a@, @a\/b@ and @a\/b\/f@.
--
-- /Directories moved from outside/: A directory that is moved from outside the
-- watched tree is scanned, and its pre-existing entries are emitted as
-- 'isCreated' events after its 'isMovedTo' event. For example, @mv
-- \/outside\/d d@ reports the move of @d@ and @d\/f@.
--
-- /Renamed directories/: It is not always possible to distinguish an in-tree
-- rename from a directory that was moved from outside, or from a directory
-- created and renamed before its create event is read. Therefore, every
-- rename of a directory is reported as the tree leaving and coming back in.
-- On 'isMovedFrom' the consumer removes the directory and every path under
-- it.
-- The directory is scanned at its new path, and all its entries are emitted as
-- 'isCreated' events after its 'isMovedTo' event. For example, @mv d e@
-- reports the move of @d@ to @e@ and then @e\/f@. A rename that does not drop
-- the watch does not miss any event, the entries are only reported again.
--
-- /Loss of pre-scan non-create events:/ The scan reports 'isCreated' for all
-- the entries that were created before the watch was added. These entries may
-- have undergone modifications or other operations before the scan got to
-- them, such events are just lost.
--
-- /Duplicate events/: An entry created after the watch is added and before
-- the scan reads it is reported twice, once as a normal event and once as an
-- 'isScanEvent'. See 'isScanEvent' for removing the duplicates.
--
-- /Cost of the scan/: The scan reads the tree as the stream reads the entries
-- it reports, the kernel event queue is not read until all the entries of the
-- scan have been read. The entries are not collected in memory, the memory
-- used by the scan is proportional to the depth of the tree. A rename of a
-- large tree reports all the entries of the tree.
--
-- A directory is watched before its 'isCreated' or 'isMovedTo' event is
-- delivered, whether the event is generated by the kernel or by the scan. The
-- open, access and close events of the directories of the tree, generated
-- before the stream has read all the entries of the scan, are not reported.
--
-- /Effect of disabling the scan/: Disabling this setting does not avoid
-- reading the tree. Adding the watches of a new, moved or renamed tree reads
-- all its directories in either case. It saves the events reported. The
-- program then has to scan a directory on its 'isCreated' and 'isMovedTo'
-- events itself, the watch of the directory is added before these events are
-- delivered and the scan misses no entry. It removes the directory and every
-- path under it on 'isMovedFrom', as with the scan enabled, and applies an
-- entry found by its scan as an update.
--
-- /default: True/
setScanNewDirs :: Bool -> Config -> Config
setScanNewDirs scan cfg@Config{} = cfg {scanNewDirs = scan}

-- XXX we should give priority to the kernel event queue processing to avoid an
-- overflow. The scans should be working in separate threads to not block the
-- queue processing. This is especially true for the move-in/rename scans. New
-- create scans can possibly run in the same thread.

foreign import capi
    "sys/inotify.h value IN_DONT_FOLLOW" iN_DONT_FOLLOW :: Word32

-- | If the pathname to be watched is a symbolic link then watch the target of
-- the symbolic link instead of the symbolic link itself. Applies to the watch
-- roots only. When this is 'False', in recursive mode the directory tree of
-- the target is not watched.
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

-- | Report when the watched root path itself gets renamed. A renamed root is
-- removed from the watch whether or not this is enabled, see 'isRootMoved'.
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
-- In recursive mode, this also enables the report of the entries found by a
-- scan of a directory created in the tree or moved into it, see
-- 'setScanNewDirs'. When it is disabled these entries are not reported.
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
-- In recursive mode, a directory that arrives by a move is watched and scanned
-- whether this event is enabled or not, see 'setRecursiveMode' and
-- 'setScanNewDirs'.
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
        , createFlags = 0
        }

-- | The events that the kernel watch must generate in both recursive and
-- non-recursive mode. A root that reports IN_MOVE_SELF is removed from the
-- watch if its path no longer refers to the watched object, see
-- 'unwatchGoneRoots'.
rootEvents :: Word32
rootEvents = iN_MOVE_SELF

-- | The events that the kernel watch must generate in recursive mode, to add
-- and remove the watches of the subdirectories.
recursiveEvents :: Word32
recursiveEvents =
    iN_CREATE .|. iN_MOVED_FROM .|. iN_MOVED_TO .|. rootEvents

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
    else flags .|. rootEvents

    where

    flags = createFlags .|. iN_MASK_ADD

-- | The flags of the kernel watch while a directory and the tree under it are
-- being listed in recursive mode.
--
-- Listing a directory generates open, access and close events on its watch
-- and on the watch of its parent, 8 events for an empty directory. The kernel
-- queue is not read during the scan, on a large tree these events overflow
-- the queue. These events are requested after the directory and the tree
-- under it have been listed, see 'addRoot' and 'scanTree'. The scan of a
-- new directory lists the tree as the stream reads its entries, the period
-- lasts until the stream has read the scan of the tree. The same events
-- generated by another process in that period are not reported, all other
-- events are.
--
-- A directory that is already watched is listed too, its mask is replaced
-- with these flags during the listing, see 'rescanDir'. When the directory is
-- also watched via another root, for example, a root under another root, the
-- roots share its kernel watch. The other root then does not report these
-- events of another process during the scan either.
--
-- A directory needs these flags only until it and its immediate
-- subdirectories are listed, a listing of a deeper directory does not
-- generate events on its watch. The walk could list one level of the tree at
-- a time and request the events of a directory after the next level is
-- listed. This shortens the period in which the events of another process
-- are not reported, but it needs the subdirectories of a level to be kept
-- until the next level is listed, and it changes the order of the scan
-- events. It is not done, the depth first walk is simpler.
--
-- The watch of the parent of the top directory is not changed, it can report
-- the activity of its other entries. It reports the listing of the top
-- directory.
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

-- | @isInSubtree dir pRep@ is 'True' if @pRep@ is the subdirectory @dir@ of a
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
    { wpRep :: !PathRep
    -- | 'removeRoot' was called for the root, the path reports the
    -- events until its 'isRootUnwatched' event.
    , wpRemoved :: !Bool
    -- | The cookie of the IN_MOVED_FROM event read for the path, if its
    -- IN_MOVED_TO or IN_MOVE_SELF event has not been read yet.
    , wpMoveCookie :: !(Maybe Word32)
    -- | The path was set by an IN_MOVED_TO event whose new path did not exist
    -- when the event was read, and is not confirmed yet. See the NOTE above
    -- 'renameDir'.
    , wpPending :: !Bool
    }

-- | A path with the watch descriptor of its directory.
type WdPath = (Int, PathRep)

-- | A path marked removed or removed from the watch, with its user watch
-- descriptor, to be reported as 'isRootUnwatched'. The watch descriptor is
-- not removed if other paths share it.
type WdUnwatched = (Int, PathRep)

-- | The map entry of a watch descriptor. A user watch descriptor and a
-- marker watch descriptor are different watch descriptors, see
-- 'addRemovalMarker' for markers.
data WdEntry =
    -- | Entry for a user watch descriptor: the paths watched via it, in the
    -- order added.
      WdUser [WatchPath]
    -- | Entry for a marker watch descriptor: the removed paths, to be
    -- reported 'isRootUnwatched' when its IN_IGNORED event is read.
    | WdMarker [WdUnwatched]

-- | Build a 'WdUser' entry with its paths evaluated. An unevaluated path
-- would keep the old entry, its paths and their bytes alive until the entry
-- is used.
wdUser :: [WatchPath] -> WdEntry
wdUser wps = foldr (seq . forcePaths) () wps `seq` WdUser wps

    where

    forcePaths WatchPath{wpRep = (root, sub)} =
        root `seq` maybe () (`seq` ()) sub

-- | Compare the subdirectory paths of two 'PathRep's bytewise, with the path
-- separator ordered before any other byte. A directory is followed by the
-- paths under it, before any other path. In the plain bytewise order
-- @a\/b-c@ comes between @a\/b@ and @a\/b\/c@. A root is ordered before
-- the subdirectories.
compareSub :: Maybe Path -> Maybe Path -> Ordering
compareSub Nothing Nothing = EQ
compareSub Nothing (Just _) = LT
compareSub (Just _) Nothing = GT
compareSub (Just a) (Just b) =
    runIdentity $ S.cmpBy compare (bytes a) (bytes b)

    where

    sep = fromIntegral (ord '/') :: Word8

    -- A file name does not contain the byte 0.
    rank w = if w == sep then 0 else w

    bytes = fmap rank . A.read . Path.toArray

-- | The key of a path in the path index of a 'WdMap'. The keys are ordered
-- by the subdirectory path, see 'compareSub', and then by the root. The paths
-- of a tree are adjacent in the index, together with the paths of the trees
-- of the same subdirectory path in other roots. The roots are compared last,
-- most of the paths of a watch have the same root.
newtype PathKey = PathKey PathRep

-- The comparison must not be inlined or split into a worker that takes the
-- fields of the 'PathRep'. The insert of the map would then take the key
-- unboxed and allocate a new tuple to store it, 24 bytes for each path,
-- instead of sharing the 'PathRep' of the 'WatchPath'.
#if __GLASGOW_HASKELL__ >= 904
{-# OPAQUE comparePathKey #-}
#else
{-# NOINLINE comparePathKey #-}
#endif
comparePathKey :: PathKey -> PathKey -> Ordering
comparePathKey (PathKey (r1, s1)) (PathKey (r2, s2)) =
    compareSub s1 s2 <> compare (Path.toArray r1) (Path.toArray r2)

instance Eq PathKey where
    a == b = comparePathKey a b == EQ

instance Ord PathKey where
    compare = comparePathKey

-- | The map of the watch descriptors of a watch, and the indexes derived
-- from it. Only 'insertWdUser', 'insertMarker' and 'deleteWd' change the map,
-- they keep the indexes up to date.
--
-- A directory event is handled in time proportional to the number of paths
-- it changes, using the indexes. Without them a rename or a removal of a
-- directory visited every watch descriptor, a watch of 24000 directories
-- handled 74 directory renames in a second.
data WdMap = WdMap
    { -- | Map key is the watch descriptor.
      wmEntries :: !(IntMap WdEntry)
      -- | The watch descriptors that have a given path, removed or not.
    , wmPaths :: !(OrdMap.Map PathKey IntSet)
      -- | The watch descriptors that have a path with a cookie, see
      -- 'markMoved'.
    , wmMoved :: !IntSet
      -- | The watch descriptors that have a pending path, see
      -- 'markPending'.
    , wmPending :: !IntSet
    }

emptyWdMap :: WdMap
emptyWdMap = WdMap Map.empty OrdMap.empty IntSet.empty IntSet.empty

lookupWd :: Int -> WdMap -> Maybe WdEntry
lookupWd wd = Map.lookup wd . wmEntries

-- | The entries in the ascending order of watch descriptors.
wdEntries :: WdMap -> [(Int, WdEntry)]
wdEntries = Map.toList . wmEntries

-- | All the watch descriptors, in the ascending order.
allWds :: WdMap -> [Int]
allWds = Map.keys . wmEntries

-- | The paths of a user watch descriptor. Empty for a marker, and for a
-- watch descriptor that is not in the map.
wdUserPaths :: Int -> WdMap -> [WatchPath]
wdUserPaths wd m =
    case lookupWd wd m of
        Just (WdUser wps) -> wps
        _ -> []

-- | Remove a watch descriptor from the index entries of the given paths.
unindexPaths ::
       Int
    -> [WatchPath]
    -> OrdMap.Map PathKey IntSet
    -> OrdMap.Map PathKey IntSet
unindexPaths wd wps index = foldl' remove index wps

    where

    remove ix wp = OrdMap.update without (PathKey (wpRep wp)) ix

    without wds =
        let wds1 = IntSet.delete wd wds
         in if IntSet.null wds1 then Nothing else Just wds1

-- | Add a watch descriptor to the index entries of the given paths. The key
-- of a new index entry is the 'PathRep' of the path itself, the index does
-- not hold a copy of the path.
indexPaths ::
       Int
    -> [WatchPath]
    -> OrdMap.Map PathKey IntSet
    -> OrdMap.Map PathKey IntSet
indexPaths wd wps index = foldl' add index wps

    where

    add ix wp =
        OrdMap.insertWith
            IntSet.union (PathKey (wpRep wp)) (IntSet.singleton wd) ix

-- | Add the entry of a user watch descriptor with the given paths, replacing
-- its existing entry if any, see 'WdUser'.
insertWdUser :: Int -> [WatchPath] -> WdMap -> WdMap
insertWdUser wd wps m =
    let !entry = wdUser wps
     in WdMap
            { wmEntries = Map.insert wd entry (wmEntries m)
            , wmPaths =
                indexPaths wd wps
                    $ unindexPaths wd (wdUserPaths wd m) (wmPaths m)
            , wmMoved = member (any (isJust . wpMoveCookie) wps) (wmMoved m)
            , wmPending = member (any wpPending wps) (wmPending m)
            }

    where

    member c = if c then IntSet.insert wd else IntSet.delete wd

-- | Remove a watch descriptor from the map.
deleteWd :: Int -> WdMap -> WdMap
deleteWd wd m =
    WdMap
        { wmEntries = Map.delete wd (wmEntries m)
        , wmPaths = unindexPaths wd (wdUserPaths wd m) (wmPaths m)
        , wmMoved = IntSet.delete wd (wmMoved m)
        , wmPending = IntSet.delete wd (wmPending m)
        }

-- | Add the entry of a marker watch descriptor, see 'WdMarker'.
insertMarker :: Int -> [WdUnwatched] -> WdMap -> WdMap
insertMarker wd paths m =
    let m1 = deleteWd wd m
     in m1 {wmEntries = Map.insert wd (WdMarker paths) (wmEntries m1)}

-- | Change the paths of the given user watch descriptors. The function
-- returns 'Nothing' if the paths of a watch descriptor do not change.
adjustWds :: [Int] -> ([WatchPath] -> Maybe [WatchPath]) -> WdMap -> WdMap
adjustWds wds f m0 = foldl' step m0 wds

    where

    step m wd =
        case lookupWd wd m of
            Just (WdUser wps) | Just wps1 <- f wps -> insertWdUser wd wps1 m
            _ -> m

-- | The watch descriptors that have the given path, removed or not, in the
-- ascending order.
pathWds :: PathRep -> WdMap -> [Int]
pathWds pRep =
    foldMap IntSet.toAscList . OrdMap.lookup (PathKey pRep) . wmPaths

-- | The watch descriptors that have the given subdirectory of a root or a
-- path under it, removed or not, in the ascending order. See 'isInSubtree'.
-- The watch descriptors of the same subdirectory path in other roots are
-- included, the caller selects the paths with 'isInSubtree'.
subtreeWds :: PathRep -> WdMap -> [Int]
subtreeWds (_, Just sub) m =
    IntSet.toAscList
        $ IntSet.unions
        $ OrdMap.elems
        $ OrdMap.takeWhileAntitone (isBefore end)
        $ OrdMap.dropWhileAntitone (isBefore (Just sub)) (wmPaths m)

    where

    isBefore s (PathKey (_, s1)) = compareSub s1 s == LT

    -- The byte 1 is ordered after the path separator and not after any
    -- other byte of a name, see 'compareSub'. The paths under @sub@ are
    -- ordered before this path, all other paths ordered after @sub@ are
    -- not.
    end = Just (Path.unsafeFromArray (Path.toArray sub <> A.fromList [1]))
subtreeWds _ _ = []

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

-- | A synthetic event, generated by the library instead of read from the fd,
-- by a call outside the event stream. It is reported before the events of
-- the next read of the fd. The event stream reports the synthetic events of
-- the kernel events it handles itself.
data SynthEvent =
    -- | A subdirectory not added because of a permission error, reported as
    -- 'isAccessDenied'.
      SynthDenied PathRep
    -- | A path removed without a marker, as (user watch descriptor, path),
    -- reported as 'isRootUnwatched'. See 'addRemovalMarker'.
    | SynthUnwatched Int PathRep

-- | The state of the fd of a watch.
data FdState = FdState
    { fsWatchClosed :: !Bool -- ^ 'closeWatch' was called
    , fsFdOpen :: !Bool -- ^ The fd is not closed yet
    -- | The threads waiting for the fd to become readable, counted in the
    -- non-threaded RTS only.
    , fsFdWaiters :: !Int
    }

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

-- | An item of the event stream kept in a 'Watch', see 'eventStream'.
data StreamItem =
      ItemEvent Event
      -- | No event is available. The action waits for an event. It is run
      -- after the rest of the stream is saved in the 'Watch', an async
      -- exception that interrupts the wait does not change the stream.
    | ItemWait (IO ())
      -- | An operation of the event stream failed. The exception is raised
      -- to the reader after the rest of the stream is saved in the 'Watch'.
    | ItemFailed IOException
      -- | The stream ends, the items after it are not reported.
    | ItemEnd

-- | A handle for a watch.
--
data Watch = Watch
    { watchConfig :: Config -- ^ The config used to open the watch
    , watchFd :: CInt -- ^ The inotify fd
      -- | Held during a system call on the fd, 'closeWatch' cannot close the
      -- fd, and the fd number cannot be reused, while the system call is in
      -- progress. The system calls made with the lock held must not block.
    , watchLock :: MVar FdState
    , watchWdMap :: IORef WdMap
      -- | The rest of the event stream of 'eventStream': the events
      -- read from the fd but not yet returned to the application, and the
      -- entries of a new directory scan not yet returned, followed by the
      -- reads of the fd still to be made. 'Nothing' until the first event
      -- is taken. A stream that stops reading leaves the rest here, the
      -- next stream on the watch continues from it.
    , watchUndelivered :: MVar (Maybe (Stream IO StreamItem))
      -- | The synthetic events, most recent first. A thread waiting for
      -- events in the threaded RTS is woken up when one is added.
    , watchSynthEvents :: TVar [SynthEvent]
    }

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

-- | Create a 'Watch' handle. 'addRoot' can be used to add paths being
-- monitored by this watch.
--
-- /Pre-release/
--
createWatch :: Config -> IO Watch
createWatch cfg = do
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
    emptyMapRef <- newIORef emptyWdMap
    undeliveredRef <- newMVar Nothing
    synthVar <- newTVarIO []
    return
        $ Watch
            { watchConfig = cfg
            , watchFd = fd
            , watchLock = lock
            , watchWdMap = emptyMapRef
            , watchUndelivered = undeliveredRef
            , watchSynthEvents = synthVar
            }

-- | Run an action on the fd of a watch, holding the lock of the watch. The
-- action must not block. Throw an exception if the watch is closed. The
-- string is the name of the operation, used in the exception message.
withWatchFd :: String -> Watch -> (CInt -> IO a) -> IO a
withWatchFd op Watch {watchFd = fd, watchLock = lock} action =
    withMVar lock $ \st ->
        if fsWatchClosed st
        then ioError
                $ mkIOError
                    illegalOperationErrorType
                    (op ++ ": the watch is closed")
                    Nothing
                    Nothing
        else action fd

-- | Run an action holding the lock of the watch, return the default if the
-- watch is closed. The action must not block. See 'withWatchFd'.
whenWatchOpen :: Watch -> a -> IO a -> IO a
whenWatchOpen Watch {watchLock = lock} def action =
    withMVar lock $ \st -> if fsWatchClosed st then return def else action

isWatchClosed :: Watch -> IO Bool
isWatchClosed Watch {watchLock = lock} = fsWatchClosed <$> readMVar lock

-- | Record synthetic events, oldest first.
recordSynthEvents :: Watch -> [SynthEvent] -> IO ()
recordSynthEvents Watch {watchSynthEvents = synthVar} new =
    atomically $ do
        ps <- readTVar synthVar
        writeTVar synthVar (reverse new ++ ps)

-- | Take the events recorded by 'recordSynthEvents', oldest first.
takeSynthEvents :: Watch -> IO [SynthEvent]
takeSynthEvents Watch {watchSynthEvents = synthVar} =
    atomically $ do
        ps <- readTVar synthVar
        writeTVar synthVar []
        return (reverse ps)

foreign import ccall unsafe
    "sys/inotify.h inotify_add_watch" c_inotify_add_watch
        :: CInt -> CString -> CUInt -> IO CInt

foreign import capi "fcntl.h openat" c_openat3
    :: CInt -> CString -> CInt -> IO CInt

foreign import capi "fcntl.h value AT_FDCWD" aT_FDCWD :: CInt

foreign import capi "fcntl.h value O_PATH" o_PATH :: CInt

foreign import capi "fcntl.h value O_RDONLY" o_RDONLY :: CInt

foreign import capi "fcntl.h value O_DIRECTORY" o_DIRECTORY :: CInt

foreign import capi "fcntl.h value O_NOFOLLOW" o_NOFOLLOW :: CInt

foreign import ccall unsafe "unistd.h close" c_close :: CInt -> IO CInt

foreign import ccall unsafe "dirent.h fdopendir"
    c_fdopendir :: CInt -> IO (Ptr ())

-- | Whether adding or listing a subdirectory failed because the path no longer
-- refers to a directory. The path may have been deleted, or it may now be a
-- file or a symbolic link. ELOOP is a symbolic link loop in place of the
-- directory or of its parent.
isNotSubdirError :: IOError -> Bool
isNotSubdirError e =
    isDoesNotExistError e || fmap Errno (ioe_errno e) `elem` errnos

    where

    errnos = [Just eNOTDIR, Just eLOOP]

-- | Whether adding or listing a directory failed because of missing
-- permission. inotify_add_watch needs the read permission, listing the
-- directory needs the read and the search permissions.
isAccessError :: IOError -> Bool
isAccessError e = fmap Errno (ioe_errno e) `elem` [Just eACCES, Just ePERM]

-- | The components of a subdirectory path.
subComponents :: Path -> [Array Word8]
subComponents =
    map Path.toArray . runIdentity . S.toList . Path.splitPath_

-- | Open an O_PATH fd of a subdirectory, starting from its root and without
-- following a symbolic link in any component under the root. Raise an
-- exception if a component is not a directory.
openSubdir :: Path -> Path -> IO CInt
openSubdir root sub = do
    fd <-
        A.asCStringUnsafe (Path.toArray root) $ \ptr ->
            throwErrnoIfMinus1 err
                $ c_openat3 aT_FDCWD ptr (dirFlags .|. o_CLOEXEC)
    go fd (subComponents sub)

    where

    err = "add watch: " ++ Path.toString (Path.join root sub)
    dirFlags = o_PATH .|. o_DIRECTORY
    subFlags = dirFlags .|. o_NOFOLLOW .|. o_CLOEXEC

    go fd [] = return fd
    go fd (x : xs) = do
        next <- A.asCStringUnsafe x $ \ptr -> c_openat3 fd ptr subFlags
        errno <- getErrno
        _ <- c_close fd
        if next == -1
        then ioError (errnoToIOError err errno Nothing Nothing)
        else go next xs

-- | The O_PATH fd of an open directory and the name of an entry in it. A
-- walk of a tree opens a subdirectory relative to the fd of its parent, one
-- openat call, instead of opening every component from the root.
type ParentDir = (CInt, Path)

-- | Open the directory of an @O_PATH@ fd again, for reading.
openDot :: PathRep -> CInt -> IO CInt
openDot pRep pfd =
    withCString "." $ \ptr ->
        throwErrnoIfMinus1
            ("add watch: " ++ Path.toString (combinePathRep pRep))
            $ c_openat3 pfd ptr (o_RDONLY .|. o_DIRECTORY .|. o_CLOEXEC)

-- | Open a directory with the access mode @O_PATH@ or @O_RDONLY@, without
-- following a symbolic link in any component under its root. With a parent,
-- the directory is opened relative to the parent fd, the parent was opened
-- the same way. A root is opened by its path. A symbolic link to a root is
-- followed, unless the inotify flags of the watch have IN_DONT_FOLLOW: the
-- watch is then on the link itself, and its target must not be listed. Raise
-- an exception if the path is not a directory.
--
-- The parent fd refers to the directory, not to its path. If the parent is
-- moved out of the tree during the walk, its subdirectories are added under
-- the old path, and the move event of the parent removes them when it is
-- read. Without the parent fd, opening the old path fails and they are not
-- added.
--
-- 'openSubdir' opens the components of a path with @O_PATH@, which needs only
-- the search permission. With @O_RDONLY@ the directory is then opened again
-- as @.@ relative to the @O_PATH@ fd.
openDirAt :: CInt -> Word32 -> Maybe ParentDir -> PathRep -> IO CInt
openDirAt mode flags parent pRep =
    case (parent, pRep) of
        (Just (pfd, name), (_, Just _)) ->
            A.asCStringUnsafe (Path.toArray name) $ \ptr ->
                throwErrnoIfMinus1 err $ c_openat3 pfd ptr subFlags
        (_, (root, Just sub))
            | mode == o_PATH -> openSubdir root sub
            | otherwise -> bracket (openSubdir root sub) c_close (openDot pRep)
        (_, (root, Nothing)) ->
            A.asCStringUnsafe (Path.toArray root) $ \ptr ->
                throwErrnoIfMinus1 err $ c_openat3 aT_FDCWD ptr rootFlags

    where

    err = "add watch: " ++ Path.toString (combinePathRep pRep)
    dirFlags = mode .|. o_DIRECTORY
    subFlags = dirFlags .|. o_NOFOLLOW .|. o_CLOEXEC
    rootFlags =
        if flags .&. iN_DONT_FOLLOW /= 0
        then subFlags
        else dirFlags .|. o_CLOEXEC

-- | Open a directory for reading using the given action, and return its fd
-- and a directory stream on the fd. The directory is listed through the fd,
-- not by its path, a symbolic link that replaces it or a parent later is not
-- followed. Closing the directory stream closes the fd.
--
-- Opening a directory for reading generates @IN_OPEN@ on its watch and on the
-- watch of its parent, and closing it @IN_CLOSE_NOWRITE@. These watches must
-- have 'scanFlags' from before the open until after the close.
openDirStream :: PathRep -> IO CInt -> IO (CInt, ReadDir.DirStream)
openDirStream pRep open = do
    fd <- open
    dirp <- c_fdopendir fd
    if dirp == nullPtr
    then do
        errno <- getErrno
        _ <- c_close fd
        ioError
            $ errnoToIOError
                ("fdopendir: " ++ Path.toString (combinePathRep pRep))
                errno Nothing Nothing
    else return (fd, ReadDir.DirStream (castPtr dirp))

-- | Open a directory for reading, see 'openDirAt' and 'openDirStream'.
openDirStreamAt ::
    Word32 -> Maybe ParentDir -> PathRep -> IO (CInt, ReadDir.DirStream)
openDirStreamAt flags parent pRep =
    openDirStream pRep (openDirAt o_RDONLY flags parent pRep)

-- | The entries of a directory stream, directories on the 'Left'. An entry
-- deleted after it is read is skipped.
readEntries :: PathRep -> ReadDir.DirStream -> Stream IO (Either Path Path)
readEntries pRep ds = S.unfoldrM step ()

    where

    step () =
        fmap (\e -> (e, ()))
            <$> ReadDir.readDirStreamEither (Dir.ignoreMissing True)
                    (combinePathRep pRep, ds)

-- | Call inotify_add_watch on the path of a 'PathRep'.
addWatchByPath :: CInt -> Word32 -> PathRep -> IO CInt
addWatchByPath fd flags pRep =
    A.asCStringUnsafe (Path.toArray absPath) $ \pathPtr ->
        throwErrnoIfMinus1 ("add watch: " ++ Path.toString absPath)
            $ c_inotify_add_watch fd pathPtr (CUInt flags)

    where

    absPath = combinePathRep pRep

foreign import ccall unsafe "unistd.h readlink"
    c_readlink :: CString -> CString -> CSize -> IO CSsize

-- | Whether a @\/proc\/self\/fd\/N@ link can be read. Reading the link needs
-- access to @\/proc@, it does not need any permission on the file that the
-- fd refers to.
isProcLinkReadable :: String -> IO Bool
isProcLinkReadable path =
    withCString path $ \pathPtr ->
        allocaArray 1 $ \buf -> (/= -1) <$> c_readlink pathPtr buf 1

-- | Call inotify_add_watch on a subdirectory opened by 'openDirAt', using its
-- fd. See 'addWatchFor'.
addWatchViaFd :: CInt -> Word32 -> PathRep -> CInt -> IO CInt
addWatchViaFd fd flags pRep dirFd = do
    -- The watch is added through the fd of the directory, not its path, so
    -- a symbolic link in the path is not followed. The directory has been
    -- opened by 'openDirAt' without following symbolic links. The link
    -- /proc/self/fd/N itself must be followed to reach the directory.
    wd <-
        withCString procPath $ \pathPtr ->
            c_inotify_add_watch fd pathPtr
                (CUInt (flags .&. complement iN_DONT_FOLLOW))
    errno <- getErrno
    if wd /= -1
    then return wd
    -- ENOENT: /proc is not mounted. EACCES: access to /proc is denied, for
    -- example, by a security policy, or the directory has no read
    -- permission. The watch is added by path only if the /proc link cannot
    -- be read. If EACCES always fell back to the path, any user who can
    -- write to a directory in the tree could reach the fallback by creating
    -- a directory without read permission, and replace its parent before
    -- the call: @mkdir -m 000 u/a/secret; mv u/a u/b; ln -s /outside u/a@
    -- would watch @/outside/secret@.
    else if errno == eNOENT || errno == eACCES
    then do
        hasProc <- isProcLinkReadable procPath
        if hasProc
        then failWith errno
        -- The flaw of adding by path: inotify_add_watch resolves the path
        -- again and follows a symbolic link in any component but the last.
        -- A parent replaced by a link since the directory was opened
        -- redirects the watch, possibly to a directory outside the tree.
        else addWatchByPath fd flags pRep
    else failWith errno

    where

    procPath = "/proc/self/fd/" ++ show dirFd

    failWith errno =
        ioError
            $ errnoToIOError
                ("add watch: " ++ Path.toString (combinePathRep pRep))
                errno Nothing Nothing

-- | Call inotify_add_watch on a path and return the watch descriptor. Raise an
-- exception on failure.
--
-- IN_DONT_FOLLOW covers only the last component of a path. inotify_add_watch
-- resolves a subdirectory path when its event is read, a parent may have been
-- replaced by a symbolic link by then, which can be a link to a directory
-- outside the tree. A subdirectory is therefore opened without following
-- symbolic links, and the watch is added using the fd, via @\/proc@. Checking
-- the components and then calling inotify_add_watch with the path would leave
-- a gap between the two.
--
-- A root is added using its path, a symbolic link can be used to reach a root.
-- If @\/proc@ is not mounted, or access to it is denied, a subdirectory is
-- also added using its path, the check of the components by 'openDirAt'
-- still applies. A subdirectory without read permission fails with EACCES
-- when @\/proc@ is available, it is not added by path.
addWatchFor :: CInt -> Word32 -> PathRep -> IO CInt
addWatchFor fd flags pRep =
    case pRep of
        (_, Nothing) -> addWatchByPath fd flags pRep
        (_, Just _) ->
            bracket (openDirAt o_PATH flags Nothing pRep) c_close
                (addWatchViaFd fd flags pRep)

-- | @renamePaths from to@ changes the directory @from@ and the paths under it
-- to @to@, in all the watch descriptors. A path of the watch descriptor equal
-- to a new path is dropped. It was added on the IN_CREATE event of a directory
-- that no longer exists at that path, when the path already referred to the
-- renamed directory, see the NOTE in 'addToWatchAt'.
renamePaths :: PathRep -> PathRep -> WdMap -> WdMap
renamePaths from to m0 = adjustWds (subtreeWds from m0) step m0

    where

    prefixLen = length (subBytes from)

    subBytes = foldMap (A.toList . Path.toArray) . getRepSub

    isMoving wp = not (wpRemoved wp) && isInSubtree from (wpRep wp)

    newPath wp =
        case drop (prefixLen + 1) (subBytes (wpRep wp)) of
            [] -> wp {wpRep = to}
            rest ->
                wp {wpRep = addSub to (Path.unsafeFromArray (A.fromList rest))}

    step wps
        | any isMoving wps =
            let moved = map newPath (filter isMoving wps)
                isTarget wp = any (eqPathRep (wpRep wp) . wpRep) moved
                isReplaced wp =
                    not (isMoving wp) && not (wpRemoved wp) && isTarget wp
             in Just
                    [ if isMoving wp then newPath wp else wp
                    | wp <- wps
                    , not (isReplaced wp)
                    ]
        | otherwise = Nothing

-- | Clear the pending flag of the paths of a watch descriptor.
clearPending :: Int -> WdMap -> WdMap
clearPending wd = adjustWds [wd] step

    where

    step wps
        | any wpPending wps = Just [wp {wpPending = False} | wp <- wps]
        | otherwise = Nothing

-- | @relocatePending wd pRep@: the directory of @wd@ is at @pRep@. If @wd@ has
-- a pending path, see 'renameDir', its directory was not moved to that path,
-- or it has been moved on since. The pending path and the paths under it are
-- changed to @pRep@.
relocatePending :: Int -> PathRep -> WdMap -> WdMap
relocatePending wd pRep m =
    case lookupWd wd m of
        Just (WdUser wps)
            | (q : _) <- [wpRep wp | wp <- wps, isPending wp] ->
                if eqPathRep q pRep
                then insertWdUser wd (map clear wps) m
                else renamePaths q pRep (clearPending wd m)
        _ -> m

    where

    isPending wp = wpPending wp && not (wpRemoved wp)
    clear wp = wp {wpPending = False}

-- Returns 'False' if the path is already being watched.
insertPath :: Int -> PathRep -> WdMap -> (WdMap, Bool)
insertPath wd pRep m0 =
    case lookupWd wd m of
        Just (WdUser wps)
            | any isSame wps -> (m, False)
            | otherwise -> (insertWdUser wd (wps ++ [new]) m, True)
        _ -> (insertWdUser wd [new] m, True)

    where

    m = relocatePending wd pRep m0
    new =
        WatchPath
            { wpRep = pRep
            , wpRemoved = False
            , wpMoveCookie = Nothing
            , wpPending = False
            }
    isSame wp = not (wpRemoved wp) && eqPathRep (wpRep wp) pRep

-- | Add a watch using the given action on the inotify fd, and insert the
-- path in the map.
addPathUsing :: Watch -> PathRep -> (CInt -> IO CInt) -> IO (Int, Bool)
addPathUsing watch0@Watch {watchWdMap = wdMapRef} pRep addWatch =
    -- The watch descriptor is inserted in the map with the lock held, so that
    -- the watch cannot be closed in between.
    withWatchFd "addRoot" watch0 $ \fd -> do
        wd <- addWatch fd

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
        -- modifies the map. 'addRoot' and 'removeRoot' can be called
        -- from any thread while the thread reading the event stream adds
        -- subdirectories on create events and removes watch descriptors on
        -- IN_IGNORED events. A separate readIORef and writeIORef could lose
        -- an update made by another thread in between. 'insertPath' also
        -- checks whether the path is already watched and inserts it in the
        -- same step, so when a scan and a create event in different threads
        -- add the same directory, only one of them gets 'isNew' as 'True' and
        -- scans the directory.
        (,) (fromIntegral wd)
            <$> atomicModifyIORef' wdMapRef (insertPath (fromIntegral wd) pRep)

-- | Add the watch of a path using the given flags, without scanning it.
-- Returns the watch descriptor, and 'False' if the path is already being
-- watched.
addPathWith :: Word32 -> Watch -> PathRep -> IO (Int, Bool)
addPathWith flags wtch pRep =
    addPathUsing wtch pRep (\fd -> addWatchFor fd flags pRep)

-- | Like 'addPathWith' for a directory already opened by 'openDirAt'.
addPathAt :: Word32 -> Watch -> PathRep -> CInt -> IO (Int, Bool)
addPathAt flags wtch pRep dirFd =
    addPathUsing wtch pRep (\fd -> addWatchViaFd fd flags pRep dirFd)

-- | The directory opened by 'withAddedDir'.
data AddedDir =
      -- | A root that is not a directory, or a directory symbolic link that
      -- is not followed.
      NotDir
      -- | A subdirectory added but not listed because of a permission error
    | DirDenied
    | DirOpened (CInt, ReadDir.DirStream) -- ^ The fd and the directory stream

-- | The denied subdirectory of an 'AddedDir'.
deniedOf :: PathRep -> AddedDir -> [PathRep]
deniedOf pRep DirDenied = [pRep]
deniedOf _ _ = []

-- | Add the watch of a directory and run an action with the result of
-- 'addPathWith' and the fd and the directory stream of the directory, see
-- 'openDirStream'. The directory is opened with @O_PATH@, which does not
-- generate any events, the watch is added with the given flags through this
-- fd, and the directory is then opened for reading relative to it. The fds are
-- closed when the action ends, the subdirectories are opened relative to the
-- fd while it is open. A walk holds two fds for each level of the tree that
-- it keeps open, see 'maxOpenLevels'. The caller restores 'watchFlags' after
-- this returns, after the close.
--
-- A root is added by its path, see 'addWatchFor'.
withAddedDir ::
       Word32
    -> Watch
    -> Maybe ParentDir
    -> PathRep
    -> ((Int, Bool) -> AddedDir -> IO a)
    -> IO a
withAddedDir flags wtch parent pRep action =
    case pRep of
        (_, Nothing) -> do
            r <- addPathWith flags wtch pRep
            bracket openRoot close (action r)
        (_, Just _) ->
            bracket (openDirAt o_PATH flags parent pRep) c_close $ \pfd -> do
                r <- addPathAt flags wtch pRep pfd
                bracket (openSub pfd) close (action r)

    where

    close (DirOpened (_, ds)) = ReadDir.closeDirStream ds
    close _ = return ()

    openSub pfd = do
        r <- try (openDirStream pRep (openDot pRep pfd))
        case r of
            Left e
                | isAccessError e -> return DirDenied
                | otherwise -> ioError e
            Right dir -> return (DirOpened dir)

    openRoot = do
        r <- try (openDirStreamAt flags Nothing pRep)
        case r of
            Left e
                | isNotSubdirError e -> return NotDir
                | otherwise -> ioError e
            Right dir -> return (DirOpened dir)

foreign import ccall unsafe
    "sys/inotify.h inotify_rm_watch" c_inotify_rm_watch
        :: CInt -> CInt -> IO CInt

-- | The result of 'probeWatch'.
data Probe =
      ProbeSame -- ^ The path refers to the directory of the watch descriptor
    | ProbeOther -- ^ The path refers to another directory
    | ProbeMissing -- ^ The path does not exist or is not a directory
    deriving Eq

-- | Like 'updateWatch', and tells a path that is not a directory from a path
-- that is another directory.
probeWatch :: Word32 -> Watch -> Int -> PathRep -> IO Probe
probeWatch flags wtch@Watch {watchWdMap = wdMapRef} wd pRep =
    withWatchFd "addRoot" wtch $ \fd -> do
        r <- try (addWatchFor fd flags pRep)
        case r of
            Left (e :: IOError)
                | isNotSubdirError e -> return ProbeMissing
                | isAccessError e && isJust (getRepSub pRep) ->
                    return ProbeMissing
                | otherwise -> ioError e
            Right wd1 -> do
                m <- readIORef wdMapRef
                let same = fromIntegral wd1 == wd
                when (not same && isNothing (lookupWd (fromIntegral wd1) m))
                    $ void $ c_inotify_rm_watch fd wd1
                if same
                then do
                    atomicModifyIORef' wdMapRef
                        (\m1 -> (relocatePending wd pRep m1, ()))
                    return ProbeSame
                else return ProbeOther

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
--
-- A subdirectory that cannot be reached because of a permission error is
-- treated like a path that is not a directory. A root raises the exception.
--
-- If the path refers to the directory and the watch descriptor has a pending
-- path, the pending path is changed to this path, see 'relocatePending'.
updateWatch :: Word32 -> Watch -> Int -> PathRep -> IO Bool
updateWatch flags wtch wd pRep =
    (== ProbeSame) <$> probeWatch flags wtch wd pRep

subdirConfig :: Config -> Config
subdirConfig = setOnlyDir True . setFollowSymLinks False

-- | Return the default value if the action fails because the subdirectory
-- does not exist, is not a directory, or if the watch is closed.
ignoreInvalid :: Watch -> a -> IO a -> IO a
ignoreInvalid wtch def action =
    action `catchIOError` \e -> do
        closed <- isWatchClosed wtch
        if isNotSubdirError e || closed
        then return def
        else ioError e

-- | Like 'ignoreInvalid', and return the second value if the action fails
-- because of a permission error on a subdirectory. The tree under the
-- subdirectory is not watched, its entries are not reported by a scan. The
-- caller reports the subdirectory as 'isAccessDenied'.
--
-- A subdirectory created in the tree with mode 000 by any user who can write
-- to its parent would otherwise end the watch of the whole tree.
ignoreInvalidOrInaccessible :: Watch -> a -> a -> IO a -> IO a
ignoreInvalidOrInaccessible wtch def denied action =
    action `catchIOError` \e -> do
        closed <- isWatchClosed wtch
        if isNotSubdirError e || closed
        then return def
        else if isAccessError e
        then return denied
        else ioError e

-- | Run an action on the name of each subdirectory in a directory stream,
-- and concatenate the results.
forSubdirs :: PathRep -> ReadDir.DirStream -> (Path -> IO [a]) -> IO [a]
forSubdirs pRep ds action =
    -- The errors ignored:
    --
    -- * ENOENT from lstat of a DT_UNKNOWN entry deleted after readdir
    --   returns it, 'ignoreMissing' skips the entry.
    -- * ENOENT, ELOOP or ENOTDIR from lstat of a DT_UNKNOWN entry, by its
    --   path, if a parent was replaced.
    concat
        <$> S.toList
            (S.mapM action
                $ S.catLefts
                $ S.handle ignoreErr (readEntries pRep ds))

    where

    ignoreErr (e :: IOError) =
        if isNotSubdirError e then return S.nil else ioError e

-- | The number of levels of a walk of a tree that are kept open. A walk
-- keeps a directory open while it walks the tree under it, to continue the
-- listing and to open the subdirectories relative to its fd. The open fds
-- would grow with the depth of the tree, and any user who can create or move
-- a deep chain of directories into the tree could exhaust the fds of the
-- process.
--
-- A directory at this level of a walk or deeper is closed before its
-- subdirectories are walked: the names of its subdirectories are collected,
-- and each subdirectory is then opened from the root, one component at a
-- time, see 'openSubdir'. The memory used at these levels is proportional to
-- the number of subdirectories of a directory, each of them has an entry in
-- the map as well.
maxOpenLevels :: Int
maxOpenLevels = 32

-- | @walkSubdirs level pRep dir action@ runs the action on each subdirectory
-- of an open directory at the given level of a walk, the level of the top
-- directory is 0. The action takes the parent to open the subdirectory with,
-- and the name of the subdirectory.
--
-- Below 'maxOpenLevels' the action runs with the directory as the parent, and
-- the results are returned on the 'Right'. Otherwise the names of the
-- subdirectories are returned on the 'Left', to be passed to
-- 'walkCollected' after the directory is closed.
walkSubdirs ::
       Int
    -> PathRep
    -> (CInt, ReadDir.DirStream)
    -> (Maybe ParentDir -> Path -> IO [a])
    -> IO (Either [Path] [a])
walkSubdirs level pRep (fd, ds) action
    | level < maxOpenLevels =
        Right <$> forSubdirs pRep ds (\name -> action (Just (fd, name)) name)
    | otherwise = Left <$> forSubdirs pRep ds (\name -> return [name])

-- | Run the action of 'walkSubdirs' on the subdirectories it collected,
-- without a parent.
walkCollected ::
    (Maybe ParentDir -> Path -> IO [a]) -> Either [Path] [a] -> IO [a]
walkCollected action = either (fmap concat . mapM (action Nothing)) return

-- | Like 'addRoot', for a directory at the given level of a walk, see
-- 'walkSubdirs'. A subdirectory is opened relative to its parent, if the
-- parent is given. Returns the subdirectories not added or not listed because
-- of a permission error, in the order they were found.
--
-- The subdirectories already watched via the same root are not scanned again.
-- An error in a subdirectory is handled as in 'addSubdirToWatch'.
addToWatchAt ::
    Config -> Watch -> Int -> Maybe ParentDir -> PathRep -> IO [PathRep]
addToWatchAt cfg@Config{..} watch0 level parent pRep = do
    --    showPathRep (root, Nothing) = "root = " ++ Path.toString root
    --    showPathRep (root, Just sub) =
    --        "root = " ++ Path.toString root ++ " sub = " ++ Path.toString sub
    -- putStrLn $ showPathRep pRep ++ " absPath = " ++ Path.toString absPath

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
    --   IN_MOVED_TO of e finds that e is not the directory watched as d, and
    --   removes the watch of the second d. The second IN_CREATE adds it again
    --   with a new watch descriptor and scans it again. No watch covers the
    --   second d in between.
    -- * mkdir d, mv d e, mv f d, where f is watched: IN_CREATE of d finds
    --   the watch descriptor of f and adds d as its second path. Until the
    --   marker queued by IN_MOVED_TO of e, the events in f are reported
    --   twice, under f and under d.
    --
    -- * mkdir d, rmdir d, mv f d: IN_CREATE of d adds d as the second path of
    --   f, nothing removes it. 'renamePaths' changes f to d and drops the
    --   second path.
    -- * mkdir d, mv d /outside, mv f d: IN_MOVED_FROM of d records its cookie
    --   in the second path of f. 'renamePaths' drops that path, so the cookie
    --   does not reach IN_MOVE_SELF of f.
    -- * mkdir d, mv d /outside, mv m d, where m is outside the tree:
    --   IN_MOVED_TO of d finds the cookie, 'addMovedDir' reports the entries
    --   again because the consumer removed them on IN_MOVED_FROM.
    -- * mkdir f/new, mv f g, ln -s /outside f: the add of f/new is refused,
    --   see 'addWatchFor'. The rename adds g/new.
    --
    -- 'addNewDir' reports the entries of a created or moved directory. This
    -- scan adds the subdirectories only, it is used for the roots and after an
    -- overflow.
    --
    -- A root that cannot be added or listed because of a permission error
    -- raises the exception. A subdirectory is skipped and reported as
    -- 'isAccessDenied', see 'ignoreInvalidOrInaccessible'.
    --
    (wd, subdirs) <- withAddedDir (scanFlags cfg) watch0 parent pRep
        $ \(wd, isNew) dir ->
            -- Now add the children. If we missed any creates while we were
            -- adding the parent, this will make sure they are added too.
            case dir of
                DirOpened d | isNew && watchRec ->
                    (,) wd <$> walkSubdirs level pRep d addSubdir
                _ -> return (wd, Right (deniedOf pRep dir))
    denied <- walkCollected addSubdir subdirs

    -- The directory has been listed and closed. The watch of its parent still
    -- has 'scanFlags' until the scan of the parent ends, the directories are
    -- changed in the reverse order of adding them. An already watched path is
    -- changed too, to add the flags of this config.
    when (scanFlags cfg /= watchFlags cfg)
        $ void $ updateWatch (watchFlags cfg) watch0 wd pRep
    return denied

    where

    addSubdir subParent name = do
        let sub = addSub pRep name
        ignoreInvalidOrInaccessible watch0 [] [sub]
            $ addToWatchAt
                (subdirConfig cfg) watch0 (level + 1) subParent sub

-- | @addRoot watch root@ adds the path @root@ as a root of the watch
-- @watch@, with the 'Config' used to open the watch. @root@ must be an
-- absolute path.
--
-- If the same directory is added via two different roots, for example, a
-- recursive root and a root inside it, its events are reported once for each
-- root.
--
-- Can be called from any thread, including while another thread reads the
-- events of the watch using 'eventStream'. The call does not wait for an
-- event. Raise an exception if the watch is closed.
--
-- /Pre-release/
--
addRoot :: Watch -> Path -> IO ()
addRoot wtch@Watch {watchConfig = cfg} root = do
    denied <- addToWatchAt cfg wtch 0 Nothing (root, Nothing)
    recordSynthEvents wtch (map SynthDenied denied)

-- | Like 'addRoot' but does not raise an exception if the path does not
-- exist, is not a directory, or if the watch is closed. Used to add the
-- subdirectories found by the scan or reported by IN_CREATE or IN_MOVED_TO,
-- a subdirectory may be deleted, moved or replaced by another file before its
-- watch is added. Its parent reports the deletion or the move. The watch may
-- be closed by another thread while the event stream adds a subdirectory.
-- A subdirectory that cannot be added because of a permission error is
-- skipped, see 'ignoreInvalidOrInaccessible'. Returns the subdirectories
-- skipped because of a permission error, see 'addToWatchAt'.
--
-- The subdirectory is added with 'setOnlyDir' and without
-- 'setFollowSymLinks', the add fails with ENOTDIR if the path is now a file
-- or a symbolic link. The scan does not follow symbolic links either.
addSubdirToWatch :: Config -> Watch -> PathRep -> IO [PathRep]
addSubdirToWatch cfg wtch pRep =
    ignoreInvalidOrInaccessible wtch [] [pRep]
        (addToWatchAt (subdirConfig cfg) wtch 0 Nothing pRep)

-- | An entry found by 'addNewDir' or 'rescanDir'.
data ScannedEntry = ScannedEntry
    { seIsDir :: Bool
    , sePath :: PathRep
    }

-- | An item of the stream of a scan.
data ScanItem =
      ScanFound ScannedEntry
      -- | A subdirectory not added or not listed because of a permission
      -- error, reported as 'isAccessDenied'.
    | ScanDenied PathRep
      -- | An operation of the scan failed, for example, the process has no
      -- free fd or has reached the max_user_watches limit. The event stream
      -- raises the exception when it reads the item. The scan continues
      -- after the item, the stream of a scan does not raise an exception.
      -- An exception raised in the middle of a listing would leave a closed
      -- directory in the part of the stream already saved in the 'Watch'.
    | ScanFailed IOException

-- | An item found by the walk of a scan, whether it is reported, and the
-- items under it, empty for a file. The watch of a directory is added
-- before its node is returned, the entries under it are read when the stream
-- is read.
data ScanNode = ScanNode
    { snReported :: Bool
    , snItem :: ScanItem
    , snChildren :: Stream IO ScanNode
    }

-- | The node of a subdirectory denied by a permission error, it is always
-- reported.
deniedNode :: PathRep -> ScanNode
deniedNode pRep = ScanNode True (ScanDenied pRep) S.nil

-- | The node of a failed operation of the scan, it is always reported.
failedNode :: IOException -> ScanNode
failedNode e = ScanNode True (ScanFailed e) S.nil

-- | Handle an exception raised by the listing of a subdirectory. If the
-- subdirectory does not exist, is not a directory, or if the watch is closed,
-- the stream ends. If the subdirectory cannot be listed because of a
-- permission error, the stream ends with its 'deniedNode'. Any other
-- 'IOException' ends the stream with its 'failedNode'.
handleListing :: Watch -> PathRep -> Stream IO ScanNode -> Stream IO ScanNode
handleListing wtch pRep =
    S.handle $ \(e :: IOException) ->
        ignoreInvalidOrInaccessible
            wtch S.nil (S.fromPure (deniedNode pRep)) (ioError e)
            `catchIOError` (return . S.fromPure . failedNode)

-- | Change the flags of the watch of a directory to 'watchFlags' when the
-- stream ends. A failure follows the stream as a 'failedNode'.
withWatchFlags ::
       Config
    -> Watch
    -> Int
    -> PathRep
    -> Stream IO ScanNode
    -> Stream IO ScanNode
withWatchFlags cfg wtch wd pRep stream
    | scanFlags cfg == watchFlags cfg = stream
    | otherwise = stream `S.append` restoreFlags

    where

    restoreFlags =
        S.handle (return . S.fromPure . failedNode)
            $ S.nilM
            $ ignoreInvalid wtch False
            $ updateWatch (watchFlags cfg) wtch wd pRep

-- | Add the watch of a directory with 'scanFlags' and return the stream of its
-- entries. The directory is read when the stream is read. The watch of a
-- subdirectory is added when the stream reads its entry, before the entry is
-- returned.
--
-- A subdirectory is opened relative to the fd of its parent, see 'openDirAt'.
-- The fd used to add the watch is closed before the stream is returned, the
-- stream may never be read. The stream opens the directory again when it is
-- read, and holds the fd until it ends, to open the subdirectories. A
-- subdirectory's stream is read in the depth first walk of 'scanTree' while
-- the stream of its parent is open, the parent fd is open then.
--
-- The directory is at the given level of the walk, the level of the top
-- directory is 0. At 'maxOpenLevels' and deeper the directory is not kept
-- open while the trees of its subdirectories are walked. Its stream returns
-- all its entries first, the watch of a subdirectory is added when its entry
-- is read as at the other levels. The directory is then closed, and the
-- stream returns one node for each subdirectory that is not reported and has
-- the entries of the subdirectory as its children. Such a subdirectory is
-- opened from the root, see 'openSubdir'.
--
-- The entries of the directory are reported if @reportAll@ is 'True' or if
-- the walk added the directory, and then @reportAll@ is 'True' for its
-- subdirectories. The watch is added without IN_MASK_ADD, the mask of a
-- directory already watched is replaced with 'scanFlags'.
--
-- The stream does not raise an 'IOException', see 'ScanFailed'.
scanChildren ::
       Watch
    -> Bool
    -> Config
    -> Int
    -> Maybe ParentDir
    -> PathRep
    -> IO (Stream IO ScanNode)
scanChildren wtch reportAll cfg level parent pRep = do
    (wd, added) <- add (scanFlags cfg .&. complement iN_MASK_ADD)
    return $ list wd (reportAll || added)

    where

    add flags =
        case pRep of
            (_, Nothing) -> addPathWith flags wtch pRep
            (_, Just _) ->
                bracket (openDirAt o_PATH flags parent pRep) c_close
                    (addPathAt flags wtch pRep)

    -- The parent is open when this directory is listed only if the level of
    -- the parent is kept open.
    listParent = if level <= maxOpenLevels then parent else Nothing

    list wd reported =
        withWatchFlags cfg wtch wd pRep
            $ if level < maxOpenLevels
              then listing (node reported)
              else S.concatEffect (deferred reported)

    listing f =
        handleListing wtch pRep
            $ S.bracketIO
                (openDirStreamAt (scanFlags cfg) listParent pRep)
                (closeDir . snd)
            $ \(fd, ds) -> S.mapM (f fd) (readEntries pRep ds)

    -- closedir releases the directory stream even if it fails.
    closeDir ds = ReadDir.closeDirStream ds `catchIOError` \_ -> return ()

    -- The nodes of the subdirectories are collected while the directory is
    -- listed, and returned with their children after the directory is
    -- closed.
    deferred reported = do
        ref <- newIORef []
        let defer fd entry = do
                n <- node reported fd entry
                case entry of
                    Left _ -> do
                        modifyIORef' ref (n {snReported = False} :)
                        return n {snChildren = S.nil}
                    Right _ -> return n
            collected = S.fromList . reverse <$> readIORef ref
        return $ listing defer `S.append` S.concatEffect collected

    -- A failure to add a subdirectory is returned as a node, the listing
    -- continues with the next entry.
    node reported fd (Left dir) = do
        let path = addSub pRep dir
        children <-
            ignoreInvalidOrInaccessible
                wtch S.nil (S.fromPure (deniedNode path))
                (scanChildren
                    wtch reported (subdirConfig cfg) (level + 1)
                    (Just (fd, dir)) path)
                `catchIOError` (return . S.fromPure . failedNode)
        return $ ScanNode reported (found True path) children
    node reported _ (Right file) =
        return $ ScanNode reported (found False (addSub pRep file)) S.nil

    found dir path = ScanFound (ScannedEntry dir path)

-- | Add the watch of a directory and walk the tree under it depth first,
-- adding the watches of the subdirectories. Returns the reported entries of
-- the tree, not including the directory itself, and the subdirectories denied
-- by a permission error. The watch of the directory is
-- added before the stream is returned, the tree is walked when the stream is
-- read. The watch of a subdirectory is added before its entry is returned.
-- For this reason the walk iterates on 'ScanNode's, which are created after
-- their watches are added, not on paths. The memory used is proportional to
-- the depth of the tree.
--
-- A directory needs 'scanFlags' until it and its subdirectories are listed.
-- In a depth first walk the stream of the entries of a directory ends after
-- the trees of all its subdirectories are walked, 'withWatchFlags' changes
-- the flags then. A breadth first walk would have to change the flags of a
-- directory after the next level is listed.
--
-- A directory is opened when the walk reaches it and closed when its stream
-- ends, a walk holds one open directory for each level of the tree that it
-- keeps open, see 'maxOpenLevels'.
scanTree ::
    Watch -> Bool -> Config -> PathRep -> IO (Stream IO ScanItem)
scanTree wtch reportAll cfg pRep = do
    top <- scanChildren wtch reportAll cfg 0 Nothing pRep
    return
        $ fmap snItem
        $ S.filter snReported
        $ StreamK.toStream
        $ StreamK.concatIterateWith
            StreamK.append (StreamK.fromStream . snChildren)
        $ StreamK.fromStream top

-- | Walk the tree of a directory already watched, add the subdirectories that
-- are not watched and return the entries found in them, or all the entries if
-- @reportAll@ is 'True'. The flags of each watch are replaced with
-- 'scanFlags' while it is listed, see 'scanTree', the listing does not
-- generate any events on the watches of the tree.
-- Listing the directory generates the open, access and close events of its
-- parent, which are reported if they are enabled. See 'scanTree'.
rescanDir :: Bool -> Config -> Watch -> PathRep -> IO (Stream IO ScanItem)
rescanDir reportAll cfg wtch = scanTree wtch reportAll cfg

-- | Add a directory reported by IN_CREATE or IN_MOVED_TO and return the
-- entries in it. The watch is added before the directory is read, an entry
-- created later is either found by the scan or reported by the kernel, or
-- both. See 'scanTree'.
--
-- The tree is walked by 'rescanDir' with @reportAll@ 'True'. A directory
-- already watched returns all its entries too. The scan that added it may
-- have reported them before an IN_DELETE of the path, for example, in mkdir
-- a; rmdir a; mkdir a, all done before the events are read, the first
-- IN_CREATE adds and scans the second a, and the consumer removes its entries
-- on the IN_DELETE.
addNewDir :: Config -> Watch -> PathRep -> IO (Stream IO ScanItem)
addNewDir cfg wtch pRep =
    ignoreInvalidOrInaccessible wtch S.nil (S.fromPure (ScanDenied pRep))
        $ rescanDir True (subdirConfig cfg) wtch pRep

-- | Whether the entries found by 'addNewDir' are reported.
reportsScan :: Config -> Bool
reportsScan cfg = scanNewDirs cfg && createFlags cfg .&. iN_CREATE /= 0

-- | Add a new directory, and return its entries if they are reported, and the
-- subdirectories denied by a permission error.
addDirWithScan :: Config -> Watch -> PathRep -> IO (Stream IO ScanItem)
addDirWithScan cfg wtch pRep
    | reportsScan cfg = addNewDir cfg wtch pRep
    | otherwise =
        S.fromList . map ScanDenied <$> addSubdirToWatch cfg wtch pRep

-- | Concatenate a list of streams.
concatStreams :: [Stream IO a] -> Stream IO a
concatStreams = S.concatMap id . S.fromList

foreign import ccall unsafe "unistd.h pipe2"
    c_pipe2 :: Ptr CInt -> CInt -> IO CInt

foreign import capi "fcntl.h value O_CLOEXEC" o_CLOEXEC :: CInt

-- | Mark the paths of the given watch descriptors that satisfy the predicate
-- as removed. The watch descriptors are in the ascending order. Returns the
-- watch descriptors whose paths are now all removed, and the removed paths of
-- the other watch descriptors, both in the ascending order of watch
-- descriptors.
markRemoved ::
       [Int]
    -> (PathRep -> Bool)
    -> WdMap
    -> (WdMap, ([Int], [WdUnwatched]))
markRemoved candidates p m0 =
    let (m1, wds, shared) = foldl' step (m0, [], []) candidates
     in (m1, (reverse wds, reverse shared))

    where

    isRemoving wp = not (wpRemoved wp) && p (wpRep wp)
    setRemoved wp = if isRemoving wp then wp {wpRemoved = True} else wp

    step acc@(!m, wds, shared) wd =
        let wps = wdUserPaths wd m
            removing = filter isRemoving wps
            wps1 = map setRemoved wps
            shared1 = reverse [(wd, wpRep wp) | wp <- removing] ++ shared
            m1 = insertWdUser wd wps1 m
         in if null removing
            then acc
            else if all wpRemoved wps1
            then (m1, wd : wds, shared)
            else (m1, wds, shared1)

-- | Undo 'markRemoved' for the listed paths. For each path, the first path of
-- its watch descriptor that is marked removed and equal to it is unmarked.
-- Identical paths of a watch descriptor report the same events, it does not
-- matter which one is unmarked.
unmarkRemoved :: [WdUnwatched] -> WdMap -> WdMap
unmarkRemoved paths m0 = foldl' step m0 paths

    where

    step m (wd, pRep) =
        let isMatch wp = wpRemoved wp && eqPathRep (wpRep wp) pRep
         in case break isMatch (wdUserPaths wd m) of
                (xs, wp : ys) ->
                    let wp1 = wp {wpRemoved = False}
                     in insertWdUser wd (xs ++ wp1 : ys) m
                _ -> m

-- | Queue an IN_IGNORED event on the inotify fd by adding a watch and
-- removing it. The action is run with the watch descriptor after adding the
-- watch and before removing it. The string is used in the exception
-- messages.
--
-- Returns 'False' and does not run the action if @\/proc@ is not mounted or
-- access to it is denied. Requires a free fd for a pipe, throw an exception
-- if not.
queueIgnored :: String -> CInt -> (CInt -> IO ()) -> IO Bool
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
                c_inotify_add_watch fd pathPtr (CUInt iN_ACCESS)
            errno <- getErrno
            if wd /= -1
            then do
                action wd
                throwErrnoIfMinus1_ err $ c_inotify_rm_watch fd wd
                return True
            else if errno == eNOENT || errno == eACCES
            then return False
            else ioError (errnoToIOError err errno Nothing Nothing)

-- | Remove the given removed paths from the map. Returns the paths that were
-- still in the map.
removeListed :: [WdUnwatched] -> WdMap -> (WdMap, [WdUnwatched])
removeListed paths m0 =
    let (m1, removed) = foldl' step (m0, []) paths
     in (m1, reverse removed)

    where

    step (!m, acc) (wd, pRep) =
        let isMatch wp = wpRemoved wp && eqPathRep (wpRep wp) pRep
         in case break isMatch (wdUserPaths wd m) of
                (xs, _ : ys) -> (update wd (xs ++ ys) m, (wd, pRep) : acc)
                _ -> (m, acc)

    update wd [] m = deleteWd wd m
    update wd wps m = insertWdUser wd wps m

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
-- If @\/proc@ is not available the marker cannot be queued. The paths are
-- then removed from the map at once, and the paths
-- still in the map are returned, to be reported 'isRootUnwatched' by the
-- caller. The events already queued for the paths and not yet read are not
-- reported. Returns an empty list if the marker is queued.
--
addRemovalMarker :: Watch -> [WdUnwatched] -> IO [WdUnwatched]
addRemovalMarker Watch {watchFd = fd, watchWdMap = wdMapRef} paths = do
    queued <-
        queueIgnored err fd $ \wd ->
            -- The marker must be in the map before its IN_IGNORED is read.
            atomicModifyIORef' wdMapRef
                $ \m -> (insertMarker (fromIntegral wd) paths m, ())
    if queued
    then return []
    else atomicModifyIORef' wdMapRef (removeListed paths)

    where

    err = "unwatchPaths: marker " ++ show (map fst paths)

-- | Remove the paths that satisfy the predicate from the watch. The string
-- describes the paths in the exception message. The function returns the
-- watch descriptors that can have such paths, in the ascending order, see
-- 'allWds' and 'subtreeWds'.
--
-- The events already queued for the paths are reported, followed by an
-- 'isRootUnwatched' event for each path.
--
-- If @\/proc@ is not mounted or access to it is denied, a path that shares
-- the directory with another path is removed at once: the events queued for
-- it and not yet read are not reported. Returns such paths, the caller
-- reports their 'isRootUnwatched' events. See 'removeRoot'.
--
-- Removing a path that shares the directory with another path requires a
-- free fd for a pipe, throw an exception if not. The paths of the shared
-- watch descriptors are not removed in that case, the call can be retried.
--
-- Does nothing if the watch is closed, closing the watch removes all the
-- paths.
unwatchPaths ::
       Watch
    -> String
    -> (WdMap -> [Int])
    -> (PathRep -> Bool)
    -> IO [WdUnwatched]
unwatchPaths
    wtch@Watch {watchFd = fd, watchWdMap = wdMapRef} desc candidates p =
    whenWatchOpen wtch [] $ do
        -- A watch descriptor can be shared by more than one path, see
        -- 'WatchPath' for the cases.
        --
        -- Only when all the paths of a watch descriptor are removed we remove
        -- the kernel watch. Otherwise, the kernel watch is still needed for
        -- the other paths.
        --
        (wds, shared) <-
            atomicModifyIORef' wdMapRef
                (\m -> markRemoved (candidates m) p m)

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
        if null shared
        then return []
        else addRemovalMarker wtch shared
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
-- If @\/proc@ is not mounted or access to it is denied, the directories that
-- the root shares with another root are removed for the root at once: the
-- events queued for them and not yet read are not reported for the root, and
-- their 'isRootUnwatched' events are reported before the events of the next
-- read of the fd, the events already read are reported first. A thread
-- waiting for events reports them at once in the threaded RTS, and when the
-- next event arrives in the non-threaded RTS.
--
-- Removing a root that shares a directory with another root requires a free
-- fd for a pipe, throw an exception if not. The shared directories remain
-- watched for the root in that case, the call can be retried.
--
-- Can be called from any thread, including while another thread reads the
-- events of the watch using 'eventStream'. The call does not wait for an
-- event. Does nothing if the watch is closed.
--
-- /Pre-release/
--
removeRoot :: Watch -> Path -> IO ()
removeRoot wtch root = do
    removed <-
        unwatchPaths
            wtch (show (Path.toString root)) allWds
            (eqPath root . getRepRoot)
    recordSynthEvents wtch (map (uncurry SynthUnwatched) removed)

-- | Close the fd of a watch. closeFdWith wakes up the threads waiting for
-- the fd to become readable, in the threaded RTS. The kernel releases the fd
-- even if close fails.
closeInotifyFd :: CInt -> IO ()
closeInotifyFd fd = closeFdWith (\_ -> void (c_close fd)) (Fd fd)

-- | Close a 'Watch' handle. The event streams of the watch end, a thread
-- waiting for events in 'eventStream' is woken up. Closing a closed watch
-- does nothing.
--
-- Can be called from any thread, including while another thread reads the
-- events of the watch using 'eventStream'. The call does not wait for an
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
closeWatch Watch {watchFd = fd, watchLock = lock} =
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
            void (queueIgnored "closeWatch" fd (\_ -> return ()))
                `catchIOError` \_ -> return ()
            return st {fsWatchClosed = True}

-- | Given a 'Config' and list of @paths@ ("/" separated byte arrays) start
-- monitoring the paths for file system events. Returns a 'Watch' handle which
-- can then be used to read the event stream or to close the watch.
--
-- The 'Config' is kept in the 'Watch', 'readEvent', 'eventStream' and
-- 'addRoot' use it.
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
        w <- createWatch cfg
        restore (mapM_ (addRoot w) (NonEmpty.toList paths))
            `onException` closeWatch w
        return w

-------------------------------------------------------------------------------
-- Raw events read from the watch file handle
-------------------------------------------------------------------------------

-- | Identifies the 'isMovedFrom' and 'isMovedTo' events of the same move
-- operation, see 'getCookie'.
newtype Cookie = Cookie Word32 deriving (Show, Eq)

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

-- | The flag of an 'isAccessDenied' event. It is not an inotify flag, the
-- inotify events do not use the bits from 0x00010000 to 0x00800000.
accessDeniedFlag :: Word32
accessDeniedFlag = 0x00100000

-- | Whether the path of a root still refers to the object watched via its
-- watch descriptor. The flags of the watch are set to 'watchFlags'.
isRootAt :: Config -> Watch -> WdPath -> IO Bool
isRootAt cfg wtch (wd, pRep) =
    ignoreInvalid wtch False $ updateWatch (watchFlags cfg) wtch wd pRep

-- | Remove the roots whose path no longer refers to the watched object, and
-- the paths under them. Such a root was renamed, deleted or replaced. Its
-- events would report the old path, which does not exist or refers to another
-- object, and the kernel does not report the new path of a root.
--
-- Called with the roots of a watch descriptor that reported IN_MOVE_SELF. A
-- root renamed back before the check stays watched. Two hard links
-- watched as roots share the watch descriptor, the rename of one link reports
-- IN_MOVE_SELF for both, only the renamed one is removed.
--
-- The roots are matched by their paths, if a root that is kept has the
-- same path, the paths are kept. Returns the paths removed without a marker,
-- see 'unwatchPaths'.
unwatchGoneRoots ::
    Config -> Watch -> [WdPath] -> IO [WdUnwatched]
unwatchGoneRoots cfg wtch roots = do
    results <- mapM (\root -> (,) (snd root) <$> isRootAt cfg wtch root) roots
    let kept = [root | (root, True) <- results]
        gone = [root | (root, False) <- results]
        isGone p = any (isUnder p) gone && not (any (isUnder p) kept)
    if null gone
    then return []
    else unwatchPaths wtch "unwatchGoneRoots" allWds isGone

    where

    isUnder p root = eqPath (getRepRoot p) (getRepRoot root)

-- | Remove a directory and the tree under it from the watch. Returns the
-- paths removed without a marker, see 'unwatchPaths'.
unwatchSubtree :: Watch -> PathRep -> IO [WdUnwatched]
unwatchSubtree wtch pRep =
    unwatchPaths wtch
        (show (Path.toString (combinePathRep pRep)))
        (subtreeWds pRep)
        (isInSubtree pRep)

-------------------------------------------------------------------------------
-- IMPORTANT: Dependency on the order of the kernel events
-------------------------------------------------------------------------------
--
-- A directory moved out of the tree is detected by assuming that rename(2)
-- queues IN_MOVED_TO before IN_MOVE_SELF. An IN_MOVE_SELF event read for a
-- path that still has a cookie is taken to mean that no IN_MOVED_TO event
-- exists.
--
-- This order is not documented. inotify(7) lists the events of a rename in
-- this order, but guarantees only that the events form an ordered queue. It
-- does not state the position of IN_MOVE_SELF. The order was observed on Linux
-- 6.12 only, the kernel source was not checked.
--
-- If a kernel queues IN_MOVE_SELF first, every rename within the tree is
-- handled as a move out followed by a move in. The watches are removed and
-- added again, the events generated in between are lost, and the moved paths
-- report 'isRootUnwatched'. The set of watched paths remains correct. The test
-- recRenameNoUnwatched fails in that case.

-- NOTE: A directory renamed within the tree:
--
-- rename(2) generates IN_MOVED_FROM and IN_MOVED_TO on the parents, and then
-- IN_MOVE_SELF on the directory, in that order. A directory moved out of the
-- tree has no IN_MOVED_TO event. A watch belongs to an inode, the watches of
-- the directory and of the tree under it are not affected by the rename. We
-- keep these watches and change their paths in the map on IN_MOVED_TO.
-- Removing the watches and adding them again would lose the events generated
-- in between.
--
-- The kernel queues IN_MOVED_TO during the rename, before the events generated
-- in the directory after the rename, these events are read after the map is
-- changed and report the new path.
--
-- IN_MOVED_FROM records its cookie in the path, 'markMoved'. Other events can
-- be queued between the three events of a rename. The next event of the
-- directory decides:
--
-- * IN_MOVED_TO with the same cookie: the directory is renamed, the cookie is
--   removed.
-- * IN_MOVE_SELF of a path that still has a cookie: there was no IN_MOVED_TO,
--   the directory was moved out, its paths are removed from the watch.
-- * IN_Q_OVERFLOW: IN_MOVED_TO or IN_MOVE_SELF may have been dropped, the
--   stream ends.
--
-- A directory can be a different directory by the time IN_MOVED_TO is read,
-- the watch of the old path may have been added after the rename, when the
-- path was reused, see the NOTE in 'addToWatchAt'. 'updateWatch' checks
-- that the new path refers to the watched directory. If it does not, or if
-- the new path does not exist, the paths are removed and the new path is
-- added the same way as a directory moved in.
--
-- In both cases the tree at the new path is walked and all its entries are
-- returned, a rename is reported as the tree leaving and coming back in. See
-- "Moves and scans" in dev/design.linux.md. The walk also adds the
-- subdirectories that are not watched. Such a subdirectory had its create
-- event read when the old path no longer existed and it could not be added,
-- or the event is not read yet. The walk also restores 'watchFlags' on a
-- directory that was renamed while it was being listed, 'updateWatch' could
-- not find its old path.

-- | The watch descriptors that have a path with a cookie, in the ascending
-- order.
movedWds :: WdMap -> [Int]
movedWds = IntSet.toAscList . wmMoved

-- | Remove the cookie of the paths that satisfy the predicate. The predicate
-- must be 'False' for a path without a cookie.
clearMoved :: (WatchPath -> Bool) -> WdMap -> WdMap
clearMoved p m0 = adjustWds (movedWds m0) step m0

    where

    step wps
        | any p wps =
            Just [if p wp then wp {wpMoveCookie = Nothing} else wp | wp <- wps]
        | otherwise = Nothing

-- | Record the cookie of IN_MOVED_FROM in the paths of a directory. A path
-- that is not watched has no entry and records nothing, no IN_MOVE_SELF event
-- is generated for it.
markMoved :: Word32 -> [PathRep] -> WdMap -> WdMap
markMoved cookie paths m0 =
    adjustWds (concatMap (`pathWds` m0) paths) step m0

    where

    isSource wp =
        not (wpRemoved wp) && any (eqPathRep (wpRep wp)) paths

    step wps
        | any isSource wps =
            Just
                [ if isSource wp then wp {wpMoveCookie = Just cookie} else wp
                | wp <- wps
                ]
        | otherwise = Nothing

-- | Remove a cookie from the map. Returns the paths that had it, with their
-- watch descriptors.
takeMoved :: Word32 -> WdMap -> (WdMap, [WdPath])
takeMoved cookie m =
    ( clearMoved isMatch m
    , [ (wd, wpRep wp)
      | wd <- movedWds m
      , wp <- wdUserPaths wd m
      , isMatch wp
      ]
    )

    where

    isMatch wp = not (wpRemoved wp) && wpMoveCookie wp == Just cookie

-- | Add a directory that arrived at the path @to@ by a move, and return all
-- the entries under it if they are reported, see 'reportsScan', whether the
-- path is already watched or not. A move is
-- reported as the tree leaving and coming back in, see "Moves and scans" in
-- dev/design.linux.md.
--
-- The path may be already watched with the cookie of a move out. The path was
-- added on the IN_CREATE event of a directory that was then moved out, when
-- the path already referred to this directory. The cookie is removed.
addMovedDir :: Config -> Watch -> PathRep -> IO (Stream IO ScanItem)
addMovedDir cfg wtch@Watch {watchWdMap = wdMapRef} to = do
    reused <- atomicModifyIORef' wdMapRef takeStale
    if reused || reportsScan cfg
    then
        ignoreInvalidOrInaccessible wtch S.nil (S.fromPure (ScanDenied to))
            (rescanDir (reportsScan cfg) (subdirConfig cfg) wtch to)
    else addDirWithScan cfg wtch to

    where

    isStale wp =
        not (wpRemoved wp)
            && isJust (wpMoveCookie wp)
            && eqPathRep (wpRep wp) to

    hasStale m wd = any isStale (wdUserPaths wd m)

    takeStale m = (clearMoved isStale m, any (hasStale m) (movedWds m))

-- NOTE: A rename to a path that no longer exists:
--
-- For a watched a, in mv a b; mv b c read after both renames, b does not
-- exist when IN_MOVED_TO of b is read. The watch W of a is kept, its paths
-- are changed to b, and b is marked pending. b is not scanned. The events of
-- b are read later and decide:
--
-- * IN_MOVE_SELF of W: W is the directory that was renamed to b, the pending
--   mark is removed. rename(2) queues IN_MOVE_SELF after IN_MOVED_TO while it
--   holds the lock of the parent of b, before any later event of b.
-- * inotify_add_watch on a path p returns W: W is at p, the pending path is
--   changed to p, see 'relocatePending'. Such a W was added after the rename,
--   on the IN_CREATE event of a reused path, see the NOTE in 'addToWatchAt'.
-- * IN_MOVED_FROM or IN_DELETE of b before IN_MOVE_SELF of W: the event is
--   for the directory that was renamed, W is not that directory. Its pending
--   paths are removed, as for a new path that is not the watched directory.
--
-- If the new path exists and is another directory, the paths are removed and
-- the new path is added as a directory moved in.

-- | Mark the path of a watch descriptor equal to the given path as pending.
markPending :: Int -> PathRep -> WdMap -> WdMap
markPending wd pRep = adjustWds [wd] (Just . map step)

    where

    step wp =
        if not (wpRemoved wp) && eqPathRep (wpRep wp) pRep
        then wp {wpPending = True}
        else wp

-- | The pending paths named by an IN_MOVED_FROM or IN_DELETE event of a
-- directory, see the NOTE above 'markPending'. The pending marks are removed,
-- the caller removes the paths.
takeNamedPending ::
    [PathRep] -> WdMap -> (WdMap, [PathRep])
takeNamedPending paths m =
    (foldr (clearPending . fst) m found, map snd found)

    where

    isNamed wp =
        wpPending wp
            && not (wpRemoved wp)
            && any (eqPathRep (wpRep wp)) paths
    found =
        [ (wd, wpRep wp)
        | wd <- IntSet.toAscList (wmPending m)
        , wp <- wdUserPaths wd m
        , isNamed wp
        ]

-- | Handle the rename of the directory @from@ to @to@ under the same root, see
-- the NOTE above 'clearMoved'. Returns all the entries under the new path if
-- they are reported, see 'reportsScan', whether the watch is kept or not. A
-- new path that does not exist is pending, see the NOTE above 'markPending'.
-- Also returns the paths removed without a marker, see 'unwatchPaths'.
renameDir ::
       Config
    -> Watch
    -> WdPath
    -> PathRep
    -> IO ([WdUnwatched], Stream IO ScanItem)
renameDir cfg wtch@Watch {watchWdMap = wdMapRef} (wd, from) to = do
    r <-
        ignoreInvalid wtch ProbeOther
            $ probeWatch (watchFlags subCfg) wtch wd to
    case r of
        ProbeSame -> do
            atomicModifyIORef' wdMapRef (\m -> (renamePaths from to m, ()))
            fmap ((,) [])
                $ ignoreInvalidOrInaccessible
                    wtch S.nil (S.fromPure (ScanDenied to))
                    (rescanDir (reportsScan cfg) subCfg wtch to)
        ProbeMissing -> do
            atomicModifyIORef' wdMapRef
                (\m -> (markPending wd to (renamePaths from to m), ()))
            return ([], S.nil)
        ProbeOther -> do
            removed <- unwatchSubtree wtch from
            (,) removed <$> addMovedDir cfg wtch to

    where

    subCfg = subdirConfig cfg

-- NOTE: A directory created in the tree and a directory moved in:
--
-- The entries found by a scan are reported after the event that caused the
-- scan, IN_CREATE or IN_MOVED_TO of the directory. The events are read after
-- the operations, the event that causes the scan does not always match how
-- the directory arrived. In the cases below, all the operations happen before
-- the events are read. "Watched a" is a directory whose events have been read,
-- d is a directory outside the tree containing f.
--
-- | Operations                     | Dir | Actual   | Reported as |
-- |--------------------------------|-----|----------|-------------|
-- | mkdir a; touch a/f; mv a b     | b   | created  | moved in    |
-- | Watched a:                     | b/s | created  | moved in    |
-- | mkdir a/s; touch a/s/f; mv a b |     |          |             |
-- | mkdir a; rmdir a; mv d a       | a   | moved in | created     |
--
-- * mkdir a; touch a/f; mv a b: the add of a on IN_CREATE fails, the path no
--   longer exists. b is scanned on IN_MOVED_TO, b/f follows the isMovedTo
--   event of b.
-- * mkdir a/s; touch a/s/f; mv a b: the add of a/s on IN_CREATE fails. The
--   rename keeps the watch of a, the walk of b adds b/s. b/s and b/s/f follow
--   the isMovedTo event of b.
-- * mkdir a; rmdir a; mv d a: IN_CREATE of a finds d at the path and scans it,
--   a/f follows the isCreated event of a, before the isDeleted event of a.
--   IN_MOVED_TO of a scans the path again and reports a/f again.
-- * Watched a: mv a b; mv b c. b does not exist when IN_MOVED_TO of b is
--   read, the watch of a is kept with the pending path b, see the NOTE above
--   'markPending'. c is scanned on IN_MOVED_TO of c, as every rename, and the
--   entries of a are reported again.
--
-- For this reason a scan event has one flag, 'isScanEvent', for both causes,
-- and every IN_MOVED_TO of a directory reports all its entries.

-- | Handle IN_MOVED_TO of a directory that reports the given paths. A path
-- whose root has a path with the cookie is a rename within that root. A path
-- with the cookie whose root does not have a new path is moved out of that
-- root. The other new paths are paired, in order, with the paths moved out of
-- their roots, such a pair is a rename between two roots of the watch, the
-- directory stays watched. The remaining new paths are moved in, and the
-- remaining paths moved out are removed. Returns the entries found by the
-- renames and in the directories moved in, and the paths removed without a
-- marker, see 'unwatchPaths'.
movedInDir ::
       Config
    -> Watch
    -> Word32
    -> [PathRep]
    -> IO ([WdUnwatched], Stream IO ScanItem)
movedInDir cfg wtch@Watch {watchWdMap = wdMapRef} cookie paths = do
    froms <- atomicModifyIORef' wdMapRef (takeMoved cookie)
    let isSameRoot p1 p2 = eqPath (getRepRoot p1) (getRepRoot p2)
        sameRootFrom to = find (isSameRoot to . snd) froms
        movedOut = filter (\f -> not (any (isSameRoot (snd f)) paths)) froms
        movedIn = filter (isNothing . sameRootFrom) paths
        acrossRoots = zip movedIn movedOut
        moveTo to =
            case sameRootFrom to of
                Just from -> renameDir cfg wtch from to
                Nothing ->
                    case find (eqPathRep to . fst) acrossRoots of
                        Just (_, from) -> renameDir cfg wtch from to
                        Nothing -> (,) [] <$> addMovedDir cfg wtch to
    let goneOut = map snd (drop (length movedIn) movedOut)
    removed <- concat <$> mapM (unwatchSubtree wtch) goneOut
    (moved, scans) <- mapAndUnzipM moveTo paths
    return (removed ++ concat moved, concatStreams scans)

-- | An event of a synthetic event, see 'synthEvent'.
mkSynth :: CInt -> Word32 -> PathRep -> Event
mkSynth wd flags (root, sub) =
    Event
        { eventWd = wd
        , eventFlags = flags
        , eventCookie = 0
        , eventRoot = root
        , eventRelPath = sub
        , eventScanned = False
        }

-- | The event reported for a synthetic event.
synthEvent :: SynthEvent -> Event
synthEvent (SynthDenied pRep) =
    mkSynth (-1) (accessDeniedFlag .|. iN_ISDIR) pRep
synthEvent (SynthUnwatched wd pRep) =
    mkSynth (fromIntegral wd) iN_IGNORED pRep

-- | A kernel event read from the fd of a watch: the watch descriptor, the
-- flags, the cookie and the name.
data RawEvent = RawEvent CInt Word32 Word32 (Maybe Path)

-- XXX We can perhaps use parseD monad instance for fusing with parseMany? Need
-- to measure the perf.
{-# INLINE readOneEvent #-}
readOneEvent :: Parser Word8 IO RawEvent
readOneEvent = do
    -- XXX the header format is implicit in the code, this could be bug prone.
    let headerLen = sizeOf (undefined :: CInt) + 12
    arr <- PR.takeEQ headerLen (A.createOf headerLen)
    (ewd, eflags, cookie, pathLen) <-
        PR.fromEffect $ A.unsafePinnedAsPtr arr readHeader
    mpth <-
        if pathLen /= 0
        then do
            arr1 <- PR.takeEQ pathLen (A.createOf pathLen)
            -- The name is null terminated and null padded to pathLen bytes.
            let pth = fst (A.breakEndBy_ (== 0) arr1)
            return $ Just (Path.unsafeFromArray pth)
        else return Nothing
    return (RawEvent ewd eflags cookie mpth)

    where

    readHeader (ptr :: Ptr Word8) _ = do
        let len = sizeOf (undefined :: CInt)
        ewd :: CInt <- peek (castPtr ptr)
        eflags <- peekByteOff ptr len
        cookie <- peekByteOff ptr (len + 4)
        pathLen :: Word32 <- peekByteOff ptr (len + 8)
        return (ewd, eflags, cookie, fromIntegral pathLen)

-- | Run the action of a kernel event. Returns the given events of the kernel
-- event followed by the stream returned by the action. If the action raises
-- an 'IOException', the exception follows the events as an 'ItemFailed'.
--
-- The action runs before any of the events is returned. An exception raised
-- by the action itself would reach the reader before the events read from the
-- fd along with this kernel event are saved in the 'Watch', and those events
-- would be lost.
withEffects ::
    [Event] -> IO (Stream IO StreamItem) -> IO (Stream IO StreamItem)
withEffects evs action = do
    r <- try action
    return
        $ S.fromList (map ItemEvent evs)
        `S.append` either (S.fromPure . ItemFailed) id r

-- | Handle a kernel event: change the watches and the map, and return the
-- events to report.
--
-- An event is reported once for each path of its watch descriptor. An
-- IN_IGNORED event removes its watch descriptor from the map, the event
-- itself still reports the root. Events for a watch descriptor that is not in
-- the map are dropped.
--
-- The IN_IGNORED event of a marker reports 'isRootUnwatched' for the paths
-- listed in the marker, and removes the paths and the marker from the map.
--
-- An IN_Q_OVERFLOW event reports 'isEventsLost' for each root, closes the
-- watch and ends the stream.
--
-- An 'IOException' raised while handling the event is returned as an
-- 'ItemFailed' after the events of the kernel event, see 'withEffects'. The
-- rest of the handling of that kernel event is skipped.
handleEvent :: Config -> Watch -> RawEvent -> IO (Stream IO StreamItem)
handleEvent cfg wtch (RawEvent ewd eflags cookie mpth) = do
    let wdMapRef = watchWdMap wtch
    wdm <- readIORef wdMapRef
    let mkEvent wd (root, rel) =
            Event
                { eventWd = wd
                , eventFlags = eflags
                , eventCookie = cookie
                , eventRoot = root
                , eventRelPath = rel
                , eventScanned = False
                }
        scanItem (ScanFound ScannedEntry{..}) =
            ItemEvent
                (mkEvent ewd sePath)
                    { eventFlags =
                        if seIsDir then iN_CREATE .|. iN_ISDIR else iN_CREATE
                    , eventCookie = 0
                    , eventScanned = True
                    }
        scanItem (ScanDenied pRep) = ItemEvent (synthEvent (SynthDenied pRep))
        scanItem (ScanFailed e) = ItemFailed e
        mkUnwatched (wd, pRep) = synthEvent (SynthUnwatched wd pRep)
        isIgnored = eflags .&. iN_IGNORED /= 0
        events = S.fromList . map ItemEvent
    -- The kernel event queue overflowed and events were dropped. The overflow
    -- event is not tied to a watch, its wd is -1. Report one overflow event
    -- per watch root, so that the consumer knows that events may be missing
    -- for every root, close the watch and end the stream. The events after
    -- the overflow event are not reported, see "Overflow: the stream ends" in
    -- dev/design.linux.md.
    -- The roots are the paths without a sub path; a path with a sub path is a
    -- subdirectory added under a recursive root. Marker entries are skipped,
    -- they are not watched paths.
    if eflags .&. iN_Q_OVERFLOW /= 0
    then do
        let lost =
                [ mkEvent (fromIntegral wd) (wpRep wp)
                | (wd, WdUser wps) <- wdEntries wdm
                , wp <- wps
                , isNothing (getRepSub (wpRep wp))
                ]
        closeWatch wtch
        return $ events lost `S.append` S.fromPure ItemEnd
    else
        case lookupWd (fromIntegral ewd) wdm of
            Nothing -> return S.nil
            Just (WdMarker paths)
                | isIgnored -> do
                    removed <-
                        atomicModifyIORef' wdMapRef
                            $ removeListed paths . deleteWd (fromIntegral ewd)
                    return
                        $ events
                            [ mkEvent (fromIntegral wd) pRep
                            | (wd, pRep) <- removed
                            ]
                | otherwise -> return S.nil
            Just (WdUser wps) -> do
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
                    isDirDeleted = isDirEv && eflags .&. iN_DELETE /= 0
                    -- The paths removed without a marker are reported
                    -- after the event and its scan.
                    effectIf c action = if c then action else return []
                    unwatchAll = fmap concat . mapM (unwatchSubtree wtch)
                    -- The parent of a subdirectory reports its deletion and
                    -- move as IN_DELETE and IN_MOVED_FROM, the
                    -- IN_DELETE_SELF and IN_MOVE_SELF events are reported
                    -- for the roots only.
                    isSelfEvent =
                        eflags .&. (iN_DELETE_SELF .|. iN_MOVE_SELF) /= 0
                    -- The events added by 'watchFlags' only.
                    unrequested =
                        (recursiveEvents .|. rootEvents)
                            .&. complement (createFlags cfg)
                    isUnrequested = eflags .&. unrequested /= 0
                    isReported wp =
                        not isSelfEvent || isNothing (getRepSub (wpRep wp))
                    reported =
                        [mkEvent ewd (wpRep wp) | wp <- wps1, isReported wp]
                withEffects (if isUnrequested then [] else reported) $ do
                    -- See the NOTE above 'markPending'.
                    removedPending <-
                        effectIf
                            (watchRec cfg && (isDirMovedOut || isDirDeleted))
                            $ atomicModifyIORef' wdMapRef
                                (takeNamedPending live)
                                >>= unwatchAll
                    when (watchRec cfg && eflags .&. iN_MOVE_SELF /= 0)
                        $ atomicModifyIORef' wdMapRef
                        $ \m -> (clearPending (fromIntegral ewd) m, ())
                    -- See the NOTE above 'clearMoved'.
                    when (watchRec cfg && isDirMovedOut)
                        $ atomicModifyIORef' wdMapRef
                        $ \m -> (markMoved cookie live m, ())
                    -- Assumes that IN_MOVED_TO is queued before
                    -- IN_MOVE_SELF, see the IMPORTANT note above
                    -- 'clearMoved'.
                    removedMoved <-
                        effectIf (watchRec cfg && eflags .&. iN_MOVE_SELF /= 0)
                            $ unwatchAll
                                [ wpRep wp
                                | wp <- wps
                                , not (wpRemoved wp)
                                , isJust (wpMoveCookie wp)
                                ]
                    removedRoots <-
                        effectIf (eflags .&. iN_MOVE_SELF /= 0)
                            $ unwatchGoneRoots cfg wtch
                                [ (fromIntegral ewd, wpRep wp)
                                | wp <- wps
                                , not (wpRemoved wp)
                                , isNothing (getRepSub (wpRep wp))
                                ]
                    -- The watch of a new directory is added before its
                    -- event is delivered. The watches of the tree under it
                    -- are added when the stream of the scan is read, before
                    -- the next event is parsed. If the entries are not
                    -- reported, all the watches are added now.
                    (removedOut, scan) <-
                        if watchRec cfg && isDirCreated
                        then
                            (,) [] . concatStreams
                                <$> mapM (addDirWithScan cfg wtch) live
                        else if watchRec cfg && isDirMovedIn
                        then movedInDir cfg wtch cookie live
                        else return ([], S.nil)
                    -- The denied subdirectories and the failures are
                    -- reported even if the entries are not.
                    scanned <-
                        if reportsScan cfg
                        then return scan
                        else
                            fmap S.fromList
                                $ S.toList
                                $ S.filter (not . isFound) scan
                    when isIgnored
                        $ atomicModifyIORef' wdMapRef
                        $ \m -> (deleteWd (fromIntegral ewd) m, ())
                    let removed =
                            removedPending ++ removedMoved ++ removedRoots
                                ++ removedOut
                    return
                        $ fmap scanItem scanned
                        `S.append` events (map mkUnwatched removed)

    where

    isFound (ScanFound _) = True
    isFound _ = False

foreign import ccall unsafe "unistd.h read"
    c_read :: CInt -> Ptr Word8 -> CSize -> IO CSsize

-- | Read the events available on the fd of a watch, and take the events
-- recorded by 'recordSynthEvents'. The array is empty if no event is
-- available on the fd. If there are none of either, returns an action that
-- waits for an event. Returns 'Nothing' if the watch is closed.
--
-- The wait is registered, or counted, before the action is returned. The
-- caller must mask async exceptions and must run the action. The action can
-- be interrupted by an async exception.
readEvents :: Watch -> IO (Maybe (Either (IO ()) ([SynthEvent], Array Word8)))
readEvents
    wtch@Watch {watchFd = fd, watchLock = lock, watchSynthEvents = synthVar} =
    modifyMVar lock $ \st ->
        if fsWatchClosed st
        then return (st, Nothing)
        else do
            arr <- MArray.unsafeCreateWithPtr' bufSize readFd
            synth <- takeSynthEvents wtch
            if MArray.byteLength arr /= 0 || not (null synth)
            then
                return
                    (st, Just $ Right (synth, unsafeFreezeWithShrink arr))
            else fmap (Just . Left) <$> register st

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
            else throwErrno "readEvent"

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
            -- The next read returns 'Nothing' if the exception is due to
            -- 'closeWatch'.
            let waitSynth = do
                    ps <- readTVar synthVar
                    when (null ps) retry
                waitTh =
                    Exception.interruptible
                        (atomically (wait `orElse` waitSynth))
                        `catchIOError` (\_ -> return ())
                        `onException` unregister
            return (st, waitTh)
        else do
            -- The lock is never held across a blocking call, taking it
            -- uninterruptibly cannot block for long.
            let waitNonTh =
                    Exception.interruptible (threadWaitRead (Fd fd))
                        `finally` Exception.uninterruptibleMask_
                            (modifyMVar_ lock unregisterNonTh)
            return (st {fsFdWaiters = fsFdWaiters st + 1}, waitNonTh)

    unregisterNonTh st = do
        let st1 = st {fsFdWaiters = fsFdWaiters st - 1}
        if fsWatchClosed st1 && fsFdOpen st1 && fsFdWaiters st1 == 0
        then do
            closeInotifyFd fd
            return st1 {fsFdOpen = False}
        else return st1

-- | Read the next event from a 'Watch', wait for one if none is available.
-- Returns 'Nothing' when the watch is closed, and on every later call.
--
-- While a thread reads the watch, other threads can use 'addRoot',
-- 'removeRoot' and 'closeWatch' on it.
--
-- More than one thread can read the same watch at the same time, using
-- 'readEvent' or 'eventStream'. Each event is returned to exactly one of
-- the readers. The reads run one at a time: while a thread reads, including
-- while it waits for an event, the other readers wait for it to finish.
-- Each reader gets its events in the order in which they occurred. Related
-- events can go to different readers, for example, the 'isMovedFrom' and
-- 'isMovedTo' events of a rename, or the 'isCreated' event of a directory
-- and the 'isScanEvent' events of its entries. When the watch is closed,
-- every reader gets 'Nothing'.
--
-- A read that is interrupted by an async exception while it waits for an
-- event, for example, by a timeout, does not lose any event. The next read
-- returns the first event not yet read. The scan of a new directory
-- continues in the next read, see 'setScanNewDirs'.
--
-- If the read raises an 'IOException', for example, when a directory
-- cannot be added because the process has reached the
-- @max_user_watches@ limit or has no free file descriptor, the operation that
-- failed is not tried again. The directory is not watched, and the next
-- read continues after it.
--
-- /Pre-release/
--
readEvent :: Watch -> IO (Maybe Event)
readEvent
    wtch@Watch {watchConfig = cfg, watchUndelivered = undeliveredRef} =
    Exception.mask_ $ do
        r <- takeMVar undeliveredRef
        next (fromMaybe items r)

    where

    fdReads = S.unfoldrM (\() -> fmap (, ()) <$> readEvents wtch) ()

    -- The synthetic events recorded by 'openWatch', 'addRoot' or
    -- 'removeRoot' are reported before the kernel events of the same
    -- read. A read returns whole events, the events of a read are parsed on
    -- their own.
    readOne (Left wait) = S.fromPure (ItemWait wait)
    readOne (Right (synth, arr)) =
        S.append synthEvents kernelEvents

        where

        synthEvents = S.fromList (map (ItemEvent . synthEvent) synth)

        -- 'handleEvent' returns a stream for each kernel event: the event
        -- for each of its paths, followed by the entries found by its scan.
        kernelEvents =
            S.concatMapM (handleEvent cfg wtch)
                $ S.catRights
                $ S.parseMany readOneEvent (A.read arr)

    items = S.concatMap readOne fdReads

    save = putMVar undeliveredRef . Just

    -- The rest of the stream is not a pure value, it refers to the open
    -- directories of a scan in progress, and running a part of it changes
    -- the watches. A stream saved before a step cannot be run again after
    -- the step has failed. Therefore:
    --
    -- * The step runs with async exceptions masked uninterruptibly. It
    --   blocks only on the lock of the watch, which is never held across a
    --   blocking call.
    -- * The step does not wait for events. It returns the wait as an item,
    --   the wait runs after the step, and can be interrupted. The rest of
    --   the stream is saved if the wait is interrupted. Otherwise the
    --   'MVar' stays empty during the wait, other readers of the watch
    --   wait for the 'MVar', not for the fd.
    -- * An 'IOException' is returned as an item, by 'handleEvent' and by the
    --   stream of a scan, and is raised after the rest of the stream is
    --   saved.
    --
    -- Any other exception leaves the stream in an unknown state. It is
    -- saved in place of the stream, every later read raises it again.
    next stream = do
        r <- try (Exception.uninterruptibleMask_ (S.uncons stream))
        case r of
            Left (e :: SomeException) -> do
                -- Async exceptions thrown by other threads do not reach
                -- here, they are masked while the step runs. Only an
                -- exception raised by the step itself does, for example,
                -- an 'IOException' from the read of the inotify fd, an
                -- 'ErrorCall', or a 'StackOverflow', which the RTS raises
                -- even when async exceptions are masked.
                --
                -- The watch cannot be recovered after such an exception,
                -- it has to be closed and opened again.
                save (S.concatEffect (throwIO e))
                throwIO e
            Right Nothing -> save S.nil >> return Nothing
            -- An async exception that arrived during the step is raised
            -- when 'mask_' ends, and the event is lost. See
            -- dev/design-issues.md.
            Right (Just (ItemEvent ev, rest)) ->
                save rest >> return (Just ev)
            -- Similarly, the async exception replaces the 'IOException'
            -- when 'mask_' ends, and the failure is not reported.
            Right (Just (ItemFailed e, rest)) -> save rest >> ioError e
            Right (Just (ItemEnd, _)) -> save S.nil >> return Nothing
            Right (Just (ItemWait wait, rest)) -> do
                wait `onException` save rest
                next rest

-- | Read the events from a 'Watch' as a stream, using 'readEvent'. The
-- stream does not close the watch, use 'closeWatch' to close it. The stream
-- ends when the watch is closed.
--
-- A stream can stop reading before the watch is closed. The next stream on
-- the watch starts with the first event not yet read.
--
-- More than one stream can read the same watch at the same time. See
-- 'readEvent' for how the events are divided among the readers, and for
-- async exceptions and failures.
--
-- /Pre-release/
--
eventStream :: Watch -> Stream IO Event
eventStream wtch = S.unfoldrM (\() -> fmap (, ()) <$> readEvent wtch) ()

-- XXX We should not go across the mount points of network file systems or file
-- systems that are known to not generate any events.

-- | Start monitoring a list of file system paths for file system events with
-- the supplied configuration operation over the 'defaultConfig'. The
-- paths could be files or directories. When recursive mode is set and the
-- path is a directory, the whole directory tree under it is watched
-- recursively. Monitoring starts from the current time onwards.
--
-- See the Linux __inotify__ man page for more details.
--
-- @
-- watchWith
--      ('setFollowSymLinks' True . 'setExcludeUnlinked' False)
--      [Path.fromString "dir"]
-- @
watchWith :: (Config -> Config) -> NonEmpty Path -> Stream IO Event
watchWith f paths = S.bracketIO before after eventStream

    where

    cfg = f defaultConfig
    before = openWatch cfg paths
    after = closeWatch

-- | Same as 'watchWith' using 'defaultConfig' and recursive mode.
--
-- >>> watchRecursive = watchWith (setRecursiveMode True)
--
-- See "Streamly.FileSystem.Event.Linux" for the performance and limitations of
-- a recursive watch on Linux.
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
-- The event is reported once for each watch root. After these events the
-- watch is closed and the event stream ends, in both modes. The events that
-- the kernel queued after the overflow are not reported, they can have gaps
-- that no event reports. To continue watching, open a new watch on the same
-- paths and then scan the tree. Opening a watch does not generate any events,
-- see 'setRecursiveMode'.
isEventsLost :: Event -> Bool
isEventsLost = getFlag iN_Q_OVERFLOW

-- | In recursive mode, a directory in the tree could not be added to the
-- watch or listed because of missing permission. A directory needs the read
-- and the search (execute) permissions. The directory is skipped and the
-- watch continues. The tree under it is not watched, the events in it are
-- not reported, except possibly the events of its direct entries. 'isDir' is
-- true for this event.
--
-- The directory is not added again when its permissions change. It is added
-- if it is moved. The event may be reported more than once for the
-- same directory, for example, when it is created while its parent is being
-- scanned.
--
-- A watch root without permission raises an exception instead.
--
-- /Occurs only in recursive mode/
isAccessDenied :: Event -> Bool
isAccessDenied = getFlag accessDeniedFlag

-------------------------------------------------------------------------------
-- Events affecting the watched path only
-------------------------------------------------------------------------------

-- Compare with isRootChanged on macOS. isRootChanged includes all these cases.
--
-- | A path was removed from the watch explicitly using 'removeRoot' or
-- automatically (file was deleted or renamed, or filesystem was unmounted).
--
-- Note that in recursive watch mode all the subdirectories are watch roots,
-- therefore, they will all generate this event.
--
-- This event refers to the directory that was at the path, it does not mean
-- that the path is unwatched at the time the event is read. In recursive mode,
-- in @mv d \/outside; mkdir d@ this event of the old @d@ is emitted after the
-- create event of the new @d@, and the new @d@ is watched. Similarly, in
-- @mkdir d; mv d e; mv f d@ with @f@ watched, this event of the new directory
-- is emitted for the path @d@ after @f@ is renamed to @d@, and @d@ stays
-- watched.
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
-- The root is removed from the watch, and 'isRootUnwatched' is reported
-- later. The root is removed whether or not this event is enabled by
-- 'setRootMoved'.
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
-- In recursive mode, this is also true for an entry found by a scan, for which
-- 'isScanEvent' is true. The entry may not be new, the entries of a directory
-- moved into the tree are reported this way. See 'setScanNewDirs'.
--
-- /Occurs only for an object inside the watched directory/
isCreated :: Event -> Bool
isCreated = getFlag iN_CREATE

-- | File/directory deleted from watched directory.
--
-- /Occurs only for an object inside the watched directory/
isDeleted :: Event -> Bool
isDeleted = getFlag iN_DELETE

-- | Generated for the original path when an object is moved from under a
-- monitored directory.
--
-- A directory that moves out of the watched tree reports this event. It is
-- reported only for the directory and not for any of the entries under it. The
-- watcher must remove everthing under that tree from its view.
--
-- In recursive mode, a directory renamed within the watched tree reports
-- 'isMovedTo' for the new path later, and all the entries under it are
-- reported after that as 'isCreated' events with 'isScanEvent' true.
--
-- /Occurs only for an object inside the watched directory/
isMovedFrom :: Event -> Bool
isMovedFrom = getFlag iN_MOVED_FROM

-- | Generated for the new path when an object is moved under a monitored
-- directory.
--
-- In recursive mode, all the entries under a directory moved into the tree, or
-- renamed within it, are reported in the event stream, as events with
-- 'isCreated' and 'isScanEvent' true after this event, see 'setScanNewDirs'.
-- In contrast, a directory moved out is not reported this way, see
-- 'isMovedFrom'.
--
-- A directory renamed twice before its events are read reports this event and
-- then 'isMovedFrom' for the intermediate path, in @mv a b; mv b c@ the path
-- @b@ reports both, although it did not exist when the events are read.
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
-- event. Such an event says that the entry existed when it was scanned, it may
-- not be new. The entries of a directory moved in or renamed are reported this
-- way. Apply such events as update, the path may be already known.
--
-- Two scans can report the same path. Every create or move event of a
-- directory scans it, even if an earlier scan has added it. In
-- @mv a b; mkdir b\/x; touch b\/x\/f@, all done before the events are read,
-- the scan on the rename reports @b\/x@ and @b\/x\/f@, and the scan on the
-- create event of @x@, read later, reports @b\/x\/f@ again.
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
--
-- The format of the output is for debugging only, it cannot be relied upon and
-- can change at any time.
showEvent :: Event -> String
showEvent ev@Event{..} =
       "--------------------------"
    ++ "\nWd = " ++ show eventWd
    ++ "\nRoot = " ++ show (Path.toString $ getRoot ev)
    ++ "\nPath = " ++ maybe "" (show . Path.toString) (getRelPath ev)
    ++ "\nCookie = " ++ show (getCookie ev)
    ++ "\nFlags " ++ show eventFlags

    ++ showev isEventsLost "Overflow"
    ++ showev isAccessDenied "AccessDenied"

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
