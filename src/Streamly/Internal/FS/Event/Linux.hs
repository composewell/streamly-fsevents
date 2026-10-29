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

#include "config.h"

#if HAVE_DECL_IN_EXCL_UNLINK
module Streamly.Internal.FS.Event.Linux
    (
    -- * Subscribing to events

    -- ** Default configuration
      Config (..)
    , defaultConfig

    -- ** Watch Behavior
    , setRecursiveMode
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

    -- ** Exception Conditions
    , isEventsLost

    -- * Debugging
    , showEvent
    )
where

import Control.Exception (finally)
import Control.Monad (when)
import Data.Bits ((.|.), (.&.), complement)
import Data.IntMap.Lazy (IntMap)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (isNothing)
#if !(MIN_VERSION_base(4,13,0))
import Data.Semigroup (Semigroup(..))
#endif
import Data.Word (Word8, Word32)
import Foreign.C.Error
    ( eINVAL, getErrno, throwErrno, throwErrnoIfMinus1
    , throwErrnoIfMinus1_
    )
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt(..), CUInt(..))
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek, peekByteOff, peekElemOff, sizeOf)
import GHC.IO.Device (IODeviceType(Stream))
import GHC.IO.FD (fdFD, FD(..))
import GHC.IO.Handle.FD (mkHandleFromFD)
import Streamly.Data.Stream (Stream)
import Streamly.Data.Parser (Parser)
import System.Directory (doesDirectoryExist)
import System.IO (Handle, hClose, IOMode(ReadMode))
import GHC.IO.Handle.FD (handleToFd)

import Streamly.Internal.Data.Array (byteLength)
import Streamly.Internal.FileSystem.Path (Path)

import qualified Data.IntMap.Lazy as Map
import qualified Data.List.NonEmpty as NonEmpty
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Data.Array as A (createOf)
import qualified Streamly.Data.Stream as S
import qualified Streamly.Data.Unfold as UF
import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Internal.FileSystem.Path as Path

import qualified Streamly.Internal.Data.Array as A
    ( asCStringUnsafe, unsafePinnedAsPtr
    )
import qualified Streamly.Internal.FileSystem.DirIO as Dir (readDirs)
import qualified Streamly.Internal.Data.Parser as PR
    (takeEQ, fromEffect, fromFold)

-------------------------------------------------------------------------------
-- Subscription to events
-------------------------------------------------------------------------------

-- | Watch configuration, used to specify the events of interest and the
-- behavior of the watch.
--
-- /Pre-release/
--
data Config = Config
    { watchRec :: Bool
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
-- /default: False/
--
-- /Pre-release/
--
setRecursiveMode :: Bool -> Config -> Config
setRecursiveMode recursive cfg@Config{} = cfg {watchRec = recursive}

foreign import capi
    "sys/inotify.h value IN_DONT_FOLLOW" iN_DONT_FOLLOW :: Word32

-- | If the pathname to be watched is a symbolic link then watch the target of
-- the symbolic link instead of the symbolic link itself.
--
-- Note that the path location in the events is through the original symbolic
-- link path rather than the resolved path.
--
-- /default: True/
--
-- /Pre-release/
--
setFollowSymLinks :: Bool -> Config -> Config
setFollowSymLinks s = setFlag iN_DONT_FOLLOW (not s)

foreign import capi
    "sys/inotify.h value IN_EXCL_UNLINK" iN_EXCL_UNLINK :: Word32

-- | Do not report the events of a file after it is unlinked from the watched
-- directory. A process can keep a file open after the file is deleted, its
-- reads and writes generate events, the events have the deleted path.
--
-- macOS does not report the events of an unlinked file.
--
-- /default: True/
--
-- /Pre-release/
--
setExcludeUnlinked :: Bool -> Config -> Config
setExcludeUnlinked = setFlag iN_EXCL_UNLINK

-- | Same as 'setExcludeUnlinked'.
--
-- /Pre-release/
--
{-# DEPRECATED setUnwatchMoved "Use setExcludeUnlinked" #-}
setUnwatchMoved :: Bool -> Config -> Config
setUnwatchMoved = setExcludeUnlinked

#if HAVE_DECL_IN_MASK_CREATE
foreign import capi
    "sys/inotify.h value IN_MASK_CREATE" iN_MASK_CREATE :: Word32
#else
-- IN_MASK_CREATE was added in Linux 4.18, older C library headers do not
-- define it. The value is from the Linux kernel header
-- include/uapi/linux/inotify.h.
iN_MASK_CREATE :: Word32
iN_MASK_CREATE = 0x10000000
#endif

foreign import capi
    "sys/inotify.h value IN_MASK_ADD" iN_MASK_ADD :: Word32

-- | What to do if a watch already exists when 'openWatch' or 'addToWatch' is
-- called for a path.
--
-- /Pre-release/
--
data WhenExists =
      AddIfExists -- ^ Do not set an existing setting to 'False' only set to 'True'
    | ReplaceIfExists -- ^ Replace the existing settings with new settings
    | FailIfExists -- ^ Fail the API. Requires Linux 4.18 or later, older
                   -- kernels do not support it.

-- | When adding a new path to the watch, specify what to do if a watch already
-- exists on that path.
--
-- /default: AddIfExists/
--
-- /Pre-release/
--
setWhenExists :: WhenExists -> Config -> Config
setWhenExists val =
    -- inotify_add_watch fails with EINVAL if both IN_MASK_ADD and
    -- IN_MASK_CREATE are set, therefore, we always clear the other one.
    case val of
        AddIfExists -> setFlag iN_MASK_ADD True . setFlag iN_MASK_CREATE False
        ReplaceIfExists ->
            setFlag iN_MASK_ADD False . setFlag iN_MASK_CREATE False
        FailIfExists -> setFlag iN_MASK_CREATE True . setFlag iN_MASK_ADD False

foreign import capi
    "sys/inotify.h value IN_ONESHOT" iN_ONESHOT :: Word32

-- | Watch the object only for one event and then remove it from the watch.
--
-- /default: False/
--
-- /Pre-release/
--
setOneShot :: Bool -> Config -> Config
setOneShot = setFlag iN_ONESHOT

foreign import capi
    "sys/inotify.h value IN_ONLYDIR" iN_ONLYDIR :: Word32

-- | Watch the object only if it is a directory. This provides a race-free way
-- to ensure that the watched object is a directory.
--
-- /default: False/
--
-- /Pre-release/
--
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
--
-- /Pre-release/
--
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
--
-- /Pre-release/
--
setRootMoved :: Bool -> Config -> Config
setRootMoved = setFlag iN_MOVE_SELF

-- | Report when the watched root path itself gets deleted or renamed.
--
-- In recursive mode, the events are reported for the watch roots only.
--
-- /default: False/
--
-- /Pre-release/
--
setRootPathEvents :: Bool -> Config -> Config
setRootPathEvents = setFlag (iN_DELETE_SELF .|. iN_MOVE_SELF)

foreign import capi
    "sys/inotify.h value IN_ATTRIB" iN_ATTRIB :: Word32

-- | Report when the metadata e.g. owner, permission modes, modifications times
-- of an object changes.
--
-- macOS reports the metadata changes. On Linux, @touch@ on an existing file
-- generates 'isAttrsModified' but not 'isModified'.
--
-- /default: True/
--
-- /Pre-release/
--
setAttrsModified :: Bool -> Config -> Config
setAttrsModified = setFlag iN_ATTRIB

foreign import capi
    "sys/inotify.h value IN_ACCESS" iN_ACCESS :: Word32

-- | Report when a file is accessed.
--
-- /default: False/
--
-- /Pre-release/
--
setAccessed :: Bool -> Config -> Config
setAccessed = setFlag iN_ACCESS

foreign import capi
    "sys/inotify.h value IN_OPEN" iN_OPEN :: Word32

-- | Report when a file is opened.
--
-- /default: False/
--
-- /Pre-release/
--
setOpened :: Bool -> Config -> Config
setOpened = setFlag iN_OPEN

foreign import capi
    "sys/inotify.h value IN_CLOSE_WRITE" iN_CLOSE_WRITE :: Word32

-- | Report when a file that was opened for writes is closed.
--
-- /default: False/
--
-- /Pre-release/
--
setWriteClosed :: Bool -> Config -> Config
setWriteClosed = setFlag iN_CLOSE_WRITE

foreign import capi
    "sys/inotify.h value IN_CLOSE_NOWRITE" iN_CLOSE_NOWRITE :: Word32

-- | Report when a file that was opened for not writing is closed.
--
-- /default: False/
--
-- /Pre-release/
--
setNonWriteClosed :: Bool -> Config -> Config
setNonWriteClosed = setFlag iN_CLOSE_NOWRITE

foreign import capi
    "sys/inotify.h value IN_CREATE" iN_CREATE :: Word32

-- | Report when a file is created.
--
-- /default: True/
--
-- /Pre-release/
--
setCreated :: Bool -> Config -> Config
setCreated = setFlag iN_CREATE

foreign import capi
    "sys/inotify.h value IN_DELETE" iN_DELETE :: Word32

-- | Report when a file is deleted.
--
-- /default: True/
--
-- /Pre-release/
--
setDeleted :: Bool -> Config -> Config
setDeleted = setFlag iN_DELETE

foreign import capi
    "sys/inotify.h value IN_MOVED_FROM" iN_MOVED_FROM :: Word32

-- | Report the source of a move.
--
-- /default: True/
--
-- /Pre-release/
--
setMovedFrom :: Bool -> Config -> Config
setMovedFrom = setFlag iN_MOVED_FROM

foreign import capi
    "sys/inotify.h value IN_MOVED_TO" iN_MOVED_TO :: Word32

-- | Report the target of a move.
--
-- /default: True/
--
-- /Pre-release/
--
setMovedTo :: Bool -> Config -> Config
setMovedTo = setFlag iN_MOVED_TO

foreign import capi
    "sys/inotify.h value IN_MODIFY" iN_MODIFY :: Word32

-- | Report when a file is modified.
--
-- /default: True/
--
-- /Pre-release/
--
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
--
-- /Pre-release/
--
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
-- * 'setFollowSymLinks' 'True'
-- * 'setExcludeUnlinked' 'True'
-- * 'setOneShot' 'False'
-- * 'setOnlyDir' 'False'
-- * 'setWhenExists' 'AddIfExists'
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
      setWhenExists AddIfExists
    $ setExcludeUnlinked True
    $ setCreated True
    $ setDeleted True
    $ setMovedFrom True
    $ setMovedTo True
    $ setModified True
    $ setAttrsModified True
    $ Config
        { watchRec = False
        , createFlags = 0
        }

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

-------------------------------------------------------------------------------
-- Open an event stream
-------------------------------------------------------------------------------

-- | A path added to the watch. The same inode can be added via more than one
-- root, for example, a recursive root and a root inside it. All the paths
-- added for an inode share the same watch descriptor.
data WatchPath = WatchPath
    { wpRep :: PathRep
    -- | 'removeFromWatch' was called for the root, the path reports the
    -- events until its 'isRootUnwatched' event.
    , wpRemoved :: Bool
    }

data WdEntry =
    -- | The paths watched via the watch descriptor, in the order added.
      WdPaths [WatchPath]
    -- | A private watch used as a marker in the event queue. When its
    -- IN_IGNORED event is read, the listed paths, which were removed from a
    -- shared watch descriptor, report 'isRootUnwatched'.
    | WdMarker [(Int, PathRep)]

-- | A handle for a watch.
data Watch =
    Watch
        Handle                  -- File handle for the watch
        (IORef
            (IntMap             -- Key is the watch descriptor
                WdEntry
            )
        )

-- Instead of using the watch descriptor we can provide APIs that use the path
-- itself to identify the watch. That will require us to maintain a map from wd
-- to path in the Watch handle.

newtype WD = WD CInt deriving Show

-- XXX Use inotify_init1? and IN_NONBLOCK?
foreign import ccall unsafe
    "sys/inotify.h inotify_init" c_inotify_init :: IO CInt

-- | Create a 'Watch' handle. 'addToWatch' can be used to add paths being
-- monitored by this watch.
--
-- /Pre-release/
--
createWatch :: IO Watch
createWatch = do
    rawfd <- throwErrnoIfMinus1 "createWatch" c_inotify_init
    -- We cannot use fdToHandle because it calls fdStat, and fdStat raises an
    -- "unknown file type" exception for the inotify fd. The inotify fd is an
    -- anonymous inode and its st_mode has no file type bits.
    --
    -- mkFD calls fdStat only when its stat argument is Nothing, and locks the
    -- fd only when the fd type is RegularFile. Locking a regular file leads to
    -- a "resource busy" error in some test cases.
    --
    -- XXX "mkFD rawfd ReadMode (Just (Stream, 0, 0)) False False" neither
    -- calls fdStat nor locks the fd, we can use it instead of constructing
    -- the FD below.
    let fd =
            FD
                { fdFD = rawfd
#if !defined(mingw32_HOST_OS)
                , fdIsNonBlocking = 0
#else
                , fdIsSocket_ = 0
#endif
              }

    -- XXX Do we need non-blocking IO?
    -- With non-blocking IO it fails because GHC-9.10 uses fdStat in
    -- FD.setNonBlockingMode to determine the fd type which fails for the
    -- inotify fd. However, without non-blocking IO "select" has a limitation on
    -- the number of FDs being watched (1024).
    --
    -- XXX Since base-4.20.1 (GHC 9.10.2) setNonBlockingMode uses
    -- statGetType_maybe which does not fail for an unknown file type, so
    -- non-blocking IO may work with newer GHC versions.

    -- mkHandleFromFD does not lock the fd. The fd type Stream is used because
    -- statGetType has no type for an anonymous inode, and the inotify fd is
    -- read like a stream, it is not seekable.
    h <-
        mkHandleFromFD
           fd
           Stream
           ("<createWatch fd: " ++ show fd ++ ">")
           ReadMode
           False    -- use non-blocking IO
           Nothing -- TextEncoding (binary)
    emptyMapRef <- newIORef Map.empty
    return $ Watch h emptyMapRef

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

-- | @addToWatch cfg watch root subpath@ adds @subpath@ to the list of paths
-- being monitored under @root@ via the watch handle @watch@.  @root@ must be
-- an absolute path and @subpath@ must be relative to @root@.
--
-- If the same directory is added via two different roots, for example, a
-- recursive root and a root inside it, its events are reported once for each
-- root.
--
-- /Pre-release/
--
addToWatch :: Config -> Watch -> PathRep -> IO ()
addToWatch cfg@Config{..} watch0@(Watch handle wdMapRef) prep = do
    -- XXX if the watch is added by the scan and not via an event we can
    -- generate a create event assuming that the create may have been lost. We
    -- can also mark in the map that this entry was added by the scan. So if an
    -- actual create event later comes and tries to add this again then we can
    -- ignore that and drop the create event to avoid duplicate create, because
    -- we have already emitted it.
    --
    -- When a directory is added by the scan we should also emit create events
    -- for files that may have got added to the dir. However, such create
    -- events may get duplicated because of a race between the scan generated
    -- versus real events.
    --
    -- Or we may distinguish between scan generated events and real events so
    -- that the application can assume that other events may been lost and
    -- handle it. For example, if it is a dir create the application can read
    -- the dir to scan the files in it.
    --
    let absPath = combinePathRep prep
    --    showPathRep (root, Nothing) = "root = " ++ Path.toString root
    --    showPathRep (root, Just sub) =
    --        "root = " ++ Path.toString root ++ " sub = " ++ Path.toString sub
    -- putStrLn $ showPathRep prep ++ " absPath = " ++ Path.toString absPath

    fd <- handleToFd handle

    -- XXX we need to tolerate an error where we are adding a watch for a
    -- non-existing file because the file may have got deleted by the time we
    -- added the watch. Perhaps we can have a flag in config for this and keep
    -- the default value to tolerate the error.
    --
    -- XXX The file may have even got deleted and then recreated which we will
    -- never get to know, document this.
    --
    -- XXX If the caller does not have permission to read the path,
    -- inotify_add_watch fails with EACCES and we throw an exception. We could
    -- add a config option to ignore such errors, like the options for
    -- ignoring errors when reading a directory.
    wd <- A.asCStringUnsafe (Path.toArray absPath) $ \pathPtr ->
            throwErrnoIfMinus1 ("addToWatch: " ++ Path.toString absPath) $
                c_inotify_add_watch (fdFD fd) pathPtr (CUInt createFlags)

    -- We add the parent first so that we start getting events for any new
    -- creates and add the new subdirectories on creates while we are adding
    -- the children.
    --
    -- A directory created while its parent is being scanned is added twice,
    -- once by the scan and once on its create event. The second add finds the
    -- path already watched and does not scan the directory again.
    --
    -- The map update must be atomic because more than one thread modifies
    -- the map. 'addToWatch' and 'removeFromWatch' can be called from any
    -- thread while the thread reading the event stream adds subdirectories
    -- on create events and removes watch descriptors on IN_IGNORED events. A
    -- separate readIORef and writeIORef could lose an update made by another
    -- thread in between. 'insertPath' also checks whether the path is already
    -- watched and inserts it in the same step, so when a scan and a create
    -- event in different threads add the same directory, only one of them
    -- gets 'isNew' as 'True' and scans the directory.
    isNew <- atomicModifyIORef' wdMapRef $ insertPath (fromIntegral wd) prep

    -- Now add the children. If we missed any creates while we were adding the
    -- parent, this will make sure they are added too.
    --
    -- XXX Ensure that we generate events that we may have missed while we were
    -- adding the dirs. That may generate spurious events though.
    --
    -- XXX Are the following comments obselete?
    -- XXX readDirs currently uses paths as String, we need to convert it
    -- to "/" separated by byte arrays.
    -- XXX Need a FileSystem.Stat module to remove this
    pathIsDir <- doesDirectoryExist (Path.toString absPath)
    when (isNew && watchRec && pathIsDir) $ do
        let f = addToWatch cfg watch0 . addSub prep
            in S.fold (FL.drainMapM f) $ Dir.readDirs id absPath

foreign import ccall unsafe
    "sys/inotify.h inotify_rm_watch" c_inotify_rm_watch
        :: CInt -> CInt -> IO CInt

foreign import ccall unsafe "unistd.h pipe" c_pipe :: Ptr CInt -> IO CInt

foreign import ccall unsafe "unistd.h close" c_close :: CInt -> IO CInt

-- | Mark the paths of a root as removed. Returns the watch descriptors whose
-- paths are now all removed, and the removed paths of the other watch
-- descriptors, both in the ascending order of watch descriptors.
markRemoved ::
    Path -> IntMap WdEntry -> (IntMap WdEntry, ([Int], [(Int, PathRep)]))
markRemoved root m =
    let ((wds, shared), m1) = Map.mapAccumWithKey step ([], []) m
     in (m1, (reverse wds, reverse shared))

    where

    isRemoving wp = not (wpRemoved wp) && eqPath (getRepRoot (wpRep wp)) root
    setRemoved wp = if isRemoving wp then wp {wpRemoved = True} else wp

    step acc _ e@(WdMarker _) = (acc, e)
    step acc@(wds, shared) wd (WdPaths wps) =
        let removing = filter isRemoving wps
            wps1 = map setRemoved wps
            shared1 = reverse [(wd, wpRep wp) | wp <- removing] ++ shared
         in if null removing
            then (acc, WdPaths wps)
            else if all wpRemoved wps1
            then ((wd : wds, shared), WdPaths wps1)
            else ((wds, shared1), WdPaths wps1)

-- | Queue a marker IN_IGNORED event after the events already queued, by
-- adding and removing a watch on a private pipe. The pipe is watched via
-- @\/proc\/self\/fd@, the user cannot add the pipe to the watch, therefore,
-- the watch descriptor is not shared with any user path.
addRemovalMarker ::
    CInt -> IORef (IntMap WdEntry) -> [(Int, PathRep)] -> IO ()
addRemovalMarker fd wdMapRef paths =
    allocaArray 2 $ \fds -> do
        throwErrnoIfMinus1_ err $ c_pipe fds
        rfd <- peekElemOff fds 0
        wfd <- peekElemOff fds 1
        let path = "/proc/self/fd/" ++ show rfd
        flip finally (c_close rfd >> c_close wfd) $ do
            wd <- withCString path $ \pathPtr ->
                throwErrnoIfMinus1 err
                    $ c_inotify_add_watch fd pathPtr (CUInt iN_ACCESS)
            -- The marker must be in the map before its IN_IGNORED is read.
            atomicModifyIORef' wdMapRef
                $ \m -> (Map.insert (fromIntegral wd) (WdMarker paths) m, ())
            throwErrnoIfMinus1_ err $ c_inotify_rm_watch fd wd

    where

    err = "removeFromWatch: marker " ++ show (map fst paths)

-- | Remove an absolute root path from a 'Watch', if a path was moved after
-- adding you need to provide the original path which was used to add the
-- Watch.
--
-- The events already queued for the root are reported, followed by an
-- 'isRootUnwatched' event for each path watched under the root. A directory
-- that is also watched via another root remains watched for that root.
--
-- Removing a root that shares a directory with another root requires
-- @\/proc@ to be mounted, throw an exception if it is not.
--
-- /Pre-release/
--
removeFromWatch :: Watch -> Path -> IO ()
removeFromWatch (Watch handle wdMapRef) root = do
    fd <- handleToFd handle
    -- The paths stay in the map until their 'isRootUnwatched' event is read,
    -- so that the events already queued for the removed root, including
    -- 'isRootUnwatched', report the root.
    --
    -- If all the paths of a watch descriptor are removed we remove the
    -- kernel watch, the kernel queues an IN_IGNORED event for it. Otherwise,
    -- the kernel watch is still needed for the other paths, we queue a
    -- marker event instead.
    (wds, shared) <- atomicModifyIORef' wdMapRef (markRemoved root)
    mapM_ (rmWatch (fdFD fd)) wds
    when (not (null shared)) $ addRemovalMarker (fdFD fd) wdMapRef shared

    where

    -- EINVAL means that the kernel has already removed the watch, for
    -- example, when the root was deleted.
    rmWatch fd wd = do
        let err = "removeFromWatch: " ++ show (Path.toString root)
        r <- c_inotify_rm_watch fd (fromIntegral wd)
        when (r == -1) $ do
            errno <- getErrno
            when (errno /= eINVAL) $ throwErrno err

-- | Given a 'Config' and list of @paths@ ("/" separated byte arrays) start
-- monitoring the paths for file system events. Returns a 'Watch' handle which
-- can then be used to read the event stream or to close the watch.
--
-- /Pre-release/
--
openWatch :: Config -> NonEmpty Path -> IO Watch
openWatch cfg paths = do
    w <- createWatch
    mapM_ (\root -> addToWatch cfg w (root, Nothing)) $ NonEmpty.toList paths
    return w

-- | Close a 'Watch' handle.
--
-- /Pre-release/
--
closeWatch :: Watch -> IO ()
closeWatch (Watch h _) = hClose h

-------------------------------------------------------------------------------
-- Raw events read from the watch file handle
-------------------------------------------------------------------------------

-- | Identifies the 'isMovedFrom' and 'isMovedTo' events of the same move
-- operation, see 'getCookie'.
--
-- /Pre-release/
--
newtype Cookie = Cookie Word32 deriving (Show, Eq)

-- | An Event generated by the file system. Use the accessor functions to
-- examine the event.
--
-- /Pre-release/
--
data Event = Event
   { eventWd :: CInt
   , eventFlags :: Word32
   , eventCookie :: Word32
   , eventRoot :: Path
   , eventRelPath :: Maybe Path
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
    -> [(Int, PathRep)]
    -> IntMap WdEntry
    -> (IntMap WdEntry, [(Int, PathRep)])
removeMarker marker paths m0 =
    let (m1, removed) = foldl step (Map.delete marker m0, []) paths
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

-- XXX We can perhaps use parseD monad instance for fusing with parseMany? Need
-- to measure the perf.
--
-- An event is reported once for each path of its watch descriptor. An
-- IN_IGNORED event removes its watch descriptor from the map, the event
-- itself still reports the root. Events for a watch descriptor that is not in
-- the map are dropped.
--
-- The IN_IGNORED event of a marker reports 'isRootUnwatched' for the paths
-- listed in the marker, and removes the paths from the map.
--
{-# INLINE readOneEvent #-}
readOneEvent :: Config -> Watch -> Parser Word8 IO [Event]
readOneEvent cfg  wt@(Watch _ wdMapRef) = do
    let headerLen = sizeOf (undefined :: CInt) + 12
    arr <- PR.takeEQ headerLen (A.createOf headerLen)
    (ewd, eflags, cookie, pathLen) <- PR.fromEffect $ A.unsafePinnedAsPtr arr readHeader
    -- XXX need the "initial" in parsers to return a step type so that "take 0"
    -- can return without an input. otherwise if pathLen is 0 we will keep
    -- waiting to read one more char before we return this event.
    mpth <-
        if pathLen /= 0
        then do
            -- XXX takeEndBy_ drops the separator so assumes a null
            -- terminated path, we should use a takeWhile nested inside a
            -- takeP
            pth <-
                PR.fromFold
                    $ FL.takeEndBy_ (== 0)
                    $ FL.take pathLen (A.createOf pathLen)
            let remaining = pathLen - byteLength pth - 1
            when (remaining /= 0) $ PR.takeEQ remaining FL.drain
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
    then return
            [ mkEvent (fromIntegral wd) (wpRep wp)
            | (wd, WdPaths wps) <- Map.toList wdm
            , wp <- wps
            , isNothing (getRepSub (wpRep wp))
            ]
    else
        case Map.lookup (fromIntegral ewd) wdm of
            Nothing -> return []
            Just (WdMarker paths)
                | isIgnored -> do
                    removed <-
                        PR.fromEffect
                            $ atomicModifyIORef' wdMapRef
                            $ removeMarker (fromIntegral ewd) paths
                    return
                        [ mkEvent (fromIntegral wd) prep
                        | (wd, prep) <- removed
                        ]
                | otherwise -> return []
            Just (WdPaths wps) -> do
                let addName wp =
                        wp {wpRep = maybe id (flip addSub) mpth (wpRep wp)}
                    wps1 = map addName wps
                -- Check for "ISDIR" first because it is less likely
                let isDirCreate =
                        eflags .&. iN_ISDIR /= 0 && eflags .&. iN_CREATE /= 0
                -- Do not add the new directories of a removed root.
                when (watchRec cfg && isDirCreate)
                    $ PR.fromEffect
                    $ mapM_ (addToWatch cfg wt . wpRep)
                    $ filter (not . wpRemoved) wps1
                when isIgnored
                    $ PR.fromEffect
                    $ atomicModifyIORef' wdMapRef
                    $ \m -> (Map.delete (fromIntegral ewd) m, ())
                -- XXX Handle IN_DELETE, IN_MOVED_FROM, IN_MOVED_TO
                -- What if a large dir tree gets moved in to our hierarchy? Do
                -- we get a single event for the top level dir in this case?
                --
                -- The parent of a subdirectory reports its deletion and move
                -- as IN_DELETE and IN_MOVED_FROM, the IN_DELETE_SELF and
                -- IN_MOVE_SELF events are reported for the roots only.
                let isSelfEvent =
                        eflags .&. (iN_DELETE_SELF .|. iN_MOVE_SELF) /= 0
                    isReported wp =
                        not isSelfEvent || isNothing (getRepSub (wpRep wp))
                return [mkEvent ewd (wpRep wp) | wp <- wps1, isReported wp]

    where

    readHeader (ptr :: Ptr Word8) _ = do
        let len = sizeOf (undefined :: CInt)
        ewd :: CInt <- peek (castPtr ptr)
        eflags <- peekByteOff ptr len
        cookie <- peekByteOff ptr (len + 4)
        pathLen :: Word32 <- peekByteOff ptr (len + 8)
        return (ewd, eflags, cookie, fromIntegral pathLen)

-- | Read the events from a 'Watch' as a stream. The 'Config' must be the one
-- used to open the watch. The stream does not close the watch, use
-- 'closeWatch' to close it.
--
-- /Pre-release/
--
watchToStream :: Config -> Watch -> Stream IO Event
watchToStream cfg wt@(Watch handle _) = do
    -- Do not use too small a buffer. As per inotify man page:
    --
    -- The behavior when the buffer given to read(2) is too small to return
    -- information about the next event depends on the kernel version: in
    -- kernels before 2.6.21, read(2) returns 0; since kernel 2.6.21, read(2)
    -- fails with the error EINVAL.  Specifying a buffer of size
    --
    --          sizeof(struct inotify_event) + NAME_MAX + 1
    --
    -- will be sufficient to read at least one event.
    S.unfoldEach UF.fromList
        $ S.catRights
        $ S.parseMany (readOneEvent cfg wt)
        $ S.unfold FH.reader handle

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
--
-- /Pre-release/
--
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
--
-- /Pre-release/
--
watchRecursive :: NonEmpty Path -> Stream IO Event
watchRecursive = watchWith (setRecursiveMode True)

-- | Same as 'watchWith' using defaultConfig and non-recursive mode.
--
-- >>> watch = watchWith id
--
-- /Pre-release/
--
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
-- Note that if a path was moved after adding to the watch, this will give the
-- original path and not the new path after moving.
--
-- TBD: we can possibly update the watch root on a move self event.
--
-- /Pre-release/
--
getRoot :: Event -> Path
getRoot = eventRoot

-- | Get the file system object path for which the event is generated, relative
-- to the watched root. Returns 'Nothing' if the event is for the watch root
-- itself.
--
-- /Pre-release/
--
getRelPath :: Event -> Maybe Path
getRelPath Event{..} = eventRelPath

-- | Get the absolute file system object path for which the event is generated.
--
-- When the watch root is a symlink, the absolute path returned is via the
-- original symlink and not through the resolved path.
--
-- /Pre-release/
--
getAbsPath :: Event -> Path
getAbsPath ev =
    case getRelPath ev of
        Just relPath -> Path.join (getRoot ev) relPath
        Nothing -> getRoot ev

-- XXX should we use a Maybe?
-- | Cookie is set when a rename occurs. The cookie value can be used to
-- connect the 'isMovedFrom' and 'isMovedTo' events, if both the events belong
-- to the same move operation then they will have the same cookie value.
--
-- /Pre-release/
--
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
-- The kernel generates a single overflow event for all the watch roots. It is
-- reported as one event per watch root, 'getRoot' returns the root and
-- 'getRelPath' returns 'Nothing'.
--
-- /Pre-release/
--
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
--
-- /Pre-release/
--
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
--
-- /Pre-release/
--
isRootDeleted :: Event -> Bool
isRootDeleted = getFlag iN_DELETE_SELF

-- | Watched file/directory was itself moved within the file system.
--
-- In recursive mode, the event is reported for the watch roots only, the
-- move of a subdirectory is reported as 'isMovedFrom' and 'isMovedTo'.
--
-- /Occurs only for a watched path/
--
-- /Pre-release/
--
isRootMoved :: Event -> Bool
isRootMoved = getFlag iN_MOVE_SELF

foreign import capi
    "sys/inotify.h value IN_UNMOUNT" iN_UNMOUNT :: Word32

-- | Filesystem containing watched object was unmounted.  In addition, an
-- 'isRootUnwatched' event will subsequently be generated for the watch
-- descriptor.
--
-- /Occurs only for a watched path/
--
-- /Pre-release/
--
isRootUnmounted :: Event -> Bool
isRootUnmounted = getFlag iN_UNMOUNT

-- | Determine whether the event indicates a change of path of the monitored
-- object itself. Note that the object may become unreachable or deleted after
-- a change of path.
--
-- /Occurs only for a watched path/
--
-- /Pre-release/
--
isRootPathEvent :: Event -> Bool
isRootPathEvent = getFlag (iN_DELETE_SELF .|. iN_MOVE_SELF .|. iN_UNMOUNT)

-------------------------------------------------------------------------------
-- Metadata change Events
-------------------------------------------------------------------------------

-- macOS has multiple APIs for metadata change for different metadata.
--
-- | Determine whether the event indicates inode metadata change for an object
-- contained within the monitored path.
--
-- Metadata change may include, permissions (e.g., chmod(2)), timestamps
-- (e.g., utimensat(2)), extended attributes (setxattr(2)), link count (since
-- Linux 2.6.25; e.g., for the target of link(2) and for unlink(2)), and
-- user/group ID (e.g., chown(2)).
--
-- /Can occur for watched path or a file inside it/
--
-- /Pre-release/
--
isAttrsModified :: Event -> Bool
isAttrsModified = getFlag iN_ATTRIB

-------------------------------------------------------------------------------
-- Access
-------------------------------------------------------------------------------

-- | File was accessed (e.g. read, execve).
--
-- /Occurs only for a file inside the watched directory/
--
-- /Pre-release/
--
isAccessed :: Event -> Bool
isAccessed = getFlag iN_ACCESS

-- | File or directory was opened.
--
-- /Occurs only for a file inside the watched directory/
--
-- /Pre-release/
--
isOpened :: Event -> Bool
isOpened = getFlag iN_OPEN

-- | File opened for writing was closed.
--
-- /Occurs only for a file inside the watched directory/
--
-- /Pre-release/
--
isWriteClosed :: Event -> Bool
isWriteClosed = getFlag iN_CLOSE_WRITE

-- XXX what if it was opened for append? Does NOWRITE mean all cases where the
-- mode was not write? A dir open comes in this category?
--
-- | File or directory opened for read but not write was closed.
--
-- /Can occur for watched path or a file inside it/
--
-- /Pre-release/
--
isNonWriteClosed :: Event -> Bool
isNonWriteClosed = getFlag iN_CLOSE_NOWRITE

-------------------------------------------------------------------------------
-- CRUD Events
-------------------------------------------------------------------------------

-- On macOS this is not generated on hard linking but on Linux it is.
--
-- | File/directory created in watched directory (e.g., open(2) O_CREAT,
-- mkdir(2), link(2), symlink(2), bind(2) on a UNIX domain socket).
--
-- /Occurs only for an object inside the watched directory/
--
-- /Pre-release/
--
isCreated :: Event -> Bool
isCreated = getFlag iN_CREATE

-- | File/directory deleted from watched directory.
--
-- /Occurs only for an object inside the watched directory/
--
-- /Pre-release/
--
isDeleted :: Event -> Bool
isDeleted = getFlag iN_DELETE

-- XXX what if an object is moved in from outside or moved out of the monitored
-- dir?
--
-- | Generated for the original path when an object is moved from under a
-- monitored directory.
--
-- /Occurs only for an object inside the watched directory/
--
-- /Pre-release/
--
isMovedFrom :: Event -> Bool
isMovedFrom = getFlag iN_MOVED_FROM

-- | Generated for the new path when an object is moved under a monitored
-- directory.
--
-- /Occurs only for an object inside the watched directory/
--
-- /Pre-release/
--
isMovedTo :: Event -> Bool
isMovedTo = getFlag iN_MOVED_TO

-- | Generated for a path that is moved from or moved to the monitored
-- directory.
--
-- >>> isMoved ev = isMovedFrom ev || isMovedTo ev
--
-- /Occurs only for an object inside the watched directory/
--
-- /Pre-release/
--
isMoved :: Event -> Bool
isMoved ev = isMovedFrom ev || isMovedTo ev

-- | Determine whether the event indicates modification of an object within the
-- monitored path. This event is generated only for files and not directories.
--
-- /Occurs only for an object inside the watched directory/
--
-- /Pre-release/
--
isModified :: Event -> Bool
isModified = getFlag iN_MODIFY

-------------------------------------------------------------------------------
-- Information about path type (applicable only when 'setFileEvents' is 'True')
-------------------------------------------------------------------------------

foreign import capi
    "sys/inotify.h value IN_ISDIR" iN_ISDIR :: Word32

-- | Determine whether the event is for a directory path.
--
-- /Pre-release/
--
isDir :: Event -> Bool
isDir = getFlag iN_ISDIR

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
    ++ "\n"

        where showev f str = if f ev then "\n" ++ str else ""
#else
#warning "Disabling module Streamly.Internal.FS.Event.Linux. Does not support kernels older than 2.6.36."
module Streamly.Internal.FS.Event.Linux () where
#endif
