-- |
-- Module      : Streamly.Internal.FS.Event.Darwin
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC
--
-- =Overview
--
-- Use 'watchRecursive' with a list of file system paths you want to watch as
-- argument. It returns a stream of 'Event' representing the file system events
-- occurring under the watched paths.
--
-- @
-- Stream.mapM_ (putStrLn . 'showEvent') $ 'watchRecursive' [Path.fromString "path"]
-- @
--
-- 'Event' is an opaque type. Accessor functions (e.g. 'showEvent' above)
-- provided in this module are used to determine the attributes of the event.
--
-- =Design notes
--
-- For reference documentation see:
--
-- * "<https://developer.apple.com/library/archive/documentation/Darwin/Conceptual/FSEvents_ProgGuide/UsingtheFSEventsFramework/UsingtheFSEventsFramework.html Apple FS Events Programming Guide>"
-- * "<https://developer.apple.com/documentation/coreservices/file_system_events?language=objc Apple FS Events Reference>"
--
-- We try to keep the macOS\/Linux/Windows event handling APIs and defaults
-- semantically and syntactically as close as possible.
--
-- =BUGS
--
-- XXX Check the behavior for Windows/Linux where applicable. Mention these in
-- the common module so that users are aware of the portability issues.
--
-- There may be some idiosyncrasies in event reporting.
--
-- 1. Multiple events may be coalesced into a single event having multiple
-- flags set.  For example, on macOS 10.15.6, "touch x; rm x" or "touch x; mv x
-- y" produces an event with both "Created" and "Deleted/Moved" flags set.
--
-- 2. The same event can occur multiple times. For example, "touch x; sleep 1;
-- rm x" generates a "Created" event followed by an event with both "Created"
-- and "Deleted" flags set. Similarly, a cloned event can also occur multiple
-- times.
--
-- 3. Some events can go missing and only the latest one is reported. See
-- 'isCreated' for one such case.
--
-- XXX write a test case for this, and check on all platforms.
--
-- 4. When @rm -rf@ is used on the watched root directory, only one 'isDeleted'
-- event occurs and that is for the root. However, @rm -rf@ on a directory
-- inside the watch root produces 'isDeleted' events for all files.
--
-- XXX write a test case for this, and check on all platforms.
--
-- 5. When a file is moved, a move event occurs for both the source and the
-- destination files. There seems to be no way to distinguish the source and
-- the destination. Perhaps the lower eventId can be considered as source.
--
-- XXX cloned event is specific to macOS.  Translate it to the same events as
-- on copying.
--
-- 6. When a file is cloned both source and destination generate a cloned
-- event, however the source has lower eventId and destination may have
-- OwnerGroupModeChanged, InodeAttrsChanged, Created flags as well.
--
-- You may have to stat the path of the event to know more information about
-- what might have happened.  It may be a good idea to watch the behavior of
-- events you are handling before you can rely on it. See the individual event
-- APIs for some of the known issues marked with `BUGS`. Also see the "Handling
-- Events" section in the "Apple FS Events Programming Guide".
--
-- * "<https://stackoverflow.com/questions/18415285/osx-fseventstreameventflags-not-working-correctly Stack overflow question on event coalescing>"
--
-- =TODO
--
-- APIs for persistent event-id related functionality, to get events from
-- specific event-id etc are not implemented.

#include <config.h>

-- macOS 10.7+
#if HAVE_DECL_KFSEVENTSTREAMCREATEFLAGFILEEVENTS

module Streamly.Internal.FS.Event.Darwin
    (
    -- * Creating a Watch

    -- ** Default configuration
      Config (..)
    , defaultConfig

    -- ** Watch Behavior
    , setRecursiveMode
    , BatchInfo (..)
    , setEventBatching

    -- ** Events of Interest
    , setRootPathEvents
    , setFileEvents
    , setIgnoreSelf
    , setFullHistory
    , setAllEvents

    -- ** Watch APIs
    , watch
    , watchRecursive
    , watchWith

    -- * Handling Events
    , Event
    , getEventId
    , getRoot
    , getRelPath
    , getAbsPath

    -- ** Root Level Events
    -- | Events that belong to the root path as a whole and not to specific
    -- items contained in it.
    , isMounted
    , isUnmounted
    , isHistoryDone
    , isRootPathEvent

    -- ** Item Level Metadata change
    , isAttrsModified
    , isSecurityModified
    , isXAttrsModified
    , isFinderInfoModified

    -- ** Item Level CRUD events
    , isCreated
    , isDeleted
    , isMoved
    , isModified
    , isCloned

    -- ** Item Path info
    , isDir
    , isFile
    , isSymLink
    , isHardLink
    , isLastHardLink

    -- ** Exception Conditions
    , isEventIdWrapped
    , isEventsLost
    , isKernelDropped
    , isUserDropped

    -- * Debugging
    , showEvent
    )
where

import Control.Concurrent (MVar, newMVar, takeMVar, putMVar)
import Control.Monad (when)
import Control.Monad.Cont (runCont, cont)
import Data.Bits ((.|.), (.&.), complement)
import Data.List (dropWhileEnd, sortOn, stripPrefix)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Word (Word8, Word32, Word64)
import Foreign.C.Types (CInt(..), CDouble(..), CSize(..), CUChar(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable(..))
import GHC.IO.Handle.FD (fdToHandle)
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.Parser (Parser)
import Streamly.Internal.Data.Array (Array(..))
import Streamly.Internal.FileSystem.Path (Path)
import System.Directory (canonicalizePath)
import System.IO (Handle, hClose)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Stream as S
import qualified Streamly.Internal.FileSystem.Handle as FH
import qualified Streamly.Internal.Data.Array as A
import qualified Streamly.Internal.FileSystem.Path as Path

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

-- NOTE: "contListMap" is part of the unexposed module:
-- Streamly.Internal.Data.Cont

-- | Given a continuation based transformation from @a@ to @b@ and a
-- continuation based transformation from @[b]@ to @c@, make continuation based
-- transformation from @[a]@ to @c@.
--
-- /Pre-release/

-- You can read the definition as:
--
-- > contListMap f g = \xs final ->
--
contListMap ::
       (a -> (b -> r) -> r)      -- transform a -> b
    -> ([b] -> (c -> r) -> r)    -- transform [b] -> c
    -> ([a] -> (c -> r) -> r)    -- transform [a] -> c
contListMap f g xs final =
    let bconts = fmap (cont . f) xs  -- [Cont b]
        blistCont = sequence bconts  -- Cont [b]
        k ys = g ys final            -- [b] -> r
     in runCont blistCont k          -- r

-------------------------------------------------------------------------------
-- Subscription to events
-------------------------------------------------------------------------------

-- | Watch configuration, used to specify the events of interest and the
-- behavior of the watch.
data Config = Config
    { watchRec :: Bool
    , latency :: Double
    , createFlags   :: Word32
    }

-------------------------------------------------------------------------------
-- Recursive mode
-------------------------------------------------------------------------------

-- | Watch the whole directory tree recursively instead of watching just one
-- level of directory.
--
-- macOS FSEvents can only watch a directory tree recursively. When the
-- recursive mode is off, a recursive watch is started anyway and the events
-- for paths more than one level below a watch root are dropped. The following
-- events are reported:
--
-- * events for a watch root itself
-- * events for the immediate children of a watch root
-- * 'isRootPathEvent', 'isEventsLost', 'isHistoryDone' and 'isEventIdWrapped'
-- events, irrespective of the path
--
-- The events are filtered using 'getRelPath'. Events under a subdirectory are
-- still generated by the OS and delivered to this process, the filtering only
-- hides them.
--
-- /default: False/
setRecursiveMode :: Bool -> Config -> Config
setRecursiveMode recursive cfg@Config{} = cfg {watchRec = recursive}

-------------------------------------------------------------------------------
-- Batching Events
-------------------------------------------------------------------------------

foreign import ccall safe
    "FS/Event/Darwin.h FSEventStreamCreateFlagNoDefer"
    kFSEventStreamCreateFlagNoDefer :: Word32

-- | Determines how multiple events are batched or throttled.
data BatchInfo =
      Throttle Double -- ^ Deliver an event immediately but suppress the
                      -- following events upto the specified time.
    | Batch Double    -- ^ Collapse all events that occurred in the specified
                      -- time window (in seconds) and deliver them as a single
                      -- batch.

-- | Set how the events should be batched. See 'BatchInfo' for details.  A
-- negative value for time is treated as 0.
--
-- /default: Batch 0.0/
--
-- /macOS 10.5+/
setEventBatching :: BatchInfo -> Config -> Config
setEventBatching batchInfo cfg =
    let (t, status) =
            case batchInfo of
                Throttle sec | sec < 0 -> (0, True)
                Throttle sec -> (sec, True)
                Batch sec | sec < 0 -> (0, False)
                Batch sec -> (sec, False)
    in setFlag kFSEventStreamCreateFlagNoDefer status $ cfg {latency = t}

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

foreign import ccall safe
    "FSEventStreamCreateFlagWatchRoot"
    kFSEventStreamCreateFlagWatchRoot :: Word32

-- XXX write a test case for this, and check on all platforms. The behavior on
-- macOS is slightly different as a create event is also generated.
--
-- | Any changes to the path (delete, rename, or create) of an object are
-- reported as events for its parent directory. Such events are not reported
-- for the watch root itself because its parent is not being watched. When
-- 'setRootPathEvents' is 'True', an 'isRootPathEvent' event is generated with an
-- eventId 0 if the watch root is deleted, renamed or created.
--
-- Note: We have observed that an 'isDeleted' event occurs for the root path
-- (when it is a regular dir and not symlink) even when this option is off.
--
-- /default: False/
--
-- /macOS 10.5+/
setRootPathEvents :: Bool -> Config -> Config
setRootPathEvents = setFlag kFSEventStreamCreateFlagWatchRoot

foreign import ccall safe
    "FSEventStreamCreateFlagFileEvents"
    kFSEventStreamCreateFlagFileEvents :: Word32

-- | When this is 'False' only events for the watched directories are reported.
-- For example, when a file is created inside a directory it is reported as an
-- event, the path name in the event is the path of the directory being watched
-- and not of the specific item for which the event occurred.  Events cannot be
-- distinguished based on the type of event.  For example, we cannot determine
-- if an event is a "create" or "delete", we only know that some event ocurred
-- under the watched directory hierarchy.
--
-- When it is 'True' the path reported in the event is the path of the item and
-- not the of the directory in which it is contained. We can use the event
-- accessor functions to determine the event type and the path type etc.
--
-- /default: True/
--
-- /macOS 10.7+/
setFileEvents :: Bool -> Config -> Config
setFileEvents = setFlag kFSEventStreamCreateFlagFileEvents

foreign import ccall safe
    "FSEventStreamCreateFlagIgnoreSelf"
    kFSEventStreamCreateFlagIgnoreSelf :: Word32

-- | When this is 'True' events generated by the current process are not
-- reported.
--
-- /default: False/
--
-- /macOS 10.6+/
setIgnoreSelf :: Bool -> Config -> Config
setIgnoreSelf = setFlag kFSEventStreamCreateFlagIgnoreSelf

foreign import ccall safe
    "FSEventStreamCreateFlagFullHistory"
    kFSEventStreamCreateFlagFullHistory :: Word32

-- | When this is 'True' all events since the beginning of time are reported.
--
-- /default: False/
--
-- /macOS 10.15+/, this setting is a no-op when built with an older SDK.
setFullHistory :: Bool -> Config -> Config
setFullHistory = setFlag kFSEventStreamCreateFlagFullHistory

-- | Set all tunable events 'True' or 'False'. Equivalent to setting:
--
-- * setRootPathEvents
-- * setFileEvents
setAllEvents :: Bool -> Config -> Config
setAllEvents s =
      setRootPathEvents s
    . setFileEvents s

-------------------------------------------------------------------------------
-- Default config
-------------------------------------------------------------------------------

-- | The default settings are:
--
-- * 'setRecursiveMode' 'False'
-- * 'setEventBatching' ('Batch' 0.0)
-- * 'setFileEvents' 'True'
-- * 'setRootPathEvents' 'False'
-- * 'setIgnoreSelf' 'False'
-- * 'setFullHistory' 'False'
--
-- /Pre-release/
--
defaultConfig :: Config
defaultConfig = setFileEvents True $ Config
    { watchRec = False
    , latency = 0.0
    , createFlags = 0
    }

-------------------------------------------------------------------------------
-- Open an event stream (C FFI)
-------------------------------------------------------------------------------

-- See FS/Event/Darwin.h

data CWatch

-- Representation of "struct pathName" in "FS/Event/Darwin.h"
data PathName = PathName
    { pathBytes :: Ptr CUChar
    , pathLen :: CSize
    }

instance Storable PathName where
    alignment _ = sizeOf (undefined :: Ptr a)
    sizeOf _ = sizeOf (undefined :: Ptr CUChar) + sizeOf (undefined :: CSize)
    peek ptr = do
        bptr <- peekByteOff ptr 0
        len <- peekByteOff ptr (sizeOf (undefined :: Ptr CUChar))
        return $ PathName bptr len
    poke ptr PathName{..} = do
        pokeByteOff ptr 0 pathBytes
        pokeByteOff ptr (sizeOf (undefined :: Ptr CUChar)) pathLen

foreign import ccall safe "FS/Event/Darwin.h createWatch" createWatch
    :: Ptr PathName
    -> CInt
    -> Word32
    -> Word64
    -> CDouble
    -> Ptr CInt
    -> Ptr (Ptr CWatch)
    -> IO CInt

foreign import ccall safe "FS/Event/Darwin.h destroyWatch" destroyWatch
    :: Ptr CWatch -> IO ()

-------------------------------------------------------------------------------
-- Open an event stream
-------------------------------------------------------------------------------

-- | A handle for a watch.
data Watch = Watch Handle (Ptr CWatch) (MVar Bool)

-- | Given a 'Config' and @paths@ start monitoring the paths for file system
-- events. Returns a 'Watch' handle which can then be used to read the event
-- stream or to close the watch.
--
-- Implementation Note: This API creates an OS level thread where an event loop
-- runs and writes the events to the write side fd of a POSIX pipe. Event
-- stream APIs read the events from the read end of this pipe.  Since an OS
-- level thread is forked, creating a new watch stream is expensive. It is
-- advised to minimize the number of watches.
--
-- /Pre-release/
--
openWatch :: Config -> NonEmpty Path -> IO Watch
openWatch Config{..} paths = do
    -- XXX write a test case when path does not exist
    -- XXX write a test case when there are no permissions
    -- XXX check whether the path exists. If the path does not exist and
    -- is created later it cannot be properly monitored. Also a user may
    -- inadvertently provide a path for which the user may not have permission
    -- and then later wonder why events are not being reported.
    withPathNames (NonEmpty.toList paths) $ \arraysPtr ->
        alloca $ \fdPtr -> do
        alloca $ \watchPPtr -> do
            let nArrays = fromIntegral (NonEmpty.length paths)
                seconds = realToFrac latency
            r <- createWatch
                    arraysPtr nArrays createFlags 0 seconds fdPtr watchPPtr
            when (r /= 0) $
                ioError (userError "openWatch: failed to create watch.")
            fd <- peek fdPtr
            -- fdToHandle locks the fd if it is a regular file. This fd is
            -- the read end of a pipe, therefore, it is not locked.
            h <- fdToHandle fd
            watchPtr <- peek watchPPtr
            closeLock <- newMVar False
            return $ Watch h watchPtr closeLock

    where

    withPathName :: Path -> (PathName -> IO a) -> IO a
    withPathName path act = do
        A.unsafePinnedAsPtr (Path.toArray path) $ \ptr byteLen ->
            let pname = PathName (castPtr ptr) (fromIntegral byteLen)
            in act pname

    withPathNames = contListMap withPathName withArray

-- | Close a 'Watch' handle.
--
-- /Pre-release/
--
closeWatch :: Watch -> IO ()
closeWatch (Watch h watchPtr mvar) = do
    hClose h
    closed <- takeMVar mvar
    when (not closed) $ destroyWatch watchPtr
    putMVar mvar True

-------------------------------------------------------------------------------
-- Raw events read from the watch file handle
-------------------------------------------------------------------------------

-- | An Event generated by the file system. Use the accessor functions to
-- examine the event.
data Event = Event
   { eventId :: Word64
   , eventFlags :: Word32
   , eventPath :: Path -- absolute path reported by the OS
   , eventRoot :: Maybe Path -- watch root as supplied, if any matched
   , eventRelPath :: Maybe Path -- relative to eventRoot
   }

-- XXX We can perhaps use parseD monad instance for fusing with parseMany? Need
-- to measure the perf.
--
-- XXX should we use a magic in the header to avoid any issues due to
-- misalignment of records? Can that ever happen?
--
readOneEvent :: Parser Word8 IO Event
readOneEvent = do
    arr <- PR.takeEQ 24 (A.createOf 24)
    let arr1 = A.unsafeCast arr :: Array Word64
        eid = A.unsafeGetIndex 0 arr1
        eflags = A.unsafeGetIndex 1 arr1
        pathLen = fromIntegral $ A.unsafeGetIndex 2 arr1
    path <- PR.takeEQ pathLen (A.createOf pathLen)
    return $ Event
        { eventId = eid
        , eventFlags = fromIntegral eflags
        , eventPath = Path.unsafeFromArray path
        , eventRoot = Nothing
        , eventRelPath = Nothing
        }

watchToStream :: Watch -> Stream IO Event
watchToStream (Watch handle _ _) =
    S.catRights $ S.parseMany readOneEvent $ S.unfold FH.reader handle

-- | A watch root as supplied by the user, and its canonical path without
-- trailing separators. The OS reports the event paths with symbolic links
-- resolved, we compare them against the canonical path.
type Root = (Path, String)

dropTrailingSeparators :: String -> String
dropTrailingSeparators = dropWhileEnd (== '/')

-- XXX Use OsPath or streamly's canonicalize
toRoot :: Path -> IO Root
toRoot p = do
    canonical <- canonicalizePath (Path.toString p)
    return (p, dropTrailingSeparators canonical)

-- | Set the watch root of the event and the path relative to it. If the event
-- path is under more than one root, the longest root is used. If it is not
-- under any root, the root is not set.
setRoot :: [Root] -> Event -> IO Event
setRoot roots ev = do
    let path = dropTrailingSeparators (Path.toString (eventPath ev))
        matches =
            [ (root, rel)
            | (root, canonical) <- roots
            , Just rel <- [relativeTo canonical path]
            ]
    -- The shortest relative path belongs to the longest root
    case listToMaybe (sortOn (length . snd) matches) of
        Nothing -> return ev
        Just (root, rel) -> do
            relPath <-
                if null rel
                then return Nothing
                else Just <$> Path.fromString rel
            return ev {eventRoot = Just root, eventRelPath = relPath}

    where

    relativeTo canonical path
        | path == canonical = Just ""
        | otherwise = stripPrefix (canonical ++ "/") path

-- | Drop the events for paths more than one level below the watch roots. See
-- 'setRecursiveMode'.
filterNonRecursive :: Stream IO Event -> Stream IO Event
filterNonRecursive = S.filter isWatched

    where

    isChild ev =
        case (eventRoot ev, eventRelPath ev) of
            (Nothing, _) -> False
            (Just _, Nothing) -> True
            (Just _, Just rel) -> '/' `notElem` Path.toString rel

    isWatched ev =
           isRootPathEvent ev
        || isEventsLost ev
        || isHistoryDone ev
        || isEventIdWrapped ev
        || isChild ev

-- XXX Write tests for all the points in macOS specific behavior.

-- | Start monitoring a list of file system paths for file system events with
-- the supplied configuration operation over the 'defaultConfig'. The paths
-- could be files or directories.  When the path is a directory and recursive
-- mode is set, the whole directory tree under it is watched recursively. See
-- 'setRecursiveMode' for how the non-recursive mode is implemented on macOS.
-- Monitoring starts from the current time onwards.
--
-- Important notes for macOS specific behavior:
--
-- From the observed behavior it seems macOS watches the paths, whatever they
-- are pointing to at any given point of time:
--
-- /Watch root deletion:/ If the the watch root is deleted and then recreated,
-- the newly created file or directory is automatically watched.
--
-- /Watch root moved:/ If the watch root is moved to a new path, the object
-- will no longer be watched unless the new path is also being watched and was
-- pointing to an existing file at the time of starting the watch (see notes
-- about non-existing paths below).
--
-- /Symbolic link watch root:/ If the path name to be watched is a symbolic
-- link then the target of the link is watched instead of the symbolic link
-- itself. It is equivalent to as if the target of the symbolic link itself was
-- directly added to the watch API. That is, the symbolic link is resolved at
-- the time of adding the watch. However, the paths in the events are reported
-- via the original symbolic link, see 'getAbsPath'.
--
-- Note that if a watched path is deleted and recreated as a symbolic link
-- pointing to another path then the symbolic link file itself is watched, it
-- won't be resolved. The symbolic link resolution happens only at the time of
-- adding the watch.
--
-- /Non-existing Paths:/ If a watch is started on a non-existing path then the
-- path is not watched even if it is created later.  The macOS API does not
-- fail for a non-existing path.
--
-- If a non-existing path is watched with 'setRootPathEvents' then an
-- 'isRootPathEvent' event is reported if the path is created later and the
-- "path" field in the event is set to the dirname of the path rather than the
-- full absolute path. This is the observed behavior on macOS 10.15.1.
--
-- @
-- watchWith
--      ('setIgnoreSelf' 'True' . 'setRootPathEvents' 'True')
--      [Path.fromString "path"]
-- @
watchWith ::
    (Config -> Config) -> NonEmpty Path -> Stream IO Event
watchWith f paths = S.bracketIO before after toStream

    where

    cfg = f defaultConfig

    before = do
        -- Canonicalize before starting the watch, the OS resolves the
        -- symbolic links in the roots when the watch is started.
        roots <- mapM toRoot (NonEmpty.toList paths)
        w <- openWatch cfg paths
        return (w, roots)

    after (w, _) = closeWatch w

    toStream (w, roots) =
        let events = S.mapM (setRoot roots) (watchToStream w)
         in if watchRec cfg
            then events
            else filterNonRecursive events

-- | Same as 'watchWith' using 'defaultConfig' and recursive mode.
--
-- >>> watchRecursive = watchWith (setRecursiveMode True)
watchRecursive :: NonEmpty Path -> Stream IO Event
watchRecursive = watchWith (setRecursiveMode True)

-- | Same as 'watchWith' using 'defaultConfig' and non-recursive mode.
--
-- >>> watch = watchWith id
watch :: NonEmpty Path -> Stream IO Event
watch = watchWith id

-------------------------------------------------------------------------------
-- Examine the event stream
-------------------------------------------------------------------------------

-- | Get the event id of an 'Event'.  Event-id is a monotonically increasing
-- 64-bit integer identifying an event uniquely.
getEventId :: Event -> Word64
getEventId Event{..} = eventId

foreign import ccall safe
    "FSEventStreamEventFlagEventIdsWrapped"
    kFSEventStreamEventFlagEventIdsWrapped :: Word32

-- | Determine whether the event id has wrapped. This is impossible on any
-- traditional hardware in any reasonable amount of time unless the event-id
-- starts from a very high value, because the event-id is a 64-bit integer.
-- However, apple still recommends to check and handle this flag.
--
-- /macOS 10.5+/
isEventIdWrapped :: Event -> Bool
isEventIdWrapped = getFlag kFSEventStreamEventFlagEventIdsWrapped

-- | Get the watch root to which this event belongs, as it was supplied to the
-- watch API. When the watch roots are nested, the innermost root containing
-- the event path is returned.
--
-- If the event path is not under any watch root, the path reported by the OS
-- is returned. For example, the 'isRootPathEvent' event for a non-existing
-- watch root that is created later reports the parent directory of the root.
getRoot :: Event -> Path
getRoot Event{..} = fromMaybe eventPath eventRoot

-- getRelPath returns 'Nothing' rather than "." for the watch root because
-- the watch root may be a file, see the comment on getRelPath in
-- "Streamly.Internal.FS.Event".

-- | Get the file system object path for which the event is generated,
-- relative to the watch root. Returns 'Nothing' if the event is for the watch
-- root itself, or if the event path is not under any watch root (see
-- 'getRoot').
getRelPath :: Event -> Maybe Path
getRelPath Event{..} = eventRelPath

-- | Get the absolute file system object path for which the event is generated.
--
-- When the watch root is a symlink, the absolute path returned is via the
-- original symlink and not through the resolved path.
getAbsPath :: Event -> Path
getAbsPath ev =
    case getRelPath ev of
        Just relPath -> Path.join (getRoot ev) relPath
        Nothing -> getRoot ev

-------------------------------------------------------------------------------
-- Event types
-------------------------------------------------------------------------------

getFlag :: Word32 -> Event -> Bool
getFlag mask Event{..} = eventFlags .&. mask /= 0

-------------------------------------------------------------------------------
-- Error events
-------------------------------------------------------------------------------

foreign import ccall safe
    "FSEventStreamEventFlagMustScanSubDirs"
    kFSEventStreamEventFlagMustScanSubDirs :: Word32

-- | Apple documentation says that "If an event in a directory occurs at about
-- the same time as one or more events in a subdirectory of that directory, the
-- events may be coalesced into a single event." In that case you will recieve
-- 'isEventsLost' event. In that case the path listed in the event is
-- invalidated and you must rescan it to know the current state.
--
-- This event can also occur if a communication error (overflow) occurs in
-- sending the event and the event gets dropped. In that case 'isKernelDropped'
-- and/or 'isUserDropped' attributes will also be set.
--
-- /macOS 10.5+/
isEventsLost :: Event -> Bool
isEventsLost = getFlag kFSEventStreamEventFlagMustScanSubDirs

foreign import ccall safe
    "FSEventStreamEventFlagKernelDropped"
    kFSEventStreamEventFlagKernelDropped :: Word32

-- | Did an event get dropped due to a kernel processing issue? Set only when
-- 'isEventsLost' is also true.
--
-- /macOS 10.5+/
isKernelDropped :: Event -> Bool
isKernelDropped = getFlag kFSEventStreamEventFlagKernelDropped

foreign import ccall safe
    "FSEventStreamEventFlagUserDropped"
    kFSEventStreamEventFlagUserDropped :: Word32

-- | Did an event get dropped due to a user process issue? Set only when
-- 'isEventsLost' is also true.
--
-- /macOS 10.5+/
isUserDropped :: Event -> Bool
isUserDropped = getFlag kFSEventStreamEventFlagUserDropped

-------------------------------------------------------------------------------
-- Global Sentinel Events
-------------------------------------------------------------------------------

foreign import ccall safe
    "FSEventStreamEventFlagHistoryDone"
    kFSEventStreamEventFlagHistoryDone :: Word32

-- | Determine whether the event is a history done marker event. A history done
-- event is generated when the historical events from before the current time
-- are done. Historical events are generated when the "since" parameter in the
-- watch config is set to before the current time.
--
-- /macOS 10.5+/
isHistoryDone :: Event -> Bool
isHistoryDone = getFlag kFSEventStreamEventFlagHistoryDone

-------------------------------------------------------------------------------
-- Events affecting the watched path only
-------------------------------------------------------------------------------

foreign import ccall safe
    "FSEventStreamEventFlagRootChanged"
    kFSEventStreamEventFlagRootChanged :: Word32

-- | Determine whether the event indicates a change of path of the monitored
-- object itself. Note that the object may become unreachable or deleted after
-- a change of path.
--
-- /Applicable only when 'setRootPathEvents' is 'True'/
--
-- /Occurs only for the watched path/
--
-- /macOS 10.5+/
isRootPathEvent :: Event -> Bool
isRootPathEvent = getFlag kFSEventStreamEventFlagRootChanged

-------------------------------------------------------------------------------
-- Global events under the watched path
-------------------------------------------------------------------------------

foreign import ccall safe
    "FSEventStreamEventFlagMount"
    kFSEventStreamEventFlagMount :: Word32

-- | Determine whether the event is a mount event. A mount event is generated
-- if a volume is mounted under the path being watched.
--
-- /macOS 10.5+/
isMounted :: Event -> Bool
isMounted = getFlag kFSEventStreamEventFlagMount

foreign import ccall safe
    "FSEventStreamEventFlagUnmount"
    kFSEventStreamEventFlagUnmount :: Word32

-- | Determine whether the event is an unmount event. An unmount event is
-- generated if a volume mounted under the path being watched is unmounted.
--
-- /macOS 10.5+/
isUnmounted :: Event -> Bool
isUnmounted = getFlag kFSEventStreamEventFlagUnmount

-------------------------------------------------------------------------------
-- Metadata change Events (applicable only when 'setFileEvents' is 'True')
-------------------------------------------------------------------------------

foreign import ccall safe
    "FSEventStreamEventFlagItemChangeOwner"
    kFSEventStreamEventFlagItemChangeOwner :: Word32

-- | Determine whether the event is ownership, group, permissions or ACL change
-- event of an object contained within the monitored path.
--
-- Note that this event may be generated even if the metadata is changed to the
-- same value again.
--
-- /Applicable only when 'setFileEvents' is 'True'/
--
-- /Can occur for watched path or a file inside it/
--
-- /macOS 10.7+/
isSecurityModified :: Event -> Bool
isSecurityModified = getFlag kFSEventStreamEventFlagItemChangeOwner

foreign import ccall safe
    "FSEventStreamEventFlagItemInodeMetaMod"
    kFSEventStreamEventFlagItemInodeMetaMod :: Word32

-- | Determine whether the event indicates inode metadata change for an object
-- contained within the monitored path. This event is generated when inode
-- attributes other than the owner, group or permissions are changed e.g. file
-- modification time. It does not occur when the link count of a file changes
-- (as of macOS 10.15.1).
--
-- /Applicable only when 'setFileEvents' is 'True'/
--
-- /Can occur for watched path or a file inside it/
--
-- /macOS 10.7+/
isAttrsModified :: Event -> Bool
isAttrsModified = getFlag kFSEventStreamEventFlagItemInodeMetaMod

foreign import ccall safe
    "FSEventStreamEventFlagItemFinderInfoMod"
    kFSEventStreamEventFlagItemFinderInfoMod :: Word32

-- | Determine whether the event indicates finder information metadata change
-- for an object contained within the monitored path.
--
-- /Applicable only when 'setFileEvents' is 'True'/
--
-- /macOS 10.7+/
isFinderInfoModified :: Event -> Bool
isFinderInfoModified = getFlag kFSEventStreamEventFlagItemFinderInfoMod

foreign import ccall safe
    "FSEventStreamEventFlagItemXattrMod"
    kFSEventStreamEventFlagItemXattrMod :: Word32

-- | Determine whether the event indicates extended attributes metadata change
-- for an object contained within the monitored path.
--
-- /Applicable only when 'setFileEvents' is 'True'/
--
-- /Can occur for watched path or a file inside it/
--
-- /macOS 10.7+/
isXAttrsModified :: Event -> Bool
isXAttrsModified = getFlag kFSEventStreamEventFlagItemXattrMod

-------------------------------------------------------------------------------
-- CRUD Events (applicable only when 'setFileEvents' is 'True')
-------------------------------------------------------------------------------

foreign import ccall safe
    "FSEventStreamEventFlagItemCreated"
    kFSEventStreamEventFlagItemCreated :: Word32

-- | Determine whether the event indicates creation of an object within the
-- monitored path. This event is generated when any file system object other
-- than a hard link is created.  On hard linking only an 'isAttrsModified'
-- event on the directory is generated, it is not a create event. However, when
-- a hard link is deleted 'isDeleted' and 'isHardLink' both are true.
--
-- This event can occur for the watched path if the path was deleted/moved and
-- created again (tested on macOS 10.15.1). However, if a watch is started on a
-- non-existing path and the path is created later, then this event is not
-- generated, the path is not watched.
--
-- BUGS: On 10.15.1 when we use a "touch x" to create a file for the first time
-- only an 'isAttrsModified' event occurs and there is no 'isCreated'
-- event. However, this seems to have been fixed on 10.15.6.
--
-- /Applicable only when 'setFileEvents' is 'True'/
--
-- /Can occur for watched path or a file inside it/
--
-- /macOS 10.7+/
isCreated :: Event -> Bool
isCreated = getFlag kFSEventStreamEventFlagItemCreated

foreign import ccall safe
    "FSEventStreamEventFlagItemRemoved"
    kFSEventStreamEventFlagItemRemoved :: Word32

-- | Determine whether the event indicates deletion of an object within the
-- monitored path. This is true when a file or a hardlink is deleted.
--
-- Applicable only when 'setFileEvents' is 'True'.
--
-- /Can occur for watched path or a file inside it/
--
-- /macOS 10.7+/
isDeleted :: Event -> Bool
isDeleted = getFlag kFSEventStreamEventFlagItemRemoved

foreign import ccall safe
    "FSEventStreamEventFlagItemRenamed"
    kFSEventStreamEventFlagItemRenamed :: Word32

-- | Determine whether the event indicates rename of an object within the
-- monitored path. This event is generated when a file is renamed within the
-- watched directory or if it is moved out of or in the watched directory.
--
-- /Applicable only when 'setFileEvents' is 'True'/
--
-- /Can occur for watched path or a file inside it/
--
-- /macOS 10.7+/
isMoved :: Event -> Bool
isMoved = getFlag kFSEventStreamEventFlagItemRenamed

foreign import ccall safe
    "FSEventStreamEventFlagItemModified"
    kFSEventStreamEventFlagItemModified :: Word32

-- | Determine whether the event indicates modification of an object within the
-- monitored path. This event is generated only for files and not directories.
--
-- Applicable only when 'setFileEvents' is 'True'.
--
-- /macOS 10.7+/
isModified :: Event -> Bool
isModified = getFlag kFSEventStreamEventFlagItemModified

foreign import ccall safe
    "FSEventStreamEventFlagItemCloned"
    kFSEventStreamEventFlagItemCloned :: Word32

-- | Determine whether the event indicates cloning of an object within the
-- monitored path. The "Duplicate" command in the "File" menu of the "Finder"
-- application generates a "clone" event.
--
-- Applicable only when 'setFileEvents' is 'True'.
--
-- /macOS 10.13+/, always 'False' when built with an older SDK.
isCloned :: Event -> Bool
isCloned = getFlag kFSEventStreamEventFlagItemCloned

-------------------------------------------------------------------------------
-- Information about path type (applicable only when 'setFileEvents' is 'True')
-------------------------------------------------------------------------------

foreign import ccall safe
    "FSEventStreamEventFlagItemIsDir"
    kFSEventStreamEventFlagItemIsDir :: Word32

-- | Determine whether the event is for a directory path.
--
-- Applicable only when 'setFileEvents' is 'True'.
--
-- /macOS 10.7+/
isDir :: Event -> Bool
isDir = getFlag kFSEventStreamEventFlagItemIsDir

foreign import ccall safe
    "FSEventStreamEventFlagItemIsFile"
    kFSEventStreamEventFlagItemIsFile :: Word32

-- | Determine whether the event is for a file path.
--
-- Applicable only when 'setFileEvents' is 'True'.
--
-- /macOS 10.7+/
isFile :: Event -> Bool
isFile = getFlag kFSEventStreamEventFlagItemIsFile

foreign import ccall safe
    "FSEventStreamEventFlagItemIsSymlink"
    kFSEventStreamEventFlagItemIsSymlink :: Word32

-- | Determine whether the event is for a symbolic link path.
--
-- Applicable only when 'setFileEvents' is 'True'.
--
-- /macOS 10.7+/
isSymLink :: Event -> Bool
isSymLink = getFlag kFSEventStreamEventFlagItemIsSymlink

foreign import ccall safe
    "FSEventStreamEventFlagItemIsHardlink"
    kFSEventStreamEventFlagItemIsHardlink :: Word32

-- | Determine whether the event is for a file with more than one hard link.
-- When 'isFile' is true we can check for 'isHardLink'. Note that 'isHardLink'
-- is not true when a hard link is created, however, it is true when a file
-- which has or had more than one link in the past is removed. This is not true
-- if a file never had more than one link.
--
-- Applicable only when 'setFileEvents' is 'True'.
--
-- /macOS 10.10+/, always 'False' when built with an older SDK.
isHardLink :: Event -> Bool
isHardLink = getFlag kFSEventStreamEventFlagItemIsHardlink

foreign import ccall safe
    "FSEventStreamEventFlagItemIsLastHardlink"
     kFSEventStreamEventFlagItemIsLastHardlink :: Word32

-- | Determine whether the event is for a hard link path with only one hard
-- link.  If 'isHardLink' is true then we can check for 'isLastHardLink'.  This
-- is true when a file has had more than one link and now the last link is
-- removed. In that case both 'isHardLink' and 'isLastHardLink" would be true.
--
-- Applicable only when 'setFileEvents' is 'True'.
--
-- /macOS 10.10+/, always 'False' when built with an older SDK.
isLastHardLink :: Event -> Bool
isLastHardLink = getFlag kFSEventStreamEventFlagItemIsLastHardlink

-------------------------------------------------------------------------------
-- Debugging
-------------------------------------------------------------------------------

-- | Convert an 'Event' record to a String representation.
showEvent :: Event -> String
showEvent ev@Event{..} =
    "--------------------------"
        ++ "\nId = " ++ show eventId
        ++ "\nRoot = " ++ show (Path.toString (getRoot ev))
        ++ "\nPath = " ++ maybe "" (show . Path.toString) (getRelPath ev)
        ++ "\nOS Path = " ++ show (Path.toString eventPath)
        ++ "\nFlags " ++ show eventFlags

        ++ showev isEventsLost "MustScanSubdirs"
        ++ showev isKernelDropped "KernelDropped"
        ++ showev isUserDropped "UserDropped"

        ++ showev isRootPathEvent "RootPathEvent"
        ++ showev isMounted "Mounted"
        ++ showev isUnmounted "Unmounted"
        ++ showev isHistoryDone "HistoryDone"

        ++ showev isSecurityModified "SecurityModified"
        ++ showev isAttrsModified "AttrsModified"
        ++ showev isFinderInfoModified "FinderInfoChanged"
        ++ showev isXAttrsModified "XAttrsModified"

        ++ showev isCreated "Created"
        ++ showev isDeleted "Deleted"
        ++ showev isModified "Modified"
        ++ showev isMoved "Moved"
        ++ showev isCloned "Cloned"

        ++ showev isDir "Dir"
        ++ showev isFile "File"
        ++ showev isSymLink "SymLink"
        ++ showev isHardLink "HardLink"
        ++ showev isLastHardLink "LastHardLink"
        ++ "\n"

        where showev f str = if f ev then "\n" ++ str else ""
#else
module Streamly.Internal.FS.Event.Darwin () where
#warning "Autoconf did not find the definition \
kFSEventStreamCreateFlagFileEvents in Darwin header files.\
Do you have Cocoa framework header files installed?\
Not compiling the Streamly.Internal.FS.Event.Darwin module. \
Programs depending on this module may not compile. \
Check if HAVE_DECL_KFSEVENTSTREAMCREATEFLAGFILEEVENTS is defined in config.h \
generated from src/config.h.in"
#endif
