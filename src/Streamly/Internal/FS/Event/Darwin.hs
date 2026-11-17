-- |
-- Module      : Streamly.Internal.FS.Event.Darwin
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC
--
-- The differences in event reporting from Linux and Windows, such as the
-- coalescing of events, are listed in docs/platform-differences.md.

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

    -- Low level watch APIs
    , Watch
    , openWatch
    , closeWatch
    , readEvent
    , eventStream
    , addRoot
    , removeRoot

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

import Control.Concurrent (rtsSupportsBoundThreads, threadWaitRead)
import Control.Concurrent.MVar
    (MVar, modifyMVar, modifyMVar_, newMVar, putMVar, takeMVar)
import Control.Exception (SomeException, finally, onException, throwIO, try)
import Control.Monad (void, when)
import Control.Monad.Cont (runCont, cont)
import Data.Bits ((.|.), (.&.), complement)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (dropWhileEnd, nubBy, stripPrefix)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.Word (Word8, Word32, Word64)
import Foreign.C.Error (eAGAIN, eINTR, eWOULDBLOCK, getErrno, throwErrno)
import Foreign.C.Types (CInt(..), CDouble(..), CSize(..), CUChar(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable(..))
import GHC.Conc (atomically, closeFdWith, threadWaitReadSTM)
import Streamly.Data.Array (Array)
import Streamly.Data.Stream (Stream)
import Streamly.Internal.Data.Array (unsafeFreezeWithShrink)
import Streamly.Internal.FS.Event.Darwin.Replace (Gen, Since(..))
import Streamly.Internal.FileSystem.Path (Path)
import Streamly.Internal.FS.Event.RootPath (isDotPath, normalizeRoot)
import System.Directory (canonicalizePath, getCurrentDirectory)
import System.IO.Error (catchIOError)
import System.Posix.Types (CSsize(..), Fd(..))

import qualified Control.Exception as Exception
    (interruptible, mask, mask_, uninterruptibleMask_)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Streamly.Data.Array as A (createOf, read)
import qualified Streamly.Data.Stream as S
import qualified Streamly.Internal.Data.Array as A
    (byteLength, unsafePinnedAsPtr, unsafeSliceOffLen)
import qualified Streamly.Internal.Data.MutArray as MArray
    (unsafeCreateWithPtr')
import qualified Streamly.Internal.FileSystem.Path as Path
import qualified Streamly.Internal.FS.Event.Darwin.Replace as Replace

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
-- When the recursive mode is off, the following events are reported:
--
-- * events for a watch root itself
-- * events for the immediate children of a watch root
-- * 'isRootPathEvent', 'isEventsLost', 'isHistoryDone' and 'isEventIdWrapped'
-- events, irrespective of the path
--
-- When 'setFileEvents' is 'False', the path of an event is the directory in
-- which an item changed. The event for a watch root reports the changes to
-- its immediate children, the events for its subdirectories are dropped.
--
-- Turning the recursive mode off does not reduce the cost of the watch.
-- macOS can only watch a directory tree recursively, the events for the paths
-- more than one level below a watch root are delivered to this process and
-- dropped by the library.
--
-- The entries already in a directory moved into the tree are not reported.
-- Scan the directory on its move event to find them.
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

-- | When the historical events are replayed starting from an event id,
-- FSEvents may skip some of the events stored in the same chunk as that id.
-- When this is 'True', all the events of that chunk are reported, including
-- those with an id lower than the starting id.
--
-- The watch APIs in this module always start a watch from the current time,
-- historical events are not replayed. Therefore, this setting has no effect.
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
-- C FFI
-------------------------------------------------------------------------------

-- See FS/Event/Darwin.h

data CWatch

data CStream

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

foreign import ccall safe "FS/Event/Darwin.h createWatch" c_createWatch
    :: CDouble
    -> Ptr CInt
    -> Ptr (Ptr CWatch)
    -> IO CInt

foreign import ccall safe "FS/Event/Darwin.h startStream" c_startStream
    :: Ptr CWatch
    -> Ptr PathName
    -> CInt
    -> Word64
    -> Word64
    -> Word32
    -> IO (Ptr CStream)

foreign import ccall safe "FS/Event/Darwin.h stopStream" c_stopStream
    :: Ptr CStream -> IO ()

foreign import ccall unsafe "FS/Event/Darwin.h currentEventId"
    c_currentEventId :: IO Word64

foreign import ccall unsafe "FS/Event/Darwin.h eventIdSinceNow"
    c_eventIdSinceNow :: IO Word64

foreign import ccall safe "FS/Event/Darwin.h destroyWatch" c_destroyWatch
    :: Ptr CWatch -> IO ()

foreign import ccall unsafe "unistd.h read"
    c_read :: CInt -> Ptr Word8 -> CSize -> IO CSsize

foreign import ccall unsafe "unistd.h close"
    c_close :: CInt -> IO CInt

-------------------------------------------------------------------------------
-- Events
-------------------------------------------------------------------------------

-- | An Event generated by the file system. Use the accessor functions to
-- examine the event.
data Event = Event
   { eventId :: Word64
   , eventFlags :: Word32
   , eventPath :: Path -- absolute path reported by the OS
   , eventRoot :: Maybe Path -- watch root as supplied, if any matched
   , eventAbsRoot :: Maybe Path -- absolute path of eventRoot
   , eventRelPath :: Maybe Path -- relative to eventRoot
   }

-------------------------------------------------------------------------------
-- Watch roots
-------------------------------------------------------------------------------

-- | A watch root.
data Root = Root
    { rootPath :: Path -- ^ As supplied by the user, see 'normalizeRoot'
      -- | The absolute path of the root: a relative 'rootPath' joined to
      -- the working directory at the time the root was added. Symbolic
      -- links are not resolved. The FSEvents streams are created with this
      -- path, a stream created after a change of the working directory
      -- watches the same directory.
    , rootAbs :: Path
      -- | The canonical path without trailing separators. The OS reports
      -- the event paths with symbolic links resolved, we compare them
      -- against the canonical path.
    , rootCanonical :: String
    }

dropTrailingSeparators :: String -> String
dropTrailingSeparators = dropWhileEnd (== '/')

-- | The 'Root' of a path supplied by the user, the path is normalized by
-- 'normalizeRoot'. 'Path.isRooted' is true for @.@, the absolute path of
-- @.@ is the working directory. A path starting with @.\/@ is not rooted
-- after 'normalizeRoot'.
--
-- XXX Use OsPath or streamly's canonicalize
toRoot :: Path -> IO Root
toRoot p0 = do
    absPath <-
        if Path.isRooted p && not (isDotPath p)
        then return p
        else do
            cwd <- getCurrentDirectory >>= Path.fromString
            return (if isDotPath p then cwd else Path.join cwd p)
    canonical <- canonicalizePath (Path.toString absPath)
    return
        Root
            { rootPath = p
            , rootAbs = absPath
            , rootCanonical = dropTrailingSeparators canonical
            }

    where

    p = normalizeRoot p0

-- | Whether the two roots were supplied as the same path, after
-- normalization.
isSameRoot :: Root -> Root -> Bool
isSameRoot r1 r2 = Path.eqPathBytes (rootPath r1) (rootPath r2)

-- | The event with its watch root and the path relative to the root set. If
-- the event path is under more than one root, for example, a root and a
-- root inside it, one event is returned for each of them, in the order of
-- the roots, as on Linux and Windows. If it is not under any root, the event
-- is returned without a root.
setRoot :: [Root] -> Event -> IO [Event]
setRoot roots ev = do
    let path = dropTrailingSeparators (Path.toString (eventPath ev))
        matches =
            [ (root, rel)
            | root <- roots
            , Just rel <- [relativeTo (rootCanonical root) path]
            ]
        withRoot (root, rel) = do
            relPath <-
                if null rel
                then return Nothing
                else Just <$> Path.fromString rel
            return
                ev
                    { eventRoot = Just (rootPath root)
                    , eventAbsRoot = Just (rootAbs root)
                    , eventRelPath = relPath
                    }
    if null matches then return [ev] else mapM withRoot matches

    where

    relativeTo canonical path
        | path == canonical = Just ""
        | otherwise = stripPrefix (canonical ++ "/") path

-- | Whether the event is reported in the non-recursive mode, see
-- 'setRecursiveMode'. The first argument is 'True' when the file events are
-- enabled, see 'setFileEvents'.
isNonRecursiveEvent :: Bool -> Event -> Bool
isNonRecursiveEvent fileEvents ev =
       isRootPathEvent ev
    || isEventsLost ev
    || isHistoryDone ev
    || isEventIdWrapped ev
    || isChild

    where

    -- Without the file events the path is the directory in which an item
    -- changed, a child directory reports the changes to its own children.
    isChild =
        case (eventRoot ev, eventRelPath ev) of
            (Nothing, _) -> False
            (Just _, Nothing) -> True
            (Just _, Just rel) ->
                fileEvents && '/' `notElem` Path.toString rel

-------------------------------------------------------------------------------
-- Records read from the pipe
-------------------------------------------------------------------------------

-- | A record written to the pipe by "Darwin.c", with the generation of the
-- stream that wrote it.
data Record =
      RecordEvent !Word64 Event
      -- | The stream of the generation is released, it writes no more
      -- records.
    | RecordRetired !Word64

recordHeaderLen :: Int
recordHeaderLen = 5 * sizeOf (undefined :: Word64)

-- | The header of a record, five 'Word64' fields in this order.
data Header = Header
    { hdrKind :: !Word64 -- ^ 0 for an event, non-zero for a retired stream
    , hdrGen :: !Word64
    , hdrEventId :: !Word64
    , hdrEventFlags :: !Word64
    , hdrPathLen :: !Word64 -- ^ The length of the path after the header
    }

-- | Decode the header at the start of the bytes. The bytes must hold at
-- least 'recordHeaderLen' bytes.
peekHeader :: Array Word8 -> IO Header
peekHeader arr =
    A.unsafePinnedAsPtr arr $ \p _ ->
        let field :: Int -> IO Word64
            field i = peekByteOff p (i * sizeOf (undefined :: Word64))
         in Header
                <$> field 0 <*> field 1 <*> field 2 <*> field 3 <*> field 4

-- | Parse the record at the start of the bytes. Returns the record and the
-- bytes after it, or 'Nothing' if the bytes do not hold a complete record.
parseRecord :: Array Word8 -> IO (Maybe (Record, Array Word8))
parseRecord arr
    | len < recordHeaderLen = return Nothing
    | otherwise = do
        Header{..} <- peekHeader arr
        let plen = fromIntegral hdrPathLen
            total = recordHeaderLen + plen
        if len < total
        then return Nothing
        else do
            -- An empty slice would keep the whole read buffer alive.
            let rest =
                    if len == total
                    then mempty
                    else A.unsafeSliceOffLen total (len - total) arr
            rec <-
                if hdrKind /= 0
                then return (RecordRetired hdrGen)
                else do
                    -- Copy the path, a slice would keep the whole read
                    -- buffer alive.
                    path <-
                        S.fold (A.createOf plen)
                            $ A.read
                            $ A.unsafeSliceOffLen recordHeaderLen plen arr
                    return
                        $ RecordEvent hdrGen
                        $ Event
                            { eventId = hdrEventId
                            , eventFlags = fromIntegral hdrEventFlags
                            , eventPath = Path.unsafeFromArray path
                            , eventRoot = Nothing
                            , eventAbsRoot = Nothing
                            , eventRelPath = Nothing
                            }
            return (Just (rec, rest))

    where

    len = A.byteLength arr

-------------------------------------------------------------------------------
-- Watch handle
-------------------------------------------------------------------------------

-- Adding a root:
--
-- The paths of an FSEvents stream cannot be changed after it is created.
-- 'addRoot' and 'removeRoot' replace the stream of the watch by a new stream
-- with the new list of roots. Each stream has a generation number, the
-- records written by a stream to the pipe carry its generation, the reader
-- uses the roots of that generation to find the root of an event.
--
-- The new stream starts from an event id already read, the FSEvents
-- history replays the events after that id. The old stream keeps reporting
-- until the reader reads the end of the replay, then it is stopped. An
-- event reported by both streams is reported once. The state machine in
-- "Streamly.Internal.FS.Event.Darwin.Replace" decides which streams to
-- start and stop, and which events to report.
--
-- Until the replay ends, usually within milliseconds, the old stream
-- reports the events of a removed root. The events of a new root that occur
-- before 'addRoot' returns may not be reported.

-- | The state of the read end of the pipe of a watch.
data FdState = FdState
    { fsWatchClosed :: !Bool -- ^ 'closeWatch' was called
    , fsFdOpen :: !Bool -- ^ The fd is not closed yet
    -- | The threads waiting for the fd to become readable, counted in the
    -- non-threaded RTS only.
    , fsFdWaiters :: !Int
    }

-- | The roots of a watch.
data RootState = RootState
    { rsClosed :: !Bool -- ^ 'closeWatch' was called
    , rsRoots :: [Root] -- ^ In the order in which they were added
    , rsGen :: !Word64 -- ^ The generation of the current stream
    }

-- | The FSEvents streams of a watch. Taken by the reader to process the
-- records it read, and by the root changes. Not held across a blocking
-- call.
data StreamState = StreamState
    { ssReplace :: Replace.State
      -- | The streams that are not stopped
    , ssHandles :: [(Gen, Ptr CStream)]
      -- | The roots of each generation whose stream may still write records.
    , ssRoots :: [(Gen, [Root])]
    , ssClosed :: !Bool -- ^ The streams are stopped by 'closeWatch'
    }

-- | The records read from the pipe and not yet returned.
data ReadState = ReadState
    { -- | The bytes read from the pipe and not yet parsed, the last record
      -- may be incomplete.
      rdBuffer :: Array Word8
    , rdEnded :: !Bool -- ^ The watch is closed
      -- | The events of a processed record that are not returned yet. A
      -- record has one event for each root that its path is under. An
      -- event is also kept here by a read that was interrupted by an async
      -- exception before it could return the event.
    , rdUndelivered :: [Event]
    }

-- | A handle for a watch.
data Watch = Watch
    { watchConfig :: Config -- ^ The config used to open the watch
    , watchFd :: CInt -- ^ The read end of the pipe, non-blocking
    , watchPtr :: Ptr CWatch
      -- | Held during a system call on the fd, 'closeWatch' cannot close the
      -- fd while the call is in progress. The calls made with the lock held
      -- must not block.
    , watchFdLock :: MVar FdState
      -- | Held while the stream is replaced or destroyed.
    , watchRootLock :: MVar RootState
    , watchStreamState :: MVar StreamState
    , watchReadState :: MVar ReadState
    }

-- | Create a watch with no root.
createWatch :: Config -> IO Watch
createWatch cfg@Config{..} =
    alloca $ \fdPtr ->
    alloca $ \watchPPtr -> do
        r <- c_createWatch (realToFrac latency) fdPtr watchPPtr
        when (r /= 0)
            $ ioError (userError "openWatch: failed to create watch.")
        fd <- peek fdPtr
        ptr <- peek watchPPtr
        fdLock <-
            newMVar
                FdState
                    {fsWatchClosed = False, fsFdOpen = True, fsFdWaiters = 0}
        rootLock <-
            newMVar RootState {rsClosed = False, rsRoots = [], rsGen = 0}
        streams <-
            newMVar
                StreamState
                    { ssReplace = Replace.initial
                    , ssHandles = []
                    , ssRoots = []
                    , ssClosed = False
                    }
        readState <-
            newMVar
                ReadState
                    { rdBuffer = mempty
                    , rdEnded = False
                    , rdUndelivered = []
                    }
        return
            Watch
                { watchConfig = cfg
                , watchFd = fd
                , watchPtr = ptr
                , watchFdLock = fdLock
                , watchRootLock = rootLock
                , watchStreamState = streams
                , watchReadState = readState
                }

-- | Stop the streams of the generations, the lock of the streams must be
-- held.
stopStreams :: [Gen] -> StreamState -> IO StreamState
stopStreams gens ss = do
    mapM_ c_stopStream [h | (g, h) <- ssHandles ss, g `elem` gens]
    return ss {ssHandles = filter ((`notElem` gens) . fst) (ssHandles ss)}

-- | Replace the stream of the watch by a stream on @roots@ with the
-- generation @gen@. Must be called with the root lock held. Raise an
-- exception if the stream cannot be started, the watch keeps its stream.
setRoots :: String -> Watch -> Gen -> [Root] -> IO ()
setRoots name Watch{..} gen roots =
    modifyMVar_ watchStreamState $ \ss ->
        if null roots
        then do
            let (stops, rs) = Replace.stopAll (ssReplace ss)
            ss1 <- stopStreams stops ss
            return ss1 {ssReplace = rs}
        else do
            now <- c_currentEventId
            let (since, stops, rs) = Replace.replace gen now (ssReplace ss)
                flags = createFlags watchConfig
            (sinceId, flags1) <-
                case since of
                    SinceNow -> (, flags) <$> c_eventIdSinceNow
                    -- The full history flag reports all the events of the
                    -- chunk of the start id, otherwise some events after
                    -- it may be skipped.
                    SinceId eid ->
                        let full = kFSEventStreamCreateFlagFullHistory
                        in return (eid, flags .|. full)
            h <- withPathNames (map rootAbs roots) $ \arraysPtr ->
                    c_startStream
                        watchPtr arraysPtr (fromIntegral (length roots))
                        gen sinceId flags1
            when (h == nullPtr)
                $ ioError (userError (name ++ ": failed to create watch."))
            -- The reader cannot read the records of the new stream before
            -- the lock is released, its roots are known by then.
            ss1 <- stopStreams stops ss
            return
                ss1
                    { ssReplace = rs
                    , ssHandles = (gen, h) : ssHandles ss1
                    , ssRoots = (gen, roots) : ssRoots ss1
                    }

    where

    withPathName :: Path -> (PathName -> IO a) -> IO a
    withPathName path act =
        A.unsafePinnedAsPtr (Path.toArray path) $ \ptr byteLen ->
            act (PathName (castPtr ptr) (fromIntegral byteLen))

    withPathNames = contListMap withPathName withArray

deleteGen :: Gen -> [(Gen, a)] -> [(Gen, a)]
deleteGen gen = filter ((/= gen) . fst)

-- | @addRoot watch root@ adds the path @root@ as a root of the watch
-- @watch@, with the 'Config' used to open the watch. A relative @root@ is
-- relative to the working directory at the time of the call, a later change
-- of the working directory does not affect the watch.
--
-- The path is normalized: redundant separators and @.@ components are
-- removed, @x\/\/y@, @x\/.\/y@ and @.\/x\/y@ become @x\/y@. 'getRoot'
-- reports the normalized path. A path that is already a root of the watch is
-- not added again. Symbolic links are not resolved, case is significant even
-- on a case-insensitive file system: @\/tmp\/Foo@ and @\/tmp\/foo@ are two
-- roots, and so are a symbolic link and its target.
--
-- The events of @root@ that occur before 'addRoot' returns may not be
-- reported. The events of the other roots of the watch are not lost or
-- duplicated by the call, except on a volume that does not keep a history
-- of file system events, e.g. a read-only or a network volume. On such a
-- volume the events of the other roots that occur during the call may be
-- lost, an 'isEventsLost' event may be reported in their place.
--
-- Can be called from any thread, including while another thread reads the
-- events of the watch using 'eventStream'. The call does not wait for an
-- event. Raise an exception if the watch is closed, or if the new stream
-- cannot be started, e.g. when the watch would have more than 4096 roots.
--
-- /Pre-release/
--
addRoot :: Watch -> Path -> IO ()
addRoot wtch@Watch{..} root = do
    -- Canonicalize before starting the stream, the OS resolves the symbolic
    -- links in the roots when the stream is started.
    r <- toRoot root
    modifyMVar_ watchRootLock $ \st -> do
        when (rsClosed st)
            $ ioError (userError "addRoot: the watch is closed.")
        let roots = rsRoots st ++ [r]
            gen = rsGen st + 1
        -- A path that is already a root is watched once
        if any (isSameRoot r) (rsRoots st)
        then return st
        else do
            setRoots "addRoot" wtch gen roots
            return st {rsRoots = roots, rsGen = gen}

-- | Remove a root path from a 'Watch'. The path must be the same as the
-- one used to add the root, with the same case, ignoring redundant
-- separators and @.@ components.
--
-- The events of the root that are queued before the call are reported. The
-- events of the root may still be reported for a short time after the call
-- returns, usually a few milliseconds. There is no event to mark the end of
-- the events of the root, unlike the @isRootUnwatched@ event on Linux. A
-- directory that is also watched via another root remains watched for that
-- root.
--
-- The events of the other roots of the watch are not lost or duplicated by
-- the call, except on a volume that does not keep a history of file system
-- events, e.g. a read-only or a network volume. On such a volume the events
-- of the other roots that occur during the call may be lost, an
-- 'isEventsLost' event may be reported in their place.
--
-- Can be called from any thread, including while another thread reads the
-- events of the watch using 'eventStream'. The call does not wait for an
-- event. Does nothing if the watch is closed or if the path is not a root
-- of the watch.
--
-- /Pre-release/
--
removeRoot :: Watch -> Path -> IO ()
removeRoot wtch@Watch{..} root =
    modifyMVar_ watchRootLock $ \st -> do
        let path = normalizeRoot root
            roots =
                filter
                    (not . Path.eqPathBytes path . rootPath) (rsRoots st)
            gen = rsGen st + 1
        if rsClosed st || length roots == length (rsRoots st)
        then return st
        else do
            setRoots "removeRoot" wtch gen roots
            return st {rsRoots = roots, rsGen = gen}

-- | Close the read end of the pipe. closeFdWith wakes up the threads
-- waiting for the fd to become readable, in the threaded RTS.
closeReadFd :: CInt -> IO ()
closeReadFd fd = closeFdWith (\_ -> void (c_close fd)) (Fd fd)

-- | Close a 'Watch' handle. The event streams of the watch end, a thread
-- waiting for events in 'eventStream' is woken up. Closing a closed watch
-- does nothing.
--
-- Can be called from any thread, including while another thread reads the
-- events of the watch using 'eventStream'. The call does not wait for an
-- event.
--
-- With the non-threaded RTS, a thread waiting for events is not woken up
-- while a child process created by @fork@ without @exec@ is running, unless
-- the child has closed the file descriptors that it inherited.
--
-- /Pre-release/
--
closeWatch :: Watch -> IO ()
closeWatch Watch{..} =
    modifyMVar_ watchRootLock $ \st ->
        if rsClosed st
        then return st
        else do
            modifyMVar_ watchStreamState $ \ss -> do
                let (stops, rs) = Replace.stopAll (ssReplace ss)
                ss1 <- stopStreams stops ss
                return ss1 {ssReplace = rs, ssClosed = True}
            -- Waits for the stopped streams and closes the write end of the
            -- pipe
            c_destroyWatch watchPtr
            modifyMVar_ watchFdLock $ \fs ->
                -- The non-threaded RTS waits for the fds in select, select
                -- fails with EBADF if a waited fd is closed, and the RTS
                -- terminates the program. The last waiting thread closes
                -- the fd.
                if rtsSupportsBoundThreads || fsFdWaiters fs == 0
                then do
                    closeReadFd watchFd
                    return fs {fsWatchClosed = True, fsFdOpen = False}
                else return fs {fsWatchClosed = True}
            return st {rsClosed = True}

-- The FSEvents callbacks of a watch run on a dispatch queue and write the
-- events to a POSIX pipe. 'readEvent' reads the events from the read end of
-- the pipe.

-- | Start monitoring the @paths@ for file system events, with the supplied
-- modifications applied to the 'defaultConfig'. Returns a 'Watch' handle
-- which can then be used to read the event stream or to close the watch.
--
-- The 'Config' is kept in the 'Watch', 'readEvent', 'eventStream' and
-- 'addRoot' use it.
--
-- A process can use at most 512 FSEvents streams. A watch uses one of them,
-- and more than one for a short time during 'addRoot' and 'removeRoot'.
--
-- Raise an exception if the watch cannot be started.
--
-- /Pre-release/
--
openWatch :: (Config -> Config) -> NonEmpty Path -> IO Watch
openWatch f paths = do
    -- XXX write a test case when path does not exist
    -- XXX write a test case when there are no permissions
    -- XXX check whether the path exists. If the path does not exist and
    -- is created later it cannot be properly monitored. Also a user may
    -- inadvertently provide a path for which the user may not have permission
    -- and then later wonder why events are not being reported.
    -- A path listed more than once is watched once
    roots <- nubBy isSameRoot <$> mapM toRoot (NonEmpty.toList paths)
    -- The handle is not returned to the caller if starting the stream
    -- fails, the caller cannot close it. 'mask' ensures that an async
    -- exception cannot arrive between creating the watch and installing the
    -- handler.
    Exception.mask $ \restore -> do
        w <- createWatch (f defaultConfig)
        let start st = do
                setRoots "openWatch" w 1 roots
                return st {rsRoots = roots, rsGen = 1}
        restore (modifyMVar_ (watchRootLock w) start)
            `onException` closeWatch w
        return w

-------------------------------------------------------------------------------
-- Reading events
-------------------------------------------------------------------------------

-- | The result of a read of the pipe.
data ReadResult =
      ReadClosed -- ^ The watch is closed
    | ReadData (Array Word8)
      -- | No data is available. The action waits for it, it must be run.
    | ReadWait (IO ())

-- | Read the bytes available in the pipe of a watch, without waiting. If
-- none are available, returns an action that waits for them.
--
-- The wait is registered, or counted, before the action is returned. The
-- caller must mask async exceptions and must run the action. The action can
-- be interrupted by an async exception.
readPipe :: Watch -> IO ReadResult
readPipe Watch{watchFd = fd, watchFdLock = lock} =
    modifyMVar lock $ \st ->
        if fsWatchClosed st
        then return (st, ReadClosed)
        else do
            resRef <- newIORef 0
            arr <- MArray.unsafeCreateWithPtr' bufSize $ \p -> do
                n <- c_read fd p (fromIntegral bufSize)
                writeIORef resRef n
                return (max 0 (fromIntegral n))
            n <- readIORef resRef
            if n > 0
            then return (st, ReadData (unsafeFreezeWithShrink arr))
            -- The write end is closed only by 'closeWatch'
            else if n == 0
            then return (st, ReadClosed)
            else do
                errno <- getErrno
                if errno == eAGAIN || errno == eWOULDBLOCK || errno == eINTR
                then fmap ReadWait <$> register st
                else throwErrno "readEvent"

    where

    bufSize = 32 * 1024

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
            -- The next read returns 'ReadClosed' if the exception is due to
            -- 'closeWatch'.
            let waitTh =
                    Exception.interruptible (atomically wait)
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
            closeReadFd fd
            return st1 {fsFdOpen = False}
        else return st1

-- | Process a record: forget the generation of a retired stream, stop the
-- streams that are replaced, and find the roots of an event. Returns the
-- event for each of its roots, see 'setRoot'. Returns no event for a retired
-- stream, for an event not reported while the streams are replaced, and for
-- an event not reported in the non-recursive mode.
processRecord :: Watch -> Record -> IO [Event]
processRecord Watch{watchConfig = cfg, watchStreamState = streams} rec =
    modifyMVar streams $ \ss -> go ss rec

    where

    fileEvents = createFlags cfg .&. kFSEventStreamCreateFlagFileEvents /= 0

    go ss (RecordRetired gen) =
        return
            ( ss
                { ssReplace = Replace.onRetired gen (ssReplace ss)
                , ssRoots = deleteGen gen (ssRoots ss)
                }
            , []
            )
    go ss (RecordEvent gen ev) = do
        let (report, stops, rs) =
                Replace.onEvent
                    gen (eventId ev) (isHistoryDone ev) (ssReplace ss)
        -- After 'closeWatch' the streams are stopped already
        ss1 <-
            if ssClosed ss
            then return ss {ssReplace = rs}
            else (\x -> x {ssReplace = rs}) <$> stopStreams stops ss
        if not report
        then return (ss1, [])
        else do
            evs <- setRoot (fromMaybe [] (lookup gen (ssRoots ss1))) ev
            if watchRec cfg
            then return (ss1, evs)
            else return (ss1, filter (isNonRecursiveEvent fileEvents) evs)

-- | The result of a step of 'readEvent'.
data ReadStep =
      StepEvent Event ReadState
    | StepContinue ReadState -- ^ No event yet, run the next step
      -- | No data is available. The action waits for it, it must be run.
    | StepWait (IO ())
    | StepEnd -- ^ The watch is closed and all the records are returned

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
-- Each reader gets its events in the order in which they occurred. When the
-- watch is closed, every reader gets 'Nothing'.
--
-- A read that is interrupted by an async exception, for example, by a
-- timeout, does not take an event out of the watch. The next read returns
-- the first event not yet read.
--
-- The read returns the event with the masking state of the caller. If async
-- exceptions are not masked, an exception that arrives after the read has
-- taken the event and before the caller records it loses the event. To avoid
-- that, call 'readEvent' and the code that records the event with async
-- exceptions masked. The read can still be interrupted: while it waits for an
-- event, and before it takes an event that is available.
--
-- /Pre-release/
--
readEvent :: Watch -> IO (Maybe Event)
readEvent wtch@Watch{watchReadState = stRef} =
    Exception.mask_ $ takeMVar stRef >>= loop

    where

    loop st = do
        -- The step does not block, see 'readPipe'. Running it
        -- uninterruptibly does not lose the bytes read from the pipe, and
        -- does not process a record without removing it from the buffer.
        r <- try (Exception.uninterruptibleMask_ (step st))
        case r of
            Left (e :: SomeException) -> putMVar stRef st >> throwIO e
            Right (StepEvent ev st1) -> deliver ev st1
            Right (StepContinue st1) -> loop st1
            Right (StepWait wait) -> do
                wait `onException` putMVar stRef st
                loop st
            Right StepEnd -> putMVar stRef st >> return Nothing

    -- An async exception that arrived during the step is pending. Raise it
    -- now and keep the event in the state, the next read returns it.
    -- Otherwise the exception would be raised when 'mask_' ends, after the
    -- event is taken out of the state, and the event would be lost. If the
    -- caller has masked async exceptions, this is the only point other than
    -- the wait where the read can be interrupted.
    deliver ev st = do
        r <- try (Exception.interruptible (return ()))
        case r of
            Left (e :: SomeException) -> do
                putMVar stRef st {rdUndelivered = ev : rdUndelivered st}
                throwIO e
            Right () -> putMVar stRef st >> return (Just ev)

    -- Return an event of the last processed record if any. Otherwise parse
    -- and process one record of the buffer. Read the pipe only when the
    -- buffer does not hold a complete record.
    step st@ReadState{rdUndelivered = ev : evs} =
        return (StepEvent ev st {rdUndelivered = evs})
    step st = do
        r <- parseRecord (rdBuffer st)
        case r of
            Just (rec, rest) -> do
                evs <- processRecord wtch rec
                -- The next step returns the first event
                return
                    $ StepContinue st {rdBuffer = rest, rdUndelivered = evs}
            Nothing | rdEnded st -> return StepEnd
            Nothing -> do
                res <- readPipe wtch
                case res of
                    -- We do not use StepEnd here because it does not carry the
                    -- state, the next step returns StepEnd after this state is
                    -- passed on and updated.
                    ReadClosed ->
                        return
                            $ StepContinue
                                st {rdBuffer = mempty, rdEnded = True}
                    ReadWait wait -> return (StepWait wait)
                    ReadData chunk ->
                        return
                            $ StepContinue st {rdBuffer = rdBuffer st <> chunk}

-- | Read the events from a 'Watch' as a stream, using 'readEvent'. The
-- stream does not close the watch, use 'closeWatch' to close it. The stream
-- ends when the watch is closed.
--
-- A stream can stop reading before the watch is closed. The next stream on
-- the watch starts with the first event not yet read.
--
-- The stream reports the events of the current roots of the watch: after
-- 'addRoot' it reports the events of the new root, after 'removeRoot' it
-- stops reporting the events of the removed root. See those functions for
-- the events reported while the roots change.
--
-- More than one stream can read the same watch at the same time. See
-- 'readEvent' for how the events are divided among the readers.
--
-- /Pre-release/
--
eventStream :: Watch -> Stream IO Event
eventStream wtch = S.unfoldrM step ()

    where

    step () = fmap (\ev -> (ev, ())) <$> readEvent wtch

-- | Start monitoring a list of file system paths for file system events with
-- the supplied configuration operation over the 'defaultConfig'. The paths
-- could be files or directories.  When the path is a directory and recursive
-- mode is set, the whole directory tree under it is watched recursively. See
-- 'setRecursiveMode' for the events reported in the non-recursive mode.
-- Monitoring starts from the current time onwards.
--
-- @
-- watchWith
--      ('setIgnoreSelf' 'True' . 'setRootPathEvents' 'True')
--      [Path.fromString "path"]
-- @
--
-- The stream does not end when a root is deleted or renamed, or when a root
-- does not exist. The watch is on the path of the root, an object created
-- at that path later is watched. On Linux and Windows a root that does not
-- exist raises an exception, and the stream ends when none of the roots is
-- watched any more.
--
-- Each run of the stream opens a new watch and closes it when the stream
-- ends. The handle of the watch is not exposed, so the roots of the watch
-- are the given paths for as long as the stream runs. To add or remove
-- roots while reading the events, open the watch using 'openWatch', read it
-- using 'eventStream' and change the roots using 'addRoot' and
-- 'removeRoot'.
watchWith ::
    (Config -> Config) -> NonEmpty Path -> Stream IO Event
watchWith f paths = S.bracketIO before closeWatch eventStream

    where

    before = openWatch f paths

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
-- watch API, with redundant separators and @.@ components removed. If a
-- path is under more than one root, for example, a root and a root inside
-- it, its events are reported once for each root.
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

-- | Get the absolute file system object path for which the event is
-- generated.
--
-- If the watch root was supplied as a relative path, it is taken relative to
-- the working directory at the time the root was added to the watch.
--
-- When the watch root is a symlink, the path returned is via the original
-- symlink and not through the resolved path.
--
-- If the event is for the watch root, and the root was supplied as an
-- absolute path, this is the same as 'getRoot'.
getAbsPath :: Event -> Path
getAbsPath Event{..} =
    case eventAbsRoot of
        -- The event path is not under any watch root
        Nothing -> eventPath
        Just root -> maybe root (Path.join root) eventRelPath

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
-- event is generated after the historical events, the events from before the
-- start of the watch, are reported. Its path is not meaningful.
--
-- Historical events are reported only when a watch is started from a past
-- event id. The watch APIs in this module always start a watch from the
-- current time, therefore, this event does not occur.
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
-- monitored path. This event is generated when any file system object is
-- created, including a hard link. Creating a hard link also generates an
-- 'isAttrsModified' event on the directory. When a hard link is deleted
-- 'isDeleted' and 'isHardLink' both are true.
--
-- This event can occur for the watched path if the path was deleted/moved and
-- created again (tested on macOS 10.15.1). If a watch is started on a
-- non-existing path and the path is created later, the path is watched
-- (tested on macOS 26).
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
-- monitored path. This event is generated when an object is renamed within the
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
-- application generates a "clone" event, and so does @cp -c@ on APFS. The
-- source and the clone both report 'isCloned', the clone also reports
-- 'isCreated'.
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
--
-- The format of the output is for debugging only, it cannot be relied upon and
-- can change at any time.
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
