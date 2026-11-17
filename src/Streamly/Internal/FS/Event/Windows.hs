-- Some code snippets are adapted from the fsnotify package.
-- http://hackage.haskell.org/package/fsnotify-0.3.0.1/
--
-- |
-- Module      : Streamly.Internal.FS.Event.Windows
-- Copyright   : (c) 2020 Composewell Technologies
--               (c) 2012, Mark Dittmer
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC

module Streamly.Internal.FS.Event.Windows
    (
    -- * Subscribing to events

    -- ** Default configuration
      Config
    , defaultConfig

    -- ** Watch Behavior
    , setRecursiveMode

    -- ** Events of Interest
    , setFileNameEvents
    , setDirNameEvents
    , setAttrsModified
    , setSecurityModified
    , setSizeModified
    , setLastWriteTimeModified
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

    -- ** Item CRUD events
    , isCreated
    , isDeleted
    , isModified
    , isMoved
    , isMovedFrom
    , isMovedTo

    -- ** Exception Conditions
    , isEventsLost

    -- * Debugging
    , showEvent
    )
where

import Control.Concurrent (forkIO, rtsSupportsBoundThreads)
import Control.Concurrent.MVar
    (MVar, modifyMVar, modifyMVar_, newMVar, withMVar)
import Control.Exception
    (IOException, SomeException, onException, throwIO, try)
import Control.Monad (unless, void, when)
import Data.Bits ((.|.), (.&.), complement)
import Data.Char (isAlpha, isAscii)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Word (Word8)
import Foreign.C.Types (CInt(..), CUIntPtr(..))
import Foreign.Marshal.Alloc (alloca, allocaBytes, free, mallocBytes)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Storable (peek, peekByteOff, poke)
import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr, nullFunPtr, plusPtr)
import GHC.Conc
    (TVar, atomically, newTVarIO, readTVar, retry, writeTVar)
import System.IO.Error (catchIOError)
import System.Win32.File
    ( LPOVERLAPPED
    , closeHandle
    , createFile
    , fILE_FLAG_BACKUP_SEMANTICS
    , fILE_FLAG_OVERLAPPED
    , fILE_LIST_DIRECTORY
    , fILE_NOTIFY_CHANGE_FILE_NAME
    , fILE_NOTIFY_CHANGE_DIR_NAME
    , fILE_NOTIFY_CHANGE_ATTRIBUTES
    , fILE_NOTIFY_CHANGE_SIZE
    , fILE_NOTIFY_CHANGE_LAST_WRITE
    , fILE_NOTIFY_CHANGE_SECURITY
    , fILE_SHARE_DELETE
    , fILE_SHARE_READ
    , fILE_SHARE_WRITE
    , oPEN_EXISTING
    )
import System.Win32.Info (getFullPathName)
import System.Win32.Types
    ( BOOL
    , DWORD
    , ErrCode
    , HANDLE
    , LPVOID
    , LPDWORD
    , failWith
    , getLastError
    , iNVALID_HANDLE_VALUE
    )

import Streamly.Data.Stream (Stream)
import Streamly.Internal.FileSystem.Path (Path)

import qualified Control.Exception as Exception (interruptible, mask_)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Streamly.Data.Stream as S
import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.FileSystem.Path as Path

-- | Watch configuration, used to specify the events of interest and the
-- behavior of the watch.
data Config = Config
    { watchRec :: BOOL
    , createFlags :: DWORD
    }

-------------------------------------------------------------------------------
-- Boolean settings
-------------------------------------------------------------------------------

setFlag :: DWORD -> Bool -> Config -> Config
setFlag mask status cfg@Config{..} =
    let flags =
            if status
            then createFlags .|. mask
            else createFlags .&. complement mask
    in cfg {createFlags = flags}

-- | Set watch event on directory recursively.
--
-- The entries already in a directory moved into the tree are not reported.
-- Scan the directory on its move event to find them.
--
-- /default: False/
setRecursiveMode :: Bool -> Config -> Config
setRecursiveMode recursive cfg@Config{} = cfg {watchRec = recursive}

-- | Generate notify events on file create, rename or delete.
--
-- From Windows API documentation: Any file name change in the watched
-- directory or subtree causes a change notification wait operation to return.
-- Changes include renaming, creating, or deleting a file.
--
-- /default: True/
setFileNameEvents :: Bool -> Config -> Config
setFileNameEvents = setFlag fILE_NOTIFY_CHANGE_FILE_NAME

-- | Generate notify events on directory create, rename or delete.
--
-- From Windows API documentaiton: Any directory-name change in the watched
-- directory or subtree causes a change notification wait operation to return.
-- Changes include creating or deleting a directory.
--
-- /default: True/
setDirNameEvents :: Bool -> Config -> Config
setDirNameEvents = setFlag fILE_NOTIFY_CHANGE_DIR_NAME

-- | Generate an 'isModified' event on any attribute change in the watched
-- directory or subtree.
--
-- /default: False/
setAttrsModified :: Bool -> Config -> Config
setAttrsModified = setFlag fILE_NOTIFY_CHANGE_ATTRIBUTES

-- | Generate an 'isModified' event when the file size is changed.
--
-- From Windows API documentation: Any file-size change in the watched
-- directory or subtree causes a change notification wait operation to return.
-- The operating system detects a change in file size only when the file is
-- written to the disk. For operating systems that use extensive caching,
-- detection occurs only when the cache is sufficiently flushed.
--
-- /default: True/
setSizeModified :: Bool -> Config -> Config
setSizeModified = setFlag fILE_NOTIFY_CHANGE_SIZE

-- | Generate an 'isModified' event when the last write timestamp of the file
-- inode is changed.
--
-- From Windows API documentation: Any change to the last write-time of files
-- in the watched directory or subtree causes a change notification wait
-- operation to return. The operating system detects a change to the last
-- write-time only when the file is written to the disk. For operating systems
-- that use extensive caching, detection occurs only when the cache is
-- sufficiently flushed.
--
-- /default: True/
setLastWriteTimeModified :: Bool -> Config -> Config
setLastWriteTimeModified = setFlag fILE_NOTIFY_CHANGE_LAST_WRITE

-- | Generate an 'isModified' event when any security-descriptor change occurs
-- in the watched directory or subtree.
--
-- /default: False/
setSecurityModified :: Bool -> Config -> Config
setSecurityModified = setFlag fILE_NOTIFY_CHANGE_SECURITY

-- | Set all tunable events 'True' or 'False'. Equivalent to setting:
--
-- * setFileNameEvents
-- * setDirNameEvents
-- * setAttrsModified
-- * setSizeModified
-- * setLastWriteTimeModified
-- * setSecurityModified
setAllEvents :: Bool -> Config -> Config
setAllEvents s =
      setFileNameEvents s
    . setDirNameEvents s
    . setAttrsModified s
    . setSizeModified s
    . setLastWriteTimeModified s
    . setSecurityModified s

-- | The tunable events that are enabled by default are:
--
-- * setFileNameEvents True
-- * setDirNameEvents True
-- * setSizeModified True
-- * setLastWriteTimeModified True
--
-- /Pre-release/
--
defaultConfig :: Config
defaultConfig =
      setFileNameEvents True
    $ setDirNameEvents True
    $ setSizeModified True
    $ setLastWriteTimeModified True
    $ Config {watchRec = False, createFlags = 0}

getConfigFlag :: Config -> DWORD
getConfigFlag Config{..} = createFlags

getConfigRecMode :: Config -> BOOL
getConfigRecMode Config{..} = watchRec

-- | 'totalBytes' is 0 for an event generated by the library for the watch
-- root, 'isEventsLost' or the deletion of the root.
data Event = Event
    { eventFlags :: DWORD
    , eventRelPath :: Path
    , eventRootPath :: Path -- ^ The watch root as supplied
    , eventAbsRootPath :: Path -- ^ The absolute path of the watch root
    , totalBytes :: DWORD
    }

-- For reference documentation see:
--
-- See https://docs.microsoft.com/en-us/windows/win32/api/winnt/ns-winnt-file_notify_information
data FILE_NOTIFY_INFORMATION = FILE_NOTIFY_INFORMATION
    { fniNextEntryOffset :: DWORD
    , fniAction :: DWORD
    , fniFileName :: Path
    }

type LPOVERLAPPED_COMPLETION_ROUTINE =
    FunPtr ((DWORD, DWORD, LPOVERLAPPED) -> IO ())

-- | Open a directory handle for watching.
openDirHandle :: Path -> IO HANDLE
openDirHandle dir = do
    let dirStr = Path.toString dir
    createFile dirStr
        -- Access mode
        fILE_LIST_DIRECTORY
        -- Share mode. FILE_SHARE_DELETE lets other processes delete or
        -- rename the watched directory.
        (fILE_SHARE_READ .|. fILE_SHARE_WRITE .|. fILE_SHARE_DELETE)
        -- Security attributes
        Nothing
        -- Create mode, we want to look at an existing directory
        oPEN_EXISTING
        -- File attribute, OVERLAPPED since the changes are read
        -- asynchronously
        (fILE_FLAG_BACKUP_SEMANTICS .|. fILE_FLAG_OVERLAPPED)
        -- No template file
        Nothing

-- | Close a handle, ignore a failure.
closeQuietly :: HANDLE -> IO ()
closeQuietly h = closeHandle h `catchIOError` \_ -> return ()

-- For reference documentation see:
--
-- See https://docs.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-readdirectorychangesw
-- Note that this API uses UTF-16 for file system paths:
-- 1. https://docs.microsoft.com/en-us/windows/win32/intl/unicode-in-the-windows-api
-- 2. https://docs.microsoft.com/en-us/windows/win32/intl/unicode
--
-- The handle is opened with FILE_FLAG_OVERLAPPED, the call starts a read and
-- returns without waiting for a change.
foreign import ccall unsafe
    "windows.h ReadDirectoryChangesW" c_ReadDirectoryChangesW ::
           HANDLE
        -> LPVOID
        -> DWORD
        -> BOOL
        -> DWORD
        -> LPDWORD
        -> LPOVERLAPPED
        -> LPOVERLAPPED_COMPLETION_ROUTINE
        -> IO BOOL

-- See https://learn.microsoft.com/en-us/windows/win32/fileio/createiocompletionport
foreign import ccall unsafe
    "windows.h CreateIoCompletionPort" c_CreateIoCompletionPort ::
        HANDLE -> HANDLE -> CUIntPtr -> DWORD -> IO HANDLE

-- See https://learn.microsoft.com/en-us/windows/win32/api/ioapiset/nf-ioapiset-getqueuedcompletionstatus
--
-- The call blocks until a completion packet is queued to the port.
foreign import ccall safe
    "windows.h GetQueuedCompletionStatus" c_GetQueuedCompletionStatus ::
           HANDLE
        -> LPDWORD
        -> Ptr CUIntPtr
        -> Ptr LPOVERLAPPED
        -> DWORD
        -> IO BOOL

-- See https://learn.microsoft.com/en-us/windows/win32/api/ioapiset/nf-ioapiset-postqueuedcompletionstatus
foreign import ccall unsafe
    "windows.h PostQueuedCompletionStatus" c_PostQueuedCompletionStatus ::
        HANDLE -> DWORD -> CUIntPtr -> LPOVERLAPPED -> IO BOOL

-- See https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-getfileinformationbyhandleex
foreign import ccall unsafe
    "windows.h GetFileInformationByHandleEx"
    c_GetFileInformationByHandleEx ::
        HANDLE -> CInt -> LPVOID -> DWORD -> IO BOOL

-- | The FileStandardInfo value of FILE_INFO_BY_HANDLE_CLASS.
fileStandardInfo :: CInt
fileStandardInfo = 1

-- | The size of FILE_STANDARD_INFO: two LARGE_INTEGER, a DWORD, two BOOLEAN
-- and the padding to an 8 byte boundary.
fileStandardInfoSize :: Int
fileStandardInfoSize = 24

-- | The offset of the DeletePending field in FILE_STANDARD_INFO.
deletePendingOffset :: Int
deletePendingOffset = 20

-- | Whether the file or directory of the handle has been deleted. A deleted
-- directory stays in the delete pending state, with its path still visible,
-- until all the handles to it are closed. Returns 'False' if the call
-- fails.
isDeletePending :: HANDLE -> IO Bool
isDeletePending h =
    allocaBytes fileStandardInfoSize $ \buf -> do
        ok <- c_GetFileInformationByHandleEx
                h fileStandardInfo buf (toEnum fileStandardInfoSize)
        if ok
        then (/= (0 :: Word8)) <$> peekByteOff buf deletePendingOffset
        else return False

-- | A ReadDirectoryChangesW call reports this error code when the watched
-- directory has been deleted, or when the access to it has been revoked.
eRROR_ACCESS_DENIED :: ErrCode
eRROR_ACCESS_DENIED = 5

-- | A ReadDirectoryChangesW call reports this error code when the system
-- was unable to record all the changes to the directory.
eRROR_NOTIFY_ENUM_DIR :: ErrCode
eRROR_NOTIFY_ENUM_DIR = 1022

peekFNI :: Ptr FILE_NOTIFY_INFORMATION -> IO FILE_NOTIFY_INFORMATION
peekFNI buf = do
    neof <- peekByteOff buf 0
    acti <- peekByteOff buf 4
    (fnle :: DWORD) <- peekByteOff buf 8
    fnam0 <- Array.fromPtrN (fromIntegral fnle) (buf `plusPtr` 12)
    -- Path.fromArray rejects names that the file system can hold, e.g.
    -- "aux.c" or "con", which can be created using a \\?\ path. The name is
    -- reported by the system, do not validate it.
    let fnam = Path.unsafeFromArray (Array.unsafeCast fnam0)
    return $ FILE_NOTIFY_INFORMATION neof acti fnam

-- | The events of the records of a read. The paths are the root as supplied
-- and its absolute path.
readChangeEvents ::
    Ptr FILE_NOTIFY_INFORMATION -> Path -> Path -> DWORD -> IO [Event]
readChangeEvents pfni root absRoot bytesRet = do
    fni <- peekFNI pfni
    let entry = Event
            { eventFlags = fniAction fni
            , eventRelPath = fniFileName fni
            , eventRootPath = root
            , eventAbsRootPath = absRoot
            , totalBytes = bytesRet
            }
        nioff = fromEnum $ fniNextEntryOffset fni
    entries <-
        if nioff == 0
        then return []
        else readChangeEvents (pfni `plusPtr` nioff) root absRoot bytesRet
    return $ entry : entries

-- XXX Try to get these from windows header files
type FileAction = DWORD

fILE_ACTION_ADDED             :: FileAction
fILE_ACTION_ADDED             =  1

fILE_ACTION_REMOVED           :: FileAction
fILE_ACTION_REMOVED           =  2

fILE_ACTION_MODIFIED          :: FileAction
fILE_ACTION_MODIFIED          =  3

fILE_ACTION_RENAMED_OLD_NAME  :: FileAction
fILE_ACTION_RENAMED_OLD_NAME  =  4

fILE_ACTION_RENAMED_NEW_NAME  :: FileAction
fILE_ACTION_RENAMED_NEW_NAME  =  5

-- | The event reported when the watch root has been deleted. 'totalBytes' 0
-- and 'eventFlags' 'fILE_ACTION_REMOVED' identify it. 'eventRelPath' is a
-- placeholder, 'getRelPath' returns 'Nothing'.
rootDeletedEvent :: Path -> Path -> Event
rootDeletedEvent root absRoot =
    Event
        { eventFlags = fILE_ACTION_REMOVED
        , eventRelPath = root
        , eventRootPath = root
        , eventAbsRootPath = absRoot
        , totalBytes = 0
        }

-- | The event reported when the changes were lost. 'totalBytes' 0 and
-- 'eventFlags' 0 identify it. 'eventRelPath' is a placeholder, 'getRelPath'
-- returns 'Nothing'.
eventsLostEvent :: Path -> Path -> Event
eventsLostEvent root absRoot =
    Event
        { eventFlags = 0
        , eventRelPath = root
        , eventRootPath = root
        , eventAbsRootPath = absRoot
        , totalBytes = 0
        }

-------------------------------------------------------------------------------
-- Watch handle
-------------------------------------------------------------------------------

-- Reading the changes:
--
-- Each root has a directory handle associated with the I/O completion port
-- of the watch, and at most one asynchronous ReadDirectoryChangesW call in
-- progress. The system starts recording the changes of a root on the first
-- call. The changes that occur between two calls are held in a buffer that
-- the system allocates on the first call, of the size of the buffer passed
-- to that call.
--
-- One thread for each watch waits for the completion packets of the port,
-- adds the events of a completed read to the event queue of the watch, and
-- then starts the next read of the root. It waits while the queue is full.
-- If the reader does not read the events fast enough the buffer of the
-- system overflows, the next read completes with no records and an
-- 'isEventsLost' event is reported.
--
-- A read completes with no records in two more cases, when the handle is
-- closed and when the buffer overflows. The completion status does not
-- tell them apart, the state of the root in the watch does.
--
-- From Windows Vista onwards, a read on a handle associated with a
-- completion port is not cancelled when the thread that started it exits.

-- | The size of the memory reserved for the OVERLAPPED structure of a root.
-- The structure is 32 bytes on a 64-bit system.
overlappedSize :: Int
overlappedSize = 64

-- | The size of the buffer passed to ReadDirectoryChangesW. On overflow of
-- the buffer of the system, the read completes with zero bytes or with
-- 'eRROR_NOTIFY_ENUM_DIR'.
changeBufferSize :: Int
changeBufferSize = 63 * 1024

-- | The number of events in the queue of a watch above which the reads of
-- the roots are not started.
maxQueued :: Int
maxQueued = 1024

-- | A root of a watch.
data Root = Root
    { rootPath :: Path -- ^ As supplied by the user
      -- | The absolute path of the root, see 'toAbsolute'
    , rootAbsPath :: Path
    , rootHandle :: HANDLE
      -- | 'overlappedSize' bytes for the OVERLAPPED structure followed by
      -- 'changeBufferSize' bytes for the changes. The system writes to it
      -- while a read is in progress.
    , rootMem :: Ptr Word8
    }

-- | The roots of a watch. A root in 'rsActive' has a read in progress,
-- except the 'rsBusy' root.
data Roots = Roots
    { rsClosed :: !Bool -- ^ 'closeWatch' was called
    , rsNextKey :: !CUIntPtr -- ^ The completion key of the next root
    , rsActive :: Map CUIntPtr Root -- ^ The roots, by completion key
      -- | The root whose completed read is being processed by the thread
      -- of the watch. The thread frees its memory if the root is removed
      -- meanwhile.
    , rsBusy :: Maybe CUIntPtr
      -- | The memory of the removed roots whose read is yet to complete,
      -- by completion key. Their handles are closed.
    , rsRetired :: Map CUIntPtr (Ptr Word8)
    }

-- | An item of the event queue of a watch.
data Item =
      ItemEvent Event
      -- | A read of a root failed, the exception is raised to the reader.
    | ItemFailed IOException

data Queue = Queue
    { qClosed :: !Bool -- ^ 'closeWatch' was called
    , qItems :: Seq Item
    }

-- | A handle for a watch.
data Watch = Watch
    { watchConfig :: Config -- ^ The config used to open the watch
    , watchPort :: HANDLE -- ^ The I/O completion port
      -- | Held while the roots are changed and while the handle or the
      -- memory of a root is used. Not held across a blocking call.
    , watchRoots :: MVar Roots
    , watchQueue :: TVar Queue
    }

-- | Start a read on the root. Returns the error code if the call fails, no
-- completion packet is queued in that case.
startRead :: Config -> Root -> IO (Maybe ErrCode)
startRead cfg Root{..} = do
    fillBytes rootMem 0 overlappedSize
    ok <- c_ReadDirectoryChangesW
            rootHandle
            (castPtr (rootMem `plusPtr` overlappedSize))
            (fromIntegral changeBufferSize)
            (getConfigRecMode cfg)
            (getConfigFlag cfg)
            nullPtr
            (castPtr rootMem)
            nullFunPtr
    if ok then return Nothing else Just <$> getLastError

-- | The 'IOException' for a Windows error code.
errorToException :: String -> ErrCode -> IO IOException
errorToException name err = do
    r <- try (failWith name err)
    return $ either id (\() -> userError name) r

-- | The result of a read of a root.
data Outcome =
      Changes DWORD -- ^ The buffer holds this many bytes of records
    | ChangesLost
    | RootDeleted
    | ReadFailed IOException

-- | Classify the result of a read: the error code, or the number of bytes
-- returned. The handle of the root must be open. A deleted root reports
-- 'eRROR_ACCESS_DENIED' and its directory is delete pending.
classify :: HANDLE -> Either ErrCode DWORD -> IO Outcome
classify _ (Right 0) = return ChangesLost
classify _ (Right n) = return (Changes n)
classify h (Left err)
    | err == eRROR_NOTIFY_ENUM_DIR = return ChangesLost
    | otherwise = do
        deleted <-
            if err == eRROR_ACCESS_DENIED
            then isDeletePending h
            else return False
        if deleted
        then return RootDeleted
        else ReadFailed <$> errorToException "ReadDirectoryChangesW" err

-- | Whether the root is not watched after the outcome.
isTerminal :: Outcome -> Bool
isTerminal (Changes _) = False
isTerminal ChangesLost = False
isTerminal RootDeleted = True
isTerminal (ReadFailed _) = True

-- | The queue items for the outcome of a read. The read must be complete,
-- and the memory of the root must not be freed during the call.
outcomeItems :: Root -> Outcome -> IO [Item]
outcomeItems Root{..} outcome =
    case outcome of
        Changes n ->
            let buf = castPtr (rootMem `plusPtr` overlappedSize)
             in map ItemEvent
                    <$> readChangeEvents buf rootPath rootAbsPath n
        ChangesLost ->
            return [ItemEvent (eventsLostEvent rootPath rootAbsPath)]
        RootDeleted ->
            return [ItemEvent (rootDeletedEvent rootPath rootAbsPath)]
        ReadFailed e -> return [ItemFailed e]

-- | Add the items to the event queue of the watch. Wait while the queue
-- holds 'maxQueued' items or more. The items are dropped if the watch is
-- closed.
enqueue :: Watch -> [Item] -> IO ()
enqueue Watch{..} items =
    atomically $ do
        q <- readTVar watchQueue
        if qClosed q
        then return ()
        else if Seq.length (qItems q) >= maxQueued
        then retry
        else writeTVar watchQueue q {qItems = qItems q <> Seq.fromList items}

-- | Remove the roots that satisfy the predicate and close their handles.
-- The lock of the roots must be held. The memory of a root is freed when
-- its read completes.
retireRoots :: (Root -> Bool) -> Roots -> IO Roots
retireRoots p rs = do
    let (gone, kept) = Map.partition p (rsActive rs)
        -- The busy root has no read in progress
        reading = Map.filterWithKey (\k _ -> Just k /= rsBusy rs) gone
    mapM_ (closeQuietly . rootHandle) (Map.elems gone)
    return
        rs
            { rsActive = kept
            , rsRetired = Map.union (Map.map rootMem reading) (rsRetired rs)
            }

-- | Close a 'Watch' handle. The event streams of the watch end, a thread
-- waiting for events in 'readEvent' or 'eventStream' is woken up. The
-- events already in the event queue of the watch are still returned by
-- the reads, followed by 'Nothing'. Closing a closed watch does nothing.
--
-- The directory handles of the roots are closed when the call returns.
--
-- Can be called from any thread, including while another thread reads the
-- events of the watch using 'eventStream'. The call does not wait for an
-- event.
--
-- /Pre-release/
--
closeWatch :: Watch -> IO ()
closeWatch Watch{..} =
    modifyMVar_ watchRoots $ \rs ->
        if rsClosed rs
        then return rs
        else do
            atomically $ do
                q <- readTVar watchQueue
                writeTVar watchQueue q {qClosed = True}
            rs1 <- retireRoots (const True) rs
            -- Wake up the thread of the watch, the roots may not have any
            -- read in progress.
            void $ c_PostQueuedCompletionStatus watchPort 0 0 nullPtr
            return rs1 {rsClosed = True}

-- | A completion packet of the port of a watch.
data Completion =
      -- | The packet posted by 'closeWatch'
      Wake
      -- | The read of the root with this completion key is complete, with
      -- an error code or the number of bytes returned.
    | Completed CUIntPtr (Either ErrCode DWORD)
      -- | No packet could be read from the port
    | PortFailed ErrCode

-- | Wait for a completion packet of the port.
getCompletion :: HANDLE -> IO Completion
getCompletion port =
    alloca $ \pBytes ->
    alloca $ \pKey ->
    alloca $ \pOverlapped -> do
        poke pOverlapped nullPtr
        ok <- c_GetQueuedCompletionStatus
                port pBytes pKey pOverlapped 0xFFFFFFFF
        err <- if ok then return 0 else getLastError
        overlapped <- peek pOverlapped
        -- The packet of a read has the OVERLAPPED pointer of the read
        if overlapped == nullPtr
        then return (if ok then Wake else PortFailed err)
        else do
            key <- peek pKey
            if ok
            then Completed key . Right <$> peek pBytes
            else return (Completed key (Left err))

-- | The thread of a watch. Processes the completion packets of the port
-- until the watch is closed and the reads of all its roots are complete,
-- then closes the port.
pumpEvents :: Watch -> IO ()
pumpEvents w@Watch{..} = loop

    where

    -- Take the root of a completed read, with the lock held.
    takeRoot key res rs =
        case Map.lookup key (rsRetired rs) of
            Just mem -> do
                free mem
                let retired = Map.delete key (rsRetired rs)
                return (rs {rsRetired = retired}, Nothing)
            Nothing ->
                case Map.lookup key (rsActive rs) of
                    Nothing -> return (rs, Nothing)
                    Just root -> do
                        outcome <- classify (rootHandle root) res
                        return (rs {rsBusy = Just key}, Just (root, outcome))

    -- Start the next read of the busy root or release the root, with the
    -- lock held. If the read cannot be started, returns the outcome and the
    -- root remains busy.
    releaseRoot key root outcome rs
        -- The root was removed, or the watch was closed, the handle is
        -- closed.
        | not (Map.member key (rsActive rs)) = do
            free (rootMem root)
            return (rs {rsBusy = Nothing}, Nothing)
        | isTerminal outcome = do
            closeQuietly (rootHandle root)
            free (rootMem root)
            let active = Map.delete key (rsActive rs)
            return (rs {rsBusy = Nothing, rsActive = active}, Nothing)
        | otherwise = do
            r <- startRead watchConfig root
            case r of
                Nothing -> return (rs {rsBusy = Nothing}, Nothing)
                Just err -> do
                    outcome1 <- classify (rootHandle root) (Left err)
                    return (rs, Just outcome1)

    process key root outcome = do
        items <- outcomeItems root outcome
        enqueue w items
        r <- modifyMVar watchRoots (releaseRoot key root outcome)
        mapM_ (process key root) r

    isDone rs = rsClosed rs && Map.null (rsRetired rs)

    next = do
        done <- withMVar watchRoots (return . isDone)
        if done then closeQuietly watchPort else loop

    loop = do
        c <- getCompletion watchPort
        case c of
            Wake -> next
            Completed key res -> do
                r <- modifyMVar watchRoots (takeRoot key res)
                mapM_ (uncurry (process key)) r
                next
            -- Not expected. The reads in progress cannot be completed, the
            -- memory of their roots is not freed.
            PortFailed err -> do
                e <- errorToException "GetQueuedCompletionStatus" err
                enqueue w [ItemFailed e]
                closeWatch w
                closeQuietly watchPort

-- XXX
-- Remove the encoding requirement of paths? It can be encoding agnostic
-- "\" separated bytes, the application can decide what encoding to use.
-- Instead of always using widechar (-W) APIs can we call the underlying APIs
-- based on the configured code page?
-- https://docs.microsoft.com/en-us/windows/uwp/design/globalizing/use-utf8-code-page

-- | The thread of a watch waits for the completion packets in a blocking
-- foreign call. With the non-threaded RTS the call blocks all the Haskell
-- threads.
checkThreadedRTS :: IO ()
checkThreadedRTS =
    unless rtsSupportsBoundThreads
        $ ioError $ userError
        $ "Streamly.FileSystem.Event.Windows: watching requires the "
            ++ "threaded RTS, link the program with -threaded"

-- | Create a watch with no root.
createWatch :: Config -> IO Watch
createWatch cfg = do
    port <- c_CreateIoCompletionPort iNVALID_HANDLE_VALUE nullPtr 0 1
    when (port == nullPtr)
        $ getLastError >>= failWith "CreateIoCompletionPort"
    roots <-
        newMVar
            Roots
                { rsClosed = False
                -- 0 is the key of the packet posted by 'closeWatch'
                , rsNextKey = 1
                , rsActive = Map.empty
                , rsBusy = Nothing
                , rsRetired = Map.empty
                }
    queue <- newTVarIO Queue {qClosed = False, qItems = Seq.empty}
    let w =
            Watch
                { watchConfig = cfg
                , watchPort = port
                , watchRoots = roots
                , watchQueue = queue
                }
    void $ forkIO (pumpEvents w)
    return w

-- | Whether the path starts with a drive followed by a separator, or with
-- two separators. Such a path does not depend on the current directory or
-- the current drive. For example, @C:\\dir@ and @\\\\server\\share@ are
-- fully qualified, @dir@, @\\dir@ and @C:dir@ are not.
isFullyQualified :: String -> Bool
isFullyQualified str =
    case str of
        a : b : _ | isSep a && isSep b -> True
        d : ':' : c : _ -> isAscii d && isAlpha d && isSep c
        _ -> False

    where

    isSep c = c == '\\' || c == '/'

-- | The absolute path of a root. A path that is not fully qualified is
-- resolved against the current directory, symbolic links are not resolved.
toAbsolute :: Path -> IO Path
toAbsolute path
    | isFullyQualified str = return path
    -- The path is returned by the system, do not validate it, see 'peekFNI'.
    | otherwise = Path.unsafeFromString <$> getFullPathName str

    where

    str = Path.toString path

-- | @addRoot watch root@ adds the directory @root@ as a root of the watch
-- @watch@, with the 'Config' used to open the watch. The root is watched
-- when the call returns: a change made after it is reported. A relative
-- @root@ is relative to the current directory at the time of the call, a
-- later change of the current directory does not affect the watch.
--
-- A directory added more than once, via the same path or via different
-- roots, reports its events once for each root.
--
-- Can be called from any thread, including while another thread reads the
-- events of the watch using 'eventStream'. The call does not wait for an
-- event. Raise an exception if the watch is closed, or if the directory
-- cannot be opened or watched.
--
-- /Pre-release/
--
addRoot :: Watch -> Path -> IO ()
addRoot Watch{..} root =
    modifyMVar_ watchRoots $ \rs -> do
        when (rsClosed rs)
            $ ioError (userError "addRoot: the watch is closed.")
        let key = rsNextKey rs
        absRoot <- toAbsolute root
        h <- openDirHandle root
        flip onException (closeQuietly h) $ do
            port <- c_CreateIoCompletionPort h watchPort key 0
            when (port == nullPtr)
                $ getLastError >>= failWith "CreateIoCompletionPort"
            mem <- mallocBytes (overlappedSize + changeBufferSize)
            let r =
                    Root
                        { rootPath = root
                        , rootAbsPath = absRoot
                        , rootHandle = h
                        , rootMem = mem
                        }
            res <- startRead watchConfig r
            case res of
                Nothing -> return ()
                Just err -> do
                    free mem
                    failWith "ReadDirectoryChangesW" err
            return
                rs
                    { rsNextKey = key + 1
                    , rsActive = Map.insert key r (rsActive rs)
                    }

-- | Remove a root path from a 'Watch'. The path must be the same as the
-- one used to add the root. If the path was added more than once, all the
-- roots added using it are removed.
--
-- The directory handle of the root is closed when the call returns. The
-- events of the root already in the event queue of the watch are reported,
-- the changes that the watch has not yet read from the system are not
-- reported. No event of the root is reported for a change made after the
-- call returns. There is no event to mark the end of the events of the
-- root, unlike the @isRootUnwatched@ event on Linux. A directory that is
-- also watched via another root remains watched for that root.
--
-- The events of the other roots of the watch are not lost or duplicated by
-- the call.
--
-- Can be called from any thread, including while another thread reads the
-- events of the watch using 'eventStream'. The call does not wait for an
-- event. Does nothing if the watch is closed or if the path is not a root
-- of the watch.
--
-- /Pre-release/
--
removeRoot :: Watch -> Path -> IO ()
removeRoot Watch{..} root =
    modifyMVar_ watchRoots (retireRoots isRoot)

    where

    name = Path.toString root
    isRoot = (== name) . Path.toString . rootPath

-- | Given a 'Config' and @paths@ start monitoring the directories for file
-- system events. Returns a 'Watch' handle which can then be used to read
-- the event stream or to close the watch. The watch is active when the call
-- returns: a change made after it is reported.
--
-- The 'Config' is kept in the 'Watch', 'addRoot' uses it.
--
-- The watch holds up to 1024 events that are not yet read, and the events
-- of one more read of each root. Beyond that the changes are held by the
-- system, in a buffer of 63 KiB for each root. An 'isEventsLost' event is
-- reported for a root if its buffer overflows.
--
-- A watch uses one thread, an I/O completion port, and a directory handle
-- for each root.
--
-- Raise an exception if the program uses the non-threaded RTS. If adding a
-- path fails, the watch is closed and the exception is rethrown.
--
-- /Pre-release/
--
openWatch :: Config -> NonEmpty Path -> IO Watch
openWatch cfg paths = do
    checkThreadedRTS
    -- The handle is not returned to the caller if adding a root fails, the
    -- caller cannot close it.
    Exception.mask_ $ do
        w <- createWatch cfg
        mapM_ (addRoot w) (NonEmpty.toList paths) `onException` closeWatch w
        return w

-------------------------------------------------------------------------------
-- Reading events
-------------------------------------------------------------------------------

-- | Read the next event from a 'Watch', wait for one if none is available.
-- Returns 'Nothing' when the watch is closed and the events in its event
-- queue are read, and on every later call.
--
-- While a thread reads the watch, other threads can use 'addRoot',
-- 'removeRoot' and 'closeWatch' on it.
--
-- More than one thread can read the same watch at the same time, using
-- 'readEvent' or 'eventStream'. Each event is returned to exactly one of
-- the readers. Each reader gets its events in the order in which they
-- occurred. Related events can go to different readers, for example, the
-- 'isMovedFrom' and 'isMovedTo' events of a rename. When the watch is
-- closed, every reader gets 'Nothing'.
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
-- When a root is deleted, an 'isDeleted' event is returned for the root
-- and the root is removed from the watch.
--
-- If the system fails to report the changes of a root, the read raises
-- the 'IOException' once and the root is removed from the watch. The next
-- read continues with the events of the other roots.
--
-- /Pre-release/
--
readEvent :: Watch -> IO (Maybe Event)
readEvent Watch{..} =
    Exception.mask_ $ do
        r <- atomically $ do
            q <- readTVar watchQueue
            case Seq.viewl (qItems q) of
                item Seq.:< rest -> do
                    writeTVar watchQueue q {qItems = rest}
                    return (Just item)
                Seq.EmptyL -> if qClosed q then return Nothing else retry
        case r of
            Nothing -> return Nothing
            Just item -> do
                -- The transaction is interrupted only if it waits. An async
                -- exception that arrived while it removed the item is
                -- pending. Raise it now and put the item back, the next
                -- read returns it. Otherwise the exception would be raised
                -- when 'mask_' ends and the item would be lost.
                i <- try (Exception.interruptible (return ()))
                case i of
                    Left (e :: SomeException) -> do
                        atomically $ do
                            q <- readTVar watchQueue
                            writeTVar
                                watchQueue q {qItems = item Seq.<| qItems q}
                        throwIO e
                    Right () ->
                        case item of
                            ItemEvent ev -> return (Just ev)
                            ItemFailed e -> ioError e

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
-- 'readEvent' for how the events are divided among the readers, and for
-- async exceptions and failures.
--
-- /Pre-release/
--
eventStream :: Watch -> Stream IO Event
eventStream wtch = S.unfoldrM (\() -> fmap (, ()) <$> readEvent wtch) ()

-- | Whether the event is the one reported when a watch root is deleted.
isRootDeletedEvent :: Event -> Bool
isRootDeletedEvent Event{..} =
    totalBytes == 0 && eventFlags == fILE_ACTION_REMOVED

-- | Start monitoring a list of directory paths for file system events with the
-- supplied configuration modifier operation over the' defaultConfig'. The
-- paths could be directories or symbolic links to directories.
--
-- Raise an exception when the stream starts if the program uses the
-- non-threaded RTS, or if a path cannot be watched.
--
-- @
-- watchWith
--  ('setAttrsModified' True . 'setLastWriteTimeModified' False)
--  [Path.fromString "dir"]
-- @
--
-- Each run of the stream opens a new watch and closes it when the stream
-- ends. The stream ends after the 'isDeleted' event of the last root when
-- all the roots are deleted. The handle of the watch is not exposed, so the
-- roots of the watch are the given paths for as long as the stream runs. To
-- add or remove roots while reading the events, open the watch using
-- 'openWatch', read it using 'eventStream' and change the roots using
-- 'addRoot' and 'removeRoot'.
watchWith :: (Config -> Config) -> NonEmpty Path -> Stream IO Event
watchWith f paths = S.bracketIO before closeWatch events

    where

    before = openWatch (f defaultConfig) paths

    -- The state is the number of roots that are not deleted
    step _ 0 = return Nothing
    step w n = do
        r <- readEvent w
        return $ case r of
            Nothing -> Nothing
            Just ev | isRootDeletedEvent ev -> Just (ev, n - 1)
            Just ev -> Just (ev, n)

    events w = S.unfoldrM (step w) (NonEmpty.length paths)

-- | Same as 'watchWith' using 'defaultConfig' and recursive mode.
--
-- >>> watchRecursive = watchWith (setRecursiveMode True)
watchRecursive :: NonEmpty Path -> Stream IO Event
watchRecursive = watchWith (setRecursiveMode True)

-- | Same as 'watchWith' using defaultConfig and non-recursive mode.
--
-- >>> watch = watchWith id
watch :: NonEmpty Path -> Stream IO Event
watch = watchWith id

getFlag :: DWORD -> Event -> Bool
getFlag mask Event{..} = eventFlags == mask

-- | Get the file system object path for which the event is generated, relative
-- to the watched root. Windows does not report events for the watch root
-- itself, therefore, this is a 'Just' value for all events except
-- 'isEventsLost' and the 'isDeleted' event for the deletion of the watch
-- root, for which it is 'Nothing'.
getRelPath :: Event -> Maybe Path
getRelPath Event{..} =
    -- totalBytes == 0 is an event generated by the library for the root
    if totalBytes == 0
    then Nothing
    else Just eventRelPath

-- | Get the watch root directory to which this event belongs.
getRoot :: Event -> Path
getRoot Event{..} = eventRootPath

-- | Get the absolute file system object path for which the event is generated.
--
-- If the watch root was supplied as a relative path, it is taken relative to
-- the current directory at the time the root was added to the watch.
--
-- When the watch root is a symlink, the absolute path returned is via the
-- original symlink and not through the resolved path.
--
-- If the event is for the watch root, and the root was supplied as an
-- absolute path, this is the same as 'getRoot'.
getAbsPath :: Event -> Path
getAbsPath ev@Event{..} =
    case getRelPath ev of
        -- The path is relative to the root as reported by the system.
        -- Path.join raises an exception if the path starts with a letter
        -- and a colon, e.g. a stream "a:s" of a file named "a".
        Just relPath -> Path.unsafeJoin eventAbsRootPath relPath
        Nothing -> eventAbsRootPath

-- XXX need to document the exact semantics of these.
--
-- | This event is generated when a file or directory is created in a watched
-- directory or directory tree when in recursive watch mode.  Creating a hard
-- link also generates this event.
--
-- /Occurs when either 'setFileNameEvents' or 'setDirNameEvents' is enabled/
isCreated :: Event -> Bool
isCreated = getFlag fILE_ACTION_ADDED

-- | This event is generated when a file or directory is deleted from the
-- watched directory or directory tree when in recursive mode. This event is
-- generated even when a hard link is deleted.
--
-- When the watch root is deleted, this event is generated for the root,
-- 'getRelPath' returns 'Nothing', and the watch of the root ends.
--
-- /Occurs when either 'setFileNameEvents' or 'setDirNameEvents' is enabled/
isDeleted :: Event -> Bool
isDeleted = getFlag fILE_ACTION_REMOVED

-- | Generated for the original path when an object is moved from under a
-- monitored directory.
--
-- /Occurs when either 'setFileNameEvents' or 'setDirNameEvents' is enabled/
isMovedFrom :: Event -> Bool
isMovedFrom = getFlag fILE_ACTION_RENAMED_OLD_NAME

-- | Generated for the new path when an object is moved under a monitored
-- directory.
--
-- /Occurs when either 'setFileNameEvents' or 'setDirNameEvents' is enabled/
isMovedTo :: Event -> Bool
isMovedTo = getFlag fILE_ACTION_RENAMED_NEW_NAME

-- | Generated for a path that is moved from or moved to the monitored
-- directory.
--
-- >>> isMoved ev = isMovedFrom ev || isMovedTo ev
--
-- /Occurs when either 'setFileNameEvents' or 'setDirNameEvents' is enabled/
-- /Occurs only for an object inside the watched directory/
isMoved :: Event -> Bool
isMoved ev = isMovedFrom ev || isMovedTo ev

-- XXX This event is generated only for files and not directories?
--
-- | This event occurs when a file or directory contents, timestamps or
-- attributes are modified.  Since it can occur on multiple changes, you may
-- have to check the attributes to know what exactly changed when multiple type
-- of modified events are enabled.
--
-- In non-recursive mode this event does not occur for directories.  In
-- recursive mode this event occurs for the parent directory if a file or
-- directory inside it is created or renamed.
--
-- /Occurs when one of the @set*Modified@ events is enabled/
isModified :: Event -> Bool
isModified = getFlag fILE_ACTION_MODIFIED

-- | The system buffer of a watch root overflowed and the changes in it were
-- lost. The overflow occurs when the application does not read the events
-- fast enough.
--
-- The watch continues after this event. The application scans the tree
-- under the watch root to know its current state. See
-- @docs\/robustness.md@ in this package for the recovery.
--
-- For this event, 'getRoot' and 'getAbsPath' return the watch root whose
-- events were lost, and 'getRelPath' returns 'Nothing'.
isEventsLost :: Event -> Bool
isEventsLost Event{..} = totalBytes == 0 && eventFlags == 0

-------------------------------------------------------------------------------
-- Debugging
-------------------------------------------------------------------------------

-- | Convert an 'Event' record to a String representation.
--
-- The format of the output is for debugging only, it cannot be relied upon and
-- can change at any time.
--
-- Paths are decoded leniently, invalid UTF-16 such as an unpaired surrogate,
-- which NTFS allows in names, is shown as the Unicode replacement character.
showEvent :: Event -> String
showEvent ev@Event{..} =
        "--------------------------"
    ++ "\nRoot = " ++ Path.toString_ (getRoot ev)
    ++ "\nPath = " ++ maybe "" Path.toString_ (getRelPath ev)
    ++ "\ngetAbsPath = " ++ Path.toString_ (getAbsPath ev)
    ++ "\nFlags " ++ show eventFlags
    ++ showev isEventsLost "Overflow"
    ++ showev isCreated "Created"
    ++ showev isDeleted "Deleted"
    ++ showev isModified "Modified"
    ++ showev isMovedFrom "MovedFrom"
    ++ showev isMovedTo "MovedTo"
    ++ "\n"

    where showev f str = if f ev then "\n" ++ str else ""
