{-# LANGUAGE InterruptibleFFI #-}

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

import Control.Concurrent (rtsSupportsBoundThreads)
import Control.Monad (unless)
import Data.Bits ((.|.), (.&.), complement)
import Data.List.NonEmpty (NonEmpty)
import Data.Word (Word8)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Storable (peekByteOff)
import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr, nullFunPtr, plusPtr)
import System.Win32.File
    ( FileNotificationFlag
    , LPOVERLAPPED
    , closeHandle
    , createFile
    , fILE_FLAG_BACKUP_SEMANTICS
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
import System.Win32.Types
    ( BOOL
    , DWORD
    , ErrCode
    , HANDLE
    , LPVOID
    , LPDWORD
    , failWith
    , getLastError
    )

import Streamly.Data.Stream (Stream)
import Streamly.Data.Stream.Prelude (eager)
import Streamly.Internal.FileSystem.Path (Path)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Streamly.Data.Stream as S
import qualified Streamly.Data.Stream.Prelude as S
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
    , eventRootPath :: Path
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
        -- File attribute, NOT using OVERLAPPED since we work synchronously
        fILE_FLAG_BACKUP_SEMANTICS
        -- No template file
        Nothing

-- For reference documentation see:
--
-- See https://docs.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-readdirectorychangesw
-- Note that this API uses UTF-16 for file system paths:
-- 1. https://docs.microsoft.com/en-us/windows/win32/intl/unicode-in-the-windows-api
-- 2. https://docs.microsoft.com/en-us/windows/win32/intl/unicode
--
-- The call blocks until a change occurs. It is interruptible so that an
-- exception thrown to the calling thread, e.g. when the stream is stopped,
-- does not wait for the next change: the RTS cancels the call with
-- CancelSynchronousIo and the call fails with ERROR_OPERATION_ABORTED.
foreign import ccall interruptible
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

-- | An asynchronous ReadDirectoryChangesW call reports this error code when
-- the system was unable to record all the changes to the directory.
eRROR_NOTIFY_ENUM_DIR :: ErrCode
eRROR_NOTIFY_ENUM_DIR = 1022

-- | The result of a ReadDirectoryChangesW call.
data ReadResult
    = ReadChanges -- ^ The call succeeded.
    | ReadEnumDir -- ^ The call failed with 'eRROR_NOTIFY_ENUM_DIR'.
    | ReadRootDeleted -- ^ The watched directory has been deleted.

-- | Returns 'ReadEnumDir' if the call failed with 'eRROR_NOTIFY_ENUM_DIR',
-- in that case the number of bytes returned is not set. Returns
-- 'ReadRootDeleted' if the call failed with 'eRROR_ACCESS_DENIED' and the
-- directory of the handle is delete pending. Throws an exception on any
-- other failure.
readDirectoryChangesW ::
       HANDLE
    -> Ptr FILE_NOTIFY_INFORMATION
    -> DWORD
    -> BOOL
    -> FileNotificationFlag
    -> LPDWORD
    -> IO ReadResult
readDirectoryChangesW h buf bufSize wst f br = do
    res <- c_ReadDirectoryChangesW
                h (castPtr buf) bufSize wst f br nullPtr nullFunPtr
    if res
    then return ReadChanges
    else do
        err <- getLastError
        if err == eRROR_NOTIFY_ENUM_DIR
        then return ReadEnumDir
        else do
            deleted <-
                if err == eRROR_ACCESS_DENIED
                then isDeletePending h
                else return False
            if deleted
            then return ReadRootDeleted
            else failWith "ReadDirectoryChangesW" err

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

readChangeEvents ::
    Ptr FILE_NOTIFY_INFORMATION -> Path -> DWORD -> IO [Event]
readChangeEvents pfni root bytesRet = do
    fni <- peekFNI pfni
    let entry = Event
            { eventFlags = fniAction fni
            , eventRelPath = fniFileName fni
            , eventRootPath = root
            , totalBytes = bytesRet
            }
        nioff = fromEnum $ fniNextEntryOffset fni
    entries <-
        if nioff == 0
        then return []
        else readChangeEvents (pfni `plusPtr` nioff) root bytesRet
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
rootDeletedEvent :: Path -> Event
rootDeletedEvent root =
    Event
        { eventFlags = fILE_ACTION_REMOVED
        , eventRelPath = root
        , eventRootPath = root
        , totalBytes = 0
        }

-- | The event reported when the changes were lost. 'totalBytes' 0 and
-- 'eventFlags' 0 identify it. 'eventRelPath' is a placeholder, 'getRelPath'
-- returns 'Nothing'.
eventsLostEvent :: Path -> Event
eventsLostEvent root =
    Event
        { eventFlags = 0
        , eventRelPath = root
        , eventRootPath = root
        , totalBytes = 0
        }

-- | The changes that occur between two ReadDirectoryChangesW calls are held
-- in a buffer that the system allocates on the first call. This function
-- calls ReadDirectoryChangesW with a 63 KiB buffer.
--
-- On overflow of the system's change buffer, a synchronous call succeeds
-- with zero bytes returned. An asynchronous call reports the same condition
-- as 'eRROR_NOTIFY_ENUM_DIR'; handle that error code too in case a
-- synchronous call also reports it. The buffer has no records in either
-- case.
--
-- The 'Bool' in the result is 'False' if the watched directory has been
-- deleted; no more changes can be read from the handle after that.
readDirectoryChanges ::
    Path -> HANDLE -> Bool -> FileNotificationFlag -> IO ([Event], Bool)
readDirectoryChanges root h wst mask = do
    let maxBuf = 63 * 1024
    allocaBytes maxBuf $ \buffer -> do
        alloca $ \bret -> do
            r <- readDirectoryChangesW h buffer (toEnum maxBuf) wst mask bret
            case r of
                ReadChanges -> do
                    bytesRet <- peekByteOff bret 0
                    if bytesRet == 0
                    then return ([eventsLostEvent root], True)
                    else do
                        evs <- readChangeEvents buffer root bytesRet
                        return (evs, True)
                ReadEnumDir -> return ([eventsLostEvent root], True)
                ReadRootDeleted -> return ([rootDeletedEvent root], False)

-- | The events of one watch root. The stream ends after the event for the
-- deletion of the root. The directory handle is opened when the stream
-- starts and closed when it ends, on an exception, or on garbage collection.
-- A deleted root is removed from the file system only after the handle is
-- closed. The handle is never closed while a read on it is in progress: on an
-- exception, the worker thread reading the handle closes it after the read
-- returns; the stream is garbage collected only after the worker thread has
-- stopped running it.
eventStreamAggr :: Config -> Path -> Stream IO Event
eventStreamAggr cfg rootPath =
    S.bracketIO (openDirHandle rootPath) closeHandle $ \handle ->
        let recMode = getConfigRecMode cfg
            flagMasks = getConfigFlag cfg
            read1 = readDirectoryChanges rootPath handle recMode flagMasks
            step False = return Nothing
            step True = Just <$> read1
         in S.concatMap S.fromList $ S.unfoldrM step True

-- XXX
-- Remove the encoding requirement of paths? It can be encoding agnostic
-- "\" separated bytes, the application can decide what encoding to use.
-- Instead of always using widechar (-W) APIs can we call the underlying APIs
-- based on the configured code page?
-- https://docs.microsoft.com/en-us/windows/uwp/design/globalizing/use-utf8-code-page

-- | Each watch root is read by a thread blocked in a ReadDirectoryChangesW
-- foreign call. With the non-threaded RTS the call blocks all the Haskell
-- threads until a change occurs under that root.
checkThreadedRTS :: IO ()
checkThreadedRTS =
    unless rtsSupportsBoundThreads
        $ ioError $ userError
        $ "Streamly.FileSystem.Event.Windows: watching requires the "
            ++ "threaded RTS, link the program with -threaded"

-- | Start monitoring a list of directory paths for file system events with the
-- supplied configuration modifier operation over the' defaultConfig'. The
-- paths could be directories or symbolic links to directories.
--
-- Raise an exception when the stream starts if the program uses the
-- non-threaded RTS.
--
-- @
-- watchWith
--  ('setAttrsModified' True . 'setLastWriteTimeModified' False)
--  [Path.fromString "dir"]
-- @
watchWith :: (Config -> Config) -> NonEmpty Path -> Stream IO Event
watchWith f paths =
    S.before checkThreadedRTS
        $ S.parConcatMap (eager True) (eventStreamAggr (f defaultConfig))
        $ S.fromList (NonEmpty.toList paths)

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
-- When the watch root is a symlink, the absolute path returned is via the
-- original symlink and not through the resolved path.
getAbsPath :: Event -> Path
getAbsPath ev =
    case getRelPath ev of
        -- The path is relative to the root as reported by the system.
        -- Path.join raises an exception if the path starts with a letter
        -- and a colon, e.g. a stream "a:s" of a file named "a".
        Just relPath -> Path.unsafeJoin (getRoot ev) relPath
        Nothing -> getRoot ev

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

-- | If the kernel event buffer overflows, entire contents of the buffer are
-- discarded, therefore, events are lost.  The user application must scan
-- everything under the watched paths to know the current state of the file
-- system tree.
--
-- The overflow occurs when the application does not consume the events fast
-- enough.
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
