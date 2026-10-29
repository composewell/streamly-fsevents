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
--
-- =Overview
--
-- Use 'watchRecursive' or 'watch' with a list of file system dir paths you
-- want to watch as argument. It returns a stream of 'Event' representing the
-- file system events occurring under the watched paths.
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
-- For Windows reference documentation see:
--
-- * <https://docs.microsoft.com/en-us/windows/win32/api/winnt/ns-winnt-file_notify_information file notify information>
-- * <https://docs.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-readdirectorychangesw read directory changes>
--
-- We try to keep the macOS\/Linux/Windows event handling APIs and defaults
-- semantically and syntactically as close as possible.
--
-- =Availability
--
-- As per the Windows reference docs, the fs event notification API is
-- available in:
--
-- * Minimum supported client: Windows XP [desktop apps | UWP apps]
-- * Minimum supported server: Windows Server 2003 [desktop apps | UWP apps

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

import Data.Bits ((.|.), (.&.), complement)
import Data.List.NonEmpty (NonEmpty)
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

-- | ReadDirectoryChangesW fails with this error code when the system was
-- unable to record all the changes to the directory.
eRROR_NOTIFY_ENUM_DIR :: ErrCode
eRROR_NOTIFY_ENUM_DIR = 1022

-- | Returns 'False' if the call failed with 'eRROR_NOTIFY_ENUM_DIR', in that
-- case the number of bytes returned is not set. Throws an exception on any
-- other failure.
readDirectoryChangesW ::
       HANDLE
    -> Ptr FILE_NOTIFY_INFORMATION
    -> DWORD
    -> BOOL
    -> FileNotificationFlag
    -> LPDWORD
    -> IO Bool
readDirectoryChangesW h buf bufSize wst f br = do
    res <- c_ReadDirectoryChangesW
                h (castPtr buf) bufSize wst f br nullPtr nullFunPtr
    if res
    then return True
    else do
        err <- getLastError
        if err == eRROR_NOTIFY_ENUM_DIR
        then return False
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

-- | The event reported when the changes were lost. 'totalBytes' 0 identifies
-- it. 'eventRelPath' is a placeholder, 'getRelPath' returns 'Nothing'.
eventsLostEvent :: Path -> Event
eventsLostEvent root =
    Event
        { eventFlags = 0
        , eventRelPath = root
        , eventRootPath = root
        , totalBytes = 0
        }

-- | The changes were lost if the call succeeds with zero bytes returned or
-- fails with 'eRROR_NOTIFY_ENUM_DIR'. The buffer has no records in either
-- case.
readDirectoryChanges ::
    Path -> HANDLE -> Bool -> FileNotificationFlag -> IO [Event]
readDirectoryChanges root h wst mask = do
    let maxBuf = 63 * 1024
    allocaBytes maxBuf $ \buffer -> do
        alloca $ \bret -> do
            ok <- readDirectoryChangesW h buffer (toEnum maxBuf) wst mask bret
            bytesRet <- if ok then peekByteOff bret 0 else return 0
            if bytesRet == 0
            then return [eventsLostEvent root]
            else readChangeEvents buffer root bytesRet

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

-- | The events of one watch root. The directory handle is opened when the
-- stream starts and closed when it ends, on an exception, or on garbage
-- collection. It is never closed while a read on it is in progress: on an
-- exception, the worker thread reading the handle closes it after the read
-- returns; the stream is garbage collected only after the worker thread has
-- stopped running it.
eventStreamAggr :: Config -> Path -> Stream IO Event
eventStreamAggr cfg rootPath =
    S.bracketIO (openDirHandle rootPath) closeHandle $ \handle ->
        let recMode = getConfigRecMode cfg
            flagMasks = getConfigFlag cfg
            repeatM = S.sequence . S.repeat
         in S.concatMap S.fromList $ repeatM
                $ readDirectoryChanges rootPath handle recMode flagMasks

-- XXX
-- Remove the encoding requirement of paths? It can be encoding agnostic
-- "\" separated bytes, the application can decide what encoding to use.
-- Instead of always using widechar (-W) APIs can we call the underlying APIs
-- based on the configured code page?
-- https://docs.microsoft.com/en-us/windows/uwp/design/globalizing/use-utf8-code-page
--
-- | Start monitoring a list of directory paths for file system events with the
-- supplied configuration modifier operation over the' defaultConfig'. The
-- paths could be directories or symbolic links to directories.
--
-- When recursive mode is True, the whole directory tree under the path is
-- watched recursively.  When recursive mode is False, only the files and
-- directories directly under the watched directory are monitored, contents of
-- subdirectories are not monitored.  Monitoring starts from the current time
-- onwards.
--
-- /Symbolic Links:/ If the pathname to be watched is a symbolic link then
-- watch the target of the symbolic link instead of the symbolic link itself.
-- Note that the path location in the events is through the original symbolic
-- link path rather than the resolved path.
--
-- /Threaded RTS:/ Each watch root is read by a thread blocked in a
-- ReadDirectoryChangesW foreign call. Use the threaded RTS. With the
-- non-threaded RTS the call blocks all the Haskell threads until a change
-- occurs under that root, and a stopped watch is cleaned up only after the
-- next change.
--
-- @
-- watchWith
--  ('setAttrsModified' True . 'setLastWriteTimeModified' False)
--  [Path.fromString "dir"]
-- @
watchWith :: (Config -> Config) -> NonEmpty Path -> Stream IO Event
watchWith f paths =
    S.parConcatMap (eager True) (eventStreamAggr (f defaultConfig))
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
-- 'isEventsLost', for which it is 'Nothing'.
getRelPath :: Event -> Maybe Path
getRelPath Event{..} =
    -- totalBytes == 0 is the isEventsLost event
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
        -- Path.join raises an exception if it starts with a letter and a
        -- colon.
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
-- The changes that occur between two ReadDirectoryChangesW calls are held in
-- a buffer that the system allocates on the first call. The overflow occurs
-- when the application does not consume the events fast enough. This module
-- calls ReadDirectoryChangesW with a 63 KiB buffer.
--
-- 'getRoot' and 'getAbsPath' return the watch root whose events were lost,
-- 'getRelPath' returns 'Nothing'.
isEventsLost :: Event -> Bool
isEventsLost Event{..} = totalBytes == 0

-------------------------------------------------------------------------------
-- Debugging
-------------------------------------------------------------------------------

-- | Convert an 'Event' record to a String representation.
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
