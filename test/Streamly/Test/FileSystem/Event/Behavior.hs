-- |
-- Module      : Streamly.Test.FileSystem.Event.Behavior
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Asserts the behaviors that differ across platforms. Each behavior has the
-- expected outcome for each platform. The results are printed at the end,
-- including when an assertion fails. The tables in
-- docs/platform-differences.md record the results printed on each platform in
-- the CI logs.
--
-- A racy outcome, one that depends on a race or on the OS version, is not
-- asserted, it is recorded from the CI logs.
--
-- On macOS FSEvents can report the flags of earlier changes to a path with
-- a later event of the path. The expected outcome on macOS is Yes only for
-- an event that the operation always reports. An event reported only in
-- some runs is expected No, and the behavior is racy on macOS.
--
-- The expected outcomes were observed on Linux 6.12 and Ubuntu 24.04,
-- macOS 26 (arm64) and Windows Server 2025. The suite is skipped on Windows
-- older than Windows 10 and Windows Server 2016. On FreeBSD the expected
-- outcomes are those of Linux, except for the documented differences of the
-- FreeBSD inotify API.

module Streamly.Test.FileSystem.Event.Behavior (main) where

import Control.Concurrent
    ( MVar, forkIO, isEmptyMVar, newEmptyMVar, putMVar, readMVar
    , takeMVar, threadDelay, tryPutMVar, tryReadMVar
    )
import Control.Exception
    ( IOException, SomeException, bracket, evaluate, finally, fromException
    , try)
import Control.Monad (forM_, unless, void, when)
import Data.Either (isLeft)
import Data.Function ((&))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (group, isPrefixOf, nub, sort)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (isJust)
import System.Exit (ExitCode(..), exitFailure)
import System.Directory
    ( canonicalizePath, createDirectory, createDirectoryIfMissing
    , createDirectoryLink
    , doesDirectoryExist, getPermissions, listDirectory, removeDirectory
    , removeDirectoryLink, removeFile, removePathForcibly, renameDirectory
    , renameFile, renamePath
#if !defined(CABAL_OS_WINDOWS)
    , emptyPermissions, setOwnerReadable, setOwnerSearchable
    , setOwnerWritable, setPermissions
#endif
    )
import System.FilePath ((</>), makeRelative, takeDirectory, takeFileName)
import System.IO
    ( Handle, IOMode(AppendMode, ReadWriteMode), hClose, hFlush, hPutStr
    , hSetFileSize, openFile)
import System.IO.Temp (withSystemTempDirectory)
import System.Process
    ( CreateProcess(..), ProcessHandle, StdStream(NoStream), createProcess
    , proc, readProcess, readProcessWithExitCode, terminateProcess
    , waitForProcess
    )
#if !defined(CABAL_OS_WINDOWS)
import System.Posix.Files
    (createNamedPipe, ownerReadMode, ownerWriteMode, unionFileModes)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified System.Posix.Files as Posix
    (createLink, createSymbolicLink, ownerModes, setFileMode
    , setFileTimesHiRes)
import Foreign.C.Error (throwErrnoPathIfMinus1_)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt(..), CLong)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeByteOff)
#endif
import System.Timeout (timeout)
#if defined(CABAL_OS_WINDOWS)
import Data.Char (isDigit)
import Data.Word (Word32)
import Data.Bits ((.|.))
import Foreign.C.String (CWString, peekCWString, withCWString)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peek, poke, pokeByteOff)
import System.Exit (exitSuccess)
import System.Process (readCreateProcess, shell)
import System.Win32.HardLink (createHardLink)
import System.Win32.Types (BOOL, DWORD, HANDLE, failWith, getLastError)
import qualified System.Win32.File as Win32
import qualified System.Win32.Time as Win32
#endif
import Streamly.Internal.FS.Event (Config, Event)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.FS.Event as Event
#if defined(CABAL_OS_LINUX)
import qualified Streamly.Internal.FS.Event.Linux as Inotify
#elif defined(CABAL_OS_FREEBSD)
import qualified Streamly.Internal.FS.Event.FreeBSD as Inotify
#elif defined(CABAL_OS_DARWIN)
import qualified Streamly.Internal.FS.Event.Darwin as Darwin
#else
import qualified Streamly.Internal.FS.Event.Windows as Windows
#endif
import qualified Streamly.Internal.FileSystem.Path as Path

import Test.Hspec
import Test.Hspec.Runner (hspecResult, summaryFailures)

moduleName :: String
moduleName = "FS.Event.Behavior"

-------------------------------------------------------------------------------
-- Behavior descriptions
-------------------------------------------------------------------------------

data Outcome =
      Yes
    | No
    | Exception -- ^ The file system operation raised an exception.
    | WatchException -- ^ The watch raised an exception.
    | Failed String -- ^ The test could not determine the outcome.
    | Skipped String -- ^ The test does not apply in this environment.
    | Steps [Outcome] -- ^ The outcome of each of the 'steps'.
    deriving (Eq, Show)

-- | The paths used by a behavior.
data Paths = Paths
    { watchRoot :: FilePath
    , renamedRoot :: FilePath
    -- | The files opened by 'holdFile', closed after the behavior.
    , heldFiles :: IORef [Handle]
    -- | The processes started by 'holdCwd', ended after the behavior.
    , heldProcesses :: IORef [ProcessHandle]
    }

data Behavior = Behavior
    { title :: String
    , config :: Config -> Config
    -- | Watched along with the watch root, after it.
    , extraRoots :: Paths -> [FilePath]
    -- | Run before the watch starts.
    , setup :: Paths -> IO ()
    -- | Run after the watch ends, whatever the outcome.
    , cleanup :: Paths -> IO ()
    -- | After 'setup', the reason if the behavior cannot be tested here.
    , skip :: Paths -> IO (Maybe String)
    -- | Run 'ops' while the consumer of the stream is not reading it.
    , paused :: Bool
    , ops :: Paths -> IO ()
    -- | Run after the consumer has processed the events of 'ops'.
    , laterOps :: Paths -> IO ()
    , check :: Paths -> [Event] -> Bool
    -- | Run after 'ops', one at a time, each followed by 'settle'. Each check
    -- receives the events read during its step only, the outcome is
    -- 'Steps'. 'check' is not used if there are steps.
    , steps :: [(Paths -> IO (), Paths -> [Event] -> Bool)]
    , expected :: Outcome
    -- | The outcome depends on a race or on the OS version. It is printed but
    -- does not fail the test when it differs from 'expected'.
    , racy :: Bool
    -- | Print the events of the behavior after the results. False for a
    -- behavior whose number of events varies between runs.
    , printEvents :: Bool
    }

-- | The arguments are the outcomes on Linux, macOS and Windows. FreeBSD
-- uses the inotify backend of Linux, its outcome is the one of Linux, see
-- 'onFreeBSD' for a difference.
onPlatform :: Outcome -> Outcome -> Outcome -> Outcome
platformName :: String
#if defined(CABAL_OS_LINUX)
onPlatform linux _ _ = linux
platformName = "Linux"
#elif defined(CABAL_OS_FREEBSD)
onPlatform linux _ _ = linux
platformName = "FreeBSD"
#elif defined(CABAL_OS_DARWIN)
onPlatform _ darwin _ = darwin
platformName = "macOS"
#else
onPlatform _ _ windows = windows
platformName = "Windows"
#endif

-- | @onFreeBSD freebsd other@ is the outcome @freebsd@ on FreeBSD and
-- @other@ elsewhere.
onFreeBSD :: Outcome -> Outcome -> Outcome
#if defined(CABAL_OS_FREEBSD)
onFreeBSD freebsd _ = freebsd
#else
onFreeBSD _ other = other
#endif

createFile :: FilePath -> IO ()
createFile path = writeFile path ""

absPath :: Event -> FilePath
absPath = Path.toString . Event.getAbsPath

isNamed :: String -> Event -> Bool
isNamed name ev = takeFileName (absPath ev) == name

createdNamed :: String -> [Event] -> Bool
createdNamed name = any (\ev -> isNamed name ev && Event.isCreated ev)

modifiedNamed :: String -> [Event] -> Bool
modifiedNamed name = any (\ev -> isNamed name ev && Event.isModified ev)

-- | Whether each of the names is reported as deleted.
deletedNamed :: [String] -> [Event] -> Bool
deletedNamed names evs =
    all (\name -> any (\ev -> isNamed name ev && Event.isDeleted ev) evs) names

-- | Whether a path whose name starts with "file" has two 'Event.isCreated'
-- events.
fileCreatedTwice :: [Event] -> Bool
fileCreatedTwice =
    any ((> 1) . length) . group . sort . map absPath . filter isFileCreate

    where

    isFileCreate ev =
        Event.isCreated ev && "file" `isPrefixOf` takeFileName (absPath ev)

-- | Open a file for appending. A file held open can be written after the
-- permissions of its directory are removed.
holdFile :: Paths -> FilePath -> IO ()
holdFile p path = do
    h <- openFile path AppendMode
    modifyIORef' (heldFiles p) (h :)

-- | Start a process with the directory as its working directory. The
-- process keeps the directory in use until it ends.
holdCwd :: Paths -> FilePath -> IO ()
holdCwd p dir = do
    (_, _, _, ph) <-
        createProcess (proc cmd args)
            { cwd = Just dir, std_in = NoStream, std_out = NoStream }
    modifyIORef' (heldProcesses p) (ph :)

    where

#if defined(CABAL_OS_WINDOWS)
    (cmd, args) = ("ping", ["-n", "120", "127.0.0.1"])
#else
    (cmd, args) = ("sleep", ["120"])
#endif

-- | Append to the files opened by 'holdFile' and keep them open.
writeHeldFilesOpen :: Paths -> IO ()
writeHeldFilesOpen p =
    readIORef (heldFiles p) >>= mapM_ (\h -> hPutStr h "x" >> hFlush h)

-- | Append to the files opened by 'holdFile' and close them. On macOS and
-- Windows a write to a file that stays open may not be reported.
writeHeldFiles :: Paths -> IO ()
writeHeldFiles p = do
    writeHeldFilesOpen p
    readIORef (heldFiles p) >>= mapM_ hClose

-- | Set the size of a file to zero with @ftruncate@ on POSIX, @SetEndOfFile@
-- on Windows, without a write.
truncateFile :: FilePath -> IO ()
truncateFile path =
    bracket (openFile path ReadWriteMode) hClose (`hSetFileSize` 0)

-- | Close the files opened by 'holdFile' and end the processes started by
-- 'holdCwd'. The lists are emptied, a second call does nothing.
releaseHeld :: Paths -> IO ()
releaseHeld p = do
    readIORef (heldFiles p) >>= mapM_ hClose
    writeIORef (heldFiles p) []
    readIORef (heldProcesses p) >>= mapM_ endProcess
    writeIORef (heldProcesses p) []

    where

    endProcess ph = terminateProcess ph >> void (waitForProcess ph)

-- | Remove the permission to list a directory, keep the permission to create
-- a file in it.
denyRead :: FilePath -> IO ()
-- | Undo 'denyRead'.
allowRead :: FilePath -> IO ()
-- | Remove all the permissions of a directory, the directory cannot be
-- reached.
denyAll :: FilePath -> IO ()
-- | Undo 'denyAll'.
allowAll :: FilePath -> IO ()
-- | Remove the permission to reach the entries of a directory, keep the
-- permission to list it. Undone by 'allowAll'. On Windows the traverse access
-- is checked only without the "Bypass traverse checking" privilege, see
-- 'bypassTraverse'.
denySearch :: FilePath -> IO ()
-- | On Windows, enable or disable the "Bypass traverse checking" privilege of
-- the process. Does nothing on POSIX.
bypassTraverse :: Bool -> IO ()
-- | @hardLink source link@
hardLink :: FilePath -> FilePath -> IO ()
#if defined(CABAL_OS_WINDOWS)
-- Deny the list access of the directory only, as on POSIX the traverse
-- access is kept and the entries in the directory are not changed.
denyRead path = void $ readProcess "icacls" [path, "/deny", everyone] ""

    where

    everyone = "*S-1-1-0:(RD)"

allowRead path =
    void $ readProcess "icacls" [path, "/remove:d", "*S-1-1-0"] ""

denyAll path =
    void $ readProcess "icacls" [path, "/deny", "*S-1-1-0:(OI)(CI)(F)"] ""

allowAll = allowRead

denySearch path =
    void $ readProcess "icacls" [path, "/deny", "*S-1-1-0:(X)"] ""

hardLink = createHardLink

foreign import ccall unsafe "windows.h GetCurrentProcess"
    c_GetCurrentProcess :: IO HANDLE

foreign import ccall unsafe "windows.h OpenProcessToken"
    c_OpenProcessToken :: HANDLE -> DWORD -> Ptr HANDLE -> IO BOOL

foreign import ccall unsafe "windows.h LookupPrivilegeValueW"
    c_LookupPrivilegeValueW :: CWString -> CWString -> Ptr () -> IO BOOL

foreign import ccall unsafe "windows.h AdjustTokenPrivileges"
    c_AdjustTokenPrivileges ::
        HANDLE -> BOOL -> Ptr () -> DWORD -> Ptr () -> Ptr DWORD -> IO BOOL

-- | Enable or disable a privilege of the process, for example
-- @SeChangeNotifyPrivilege@ ("Bypass traverse checking"). Disabling a
-- privilege that the process does not have succeeds.
--
-- TOKEN_PRIVILEGES with one privilege: the count at offset 0, the LUID at
-- offset 4 and the attributes at offset 12, 16 bytes.
setPrivilege :: String -> Bool -> IO ()
setPrivilege privilege enable =
    alloca $ \tokPtr -> do
        process <- c_GetCurrentProcess
        -- TOKEN_ADJUST_PRIVILEGES .|. TOKEN_QUERY
        ok <- c_OpenProcessToken process 0x28 tokPtr
        unless ok $ getLastError >>= failWith "OpenProcessToken"
        tok <- peek tokPtr
        flip finally (Win32.closeHandle tok) $ allocaBytes 16 $ \tp -> do
            found <-
                withCWString privilege $ \name ->
                    c_LookupPrivilegeValueW nullPtr name (tp `plusPtr` 4)
            unless found $ getLastError >>= failWith "LookupPrivilegeValueW"
            poke (castPtr tp) (1 :: Word32)
            -- SE_PRIVILEGE_ENABLED
            pokeByteOff tp 12 (if enable then 2 else 0 :: Word32)
            adjusted <- c_AdjustTokenPrivileges tok False tp 0 nullPtr nullPtr
            -- The call succeeds with ERROR_NOT_ALL_ASSIGNED (1300) if the
            -- process does not have the privilege.
            err <- getLastError
            unless (adjusted && (err == 0 || not enable && err == 1300))
                $ failWith ("AdjustTokenPrivileges " ++ privilege) err

-- | Enable or disable the "Bypass traverse checking" privilege,
-- SeChangeNotifyPrivilege, of the process. A process has it enabled by
-- default.
setBypassTraverse :: Bool -> IO ()
setBypassTraverse = setPrivilege "SeChangeNotifyPrivilege"

bypassTraverse = setBypassTraverse

-- | Disable the backup and restore privileges of the process. An
-- administrator, such as the CI runner, has them enabled. A directory is
-- always opened with FILE_FLAG_BACKUP_SEMANTICS, with these privileges the
-- open is granted regardless of the ACL of the directory.
disableBackupPrivileges :: IO ()
disableBackupPrivileges = do
    setPrivilege "SeBackupPrivilege" False
    setPrivilege "SeRestorePrivilege" False
#else
denyRead path =
    setPermissions path
        $ setOwnerWritable True $ setOwnerSearchable True emptyPermissions

allowRead path = do
    perms <- getPermissions path
    setPermissions path (setOwnerReadable True perms)

denyAll path = setPermissions path emptyPermissions

allowAll path =
    setPermissions path
        $ setOwnerReadable True
        $ setOwnerWritable True
        $ setOwnerSearchable True emptyPermissions

denySearch path =
    setPermissions path
        $ setOwnerReadable True $ setOwnerWritable True emptyPermissions

bypassTraverse _ = return ()

hardLink = Posix.createLink
#endif

-- | @copyFile source copy@. On macOS the copy is a clone of the source.
--
-- The copy is created at its path. 'System.Directory.copyFile' is not used,
-- it writes a temporary file and renames it to the destination, which is
-- reported as a move.
copyFile :: FilePath -> FilePath -> IO ()
#if defined(CABAL_OS_DARWIN)
copyFile source copy = void $ readProcess "/bin/cp" ["-c", source, copy] ""
#elif defined(CABAL_OS_WINDOWS)
copyFile source copy = Win32.copyFile source copy True
#else
copyFile source copy = void $ readProcess "cp" [source, copy] ""
#endif

-- | Make a file read-only with one system call: @chmod@ on POSIX,
-- @SetFileAttributes@ on Windows.
makeReadOnly :: FilePath -> IO ()
#if defined(CABAL_OS_WINDOWS)
makeReadOnly path = Win32.setFileAttributes path Win32.fILE_ATTRIBUTE_READONLY
#else
makeReadOnly path = Posix.setFileMode path ownerReadMode
#endif

#if defined(CABAL_OS_WINDOWS)
-- | @setFileTimes creation access write path@ sets the selected times of a
-- file or a directory to the current time with one @SetFileTime@ call.
setFileTimes :: Bool -> Bool -> Bool -> FilePath -> IO ()
setFileTimes creation access write path =
    bracket
        (Win32.createFile
            path
            Win32.fILE_WRITE_ATTRIBUTES
            (Win32.fILE_SHARE_READ .|. Win32.fILE_SHARE_WRITE
                .|. Win32.fILE_SHARE_DELETE)
            Nothing
            Win32.oPEN_EXISTING
            (Win32.fILE_ATTRIBUTE_NORMAL .|. Win32.fILE_FLAG_BACKUP_SEMANTICS)
            Nothing)
        Win32.closeHandle
        $ \h -> do
            t <- Win32.getSystemTimeAsFileTime
            let at b = if b then Just t else Nothing
            Win32.setFileTime h (at creation) (at access) (at write)
#endif

-- | Set the access and the modification times of a file to the current
-- time with one system call: @utimensat@ on POSIX, @SetFileTime@ on
-- Windows. The path can be a file or a directory.
setTimes :: FilePath -> IO ()
#if defined(CABAL_OS_WINDOWS)
setTimes = setFileTimes False True True
#else
setTimes path = do
    t <- getPOSIXTime
    Posix.setFileTimesHiRes path t t
#endif

#if !defined(CABAL_OS_WINDOWS)
foreign import ccall unsafe "sys/stat.h utimensat"
    c_utimensat :: CInt -> CString -> Ptr () -> CInt -> IO CInt

-- | Set the modification time of a file to the current time and leave its
-- access time unchanged, with one @utimensat@ call. The two @timespec@
-- structures assume a 64-bit @time_t@ and @long@.
setModTime :: FilePath -> IO ()
setModTime path =
    allocaBytes 32 $ \ts -> do
        pokeByteOff ts 0 (0 :: CLong)
        pokeByteOff ts 8 utimeOmit
        pokeByteOff ts 16 (0 :: CLong)
        pokeByteOff ts 24 utimeNow
        withCString path $ \cpath ->
            throwErrnoPathIfMinus1_ "utimensat" path
                (c_utimensat atFdcwd cpath ts 0)

    where

#if defined(CABAL_OS_LINUX)
    atFdcwd = -100
    utimeNow = 2 ^ (30 :: Int) - 1 :: CLong
    utimeOmit = 2 ^ (30 :: Int) - 2 :: CLong
#elif defined(CABAL_OS_FREEBSD)
    atFdcwd = -100
    utimeNow = -1 :: CLong
    utimeOmit = -2 :: CLong
#else
    atFdcwd = -2
    utimeNow = -1 :: CLong
    utimeOmit = -2 :: CLong
#endif
#endif

-- | The setting for the metadata change events, if the platform has one.
setAttrs :: Bool -> Config -> Config
#if defined(CABAL_OS_LINUX) || defined(CABAL_OS_FREEBSD)
setAttrs = Inotify.setAttrsModified
#elif defined(CABAL_OS_DARWIN)
setAttrs _ = id
#else
setAttrs = Windows.setAttrsModified
#endif

-- | Whether the event is 'isAttrsModified'. Windows does not have this
-- event.
attrsModified :: Event -> Bool
#if defined(CABAL_OS_LINUX) || defined(CABAL_OS_FREEBSD)
attrsModified = Inotify.isAttrsModified
#elif defined(CABAL_OS_DARWIN)
attrsModified = Darwin.isAttrsModified
#else
attrsModified = const False
#endif

-- | Whether the event reports a change of the metadata of a directory:
-- 'isAttrsModified' on Linux and macOS, 'isModified' on Windows.
dirMetadataModified :: Event -> Bool
#if defined(CABAL_OS_WINDOWS)
dirMetadataModified = Event.isModified
#else
dirMetadataModified = attrsModified
#endif

-- | The setting for the directory metadata change events: the metadata
-- change events on Linux, all events on Windows.
dirMetadataConfig :: Config -> Config
#if defined(CABAL_OS_WINDOWS)
dirMetadataConfig = Windows.setAllEvents True
#else
dirMetadataConfig = setAttrs True
#endif

#if defined(CABAL_OS_WINDOWS)
-- | Set the hidden attribute of a file or a directory.
makeHidden :: FilePath -> IO ()
makeHidden path = Win32.setFileAttributes path Win32.fILE_ATTRIBUTE_HIDDEN
#endif

-- | Change the permissions of a directory to read, write and search for the
-- owner only. On Windows set the hidden attribute of the directory.
changeDirMode :: FilePath -> IO ()
#if defined(CABAL_OS_WINDOWS)
changeDirMode = makeHidden
#else
changeDirMode path = Posix.setFileMode path Posix.ownerModes
#endif

-- | Whether the event is 'isSecurityModified'. Only macOS has this event.
securityModified :: Event -> Bool
#if defined(CABAL_OS_DARWIN)
securityModified = Darwin.isSecurityModified
#else
securityModified = const False
#endif

-- | Whether the event reports the source, or the destination, of a move.
-- macOS has one predicate for both.
movedFrom, movedTo :: Event -> Bool
#if defined(CABAL_OS_LINUX) || defined(CABAL_OS_FREEBSD)
movedFrom = Inotify.isMovedFrom
movedTo = Inotify.isMovedTo
#elif defined(CABAL_OS_DARWIN)
movedFrom = Darwin.isMoved
movedTo = Darwin.isMoved
#else
movedFrom = Windows.isMovedFrom
movedTo = Windows.isMovedTo
#endif

-- | Run a command. Raise an exception, with the output of the command, if
-- the command fails.
runCommand :: FilePath -> [String] -> IO ()
runCommand cmd args = do
    (code, out, err) <- readProcessWithExitCode cmd args ""
    case code of
        ExitSuccess -> return ()
        ExitFailure n ->
            ioError $ userError $
                unwords (cmd : args) ++ ": exit code " ++ show n ++ "\n"
                    ++ out ++ err

ignoreIOError :: IO () -> IO ()
ignoreIOError action = void (try action :: IO (Either IOException ()))

-- | @mountVolume dir root@ mounts a new file system at the directory @root@.
-- The files needed for the mount are created in @dir@. Raise an exception if
-- the file system cannot be mounted, for example, without the privilege. On
-- Windows also raise an exception if the volume cannot be locked, before the
-- watch opens a handle on it.
mountVolume :: FilePath -> FilePath -> IO ()
-- | Unmount the file system mounted by 'mountVolume'. On Windows lock the
-- volume and then dismount it, the lock fails if a handle on the volume is
-- open.
unmountVolume :: FilePath -> FilePath -> IO ()
-- | On Windows dismount the volume without locking it, the open handles on
-- the volume become invalid. Elsewhere the same as 'unmountVolume'.
forceUnmountVolume :: FilePath -> FilePath -> IO ()
-- | Undo 'mountVolume', after 'unmountVolume' or if 'unmountVolume' failed.
-- Ignore the errors.
removeVolume :: FilePath -> FilePath -> IO ()
-- | The unmount event of the platform. Windows does not have one.
unmountReported :: Event -> Bool
#if defined(CABAL_OS_WINDOWS)
foreign import ccall unsafe "windows.h GetVolumeNameForVolumeMountPointW"
    c_GetVolumeNameForVolumeMountPointW ::
        CWString -> CWString -> DWORD -> IO BOOL

foreign import ccall unsafe "windows.h DeviceIoControl"
    c_DeviceIoControl ::
           HANDLE -> DWORD -> Ptr () -> DWORD -> Ptr () -> DWORD -> Ptr DWORD
        -> Ptr () -> IO BOOL

-- | The volume name, @\\\\?\\Volume{GUID}\\@, of the volume mounted at the
-- directory. The buffer holds the longest volume name, 49 characters and
-- the terminating null.
volumeName :: FilePath -> IO String
volumeName root =
    withCWString (root ++ "\\") $ \mountPoint ->
        allocaBytes (50 * 2) $ \buf -> do
            ok <- c_GetVolumeNameForVolumeMountPointW mountPoint buf 50
            unless ok
                $ getLastError >>= failWith "GetVolumeNameForVolumeMountPointW"
            peekCWString buf

-- | Open the volume mounted at the directory. The volume is opened by its
-- name without the trailing backslash, with the trailing backslash the
-- root directory of the volume is opened.
withVolume :: FilePath -> (HANDLE -> IO a) -> IO a
withVolume root action = do
    name <- volumeName root
    bracket
        (Win32.createFile
            (reverse (dropWhile (== '\\') (reverse name)))
            (Win32.gENERIC_READ .|. Win32.gENERIC_WRITE)
            (Win32.fILE_SHARE_READ .|. Win32.fILE_SHARE_WRITE)
            Nothing
            Win32.oPEN_EXISTING
            Win32.fILE_ATTRIBUTE_NORMAL
            Nothing)
        Win32.closeHandle
        action

-- | A DeviceIoControl request without input or output.
volumeControl :: String -> DWORD -> HANDLE -> IO ()
volumeControl name code h =
    alloca $ \bytes -> do
        ok <- c_DeviceIoControl h code nullPtr 0 nullPtr 0 bytes nullPtr
        unless ok $ getLastError >>= failWith name

-- | Fails if a handle on the volume is open, other than the handle of the
-- request. Closing the handle of the request unlocks the volume.
lockVolume :: HANDLE -> IO ()
lockVolume = volumeControl "FSCTL_LOCK_VOLUME" 0x00090018

unlockVolume :: HANDLE -> IO ()
unlockVolume = volumeControl "FSCTL_UNLOCK_VOLUME" 0x0009001C

dismountVolume :: HANDLE -> IO ()
dismountVolume = volumeControl "FSCTL_DISMOUNT_VOLUME" 0x00090020

-- diskpart and the volume functions need the long path, without the 8.3
-- names of the temporary directory.
mountVolume dir root = do
    image <- volumeImage dir
    root1 <- canonicalizePath root
    diskpart dir
        [ "create vdisk file=\"" ++ image ++ "\" maximum=16 type=fixed"
        , "attach vdisk"
        , "convert mbr"
        , "create partition primary"
        , "format fs=ntfs quick"
        , "assign mount=\"" ++ root1 ++ "\""
        ]
    -- Another process, for example an indexer or an antivirus, may keep a
    -- handle on the new volume open.
    withVolume root1 $ \h -> lockVolume h >> unlockVolume h

unmountVolume _ root = do
    root1 <- canonicalizePath root
    withVolume root1 $ \h -> lockVolume h >> dismountVolume h

forceUnmountVolume _ root = do
    root1 <- canonicalizePath root
    withVolume root1 dismountVolume

removeVolume dir root = do
    image <- volumeImage dir
    root1 <- canonicalizePath root
    ignoreIOError $ runCommand "mountvol" [root1 ++ "\\", "/d"]
    ignoreIOError
        $ diskpart dir
            ["select vdisk file=\"" ++ image ++ "\"", "detach vdisk"]

unmountReported _ = False

volumeImage :: FilePath -> IO FilePath
volumeImage dir = (</> "volume.vhd") <$> canonicalizePath dir

diskpart :: FilePath -> [String] -> IO ()
diskpart dir commands = do
    let script = dir </> "diskpart.txt"
    writeFile script (unlines commands)
    runCommand "diskpart" ["/s", script]
#elif defined(CABAL_OS_DARWIN)
mountVolume dir root = do
    let image = dir </> "volume.dmg"
    runCommand "hdiutil"
        [ "create", "-size", "16m", "-fs", "HFS+", "-volname", "fsevents"
        , image
        ]
    runCommand "hdiutil"
        ["attach", "-nobrowse", "-noautoopen", "-mountpoint", root, image]

unmountVolume _ root = runCommand "hdiutil" ["detach", root]

forceUnmountVolume = unmountVolume

removeVolume _ root =
    ignoreIOError $ runCommand "hdiutil" ["detach", "-force", root]

unmountReported = Darwin.isUnmounted
#else
-- A tmpfs, mounted as root or with sudo. In a user namespace the user is
-- root.
mountVolume _ root = asRoot "mount" ["-t", "tmpfs", "tmpfs", root]

unmountVolume _ root = asRoot "umount" [root]

forceUnmountVolume = unmountVolume

removeVolume dir root = ignoreIOError (unmountVolume dir root)

unmountReported = Inotify.isRootUnmounted

asRoot :: FilePath -> [String] -> IO ()
asRoot cmd args = do
    uid <- readProcess "id" ["-u"] ""
    if words uid == ["0"]
    then runCommand cmd args
    else runCommand "sudo" ("-n" : cmd : args)
#endif

base :: Behavior
base =
    Behavior
        { title = ""
        , config = id
        , extraRoots = const []
        , setup = const (return ())
        , cleanup = const (return ())
        , skip = const (return Nothing)
        , paused = False
        , ops = const (return ())
        , laterOps = const (return ())
        , check = \_ _ -> False
        , steps = []
        , expected = Exception
        , racy = False
        , printEvents = True
        }

behaviors :: [Behavior]
behaviors =
    -- On Linux and Windows a watch root that is deleted and created again is
    -- not watched, the watch is on the object, not on the path. This test
    -- checks that a file inside a watched directory does not have this
    -- problem: the watch of the directory reports the events of the file
    -- after it is deleted and created again.
    [ base
        { title = "file deleted and created again in a watched dir is reported"
        , ops = \p -> do
            let file = watchRoot p </> "file"
            createFile file
            settle
            removeFile file
            writeFile file "new"
        , check = \_ evs ->
            let named = filter (isNamed "file") evs
             in any Event.isDeleted named && any Event.isModified named
        , expected = onPlatform Yes Yes Yes
        }
    -- On macOS FSEvents can report the flags of earlier changes to a path
    -- with a later event of the path. Whether the delete, one second after
    -- the create, reports isCreated varies between runs.
    , base
        { title = "file deleted after its create is read reports isCreated"
        , ops = \p -> do
            let file = watchRoot p </> "file"
            createFile file
            settle
            removeFile file
        , check = const fileCreatedTwice
        , expected = onPlatform No Yes No
        , racy = onDarwin
        }
    , base
        { title = "A deleted directory root reports `isDeleted` for the root"
        , ops = removePathForcibly . watchRoot
        , check = \p ->
            any (\ev -> absPath ev == watchRoot p && Event.isDeleted ev)
        , expected = Yes
        }
    -- On macOS the watch is on the path, a deleted root stays watched.
    , base
        { title =
            "A deleted watch root reports `isRootUnwatched` for the root"
        , ops = removePathForcibly . watchRoot
        , check = \p ->
            any (\ev -> absPath ev == watchRoot p && Event.isRootUnwatched ev)
        , expected = onPlatform Yes No Yes
        }
    -- On Windows the rename of a root is not reported, the watch continues.
    , base
        { title =
            "A renamed watch root reports `isRootUnwatched` for the root"
        , ops = renameRoot
        , check = \p ->
            any (\ev -> absPath ev == watchRoot p && Event.isRootUnwatched ev)
        , expected = onPlatform Yes No No
        }
    , base
        { title = "root deleted and created again is watched"
        , ops = \p -> do
            removePathForcibly (watchRoot p)
            settle
            createDirectory (watchRoot p)
            settle
            createFile (watchRoot p </> "file")
        , check = const (createdNamed "file")
        , expected = onPlatform No Yes No
        }
    , base
        { title = "renamed root is watched"
        , ops = renameRootAndCreate
        , check = const (createdNamed "file")
        , expected = onPlatform No No Yes
        }
    -- On Windows the rename of a root is not reported, the watch continues.
    , base
        { title = "renamed root reports isMoved for the root"
        , ops = \p -> renameDirectory (watchRoot p) (renamedRoot p)
        , check = \p ->
            let isRoot ev = absPath ev `elem` [watchRoot p, renamedRoot p]
             in any (\ev -> isRoot ev && Event.isMoved ev)
        , expected = onPlatform Yes Yes No
        }
    -- On Linux the rename event of a directory root removes the root.
    , base
        { title = "root renamed back before the event is read is watched"
        , paused = True
        , ops = \p -> do
            renameDirectory (watchRoot p) (renamedRoot p)
            renameDirectory (renamedRoot p) (watchRoot p)
        , laterOps = \p -> createFile (watchRoot p </> "file")
        , check = const (createdNamed "file")
        , expected = onPlatform No Yes Yes
        }
    , base
        { title = "root renamed back after the event is read is watched"
        , ops = \p -> do
            renameDirectory (watchRoot p) (renamedRoot p)
            settle
            renameDirectory (renamedRoot p) (watchRoot p)
        , laterOps = \p -> createFile (watchRoot p </> "file")
        , check = const (createdNamed "file")
        , expected = onPlatform No Yes Yes
        }
    -- The root is parent/root, the rename of parent generates no event for
    -- the root on Linux. macOS watches the path, the root is not watched
    -- after the rename. Windows does not rename a directory that has a
    -- watched root under it.
    , base
        { title =
            "A root whose parent is renamed remains watched under the old path"
        , extraRoots = \p -> [nestedRoot p]
        , setup = createDirectoryIfMissing True . nestedRoot
        , ops = \p -> do
            renameNestedParent p
            createFile (movedNestedRoot p </> "file")
        , check = \p ->
            any (\ev -> absPath ev == nestedRoot p </> "file"
                    && Event.isCreated ev)
        , expected = onPlatform Yes No Exception
        }
    -- On Linux the directory created after the rename cannot be reached by
    -- its path under the root, the root is unwatched.
    , base
        { title =
            "A recursive root whose parent is renamed is unwatched on a "
                ++ "directory create"
        , config = Event.setRecursiveMode True
        , extraRoots = \p -> [nestedRoot p]
        , setup = createDirectoryIfMissing True . nestedRoot
        , ops = \p -> do
            renameNestedParent p
            createDirectory (movedNestedRoot p </> "dir")
        , check = \p ->
            any (\ev -> absPath ev == nestedRoot p && Event.isRootUnwatched ev)
        , expected = onPlatform Yes No Exception
        }
    -- A new file system is mounted at the watch root. On Windows the
    -- watch keeps a handle on the volume open, the lock of the volume
    -- fails. The outcomes other than on Linux have not been observed, they
    -- are not asserted.
    , unmount
        { title =
            "A file system can be unmounted while a tree in it is watched"
        , ops = \p -> unmountVolume (volumeDir p) (watchRoot p)
        , check = \_ _ -> True
        , expected = onPlatform Yes Yes Exception
        }
    -- On Windows the volume is dismounted forcibly.
    , unmount
        { title =
            "Unmount of a watched file system reports `isRootUnwatched` for "
                ++ "the root"
        , check = \p ->
            any (\ev -> absPath ev == watchRoot p && Event.isRootUnwatched ev)
        , expected = onPlatform Yes No WatchException
        }
    , unmount
        { title = "Unmount of a watched file system reports an unmount event"
        , check = const (any unmountReported)
        , expected = onPlatform Yes Yes WatchException
        }
    , newDir
        { title = "file created in a new dir before it is read is reported"
        , check = const (createdNamed "file")
        , expected = onPlatform Yes Yes Yes
        }
    -- On Linux, the scan reports the file, not the changes made to it before
    -- the watch of the directory is added.
    , newDir
        { title = "file modified in a new dir before it is read is reported"
        , ops = \p -> do
            let file = watchRoot p </> "dir/file"
            createDirectory (watchRoot p </> "dir")
            createFile file
            appendFile file "x"
        , check = \_ ->
            any (\ev -> isNamed "file" ev && Event.isModified ev)
        , expected = onPlatform No Yes Yes
        }
    , newDir
        { title = "file deleted in a new dir before it is read is reported"
        , ops = \p -> do
            let file = watchRoot p </> "dir/file"
            createDirectory (watchRoot p </> "dir")
            createFile file
            removeFile file
        , check = \_ -> any (isNamed "file")
        , expected = onPlatform No Yes Yes
        }
    -- The consumer reads the events while the files are created. On Linux,
    -- some of the files are created after the watch of a directory is added
    -- and before the scan of the directory reads them.
    , base
        { title = "file created in a new dir can report isCreated twice"
        , config = Event.setRecursiveMode True
        , ops = \p -> forM_ [1 .. 20 :: Int] $ \i -> do
            let dir = watchRoot p </> ("dir" ++ show i)
            createDirectory dir
            forM_ [1 .. 200 :: Int] $ \j ->
                createFile (dir </> ("file" ++ show j))
        , check = const fileCreatedTwice
        , expected = onPlatform Yes Yes No
        , racy = True
        -- The files are created until the race is seen.
        , printEvents = False
        }
    , base
        { title = "file created in a moved in dir before its read is reported"
        , config = Event.setRecursiveMode True
        , paused = True
        , ops = \p -> do
            createDirectory (renamedRoot p)
            renameDirectory (renamedRoot p) (watchRoot p </> "dir")
            createFile (watchRoot p </> "dir/file")
        , check = const (createdNamed "file")
        , expected = onPlatform Yes Yes Yes
        }
    , base
        { title = "file already in a moved in dir is reported"
        , config = Event.setRecursiveMode True
        , paused = True
        , ops = \p -> do
            createDirectory (renamedRoot p)
            createFile (renamedRoot p </> "file")
            renameDirectory (renamedRoot p) (watchRoot p </> "dir")
        , check = const (createdNamed "file")
        , expected = onPlatform Yes No No
        }
    -- On Linux the scan of the directory at its new path reports the file.
    , base
        { title = "file in a dir renamed within the tree is reported"
        , config = Event.setRecursiveMode True
        , setup = withDirFile
        , ops = \p ->
            renameDirectory (watchRoot p </> "dir") (watchRoot p </> "dir2")
        , check = \_ -> any (isNamed "file")
        , expected = onPlatform Yes No No
        }
    , base
        { title = "file in a dir moved out of the tree is reported"
        , config = Event.setRecursiveMode True
        , setup = withDirFile
        , ops = \p -> renameDirectory (watchRoot p </> "dir") (renamedRoot p)
        , check = \_ -> any (isNamed "file")
        , expected = onPlatform No No No
        }
    , newDir
        { title = "subdir created in a new dir before it is read is watched"
        , laterOps = \p -> createFile (watchRoot p </> "dir/sub/later")
        , check = const (createdNamed "later")
        , expected = onPlatform Yes Yes Yes
        }
    , base
        { title = "root created after the start of the watch is watched"
        , setup = removeDirectory . watchRoot
        , ops = \p -> do
            createDirectory (watchRoot p)
            settle
            createFile (watchRoot p </> "file")
        , check = const (createdNamed "file")
        , expected = onPlatform WatchException Yes WatchException
        }
    -- A write to a file, without any permission change. The tests of
    -- unreachable directories write to a file opened before the permissions
    -- are removed, and close it after the write. On macOS the write to a file
    -- kept open was reported in one run and not in others.
    , base
        { title = "write is reported before the file is closed"
        , setup = \p -> createFile (watchRoot p </> "file")
        , ops = \p -> do
            holdFile p (watchRoot p </> "file")
            writeHeldFilesOpen p
        , check = const (modifiedNamed "file")
        , expected = onPlatform Yes No No
        , racy = onDarwin
        }
    , base
        { title = "write is reported after the file is closed"
        , setup = \p -> createFile (watchRoot p </> "file")
        , ops = \p -> do
            holdFile p (watchRoot p </> "file")
            writeHeldFiles p
        , check = const (modifiedNamed "file")
        , expected = onPlatform Yes Yes Yes
        }
    , base
        { title = "truncating a file without a write reports isModified"
        , setup = \p -> do
            writeFile (watchRoot p </> "file") "old"
            settle
        , ops = \p -> truncateFile (watchRoot p </> "file")
        , check = const (modifiedNamed "file")
        , expected = onPlatform Yes Yes Yes
        }
    -- A directory that cannot be reached: FSEvents does not report its events
    -- to a process that is not root. The file is written through a handle
    -- opened before the permissions are removed, and closed after the write.
    , base
        { title = "A root without READ and SEARCH reports its events"
        , setup = \p -> do
            createFile (watchRoot p </> "file")
            holdFile p (watchRoot p </> "file")
            denyAll (watchRoot p)
        , cleanup = allowAll . watchRoot
        , skip = stillReadable . watchRoot
        , ops = writeHeldFiles
        , check = const (modifiedNamed "file")
        , expected = onPlatform WatchException No WatchException
        }
    , base
        { title = "A root with only SEARCH reports its events"
        , setup = denyRead . watchRoot
        , cleanup = allowRead . watchRoot
        , skip = rootStillReadable
        , ops = \p -> createFile (watchRoot p </> "file")
        , check = const (createdNamed "file")
        , expected = onPlatform WatchException Yes WatchException
        }
    -- On Windows the tests that deny SEARCH disable the "Bypass traverse
    -- checking" privilege, with it the traverse access is not checked. On
    -- FreeBSD adding the watch of a directory looks up its entries, it fails
    -- with EACCES without SEARCH.
    , base
        { title = "A root with only READ reports its events"
        , setup = \p -> do
            createFile (watchRoot p </> "file")
            holdFile p (watchRoot p </> "file")
            denySearch (watchRoot p)
            bypassTraverse False
        , cleanup = \p ->
            allowAll (watchRoot p) `finally` bypassTraverse True
        , skip = stillSearchable . watchRoot
        , ops = writeHeldFiles
        , check = const (modifiedNamed "file")
        , expected = onFreeBSD WatchException (onPlatform Yes No Yes)
        }
    -- The required permission is READ on Linux and Windows, SEARCH on macOS.
    , base
        { title =
            "A root stops reporting events after the required permission is "
                ++ "removed"
        , setup = \p -> createFile (watchRoot p </> "file")
        , cleanup = allowAll . watchRoot
        , skip = requiredNotRemovable
        , ops = \p -> do
            holdFile p (watchRoot p </> "file")
            denyRequired (watchRoot p)
            settle
            writeHeldFiles p
        , check = \_ -> not . modifiedNamed "file"
        , expected = onPlatform No Yes No
        }
    , base
        { title =
            "A root starts reporting events after the required permission is "
                ++ "enabled"
        , setup = \p -> do
            createFile (watchRoot p </> "file")
            holdFile p (watchRoot p </> "file")
            denyRequired (watchRoot p)
        , cleanup = allowAll . watchRoot
        , skip = requiredStillGranted . watchRoot
        , ops = \p -> do
            allowAll (watchRoot p)
            settle
            writeHeldFiles p
        , check = const (modifiedNamed "file")
        , expected = onPlatform WatchException Yes WatchException
        }
    , base
        { title =
            "A recursive watch with a subdirectory without READ is watched"
        , config = Event.setRecursiveMode True
        , setup = \p -> do
            createDirectory (deniedDir p)
            denyRead (deniedDir p)
        , cleanup = allowRead . deniedDir
        , skip = stillReadable . deniedDir
        , ops = \p -> createFile (watchRoot p </> "file")
        , check = const (createdNamed "file")
        , expected = onPlatform Yes Yes Yes
        }
    -- Linux reports the subdir as isAccessDenied and does not watch it.
    , base
        { title = "A subdirectory without READ, with SEARCH reports its events"
        , config = Event.setRecursiveMode True
        , setup = \p -> do
            createDirectory (deniedDir p)
            denyRead (deniedDir p)
        , cleanup = allowRead . deniedDir
        , skip = stillReadable . deniedDir
        , ops = \p -> createFile (deniedDir p </> "file")
        , check = const (createdNamed "file")
        , expected = onPlatform No Yes Yes
        }
    , base
        { title = "A subdirectory without READ and SEARCH reports its events"
        , config = Event.setRecursiveMode True
        , setup = \p -> do
            createDirectory (deniedDir p)
            createFile (deniedDir p </> "file")
            holdFile p (deniedDir p </> "file")
            denyAll (deniedDir p)
        , cleanup = allowAll . deniedDir
        , skip = stillReadable . deniedDir
        , ops = writeHeldFiles
        , check = const (modifiedNamed "file")
        , expected = onPlatform No No Yes
        }
    , base
        { title =
            "A directory without READ moved into a recursive watch does not "
                ++ "end the watch"
        , config = Event.setRecursiveMode True
        , setup = \p -> do
            createDirectory (renamedRoot p)
            denyRead (renamedRoot p)
        , cleanup = \p -> do
            ignoreError (allowRead (renamedRoot p))
            ignoreError (allowRead (deniedDir p))
        , skip = stillReadable . renamedRoot
        , ops = \p -> do
            renameDirectory (renamedRoot p) (deniedDir p)
            settle
            createFile (watchRoot p </> "file")
        , check = const (createdNamed "file")
        , expected = onPlatform Yes Yes Yes
        }
    , base
        { title = "A subdirectory reports its events after its READ is removed"
        , config = Event.setRecursiveMode True
        , setup = createDirectory . deniedDir
        , cleanup = allowRead . deniedDir
        , skip = permissionNotRemovable denyRead allowRead
        , ops = \p -> do
            denyRead (deniedDir p)
            settle
            createFile (deniedDir p </> "file")
        , check = const (createdNamed "file")
        , expected = onPlatform Yes Yes Yes
        }
    , base
        { title =
            "A subdirectory reports its events after its READ and SEARCH "
                ++ "are removed"
        , config = Event.setRecursiveMode True
        , setup = \p -> do
            createDirectory (deniedDir p)
            createFile (deniedDir p </> "file")
        , cleanup = allowAll . deniedDir
        , skip = permissionNotRemovable denyAll allowAll
        , ops = \p -> do
            holdFile p (deniedDir p </> "file")
            denyAll (deniedDir p)
            settle
            writeHeldFiles p
        , check = const (modifiedNamed "file")
        , expected = onPlatform Yes No Yes
        }
    , base
        { title =
            "A subdirectory without READ and SEARCH reports its events "
                ++ "after they are restored"
        , config = Event.setRecursiveMode True
        , setup = \p -> do
            createDirectory (deniedDir p)
            createFile (deniedDir p </> "file")
            holdFile p (deniedDir p </> "file")
            denyAll (deniedDir p)
        , cleanup = allowAll . deniedDir
        , skip = stillReadable . deniedDir
        , ops = \p -> do
            allowAll (deniedDir p)
            settle
            writeHeldFiles p
        , check = const (modifiedNamed "file")
        , expected = onPlatform No Yes Yes
        }
    -- On Linux a subdirectory is opened through the root, which needs SEARCH
    -- on the root. Windows checks the traverse access of the subdirectory
    -- that changed, not of the root. On FreeBSD the root cannot be watched
    -- without SEARCH. The file "file" in the root is used by the skip check.
    , base
        { title = "A subdirectory of a root with only READ reports its events"
        , config = Event.setRecursiveMode True
        , setup = \p -> do
            createFile (watchRoot p </> "file")
            createDirectory (watchRoot p </> "dir")
            createFile (watchRoot p </> "dir" </> "subfile")
            holdFile p (watchRoot p </> "dir" </> "subfile")
            denySearch (watchRoot p)
            bypassTraverse False
        , cleanup = \p ->
            allowAll (watchRoot p) `finally` bypassTraverse True
        , skip = stillSearchable . watchRoot
        , ops = writeHeldFiles
        , check = const (modifiedNamed "subfile")
        , expected = onFreeBSD WatchException (onPlatform No No Yes)
        }
#if defined(CABAL_OS_WINDOWS)
    -- Without the "Bypass traverse checking" privilege NTFS checks the
    -- traverse access of the watcher to a subdirectory for each change in
    -- it, as macOS does. The privilege is disabled before the
    -- watch starts.
    , base
        { title =
            "Without the \"Bypass traverse checking\" privilege, a "
                ++ "subdirectory reports its events after its READ and "
                ++ "SEARCH are removed"
        , config = Event.setRecursiveMode True
        , setup = \p -> do
            createDirectory (deniedDir p)
            createFile (deniedDir p </> "file")
            setBypassTraverse False
        , cleanup = \p ->
            allowAll (deniedDir p) `finally` setBypassTraverse True
        , skip = permissionNotRemovable denyAll allowAll
        , ops = \p -> do
            holdFile p (deniedDir p </> "file")
            denyAll (deniedDir p)
            settle
            writeHeldFiles p
        , check = const (modifiedNamed "file")
        , expected = No
        }
#endif
    , base
        { title = "regular file as a root is watched"
        , setup = \p -> do
            removeDirectory (watchRoot p)
            createFile (watchRoot p)
            settle
        , ops = \p -> appendFile (watchRoot p) "x"
        , check = \p ->
            any (\ev -> absPath ev == watchRoot p && Event.isModified ev)
        , expected = onPlatform Yes Yes WatchException
        }
    , base
        { title =
            "A deleted symbolic link to a directory root reports "
                ++ "`isDeleted` for the root"
        , setup = \p -> do
            removeDirectory (watchRoot p)
            createDirectory (renamedRoot p)
            createDirectoryLink (renamedRoot p) (watchRoot p)
            settle
        , ops = removeDirectoryLink . watchRoot
        , check = rootDeleted
        , expected = onPlatform No No No
        }
    , base
        { title =
            "A renamed symbolic link to a directory root reports `isMoved` "
                ++ "for the root"
        , setup = \p -> do
            removeDirectory (watchRoot p)
            createDirectory (renamedRoot p)
            createDirectoryLink (renamedRoot p) (watchRoot p)
            settle
        , ops = renameRoot
        , check = rootMoved
        , expected = onPlatform No No No
        }
#if defined(CABAL_OS_LINUX) || defined(CABAL_OS_FREEBSD)
    -- Without following, the watch is on the symbolic link itself.
    , base
        { title =
            "A deleted symbolic link to a directory root watched in "
                ++ "no-follow mode reports `isDeleted` for the root"
        , config = Inotify.setFollowRootSymLinks False
        , setup = \p -> do
            removeDirectory (watchRoot p)
            createDirectory (renamedRoot p)
            createDirectoryLink (renamedRoot p) (watchRoot p)
            settle
        , ops = removeDirectoryLink . watchRoot
        , check = rootDeleted
        , expected = Yes
        }
    , base
        { title =
            "A renamed symbolic link to a directory root watched in "
                ++ "no-follow mode reports `isMoved` for the root"
        , config = Inotify.setFollowRootSymLinks False
        , setup = \p -> do
            removeDirectory (watchRoot p)
            createDirectory (renamedRoot p)
            createDirectoryLink (renamedRoot p) (watchRoot p)
            settle
        , ops = renameRoot
        , check = rootMoved
        , expected = Yes
        }
#endif
    -- Linux reports IN_DELETE_SELF when the inode is freed, an open
    -- reference delays it. FreeBSD reports it when the last link is removed.
    , base
        { title =
            "A deleted directory root in use by a process reports "
                ++ "`isDeleted` for the root"
        , setup = \p -> holdCwd p (watchRoot p) >> settle
        , ops = removeDirectory . watchRoot
        , check = rootDeleted
        , expected = onFreeBSD Yes (onPlatform No Yes Exception)
        }
#if defined(CABAL_OS_LINUX)
    , base
        { title =
            "A deleted directory root in use by a process reports "
                ++ "`isDeleted` for the root after the process ends"
        , setup = \p -> holdCwd p (watchRoot p) >> settle
        , ops = \p -> removeDirectory (watchRoot p) >> releaseHeld p
        , check = rootDeleted
        , expected = Yes
        }
#endif
    -- Windows does not rename a directory that is the working directory of
    -- a process.
    , base
        { title =
            "A renamed directory root in use by a process reports `isMoved` "
                ++ "for the root"
        , setup = \p -> holdCwd p (watchRoot p) >> settle
        , ops = renameRoot
        , check = rootMoved
        , expected = onPlatform Yes Yes Exception
        }
    , base
        { title =
            "A deleted regular file root reports `isDeleted` for the root"
        , setup = withFileRoot
        , ops = removeFile . watchRoot
        , check = rootDeleted
        , expected = onPlatform Yes Yes WatchException
        }
    , base
        { title =
            "A renamed regular file root reports `isMoved` for the root"
        , setup = withFileRoot
        , ops = renameRoot
        , check = rootMoved
        , expected = onPlatform Yes Yes WatchException
        }
    , base
        { title =
            "A deleted open regular file root reports `isDeleted` for the "
                ++ "root"
        , setup = \p -> withFileRoot p >> holdFile p (watchRoot p)
        , ops = removeFile . watchRoot
        , check = rootDeleted
        , expected = onFreeBSD Yes (onPlatform No Yes WatchException)
        }
#if defined(CABAL_OS_LINUX)
    , base
        { title =
            "A deleted open regular file root reports `isDeleted` for the "
                ++ "root after the file is closed"
        , setup = \p -> withFileRoot p >> holdFile p (watchRoot p)
        , ops = \p -> removeFile (watchRoot p) >> releaseHeld p
        , check = rootDeleted
        , expected = Yes
        }
#endif
    , base
        { title =
            "A renamed open regular file root reports `isMoved` for the root"
        , setup = \p -> withFileRoot p >> holdFile p (watchRoot p)
        , ops = renameRoot
        , check = rootMoved
        , expected = onPlatform Yes Yes WatchException
        }
    , base
        { title =
            "A deleted regular file root with another hard link reports "
                ++ "`isDeleted` for the root"
        , setup = \p -> do
            withFileRoot p
            hardLink (watchRoot p) (renamedRoot p)
        , ops = removeFile . watchRoot
        , check = rootDeleted
        , expected = onPlatform No Yes WatchException
        }
    , base
        { title =
            "A renamed regular file root with another hard link reports "
                ++ "`isMoved` for the root"
        , setup = \p -> do
            withFileRoot p
            hardLink (watchRoot p) (renamedRoot p)
        , ops = renameRoot
        , check = rootMoved
        , expected = onPlatform Yes Yes WatchException
        }
#if !defined(CABAL_OS_WINDOWS)
    , base
        { title = "A deleted FIFO root reports `isDeleted` for the root"
        , setup = \p -> do
            removeDirectory (watchRoot p)
            createNamedPipe (watchRoot p)
                (ownerReadMode `unionFileModes` ownerWriteMode)
            settle
        , ops = removeFile . watchRoot
        , check = rootDeleted
        , expected = onPlatform Yes Yes Yes
        }
    , base
        { title = "A renamed FIFO root reports `isMoved` for the root"
        , setup = \p -> do
            removeDirectory (watchRoot p)
            createNamedPipe (watchRoot p)
                (ownerReadMode `unionFileModes` ownerWriteMode)
            settle
        , ops = renameRoot
        , check = rootMoved
        , expected = onPlatform Yes Yes Yes
        }
#endif
    , base
        { title = "non-recursive watch reports a file created in a subdir"
        , setup = withDir
        , ops = \p -> createFile (watchRoot p </> "dir/file")
        , check = const (createdNamed "file")
        , expected = onPlatform No No No
        }
    -- On Linux, dir2 is a new directory when IN_MOVED_TO of dir2 is read, the
    -- watch of the renamed directory is removed then and added again on
    -- IN_MOVED_TO of dir3. See "Keeping the watch over two renames" in
    -- dev/design.linux.md.
    , base
        { title =
            "write in a dir renamed twice, middle path reused, is reported"
        , config = Event.setRecursiveMode True
        , setup = \p -> do
            createDirectory (watchRoot p </> "dir1")
            createFile (watchRoot p </> "dir1/file")
            settle
        , paused = True
        , ops = \p -> do
            let root = watchRoot p
            renameDirectory (root </> "dir1") (root </> "dir2")
            renameDirectory (root </> "dir2") (root </> "dir3")
            createDirectory (root </> "dir2")
            appendFile (root </> "dir3/file") "x"
        , check = \p ->
            any (\ev ->
                absPath ev == watchRoot p </> "dir3/file"
                    && Event.isModified ev)
        , expected = onPlatform No Yes No
        }
    , base
        { title = "rename reports isMoved for the source and the destination"
        , setup = withFile
        , ops = \p ->
            renameFile (watchRoot p </> "file") (watchRoot p </> "moved")
        , check = \_ evs ->
            let moved name = any (\ev -> isNamed name ev && Event.isMoved ev)
             in moved "file" evs && moved "moved" evs
        , expected = onPlatform Yes Yes Yes
        }
    , base
        { title =
            "A move reports the source and the destination as separate "
                ++ "predicates"
        , setup = withFile
        , ops = \p ->
            renameFile (watchRoot p </> "file") (watchRoot p </> "moved")
        , check = \_ evs ->
            let reported name f g =
                    any (\ev -> isNamed name ev && f ev && not (g ev)) evs
             in reported "file" movedFrom movedTo
                    && reported "moved" movedTo movedFrom
        , expected = onPlatform Yes No Yes
        }
    , dirChange
        { title =
            "recursive watch reports isModified for a dir when a file is"
                ++ " created in it"
        , config = Event.setRecursiveMode True
        , expected = onPlatform No No No
        }
    -- On Windows the outcome is Yes on Windows Server 2025 and No on Windows
    -- Server 2012 R2.
    , dirChange
        { title =
            "recursive watch reports isModified for a dir when a file is"
                ++ " deleted from it"
        , config = Event.setRecursiveMode True
        , setup = \p -> do
            createDirectory (watchRoot p </> "dir")
            createFile (watchRoot p </> "dir/file")
            settle
        , ops = \p -> removeFile (watchRoot p </> "dir/file")
        , expected = onPlatform No No No
        , racy = onWindows
        }
    , dirChange
        { title =
            "non-recursive watch reports isModified for a dir when a file"
                ++ " is created in it"
        , expected = onPlatform No No No
        }
    , deletedOpenFile
        { title = "write to an open file after its deletion is reported"
        , expected = onPlatform No Yes Exception
        }
#if defined(CABAL_OS_LINUX) || defined(CABAL_OS_FREEBSD)
    -- FreeBSD never reports the events of an unlinked file.
    , deletedOpenFile
        { title = "write to a deleted file, setExcludeUnlinked False"
        , config = Inotify.setExcludeUnlinked False
        , expected = onFreeBSD No Yes
        }
#endif
    -- The metadata change events are enabled. On macOS chmod reports
    -- isSecurityModified in every run, isAttrsModified and isModified in
    -- some runs. Whether these are the flags of the create in the setup, or
    -- the change of an attribute by chmod, e.g. its ctime, is not known.
    , metadataChange
        { title = "making a file read-only reports isAttrsModified"
        , check = metadataEvent attrsModified
        , expected = onPlatform Yes No No
        , racy = onDarwin
        }
#if defined(CABAL_OS_DARWIN)
    -- Whether isAttrsModified is still reported with a longer time between
    -- the create and the chmod.
    , readOnlyAfter 5
    , readOnlyAfter 15
    , readOnlyAfter 30
#endif
    , metadataChange
        { title = "making a file read-only reports isSecurityModified"
        , check = metadataEvent securityModified
        , expected = onPlatform No Yes No
        }
    , metadataChange
        { title = "making a file read-only reports isModified"
        , check = metadataEvent Event.isModified
        , expected = onPlatform No No Yes
        , racy = onDarwin
        }
    , setTimesChange
        { title = "setting the file times reports isAttrsModified"
        , check = metadataEvent attrsModified
        , expected = onPlatform Yes Yes No
        }
    , setTimesChange
        { title = "setting the file times reports isModified"
        , check = metadataEvent Event.isModified
        , expected = onPlatform No No Yes
        , racy = onDarwin
        }
#if !defined(CABAL_OS_WINDOWS)
    -- Windows sets the modification time only in the tests of each config
    -- option. Linux reports it as IN_MODIFY, FreeBSD as IN_ATTRIB.
    , setTimesChange
        { title = "setting only the file modification time reports isModified"
        , ops = \p -> setModTime (watchRoot p </> "file")
        , check = metadataEvent Event.isModified
        , expected = onPlatform (onFreeBSD No Yes) No No
        , racy = onDarwin
        }
    , setTimesChange
        { title =
            "setting only the file modification time reports isAttrsModified"
        , ops = \p -> setModTime (watchRoot p </> "file")
        , check = metadataEvent attrsModified
        , expected = onPlatform (onFreeBSD Yes No) Yes No
        }
#endif
    , hardLinkSteps
        { title =
            "creating, then deleting a hard link reports isCreated, then "
                ++ "isDeleted for the link"
        , steps =
            [ (createLink, const (createdNamed "link"))
            , (deleteLink, \_ -> any (\ev -> isNamed "link" ev
                && Event.isDeleted ev))
            ]
        , expected = Steps [Yes, Yes]
        }
    , hardLinkSteps
        { title =
            "creating, then deleting a hard link reports isAttrsModified for "
                ++ "the directory"
        , config = dirMetadataConfig
        , steps = [(createLink, dirMetadata), (deleteLink, dirMetadata)]
        , expected =
            onPlatform (Steps [No, No]) (Steps [Yes, No]) (Steps [No, No])
        }
    , hardLinkSteps
        { title =
            "creating, then deleting a hard link reports isAttrsModified for "
                ++ "the file"
        , steps = [(createLink, linkFileAttrs), (deleteLink, linkFileAttrs)]
        , expected = Steps [No, No]
        }
    , dirAttrsChange
        { title = "creating a file reports isAttrsModified for the directory"
        , ops = \p -> createFile (watchRoot p </> "file")
        }
    , dirAttrsChange
        { title = "deleting a file reports isAttrsModified for the directory"
        , setup = withFile
        , ops = \p -> removeFile (watchRoot p </> "file")
        }
    , dirAttrsChange
        { title = "renaming a file reports isAttrsModified for the directory"
        , setup = withFile
        , ops = \p ->
            renameFile (watchRoot p </> "file") (watchRoot p </> "renamed")
        }
    , dirAttrsChange
        { title =
            "creating a subdirectory reports isAttrsModified for the directory"
        , ops = \p -> createDirectory (watchRoot p </> "dir")
        }
    , dirAttrsChange
        { title =
            "deleting a subdirectory reports isAttrsModified for the directory"
        , setup = withDir
        , ops = \p -> removeDirectory (watchRoot p </> "dir")
        }
#if !defined(CABAL_OS_WINDOWS)
    , dirAttrsChange
        { title =
            "creating a symbolic link reports isAttrsModified for the "
                ++ "directory"
        , setup = withFile
        , ops = \p -> Posix.createSymbolicLink "file" (watchRoot p </> "link")
        }
#endif
    , dirAttrsChange
        { title =
            "changing the permissions of the directory reports "
                ++ "isAttrsModified for the directory"
        , ops = changeDirMode . watchRoot
        , expected = onPlatform Yes No No
        }
    , dirAttrsChange
        { title =
            "setting the times of the directory reports isAttrsModified for "
                ++ "the directory"
        , ops = setTimes . watchRoot
        , expected = onPlatform Yes Yes No
        }
    , subdirAttrsChange
        { title =
            "changing the permissions of a subdirectory reports "
                ++ "isAttrsModified for the subdirectory"
        , ops = \p -> changeDirMode (watchRoot p </> "dir")
        , expected = onPlatform Yes No Yes
        }
#if defined(CABAL_OS_DARWIN)
    , base
        { title =
            "changing the permissions of the directory reports "
                ++ "isSecurityModified for the directory"
        , ops = changeDirMode . watchRoot
        , check = \p ->
            any (\ev -> absPath ev == watchRoot p && securityModified ev)
        , expected = Yes
        }
    , base
        { title =
            "changing the permissions of a subdirectory reports "
                ++ "isSecurityModified for the subdirectory"
        , setup = withDir
        , ops = \p -> changeDirMode (watchRoot p </> "dir")
        , check =
            const (any (\ev -> isNamed "dir" ev && securityModified ev))
        , expected = Yes
        }
#endif
    , subdirAttrsChange
        { title =
            "setting the times of a subdirectory reports isAttrsModified for "
                ++ "the subdirectory"
        , ops = \p -> setTimes (watchRoot p </> "dir")
        }
    -- On Linux, the kernel merges the second write into the first because the
    -- first is the last event in the queue and it has not been read. On macOS
    -- FSEvents merges the writes only if they are reported in the same batch.
    , base
        { title = "two writes to a file before the read report one isModified"
        , setup = withFile
        , paused = True
        , ops = \p -> do
            let file = watchRoot p </> "file"
            appendFile file "x"
            appendFile file "y"
        , check = \_ evs ->
            let modified ev = isNamed "file" ev && Event.isModified ev
             in length (filter modified evs) == 1
        , expected = onPlatform Yes Yes No
        , racy = onDarwin
        }
    -- On Linux only adjacent identical events are merged. On macOS FSEvents
    -- merges the writes to "file" only if they are reported in the same batch.
    , base
        { title = "interleaved writes to two files report each isModified"
        , setup = \p -> do
            createFile (watchRoot p </> "file")
            createFile (watchRoot p </> "other")
            settle
        , paused = True
        , ops = \p -> do
            appendFile (watchRoot p </> "file") "x"
            appendFile (watchRoot p </> "other") "x"
            appendFile (watchRoot p </> "file") "y"
        , check = \_ evs ->
            let modified ev = isNamed "file" ev && Event.isModified ev
             in length (filter modified evs) == 2
        , expected = onPlatform Yes No Yes
        , racy = onDarwin
        }
    -- Overwriting truncates the file and then writes it.
    , base
        { title = "one overwrite of a file reports several isModified"
        , setup = \p -> do
            writeFile (watchRoot p </> "file") "old"
            settle
        , ops = \p -> writeFile (watchRoot p </> "file") "new"
        , check = \_ evs ->
            let modified ev = isNamed "file" ev && Event.isModified ev
             in length (filter modified evs) > 1
        , expected = onPlatform No No Yes
        , racy = True
        }
    , base
        { title = "one event reports isCreated and isDeleted for a file"
        , paused = True
        , ops = \p -> do
            let file = watchRoot p </> "file"
            createFile file
            removeFile file
        , check = \_ ->
            any (\ev ->
                isNamed "file" ev && Event.isCreated ev && Event.isDeleted ev)
        , expected = onPlatform No Yes No
        }
    , base
        { title = "one event reports isCreated and isMoved for a file"
        , paused = True
        , ops = \p -> do
            createFile (watchRoot p </> "file")
            renameFile (watchRoot p </> "file") (watchRoot p </> "moved")
        , check = \_ ->
            any (\ev ->
                isNamed "file" ev && Event.isCreated ev && Event.isMoved ev)
        , expected = onPlatform No Yes No
        }
    , base
        { title =
            "A deleted directory root reports `isDeleted` for each file in it"
        , setup = \p -> do
            createFile (watchRoot p </> "file")
            createFile (watchRoot p </> "file2")
            settle
        , ops = removePathForcibly . watchRoot
        , check = const (deletedNamed ["file", "file2"])
        , expected = onPlatform Yes No Yes
        }
    , base
        { title =
            "A renamed directory root reports `isMoved` for each file in it"
        , setup = \p -> do
            createFile (watchRoot p </> "file")
            createFile (watchRoot p </> "file2")
            settle
        , ops = renameRoot
        , check = \_ evs ->
            all (\name -> any (\ev -> isNamed name ev && Event.isMoved ev) evs)
                ["file", "file2"]
        , expected = onPlatform No No No
        }
    , base
        { title =
            "Deleting a directory in a recursive watch reports `isDeleted` "
                ++ "for each file in it"
        , config = Event.setRecursiveMode True
        , setup = \p -> do
            createDirectory (watchRoot p </> "dir")
            createFile (watchRoot p </> "dir/file")
            createFile (watchRoot p </> "dir/file2")
            settle
        , ops = \p -> removePathForcibly (watchRoot p </> "dir")
        , check = const (deletedNamed ["file", "file2"])
        , expected = onPlatform Yes Yes Yes
        }
    -- On macOS the copy is a clone, on APFS it reports 'Darwin.isCloned'.
    , base
        { title = "copy of a file reports isCreated for the copy"
        , setup = withFile
        , ops = \p ->
            copyFile (watchRoot p </> "file") (watchRoot p </> "copy")
        , check = const (createdNamed "copy")
        , expected = onPlatform Yes Yes Yes
        }
    -- A path listed twice is one root.
    --
    -- On macOS a second FSEvents event for the file can also carry the
    -- created flag, it has a different event id. A copy of one event for
    -- each root has the same event id.
    , base
        { title = "root listed twice reports a file created in it twice"
        , extraRoots = \p -> [watchRoot p]
        , ops = \p -> createFile (watchRoot p </> "file")
        , check = \_ evs ->
            let created ev = isNamed "file" ev && Event.isCreated ev
#if defined(CABAL_OS_DARWIN)
                ids = sort (map Darwin.getEventId (filter created evs))
             in any ((> 1) . length) (group ids)
#else
             in length (filter created evs) == 2
#endif
        , expected = No
        }
    , base
        { title =
            "root and a symbolic link to it report a file created in it "
                ++ "for both"
        , setup = \p -> do
            createDirectoryLink (watchRoot p) (linkRoot p)
            settle
        , extraRoots = \p -> [linkRoot p]
        , ops = \p -> createFile (watchRoot p </> "file")
        , check = const (createdForRoots 2 "file")
        , expected = Yes
        }
    -- The upper case path exists only on a case-insensitive file system.
    , base
        { title =
            "root and its path in upper case report a file created in it "
                ++ "for both"
        , extraRoots = \p -> [upperRoot p]
        , skip = \p -> do
            exists <- doesDirectoryExist (upperRoot p)
            return $
                if exists
                then Nothing
                else Just "the file system is case sensitive"
        , ops = \p -> createFile (watchRoot p </> "file")
        , check = const (createdForRoots 2 "file")
        , expected = Yes
        }
    , base
        { title = "nested roots report a file in the inner root for both"
        , config = Event.setRecursiveMode True
        , setup = withDir
        , extraRoots = \p -> [watchRoot p </> "dir"]
        , ops = \p -> createFile (watchRoot p </> "dir/file")
        , check = const (createdForRoots 2 "file")
        , expected = Yes
        }
#if defined(CABAL_OS_DARWIN)
    , base
        { title = "clone reports isCloned for the source and the clone"
        , setup = withFile
        , ops = \p ->
            copyFile (watchRoot p </> "file") (watchRoot p </> "copy")
        , check = \_ evs ->
            let cloned name =
                    any (\ev -> isNamed name ev && Darwin.isCloned ev) evs
             in cloned "file" && cloned "copy"
        , expected = Yes
        }
#endif
    ]
#if defined(CABAL_OS_WINDOWS)
    ++ windowsConfigBehaviors
#endif

    where

    onWindows = platformName == "Windows"

    onDarwin = platformName == "macOS"

    rootDeleted p =
        any (\ev -> absPath ev == watchRoot p && Event.isDeleted ev)

    -- The watch root is renamed to "moved", next to it.
    movedRoot p = takeDirectory (watchRoot p) </> "moved"

    renameRoot p = renamePath (watchRoot p) (movedRoot p)

    rootMoved p =
        let isRoot ev = absPath ev `elem` [watchRoot p, movedRoot p]
         in any (\ev -> isRoot ev && Event.isMoved ev)

    -- Replace the watch root directory by a regular file.
    withFileRoot p = do
        removeDirectory (watchRoot p)
        createFile (watchRoot p)
        settle

    -- A symbolic link to the watch root, next to it.
    linkRoot p = takeDirectory (watchRoot p) </> "link"

    -- The watch root is "root".
    upperRoot p = takeDirectory (watchRoot p) </> "ROOT"

    -- Whether the create events of the file report the given number of
    -- roots.
    createdForRoots n name evs =
        let roots =
                [ Path.toString (Event.getRoot ev)
                | ev <- evs, isNamed name ev, Event.isCreated ev
                ]
         in length (nub roots) == n

    -- A root in the directory "parent", next to the watch root.
    nestedParent p = takeDirectory (watchRoot p) </> "parent"

    nestedRoot p = nestedParent p </> "root"

    movedNestedRoot p = takeDirectory (watchRoot p) </> "moved" </> "root"

    volumeDir = takeDirectory . watchRoot

    unmount =
        base
            { skip = \p -> do
                r <- try (mountVolume (volumeDir p) (watchRoot p))
                return $ case r of
                    Left (e :: IOException) ->
                        Just ("the file system cannot be mounted: " ++ show e)
                    Right () -> Nothing
            , cleanup = \p -> removeVolume (volumeDir p) (watchRoot p)
            , ops = \p -> forceUnmountVolume (volumeDir p) (watchRoot p)
            , racy = platformName /= "Linux"
            }

    renameNestedParent p = do
        renameDirectory (nestedParent p) (takeDirectory (movedNestedRoot p))
        settle

    withDir p = do
        createDirectory (watchRoot p </> "dir")
        settle

    withFile p = do
        createFile (watchRoot p </> "file")
        settle

#if defined(CABAL_OS_WINDOWS)
    withFileContents p = do
        writeFile (watchRoot p </> "file") "old"
        settle
#endif

#if defined(CABAL_OS_WINDOWS)
    only f =
        f True . Windows.setFileNameEvents True . Windows.setAllEvents False

    windowsConfigs =
        [ ("the default config", id)
        , ("only setAttrsModified", only Windows.setAttrsModified)
        , ("only setSizeModified", only Windows.setSizeModified)
        , ( "only setLastWriteTimeModified"
          , only Windows.setLastWriteTimeModified)
        , ( "only setLastAccessTimeModified"
          , only Windows.setLastAccessTimeModified)
        , ( "only setCreationTimeModified"
          , only Windows.setCreationTimeModified)
        , ("only setSecurityModified", only Windows.setSecurityModified)
        ]

    -- The items an operation is applied to, a file or a subdirectory of the
    -- watch root.
    fileItem = ("file", withFile)
    dirItem = ("dir", withDir)

    windowsMetadataOps =
        [ ("making the item read-only", makeReadOnly, [fileItem, dirItem]
          , ["only setAttrsModified"])
        , ("setting the item times", setTimes, [fileItem, dirItem]
          , [ "the default config", "only setLastWriteTimeModified"
            , "only setLastAccessTimeModified"])
        , ( "setting the item modification time"
          , setFileTimes False False True, [fileItem, dirItem]
          , ["the default config", "only setLastWriteTimeModified"])
        , ( "setting the item access time", setFileTimes False True False
          , [fileItem, dirItem], ["only setLastAccessTimeModified"])
        , ( "setting the item creation time", setFileTimes True False False
          , [fileItem, dirItem], ["only setCreationTimeModified"])
        , ( "truncating the item", truncateFile
          , [("file", withFileContents)]
          , [ "the default config", "only setSizeModified"
            , "only setLastWriteTimeModified"
            , "only setLastAccessTimeModified"])
        , ( "setting the hidden attribute of the item", makeHidden
          , [fileItem, dirItem], ["only setAttrsModified"])
        ]

    -- Each metadata change with the default config, and with one config
    -- option enabled and the other tunable events disabled. The file name
    -- events stay enabled, 'waitStarted' needs them. The last field of an
    -- operation lists the configs that report isModified.
    windowsConfigBehaviors =
        [ base
            { title =
                opName ++ " reports isModified for the " ++ name ++ " with "
                    ++ cfgName
            , config = cfg
            , setup = setupOp
            , ops = \p -> op (watchRoot p </> name)
            , check =
                const (any (\ev -> isNamed name ev && Event.isModified ev))
            , expected = if cfgName `elem` yes then Yes else No
            }
        | (opName, op, items, yes) <- windowsMetadataOps
        , (name, setupOp) <- items
        , (cfgName, cfg) <- windowsConfigs
        ]
#endif

    withDirFile p = do
        createDirectory (watchRoot p </> "dir")
        createFile (watchRoot p </> "dir/file")
        settle

    rootStillReadable = stillReadable . watchRoot

    stillReadable dir = do
        r <- try (listDirectory dir)
        return $ case r of
            Left (_ :: IOException) -> Nothing
            Right _ -> Just "the permission was not removed"

    -- The permission is removed by 'ops', after the watch starts. The probe
    -- directory is not watched.
    permissionNotRemovable ::
           (FilePath -> IO ())
        -> (FilePath -> IO ())
        -> Paths
        -> IO (Maybe String)
    permissionNotRemovable deny allow p = do
        let probe = renamedRoot p
        createDirectory probe
        deny probe
        r <- stillReadable probe
        allow probe
        removeDirectory probe
        return r

    -- Whether the entry "file" of the directory can be reached.
    stillSearchable dir = do
        r <- try (getPermissions (dir </> "file"))
        return $ case r of
            Left (_ :: IOException) -> Nothing
            Right _ -> Just "the permission was not removed"

    -- The permission required to watch a directory.
    denyRequired = if onDarwin then denySearch else denyRead

    requiredStillGranted dir =
        if onDarwin then stillSearchable dir else stillReadable dir

    -- As 'permissionNotRemovable', for 'denyRequired'.
    requiredNotRemovable p = do
        let probe = renamedRoot p
        createDirectory probe
        createFile (probe </> "file")
        denyRequired probe
        r <- requiredStillGranted probe
        allowAll probe
        removePathForcibly probe
        return r

    deniedDir p = watchRoot p </> "denied"

    ignoreError action =
        void (try action :: IO (Either IOException ()))

    dirChange =
        base
            { setup = withDir
            , ops = \p -> createFile (watchRoot p </> "dir/file")
            , check = \_ ->
                any (\ev -> isNamed "dir" ev && Event.isModified ev)
            }

    -- AppendMode does not truncate the file, the write is the only change to
    -- its contents.
    deletedOpenFile =
        base
            { setup = withFile
            , ops = \p -> do
                let file = watchRoot p </> "file"
                h <- openFile file AppendMode
                removeFile file
                hPutStr h "x"
                hFlush h
                hClose h
            , check = \_ ->
                any (\ev -> isNamed "file" ev && Event.isModified ev)
            }

    metadataChange =
        base
            { config = setAttrs True
            , setup = withFile
            , ops = \p -> makeReadOnly (watchRoot p </> "file")
            }

#if defined(CABAL_OS_DARWIN)
    readOnlyAfter seconds =
        metadataChange
            { title =
                "making a file read-only " ++ show seconds
                    ++ " s after its create reports isAttrsModified"
            , setup = \p -> do
                createFile (watchRoot p </> "file")
                threadDelay (seconds * 1000000)
            , check = metadataEvent attrsModified
            , expected = No
            , racy = True
            }
#endif

    setTimesChange =
        base
            { config = setAttrs True
            , setup = withFile
            , ops = \p -> setTimes (watchRoot p </> "file")
            }

    metadataEvent f _ = any (\ev -> isNamed "file" ev && f ev)

    hardLinkSteps = base {setup = withFile}

    createLink p = hardLink (watchRoot p </> "file") (watchRoot p </> "link")

    deleteLink p = removeFile (watchRoot p </> "link")

    dirMetadata p =
        any (\ev -> absPath ev == watchRoot p && dirMetadataModified ev)

    -- The title names the Linux and macOS event, on Windows the check is
    -- isModified.
    dirAttrsChange =
        base
            { config = dirMetadataConfig
            , check = dirMetadata
            , expected = onPlatform No No No
            }

    -- The title names the Linux and macOS event, on Windows the check is
    -- isModified. The macOS and Windows outcomes are not verified yet.
    subdirAttrsChange =
        base
            { config = dirMetadataConfig
            , setup = withDir
            , check =
                const (any (\ev -> isNamed "dir" ev && dirMetadataModified ev))
            , expected = onPlatform Yes Yes Yes
            }

    -- The file has two names, the event can report either of them.
    linkFileAttrs _ =
        any (\ev -> (isNamed "file" ev || isNamed "link" ev)
            && attrsModified ev)

    newDir =
        base
            { config = Event.setRecursiveMode True
            , paused = True
            , ops = \p -> do
                createDirectoryIfMissing True (watchRoot p </> "dir/sub")
                createFile (watchRoot p </> "dir/file")
            }

    renameRootAndCreate p = do
        renameDirectory (watchRoot p) (renamedRoot p)
        settle
        createFile (renamedRoot p </> "file")

-------------------------------------------------------------------------------
-- Running a behavior
-------------------------------------------------------------------------------

-- | The time to wait for the events of a file system operation. Events of
-- different roots are not ordered on Windows, therefore, the marker files in
-- the control root do not guarantee that the events of the watch root have
-- been read.
settle :: IO ()
settle = threadDelay 1000000

-- | Create marker files in the control root until the consumer reads one.
-- The watch has started when it is read. Stop if the consumer has ended, it
-- can end with an exception before the watch starts.
waitStarted :: MVar a -> FilePath -> MVar () -> IO ()
waitStarted finished control started = go (0 :: Int)

    where

    go n = do
        ended <- not <$> isEmptyMVar finished
        unless ended $ do
            when (n >= 100) $ error "waitStarted: the watch did not start"
            createFile (control </> ("started" ++ show n))
            threadDelay 100000
            r <- tryReadMVar started
            maybe (go (n + 1)) return r

-- | One line for an event: the path relative to the first of the
-- directories that contains it, and the names of the predicates that are true
-- for the event. The names are the lines after the flags line of
-- 'Event.showEvent'.
eventLine :: [FilePath] -> Int -> Event -> String
eventLine dirs i ev =
    stepPrefix ++ relPath ++ ": " ++ unwords names

    where

    stepPrefix = if i == 0 then "" else "step " ++ show i ++ ": "

    path = absPath ev

    relPath =
        case [r | d <- dirs, let r = makeRelative d path, r /= path] of
            r : _ -> r
            [] -> path

    names =
        drop 1
            $ dropWhile (not . isPrefixOf "Flags ")
            $ filter (not . null)
            $ lines (Event.showEvent ev)

-- | Watch the root and the control root, run the operations of the behavior
-- and check the events. The events, except those of the control root, are
-- written to the event log as lines relative to the directories.
runWatch ::
    IORef [String] -> [FilePath] -> Behavior -> Paths -> FilePath -> IO Outcome
runWatch eventLog dirs Behavior{..} paths control = do
    let root = watchRoot paths
    started <- newEmptyMVar
    blocked <- newEmptyMVar
    resume <- newEmptyMVar
    result <- newEmptyMVar
    step <- newIORef (0 :: Int)

    -- The control root stays in place when a behavior changes the watch
    -- root. Its stop file ends the stream in all cases.
    let observe ev = do
            when ("started" `isPrefixOf` takeFileName (absPath ev))
                $ void $ tryPutMVar started ()
            when (isNamed "blocked" ev) $ do
                void $ tryPutMVar blocked ()
                readMVar resume
            i <- readIORef step
            return (i, ev)
        consumer = do
            rootPath <- Path.fromString root
            extraPaths <- mapM Path.fromString (extraRoots paths)
            controlPath <- Path.fromString control
            Event.watchWith config (rootPath :| extraPaths ++ [controlPath])
                & Stream.mapM observe
                & Stream.takeWhile (not . isNamed "stop" . snd)
                & Stream.fold Fold.toList
        runOps = do
            when paused $ do
                createFile (root </> "blocked")
                readMVar blocked
            r <- tryIO (ops paths)
            printException r
            putMVar resume ()
            settle
            r1 <- if isLeft r then return r else tryIO (laterOps paths)
            settle
            if isLeft r1 then return r1 else tryIO runSteps
        runSteps =
            forM_ (zip [1 ..] steps) $ \(i, (op, _)) -> do
                writeIORef step i
                op paths
                settle

    void $ forkIO (try consumer >>= putMVar result)
    waitStarted result control started

    ended <- not <$> isEmptyMVar result
    r1 <- if ended then return (Right ()) else runOps
    createFile (control </> "stop")

    evs <- timeout 30000000 (takeMVar result)
    mapM_ printException evs
    let controlDirs = [d </> takeFileName control | d <- dirs]
        isControl ev = any (`isPrefixOf` absPath ev) controlDirs
    case evs of
        Just (Right xs) ->
            writeIORef eventLog
                [eventLine dirs i ev | (i, ev) <- xs, not (isControl ev)]
        _ -> return ()
    return $ case evs of
        Nothing -> error "the stream did not end"
        Just (Left e)
            | isIOException e -> WatchException
            | otherwise -> error (show e)
        Just (Right xs)
            | isLeft r1 -> Exception
            | not (null steps) ->
                Steps
                    [ toOutcome (chk paths [ev | (j, ev) <- xs, j == i])
                    | (i, (_, chk)) <- zip [1 ..] steps
                    ]
            | check paths (map snd xs) -> Yes
            | otherwise -> No

    where

    tryIO :: IO () -> IO (Either IOException ())
    tryIO = try

    toOutcome b = if b then Yes else No

    -- The outcome does not include the exception, it is printed for the CI
    -- logs.
    printException :: Show e => Either e a -> IO ()
    printException =
        either (\e -> putStrLn ("  exception: " ++ show e)) (const (return ()))

    isIOException :: SomeException -> Bool
    isIOException e = isJust (fromException e :: Maybe IOException)

runBehavior :: IORef [String] -> Behavior -> IO Outcome
runBehavior eventLog b =
    withSystemTempDirectory "fsevent_behavior" $ \dir -> do
        let root = dir </> "root"
            control = dir </> "control"
        held <- newIORef []
        procs <- newIORef []
        let paths =
                Paths
                    { watchRoot = root
                    , renamedRoot = dir </> "renamed"
                    , heldFiles = held
                    , heldProcesses = procs
                    }
        createDirectory root
        createDirectory control
        setup b paths
        flip finally (cleanup b paths >> releaseHeld paths) $ do
            reason <- skip b paths
            -- On macOS the event paths are canonical, the temporary
            -- directory is not.
            canonical <- canonicalizePath dir
            let run = runWatch eventLog [dir, canonical] b paths control
            maybe run (return . Skipped) reason

-- | Any failure to determine the outcome is an outcome.
outcomeWithin :: Int -> IO Outcome -> IO Outcome
outcomeWithin seconds action = do
    r <- try (timeout (seconds * 1000000) (action >>= evaluate))
    return $ case r of
        Right (Just outcome) -> outcome
        Right Nothing -> Failed "timed out"
        Left e -> Failed (show (e :: SomeException))

-- | The expected outcome printed after the observed one. A racy behavior
-- accepts any outcome. Nothing is printed if the outcome is the expected one.
expectedNote :: Behavior -> Outcome -> String
expectedNote b outcome
    | racy b = " (expected any)"
    | outcome == expected b = ""
    | otherwise = " (expected " ++ show (expected b) ++ ")"

#if defined(CABAL_OS_WINDOWS)
-- | The major version of Windows, from the output of @ver@, for example,
-- @Microsoft Windows [Version 10.0.26100.1]@. 10 if @ver@ fails or its
-- output cannot be parsed.
windowsMajorVersion :: IO Int
windowsMajorVersion = do
    -- With readProcess "cmd" ["/c", "ver"] cmd receives the quoted
    -- argument "ver" and fails, 'shell' passes the command unquoted.
    r <- try (readCreateProcess (shell "ver") "")
    return $ case r of
        Left (_ :: IOException) -> 10
        -- Not 'reads' on the rest of the output, it reads "6.3" as one
        -- number and fails for an Int.
        Right out ->
            case takeWhile isDigit (dropWhile (not . isDigit) out) of
                [] -> 10
                digits -> read digits
#endif

main :: IO ()
main = do
#if defined(CABAL_OS_WINDOWS)
    major <- windowsMajorVersion
    putStrLn $ "Windows major version: " ++ show major
    when (major < 10) $ do
        putStrLn $ "Skipped on Windows version " ++ show major
            ++ ", the suite needs Windows 10 or Windows Server 2016"
        exitSuccess
    disableBackupPrivileges
#endif
    resultsRef <- newIORef []
    let spec seconds b runner = it (title b) $ do
            eventLog <- newIORef []
            outcome <- outcomeWithin seconds (runner eventLog)
            evLines <- readIORef eventLog
            modifyIORef' resultsRef ((b, outcome, evLines) :)
            case outcome of
                Skipped reason -> pendingWith reason
                _ -> unless (racy b) $ outcome `shouldBe` expected b
    summary <- hspecResult $ describe moduleName $ do
        mapM_ (\b -> spec 60 b (`runBehavior` b)) behaviors

    results <- reverse <$> readIORef resultsRef
    putStrLn ""
    putStrLn $ "Behavior results on " ++ platformName ++ ":"
    forM_ results $ \(b, outcome, _) ->
        putStrLn $ "  " ++ title b ++ ": " ++ show outcome
            ++ expectedNote b outcome

    -- The paths are relative to the temporary directory of the behavior,
    -- "root" is the watch root.
    putStrLn ""
    putStrLn $ "Behavior events on " ++ platformName ++ ":"
    forM_ results $ \(b, _, evLines) ->
        when (printEvents b) $ do
            putStrLn $ "  " ++ title b ++ ":"
            mapM_ (putStrLn . ("    " ++)) evLines

    when (summaryFailures summary > 0) exitFailure
