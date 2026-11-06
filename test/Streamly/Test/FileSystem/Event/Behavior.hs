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
-- An outcome that is not known on a platform is not asserted there, it is
-- recorded from the CI logs.

module Streamly.Test.FileSystem.Event.Behavior (main) where

import Control.Concurrent
    ( MVar, forkIO, isEmptyMVar, killThread, newEmptyMVar, putMVar, readMVar
    , takeMVar, threadDelay, tryPutMVar, tryReadMVar
    )
import Control.Exception
    (IOException, SomeException, evaluate, finally, fromException, try)
import Control.Monad (forM_, unless, void, when)
import Data.Either (isLeft)
import Data.Function ((&))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (group, isPrefixOf, sort)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (isJust)
import System.Exit (exitFailure)
import System.Directory
    ( createDirectory, createDirectoryIfMissing, createDirectoryLink
    , getPermissions, listDirectory, removeDirectory, removeDirectoryLink
    , removeFile, removePathForcibly, renameDirectory, renameFile
    , setOwnerWritable, setPermissions
#if !defined(CABAL_OS_WINDOWS)
    , emptyPermissions, setOwnerReadable, setOwnerSearchable
#endif
    )
import System.FilePath ((</>), takeFileName)
import System.IO (IOMode(AppendMode), hClose, hFlush, hPutStr, openFile)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcess)
import System.Timeout (timeout)
#if defined(CABAL_OS_WINDOWS)
import System.Win32.HardLink (createHardLink)
#endif
import Streamly.Internal.FS.Event (Config, Event)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.FS.Event as Event
#if defined(CABAL_OS_LINUX)
import qualified Streamly.Internal.FS.Event.Linux as Linux
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
    deriving (Eq, Show)

-- | The paths used by a behavior.
data Paths = Paths
    { watchRoot :: FilePath
    , renamedRoot :: FilePath
    }

data Behavior = Behavior
    { title :: String
    , config :: Config -> Config
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
    , expected :: Outcome
    -- | The outcome depends on a race or on the OS version, or it is not known
    -- on this platform. It is printed but does not fail the test when it
    -- differs from 'expected'.
    , racy :: Bool
    }

-- | The arguments are the outcomes on Linux, macOS and Windows.
onPlatform :: Outcome -> Outcome -> Outcome -> Outcome
platformName :: String
#if defined(CABAL_OS_LINUX)
onPlatform linux _ _ = linux
platformName = "Linux"
#elif defined(CABAL_OS_DARWIN)
onPlatform _ darwin _ = darwin
platformName = "macOS"
#else
onPlatform _ _ windows = windows
platformName = "Windows"
#endif

createFile :: FilePath -> IO ()
createFile path = writeFile path ""

absPath :: Event -> FilePath
absPath = Path.toString . Event.getAbsPath

isNamed :: String -> Event -> Bool
isNamed name ev = takeFileName (absPath ev) == name

createdNamed :: String -> [Event] -> Bool
createdNamed name = any (\ev -> isNamed name ev && Event.isCreated ev)

-- | Whether a path whose name starts with "file" has two 'Event.isCreated'
-- events.
fileCreatedTwice :: [Event] -> Bool
fileCreatedTwice =
    any ((> 1) . length) . group . sort . map absPath . filter isFileCreate

    where

    isFileCreate ev =
        Event.isCreated ev && "file" `isPrefixOf` takeFileName (absPath ev)

-- | Remove the permission to list a directory, keep the permission to create
-- a file in it.
denyRead :: FilePath -> IO ()
-- | Undo 'denyRead'.
allowRead :: FilePath -> IO ()
-- | @hardLink source link@
hardLink :: FilePath -> FilePath -> IO ()
#if defined(CABAL_OS_WINDOWS)
denyRead path = void $ readProcess "icacls" [path, "/deny", everyone] ""

    where

    everyone = "*S-1-1-0:(OI)(CI)(RX)"

allowRead path =
    void $ readProcess "icacls" [path, "/remove:d", "*S-1-1-0"] ""

hardLink = createHardLink
#else
denyRead path =
    setPermissions path
        $ setOwnerWritable True $ setOwnerSearchable True emptyPermissions

allowRead path = do
    perms <- getPermissions path
    setPermissions path (setOwnerReadable True perms)

hardLink source link = void $ readProcess "ln" [source, link] ""
#endif

-- | Change the metadata of a file, without changing its contents.
changeMetadata :: FilePath -> IO ()
changeMetadata path = do
    perms <- getPermissions path
    setPermissions path (setOwnerWritable False perms)

-- | The setting for the metadata change events, if the platform has one.
setAttrs :: Bool -> Config -> Config
-- | Whether the event reports a metadata change.
metadataChanged :: Event -> Bool
#if defined(CABAL_OS_LINUX)
setAttrs = Linux.setAttrsModified
metadataChanged = Linux.isAttrsModified
#elif defined(CABAL_OS_DARWIN)
setAttrs _ = id
metadataChanged ev = Darwin.isSecurityModified ev || Darwin.isAttrsModified ev
#else
setAttrs = Windows.setAttrsModified
metadataChanged = Event.isModified
#endif

base :: Behavior
base =
    Behavior
        { title = ""
        , config = id
        , setup = const (return ())
        , cleanup = const (return ())
        , skip = const (return Nothing)
        , paused = False
        , ops = const (return ())
        , laterOps = const (return ())
        , check = \_ _ -> False
        , expected = Exception
        , racy = False
        }

behaviors :: [Behavior]
behaviors =
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
    , base
        { title = "file deleted after its create is read reports isCreated"
        , ops = \p -> do
            let file = watchRoot p </> "file"
            createFile file
            settle
            removeFile file
        , check = const fileCreatedTwice
        , expected = onPlatform No Yes No
        }
    , base
        { title = "deleted root reports isDeleted for the root"
        , ops = removePathForcibly . watchRoot
        , check = \p ->
            any (\ev -> absPath ev == watchRoot p && Event.isDeleted ev)
        , expected = onPlatform No Yes Exception
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
        , expected = onPlatform No Yes Exception
        }
    , base
        { title = "renamed root is watched"
        , ops = renameRootAndCreate
        , check = const (createdNamed "file")
        , expected = onPlatform No No Exception
        }
    , base
        { title = "events of a renamed root use the old path"
        , ops = renameRootAndCreate
        , check = \p -> any ((== watchRoot p </> "file") . absPath)
        , expected = onPlatform No No Exception
        }
    , base
        { title = "dir created in a renamed recursive root is watched"
        , config = Event.setRecursiveMode True
        , ops = \p -> do
            renameDirectory (watchRoot p) (renamedRoot p)
            settle
            createDirectory (renamedRoot p </> "dir")
            settle
            createFile (renamedRoot p </> "dir" </> "file")
        , check = const (createdNamed "file")
        , expected = onPlatform No No Exception
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
        , racy = notLinux
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
        , racy = notLinux
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
    , base
        { title = "root without read permission is watched"
        , setup = denyRead . watchRoot
        , cleanup = allowRead . watchRoot
        , skip = rootStillReadable
        , ops = \p -> createFile (watchRoot p </> "file")
        , check = const (createdNamed "file")
        , expected = onPlatform WatchException Yes WatchException
        }
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
        , racy = onWindows
        }
    , base
        { title = "deleted symbolic link root reports isDeleted for the root"
        , setup = \p -> do
            removeDirectory (watchRoot p)
            createDirectory (renamedRoot p)
            createDirectoryLink (renamedRoot p) (watchRoot p)
            settle
        , ops = removeDirectoryLink . watchRoot
        , check = \p ->
            any (\ev -> absPath ev == watchRoot p && Event.isDeleted ev)
        , expected = onPlatform No No No
        }
    , base
        { title = "non-recursive watch reports a file created in a subdir"
        , setup = withDir
        , ops = \p -> createFile (watchRoot p </> "dir/file")
        , check = const (createdNamed "file")
        , expected = onPlatform No No No
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
        , racy = onWindows
        }
#if defined(CABAL_OS_LINUX)
    , deletedOpenFile
        { title = "write to a deleted file, setExcludeUnlinked False"
        , config = Linux.setExcludeUnlinked False
        , expected = Yes
        }
#endif
    , metadataChange
        { title = "metadata change is reported by default"
        , expected = onPlatform Yes Yes No
        }
    , metadataChange
        { title = "metadata change is reported with the setting disabled"
        , config = setAttrs False
        , expected = onPlatform No Yes No
        }
    , metadataChange
        { title = "metadata change is reported with the setting enabled"
        , config = setAttrs True
        , expected = onPlatform Yes Yes Yes
        }
    , hardLinkCreate
        { title = "creating a hard link reports isCreated"
        , check = const (createdNamed "link")
        , expected = onPlatform Yes Yes Yes
        }
    , hardLinkCreate
        { title = "creating a hard link reports a metadata change on the dir"
        , check = \p ->
            any (\ev -> absPath ev == watchRoot p && metadataChanged ev)
        , expected = onPlatform No Yes No
        }
    , base
        { title = "deleting a hard link reports isDeleted"
        , setup = \p -> do
            createFile (watchRoot p </> "file")
            hardLink (watchRoot p </> "file") (watchRoot p </> "link")
            settle
        , ops = \p -> removeFile (watchRoot p </> "link")
        , check = \_ ->
            any (\ev -> isNamed "link" ev && Event.isDeleted ev)
        , expected = onPlatform Yes Yes Yes
        }
    -- On Linux, the kernel merges the second write into the first because the
    -- first is the last event in the queue and it has not been read.
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
        , racy = notLinux
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
        , racy = notLinux
        }
    ]

    where

    onWindows = platformName == "Windows"

    notLinux = platformName /= "Linux"

    withDir p = do
        createDirectory (watchRoot p </> "dir")
        settle

    withFile p = do
        createFile (watchRoot p </> "file")
        settle

    rootStillReadable p = do
        r <- try (listDirectory (watchRoot p))
        return $ case r of
            Left (_ :: IOException) -> Nothing
            Right _ -> Just "the permission was not removed"

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
            { setup = withFile
            , ops = \p -> changeMetadata (watchRoot p </> "file")
            , check = \_ ->
                any (\ev -> isNamed "file" ev && metadataChanged ev)
            }

    hardLinkCreate =
        base
            { setup = withFile
            , ops = \p ->
                hardLink (watchRoot p </> "file") (watchRoot p </> "link")
            }

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

-- | Watch the root and the control root, run the operations of the behavior
-- and check the events.
runWatch :: Behavior -> Paths -> FilePath -> IO Outcome
runWatch Behavior{..} paths control = do
    let root = watchRoot paths
    started <- newEmptyMVar
    blocked <- newEmptyMVar
    resume <- newEmptyMVar
    result <- newEmptyMVar

    -- The control root stays in place when a behavior changes the watch
    -- root. Its stop file ends the stream in all cases.
    let observe ev = do
            when ("started" `isPrefixOf` takeFileName (absPath ev))
                $ void $ tryPutMVar started ()
            when (isNamed "blocked" ev) $ do
                void $ tryPutMVar blocked ()
                readMVar resume
            return ev
        consumer = do
            rootPath <- Path.fromString root
            controlPath <- Path.fromString control
            Event.watchWith config (rootPath :| [controlPath])
                & Stream.mapM observe
                & Stream.takeWhile (not . isNamed "stop")
                & Stream.fold Fold.toList
        runOps = do
            when paused $ do
                createFile (root </> "blocked")
                readMVar blocked
            r <- tryIO (ops paths)
            putMVar resume ()
            settle
            r1 <- if isLeft r then return r else tryIO (laterOps paths)
            settle
            return r1

    void $ forkIO (try consumer >>= putMVar result)
    waitStarted result control started

    ended <- not <$> isEmptyMVar result
    r1 <- if ended then return (Right ()) else runOps
    createFile (control </> "stop")

    evs <- timeout 30000000 (takeMVar result)
    return $ case evs of
        Nothing -> error "the stream did not end"
        Just (Left e)
            | isIOException e -> WatchException
            | otherwise -> error (show e)
        Just (Right xs)
            | isLeft r1 -> Exception
            | check paths xs -> Yes
            | otherwise -> No

    where

    tryIO :: IO () -> IO (Either IOException ())
    tryIO = try

    isIOException :: SomeException -> Bool
    isIOException e = isJust (fromException e :: Maybe IOException)

runBehavior :: Behavior -> IO Outcome
runBehavior b =
    withSystemTempDirectory "fsevent_behavior" $ \dir -> do
        let root = dir </> "root"
            control = dir </> "control"
            paths = Paths {watchRoot = root, renamedRoot = dir </> "renamed"}
        createDirectory root
        createDirectory control
        setup b paths
        flip finally (cleanup b paths) $ do
            reason <- skip b paths
            maybe (runWatch b paths control) (return . Skipped) reason

-------------------------------------------------------------------------------
-- Overflow stress test
-------------------------------------------------------------------------------

-- | Enable the events generated by listing a directory, on the platforms that
-- have them.
touchEvents :: Config -> Config
#if defined(CABAL_OS_LINUX)
touchEvents =
      Linux.setOpened True
    . Linux.setAccessed True
    . Linux.setNonWriteClosed True
#else
touchEvents = id
#endif

-- | A file system scenario that overflows the event queue. The result is the
-- number of 'Event.isEventsLost' events seen 8 seconds and 10 seconds after
-- the scenario has run.
data Stress = Stress
    { behavior :: Behavior
    -- | Use more directories than the queue can hold the events of.
    , largeTree :: Bool
    -- | Create more files than the queue can hold while the consumer is
    -- blocked. Otherwise only the watch itself generates events.
    , burst :: Bool
    , judge :: Int -> Int -> Outcome
    }

-- | The number of directories in a large tree, in a small tree, and the number
-- of files in a burst. On Linux, listing a directory with the open, access and
-- close events enabled generates 8 events. The large tree generates more
-- events than the queue holds if it is listed that way, the small tree
-- generates fewer. 'Nothing' if the limits of the system do not allow this.
overflowSizes :: IO (Maybe (Int, Int, Int))
#if defined(CABAL_OS_LINUX)
overflowSizes = do
    queued <- limit "max_queued_events"
    watches <- limit "max_user_watches"
    let large = queued `div` 8 + 500
    return
        $ if queued > 100000 || large + 1000 > watches
          then Nothing
          else Just (large, queued `div` 8 - 500, queued + 1000)

    where

    limit name = read <$> readFile ("/proc/sys/fs/inotify/" ++ name)
#else
overflowSizes = return (Just (6000, 6000, 20000))
#endif

noOverflow :: Outcome
noOverflow
    | platformName == "Linux" = Failed "no overflow occurred"
    | otherwise = Skipped "no overflow occurred"

-- | 'Yes' if the 'Event.isEventsLost' events have stopped.
judgeStopped :: Int -> Int -> Outcome
judgeStopped n1 n2
    | n2 == 0 = noOverflow
    | n2 > n1 = No
    | otherwise = Yes

-- | 'Yes' if there is one 'Event.isEventsLost' event, 'No' if there are none.
judgeOnce :: Int -> Int -> Outcome
judgeOnce n1 n2
    | n2 == 0 = No
    | n1 == 1 && n2 == 1 = Yes
    | otherwise = Failed ("isEventsLost events: " ++ show (n1, n2))

runOverflow :: Stress -> IO Outcome
runOverflow Stress{..}
    -- XXX The overflow event is parsed from an uninitialized buffer on this
    -- branch. Run it when the fix on the windows-robustness branch is merged.
    | platformName == "Windows" =
        return (Skipped "enable when windows-robustness is merged")
    | otherwise = overflowSizes >>= maybe tooLow run

    where

    tooLow = return (Skipped "the inotify limits do not allow the test")

    observe :: MVar () -> MVar () -> MVar () -> IORef Int -> Event -> IO ()
    observe started blocked resume lost ev = do
        when ("started" `isPrefixOf` takeFileName (absPath ev))
            $ void $ tryPutMVar started ()
        when (isNamed "blocked" ev) $ do
            void $ tryPutMVar blocked ()
            readMVar resume
        when (Event.isEventsLost ev) $ modifyIORef' lost (+ 1)

    run (large, small, files) =
        withSystemTempDirectory "fsevent_overflow" $ \dir -> do
        let root = dir </> "root"
            dirs = if largeTree then large else small
        createDirectory root
        forM_ [1 .. dirs] $ \i -> createDirectory (root </> ("d" ++ show i))

        started <- newEmptyMVar
        blocked <- newEmptyMVar
        resume <- newEmptyMVar
        finished <- newEmptyMVar
        lost <- newIORef 0

        let consumer = do
                rootPath <- Path.fromString root
                Event.watchWith (config behavior) (rootPath :| [])
                    & Stream.fold
                        (Fold.drainMapM (observe started blocked resume lost))
        tid <- forkIO (try consumer >>= putMVar finished)
        waitStarted finished root started

        when burst $ do
            createFile (root </> "blocked")
            readMVar blocked
            forM_ [1 .. files] $ \i -> createFile (root </> ("f" ++ show i))
            putMVar resume ()

        threadDelay 8000000
        n1 <- readIORef lost
        threadDelay 2000000
        n2 <- readIORef lost
        r <- tryReadMVar finished
        killThread tid
        _ <- takeMVar finished
        putStrLn $ "  isEventsLost events: " ++ show n1 ++ " at 8s, "
            ++ show n2 ++ " at 10s"
        return $ case r of
            Just (Left e) -> Failed (show (e :: SomeException))
            Just (Right ()) -> Failed "the stream ended"
            Nothing -> judge n1 n2

-- | The outcome is not asserted on macOS and Windows. The library has no
-- restart there, the result is recorded from the CI logs.
stressBase :: Behavior
stressBase =
    base
        { config = Event.setRecursiveMode True
        , expected = Yes
        , racy = platformName /= "Linux"
        }

stressCases :: [Stress]
stressCases =
    [ Stress
        { behavior = stressBase
            { title = "the scan at the start of a watch overflows the queue"
            , config = config stressBase . touchEvents
            , expected = No
            }
        , largeTree = True
        , burst = False
        , judge = judgeOnce
        }
    , Stress
        { behavior = stressBase
            {title = "isEventsLost events stop after a burst of events"}
        , largeTree = True
        , burst = True
        , judge = judgeStopped
        }
#if defined(CABAL_OS_LINUX)
    , Stress
        { behavior = stressBase
            { title = "restart: events stop after a burst in a large tree"
            , config = config stressBase . touchEvents
            }
        , largeTree = True
        , burst = True
        , judge = judgeStopped
        }
    , Stress
        { behavior = stressBase
            { title = "restart: events stop after a burst in a small tree"
            , config = config stressBase . touchEvents
            }
        , largeTree = False
        , burst = True
        , judge = judgeStopped
        }
    , Stress
        { behavior = stressBase
            { title = "no restart: events stop after a burst"
            , config = noRestart
            }
        , largeTree = True
        , burst = True
        , judge = judgeStopped
        }
#endif
    ]
#if defined(CABAL_OS_LINUX)

    where

    noRestart = Linux.setRestartOnOverflow False . config stressBase
#endif

-- | Any failure to determine the outcome is an outcome.
outcomeWithin :: Int -> IO Outcome -> IO Outcome
outcomeWithin seconds action = do
    r <- try (timeout (seconds * 1000000) (action >>= evaluate))
    return $ case r of
        Right (Just outcome) -> outcome
        Right Nothing -> Failed "timed out"
        Left e -> Failed (show (e :: SomeException))

main :: IO ()
main = do
    resultsRef <- newIORef []
    let spec seconds b runner = it (title b) $ do
            outcome <- outcomeWithin seconds runner
            modifyIORef' resultsRef ((b, outcome) :)
            case outcome of
                Skipped reason -> pendingWith reason
                _ -> unless (racy b) $ outcome `shouldBe` expected b
    summary <- hspecResult $ describe moduleName $ do
        mapM_ (\b -> spec 60 b (runBehavior b)) behaviors
        mapM_ (\t -> spec 300 (behavior t) (runOverflow t)) stressCases

    results <- reverse <$> readIORef resultsRef
    putStrLn ""
    putStrLn $ "Behavior results on " ++ platformName ++ ":"
    forM_ results $ \(b, outcome) ->
        putStrLn $ "  " ++ title b ++ ": " ++ show outcome
            ++ if outcome == expected b
               then ""
               else " (expected " ++ show (expected b) ++ ")"

    when (summaryFailures summary > 0) exitFailure
