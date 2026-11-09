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
    ( MVar, forkIO, isEmptyMVar, newEmptyMVar, putMVar, readMVar
    , takeMVar, threadDelay, tryPutMVar, tryReadMVar
    )
import Control.Exception
    (IOException, SomeException, evaluate, finally, fromException, try)
import Control.Monad (forM_, unless, void, when)
import Data.Either (isLeft)
import Data.Function ((&))
import Data.IORef (modifyIORef', newIORef, readIORef)
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
    -- On Linux the rename of a root is reported by 'Linux.isRootMoved' only.
    , base
        { title = "renamed root reports isMoved for the root"
        , ops = \p -> renameDirectory (watchRoot p) (renamedRoot p)
        , check = \p ->
            let isRoot ev = absPath ev `elem` [watchRoot p, renamedRoot p]
             in any (\ev -> isRoot ev && Event.isMoved ev)
        , expected = onPlatform No Yes Exception
        , racy = onDarwin
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
        { title = "recursive root with an unreadable subdir is watched"
        , config = Event.setRecursiveMode True
        , setup = \p -> do
            createDirectory (deniedDir p)
            denyRead (deniedDir p)
        , cleanup = allowRead . deniedDir
        , skip = stillReadable . deniedDir
        , ops = \p -> createFile (watchRoot p </> "file")
        , check = const (createdNamed "file")
        , expected = onPlatform Yes Yes Yes
        , racy = notLinux
        }
    , base
        { title = "unreadable dir moved into a recursive root is skipped"
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
        , racy = notLinux
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
        , racy = notLinux
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

    onDarwin = platformName == "macOS"

    withDir p = do
        createDirectory (watchRoot p </> "dir")
        settle

    withFile p = do
        createFile (watchRoot p </> "file")
        settle

    rootStillReadable = stillReadable . watchRoot

    stillReadable dir = do
        r <- try (listDirectory dir)
        return $ case r of
            Left (_ :: IOException) -> Nothing
            Right _ -> Just "the permission was not removed"

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

    results <- reverse <$> readIORef resultsRef
    putStrLn ""
    putStrLn $ "Behavior results on " ++ platformName ++ ":"
    forM_ results $ \(b, outcome) ->
        putStrLn $ "  " ++ title b ++ ": " ++ show outcome
            ++ if outcome == expected b
               then ""
               else " (expected " ++ show (expected b) ++ ")"

    when (summaryFailures summary > 0) exitFailure
