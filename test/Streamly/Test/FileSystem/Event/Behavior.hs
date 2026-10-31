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
-- including when an assertion fails. The table in the documentation of
-- "Streamly.FileSystem.Event" records the results printed on each platform in
-- the CI logs.

module Streamly.Test.FileSystem.Event.Behavior (main) where

import Control.Concurrent
    ( MVar, forkIO, newEmptyMVar, putMVar, readMVar, takeMVar, threadDelay
    , tryPutMVar, tryReadMVar
    )
import Control.Exception (IOException, SomeException, evaluate, try)
import Control.Monad (forM_, void, when)
import Data.Either (isLeft)
import Data.Function ((&))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf)
import Data.List.NonEmpty (NonEmpty((:|)))
import System.Exit (exitFailure)
import System.Directory
    ( createDirectory, createDirectoryIfMissing, removeFile
    , removePathForcibly, renameDirectory
    )
import System.FilePath ((</>), takeFileName)
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Streamly.Internal.FS.Event (Config, Event)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.FS.Event as Event
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
    | Refused -- ^ The OS refused the file system operation.
    | Failed String -- ^ The test could not determine the outcome.
    deriving (Eq, Show)

-- | The paths used by a behavior.
data Paths = Paths
    { watchRoot :: FilePath
    , renamedRoot :: FilePath
    }

data Behavior = Behavior
    { title :: String
    , config :: Config -> Config
    -- | Run 'ops' while the consumer of the stream is not reading it.
    , paused :: Bool
    , ops :: Paths -> IO ()
    -- | Run after the consumer has processed the events of 'ops'.
    , laterOps :: Paths -> IO ()
    , check :: Paths -> [Event] -> Bool
    , expected :: Outcome
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
        { title = "deleted root reports isDeleted for the root"
        , ops = removePathForcibly . watchRoot
        , check = \p ->
            any (\ev -> absPath ev == watchRoot p && Event.isDeleted ev)
        , expected = onPlatform No Yes Refused
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
        , expected = onPlatform No Yes Refused
        }
    , base
        { title = "renamed root is watched"
        , ops = renameRootAndCreate
        , check = const (createdNamed "file")
        , expected = onPlatform Yes No Refused
        }
    , base
        { title = "events of a renamed root use the old path"
        , ops = renameRootAndCreate
        , check = \p -> any ((== watchRoot p </> "file") . absPath)
        , expected = onPlatform Yes No Refused
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
        , expected = onPlatform No No Refused
        }
    , newDir
        { title = "file created in a new dir before it is read is reported"
        , check = const (createdNamed "file")
        , expected = onPlatform No Yes Yes
        }
    , newDir
        { title = "subdir created in a new dir before it is read is watched"
        , laterOps = \p -> createFile (watchRoot p </> "dir/sub/later")
        , check = const (createdNamed "later")
        , expected = onPlatform Yes Yes Yes
        }
    ]

    where

    base =
        Behavior
            { title = ""
            , config = id
            , paused = False
            , ops = const (return ())
            , laterOps = const (return ())
            , check = \_ _ -> False
            , expected = Refused
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
-- The watch has started when it is read.
waitStarted :: FilePath -> MVar () -> IO ()
waitStarted control started = go (0 :: Int)

    where

    go n = do
        when (n >= 100) $ error "waitStarted: the watch did not start"
        createFile (control </> ("started" ++ show n))
        threadDelay 100000
        r <- tryReadMVar started
        maybe (go (n + 1)) return r

runBehavior :: Behavior -> IO Outcome
runBehavior Behavior{..} =
    withSystemTempDirectory "fsevent_behavior" $ \dir -> do
        let root = dir </> "root"
            control = dir </> "control"
            paths = Paths {watchRoot = root, renamedRoot = dir </> "renamed"}
        createDirectory root
        createDirectory control

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

        void $ forkIO (try consumer >>= putMVar result)
        waitStarted control started

        when paused $ do
            createFile (root </> "blocked")
            readMVar blocked
        r <- tryIO (ops paths)
        putMVar resume ()
        settle
        r1 <- if isLeft r then return r else tryIO (laterOps paths)
        settle
        createFile (control </> "stop")

        evs <- timeout 30000000 (takeMVar result)
        return $ case evs of
            Nothing -> error "the stream did not end"
            Just (Left e) -> error (show (e :: SomeException))
            Just (Right xs)
                | isLeft r1 -> Refused
                | check paths xs -> Yes
                | otherwise -> No

    where

    tryIO :: IO () -> IO (Either IOException ())
    tryIO = try

-- | Like 'runBehavior' but any failure to determine the outcome is an outcome.
behaviorOutcome :: Behavior -> IO Outcome
behaviorOutcome b = do
    r <- try (timeout 60000000 (runBehavior b >>= evaluate))
    return $ case r of
        Right (Just outcome) -> outcome
        Right Nothing -> Failed "timed out"
        Left e -> Failed (show (e :: SomeException))

main :: IO ()
main = do
    resultsRef <- newIORef []
    let spec b = it (title b) $ do
            outcome <- behaviorOutcome b
            modifyIORef' resultsRef ((b, outcome) :)
            outcome `shouldBe` expected b
    summary <- hspecResult $ describe moduleName $ mapM_ spec behaviors

    results <- reverse <$> readIORef resultsRef
    putStrLn ""
    putStrLn $ "Behavior results on " ++ platformName ++ ":"
    forM_ results $ \(b, outcome) ->
        putStrLn $ "  " ++ title b ++ ": " ++ show outcome
            ++ if outcome == expected b
               then ""
               else " (expected " ++ show (expected b) ++ ")"

    when (summaryFailures summary > 0) exitFailure
