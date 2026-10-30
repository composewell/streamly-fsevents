-- |
-- Module      : Streamly.Test.FileSystem.Event.Windows
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Test.FileSystem.Event.Windows (main) where

import Control.Concurrent
    (forkIO, killThread, newEmptyMVar, putMVar, readMVar, threadDelay)
import Control.Monad (forM_)
import Data.List.NonEmpty (NonEmpty(..))
import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Hspec

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.FileSystem.Path as Path
import qualified Streamly.Internal.FS.Event.Windows as Event

import Streamly.Test.FileSystem.Event.Common

moduleName :: String
moduleName = "FS.Event.Windows"

-------------------------------------------------------------------------------
-- Event path tests
-------------------------------------------------------------------------------

-- | Block the consumer at the first event while creating enough files to
-- overflow both the stream buffer (1500 events) and the event buffer of the
-- system.
--
-- XXX On this branch the overflow event is parsed from an uninitialized
-- buffer. Enable this test when the fix on the windows-robustness branch is
-- merged. That branch has the same test as "eventsLost", keep one of them.
eventsLostPaths :: Spec
eventsLostPaths =
    it "isEventsLost has getRelPath Nothing and getAbsPath getRoot" $ do
        pendingWith "enable when the windows-robustness branch is merged"
        withSystemTempDirectory "fsevent_dir" $ \fp -> do
            let root = fp </> "watch-root"
            createDirectory root
            p <- Path.fromString root
            created <- newEmptyMVar
            let create name = writeFile (root </> name) ""
                longName i = replicate 100 'x' ++ show (i :: Int)
                producer = do
                    threadDelay 500000
                    create "first"
                    mapM_ (create . longName) [1 .. 4000]
                    putMVar created ()
                    -- The overflow is reported when a read returns, keep
                    -- generating events to make the reads return.
                    forM_ [1 :: Int ..] $ \i -> do
                        threadDelay 100000
                        create ("sentinel" ++ show i)
            tid <- forkIO producer
            r <- timeout 120000000
                    $ Stream.fold (Fold.find Event.isEventsLost)
                    $ Stream.mapM (\ev -> readMVar created >> return ev)
                    $ Event.watch (p :| [])
            killThread tid
            case r of
                Nothing -> expectationFailure "timed out"
                Just Nothing -> expectationFailure "the stream ended"
                Just (Just ev) -> do
                    fmap Path.toString (Event.getRelPath ev)
                        `shouldBe` Nothing
                    Path.toString (Event.getRoot ev) `shouldBe` root
                    Path.toString (Event.getAbsPath ev) `shouldBe` root

eventPathTests :: IO ()
eventPathTests =
    hspec $ describe moduleName $ describe "event paths" eventsLostPaths

-- TODO Test isModified event for parent directories when a file is created or
-- deleted.

main :: IO ()
main = do
    let regularRootTests =
            commonTests
        -- The following watch-root deletion test results in:
        -- C:\tmp\fsevent_dir-e061f0b0a00e1696\watch-root:
        -- removePathForcibly:RemoveDirectory
        -- "\\\\?\\C:\\tmp\\fsevent_dir-e061f0b0a00e1696\\watch-root":
        -- permission denied (The process cannot access the file because it is
        -- being used by another process.)
        --
        -- ++ dirDelete "" (\dir -> [(dir, Event.isDeleted)])

    let w = failNested (Event.watchWith (Event.setAllEvents True))
        run = runTests moduleName "non-recursive" w

    run DirType (nestedOpsTest : regularRootTests)
    run SymLinkOrigPath (nestedOpsTest : commonTests)

    let recTests =
            [ dirCreateWithParent "subdir\\dir"
                (\dir -> [(dir, dirEvent Event.isCreated)])
            , fileCreateWithParent "subdir\\file"
                (\file -> [(file, fileEvent Event.isCreated)])
            ] ++ commonRecTests

    let recw = Event.watchWith
                (Event.setAllEvents True . Event.setRecursiveMode True)
        runRec = runTests moduleName "recursive" recw

    runRec DirType (regularRootTests ++ recTests)
    runRec SymLinkOrigPath (commonTests ++ recTests)

    eventPathTests
