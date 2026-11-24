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
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM_, void)
import Data.List.NonEmpty (NonEmpty(..))
import System.Directory (createDirectory, makeAbsolute)
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
-- Robustness tests
-------------------------------------------------------------------------------

withRoot :: (FilePath -> IO a) -> IO a
withRoot act =
    withSystemTempDirectory "fsevent_dir" $ \fp -> do
        let root = fp </> "watch-root"
        createDirectory root
        act root

-- | Block the consumer at the first event while creating enough files to
-- overflow both the event queue of the watch (1024 events) and the event
-- buffer of the system. The watch must report isEventsLost instead of
-- raising an exception or parsing an empty buffer.
eventsLost :: Spec
eventsLost =
    it "reports isEventsLost when the event buffer overflows" $
        withRoot $ \root -> do
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
                    fmap (const ()) (Event.getRelPath ev) `shouldBe` Nothing
                    Path.toString (Event.getRoot ev) `shouldBe` root
                    Path.toString (Event.getAbsPath ev) `shouldBe` root

-- | "aux.c" is a reserved device name that Path.fromArray rejects. A \\?\ path
-- bypasses the device name check of Windows and creates the file.
reservedName :: TestDesc
reservedName =
    ( "file with a reserved name created (aux.c)"
    , const (return ())
    , \fp -> do
        dir <- makeAbsolute fp
        writeFile ("\\\\?\\" ++ dir </> "aux.c") ""
    , [("aux.c", Event.isCreated)]
    )

-- | When the open of one root fails, the watch closes the other roots, whose
-- reads are in progress with no changes occurring. The exception must be
-- raised without waiting for a change in the other roots.
missingRoot :: Spec
missingRoot =
    it "raises an exception for a missing root while the others are idle" $
        withRoot $ \root -> do
            let dirs = fmap (\i -> root </> ("dir" ++ show i)) [1 .. 3 :: Int]
            mapM_ createDirectory dirs
            paths <- mapM Path.fromString (dirs ++ [root </> "missing"])
            let watcher =
                    case paths of
                        p : ps -> Event.watch (p :| ps)
                        [] -> error "missingRoot: no paths"
            -- try must be outside timeout, it would catch the exception that
            -- timeout throws.
            r <- try $ timeout 10000000 $ Stream.drain watcher
            case r of
                Left (_ :: SomeException) -> return ()
                Right Nothing -> expectationFailure "timed out"
                Right (Just ()) -> expectationFailure "the stream ended"

-- | NTFS does not validate the UTF-16 in names. showEvent must not raise an
-- exception for a name with an unpaired surrogate.
unpairedSurrogate :: Spec
unpairedSurrogate =
    it "showEvent accepts a name with an unpaired surrogate" $
        withRoot $ \root -> do
            p <- Path.fromString root
            void $ forkIO $ do
                threadDelay 500000
                writeFile (root </> "a\xD800") ""
            r <- Stream.fold Fold.one (Event.watch (p :| []))
            case r of
                Nothing -> expectationFailure "the stream ended"
                Just ev -> do
                    n <- evaluate (length (Event.showEvent ev))
                    n `shouldSatisfy` (> 0)

robustnessTests :: Spec
robustnessTests =
    describe moduleName $ describe "robustness" $ do
        eventsLost
        missingRoot
        unpairedSurrogate
        -- "a renamed root ends with isRootStale at its next change": tested
        -- in FileSystem.Event.Behavior, "A renamed watch root remains
        -- watched" and "A renamed watch root reports `isRootStale` at its
        -- next event".
        --
        -- "a renamed root stays watched without the stale root check":
        -- tested in FileSystem.Event.Behavior, "A renamed watch root remains
        -- watched without the stale root check".

-- TODO Test isModified event for parent directories when a file is created or
-- deleted.

main :: IO ()
main = do
    let regularRootTests =
            reservedName : commonTests
        -- The following watch-root deletion test failed with a sharing
        -- violation when the directory handle was opened without
        -- FILE_SHARE_DELETE:
        -- C:\tmp\fsevent_dir-e061f0b0a00e1696\watch-root:
        -- removePathForcibly:RemoveDirectory
        -- "\\\\?\\C:\\tmp\\fsevent_dir-e061f0b0a00e1696\\watch-root":
        -- permission denied (The process cannot access the file because it is
        -- being used by another process.)
        -- ReadDirectoryChangesW does not report changes to the watched
        -- directory itself, so no isDeleted event is expected. macOS
        -- reports isDeleted for a deleted root; Linux reports isDeleted
        -- for each entry in the root and then isRootDeleted.
        --
        -- ++ dirDelete "" (\dir -> [(dir, Event.isDeleted)])

    let w = failNested (Event.watchWith (Event.setAllEvents True))
        run = runTests moduleName "non-recursive" w

    run DirType (nestedOpsTest : regularRootTests)
    run SymLinkOrigPath (nestedOpsTest : commonTests)

    let recw = Event.watchWith
                (Event.setAllEvents True . Event.setRecursiveMode True)
        runRec = runTests moduleName "recursive" recw

    runRec DirType (regularRootTests ++ commonRecTests)
    runRec SymLinkOrigPath (commonTests ++ commonRecTests)

    hspec robustnessTests
