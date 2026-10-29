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
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (forM_, void)
import Data.List.NonEmpty (NonEmpty(..))
import System.Directory (createDirectory, makeAbsolute, renameDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Mem (performMajorGC)
import System.Timeout (timeout)
import System.Win32.File
    ( closeHandle
    , createFile
    , fILE_FLAG_BACKUP_SEMANTICS
    , fILE_LIST_DIRECTORY
    , fILE_SHARE_NONE
    , oPEN_EXISTING
    )
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

-- | Run the action every 100 ms until it succeeds, at most @n@ times. Raise
-- the exception of the last attempt if all the attempts fail.
retrying :: Int -> IO () -> IO ()
retrying n act = do
    r <- try act
    case r of
        Right () -> return ()
        Left (e :: SomeException) ->
            if n <= 1
            then throwIO e
            else threadDelay 100000 >> retrying (n - 1) act

-- | Open the directory with no sharing. Raises an exception if any other
-- handle with data access to the directory is open.
openExclusive :: FilePath -> IO ()
openExclusive dir = do
    h <- createFile dir
        fILE_LIST_DIRECTORY
        fILE_SHARE_NONE
        Nothing
        oPEN_EXISTING
        fILE_FLAG_BACKUP_SEMANTICS
        Nothing
    closeHandle h

-- | Run the watch until the first event and abandon the stream. The directory
-- handle of the watch must be closed after the stream is garbage collected.
handleClosedOnGC :: Spec
handleClosedOnGC =
    it "closes the directory handle after the stream is abandoned" $
        withRoot $ \root -> do
            p <- Path.fromString root
            void $ forkIO $ do
                threadDelay 500000
                writeFile (root </> "file1") ""
            r <- Stream.fold Fold.one (Event.watch (p :| []))
            fmap (const ()) r `shouldBe` Just ()
            performMajorGC
            -- A worker blocked in a read is killed only after the read
            -- returns, the next event makes the read return.
            writeFile (root </> "file2") ""
            retrying 50 (performMajorGC >> openExclusive root)

-- | Rename the watch root, then create a file in it. The watch reports the
-- event with the original root path.
rootRenamed :: TestDesc
rootRenamed =
    ( "watch root renamed while watched"
    , const (return ())
    , \fp -> do
        renameDirectory fp (fp ++ "-moved")
        writeFile (fp ++ "-moved" </> "file") ""
    , [("file", Event.isCreated)]
    )

-- | Block the consumer at the first event while creating enough files to
-- overflow both the stream buffer (1500 events) and the event buffer of the
-- system. The watch must report isEventsLost instead of raising an exception
-- or parsing an empty buffer.
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

robustnessTests :: Spec
robustnessTests =
    describe moduleName $ describe "robustness" $ do
        handleClosedOnGC
        eventsLost

-- TODO Test isModified event for parent directories when a file is created or
-- deleted.

main :: IO ()
main = do
    let regularRootTests =
            rootRenamed : reservedName : commonTests
        -- The following watch-root deletion test failed with a sharing
        -- violation when the directory handle was opened without
        -- FILE_SHARE_DELETE:
        -- C:\tmp\fsevent_dir-e061f0b0a00e1696\watch-root:
        -- removePathForcibly:RemoveDirectory
        -- "\\\\?\\C:\\tmp\\fsevent_dir-e061f0b0a00e1696\\watch-root":
        -- permission denied (The process cannot access the file because it is
        -- being used by another process.)
        -- ReadDirectoryChangesW does not report changes to the watched
        -- directory itself, so no isDeleted event is expected.
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

    hspec robustnessTests
