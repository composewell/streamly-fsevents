-- |
-- Module      : Streamly.Test.FileSystem.Event.Windows
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Test.FileSystem.Event.Windows (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (void)
import Data.List.NonEmpty (NonEmpty(..))
import System.Directory (createDirectory, renameDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Mem (performMajorGC)
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

robustnessTests :: Spec
robustnessTests =
    describe moduleName $ describe "robustness" $ do
        handleClosedOnGC

-- TODO Test isModified event for parent directories when a file is created or
-- deleted.

main :: IO ()
main = do
    let regularRootTests =
            rootRenamed : commonTests
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
