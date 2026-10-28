-- |
-- Module      : Streamly.Test.FileSystem.Event.Darwin
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Test.FileSystem.Event.Darwin (main) where

import Data.List.NonEmpty (NonEmpty)
import Streamly.Data.Stream (Stream)
import Streamly.Internal.FileSystem.Path (Path)
import Streamly.Internal.FS.Event.Darwin (Event)
import System.Directory (createDirectory)
import System.FilePath
    ((</>), dropTrailingPathSeparator, takeDirectory, takeFileName)

import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.FileSystem.Path as Path
import qualified Streamly.Internal.FS.Event.Darwin as Event

import Streamly.Test.FileSystem.Event.Common

moduleName :: String
moduleName = "FS.Event.Darwin"

-- | Raise an exception if the watch reports an event for a path in a
-- directory named @nested@.
failNested ::
    (NonEmpty Path -> Stream IO Event) -> NonEmpty Path -> Stream IO Event
failNested w paths = Stream.mapM check (w paths)

    where

    check ev =
        let path =
                dropTrailingPathSeparator
                    $ Path.toString (Event.getAbsPath ev)
         in if takeFileName (takeDirectory path) == "nested"
            then error $ "Event for a nested path: " ++ Event.showEvent ev
            else return ev

-- | Create @nested/file@ and @nested/dir@ and then @file@ in the watch root.
-- FSEvents reports events in order, therefore, the events for @nested/file@
-- and @nested/dir@, if not dropped by the non-recursive watch, arrive before
-- the event for @file@ and 'failNested' raises an exception.
nestedOpsTest :: TestDesc
nestedOpsTest =
    ( "nested ops not reported (nested/file nested/dir)"
    , \fp -> createDirectory (fp </> "nested")
    , \fp -> do
        writeFile (fp </> "nested" </> "file") ""
        createDirectory (fp </> "nested" </> "dir")
        writeFile (fp </> "file") ""
    , [("file", fileEvent Event.isCreated)]
    )

-- TODO:
-- Creation/deletion/move of hard links
-- Deletion of last hard link
-- If a directory is a symlink is it followed in recursive mode?

main :: IO ()
main = do
    let rootLevelTests =
              fileCreate "file-create-attrs"
                (\file ->
                    [ (file, fileEvent Event.isCreated)
                    , (file, fileEvent Event.isAttrsModified)
                    ]
                )
            : fileModify
                    "file-mod-attrs"
                    (\file -> [(file, fileEvent Event.isAttrsModified)])
            : dirDelete "dir-sec" (\dir ->
                [ (dir, dirEvent Event.isDeleted)
                , (dir, dirEvent Event.isSecurityModified)
                ]
                )
            : commonTests
    let rootTests =
              dirDelete "" (\dir ->
                [ (dir, dirEvent Event.isDeleted)
                , (dir, dirEvent Event.isSecurityModified)
                , (dir, Event.isRootPathEvent)
                ]
                )
            -- The watch root create event always seems to come even though the
            -- root is created before the watch is started. That may be because
            -- of batching?
            -- XXX Need to create watch root after adding the watch, otherwise
            -- it fails intermittently.
            -- : dirCreate "" (\dir -> [(dir, dirEvent Event.isCreated)])
            : rootDirMove "moved" (\src ->
                [ (src, Event.isRootPathEvent)
                , (src, Event.isMoved)
                ])
            : []

    let fileRootTests =
            [ fileDelete "" (\path ->
                [ (path, Event.isRootPathEvent)
                , (path, Event.isDeleted)
                ])
            , rootFileMove "moved" (\path ->
                [(path, Event.isRootPathEvent)
                , (path, Event.isMoved)
                ])
            , fileModify "" (const [])
            ]

    let symLinkNonRecTests = nestedOpsTest : rootLevelTests
        regularNonRecTests = rootTests ++ symLinkNonRecTests

    let w = failNested (Event.watchWith (Event.setAllEvents True))
        run = runTests moduleName "non-recursive" w

    run DirType regularNonRecTests
    run SymLinkResolvedPath symLinkNonRecTests
    run FileType fileRootTests

    let symLinkRecTests =
              dirCreateWithParent "subdir/dir"
                (\dir -> [(dir, dirEvent Event.isCreated)])
            : fileCreateWithParent "subdir/file"
                (\file ->
                    [ (file, fileEvent Event.isCreated)
                    , (file, fileEvent Event.isAttrsModified)
                    ]
                )
            : rootLevelTests
            ++ commonRecTests
        regularRecTests = rootTests ++ symLinkRecTests

    let recw = Event.watchWith
                (Event.setRecursiveMode True . Event.setAllEvents True)
        runRec = runTests moduleName "recursive" recw

    runRec DirType regularRecTests
    runRec SymLinkResolvedPath symLinkRecTests
    runRec FileType fileRootTests
