-- |
-- Module      : Streamly.Test.FileSystem.Event.Linux
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Test.FileSystem.Event.Linux (main) where

import Control.Exception (bracket)
import Streamly.Internal.FS.Event.Linux (Event)
import System.Directory (createDirectory, removeDirectory)
import System.FilePath ((</>))
import System.IO (IOMode(WriteMode), hClose, openFile)
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Hspec
-- #if __GLASGOW_HASKELL__ == 902
#if 0
import qualified Data.List as List
#endif
import qualified Data.List.NonEmpty as NonEmpty
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.FileSystem.Path as Path
import qualified Streamly.Internal.FS.Event.Linux as Event

import Streamly.Test.FileSystem.Event.Common

#define DEVBUILD

moduleName :: String
moduleName = "FS.Event.Linux"

dirTouchEvents :: String -> [([Char], Event -> Bool)]
dirTouchEvents dir =
    [ (dir, dirEvent Event.isOpened)
    , (dir, dirEvent Event.isAccessed)
    , (dir, dirEvent Event.isNonWriteClosed)
    ]

dirDelEvents :: String -> [([Char], Event -> Bool)]
dirDelEvents dir =
      (dir, dirEvent Event.isDeleted)
    : (dir, dirEvent Event.isAttrsModified)
    : dirTouchEvents dir

rootDirDelEvents :: String -> [([Char], Event -> Bool)]
rootDirDelEvents root =
      (root, Event.isRootUnwatched)
    : (root, Event.isRootDeleted)
    : (root, dirEvent Event.isAttrsModified)
    : dirTouchEvents root

dirMoveEvents :: [Char] -> [Char] -> [([Char], Event -> Bool)]
dirMoveEvents src dst =
    [ (src, dirEvent Event.isMoved)
    , (src, dirEvent Event.isMovedFrom)
    , (dst, dirEvent Event.isMoved)
    , (dst, dirEvent Event.isMovedTo)
    ]

#ifdef DEVBUILD
-- In recursive mode all subdirectories are roots therefore they will generate
-- isRootMoved.
rootDirMoveEvents :: [Char] -> [Char] -> [([Char], Event -> Bool)]
rootDirMoveEvents root _ =
      (root, Event.isRootMoved)
    : dirTouchEvents root

recDirMoveEvents :: [Char] -> [Char] -> [([Char], Event -> Bool)]
recDirMoveEvents src dst = dirMoveEvents src dst ++ rootDirMoveEvents src dst
#endif

fileTouchEvents :: String -> [([Char], Event -> Bool)]
fileTouchEvents file =
    [ (file, fileEvent Event.isOpened)
    , (file, fileEvent Event.isModified)
    , (file, fileEvent Event.isWriteClosed)
    ]

fileMoveEvents :: [Char] -> [Char] -> [([Char], Event -> Bool)]
fileMoveEvents src dst =
    [ (src, fileEvent Event.isMoved)
    , (src, fileEvent Event.isMovedFrom)
    , (dst, fileEvent Event.isMoved)
    , (dst, fileEvent Event.isMovedTo)
    ]

-- TODO: add fileRoot tests from macOS test suite

-------------------------------------------------------------------------------
-- Watch removal tests
-------------------------------------------------------------------------------

watchCfg :: Event.Config
watchCfg = Event.setAllEvents True Event.defaultConfig

-- | Open a watch on the directories @root1@ and @root2@ in a temporary
-- directory, run the action with the two root paths and the watch, and close
-- the watch.
withTwoRoots :: (FilePath -> FilePath -> Event.Watch -> IO a) -> IO a
withTwoRoots action =
    withSystemTempDirectory "fsevent_dir" $ \fp -> do
        let root1 = fp </> "root1"
            root2 = fp </> "root2"
        createDirectory root1
        createDirectory root2
        roots <- mapM Path.fromString [root1, root2]
        bracket
            (Event.openWatch watchCfg (NonEmpty.fromList roots))
            Event.closeWatch
            (action root1 root2)

-- | Read the events from the watch, using @cfg@, up to and including the first
-- event that satisfies the predicate. Throw an exception if it does not occur
-- within 10 seconds.
readUntil :: Event.Config -> (Event -> Bool) -> Event.Watch -> IO [Event]
readUntil cfg p w = do
    r <- timeout 10000000
        $ Stream.toList
        $ Stream.takeEndBy p
        $ Stream.trace (putStrLn . Event.showEvent)
        $ Event.watchToStream cfg w
    maybe (error "readUntil: timed out") return r

-- | Read the events from the watch up to and including the first
-- 'Event.isRootUnwatched' event.
readUntilUnwatched :: Event.Watch -> IO [Event]
readUntilUnwatched = readUntil watchCfg Event.isRootUnwatched

getRoot :: Event -> FilePath
getRoot = Path.toString . Event.getRoot

getAbsPath :: Event -> FilePath
getAbsPath = Path.toString . Event.getAbsPath

-- | The last event must be the isRootUnwatched event for root1.
shouldEndUnwatched :: [Event] -> FilePath -> Expectation
shouldEndUnwatched evs root1 = do
    fmap Event.isRootUnwatched (lastMaybe evs) `shouldBe` Just True
    fmap getRoot (lastMaybe evs) `shouldBe` Just root1

    where

    lastMaybe = foldl (\_ x -> Just x) Nothing

-- The events queued before removeFromWatch, including isRootUnwatched, have
-- the watch descriptor of the removed root.
removeQueuedEvents :: Spec
removeQueuedEvents =
    it "removeFromWatch reports the queued events and isRootUnwatched"
        $ withTwoRoots $ \root1 _ w -> do
            let file = root1 </> "file"
            openFile file WriteMode >>= hClose
            Event.removeFromWatch w =<< Path.fromString root1
            evs <- readUntilUnwatched w
            any (\ev -> Event.isCreated ev && getAbsPath ev == file) evs
                `shouldBe` True
            shouldEndUnwatched evs root1

-- The second removeFromWatch runs before the isRootUnwatched event is read,
-- inotify_rm_watch fails with EINVAL.
removeTwice :: Spec
removeTwice =
    it "removeFromWatch twice does not throw an exception"
        $ withTwoRoots $ \root1 _ w -> do
            p <- Path.fromString root1
            Event.removeFromWatch w p
            Event.removeFromWatch w p
            evs <- readUntilUnwatched w
            shouldEndUnwatched evs root1

-- The kernel removes the watch of a deleted root.
removeDeleted :: Spec
removeDeleted =
    it "deleted root reports isRootDeleted and isRootUnwatched"
        $ withTwoRoots $ \root1 _ w -> do
            removeDirectory root1
            evs <- readUntilUnwatched w
            any (\ev -> Event.isRootDeleted ev && getRoot ev == root1) evs
                `shouldBe` True
            shouldEndUnwatched evs root1
            Event.removeFromWatch w =<< Path.fromString root1

-------------------------------------------------------------------------------
-- Overlapping roots tests
-------------------------------------------------------------------------------

recCfg :: Event.Config
recCfg = Event.setRecursiveMode True watchCfg

-- | Open a recursive watch on the directory @root@ and its subdirectory
-- @root/sub@, both as roots. The directory @sub@ is watched via both the
-- roots, the two roots share the watch descriptor of @sub@.
withNestedRoots :: (FilePath -> FilePath -> Event.Watch -> IO a) -> IO a
withNestedRoots action =
    withSystemTempDirectory "fsevent_dir" $ \fp -> do
        let root = fp </> "root"
            sub = root </> "sub"
        createDirectory root
        createDirectory sub
        roots <- mapM Path.fromString [root, sub]
        bracket
            (Event.openWatch recCfg (NonEmpty.fromList roots))
            Event.closeWatch
            (action root sub)

createFile :: FilePath -> IO ()
createFile file = openFile file WriteMode >>= hClose

isCreatedAt :: FilePath -> Event -> Bool
isCreatedAt file ev = Event.isCreated ev && getAbsPath ev == file

-- | The roots of the create events of @file@.
createdRoots :: FilePath -> [Event] -> [FilePath]
createdRoots file = map getRoot . filter (isCreatedAt file)

-- | The roots and absolute paths of the isRootUnwatched events.
unwatched :: [Event] -> [(FilePath, FilePath)]
unwatched evs =
    [(getRoot ev, getAbsPath ev) | ev <- evs, Event.isRootUnwatched ev]

-- The roots are reported in the order in which they were added.
overlapFanOut :: Spec
overlapFanOut =
    it "events of a shared directory are reported for both the roots"
        $ withNestedRoots $ \root sub w -> do
            let file = sub </> "file"
            createFile file
            evs <- readUntil recCfg
                (\ev -> isCreatedAt file ev && getRoot ev == sub) w
            createdRoots file evs `shouldBe` [root, sub]

-- The watch descriptor of sub is still used by root, removeFromWatch queues a
-- marker event instead of removing the kernel watch.
overlapRemoveInner :: Spec
overlapRemoveInner =
    it "removing the inner root keeps the outer root watching"
        $ withNestedRoots $ \root sub w -> do
            Event.removeFromWatch w =<< Path.fromString sub
            evs <- readUntil recCfg Event.isRootUnwatched w
            unwatched evs `shouldBe` [(sub, sub)]
            let file1 = sub </> "file1"
                file2 = root </> "file2"
            createFile file1
            createFile file2
            evs1 <- readUntil recCfg (isCreatedAt file2) w
            createdRoots file1 evs1 `shouldBe` [root]
            filter (== sub) (map getRoot evs1) `shouldBe` []

-- The watch descriptor of root is removed from the kernel, the watch
-- descriptor of sub is still used by sub.
overlapRemoveOuter :: Spec
overlapRemoveOuter =
    it "removing the outer root keeps the inner root watching"
        $ withNestedRoots $ \root sub w -> do
            Event.removeFromWatch w =<< Path.fromString root
            evs <- readUntil recCfg
                (\ev -> Event.isRootUnwatched ev && getAbsPath ev == sub) w
            unwatched evs `shouldBe` [(root, root), (root, sub)]
            let file = sub </> "file"
            createFile file
            evs1 <- readUntil recCfg (isCreatedAt file) w
            createdRoots file evs1 `shouldBe` [sub]
            filter (== root) (map getRoot evs1) `shouldBe` []

-- After removing sub, removing root removes all the paths of the watch
-- descriptor of sub, the kernel watch is removed.
overlapRemoveBoth :: Spec
overlapRemoveBoth =
    it "removing both the roots reports isRootUnwatched for each path"
        $ withNestedRoots $ \root sub w -> do
            Event.removeFromWatch w =<< Path.fromString sub
            Event.removeFromWatch w =<< Path.fromString root
            evs <- readUntil recCfg
                (\ev -> Event.isRootUnwatched ev && getRoot ev == root
                    && getAbsPath ev == sub) w
            unwatched evs `shouldBe` [(sub, sub), (root, root), (root, sub)]

-------------------------------------------------------------------------------
-- Watch descriptor tests
-------------------------------------------------------------------------------

-- inotify allocates the watch descriptors of a watch in increasing order, the
-- last of the 300 roots gets a watch descriptor greater than 255.
largeWd :: Spec
largeWd =
    it "reports the path of a watch descriptor greater than 255"
        $ withSystemTempDirectory "fsevent_dir" $ \fp -> do
            let dirs = [fp </> ("dir" ++ show i) | i <- [1 .. 300 :: Int]]
                file = fp </> "dir300" </> "file"
            mapM_ createDirectory dirs
            roots <- mapM Path.fromString dirs
            bracket
                (Event.openWatch watchCfg (NonEmpty.fromList roots))
                Event.closeWatch
                $ \w -> do
                    createFile file
                    evs <- readUntil watchCfg Event.isCreated w
                    map getAbsPath (filter Event.isCreated evs)
                        `shouldBe` [file]

removeTests :: IO ()
removeTests =
    hspec
        $ describe moduleName
        $ do
            describe "removeFromWatch" $ do
                removeQueuedEvents
                removeTwice
                removeDeleted
            describe "overlapping roots" $ do
                overlapFanOut
                overlapRemoveInner
                overlapRemoveOuter
                overlapRemoveBoth
            describe "watch descriptors" largeWd

main :: IO ()
main = do
    -- We ignore the events on root/parent dir during regular non-root dir/file
    -- tests.

    -- Tests common to regular root and symlink root cases
    let regSymTests =
              fileCreate "file" fileTouchEvents
            : fileMove "file1" "file2" fileMoveEvents
            : dirMove "dir1" "dir2" dirMoveEvents
            : dirDelete "dir" dirDelEvents
            : commonTests

    let regTests =
              dirDelete "" rootDirDelEvents
            : rootDirMove "moved" (\src -> [(src, Event.isRootMoved)])
            : regSymTests

    let symTests =
#if 0
             -- No events occur when a symlink root is moved. when root is a
             -- symlinked dir, it does not recv touch, isDeleted or
             -- rootDeleted, rootUnwatched events. We are not seeing
             -- isAttrModified event as well, so disabling this altogether.
              dirDelete "" (\dir -> [(dir, dirEvent Event.isAttrsModified)])
            :
#endif
            regSymTests

    let w = failNested (Event.watchWith (Event.setAllEvents True))
        run = runTests moduleName "non-recursive" w

#if 0
    let failingTests =
            [ "File deleted (file1)"
            , "File modified (file1)"
            , "File moved (file1 file2)"
            ]
#endif

    run DirType
#if 0
        $ filter (\(desc, _, _, _) -> desc `List.notElem` failingTests)
#endif
        (nestedOpsTest : regTests)

    run SymLinkOrigPath
#if 0
        $ filter (\(desc, _, _, _) -> desc `List.notElem` failingTests)
#endif
        (nestedOpsTest : symTests)

    let fileRootTests =
            [ fileDelete "" (\path ->
                [ (path, Event.isAttrsModified)
                , (path, Event.isRootDeleted)
                , (path, Event.isRootUnwatched)
                ])
            , rootFileMove "moved" (\path -> [(path, Event.isRootMoved)])
            , fileModify "" (\path -> [(path, Event.isOpened)])
            ]

    run FileType fileRootTests

    let recw = Event.watchWith
                (Event.setAllEvents True . Event.setRecursiveMode True)
        runRec = runTests moduleName "recursive" recw

#ifdef DEVBUILD
    -- In recursive mode all subdirectories are roots therefore they will
    -- generate isRootDeleted/isRootUnwatched. Also, for subdirectories
    -- multiple events are generated, once in the parent watch and once in the
    -- self watch as a root of the watch. Therefore, additional touchEvents are
    -- generated in this case.
    --
    -- XXX We can possibly filter out the duplicate events either from the
    -- parent or self.
    let regSymRecTests =
            -- XXX Nested file create misses the create event due to a race
            -- : fileCreateWithParent "subdir/file" fileTouchEvents
              fileCreate "subdir/file" fileTouchEvents
            : fileMove "subdir/file1" "subdir/file2" fileMoveEvents
            : dirMove "dir1" "dir2" recDirMoveEvents
            : dirMove "subdir/dir1" "subdir/dir2" recDirMoveEvents
            : dirDelete "dir" (\d -> rootDirDelEvents d ++ dirDelEvents d)
            : dirDelete "subdir/dir" (\d -> rootDirDelEvents d ++ dirDelEvents d)
            -- XXX Nested dir create misses the create event due to a race
            -- : dirCreateWithParent "subdir/dir" dirTouchEvents
            : dirCreate "subdir/dir"
                (\dir -> (dir, dirEvent Event.isCreated) : dirTouchEvents dir)
            : dirCreate "dir"
                (\dir -> (dir, dirEvent Event.isCreated) : dirTouchEvents dir)
            : commonRecTests
        recRegTests = regTests ++ regSymRecTests
        recSymTests = symTests ++ regSymRecTests

    -- XXX these tests fails intermittently for recursive case
    -- FS.Event.Linux.recursive, Root type SymLinkOrigPath, File deleted (subdir/file1)
    -- FS.Event.Linux.recursive, Root type SymLinkOrigPath, File modified (subdir/file1)
    -- FS.Event.Linux.recursive, Root type SymLinkOrigPath, File moved (subdir/file1 subdir/file2)
    -- FS.Event.Linux.recursive, Root type DirType, File moved (file1 file2)
    -- FS.Event.Linux.recursive, Root type DirType, File created (file)
    --      uncaught exception: IOException of type ResourceBusy
    --      /tmp/fsevent_dir-a5bd0df64c44ab27/watch-root/file: openFile: resource busy (file is locked)

#if 0
    let failingRecTests = failingTests ++
            [ "File created (subdir/file)"
            , "File deleted (subdir/file1)"
            , "File modified (subdir/file1)"
            , "File moved (subdir/file1 subdir/file2)"
            ]
#endif

    runRec DirType
#if 0
        $ filter (\(desc, _, _, _) -> desc `List.notElem` failingRecTests)
#endif
        recRegTests

    runRec SymLinkOrigPath
#if 0
        $ filter (\(desc, _, _, _) -> desc `List.notElem` failingRecTests)
#endif
        recSymTests
#endif
    runRec FileType fileRootTests

    removeTests
