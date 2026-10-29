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
import Data.List (isPrefixOf)
import Streamly.Internal.FS.Event.Linux (Event)
import System.Directory
    ( createDirectory
    , getPermissions
    , removeDirectory
    , removeFile
    , renameDirectory
    , setOwnerExecutable
    , setPermissions
    )
import System.FilePath ((</>), takeDirectory)
import System.IO
    (IOMode(AppendMode, WriteMode), hClose, hFlush, hPutStr, openFile)
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
-- In recursive mode the watch of a subdirectory reports the touch events
-- of the subdirectory scan, and isRootUnwatched when the subdirectory is
-- deleted.
recDirMoveEvents :: [Char] -> [Char] -> [([Char], Event -> Bool)]
recDirMoveEvents src dst = dirMoveEvents src dst ++ dirTouchEvents src

recDirDelEvents :: String -> [([Char], Event -> Bool)]
recDirDelEvents dir = (dir, Event.isRootUnwatched) : dirDelEvents dir
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

-------------------------------------------------------------------------------
-- Default config tests
-------------------------------------------------------------------------------

-- | Open a watch on the directory @root@ in a temporary directory using
-- @cfg@, run the action with the root path and the watch, and close the
-- watch.
withRoot :: Event.Config -> (FilePath -> Event.Watch -> IO a) -> IO a
withRoot cfg action =
    withSystemTempDirectory "fsevent_dir" $ \fp -> do
        let root = fp </> "root"
        createDirectory root
        r <- Path.fromString root
        bracket
            (Event.openWatch cfg (r NonEmpty.:| []))
            Event.closeWatch
            (action root)

-- | The paths of the events that satisfy the predicate.
pathsOf :: (Event -> Bool) -> [Event] -> [FilePath]
pathsOf p = map getAbsPath . filter p

-- The write to the unlinked file generates IN_MODIFY without IN_EXCL_UNLINK.
-- WriteMode truncates the file before the unlink, generating IN_MODIFY,
-- AppendMode does not truncate.
defaultExcludeUnlinked :: Spec
defaultExcludeUnlinked =
    it "does not report the events of an unlinked file"
        $ withRoot Event.defaultConfig $ \root w -> do
            let file = root </> "file"
                sentinel = root </> "sentinel"
            h <- openFile file AppendMode
            removeFile file
            hPutStr h "x"
            hFlush h
            hClose h
            createFile sentinel
            evs <- readUntil Event.defaultConfig (isCreatedAt sentinel) w
            pathsOf Event.isModified evs `shouldBe` []

-- chmod generates IN_ATTRIB only.
defaultAttrsModified :: Spec
defaultAttrsModified =
    it "reports the metadata change of a file"
        $ withRoot Event.defaultConfig $ \root w -> do
            let file = root </> "file"
            createFile file
            perms <- getPermissions file
            setPermissions file (setOwnerExecutable True perms)
            evs <- readUntil Event.defaultConfig Event.isAttrsModified w
            pathsOf Event.isAttrsModified evs `shouldBe` [file]

-------------------------------------------------------------------------------
-- Recursive mode tests
-------------------------------------------------------------------------------

isSelfEvent :: Event -> Bool
isSelfEvent ev = Event.isRootDeleted ev || Event.isRootMoved ev

-- | Run the file system action in a recursive watch of @root@ using @cfg@,
-- then create a file @sentinel@ in @root@. Return the events up to the
-- create event of @sentinel@.
recEventsOf ::
    Event.Config -> FilePath -> Event.Watch -> IO () -> IO [Event]
recEventsOf cfg root w action = do
    let sentinel = root </> "sentinel"
    action
    createFile sentinel
    readUntil cfg (isCreatedAt sentinel) w

-- The subdirectory watch generates IN_DELETE_SELF and IN_MOVE_SELF, the
-- events are not reported.
recSubdirSelfEvents :: Spec
recSubdirSelfEvents =
    it "does not report the root events of a subdirectory"
        $ withRoot recCfg $ \root w -> do
            let dir1 = root </> "dir1"
                dir2 = root </> "dir2"
                dir3 = root </> "dir3"
            createDirectory dir1
            createDirectory dir2
            evs <- recEventsOf recCfg root w $ do
                renameDirectory dir1 dir3
                removeDirectory dir2
            pathsOf isSelfEvent evs `shouldBe` []
            pathsOf Event.isMovedFrom evs `shouldBe` [dir1]
            pathsOf Event.isDeleted evs `shouldBe` [dir2]

isUnwatchedAt :: FilePath -> Event -> Bool
isUnwatchedAt path ev = Event.isRootUnwatched ev && getAbsPath ev == path

-- | @dir@ or a path under it.
isUnder :: FilePath -> FilePath -> Bool
isUnder dir path = path == dir || (dir ++ "/") `isPrefixOf` path

-- | Move @root/dir@, containing @sub@, out of the tree. The watches of @dir@
-- and @sub@ are removed in the ascending order of their watch descriptors,
-- the isRootUnwatched event of @sub@ is the last. Returns the events up to
-- that event.
recMoveOut :: Event.Config -> IO [Event]
recMoveOut cfg =
    withRoot cfg $ \root w -> do
        let dir = root </> "dir"
            sub = dir </> "sub"
            out = takeDirectory root </> "out"
            sentinel = root </> "sentinel"
        createDirectory dir
        createDirectory sub
        renameDirectory dir out
        evs <- readUntil cfg (isUnwatchedAt sub) w
        unwatched evs `shouldBe` [(root, dir), (root, sub)]
        createFile (out </> "sub" </> "file")
        createFile sentinel
        evs1 <- readUntil cfg (isCreatedAt sentinel) w
        filter (isUnder dir) (map getAbsPath evs1) `shouldBe` []
        return evs

recMoveOutSpec :: Spec
recMoveOutSpec =
    it "removes a directory moved out of the tree"
        $ () <$ recMoveOut recCfg

-- The kernel watch generates IN_MOVED_FROM to remove the moved directory,
-- the event is not reported.
recMoveOutUnrequested :: Spec
recMoveOutUnrequested =
    it "removes a moved directory when the move events are disabled" $ do
        let cfg = Event.setMovedFrom False $ Event.setMovedTo False recCfg
        evs <- recMoveOut cfg
        pathsOf Event.isMoved evs `shouldBe` []

recMoveIn :: Spec
recMoveIn =
    it "adds a directory moved into the tree"
        $ withRoot recCfg $ \root w -> do
            let src = takeDirectory root </> "src"
                dir = root </> "dir"
                file = dir </> "sub" </> "file"
            createDirectory src
            createDirectory (src </> "sub")
            renameDirectory src dir
            _ <- readUntil recCfg
                (\ev -> Event.isMovedTo ev && getAbsPath ev == dir) w
            createFile file
            evs <- readUntil recCfg (isCreatedAt file) w
            pathsOf Event.isCreated evs `shouldBe` [file]

-- IN_MOVED_TO of dir2 is queued before the IN_IGNORED events of dir1, dir2
-- is watched when the isRootUnwatched event of dir1/sub is read.
recRenameWithin :: Spec
recRenameWithin =
    it "reports the new path of a directory renamed within the tree"
        $ withRoot recCfg $ \root w -> do
            let dir1 = root </> "dir1"
                dir2 = root </> "dir2"
                file = dir2 </> "sub" </> "file"
            createDirectory dir1
            createDirectory (dir1 </> "sub")
            renameDirectory dir1 dir2
            _ <- readUntil recCfg (isUnwatchedAt (dir1 </> "sub")) w
            createFile file
            evs <- readUntil recCfg (isCreatedAt file) w
            filter (isUnder dir1) (map getAbsPath evs) `shouldBe` []

watchTests :: IO ()
watchTests =
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
            describe "recursive mode" $ do
                recSubdirSelfEvents
                recMoveOutSpec
                recMoveOutUnrequested
                recMoveIn
                recRenameWithin
            describe "defaultConfig" $ do
                defaultExcludeUnlinked
                defaultAttrsModified

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
    -- In recursive mode all subdirectories are watched, they generate
    -- isRootUnwatched. Also, for subdirectories multiple events are
    -- generated, once in the parent watch and once in the self watch.
    -- Therefore, additional touchEvents are generated in this case.
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
            : dirDelete "dir" recDirDelEvents
            : dirDelete "subdir/dir" recDirDelEvents
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

    watchTests
