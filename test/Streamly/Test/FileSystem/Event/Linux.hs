-- |
-- Module      : Streamly.Test.FileSystem.Event.Linux
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Test.FileSystem.Event.Linux (main) where

import Control.Concurrent
    ( MVar, ThreadId, forkIO, killThread, newEmptyMVar, putMVar
    , rtsSupportsBoundThreads, takeMVar, threadDelay, tryPutMVar, tryTakeMVar
    )
import Control.Exception
    ( AsyncException(ThreadKilled), IOException, SomeException, bracket
    , finally, fromException, mask, throwIO, try
    )
import Control.Monad (filterM, void, when)
import Data.Bits ((.&.))
import Data.List (isPrefixOf, nub, sort)
import Numeric (readOct)
import Streamly.Internal.FS.Event.Linux (Event)
import System.Directory
    ( createDirectory
    , createDirectoryLink
    , createFileLink
    , doesDirectoryExist
    , getPermissions
    , getSymbolicLinkTarget
    , listDirectory
    , removeDirectory
    , removeFile
    , renameDirectory
    , renamePath
    , setOwnerExecutable
    , setPermissions
    )
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.IO
    ( IOMode(AppendMode, ReadMode, WriteMode), hClose, hFlush, hPutStr
    , openFile
    )
import System.IO.Temp (withSystemTempDirectory)
import System.Mem (performGC)
import System.Posix.Files (createLink)
import System.Posix.Resource
    ( Resource(ResourceOpenFiles), ResourceLimit(ResourceLimit)
    , ResourceLimits(softLimit), getResourceLimit, setResourceLimit
    )
import System.Timeout (timeout)
import Test.Hspec
-- #if __GLASGOW_HASKELL__ == 902
#if 0
import qualified Data.List as List
#endif
import qualified Data.List.NonEmpty as NonEmpty
import qualified Streamly.Internal.Data.Fold as Fold
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
    : (root, rootEvent Event.isRootDeleted)
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
-- In recursive mode the watch of a subdirectory reports isRootUnwatched when
-- the subdirectory is deleted.
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
-- | Like 'readUntil', the action is run on each event when it is received,
-- before the next event is read.
readUntilWith ::
    Event.Config -> (Event -> IO ()) -> (Event -> Bool) -> Event.Watch
    -> IO [Event]
readUntilWith cfg action p w = do
    r <- timeout 10000000
        $ Stream.toList
        $ Stream.takeEndBy p
        $ Stream.trace action
        $ Stream.trace (putStrLn . Event.showEvent)
        $ Event.watchToStream cfg w
    maybe (error "readUntil: timed out") return r

readUntil :: Event.Config -> (Event -> Bool) -> Event.Watch -> IO [Event]
readUntil cfg = readUntilWith cfg (const (return ()))

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

-- The inner root sub is moved out of the outer root. The outer root paths
-- are removed first, by a marker event, while the inner root still uses the
-- watch descriptors of sub and sub/dir. The inner root is renamed, it is
-- removed next, which removes the watch descriptors.
overlapMoveOutInner :: Spec
overlapMoveOutInner =
    it "moving the inner root out of the outer root unwatches both"
        $ withNestedRoots $ \root sub w -> do
            let dir = sub </> "dir"
                out = takeDirectory root </> "out"
                file = root </> "file"
            createDirectory dir
            waitWatched recCfg w dir
            renameDirectory sub out
            evs <- readUntil recCfg
                (\ev -> isUnwatchedAt dir ev && getRoot ev == sub) w
            unwatched evs
                `shouldBe` [(root, sub), (root, dir), (sub, sub), (sub, dir)]
            createFile (out </> "dir" </> "file")
            createFile file
            evs1 <- readUntil recCfg (isCreatedAt file) w
            map getRoot (filter Event.isCreated evs1) `shouldBe` [root]
            unwatched evs1 `shouldBe` []

-- The second add of the root finds the existing watch, the path is watched
-- once.
duplicateRoot :: Spec
duplicateRoot =
    it "reports the events of a root listed twice once"
        $ withSystemTempDirectory "fsevent_dir" $ \fp -> do
            let root = fp </> "root"
                file = root </> "file"
                sentinel = root </> "sentinel"
            createDirectory root
            r <- Path.fromString root
            bracket
                (Event.openWatch watchCfg (r NonEmpty.:| [r]))
                Event.closeWatch
                $ \w -> do
                    createFile file
                    createFile sentinel
                    evs <- readUntil watchCfg (isCreatedAt sentinel) w
                    createdRoots file evs `shouldBe` [root]

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

-- | The paths of the 'Event.isCreated' events reported by the kernel.
kernelCreated :: [Event] -> [FilePath]
kernelCreated =
    pathsOf (\ev -> Event.isCreated ev && not (Event.isScanEvent ev))

-- | The events after the 'Event.isMovedTo' event of a path.
afterMovedTo :: FilePath -> [Event] -> [Event]
afterMovedTo path = drop 1 . dropWhile (not . isMovedToPath)

    where

    isMovedToPath ev = Event.isMovedTo ev && getAbsPath ev == path

-- | The paths of the 'Event.isScanEvent' events that satisfy the predicate.
scannedPaths :: (Event -> Bool) -> [Event] -> [FilePath]
scannedPaths p =
    pathsOf (\ev -> Event.isScanEvent ev && Event.isCreated ev && p ev)

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

-- No activity follows the operation on the root. 'readUntil' raises an
-- exception on a timeout if the event is held back until a later event.
lastRootEvent :: Spec
lastRootEvent =
    describe "last event for the root is reported without a later event" $ do
        it "metadata change of the root"
            $ withRoot Event.defaultConfig $ \root w -> do
                perms <- getPermissions root
                setPermissions root (setOwnerExecutable True perms)
                evs <- readUntil Event.defaultConfig Event.isAttrsModified w
                pathsOf Event.isAttrsModified evs `shouldBe` [root]
        it "rename of the root"
            $ withRoot movedCfg $ \root w -> do
                renameDirectory root (root ++ "-moved")
                evs <- readUntil movedCfg Event.isRootMoved w
                pathsOf Event.isRootMoved evs `shouldBe` [root]

    where

    movedCfg = Event.setRootPathEvents True Event.defaultConfig

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
-- Renamed roots tests
-------------------------------------------------------------------------------

-- | Rename the watch root @root@ to @root ++ "-moved"@ and read the events
-- up to the 'Event.isRootUnwatched' event of @path@.
renameRootUntil ::
    Event.Config -> FilePath -> FilePath -> Event.Watch -> IO [Event]
renameRootUntil cfg root path w = do
    renamePath root (root ++ "-moved")
    readUntil cfg (\ev -> Event.isRootUnwatched ev && getAbsPath ev == path) w

rootRenamed :: Spec
rootRenamed =
    it "a renamed root reports isRootMoved and is unwatched"
        $ withRoot cfg $ \root w -> do
            evs <- renameRootUntil cfg root root w
            pathsOf Event.isRootMoved evs `shouldBe` [root]
            unwatched evs `shouldBe` [(root, root)]

    where

    cfg = Event.setRootMoved True Event.defaultConfig

-- IN_MOVE_SELF is requested for every root, it is not reported unless
-- 'setRootMoved' is enabled.
rootRenamedUnrequested :: Spec
rootRenamedUnrequested =
    it "a renamed root is unwatched without setRootMoved"
        $ withRoot Event.defaultConfig $ \root w -> do
            evs <- renameRootUntil Event.defaultConfig root root w
            pathsOf Event.isRootMoved evs `shouldBe` []
            unwatched evs `shouldBe` [(root, root)]

recRootRenamed :: Spec
recRootRenamed =
    it "a renamed recursive root is unwatched with the paths under it"
        $ withRoot recCfg $ \root w -> do
            let sub = root </> "sub"
            createDirectory sub
            createFile (root </> "sentinel")
            _ <- readUntil recCfg (isCreatedAt (root </> "sentinel")) w
            evs <- renameRootUntil recCfg root sub w
            unwatched evs `shouldBe` [(root, root), (root, sub)]

-- Both renames are done before the events are read.
rootRenamedBack :: Spec
rootRenamedBack =
    it "a root renamed back before the event is read stays watched"
        $ withRoot watchCfg $ \root w -> do
            let file = root </> "file"
            renameDirectory root (root ++ "-moved")
            renameDirectory (root ++ "-moved") root
            createFile file
            evs <- readUntil watchCfg (isCreatedAt file) w
            unwatched evs `shouldBe` []

-- The rename of one hard link reports IN_MOVE_SELF for both roots.
rootHardLinkRenamed :: Spec
rootHardLinkRenamed =
    it "renaming one of two hard links watched as roots unwatches it only"
        $ withSystemTempDirectory "fsevent_dir" $ \fp -> do
            let f = fp </> "f"
                g = fp </> "g"
            createFile f
            createLink f g
            roots <- mapM Path.fromString [f, g]
            bracket
                (Event.openWatch watchCfg (NonEmpty.fromList roots))
                Event.closeWatch
                $ \w -> do
                    evs <- renameRootUntil watchCfg f f w
                    unwatched evs `shouldBe` [(f, f)]
                    appendFile g "x"
                    evs1 <- readUntil watchCfg Event.isModified w
                    map getRoot (filter Event.isModified evs1)
                        `shouldBe` [g]

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

-- | Read the events up to the create event of @dir@. The watch adds @dir@
-- and the directories under it when it reads the event.
waitWatched :: Event.Config -> Event.Watch -> FilePath -> IO ()
waitWatched cfg w dir = () <$ readUntil cfg (isCreatedAt dir) w

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
            waitWatched recCfg w dir2
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

-- | Move @root/dir@, containing @sub@ and @sub/sub1@, out of the tree. The
-- watches are removed in the ascending order of their watch descriptors, the
-- isRootUnwatched event of @sub1@ is the last. Returns the events up to that
-- event.
recMoveOut :: Event.Config -> IO [Event]
recMoveOut cfg =
    withRoot cfg $ \root w -> do
        let dir = root </> "dir"
            sub = dir </> "sub"
            sub1 = sub </> "sub1"
            out = takeDirectory root </> "out"
            sentinel = root </> "sentinel"
        createDirectory dir
        createDirectory sub
        createDirectory sub1
        waitWatched cfg w dir
        renameDirectory dir out
        evs <- readUntil cfg (isUnwatchedAt sub1) w
        unwatched evs `shouldBe` [(root, dir), (root, sub), (root, sub1)]
        createFile (out </> "sub" </> "file")
        createFile (out </> "sub" </> "sub1" </> "file")
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

-- The watches of dir1 are kept, its paths are changed on IN_MOVED_TO.
recRenameWithin :: Spec
recRenameWithin =
    it "reports the new path of a directory renamed within the tree"
        $ withRoot recCfg $ \root w -> do
            let dir1 = root </> "dir1"
                dir2 = root </> "dir2"
                file = dir2 </> "sub" </> "file"
            createDirectory dir1
            createDirectory (dir1 </> "sub")
            waitWatched recCfg w dir1
            renameDirectory dir1 dir2
            _ <- readUntil recCfg
                (\ev -> Event.isMovedTo ev && getAbsPath ev == dir2) w
            createFile file
            evs <- readUntil recCfg (isCreatedAt file) w
            filter (isUnder dir1) (map getAbsPath evs) `shouldBe` []

-- | Run the test with a watched directory @dir1@ containing @sub@. The test
-- gets the root, the watch and the paths @dir1@ and @dir2@.
withRenameDirs ::
    (FilePath -> Event.Watch -> FilePath -> FilePath -> IO a) -> IO a
withRenameDirs action =
    withRoot recCfg $ \root w -> do
        let dir1 = root </> "dir1"
        createDirectory dir1
        createDirectory (dir1 </> "sub")
        waitWatched recCfg w dir1
        action root w dir1 (root </> "dir2")

-- | The paths of all the entries under a directory.
listTree :: FilePath -> IO [FilePath]
listTree dir = do
    entries <- map (dir </>) <$> listDirectory dir
    dirs <- filterM doesDirectoryExist entries
    (entries ++) . concat <$> mapM listTree dirs

-- IN_MOVED_TO of dir2 is queued before the create events of the entries, the
-- paths are changed when these events are read. The walk on IN_MOVED_TO
-- reports all the entries of dir2, before the create events of x and g. The
-- create event of x finds x already watched and reports f again.
recRenameCreateBeforeRead :: Spec
recRenameCreateBeforeRead =
    it "reports entries created after a rename under the new path"
        $ withRenameDirs $ \root w dir1 dir2 -> do
            let later = dir2 </> "x" </> "later"
            evs <- recEventsOf recCfg root w $ do
                renameDirectory dir1 dir2
                createDirectory (dir2 </> "x")
                createFile (dir2 </> "x" </> "f")
                createFile (dir2 </> "g")
            kernelCreated evs
                `shouldBe` [dir2 </> "x", dir2 </> "g", root </> "sentinel"]
            sort (scannedPaths (const True) evs)
                `shouldBe`
                    sort
                        [ dir2 </> "g"
                        , dir2 </> "sub"
                        , dir2 </> "x"
                        , dir2 </> "x" </> "f"
                        , dir2 </> "x" </> "f"
                        ]
            createFile later
            evs1 <- readUntil recCfg (isCreatedAt later) w
            pathsOf Event.isCreated evs1 `shouldBe` [later]

-- The action runs after IN_MOVED_FROM is read and before IN_MOVED_TO is read.
recRenameCreateInGap :: Spec
recRenameCreateInGap =
    it "reports entries created between the two move events"
        $ withRenameDirs $ \root w dir1 dir2 -> do
            let sentinel = root </> "sentinel"
                inGap ev =
                    when (Event.isMovedFrom ev && getAbsPath ev == dir1) $ do
                        createFile (dir2 </> "gap")
                        createFile (dir2 </> "sub" </> "gap")
                        createFile sentinel
            renameDirectory dir1 dir2
            evs <- readUntilWith recCfg inGap (isCreatedAt sentinel) w
            kernelCreated evs
                `shouldBe`
                    [dir2 </> "gap", dir2 </> "sub" </> "gap", sentinel]
            sort (scannedPaths (const True) evs)
                `shouldBe`
                    sort
                        [ dir2 </> "gap"
                        , dir2 </> "sub"
                        , dir2 </> "sub" </> "gap"
                        ]

-- This test fails if the kernel queues IN_MOVE_SELF before IN_MOVED_TO, see
-- the IMPORTANT note above 'clearMoved' in the library.
recRenameNoUnwatched :: Spec
recRenameNoUnwatched =
    it "does not report isRootUnwatched for a renamed directory"
        $ withRenameDirs $ \root w dir1 dir2 -> do
            evs <- recEventsOf recCfg root w (renameDirectory dir1 dir2)
            createFile (dir2 </> "sub" </> "file")
            createFile (root </> "sentinel1")
            evs1 <- readUntil recCfg (isCreatedAt (root </> "sentinel1")) w
            unwatched (evs ++ evs1) `shouldBe` []

-- The create event of x is read after the rename, the old path does not exist
-- and x cannot be added. It reports the old path, the path at the time of the
-- event. The walk on IN_MOVED_TO adds x and reports the entries of dir2.
recRenameCreateBeforeRename :: Spec
recRenameCreateBeforeRename =
    it "reports entries of a directory created before the rename"
        $ withRenameDirs $ \root w dir1 dir2 -> do
            let later = dir2 </> "x" </> "later"
            evs <- recEventsOf recCfg root w $ do
                createDirectory (dir1 </> "x")
                createFile (dir1 </> "x" </> "f")
                renameDirectory dir1 dir2
            kernelCreated evs `shouldBe` [dir1 </> "x", root </> "sentinel"]
            sort (scannedPaths (const True) evs)
                `shouldBe`
                    sort
                        [ dir2 </> "sub"
                        , dir2 </> "x"
                        , dir2 </> "x" </> "f"
                        ]
            createFile later
            evs1 <- readUntil recCfg (isCreatedAt later) w
            pathsOf Event.isCreated evs1 `shouldBe` [later]

-- dir2 does not exist when the IN_MOVED_TO of the first rename is read, the
-- paths of dir1 are removed and dir3 is added on the second IN_MOVED_TO. file
-- is reported under the old path, and by the scan of dir3 at the new path.
-- This is the current behavior, a change that keeps the watches over two
-- renames must change this test.
recRenameTwice :: Spec
recRenameTwice =
    it "adds the directory again if it is renamed twice before the read"
        $ withRenameDirs $ \root w dir1 dir2 -> do
            let dir3 = root </> "dir3"
                later = dir3 </> "sub" </> "later"
            evs <- recEventsOf recCfg root w $ do
                renameDirectory dir1 dir2
                renameDirectory dir2 dir3
                createFile (dir3 </> "file")
            sort (pathsOf Event.isCreated evs)
                `shouldBe` sort
                    [ dir1 </> "file"
                    , dir3 </> "file"
                    , dir3 </> "sub"
                    , root </> "sentinel"
                    ]
            createFile later
            evs1 <- readUntil recCfg (isCreatedAt later) w
            pathsOf Event.isCreated evs1 `shouldBe` [later]

-- The paths of dir1 are changed before its isMovedTo event is delivered.
recRenameScanOnMovedTo :: Spec
recRenameScanOnMovedTo =
    it "a scan on isMovedTo finds all the entries of a renamed directory"
        $ withRenameDirs $ \_ w dir1 dir2 -> do
            let inGap ev =
                    when (Event.isMovedFrom ev && getAbsPath ev == dir1)
                        $ createFile (dir2 </> "gap")
                isMovedToDir ev = Event.isMovedTo ev && getAbsPath ev == dir2
            renameDirectory dir1 dir2
            createDirectory (dir2 </> "x")
            createFile (dir2 </> "x" </> "f")
            _ <- readUntilWith recCfg inGap isMovedToDir w
            entries <- listTree dir2
            sort entries
                `shouldBe`
                    sort
                        [ dir2 </> "sub"
                        , dir2 </> "x"
                        , dir2 </> "x" </> "f"
                        , dir2 </> "gap"
                        ]

-- The second watch reports when the scan of dir opens sub, and dir is renamed
-- during the scan. sub is listed with scanFlags, and watchFlags cannot be
-- restored at the old path. The many subdirectories keep the scan running
-- until the rename. The walk on IN_MOVED_TO restores the flags of sub. Without
-- it sub reports no open events and the last read times out.
recRenameDuringScan :: Spec
recRenameDuringScan =
    it "restores the event flags of a directory renamed during its scan"
        $ withRoot recCfg $ \root w -> do
            let dir = root </> "dir"
                dir1 = root </> "dir1"
                sub = dir </> "sub"
                file = dir1 </> "sub" </> "file"
            createDirectory dir
            createDirectory sub
            createFile (sub </> "file")
            mapM_ (\i -> createDirectory (sub </> show i)) [1 .. 5000 :: Int]
            subPath <- Path.fromString sub
            done <- newEmptyMVar
            bracket
                (Event.openWatch watchCfg (subPath NonEmpty.:| []))
                Event.closeWatch
                $ \w1 -> do
                    _ <- forkIO
                        $ (do
                            _ <- readUntil watchCfg Event.isOpened w1
                            renameDirectory dir dir1
                          ) `finally` putMVar done ()
                    _ <- readUntil recCfg
                        (\ev -> Event.isMovedTo ev && getAbsPath ev == dir1) w
                    takeMVar done
            openFile file ReadMode >>= hClose
            evs <- readUntil recCfg
                (\ev -> getAbsPath ev == file && Event.isNonWriteClosed ev) w
            pathsOf Event.isOpened evs `shouldContain` [file]

-------------------------------------------------------------------------------
-- Resource tests
-------------------------------------------------------------------------------

-- | The inotify file descriptors of the process.
inotifyFds :: IO [FilePath]
inotifyFds = do
    fds <- listDirectory "/proc/self/fd"
    filterM isInotify fds

    where

    isInotify fd = do
        r <- try (getSymbolicLinkTarget ("/proc/self/fd" </> fd))
        return $ case r of
            Left (_ :: IOException) -> False
            Right target -> target == "anon_inode:inotify"

-- The second root does not exist, adding it fails after the inotify fd is
-- created and the first root is added.
openWatchNoLeak :: Spec
openWatchNoLeak =
    it "openWatch closes the inotify fd when adding a path fails"
        $ withSystemTempDirectory "fsevent_dir" $ \fp -> do
            let root = fp </> "root"
            createDirectory root
            roots <- mapM Path.fromString [root, fp </> "missing"]
            fds <- inotifyFds
            r <- try (Event.openWatch watchCfg (NonEmpty.fromList roots))
            case r of
                Left (_ :: IOException) -> return ()
                Right w -> do
                    Event.closeWatch w
                    expectationFailure "openWatch did not fail"
            inotifyFds `shouldReturn` fds

-- | The flags of a file descriptor from @\/proc\/self\/fdinfo@.
fdFlags :: FilePath -> IO Integer
fdFlags fd = do
    info <- readFile ("/proc/self/fdinfo" </> fd)
    case [w | ("flags:" : w : _) <- map words (lines info)] of
        (w : _) | [(n, "")] <- readOct w -> return n
        _ -> error $ "fdFlags: no flags in fdinfo of " ++ fd

-- O_CLOEXEC is 02000000 on Linux.
cloexecWatch :: Spec
cloexecWatch =
    it "the inotify fd is close-on-exec"
        $ withRoot watchCfg $ \_ _ -> do
            fds <- inotifyFds
            flags <- mapM fdFlags fds
            null flags `shouldBe` False
            filter (\f -> f .&. 0o2000000 == 0) flags `shouldBe` []

-------------------------------------------------------------------------------
-- Async exception tests
-------------------------------------------------------------------------------

-- | Run the action in a new thread, return the thread and an 'MVar' with its
-- result. The thread starts masked, so that an exception thrown to it right
-- after the fork is reported in the result.
forkResult :: IO () -> IO (ThreadId, MVar (Either SomeException ()))
forkResult action = do
    result <- newEmptyMVar
    tid <-
        mask $ \restore ->
            forkIO (try (restore action) >>= putMVar result)
    return (tid, result)

-- | The thread must end within 5 seconds, killed by 'killThread'. Any other
-- exception fails the test, for example, reaching an inotify limit.
shouldBeKilled :: MVar (Either SomeException ()) -> Expectation
shouldBeKilled result = do
    r <- timeout 5000000 (takeMVar result)
    case r of
        Nothing -> expectationFailure "the thread did not end"
        Just (Right ()) -> expectationFailure "the thread was not killed"
        Just (Left e) ->
            when (fromException e /= Just ThreadKilled) $ throwIO e

-- | Run the action until it returns 'True', at most 50 times, 100 ms apart.
retryUntil :: IO Bool -> IO Bool
retryUntil action = go (50 :: Int)

    where

    go 0 = return False
    go n = do
        ok <- action
        if ok
        then return True
        else threadDelay 100000 >> go (n - 1)

-- | The number of inotify fds must reach @n@ without a garbage collection.
waitFdCount :: Int -> IO ()
waitFdCount n =
    retryUntil ((== n) . length <$> inotifyFds) >>= (`shouldBe` True)

-- | The number of inotify fds must return to @n@ after garbage collection.
gcFdCount :: Int -> IO ()
gcFdCount n =
    retryUntil (performGC >> (== n) . length <$> inotifyFds)
        >>= (`shouldBe` True)

-- | A recursive watch stream of the path.
recStream :: FilePath -> IO (Stream.Stream IO Event)
recStream root = do
    p <- Path.fromString root
    return $ Event.watchWith (Event.setRecursiveMode True) (p NonEmpty.:| [])

-- | An exception raised in the stream closes the watch without a garbage
-- collection.
killWaitingReader :: Spec
killWaitingReader =
    it "killing a thread waiting for events closes the watch at once"
        $ withSystemTempDirectory "fsevent_dir" $ \fp -> do
            n <- length <$> inotifyFds
            stream <- recStream fp
            (tid, result) <- forkResult (Stream.fold Fold.drain stream)
            waitFdCount (n + 1)
            -- Let the reader block in the wait for events
            threadDelay 200000
            killThread tid
            shouldBeKilled result
            length <$> inotifyFds `shouldReturn` n

-- | The scan of the tree runs in the masked allocation of the bracket. The
-- exception can be delivered when the allocation ends, outside the stream,
-- the watch is then closed by the garbage collection.
killStartingWatch :: Spec
killStartingWatch =
    it "killing a thread during the start of a watch closes the watch"
        $ withSystemTempDirectory "fsevent_dir" $ \fp -> do
            mapM_
                (\i -> createDirectory (fp </> show i))
                [1 .. 2000 :: Int]
            n <- length <$> inotifyFds
            stream <- recStream fp
            let kill delay = do
                    (tid, result) <-
                        forkResult (Stream.fold Fold.drain stream)
                    threadDelay delay
                    killThread tid
                    shouldBeKilled result
                    gcFdCount n
            mapM_ kill [0, 100, 1000, 5000, 20000, 50000]

-- | The delays cover the allocation, the first read and the wait. The garbage
-- collections keep the number of unclosed watches below the
-- max_user_instances limit.
killRepeatedly :: Spec
killRepeatedly =
    it "killing a thread at any point leaves no fd and no blocked thread"
        $ withSystemTempDirectory "fsevent_dir" $ \fp -> do
            createDirectory (fp </> "sub")
            n <- length <$> inotifyFds
            stream <- recStream fp
            let kill i = do
                    (tid, result) <-
                        forkResult (Stream.fold Fold.drain stream)
                    when (i `mod` 3 /= 0) $ threadDelay (i `mod` 400)
                    killThread tid
                    shouldBeKilled result
                    when (i `mod` 50 == 0) performGC
            mapM_ kill [1 .. 2000 :: Int]
            gcFdCount n

-- | The consumer runs outside the stream, the stream does not see the
-- exception. The watch is closed by the garbage collection.
killConsumer :: Spec
killConsumer =
    it "killing a thread in the consumer closes the watch on GC"
        $ withSystemTempDirectory "fsevent_dir" $ \fp -> do
            n <- length <$> inotifyFds
            stream <- recStream fp
            consuming <- newEmptyMVar
            let consume _ = do
                    _ <- tryPutMVar consuming ()
                    threadDelay 60000000
            (tid, result) <-
                forkResult (Stream.fold (Fold.drainMapM consume) stream)
            waitFdCount (n + 1)
            createFile (fp </> "file")
            timeout 5000000 (takeMVar consuming) `shouldReturn` Just ()
            killThread tid
            shouldBeKilled result
            gcFdCount n

-- | 'Stream.take' stops without draining the stream, the bracket of the
-- stream does not see the end. The watch is closed by the garbage collection.
abandonStream :: Spec
abandonStream =
    it "a stream abandoned by the consumer closes the watch on GC"
        $ withSystemTempDirectory "fsevent_dir" $ \fp -> do
            n <- length <$> inotifyFds
            stream <- recStream fp
            (_, result) <-
                forkResult (Stream.fold Fold.drain (Stream.take 1 stream))
            waitFdCount (n + 1)
            createFile (fp </> "file")
            r <- timeout 5000000 (takeMVar result)
            fmap (either (const False) (const True)) r `shouldBe` Just True
            gcFdCount n

-------------------------------------------------------------------------------
-- Concurrency tests
-------------------------------------------------------------------------------

-- | Run the action while another thread reads the events of the watch up to
-- and including the first event that satisfies the predicate. Return the
-- result of the action and the events read.
whileReading ::
    Event.Config -> (Event -> Bool) -> Event.Watch -> IO a -> IO (a, [Event])
whileReading cfg p w action = do
    mv <- newEmptyMVar
    bracket
        (forkIO (try (readUntil cfg p w) >>= putMVar mv))
        killThread
        $ \_ -> do
            -- Let the reader block in read
            threadDelay 200000
            r <- action
            evs <- takeMVar mv
            either (\(e :: SomeException) -> throwIO e) (return . (r,)) evs

-- The reader thread is blocked reading the watch fd.
addWhileReading :: Spec
addWhileReading =
    it "addToWatch does not block while another thread reads the events"
        $ withRoot watchCfg $ \root w -> do
            let root2 = takeDirectory root </> "root2"
                file = root2 </> "file"
            createDirectory root2
            p <- Path.fromString root2
            (r, evs) <- whileReading watchCfg (isCreatedAt file) w $ do
                r <- timeout 2000000 $ Event.addToWatch watchCfg w (p, Nothing)
                -- Unblock the reader
                createFile file
                return r
            r `shouldBe` Just ()
            createdRoots file evs `shouldBe` [root2]

removeWhileReading :: Spec
removeWhileReading =
    it "removeFromWatch does not block while another thread reads the events"
        $ withRoot watchCfg $ \root w -> do
            p <- Path.fromString root
            (r, evs) <- whileReading watchCfg Event.isRootUnwatched w $ do
                r <- timeout 2000000 $ Event.removeFromWatch w p
                -- Unblock the reader if removeFromWatch did not
                createFile (root </> "file")
                return r
            r `shouldBe` Just ()
            unwatched evs `shouldBe` [(root, root)]

-- The reader thread is blocked reading the watch fd. The stream ends when
-- the watch is closed, without any file system event.
closeWhileReading :: Spec
closeWhileReading =
    it "closeWatch does not block while another thread reads the events"
        $ withRoot watchCfg $ \_ w -> do
            mv <- newEmptyMVar
            let reader = Stream.drain (Event.watchToStream watchCfg w)
            bracket
                (forkIO (try reader >>= putMVar mv))
                killThread
                $ \_ -> do
                    threadDelay 200000
                    timeout 2000000 (Event.closeWatch w)
                        `shouldReturn` Just ()
                    -- The stream ends without an exception
                    r <- timeout 2000000 (takeMVar mv)
                    fmap (either (\(_ :: SomeException) -> False) ok) r
                        `shouldBe` Just True

    where

    ok () = True

-------------------------------------------------------------------------------
-- Failure tests
-------------------------------------------------------------------------------

-- | Run the action with all the file descriptors of the process in use.
-- The soft limit on open files is lowered to at most 1024 for the duration,
-- a high limit would need a Handle per descriptor up to the limit. Returns
-- 'Nothing' if the limit is not reached in 100000 opens.
withFdsExhausted :: IO a -> IO (Maybe a)
withFdsExhausted action =
    bracket
        (getResourceLimit ResourceOpenFiles)
        (setResourceLimit ResourceOpenFiles)
        $ \lims -> do
            setResourceLimit ResourceOpenFiles (lower lims)
            go [] (100000 :: Int)

    where

    maxFds = 1024

    lower lims =
        case softLimit lims of
            ResourceLimit n | n <= maxFds -> lims
            _ -> lims { softLimit = ResourceLimit maxFds }

    go hs 0 = mapM_ hClose hs >> return Nothing
    go hs n = do
        r <- try (openFile "/dev/null" ReadMode)
        case r of
            Right h -> go (h : hs) (n - 1)
            Left (_ :: IOException) ->
                (Just <$> action) `finally` mapM_ hClose hs

-- The marker of the shared watch descriptor of sub needs a pipe, the pipe
-- fails with EMFILE.
removeFailRetry :: Spec
removeFailRetry =
    it "removeFromWatch can be retried after it fails"
        $ withNestedRoots $ \_ sub w -> do
            p <- Path.fromString sub
            r <- withFdsExhausted (try (Event.removeFromWatch w p))
            case r of
                Nothing -> pendingWith "the open file limit is too high"
                Just (Right ()) ->
                    expectationFailure "removeFromWatch did not fail"
                Just (Left (_ :: IOException)) -> do
                    Event.removeFromWatch w p
                    evs <- readUntil recCfg Event.isRootUnwatched w
                    unwatched evs `shouldBe` [(sub, sub)]

-- IN_MOVE_SELF tells that the directory is moved out, no fd is needed.
recMoveOutNoFd :: Spec
recMoveOutNoFd =
    it "removes a directory moved out of the tree without a free fd"
        $ withRenameDirs $ \root w dir1 _ -> do
            renameDirectory dir1 (takeDirectory root </> "out")
            r <- withFdsExhausted
                    $ readUntil recCfg (isUnwatchedAt (dir1 </> "sub")) w
            when (null r) $ pendingWith "the open file limit is too high"

-- The create event of d watches the second directory, it has no IN_MOVE_SELF
-- event. Its path stays watched, and its cookie does not affect a later move.
recDirMovedOutAndRecreated :: Spec
recDirMovedOutAndRecreated =
    it "keeps a directory created at the path of a directory moved out"
        $ withRoot recCfg $ \root w -> do
            let dir = root </> "dir"
                dir1 = root </> "dir1"
                out = takeDirectory root </> "out"
                file = dir </> "file"
                file1 = dir1 </> "file1"
            _ <- recEventsBeforeAdd root w $ do
                createDirectory dir
                renameDirectory dir out
                createDirectory dir
            createFile file
            evs <- readUntil recCfg (isCreatedAt file) w
            pathsOf Event.isCreated evs `shouldBe` [file]
            evs1 <- recEventsOf recCfg root w $ do
                renameDirectory dir dir1
                createFile file1
            kernelCreated evs1 `shouldBe` [file1, root </> "sentinel"]
            sort (scannedPaths (const True) evs1)
                `shouldBe` sort [dir1 </> "file", file1]
            unwatched evs1 `shouldBe` []

-- dir is renamed within the outer root and moved out of the inner root.
recRenameOutOfInnerRoot :: Spec
recRenameOutOfInnerRoot =
    it "a move out of the inner root is a rename within the outer root"
        $ withNestedRoots $ \root sub w -> do
            let dir = sub </> "dir"
                dir1 = root </> "dir1"
                file = dir1 </> "file"
            createDirectory dir
            waitWatched recCfg w dir
            renameDirectory dir dir1
            evs <- readUntil recCfg (isUnwatchedAt dir) w
            unwatched evs `shouldBe` [(sub, dir)]
            createFile file
            evs1 <- readUntil recCfg (isCreatedAt file) w
            createdRoots file evs1 `shouldBe` [root]

-- The scan events of a rename do not depend on its move events. The create
-- event of x finds x already watched and reports f again.
recRenameMovedToDisabled :: Spec
recRenameMovedToDisabled =
    it "reports the entries found by a rename without setMovedTo"
        $ withRoot cfg $ \root w -> do
            let dir1 = root </> "dir1"
                dir2 = root </> "dir2"
                file = dir2 </> "x" </> "f"
            createDirectory dir1
            waitWatched cfg w dir1
            evs <- recEventsOf cfg root w $ do
                renameDirectory dir1 dir2
                createDirectory (dir2 </> "x")
                createFile file
            sort (pathsOf Event.isScanEvent evs)
                `shouldBe` sort [dir2 </> "x", file, file]

    where

    cfg = Event.setMovedTo False recCfg

-- The reader thread is blocked reading the watch fd. With the non-threaded
-- RTS, closeWatch needs a pipe to wake up the reader, the pipe fails with
-- EMFILE, and the reader wakes up on the next event.
closeFailFallback :: Spec
closeFailFallback =
    it "closeWatch ends the stream without a free fd"
        $ withRoot watchCfg $ \root w -> do
            mv <- newEmptyMVar
            let reader = Stream.drain (Event.watchToStream watchCfg w)
            bracket
                (forkIO (try reader >>= putMVar mv))
                killThread
                $ \_ -> do
                    threadDelay 200000
                    r <- withFdsExhausted
                            (timeout 2000000 (Event.closeWatch w))
                    case r of
                        Nothing ->
                            pendingWith "the open file limit is too high"
                        Just c -> do
                            c `shouldBe` Just ()
                            when (not rtsSupportsBoundThreads) $ do
                                threadDelay 200000
                                done <- tryTakeMVar mv
                                fmap (const ()) done `shouldBe` Nothing
                            createFile (root </> "file")
                            -- The stream ends without an exception
                            res <- timeout 2000000 (takeMVar mv)
                            fmap (either (\(_ :: SomeException) -> False) ok)
                                res
                                `shouldBe` Just True

    where

    ok () = True

-------------------------------------------------------------------------------
-- Recursive mode robustness tests
-------------------------------------------------------------------------------

-- | @dir@ is a proper prefix of the path.
isStrictlyUnder :: FilePath -> FilePath -> Bool
isStrictlyUnder dir path = (dir ++ "/") `isPrefixOf` path

-- A subdirectory created while its parent is scanned is added by the scan and
-- on its create event, the second add finds the existing watch.
recSubdirAddedTwice :: Spec
recSubdirAddedTwice =
    it "does not fail when a subdirectory is added twice"
        $ withRoot recCfg $ \root w -> do
            let sentinel = root </> "sentinel"
                mkTree i = do
                    let dir = root </> ("dir" ++ show i)
                    createDirectory dir
                    mapM_ (\j -> createDirectory (dir </> show j))
                        [1 .. 50 :: Int]
                ops = mapM_ mkTree [1 .. 20 :: Int] >> createFile sentinel
            done <- newEmptyMVar
            bracket (forkIO (ops `finally` putMVar done ())) killThread
                $ \_ -> do
                    _ <- readUntil recCfg (isCreatedAt sentinel) w
                    takeMVar done

-- The directory is replaced by a file before its create event is read.
recDirReplacedByFile :: Spec
recDirReplacedByFile =
    it "does not watch a file that replaced a created directory"
        $ withRoot recCfg $ \root w -> do
            let dir = root </> "dir"
            createDirectory dir
            removeDirectory dir
            createFile dir
            waitWatched recCfg w dir
            evs <- recEventsOf recCfg root w (appendFile dir "x")
            pathsOf Event.isModified evs `shouldBe` [dir]

-- The directory is replaced by a symbolic link to a directory outside the
-- tree before its create event is read.
recDirReplacedBySymlink :: Spec
recDirReplacedBySymlink =
    it "does not follow a symlink that replaced a created directory"
        $ withRoot recCfg $ \root w -> do
            let dir = root </> "dir"
                out = takeDirectory root </> "out"
            createDirectory out
            createDirectory (out </> "sub")
            createDirectory dir
            removeDirectory dir
            createDirectoryLink out dir
            waitWatched recCfg w dir
            evs <- recEventsOf recCfg root w $ do
                createFile (out </> "file")
                createFile (out </> "sub" </> "file")
            filter (isStrictlyUnder dir) (map getAbsPath evs) `shouldBe` []

-- | Run the action in a recursive watch of @root@, then create a file @sync@
-- in @root@ and read the events up to its create event. The action runs
-- before the events are read, so its directories are added after all of its
-- changes are done.
recEventsBeforeAdd :: FilePath -> Event.Watch -> IO () -> IO [Event]
recEventsBeforeAdd root w action = do
    let sync = root </> "sync"
    action
    createFile sync
    readUntil recCfg (isCreatedAt sync) w

-- The events of a new directory before its watch is added are recovered by a
-- scan, the watch is added first, so the later events are reported as usual.
recScanNewDir :: Spec
recScanNewDir =
    it "reports the entries of a directory created before its watch is added"
        $ withRoot recCfg $ \root w -> do
            let dir = root </> "dir"
                sub = dir </> "sub"
                file = dir </> "file"
                file1 = sub </> "file1"
                link = dir </> "link"
                later = sub </> "later"
            evs <- recEventsBeforeAdd root w $ do
                createDirectory dir
                createDirectory sub
                createFile file
                createFile file1
                createDirectoryLink sub link
            sort (scannedPaths (const True) evs)
                `shouldBe` sort [sub, file, file1, link]
            scannedPaths Event.isDir evs `shouldBe` [sub]
            pathsOf
                (\ev -> Event.isCreated ev && not (Event.isScanEvent ev))
                evs
                `shouldBe` [dir, root </> "sync"]
            evs1 <- recEventsOf recCfg root w (createFile later)
            pathsOf Event.isCreated evs1
                `shouldBe` [later, root </> "sentinel"]

recScanDisabled :: Spec
recScanDisabled =
    it "does not report the entries of a new directory when disabled" $ do
        let cfg = Event.setScanNewDirs False recCfg
        withRoot cfg $ \root w -> do
            let dir = root </> "dir"
                sub = dir </> "sub"
                later = sub </> "later"
                sync = root </> "sync"
            createDirectory dir
            createDirectory sub
            createFile (dir </> "file")
            createFile sync
            evs <- readUntil cfg (isCreatedAt sync) w
            pathsOf Event.isCreated evs `shouldBe` [dir, sync]
            evs1 <- recEventsOf cfg root w (createFile later)
            pathsOf Event.isCreated evs1
                `shouldBe` [later, root </> "sentinel"]

-- The kernel watch generates IN_CREATE to add the new directory, the event
-- is not reported.
recScanCreatedDisabled :: Spec
recScanCreatedDisabled =
    it "does not report the scan when the create events are disabled" $ do
        let cfg = Event.setCreated False recCfg
        withRoot cfg $ \root w -> do
            let dir = root </> "dir"
                sub = dir </> "sub"
                later = sub </> "later"
            createDirectory dir
            createDirectory sub
            createFile (dir </> "file")
            writeFile (root </> "sync") "x"
            evs <- readUntil cfg Event.isModified w
            pathsOf Event.isScanEvent evs `shouldBe` []
            writeFile later "x"
            evs1 <- readUntil cfg Event.isModified w
            pathsOf Event.isModified evs1 `shouldBe` [later]

-- The listing of a new directory generates its events after the sentinel is
-- queued, they are read with the second sentinel. The watch of root keeps its
-- flags, it reports the listing of dir. dir does not report the listing of
-- itself or of sub, and sub does not report its own.
recScanNewDirSilent :: Spec
recScanNewDirSilent =
    it "the scan of a new directory is reported only by the parent watch"
        $ withRoot recCfg $ \root w -> do
            let dir = root </> "dir"
                sub = dir </> "sub"
                sentinel = root </> "sentinel"
                sentinel1 = root </> "sentinel1"
                isListing ev =
                       Event.isOpened ev
                    || Event.isAccessed ev
                    || Event.isNonWriteClosed ev
            createDirectory dir
            createDirectory sub
            createFile (sub </> "file")
            createFile sentinel
            evs <- readUntil recCfg (isCreatedAt sentinel) w
            createFile sentinel1
            evs1 <- readUntil recCfg (isCreatedAt sentinel1) w
            filter (isUnder dir) (map getAbsPath evs) `shouldContain` [sub]
            let listed = filter (isUnder dir) (pathsOf isListing (evs ++ evs1))
            filter (/= dir) listed `shouldBe` []
            -- open, one or two access events that the kernel may merge, close
            length listed `shouldSatisfy` (`elem` [3, 4])

-- The flags removed for the scan are restored after it.
recTouchAfterNewDirScan :: Spec
recTouchAfterNewDirScan =
    it "a new directory reports open events after its scan"
        $ withRoot recCfg $ \root w -> do
            let dir = root </> "dir"
                sub = dir </> "sub"
                sentinel = root </> "sentinel"
            createDirectory dir
            createDirectory sub
            evs <- recEventsOf recCfg root w (return ())
            pathsOf Event.isScanEvent evs `shouldBe` [sub]
            _ <- listDirectory sub
            createFile sentinel
            evs1 <- readUntil recCfg (\ev ->
                getAbsPath ev == sub && Event.isNonWriteClosed ev) w
            pathsOf Event.isOpened evs1 `shouldContain` [sub]

-- | Create @root/dir@ containing a file and read its events.
withWatchedNewDir :: (FilePath -> Event.Watch -> FilePath -> IO a) -> IO a
withWatchedNewDir action =
    withRoot recCfg $ \root w -> do
        let dir = root </> "dir"
            ready = root </> "ready"
        createDirectory dir
        createFile (dir </> "file")
        createFile ready
        _ <- readUntil recCfg (isCreatedAt ready) w
        action root w dir

recScanOnMoveIn :: Spec
recScanOnMoveIn =
    it "reports the entries of a directory moved in"
        $ withRoot recCfg $ \root w -> do
            let src = takeDirectory root </> "src"
                dir = root </> "dir"
                sub = dir </> "sub"
                file = sub </> "file"
                later = sub </> "later"
            createDirectory src
            createDirectory (src </> "sub")
            createFile (src </> "sub" </> "file")
            evs <- recEventsBeforeAdd root w (renameDirectory src dir)
            sort (scannedPaths (const True) evs) `shouldBe` sort [sub, file]
            evs1 <- recEventsOf recCfg root w (createFile later)
            pathsOf Event.isCreated evs1
                `shouldBe` [later, root </> "sentinel"]

recScanOnMoveInDisabled :: Spec
recScanOnMoveInDisabled =
    it "setScanNewDirs False watches a directory moved in, without reporting"
        $ withRoot cfg $ \root w -> do
            let src = takeDirectory root </> "src"
                dir = root </> "dir"
                later = dir </> "sub" </> "later"
            createDirectory src
            createDirectory (src </> "sub")
            createFile (src </> "file")
            evs <- recEventsOf cfg root w (renameDirectory src dir)
            pathsOf Event.isScanEvent evs `shouldBe` []
            createFile later
            evs1 <- readUntil cfg (isCreatedAt later) w
            pathsOf Event.isCreated evs1 `shouldBe` [later]

    where

    cfg = Event.setScanNewDirs False recCfg

-- The rename keeps the watch, and is reported as the tree leaving and coming
-- back in, the entries are reported after the move.
recScanOnRename :: Spec
recScanOnRename =
    it "reports the entries of a directory renamed within the tree"
        $ withWatchedNewDir $ \root w dir -> do
            let dir1 = root </> "dir1"
            evs <- recEventsBeforeAdd root w (renameDirectory dir dir1)
            scannedPaths (const True) (afterMovedTo dir1 evs)
                `shouldBe` [dir1 </> "file"]
            unwatched evs `shouldBe` []

-- The scan on the create event of dir finds the directory moved in, its
-- entries are reported before the delete event of dir. The later IN_MOVED_TO
-- scans dir again, the entries are reported after the move event too.
recScanCreatedFindsMovedIn :: Spec
recScanCreatedFindsMovedIn =
    it "reports after the move the entries of a directory moved in early"
        $ withRoot recCfg $ \root w -> do
            let src = takeDirectory root </> "src"
                dir = root </> "dir"
            createDirectory src
            createFile (src </> "file")
            evs <- recEventsBeforeAdd root w $ do
                createDirectory dir
                removeDirectory dir
                renameDirectory src dir
            scannedPaths (const True) evs
                `shouldBe` [dir </> "file", dir </> "file"]
            scannedPaths (const True) (afterMovedTo dir evs)
                `shouldBe` [dir </> "file"]

-- The first create event of dir adds and scans the second dir, its entries are
-- reported before the delete event of dir. The second create event finds dir
-- already watched and scans it again, the entries are reported after it too.
recScanCreatedAgain :: Spec
recScanCreatedAgain =
    it "reports after the second create the entries of a recreated directory"
        $ withRoot recCfg $ \root w -> do
            let dir = root </> "dir"
                file = dir </> "file"
            evs <- recEventsBeforeAdd root w $ do
                createDirectory dir
                removeDirectory dir
                createDirectory dir
                createFile file
            scannedPaths (const True) evs `shouldBe` [file, file]
            scannedPaths (const True) (afterDeleted dir evs) `shouldBe` [file]

    where

    afterDeleted path =
        drop 1 . dropWhile (\ev -> not (Event.isDeleted ev && isAt ev))

        where

        isAt ev = getAbsPath ev == path

-- The scan on the create event of dir fails, the path no longer exists. The
-- directory is scanned at its new path.
recScanCreatedThenRenamed :: Spec
recScanCreatedThenRenamed =
    it "reports the entries of a directory renamed before its create event"
        $ withRoot recCfg $ \root w -> do
            let dir = root </> "dir"
                dir1 = root </> "dir1"
                sub = dir1 </> "sub"
                file = sub </> "file"
                later = sub </> "later"
            evs <- recEventsBeforeAdd root w $ do
                createDirectory dir
                createDirectory (dir </> "sub")
                createFile (dir </> "sub" </> "file")
                renameDirectory dir dir1
            sort (scannedPaths (const True) evs) `shouldBe` sort [sub, file]
            scannedPaths Event.isDir evs `shouldBe` [sub]
            evs1 <- recEventsOf recCfg root w (createFile later)
            pathsOf Event.isCreated evs1
                `shouldBe` [later, root </> "sentinel"]

-- The scan at each intermediate path fails.
recScanCreatedRenamedTwice :: Spec
recScanCreatedRenamedTwice =
    it "reports the entries of a directory renamed twice before its events"
        $ withRoot recCfg $ \root w -> do
            let dir = root </> "dir"
                dir1 = root </> "dir1"
                dir2 = root </> "dir2"
            evs <- recEventsBeforeAdd root w $ do
                createDirectory dir
                createFile (dir </> "file")
                renameDirectory dir dir1
                renameDirectory dir1 dir2
            scannedPaths (const True) evs `shouldBe` [dir2 </> "file"]

-- The create event of the first dir adds the second dir, no scan fails. The
-- move event finds that dir1 is not the watched directory, and that dir is.
recScanCreatedRenamedPathReused :: Spec
recScanCreatedRenamedPathReused =
    it "reports a renamed new directory if its old path is reused"
        $ withRoot recCfg $ \root w -> do
            let dir = root </> "dir"
                dir1 = root </> "dir1"
                later = dir1 </> "later"
            evs <- recEventsBeforeAdd root w $ do
                createDirectory dir
                createFile (dir </> "file")
                renameDirectory dir dir1
                createDirectory dir
            scannedPaths (const True) evs `shouldBe` [dir1 </> "file"]
            evs1 <- recEventsOf recCfg root w (createFile later)
            pathsOf Event.isCreated evs1
                `shouldBe` [later, root </> "sentinel"]

-- dir does not exist when its move event is read, dir1 is scanned on its
-- move event.
recScanMovedInRenamed :: Spec
recScanMovedInRenamed =
    it "reports the entries of a directory moved in and renamed"
        $ withRoot recCfg $ \root w -> do
            let src = takeDirectory root </> "src"
                dir = root </> "dir"
                dir1 = root </> "dir1"
            createDirectory src
            createFile (src </> "file")
            evs <- recEventsBeforeAdd root w $ do
                renameDirectory src dir
                renameDirectory dir dir1
            scannedPaths (const True) evs `shouldBe` [dir1 </> "file"]

-- dir1 does not exist when its move event is read, dir2 is scanned on its move
-- event.
recScanCreatedPathReusedRenamed :: Spec
recScanCreatedPathReusedRenamed =
    it "reports a new directory renamed again if its old path is reused"
        $ withRoot recCfg $ \root w -> do
            let dir = root </> "dir"
                dir1 = root </> "dir1"
                dir2 = root </> "dir2"
            evs <- recEventsBeforeAdd root w $ do
                createDirectory dir
                createFile (dir </> "file")
                renameDirectory dir dir1
                createDirectory dir
                renameDirectory dir1 dir2
            scannedPaths (const True) evs `shouldBe` [dir2 </> "file"]

-- The watch of dir is removed on the first move event, dir1 does not exist by
-- then. The second move event adds dir and scans it, the entry is reported
-- again.
recScanMovedAwayAndBack :: Spec
recScanMovedAwayAndBack =
    it "reports the entries again of a directory renamed away and back"
        $ withWatchedNewDir $ \root w dir -> do
            let dir1 = root </> "dir1"
                later = dir </> "later"
            evs <- recEventsBeforeAdd root w $ do
                renameDirectory dir dir1
                renameDirectory dir1 dir
            scannedPaths (const True) evs `shouldBe` [dir </> "file"]
            evs1 <- recEventsOf recCfg root w (createFile later)
            pathsOf Event.isCreated evs1
                `shouldBe` [later, root </> "sentinel"]

-- dir1 is scanned on the first and the last move event, a scan can report an
-- entry more than once.
recScanMovedInAfterMoveAndBack :: Spec
recScanMovedInAfterMoveAndBack =
    it "reports a directory moved in after a rename away and back"
        $ withWatchedNewDir $ \root w dir -> do
            let src = takeDirectory root </> "src"
                dir1 = root </> "dir1"
            createDirectory src
            createFile (src </> "file")
            evs <- recEventsBeforeAdd root w $ do
                renameDirectory dir dir1
                renameDirectory dir1 dir
                renameDirectory src dir1
            nub (sort (scannedPaths (const True) evs))
                `shouldBe` sort [dir </> "file", dir1 </> "file"]

-- | Check that a directory renamed from @a@ to @b@ to @c@ before the events
-- are read reports both the move to and the move from the intermediate path.
checkIntermediatePath ::
    FilePath -> FilePath -> FilePath -> [Event] -> Expectation
checkIntermediatePath a b c evs = do
    pathsOf (dirEvent Event.isMovedFrom) evs `shouldBe` [a, b]
    pathsOf (dirEvent Event.isMovedTo) evs `shouldBe` [b, c]
    pathsOf (dirEvent Event.isMoved) evs `shouldBe` [a, b, b, c]

recIntermediatePath :: Spec
recIntermediatePath =
    describe "an intermediate path of two renames reports both moves" $ do
        it "new directory"
            $ withRoot recCfg $ \root w -> do
                let (a, b, c) = paths root
                evs <- recEventsBeforeAdd root w $ do
                    createDirectory a
                    createFile (a </> "file")
                    renameDirectory a b
                    renameDirectory b c
                checkIntermediatePath a b c evs
        it "watched directory"
            $ withWatchedNewDir $ \root w a -> do
                let (_, b, c) = paths root
                evs <- recEventsBeforeAdd root w $ do
                    renameDirectory a b
                    renameDirectory b c
                checkIntermediatePath a b c evs
        it "directory moved in"
            $ withRoot recCfg $ \root w -> do
                let (a, b, c) = paths root
                    src = takeDirectory root </> "src"
                createDirectory src
                evs <- recEventsBeforeAdd root w $ do
                    renameDirectory src a
                    renameDirectory a b
                    renameDirectory b c
                checkIntermediatePath' a b c evs

    where

    paths root = (root </> "dir", root </> "dir1", root </> "dir2")

    -- The move in has no move from.
    checkIntermediatePath' a b c evs = do
        pathsOf (dirEvent Event.isMovedFrom) evs `shouldBe` [a, b]
        pathsOf (dirEvent Event.isMovedTo) evs `shouldBe` [a, b, c]

recScanCreatedThenMovedOut :: Spec
recScanCreatedThenMovedOut =
    it "does not report the entries of a new directory moved out of the tree"
        $ withRoot recCfg $ \root w -> do
            let dir = root </> "dir"
                out = takeDirectory root </> "out"
            evs <- recEventsBeforeAdd root w $ do
                createDirectory dir
                createFile (dir </> "file")
                renameDirectory dir out
            pathsOf Event.isScanEvent evs `shouldBe` []

-- | Run the action on the watched @dir1@ containing @sub@, before the events
-- are read, with the directory @dir@ as the path that it reuses. Check that
-- @dir@ and @sub@ then report each event once. Returns all the events.
checkReusedPath ::
    (FilePath -> FilePath -> FilePath -> IO ()) -> IO [Event]
checkReusedPath action =
    withRenameDirs $ \root w dir1 _ -> do
        let dir = root </> "dir"
            later = dir </> "later"
            subLater = dir </> "sub" </> "later"
        evs <-
            recEventsBeforeAdd root w
                (action (takeDirectory root </> "out") dir dir1)
        evs1 <- recEventsOf recCfg root w $ do
            createFile later
            createFile subLater
        pathsOf Event.isCreated evs1
            `shouldBe` [later, subLater, root </> "sentinel"]
        return (evs ++ evs1)

-- IN_CREATE of dir finds the watch descriptor of dir1 and adds dir as its
-- second path, IN_MOVED_TO of the renamed new directory removes that path.
-- dir1 is renamed to dir on its own IN_MOVED_TO, and stays watched.
--
-- XXX isRootUnwatched of dir and dir/sub is reported after the rename to dir,
-- although the kernel watch was not removed. This test records the current
-- behavior, see "Reused paths" in dev/design.md.
recReusedPathMovedWithin :: Spec
recReusedPathMovedWithin =
    it "keeps a watched directory moved to a renamed new directory's path" $ do
        evs <- checkReusedPath $ \_ dir dir1 -> do
            createDirectory dir
            renameDirectory dir (dir ++ "-renamed")
            renameDirectory dir1 dir
        map (takeFileName . snd) (unwatched evs) `shouldBe` ["dir", "sub"]

-- No event removes the second path of dir1 added by IN_CREATE of dir, the
-- rename of dir1 to dir must not leave the path twice.
recReusedPathDeleted :: Spec
recReusedPathDeleted =
    it "keeps a watched directory moved to a path of a deleted new directory"
        $ void $ checkReusedPath $ \_ dir dir1 -> do
            createDirectory dir
            removeDirectory dir
            renameDirectory dir1 dir

-- IN_MOVED_FROM of dir records its cookie in the second path of dir1, no
-- IN_MOVED_TO removes it. IN_MOVE_SELF of dir1 must not remove dir, dir1 was
-- not moved out.
recReusedPathMovedOut :: Spec
recReusedPathMovedOut =
    it "keeps a watched directory moved to a path of a moved out new directory"
        $ void $ checkReusedPath $ \out dir dir1 -> do
            createDirectory dir
            renameDirectory dir out
            renameDirectory dir1 dir

-- IN_MOVE_SELF of dir1 removes its watch when it is read, the kernel queues
-- IN_IGNORED after the IN_CREATE event of the new dir1. isRootUnwatched refers
-- to the directory that was at the path, the new dir1 is watched. This test
-- records the current behavior, see "Reused paths" in dev/design.md.
recUnwatchedAfterRecreated :: Spec
recUnwatchedAfterRecreated =
    it "reports isRootUnwatched of a moved out directory after a path reuse"
        $ withRenameDirs $ \root w dir1 _ -> do
            let later = dir1 </> "later"
                isAtDir ev =
                    getAbsPath ev == dir1
                        && (Event.isCreated ev || Event.isRootUnwatched ev)
            evs <- recEventsBeforeAdd root w $ do
                renameDirectory dir1 (takeDirectory root </> "out")
                createDirectory dir1
            evs1 <- recEventsOf recCfg root w (createFile later)
            map Event.isRootUnwatched (filter isAtDir (evs ++ evs1))
                `shouldBe` [False, True]
            pathsOf Event.isCreated evs1
                `shouldBe` [later, root </> "sentinel"]

-- rename(2) replaces the empty dir. IN_CREATE of dir adds it as the second
-- path of dir1, the same as a deleted new directory.
recReusedPathReplaced :: Spec
recReusedPathReplaced =
    it "keeps a watched directory moved over an empty new directory"
        $ void $ checkReusedPath $ \_ dir dir1 -> do
            createDirectory dir
            renameDirectory dir1 dir

-- IN_CREATE of dir finds src and scans it. IN_MOVED_FROM of dir then tells the
-- consumer that the path and the entries under it are gone. IN_MOVED_TO finds
-- the path already watched. The entries must be reported after IN_MOVED_TO.
recReusedPathMovedIn :: Spec
recReusedPathMovedIn =
    it "reports a directory moved in at a path of a moved out new directory"
        $ withRoot recCfg $ \root w -> do
            let src = takeDirectory root </> "src"
                out = takeDirectory root </> "out"
                dir = root </> "dir"
            createDirectory src
            createFile (src </> "file")
            evs <- recEventsBeforeAdd root w $ do
                createDirectory dir
                renameDirectory dir out
                renameDirectory src dir
            let isMovedToDir ev = Event.isMovedTo ev && getAbsPath ev == dir
            scannedPaths (const True) (dropWhile (not . isMovedToDir) evs)
                `shouldBe` [dir </> "file"]

-- The second rename leaves the cookie of the moved out dir on the path dir, as
-- the only path of dir1. IN_MOVE_SELF of dir1 must not remove dir.
recReusedPathMovedOutRenamedTwice :: Spec
recReusedPathMovedOutRenamedTwice =
    it "keeps a directory renamed twice to a path of a moved out new directory"
        $ void $ checkReusedPath $ \out dir dir1 -> do
            let dir2 = dir1 ++ "-renamed"
            createDirectory dir
            renameDirectory dir1 dir2
            renameDirectory dir out
            renameDirectory dir2 dir

-- The add of sub on its IN_CREATE finds the symbolic link loop at its parent,
-- inotify_add_watch fails with ELOOP. The stream must not raise an exception.
-- The rename reports the entries of dir2, nothing is reported through the
-- loop.
recParentReplacedBySymlinkLoop :: Spec
recParentReplacedBySymlinkLoop =
    it "ignores a new directory whose parent is replaced by a symlink loop"
        $ withRenameDirs $ \root w dir1 dir2 -> do
            evs <- recEventsBeforeAdd root w $ do
                createDirectory (dir1 </> "new")
                renameDirectory dir1 dir2
                createFileLink dir1 dir1
            sort (scannedPaths (const True) evs)
                `shouldBe` sort [dir2 </> "new", dir2 </> "sub"]

-- IN_DONT_FOLLOW covers only the last component of a path. The add of new on
-- its IN_CREATE goes through the symbolic link at its parent, to a directory
-- outside the tree. That directory must not be reported.
recParentReplacedBySymlink :: Spec
recParentReplacedBySymlink =
    it "does not watch outside the tree via a symlink at a new dir's parent"
        $ withRenameDirs $ \root w dir1 dir2 -> do
            let outside = takeDirectory root </> "outside"
                inNew = dir2 </> "new" </> "later"
            createDirectory outside
            createDirectory (outside </> "new")
            _ <- recEventsBeforeAdd root w $ do
                createDirectory (dir1 </> "new")
                renameDirectory dir1 dir2
                createFileLink outside dir1
            evs1 <- recEventsOf recCfg root w $ do
                createFile (outside </> "new" </> "stray")
                createFile inNew
            pathsOf Event.isCreated evs1
                `shouldBe` [inNew, root </> "sentinel"]

recScanCreatedRenamedDisabled :: Spec
recScanCreatedRenamedDisabled =
    it "setScanNewDirs False does not report a renamed new directory"
        $ withRoot cfg $ \root w -> do
            let dir = root </> "dir"
                dir1 = root </> "dir1"
            evs <- recEventsOf cfg root w $ do
                createDirectory dir
                createFile (dir </> "file")
                renameDirectory dir dir1
            pathsOf Event.isScanEvent evs `shouldBe` []

    where

    cfg = Event.setScanNewDirs False recCfg

-- The rename keeps the watches. The walk on IN_MOVED_TO runs, its entries are
-- not reported.
recScanOnRenameDisabled :: Spec
recScanOnRenameDisabled =
    it "setScanNewDirs False keeps the watches of a renamed directory"
        $ withRoot cfg $ \root w -> do
            let dir = root </> "dir"
                dir1 = root </> "dir1"
                later = dir1 </> "sub" </> "later"
            createDirectory dir
            createDirectory (dir </> "sub")
            createFile (dir </> "file")
            waitWatched cfg w dir
            evs <- recEventsOf cfg root w (renameDirectory dir dir1)
            pathsOf Event.isScanEvent evs `shouldBe` []
            unwatched evs `shouldBe` []
            createFile later
            evs1 <- readUntil cfg (isCreatedAt later) w
            pathsOf Event.isCreated evs1 `shouldBe` [later]

    where

    cfg = Event.setScanNewDirs False recCfg

-- dir is recorded by its failed scan, the directory moved to the same path is
-- a different directory.
recScanNotOnMoveToMissedPath :: Spec
recScanNotOnMoveToMissedPath =
    it "does not report a directory moved in at a path whose scan failed"
        $ withRoot recCfg $ \root w -> do
            let src = takeDirectory root </> "src"
                dir = root </> "dir"
                dir1 = root </> "dir1"
            createDirectory src
            createFile (src </> "file")
            evs <- recEventsBeforeAdd root w $ do
                createDirectory dir
                removeDirectory dir
                renameDirectory src dir
                renameDirectory dir dir1
            pathsOf Event.isScanEvent evs `shouldBe` []

-- The first create event adds the second directory, the second create event
-- finds it already watched and reports its entries again.
recDirRecreated :: Spec
recDirRecreated =
    it "watches a directory deleted and created again before its create event"
        $ withRoot recCfg $ \root w -> do
            let dir = root </> "dir"
                file = dir </> "sub" </> "file"
            evs <- recEventsBeforeAdd root w $ do
                createDirectory dir
                removeDirectory dir
                createDirectory dir
                createDirectory (dir </> "sub")
            pathsOf (dirEvent Event.isCreated) evs
                `shouldBe` [dir, dir </> "sub", dir, dir </> "sub"]
            pathsOf (dirEvent Event.isDeleted) evs `shouldBe` [dir]
            evs1 <- recEventsOf recCfg root w (createFile file)
            pathsOf Event.isCreated evs1
                `shouldBe` [file, root </> "sentinel"]

-- The move event of the first directory removes the watch of the second
-- directory, the second create event adds it again.
recDirMovedAndRecreated :: Spec
recDirMovedAndRecreated =
    it "watches a directory moved and created again before its create event"
        $ withRoot recCfg $ \root w -> do
            let dir = root </> "dir"
                dir1 = root </> "dir1"
                file = dir </> "sub" </> "file"
                file1 = dir1 </> "sub" </> "file"
            _ <- recEventsBeforeAdd root w $ do
                createDirectory dir
                createDirectory (dir </> "sub")
                renameDirectory dir dir1
                createDirectory dir
                createDirectory (dir </> "sub")
            evs <- recEventsOf recCfg root w $ do
                createFile file
                createFile file1
            pathsOf Event.isCreated evs
                `shouldBe` [file, file1, root </> "sentinel"]

-- The create event of dir finds the watch of dir2, moved to dir. The later
-- move events remove dir2 and add it again as dir.
recDirReplacedByWatched :: Spec
recDirReplacedByWatched =
    it "watches a directory moved to the path of a created directory"
        $ withRoot recCfg $ \root w -> do
            let dir = root </> "dir"
                dir1 = root </> "dir1"
                dir2 = root </> "dir2"
                file = dir </> "sub" </> "file"
            createDirectory dir2
            createDirectory (dir2 </> "sub")
            waitWatched recCfg w dir2
            _ <- recEventsBeforeAdd root w $ do
                createDirectory dir
                renameDirectory dir dir1
                renameDirectory dir2 dir
            evs <- recEventsOf recCfg root w (createFile file)
            pathsOf Event.isCreated evs
                `shouldBe` [file, root </> "sentinel"]
            -- The scans and the removals made while reading the events of
            -- the action queue events with the paths under dir2, check only
            -- the events of the file.
            filter (== dir2 </> "sub" </> "file") (map getAbsPath evs)
                `shouldBe` []

-- | Run the action after overflowing the event queue of the watch, by
-- creating more files in @dir@ than the queue can hold. Returns 'Nothing' if
-- the queue limit is more than 100000 events.
withOverflow :: FilePath -> IO a -> IO (Maybe a)
withOverflow dir action = do
    limit <- read <$> readFile "/proc/sys/fs/inotify/max_queued_events"
    if limit > (100000 :: Int)
    then return Nothing
    else do
        mapM_ (\i -> createFile (dir </> show i)) [1 .. limit + 1000]
        Just <$> action

-- The kernel drops the IN_IGNORED event of the removed root when the queue
-- is full.
overflowRemove :: Spec
overflowRemove =
    it "removeFromWatch reports isRootUnwatched after a queue overflow"
        $ withTwoRoots $ \root1 root2 w -> do
            p <- Path.fromString root2
            r <- withOverflow root1 (Event.removeFromWatch w p)
            case r of
                Nothing -> pendingWith "max_queued_events is too high"
                Just () -> do
                    evs <- readUntil watchCfg Event.isRootUnwatched w
                    any Event.isEventsLost evs `shouldBe` True
                    unwatched evs `shouldBe` [(root2, root2)]

-- The marker of the shared watch descriptor of sub is dropped when the queue
-- is full.
overflowRemoveShared :: Spec
overflowRemoveShared =
    it "removing a shared root stops its events after a queue overflow"
        $ withNestedRoots $ \root sub w -> do
            p <- Path.fromString sub
            r <- withOverflow root (Event.removeFromWatch w p)
            case r of
                Nothing -> pendingWith "max_queued_events is too high"
                Just () -> do
                    evs <- readUntil recCfg Event.isRootUnwatched w
                    any Event.isEventsLost evs `shouldBe` True
                    unwatched evs `shouldBe` [(sub, sub)]
                    let file = sub </> "file"
                    createFile file
                    evs1 <- readUntil recCfg (isCreatedAt file) w
                    createdRoots file evs1 `shouldBe` [root]

noRestartCfg :: Event.Config
noRestartCfg = Event.setRestartOnOverflow False recCfg

-- The scan at the start of a watch lists root and sub.
recStartScanSilent :: Spec
recStartScanSilent =
    it "the start of a recursive watch reports no events of its scan"
        $ withSystemTempDirectory "fsevent_dir" $ \dir -> do
            let root = dir </> "root"
                sentinel = root </> "sentinel"
            createDirectory root
            createDirectory (root </> "sub")
            p <- Path.fromString root
            bracket
                (Event.openWatch recCfg (p NonEmpty.:| []))
                Event.closeWatch
                $ \w -> do
                    createFile sentinel
                    evs <- readUntil recCfg (isCreatedAt sentinel) w
                    map getAbsPath evs `shouldBe` [sentinel]

-- The events not requested during the scan are requested after it.
recTouchAfterScan :: Spec
recTouchAfterScan =
    it "a directory in the tree reports open events after the watch starts"
        $ withSystemTempDirectory "fsevent_dir" $ \dir -> do
            let root = dir </> "root"
                sub = root </> "sub"
            createDirectory root
            createDirectory sub
            p <- Path.fromString root
            bracket
                (Event.openWatch recCfg (p NonEmpty.:| []))
                Event.closeWatch
                $ \w -> do
                    _ <- listDirectory sub
                    evs <- readUntil recCfg (\ev ->
                        getAbsPath ev == sub && Event.isNonWriteClosed ev) w
                    pathsOf Event.isOpened evs `shouldContain` [sub]

-- The kernel drops the create event of the directory when the queue is full.
overflowRecCreate :: Spec
overflowRecCreate =
    it "watches a directory created during an overflow"
        $ withRoot recCfg $ \root w -> do
            let dir = root </> "dir"
                file = dir </> "sub" </> "file"
            r <- withOverflow root $ do
                createDirectory dir
                createDirectory (dir </> "sub")
            case r of
                Nothing -> pendingWith "max_queued_events is too high"
                Just () -> do
                    _ <- readUntil recCfg Event.isEventsLost w
                    createFile file
                    evs <- readUntil recCfg (isCreatedAt file) w
                    createdRoots file evs `shouldBe` [root]

-- The kernel drops the create events of the directories when the queue is
-- full, the restart reports them.
overflowRecCreateReported :: Spec
overflowRecCreateReported =
    it "setRestartOnOverflow reports a directory created during an overflow"
        $ withRoot recCfg $ \root w -> do
            let dir = root </> "dir"
                sub = dir </> "sub"
            r <- withOverflow root $ do
                createDirectory dir
                createDirectory sub
            case r of
                Nothing -> pendingWith "max_queued_events is too high"
                Just () -> do
                    evs <- readUntil recCfg (isCreatedAt sub) w
                    scannedPaths Event.isDir evs `shouldBe` [dir, sub]
                    kernelCreated (filter Event.isDir evs) `shouldBe` []

-- The kernel drops the move events of the directory when the queue is full.
overflowRecRename :: Spec
overflowRecRename =
    it "setRestartOnOverflow reports the new path of a renamed directory"
        $ withRootDir $ \root w -> do
            let dir = root </> "dir"
                dir1 = root </> "dir1"
                file = dir1 </> "file"
            r <- withOverflow root (renameDirectory dir dir1)
            case r of
                Nothing -> pendingWith "max_queued_events is too high"
                Just () -> do
                    evs0 <- readUntil recCfg (isCreatedAt dir1) w
                    scannedPaths Event.isDir evs0 `shouldBe` [dir1]
                    createFile file
                    evs <- readUntil recCfg (isCreatedAt file) w
                    createdRoots file evs `shouldBe` [root]
                    pathsOf Event.isCreated evs
                        `shouldNotContain` [dir </> "file"]

    where

    withRootDir action =
        withSystemTempDirectory "fsevent_dir" $ \fp -> do
            let root = fp </> "root"
            createDirectory root
            createDirectory (root </> "dir")
            p <- Path.fromString root
            bracket
                (Event.openWatch recCfg (p NonEmpty.:| []))
                Event.closeWatch
                (action root)

-- A directory moved out of the tree during the overflow.
overflowRecMoveOut :: Spec
overflowRecMoveOut =
    it "setRestartOnOverflow removes a directory moved out of the tree"
        $ withSystemTempDirectory "fsevent_dir" $ \fp -> do
            let root = fp </> "root"
                dir = root </> "dir"
                outside = fp </> "outside"
                sentinel = root </> "sentinel"
            createDirectory root
            createDirectory dir
            p <- Path.fromString root
            bracket
                (Event.openWatch recCfg (p NonEmpty.:| []))
                Event.closeWatch
                $ \w -> do
                    r <- withOverflow root (renameDirectory dir outside)
                    case r of
                        Nothing ->
                            pendingWith "max_queued_events is too high"
                        Just () -> do
                            _ <- readUntil recCfg Event.isRootUnwatched w
                            createFile (outside </> "file")
                            createFile sentinel
                            evs <- readUntil recCfg
                                    (isCreatedAt sentinel) w
                            pathsOf Event.isCreated evs `shouldBe` [sentinel]

-- The kernel drops the IN_MOVE_SELF event of the root when the queue is full.
overflowRecRootRenamed :: Spec
overflowRecRootRenamed =
    it "setRestartOnOverflow removes a root renamed during an overflow"
        $ withSystemTempDirectory "fsevent_dir" $ \fp -> do
            let root = fp </> "root"
                root1 = fp </> "root1"
                sub = root </> "sub"
            createDirectory root
            createDirectory sub
            p <- Path.fromString root
            bracket
                (Event.openWatch recCfg (p NonEmpty.:| []))
                Event.closeWatch
                $ \w -> do
                    r <- withOverflow root (renameDirectory root root1)
                    case r of
                        Nothing ->
                            pendingWith "max_queued_events is too high"
                        Just () -> do
                            evs <- readUntil recCfg (\ev ->
                                Event.isRootUnwatched ev
                                    && getAbsPath ev == sub) w
                            unwatched evs
                                `shouldBe` [(root, root), (root, sub)]

-- The kernel drops the IN_IGNORED events of the deleted root and its
-- subdirectory when the queue is full.
overflowRecRootDeleted :: Spec
overflowRecRootDeleted =
    it "setRestartOnOverflow removes a root deleted during an overflow"
        $ withSystemTempDirectory "fsevent_dir" $ \fp -> do
            let root1 = fp </> "root1"
                root2 = fp </> "root2"
                sub = root2 </> "sub"
            createDirectory root1
            createDirectory root2
            createDirectory sub
            roots <- mapM Path.fromString [root1, root2]
            bracket
                (Event.openWatch recCfg (NonEmpty.fromList roots))
                Event.closeWatch
                $ \w -> do
                    r <- withOverflow root1 $ do
                        removeDirectory sub
                        removeDirectory root2
                    case r of
                        Nothing ->
                            pendingWith "max_queued_events is too high"
                        Just () -> do
                            evs <- readUntil recCfg (\ev ->
                                Event.isRootUnwatched ev
                                    && getAbsPath ev == sub) w
                            unwatched evs
                                `shouldBe` [(root2, root2), (root2, sub)]

overflowRecCreateNoRestart :: Spec
overflowRecCreateNoRestart =
    it "does not watch a directory created during an overflow if disabled"
        $ withRoot noRestartCfg $ \root w -> do
            let dir = root </> "dir"
                file = dir </> "file"
                sentinel = root </> "sentinel"
            r <- withOverflow root (createDirectory dir)
            case r of
                Nothing -> pendingWith "max_queued_events is too high"
                Just () -> do
                    _ <- readUntil noRestartCfg Event.isEventsLost w
                    createFile file
                    createFile sentinel
                    evs <- readUntil noRestartCfg (isCreatedAt sentinel) w
                    createdRoots file evs `shouldBe` []

-------------------------------------------------------------------------------
-- Event path tests
-------------------------------------------------------------------------------

getRelPath :: Event -> Maybe FilePath
getRelPath = fmap Path.toString . Event.getRelPath

-- | The events whose 'getRelPath' is 'Nothing' must have 'getAbsPath' equal
-- to 'getRoot'.
shouldHaveRootPaths :: [Event] -> Expectation
shouldHaveRootPaths evs =
    map getAbsPath rootEvs `shouldBe` map getRoot rootEvs

    where

    rootEvs = filter ((== Nothing) . getRelPath) evs

rootEventPaths :: Spec
rootEventPaths =
    it "getRelPath is Nothing and getAbsPath is getRoot for the root"
        $ withRoot watchCfg $ \root w -> do
            let file = root </> "file"
            createFile file
            removeFile file
            removeDirectory root
            evs <- readUntilUnwatched w
            map getRelPath (filter (isCreatedAt file) evs)
                `shouldBe` [Just "file"]
            map getRelPath (filter Event.isRootDeleted evs)
                `shouldBe` [Nothing]
            shouldHaveRootPaths evs

-- The overflow events are not traced, the queue holds thousands of events.
overflowPaths :: Spec
overflowPaths =
    it "reports isEventsLost for each root with the root as its path"
        $ withTwoRoots $ \root1 root2 w -> do
            r <- withOverflow root1 (return ())
            case r of
                Nothing -> pendingWith "max_queued_events is too high"
                Just () -> do
                    r1 <- timeout 10000000
                        $ Stream.toList
                        $ Stream.take 2
                        $ Stream.filter Event.isEventsLost
                        $ Event.watchToStream watchCfg w
                    evs <- maybe (error "overflowPaths: timed out") return r1
                    sort (map getRoot evs) `shouldBe` [root1, root2]
                    map getRelPath evs `shouldBe` [Nothing, Nothing]
                    shouldHaveRootPaths evs

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
                overlapMoveOutInner
                duplicateRoot
            describe "renamed roots" $ do
                rootRenamed
                rootRenamedUnrequested
                recRootRenamed
                rootRenamedBack
                rootHardLinkRenamed
            describe "watch descriptors" largeWd
            describe "recursive mode" $ do
                recSubdirSelfEvents
                recMoveOutSpec
                recMoveOutUnrequested
                recMoveIn
                recRenameWithin
            describe "recursive mode rename races" $ do
                recRenameCreateBeforeRead
                recRenameCreateInGap
                recRenameNoUnwatched
                recRenameCreateBeforeRename
                recRenameTwice
                recRenameScanOnMovedTo
                recRenameDuringScan
                recMoveOutNoFd
                recDirMovedOutAndRecreated
                recRenameOutOfInnerRoot
                recRenameMovedToDisabled
            describe "resources" $ do
                openWatchNoLeak
                cloexecWatch
            describe "async exceptions" $ do
                killWaitingReader
                killStartingWatch
                killRepeatedly
                killConsumer
                abandonStream
            describe "concurrency" $ do
                addWhileReading
                removeWhileReading
                closeWhileReading
            describe "failures" $ do
                removeFailRetry
                closeFailFallback
            describe "queue overflow" $ do
                overflowRemove
                overflowRemoveShared
                overflowRecCreate
                overflowRecCreateReported
                overflowRecCreateNoRestart
                overflowRecRename
                overflowRecMoveOut
                overflowRecRootRenamed
                overflowRecRootDeleted
            describe "event paths" $ do
                rootEventPaths
                overflowPaths
            describe "recursive mode robustness" $ do
                recSubdirAddedTwice
                recDirReplacedByFile
                recDirReplacedBySymlink
                recDirRecreated
                recDirMovedAndRecreated
                recDirReplacedByWatched
            describe "recursive mode scan of new directories" $ do
                recScanNewDir
                recScanDisabled
                recScanCreatedDisabled
                recScanNewDirSilent
                recTouchAfterNewDirScan
                recScanOnMoveIn
                recScanOnMoveInDisabled
                recScanOnRename
                recScanCreatedFindsMovedIn
                recScanCreatedAgain
                recScanCreatedThenRenamed
                recScanCreatedRenamedTwice
                recScanCreatedRenamedPathReused
                recScanMovedInRenamed
                recScanCreatedPathReusedRenamed
                recScanMovedAwayAndBack
                recScanMovedInAfterMoveAndBack
                recIntermediatePath
                recScanCreatedThenMovedOut
                recScanCreatedRenamedDisabled
                recScanOnRenameDisabled
            describe "recursive mode reused paths" $ do
                recReusedPathMovedWithin
                recUnwatchedAfterRecreated
                recReusedPathDeleted
                recReusedPathMovedOut
                recReusedPathReplaced
                recReusedPathMovedIn
                recReusedPathMovedOutRenamedTwice
                recParentReplacedBySymlinkLoop
                recParentReplacedBySymlink
            describe "recursive mode scan of a tree" $ do
                recStartScanSilent
                recTouchAfterScan
            describe "defaultConfig" $ do
                defaultExcludeUnlinked
                defaultAttrsModified
                lastRootEvent

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
                , (path, rootEvent Event.isRootDeleted)
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
            : dirMove "dir1" "dir2" dirMoveEvents
            : dirMove "subdir/dir1" "subdir/dir2" dirMoveEvents
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
