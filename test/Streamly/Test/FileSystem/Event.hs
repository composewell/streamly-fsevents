-- |
-- Module      : Streamly.Test.FileSystem.Event
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Tests of the portable API, with the same expected events on all the
-- platforms.

module Streamly.Test.FileSystem.Event (main) where

import Control.Concurrent
    (Chan, forkIO, killThread, newChan, readChan, writeChan)
import Control.Exception
    (SomeException, bracket, finally, throwIO, toException, try)
import Control.Monad (forM_)
import Data.List (isPrefixOf, nub, sort, (\\))
import Streamly.Internal.FS.Event (Config, Event)
import System.Directory
    ( createDirectory
    , createDirectoryLink
    , createFileLink
    , removeDirectory
    , renameDirectory
    )
import System.FilePath ((</>), pathSeparator, takeDirectory, takeFileName)
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Hspec

import qualified Data.List.NonEmpty as NonEmpty
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.FS.Event as Event
import qualified Streamly.Internal.FileSystem.Path as Path
#if defined(CABAL_OS_LINUX)
import qualified Streamly.Internal.FS.Event.Linux as Linux
#endif

import Streamly.Test.FileSystem.Event.Common

moduleName :: String
moduleName = "FS.Event"

-------------------------------------------------------------------------------
-- Event helpers
-------------------------------------------------------------------------------

absPath :: Event -> FilePath
absPath = Path.toString . Event.getAbsPath

rootPath :: Event -> FilePath
rootPath = Path.toString . Event.getRoot

relPath :: Event -> Maybe FilePath
relPath = fmap Path.toString . Event.getRelPath

isCreatedAt :: FilePath -> Event -> Bool
isCreatedAt file ev = Event.isCreated ev && absPath ev == file

-- | The paths of the events that satisfy the predicate.
pathsOf :: (Event -> Bool) -> [Event] -> [FilePath]
pathsOf p = map absPath . filter p

-- | Whether the event reports a change of an entry.
isChange :: Event -> Bool
isChange ev =
       Event.isCreated ev
    || Event.isDeleted ev
    || Event.isMoved ev
    || Event.isModified ev

-- | The events checked for a path that must not be reported. With @True@, on
-- Linux, every event is checked, including the open, access and
-- @isRootUnwatched@ events. Otherwise only the changes are checked: on Linux
-- the @isRootUnwatched@ event of a directory removed from the watch can be
-- read after the events of later changes.
checkedEvents :: Bool -> [Event] -> [Event]
#if defined(CABAL_OS_LINUX)
checkedEvents everyEvent = if everyEvent then id else filter isChange
#else
checkedEvents _ = filter isChange
#endif

-- | @dir@ or a path under it.
isUnder :: FilePath -> FilePath -> Bool
isUnder dir path = path == dir || (dir ++ [pathSeparator]) `isPrefixOf` path

-- | The 'Event.isCreated' events must be for the given paths, one for each
-- path. On Linux they must be in the order of the paths, the order in which
-- the entries were created.
--
-- On macOS a later event for a path can repeat 'Event.isCreated', there must
-- be at least one for each path, and only the events of the given paths are
-- checked, the markers of 'waitStarted' and 'syncOn' can be reported again.
shouldCreate :: [Event] -> [FilePath] -> Expectation
shouldCreate evs paths =
#if defined(CABAL_OS_LINUX)
    created `shouldBe` paths
#elif defined(CABAL_OS_DARWIN)
    nub (sort (filter (`elem` paths) created)) `shouldBe` nub (sort paths)
#else
    sort created `shouldBe` sort paths
#endif

    where

    created = pathsOf Event.isCreated evs

createFile :: FilePath -> IO ()
createFile file = writeFile file ""

onWindows :: Bool
#if defined(CABAL_OS_WINDOWS)
onWindows = True
#else
onWindows = False
#endif

-- | Run the test except on Windows.
notOnWindows :: String -> IO () -> IO ()
notOnWindows reason test =
    if onWindows then pendingWith reason else test

-------------------------------------------------------------------------------
-- Running a watch
-------------------------------------------------------------------------------

-- | The events of a watch, written by the thread that reads the stream. The
-- exception that ends the stream, or an exception for the end of the stream,
-- is the last item.
data Watch = Watch
    { watchEvents :: Chan (Either SomeException Event)
    -- | The events read by 'waitStarted'.
    , startEvents :: [Event]
    }

-- | The next event of the watch. Throw an exception if the stream ended.
nextEvent :: Watch -> IO Event
nextEvent w = readChan (watchEvents w) >>= either throwIO return

-- | Read the events of the watch up to and including the first event that
-- satisfies the predicate. Throw an exception if it does not occur within 10
-- seconds.
readUntil :: (Event -> Bool) -> Watch -> IO [Event]
readUntil p w = do
    r <- timeout 10000000 (go [])
    maybe (error "readUntil: timed out") return r

    where

    go acc = do
        ev <- nextEvent w
        if p ev
        then return (reverse (ev : acc))
        else go (ev : acc)

-- | Read the events that arrive within 100 ms of each other.
readAvailable :: Watch -> IO [Event]
readAvailable w = go []

    where

    go acc = do
        r <- timeout 100000 (nextEvent w)
        maybe (return (reverse acc)) (go . (: acc)) r

-- | Create a file @startN@ in each root until the watch reports one in every
-- root, then create a file @ready@ in each root and read the events up to the
-- last of them, and the events that follow within 100 ms. A watch is not
-- known to have started until it reports an event, on macOS the start is
-- asynchronous. Returns the events read.
waitStarted :: [FilePath] -> Watch -> IO [Event]
waitStarted roots w = start (0 :: Int) (nub roots) []

    where

    start n notSeen acc
        | null notSeen = ready (nub roots) acc
        | n >= 100 = error "waitStarted: the watch did not start"
        | otherwise = do
            forM_ notSeen $ \r -> createFile (r </> ("start" ++ show n))
            evs <- readAvailable w
            let isStart ev = "start" `isPrefixOf` takeFileName (absPath ev)
                seen = map rootPath (filter isStart evs)
            start (n + 1) (notSeen \\ seen) (acc ++ evs)

    ready notSeen acc = do
        forM_ notSeen $ \r -> createFile (r </> "ready")
        wait notSeen acc

    -- On macOS a marker can be reported again after it is read.
    wait [] acc = (acc ++) <$> readAvailable w
    wait notSeen acc = do
        evs <- readUntil isReady w
        wait (notSeen \\ map rootPath (filter isReady evs)) (acc ++ evs)

    isReady ev = Event.isCreated ev && takeFileName (absPath ev) == "ready"

-- | Whether the event is for a file created by 'waitStarted' in one of the
-- roots.
isStartMarker :: [FilePath] -> Event -> Bool
isStartMarker roots ev =
    takeDirectory path `elem` roots
        && (name == "ready" || "start" `isPrefixOf` name)

    where

    path = absPath ev
    name = takeFileName path

-- | The events read by 'waitStarted' that a test checks. On macOS a watch
-- reports changes made shortly before it started, and on Windows the start is
-- not tested, the test checks them on Linux only.
startChecked :: Watch -> [Event]
#if defined(CABAL_OS_LINUX)
startChecked = startEvents
#else
startChecked = const []
#endif

-- | On Linux the watch reports all the events, as the Linux test suite does.
allEvents :: Config -> Config
#if defined(CABAL_OS_LINUX)
allEvents = Linux.setAllEvents True
#else
allEvents = id
#endif

-- | Watch the roots, read the stream in another thread, and run the action
-- after the watch has started on all the roots.
withWatch :: (Config -> Config) -> [FilePath] -> (Watch -> IO a) -> IO a
withWatch cfg roots action = do
    ch <- newChan
    paths <- mapM Path.fromString roots
    let run =
            Stream.fold (Fold.drainMapM (writeChan ch . Right))
                $ Event.watchWith (cfg . allEvents) (NonEmpty.fromList paths)
        end r = writeChan ch (Left (either id (const ended) r))
        w = Watch {watchEvents = ch, startEvents = []}
    bracket
        (forkIO (try run >>= end))
        killThread
        $ \_ -> do
            evs <- waitStarted roots w
            action w {startEvents = evs}

    where

    ended = toException (userError "the stream ended")

-- | Watch the directory @root@ in a temporary directory.
withRoot :: (Config -> Config) -> (FilePath -> Watch -> IO a) -> IO a
withRoot cfg action =
    withSystemTempDirectory "fsevent_dir" $ \fp -> do
        let root = fp </> "root"
        createDirectory root
        withWatch cfg [root] (action root)

recursive :: Config -> Config
recursive = Event.setRecursiveMode True

-- | Create the file @name@ in @root@ and read the events up to its create
-- event. The events of the file system operations done before are read
-- before it.
syncOn :: FilePath -> String -> Watch -> IO [Event]
syncOn root name w = do
    let file = root </> name
    createFile file
    readUntil (isCreatedAt file) w

-------------------------------------------------------------------------------
-- Watch roots
-------------------------------------------------------------------------------

-- On Linux the last of the 300 roots has an inotify watch descriptor greater
-- than 255.
manyRoots :: Spec
manyRoots =
    it "reports the events of the last of 300 roots"
        $ withSystemTempDirectory "fsevent_dir" $ \fp -> do
            let dirs = [fp </> ("dir" ++ show i) | i <- [1 .. 300 :: Int]]
                file = fp </> "dir300" </> "file"
            mapM_ createDirectory dirs
            withWatch id dirs $ \w -> do
                createFile file
                evs <- readUntil (isCreatedAt file) w
                evs `shouldCreate` [file]

eventPaths :: Spec
eventPaths =
    it "getRelPath is the path relative to getRoot"
        $ withRoot recursive $ \root w -> do
            let sub = root </> "sub"
                file = sub </> "file"
            createDirectory sub
            _ <- syncOn root "sync" w
            createFile file
            evs <- readUntil (isCreatedAt file) w
            let ev = last evs
            rootPath ev `shouldBe` root
            relPath ev `shouldBe` Just ("sub" </> "file")
            absPath ev `shouldBe` root </> "sub" </> "file"

-------------------------------------------------------------------------------
-- Recursive mode
-------------------------------------------------------------------------------

recSubdirMovedDeleted :: Spec
recSubdirMovedDeleted =
    it "reports a subdirectory renamed and a subdirectory deleted"
        $ withRoot recursive $ \root w -> do
            let dir1 = root </> "dir1"
                dir2 = root </> "dir2"
                dir3 = root </> "dir3"
            createDirectory dir1
            createDirectory dir2
            _ <- syncOn root "sync" w
            renameDirectory dir1 dir3
            removeDirectory dir2
            evs <- syncOn root "sentinel" w
            sort (pathsOf Event.isMoved evs) `shouldBe` [dir1, dir3]
            pathsOf Event.isDeleted evs `shouldBe` [dir2]

-- A subdirectory created while its parent is scanned on Linux is added by
-- the scan and on its create event.
recManyNewDirs :: Spec
recManyNewDirs =
    it "keeps watching while many directories are created"
        $ withRoot recursive $ \root w -> do
            let mkTree i = do
                    let dir = root </> ("dir" ++ show i)
                    createDirectory dir
                    mapM_ (\j -> createDirectory (dir </> show j))
                        [1 .. 50 :: Int]
                sentinel = root </> "sentinel"
                ops = mapM_ mkTree [1 .. 20 :: Int] >> createFile sentinel
            done <- newChan
            bracket (forkIO (ops `finally` writeChan done ())) killThread
                $ \_ -> do
                    _ <- readUntil (isCreatedAt sentinel) w
                    readChan done
            let file = root </> "dir20" </> "50" </> "file"
            createFile file
            evs <- readUntil (isCreatedAt file) w
            evs `shouldCreate` [file]

-- On Linux the directory is replaced before its create event is read. On
-- macOS the event of the write can repeat 'Event.isCreated' for the path.
recDirReplacedByFile :: Spec
recDirReplacedByFile =
    it "reports a file that replaced a new directory"
        $ withRoot recursive $ \root w -> do
            let dir = root </> "dir"
            createDirectory dir
            removeDirectory dir
            createFile dir
            _ <- syncOn root "sync" w
            appendFile dir "x"
            evs <- syncOn root "sentinel" w
            pathsOf Event.isModified evs `shouldContain` [dir]

-------------------------------------------------------------------------------
-- Recursive mode changes to the tree
-------------------------------------------------------------------------------

-- | Changes to a recursive watch of @root@. The paths are absolute, the
-- functions take @root@, the directory @out@ is outside the tree.
data TreeChange = TreeChange
    { changeTitle :: String
    , posixOnly :: Bool
    -- | On Linux 'stale' is checked against every event, see
    -- 'checkedEvents'.
    , linuxEveryEvent :: Bool
    -- | Before the watch reports the events of 'setup'.
    , setup :: FilePath -> IO ()
    -- | Run without reading the events in between.
    , changes :: FilePath -> IO ()
    -- | Each of these paths reports 'Event.isCreated' for 'changes'.
    , created :: FilePath -> [FilePath]
    -- | After the events of 'changes' are read.
    , laterOps :: FilePath -> IO ()
    -- | Files created after 'laterOps', see 'shouldCreate'.
    , laterFiles :: FilePath -> [FilePath]
    -- | No change reported for 'laterOps' and 'laterFiles' is under these
    -- paths.
    , stale :: FilePath -> [FilePath]
    }

treeChange :: TreeChange
treeChange =
    TreeChange
        { changeTitle = ""
        , posixOnly = False
        , linuxEveryEvent = False
        , setup = const (return ())
        , changes = const (return ())
        , created = const []
        , laterOps = const (return ())
        , laterFiles = const []
        , stale = const []
        }

outside :: FilePath -> FilePath
outside root = takeDirectory root </> "out"

-- | @root/dir1@ containing @sub@.
withDir1 :: TreeChange
withDir1 =
    treeChange
        { setup = \r -> do
            createDirectory (r </> "dir1")
            createDirectory (r </> "dir1" </> "sub")
        }

-- | @root/dir@ containing @file@.
withDirFile :: TreeChange
withDirFile =
    treeChange
        { setup = \r -> do
            createDirectory (r </> "dir")
            createFile (r </> "dir" </> "file")
        }

-- | A directory @src@ outside the tree containing @file@ and @sub@.
withSrc :: FilePath -> IO ()
withSrc r = do
    let src = takeDirectory r </> "src"
    createDirectory src
    createFile (src </> "file")
    createDirectory (src </> "sub")

srcOf :: FilePath -> FilePath
srcOf r = takeDirectory r </> "src"

-- | Rename @root/dir1@ to @root/dir@ after the path @root/dir@ was used by a
-- new directory. The watch of @dir1@ is kept under its new path.
reusedPath :: String -> (FilePath -> IO ()) -> TreeChange
reusedPath title action =
    withDir1
        { changeTitle = title
        , changes = action
        , laterFiles = \r ->
            [r </> "dir" </> "later", r </> "dir" </> "sub" </> "later"]
        , stale = \r -> [r </> "dir1", outside r]
        }

-- | Directories with symbolic links to them, @a/up@ to its parent,
-- @a/b/top@ to the root, @x/z@ and @z/x@ to each other's directories, and
-- files with symbolic links, @self@ to itself, @l1@ and @l2@ to each other.
cycleDirs :: [FilePath]
cycleDirs = ["a", "a" </> "b", "x", "z"]

cycleLinks :: [FilePath]
cycleLinks =
    [ "a" </> "up", "a" </> "b" </> "top", "x" </> "z", "z" </> "x"
    , "self", "l1", "l2"
    ]

symlinkCycleLinks :: FilePath -> FilePath -> IO ()
symlinkCycleLinks base top = do
    createDirectoryLink ".." (base </> "a" </> "up")
    createDirectoryLink top (base </> "a" </> "b" </> "top")
    createDirectoryLink (".." </> "z") (base </> "x" </> "z")
    createDirectoryLink (".." </> "x") (base </> "z" </> "x")
    createFileLink "self" (base </> "self")
    createFileLink "l2" (base </> "l1")
    createFileLink "l1" (base </> "l2")

symlinkCycles :: FilePath -> FilePath -> IO ()
symlinkCycles base top = do
    mapM_ (createDirectory . (base </>)) cycleDirs
    symlinkCycleLinks base top

-- | A file in each directory of 'cycleDirs' under @root/dir@.
cycleFiles :: FilePath -> [FilePath]
cycleFiles r = map (\d -> r </> "dir" </> d </> "file") cycleDirs

-- | The paths through the links of 'cycleLinks' under @root/dir@.
cycleStale :: FilePath -> [FilePath]
cycleStale r = map ((r </> "dir") </>) cycleLinks

treeChanges :: [TreeChange]
treeChanges =
    [ withDir1
        { changeTitle = "directory renamed"
        , changes = \r -> renameDirectory (r </> "dir1") (r </> "dir2")
        , laterFiles = \r ->
            [r </> "dir2" </> "file", r </> "dir2" </> "sub" </> "file"]
        , stale = \r -> [r </> "dir1"]
        , linuxEveryEvent = True
        }
    , withDir1
        { changeTitle = "directory renamed twice"
        , changes = \r -> do
            renameDirectory (r </> "dir1") (r </> "dir2")
            renameDirectory (r </> "dir2") (r </> "dir3")
        , laterFiles = \r -> [r </> "dir3" </> "sub" </> "later"]
        , stale = \r -> [r </> "dir1", r </> "dir2"]
        }
    , withDir1
        { changeTitle = "entries created after a rename"
        , changes = \r -> do
            let dir2 = r </> "dir2"
            renameDirectory (r </> "dir1") dir2
            createDirectory (dir2 </> "x")
            createFile (dir2 </> "x" </> "f")
            createFile (dir2 </> "g")
        , created = \r ->
            let dir2 = r </> "dir2"
             in [dir2 </> "x", dir2 </> "x" </> "f", dir2 </> "g"]
        , laterFiles = \r -> [r </> "dir2" </> "x" </> "later"]
        , stale = \r -> [r </> "dir1"]
        }
    , withDir1
        { changeTitle = "entries created before a rename"
        , changes = \r -> do
            createDirectory (r </> "dir1" </> "x")
            createFile (r </> "dir1" </> "x" </> "f")
            renameDirectory (r </> "dir1") (r </> "dir2")
        , laterFiles = \r -> [r </> "dir2" </> "x" </> "later"]
        , stale = \r -> [r </> "dir1"]
        }
    , treeChange
        { changeTitle = "directory moved in"
        , setup = withSrc
        , changes = \r -> renameDirectory (srcOf r) (r </> "dir")
        , laterFiles = \r -> [r </> "dir" </> "sub" </> "later"]
        , stale = \r -> [srcOf r]
        }
    , treeChange
        { changeTitle = "directory moved out"
        , setup = \r -> do
            createDirectory (r </> "dir")
            createDirectory (r </> "dir" </> "sub")
        , changes = \r -> renameDirectory (r </> "dir") (outside r)
        , laterOps = \r -> do
            createFile (outside r </> "file")
            createFile (outside r </> "sub" </> "file")
        , stale = \r -> [r </> "dir", outside r]
        }
    , treeChange
        { changeTitle = "new directory renamed"
        , changes = \r -> do
            createDirectory (r </> "dir")
            createDirectory (r </> "dir" </> "sub")
            createFile (r </> "dir" </> "sub" </> "file")
            renameDirectory (r </> "dir") (r </> "dir1")
        , laterFiles = \r -> [r </> "dir1" </> "sub" </> "later"]
        , stale = \r -> [r </> "dir"]
        }
    , treeChange
        { changeTitle = "new directory renamed twice"
        , changes = \r -> do
            createDirectory (r </> "dir")
            createFile (r </> "dir" </> "file")
            renameDirectory (r </> "dir") (r </> "dir1")
            renameDirectory (r </> "dir1") (r </> "dir2")
        , laterFiles = \r -> [r </> "dir2" </> "later"]
        , stale = \r -> [r </> "dir", r </> "dir1"]
        }
    , treeChange
        { changeTitle = "new directory renamed, its old path reused"
        , changes = \r -> do
            createDirectory (r </> "dir")
            createFile (r </> "dir" </> "file")
            renameDirectory (r </> "dir") (r </> "dir1")
            createDirectory (r </> "dir")
        , laterFiles = \r ->
            [r </> "dir1" </> "later", r </> "dir" </> "later"]
        }
    , treeChange
        { changeTitle = "new directory renamed twice, its old path reused"
        , changes = \r -> do
            createDirectory (r </> "dir")
            createFile (r </> "dir" </> "file")
            renameDirectory (r </> "dir") (r </> "dir1")
            createDirectory (r </> "dir")
            renameDirectory (r </> "dir1") (r </> "dir2")
        , laterFiles = \r ->
            [r </> "dir2" </> "later", r </> "dir" </> "later"]
        , stale = \r -> [r </> "dir1"]
        }
    , treeChange
        { changeTitle = "directory moved in and renamed"
        , setup = withSrc
        , changes = \r -> do
            renameDirectory (srcOf r) (r </> "dir")
            renameDirectory (r </> "dir") (r </> "dir1")
        , laterFiles = \r -> [r </> "dir1" </> "sub" </> "later"]
        , stale = \r -> [r </> "dir", srcOf r]
        }
    , withDirFile
        { changeTitle = "directory renamed away and back"
        , changes = \r -> do
            renameDirectory (r </> "dir") (r </> "dir1")
            renameDirectory (r </> "dir1") (r </> "dir")
        , laterFiles = \r -> [r </> "dir" </> "later"]
        , stale = \r -> [r </> "dir1"]
        }
    , withDirFile
        { changeTitle = "directory moved in after a rename away and back"
        , setup = \r -> setup withDirFile r >> withSrc r
        , changes = \r -> do
            renameDirectory (r </> "dir") (r </> "dir1")
            renameDirectory (r </> "dir1") (r </> "dir")
            renameDirectory (srcOf r) (r </> "dir1")
        , laterFiles = \r ->
            [r </> "dir" </> "later", r </> "dir1" </> "sub" </> "later"]
        , stale = \r -> [srcOf r]
        }
    , treeChange
        { changeTitle = "new directory deleted and created again"
        , changes = \r -> do
            createDirectory (r </> "dir")
            removeDirectory (r </> "dir")
            createDirectory (r </> "dir")
            createDirectory (r </> "dir" </> "sub")
        , laterFiles = \r -> [r </> "dir" </> "sub" </> "file"]
        }
    , treeChange
        { changeTitle = "new directory renamed and created again"
        , changes = \r -> do
            createDirectory (r </> "dir")
            createDirectory (r </> "dir" </> "sub")
            renameDirectory (r </> "dir") (r </> "dir1")
            createDirectory (r </> "dir")
            createDirectory (r </> "dir" </> "sub")
        , laterFiles = \r ->
            [ r </> "dir" </> "sub" </> "file"
            , r </> "dir1" </> "sub" </> "file"
            ]
        }
    , treeChange
        { changeTitle = "new directory moved out and created again"
        , changes = \r -> do
            createDirectory (r </> "dir")
            renameDirectory (r </> "dir") (outside r)
            createDirectory (r </> "dir")
        , laterFiles = \r -> [r </> "dir" </> "file"]
        , laterOps = \r -> createFile (outside r </> "file")
        , stale = \r -> [outside r]
        }
    , treeChange
        { changeTitle = "directory moved to the path of a renamed new dir"
        , setup = \r -> do
            createDirectory (r </> "dir2")
            createDirectory (r </> "dir2" </> "sub")
        , changes = \r -> do
            createDirectory (r </> "dir")
            renameDirectory (r </> "dir") (r </> "dir1")
            renameDirectory (r </> "dir2") (r </> "dir")
        , laterFiles = \r -> [r </> "dir" </> "sub" </> "file"]
        , stale = \r -> [r </> "dir2"]
        }
    , withDir1
        { changeTitle = "directory moved out and its path reused"
        , changes = \r -> do
            renameDirectory (r </> "dir1") (outside r)
            createDirectory (r </> "dir1")
        , laterOps = \r -> createFile (outside r </> "sub" </> "file")
        , laterFiles = \r -> [r </> "dir1" </> "later"]
        , stale = \r -> [outside r]
        }
    , treeChange
        { changeTitle = "directory moved in at the path of a new dir moved out"
        , setup = withSrc
        , changes = \r -> do
            createDirectory (r </> "dir")
            renameDirectory (r </> "dir") (outside r)
            renameDirectory (srcOf r) (r </> "dir")
        , laterFiles = \r -> [r </> "dir" </> "sub" </> "later"]
        , stale = \r -> [outside r, srcOf r]
        }
    , reusedPath "watched directory moved to the path of a renamed new dir"
        $ \r -> do
            createDirectory (r </> "dir")
            renameDirectory (r </> "dir") (r </> "dir-renamed")
            renameDirectory (r </> "dir1") (r </> "dir")
    , reusedPath "watched directory moved to the path of a deleted new dir"
        $ \r -> do
            createDirectory (r </> "dir")
            removeDirectory (r </> "dir")
            renameDirectory (r </> "dir1") (r </> "dir")
    , reusedPath "watched directory moved to the path of a new dir moved out"
        $ \r -> do
            createDirectory (r </> "dir")
            renameDirectory (r </> "dir") (outside r)
            renameDirectory (r </> "dir1") (r </> "dir")
    -- rename(2) replaces an empty directory, MoveFileEx on Windows does not.
    , (reusedPath "watched directory moved over an empty new directory"
        $ \r -> do
            createDirectory (r </> "dir")
            renameDirectory (r </> "dir1") (r </> "dir")
      ) {posixOnly = True}
    , reusedPath "watched directory renamed twice to a moved out new dir path"
        $ \r -> do
            createDirectory (r </> "dir")
            renameDirectory (r </> "dir1") (r </> "dir1-renamed")
            renameDirectory (r </> "dir") (outside r)
            renameDirectory (r </> "dir1-renamed") (r </> "dir")
    , treeChange
        { changeTitle = "new directory replaced by a symlink to outside"
        , setup = \r -> do
            createDirectory (outside r)
            createDirectory (outside r </> "sub")
        , changes = \r -> do
            createDirectory (r </> "dir")
            removeDirectory (r </> "dir")
            createDirectoryLink (outside r) (r </> "dir")
        , laterOps = \r -> do
            createFile (outside r </> "file")
            createFile (outside r </> "sub" </> "file")
        , stale = \r ->
            [r </> "dir" </> "file", r </> "dir" </> "sub", outside r]
        , linuxEveryEvent = True
        }
    , withDir1
        { changeTitle = "parent of a new dir replaced by a symlink to outside"
        , setup = \r -> do
            setup withDir1 r
            createDirectory (outside r)
            createDirectory (outside r </> "new")
        , changes = \r -> do
            createDirectory (r </> "dir1" </> "new")
            renameDirectory (r </> "dir1") (r </> "dir2")
            createDirectoryLink (outside r) (r </> "dir1")
        , laterOps = \r -> createFile (outside r </> "new" </> "stray")
        , laterFiles = \r -> [r </> "dir2" </> "new" </> "later"]
        , stale = \r -> [r </> "dir1", outside r]
        }
    , withDir1
        { changeTitle = "parent of a new dir replaced by a symlink loop"
        , posixOnly = True
        , changes = \r -> do
            createDirectory (r </> "dir1" </> "new")
            renameDirectory (r </> "dir1") (r </> "dir2")
            createFileLink (r </> "dir1") (r </> "dir1")
        , laterFiles = \r ->
            [ r </> "dir2" </> "new" </> "later"
            , r </> "dir2" </> "sub" </> "later"
            ]
        , stale = \r -> [r </> "dir1"]
        }
    , treeChange
        { changeTitle = "symlink cycles in a watched tree"
        , posixOnly = True
        , setup = \r -> do
            createDirectory (r </> "dir")
            mapM_ (createDirectory . ((r </> "dir") </>)) cycleDirs
        , changes = \r -> symlinkCycleLinks (r </> "dir") r
        , created = \r -> map ((r </> "dir") </>) cycleLinks
        , laterFiles = cycleFiles
        , stale = cycleStale
        }
    , treeChange
        { changeTitle = "new directory with symlink cycles"
        , posixOnly = True
        , changes = \r -> do
            createDirectory (r </> "dir")
            symlinkCycles (r </> "dir") r
        , laterFiles = cycleFiles
        , stale = cycleStale
        }
    , treeChange
        { changeTitle = "directory with symlink cycles moved in"
        , posixOnly = True
        , setup = \r -> do
            createDirectory (outside r)
            symlinkCycles (outside r) r
        , changes = \r -> renameDirectory (outside r) (r </> "dir")
        , laterFiles = cycleFiles
        , stale = cycleStale
        }
    ]

runTreeChange :: TreeChange -> Spec
runTreeChange TreeChange{..} =
    it changeTitle
        $ (if posixOnly then notOnWindows "not portable to Windows" else id)
        $ withRoot recursive $ \root w -> do
            setup root
            _ <- syncOn root "sync1" w
            changes root
            evs <- syncOn root "sync2" w
            let createdPaths = pathsOf Event.isCreated evs
            filter (`notElem` createdPaths) (created root) `shouldBe` []
            laterOps root
            let files = laterFiles root
            mapM_ createFile files
            evs1 <- syncOn root "sync3" w
            evs1 `shouldCreate` (files ++ [root </> "sync3"])
            let isStale p = any (`isUnder` p) (stale root)
            filter isStale (map absPath (checkedEvents linuxEveryEvent evs1))
                `shouldBe` []

-- The tree is watched from the start, no event is reported through a link.
-- On Linux the events read while the watch starts are checked too, the scan
-- at the start of the watch does not report the entries.
recStartSymlinkCycles :: Spec
recStartSymlinkCycles =
    it "watches a tree with symlink cycles"
        $ notOnWindows "not portable to Windows"
        $ withSystemTempDirectory "fsevent_dir" $ \fp -> do
            let root = fp </> "root"
                files = cycleFiles root
            createDirectory root
            createDirectory (root </> "dir")
            symlinkCycles (root </> "dir") root
            withWatch recursive [root] $ \w -> do
                mapM_ createFile files
                evs <- readUntil (isCreatedAt (last files)) w
                let evsAll =
                        filter (not . isStartMarker [root])
                            (startChecked w ++ evs)
                    viaLink p = any (`isUnder` p) (cycleStale root)
                evsAll `shouldCreate` files
                filter viaLink (map absPath (checkedEvents True evsAll))
                    `shouldBe` []

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

watchTests :: IO ()
watchTests =
    hspec
        $ describe moduleName
        $ do
            describe "watch roots" $ do
                manyRoots
                eventPaths
            describe "recursive mode" $ do
                recSubdirMovedDeleted
                recManyNewDirs
                recDirReplacedByFile
                recStartSymlinkCycles
            describe "recursive mode changes to the tree"
                $ mapM_ runTreeChange treeChanges

main :: IO ()
main = do
    let run = runTests moduleName "non-recursive" (failNested Event.watch)
    run DirType (nestedOpsTest : commonTests)
    run SymLinkOrigPath (nestedOpsTest : commonTests)

    let runRec = runTests moduleName "recursive" Event.watchRecursive
    runRec DirType (commonTests ++ commonRecTests)
    runRec SymLinkOrigPath (commonTests ++ commonRecTests)

    watchTests
