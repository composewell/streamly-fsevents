-- |
-- Module      : Streamly.Test.FileSystem.Event
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Tests of the portable API, with the same expected events on all the
-- platforms. Where a platform reports more, the helper used by the test
-- states the difference, for example, 'shouldCreate' and 'shouldUnwatch'.

module Streamly.Test.FileSystem.Event (main) where

-- Linux and FreeBSD use the same inotify backend, the tests expect the same
-- events on both.
#if defined(CABAL_OS_LINUX) || defined(CABAL_OS_FREEBSD)
#define INOTIFY_BACKEND
#endif

import Control.Concurrent
    ( MVar, ThreadId, forkIO, killThread, newChan, newEmptyMVar, putMVar
    , readChan, takeMVar, threadDelay, tryPutMVar, writeChan
    )
import Control.Exception
    ( AsyncException(ThreadKilled), IOException, SomeException, bracket
    , finally, fromException, mask, mask_, throwIO, try
    , uninterruptibleMask_
    )
import Control.Monad (forM_, unless, when)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (elemIndex, isPrefixOf, nub, sort, sortOn)
import Streamly.Internal.FS.Event (Config, Event)
import System.Directory
    ( createDirectory
    , createDirectoryIfMissing
    , createDirectoryLink
    , createFileLink
    , getCurrentDirectory
    , removeDirectory
    , removePathForcibly
    , renameDirectory
    , setCurrentDirectory
    )
import System.FilePath ((</>), normalise, pathSeparator, takeDirectory)
#if defined(CABAL_OS_DARWIN)
import System.FilePath (takeFileName)
#endif
import System.IO.Temp (withSystemTempDirectory)
import System.Mem (performGC)
import System.Timeout (timeout)
#if defined(CABAL_OS_LINUX)
import System.Directory (getSymbolicLinkTarget, listDirectory)
#elif defined(CABAL_OS_DARWIN)
import System.Directory (listDirectory)
#endif
#if !defined(CABAL_OS_WINDOWS)
import Data.Bits ((.|.))
import Data.List.NonEmpty (NonEmpty((:|)))
import System.IO (IOMode(..), hClose, hPutStr, openFile, withFile)
import System.Posix.Files
    (createNamedPipe, ownerReadMode, ownerWriteMode)
import System.Process (callProcess)
#endif
import Test.Hspec

import qualified Data.List.NonEmpty as NonEmpty
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.FS.Event as Event
import qualified Streamly.Internal.FileSystem.Path as Path
#if defined(CABAL_OS_LINUX)
import qualified Streamly.Internal.FS.Event.Linux as Inotify
#elif defined(CABAL_OS_FREEBSD)
import Foreign.C.Types (CInt(..))
import qualified Streamly.Internal.FS.Event.FreeBSD as Inotify
#elif defined(CABAL_OS_WINDOWS)
import GHC.IO.Exception (IOErrorType(InappropriateType))
import System.IO.Error (ioeGetErrorType)
import qualified System.Win32.File as Win32
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

-- | The roots of the create events of the file. On macOS and Windows a root
-- is listed once, a later event for a path can repeat 'Event.isCreated' on
-- macOS.
createdRoots :: FilePath -> [Event] -> [FilePath]
#if defined(INOTIFY_BACKEND)
createdRoots file = map rootPath . filter (isCreatedAt file)
#else
createdRoots file = nub . map rootPath . filter (isCreatedAt file)
#endif

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
#if defined(INOTIFY_BACKEND)
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
-- checked, the markers of 'skipEarlier' and 'syncOn' can be reported again.
shouldCreate :: [Event] -> [FilePath] -> Expectation
shouldCreate evs paths =
#if defined(INOTIFY_BACKEND)
    created `shouldBe` paths
#elif defined(CABAL_OS_DARWIN)
    nub (sort (filter (`elem` paths) created)) `shouldBe` nub (sort paths)
#else
    sort created `shouldBe` sort paths
#endif

    where

    created = pathsOf Event.isCreated evs

-- | The 'Event.isModified' events must include the given paths. On Linux
-- they must be for the given paths only, one for each path, in order.
shouldModify :: [Event] -> [FilePath] -> Expectation
shouldModify evs paths =
#if defined(INOTIFY_BACKEND)
    modified `shouldBe` paths
#else
    filter (`notElem` modified) paths `shouldBe` []
#endif

    where

    modified = pathsOf Event.isModified evs

onInotify :: Bool
#if defined(INOTIFY_BACKEND)
onInotify = True
#else
onInotify = False
#endif

-- | Whether the event is an @isRootUnwatched@ event. Linux and FreeBSD
-- also report it for a directory of a recursive watch, macOS and Windows
-- only for a root.
isUnwatched :: Event -> Bool
isUnwatched = Event.isRootUnwatched

-- | The @isRootUnwatched@ events must be for the given roots and paths, in
-- order. On macOS and Windows only the expected events whose path is the
-- root are checked.
shouldUnwatch :: [Event] -> [(FilePath, FilePath)] -> Expectation
shouldUnwatch evs expected =
    [(rootPath ev, absPath ev) | ev <- evs, isUnwatched ev]
        `shouldBe` checked

    where

    checked =
        if onInotify then expected else filter (uncurry (==)) expected

createFile :: FilePath -> IO ()
createFile file = writeFile file ""

-- | The directory @out@ next to the root, outside the watched tree.
outside :: FilePath -> FilePath
outside root = takeDirectory root </> "out"

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

-- | Run the test except on macOS.
notOnDarwin :: String -> IO () -> IO ()
#if defined(CABAL_OS_DARWIN)
notOnDarwin reason _ = pendingWith reason
#else
notOnDarwin _ test = test
#endif

-------------------------------------------------------------------------------
-- Running a watch
-------------------------------------------------------------------------------

-- The tests open the watch using 'Event.openWatch' and read it using
-- 'Event.readEvent' in the test thread. On Linux the watch adds, removes and
-- scans the directories of an event when the event is read. The changes that
-- a test makes without reading the watch are all done before the watch
-- handles the event of the first of them.

-- | Read the events of the watch up to and including the first event that
-- satisfies the predicate. Throw an exception if it does not occur within 10
-- seconds or the watch is closed.
readUntil :: (Event -> Bool) -> Event.Watch -> IO [Event]
readUntil p w = do
    r <- timeout 10000000 (go [])
    maybe (error "readUntil: timed out") return r

    where

    go acc = do
        r <- Event.readEvent w
        case r of
            Nothing -> error "readUntil: the watch is closed"
            Just ev
                | p ev -> return (reverse (ev : acc))
                | otherwise -> go (ev : acc)

-- | Read the events up to the @isRootUnwatched@ event of the path for the
-- root. On macOS and Windows nothing is read if the path is not the root.
readUnwatched :: Event.Watch -> FilePath -> FilePath -> IO [Event]
readUnwatched w root path =
    if not onInotify && root /= path
    then return []
    else
        readUntil
            (\ev ->
                isUnwatched ev && rootPath ev == root && absPath ev == path)
            w

-- | On macOS a watch reports changes made shortly before it was opened.
-- Create a file @ready@ in each root, read the events up to the last of
-- them, and the events that follow within 100 ms of each other. On Linux and
-- Windows nothing is read, the test gets every event of the watch.
skipEarlier :: [FilePath] -> Event.Watch -> IO ()
#if defined(CABAL_OS_DARWIN)
skipEarlier roots w = do
    forM_ (nub roots) $ \r -> createFile (r </> "ready")
    wait (nub roots)

    where

    isReady ev = Event.isCreated ev && takeFileName (absPath ev) == "ready"

    wait [] = drain
    wait notSeen = do
        evs <- readUntil isReady w
        let seen = map rootPath (filter isReady evs)
        wait (filter (`notElem` seen) notSeen)

    drain = do
        r <- timeout 100000 (Event.readEvent w)
        case r of
            Just (Just _) -> drain
            _ -> return ()
#else
skipEarlier _ _ = return ()
#endif

-- | On Linux the watch reports all the events, as the Linux test suite does.
allEvents :: Config -> Config
#if defined(INOTIFY_BACKEND)
allEvents = Inotify.setAllEvents True
#else
allEvents = id
#endif

-- | Open a watch on the roots, run the action and close the watch.
withWatchCfg ::
    (Config -> Config) -> [FilePath] -> (Event.Watch -> IO a) -> IO a
withWatchCfg cfg roots action = do
    paths <- mapM Path.fromString roots
    bracket
        (Event.openWatch cfg (NonEmpty.fromList paths))
        Event.closeWatch
        $ \w -> skipEarlier roots w >> action w

-- | Like 'withWatchCfg', with 'allEvents'.
withWatch ::
    (Config -> Config) -> [FilePath] -> (Event.Watch -> IO a) -> IO a
withWatch cfg = withWatchCfg (cfg . allEvents)

-- | Watch the directory @root@ in a temporary directory.
withRootCfg ::
    (Config -> Config) -> (FilePath -> Event.Watch -> IO a) -> IO a
withRootCfg cfg action =
    withSystemTempDirectory "fsevent_dir" $ \fp -> do
        let root = fp </> "root"
        createDirectory root
        withWatchCfg cfg [root] (action root)

-- | Like 'withRootCfg', with 'allEvents'.
withRoot ::
    (Config -> Config) -> (FilePath -> Event.Watch -> IO a) -> IO a
withRoot cfg = withRootCfg (cfg . allEvents)

recursive :: Config -> Config
recursive = Event.setRecursiveMode True

-- | Watch the directories @root1@ and @root2@ in a temporary directory.
withTwoRoots :: (FilePath -> FilePath -> Event.Watch -> IO a) -> IO a
withTwoRoots action =
    withSystemTempDirectory "fsevent_dir" $ \fp -> do
        let root1 = fp </> "root1"
            root2 = fp </> "root2"
        createDirectory root1
        createDirectory root2
        withWatch id [root1, root2] (action root1 root2)

-- | A recursive watch of the directory @root@ and of its subdirectory
-- @root/sub@, both as roots.
withNestedRoots :: (FilePath -> FilePath -> Event.Watch -> IO a) -> IO a
withNestedRoots action =
    withSystemTempDirectory "fsevent_dir" $ \fp -> do
        let root = fp </> "root"
            sub = root </> "sub"
        createDirectory root
        createDirectory sub
        withWatch recursive [root, sub] (action root sub)

-- | Create the file @name@ in @root@ and read the events up to its create
-- event. The events of the file system operations done before are read
-- before it.
syncOn :: FilePath -> String -> Event.Watch -> IO [Event]
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

-- | Watch the relative path @root@ recursively. The watch is opened with a
-- temporary directory as the working directory, the action gets the
-- absolute path of that directory as the system reports it. The working
-- directory is restored when the action ends.
withRelativeRoot :: (FilePath -> Event.Watch -> IO a) -> IO a
withRelativeRoot action =
    withSystemTempDirectory "fsevent_dir" $ \fp -> do
        createDirectory (fp </> "root")
        r <- Path.fromString "root"
        orig <- getCurrentDirectory
        flip finally (setCurrentDirectory orig) $ do
            setCurrentDirectory fp
            cwd <- getCurrentDirectory
            bracket
                (Event.openWatch (recursive . allEvents) (r NonEmpty.:| []))
                Event.closeWatch
                (action cwd)

isCreatedRel :: FilePath -> Event -> Bool
isCreatedRel rel ev = Event.isCreated ev && relPath ev == Just rel

-- On Linux the watch of a new directory is added when its create event is
-- read, the file is created after that.
relativeRoot :: Spec
relativeRoot = do
    it "a relative root is watched after the working directory changes"
        $ withRelativeRoot $ \cwd w -> do
            let dir = cwd </> "root" </> "dir"
            setCurrentDirectory (takeDirectory cwd)
            createDirectory dir
            _ <- readUntil (isCreatedRel "dir") w
            createFile (dir </> "file")
            evs <- readUntil (isCreatedRel ("dir" </> "file")) w
            rootPath (last evs) `shouldBe` "root"
    it "getAbsPath is absolute for a relative root"
        $ withRelativeRoot $ \cwd w -> do
            createFile (cwd </> "root" </> "file")
            evs <- readUntil (isCreatedRel "file") w
            rootPath (last evs) `shouldBe` "root"
            absPath (last evs) `shouldBe` cwd </> "root" </> "file"

-- | @normalizedRoot supplied expected@ watches the directory @root@ in a
-- temporary directory @fp@, via the path @supplied fp@, and checks that
-- 'Event.getRoot' reports @expected fp@. The working directory is @fp@.
normalizedRoot ::
    String -> (FilePath -> FilePath) -> (FilePath -> FilePath) -> Spec
normalizedRoot title supplied expected =
    it title
        $ notOnWindows "getRoot reports the path as supplied"
        $ withSystemTempDirectory "fsevent_dir" $ \tmp -> do
            orig <- getCurrentDirectory
            flip finally (setCurrentDirectory orig) $ do
                setCurrentDirectory tmp
                fp <- getCurrentDirectory
                createDirectory (fp </> "root")
                path <- Path.fromString (supplied fp)
                bracket
                    (Event.openWatch allEvents (path NonEmpty.:| []))
                    Event.closeWatch
                    $ \w -> do
                        skipEarlier [expected fp] w
                        createFile (fp </> "root" </> "file")
                        evs <- readUntil (isCreatedRel "file") w
                        rootPath (last evs) `shouldBe` expected fp

normalizedRoots :: Spec
normalizedRoots = do
    normalizedRoot
        "getRoot reports an absolute root without redundant components"
        (\fp -> fp ++ "//./root/.")
        (</> "root")
    normalizedRoot
        "getRoot reports a relative root without a leading ./"
        (const "./root")
        (const "root")

-- The stream is read by another thread. The watch is open when it reports
-- a file created in each root, the files are created until it does.
--
-- On Linux and Windows a deleted root is not watched again, even if it is
-- created again, the stream ends when none of the roots is watched. On
-- macOS the watch is on the path of a root, the stream does not end.
--
-- @names@ are the roots given to the watch, a name can be listed twice, or
-- via two paths that differ only in redundant separators and @.@
-- components. The first of such paths must be normalized, Windows reports
-- the root as the first path supplied.
deletedRootsEnd :: String -> [FilePath] -> Spec
deletedRootsEnd title names =
    it title
        $ notOnDarwin "the watch of a deleted root continues"
        $ withSystemTempDirectory "fsevent_dir" $ \fp -> do
            let roots = nub (map (normalise . (fp </>)) names)
            mapM_ createDirectory roots
            paths <- mapM (Path.fromString . (fp </>)) names
            ref <- newIORef []
            result <- newEmptyMVar
            let collect ev = atomicModifyIORef' ref (\evs -> (ev : evs, ()))
                stream = Event.watch (NonEmpty.fromList paths)
                reader =
                    try (Stream.fold (Fold.drainMapM collect) stream)
                        >>= putMVar result
                isStarted = do
                    evs <- readIORef ref
                    let started = map rootPath (filter Event.isCreated evs)
                    return (all (`elem` started) roots)
                start i = do
                    forM_ roots $ \r -> createFile (r </> ("start" ++ show i))
                    threadDelay 10000
                    ok <- isStarted
                    unless ok $ start (i + 1 :: Int)
            bracket (forkIO reader) killThread $ \_ -> do
                r <- timeout 10000000 (start 0)
                r `shouldBe` Just ()
                mapM_ removePathForcibly roots
                ended <- timeout 10000000 (takeMVar result)
                case ended of
                    Nothing -> expectationFailure "the stream did not end"
                    Just (Left (e :: SomeException)) -> throwIO e
                    Just (Right ()) -> return ()
                evs <- readIORef ref
                let isRootDeleted ev =
                        Event.isDeleted ev && relPath ev == Nothing
                sort (pathsOf isRootDeleted evs) `shouldBe` sort roots
                sort (pathsOf isUnwatched evs) `shouldBe` sort roots

deletedRootsEndStream :: Spec
deletedRootsEndStream = do
    deletedRootsEnd
        "the stream ends when all its roots are deleted" ["root1", "root2"]
    deletedRootsEnd
        "the stream ends when a root listed twice is deleted"
        ["root1", "root1"]
    deletedRootsEnd
        "the stream ends when a root listed via two equal paths is deleted"
        ["root1", "." </> "root1"]

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

-- On Linux the directory is replaced before its create event is read, the
-- file must not be watched as a directory: the write is reported once. The
-- close of "sync" can be reported after its create event, so only the events
-- of @dir@ are checked.
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
            filter ((== dir) . absPath) evs `shouldModify` [dir]

-- The create event of the first @dir@ is read after the second @dir@ is
-- created. The second @dir@ stays watched when it is renamed later.
recDirMovedOutRecreatedRenamed :: Spec
recDirMovedOutRecreatedRenamed =
    it "keeps a directory created at the path of a directory moved out"
        $ withRoot recursive $ \root w -> do
            let dir = root </> "dir"
                dir1 = root </> "dir1"
                file = dir </> "file"
                file1 = dir1 </> "file1"
            createDirectory dir
            renameDirectory dir (outside root)
            createDirectory dir
            _ <- syncOn root "sync" w
            createFile file
            evs <- readUntil (isCreatedAt file) w
            evs `shouldCreate` [file]
            renameDirectory dir dir1
            evs1 <- syncOn root "sync1" w
            createFile file1
            createFile (outside root </> "stray")
            evs2 <- syncOn root "sync2" w
            evs2 `shouldCreate` [file1, root </> "sync2"]
            (evs1 ++ evs2) `shouldUnwatch` []
            filter (isUnder (outside root))
                (map absPath (checkedEvents False (evs1 ++ evs2)))
                `shouldBe` []

-- A directory of the tree is moved out of the inner root and stays in the
-- outer root. On Linux the inner root reports it as no longer watched.
renameOutOfInnerRoot :: Spec
renameOutOfInnerRoot =
    it "a move out of the inner root is a rename within the outer root"
        $ withNestedRoots $ \root sub w -> do
            let dir = sub </> "dir"
                dir1 = root </> "dir1"
                file = dir1 </> "file"
            createDirectory dir
            _ <- syncOn root "sync" w
            renameDirectory dir dir1
            evs <- readUnwatched w sub dir
            evs `shouldUnwatch` [(sub, dir)]
            createFile file
            evs1 <- readUntil (isCreatedAt file) w
            createdRoots file evs1 `shouldBe` [root]

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
        { changeTitle = "subdirectory of a directory moved in replaced"
        , setup = \r -> do
            withSrc r
            createDirectory (srcOf r </> "sub" </> "c")
        , changes = \r -> do
            let dir = r </> "dir"
            renameDirectory (srcOf r) dir
            renameDirectory (dir </> "sub") (dir </> "sub2")
            createDirectory (dir </> "sub")
        , laterFiles = \r ->
            let dir = r </> "dir"
             in [ dir </> "sub" </> "new"
                , dir </> "sub2" </> "new"
                , dir </> "sub2" </> "c" </> "new"
                ]
        , stale = \r -> [r </> "dir" </> "sub" </> "c", srcOf r]
        }
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
-- On Linux every event from the open of the watch is checked, the scan at
-- the start of the watch does not report the entries.
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
                let viaLink p = any (`isUnder` p) (cycleStale root)
                evs `shouldCreate` files
                filter viaLink (map absPath (checkedEvents True evs))
                    `shouldBe` []

-------------------------------------------------------------------------------
-- Handle API
-------------------------------------------------------------------------------

-- | Open a watch on the directory @root@ in a temporary directory, run the
-- action with the root and the watch, and close the watch.
withRootHandle ::
    (Config -> Config) -> (FilePath -> Event.Watch -> IO a) -> IO a
withRootHandle cfg action =
    withSystemTempDirectory "fsevent_dir" $ \fp -> do
        let root = fp </> "root"
        createDirectory root
        r <- Path.fromString root
        bracket
            (Event.openWatch (cfg . allEvents) (r NonEmpty.:| []))
            Event.closeWatch
            (action root)

-- | Read the events from the watch using 'Event.readEvent' until the
-- predicate holds for the events read so far, or the watch is closed.
-- Throw an exception if neither happens within 10 seconds.
readEventsUntil :: ([Event] -> Bool) -> Event.Watch -> IO [Event]
readEventsUntil done w = do
    r <- timeout 10000000 (go [])
    maybe (error "readEventsUntil: timed out") return r

    where

    go acc
        | done (reverse acc) = return (reverse acc)
        | otherwise = do
            r <- Event.readEvent w
            case r of
                Nothing -> return (reverse acc)
                Just ev -> go (ev : acc)

-- | Like 'readUntil' but read the events using 'Event.eventStream'.
readUntilS :: (Event -> Bool) -> Event.Watch -> IO [Event]
readUntilS p w = do
    r <- timeout 10000000
        $ Stream.fold (Fold.takeEndBy p Fold.toList)
        $ Event.eventStream w
    evs <- maybe (error "readUntilS: timed out") return r
    unless (any p evs) $ error "readUntilS: the watch is closed"
    return evs

-- | Wait until the action returns True, check it every 10 ms. Throw an
-- exception if it does not return True within 10 seconds.
waitUntil :: String -> IO Bool -> IO ()
waitUntil desc action = do
    r <- timeout 10000000 go
    maybe (error (desc ++ ": timed out")) return r

    where

    go = do
        ok <- action
        unless ok (threadDelay 10000 >> go)

-- | Run the action while another thread reads the events of the watch up to
-- and including the first event that satisfies the predicate. Return the
-- result of the action and the events read. The reader uses one
-- 'Event.eventStream' from before the action to after it, the stream must
-- report the events of the roots that the action adds.
whileReading :: (Event -> Bool) -> Event.Watch -> IO a -> IO (a, [Event])
whileReading p w action = do
    mv <- newEmptyMVar
    bracket
        (forkIO (try (readUntilS p w) >>= putMVar mv))
        killThread
        $ \_ -> do
            -- Let the reader block in the read
            threadDelay 200000
            r <- action
            evs <- takeMVar mv
            either (\(e :: SomeException) -> throwIO e) (return . (r,)) evs

-- The watch reports a change made right after 'Event.openWatch' returns,
-- without waiting for a marker event. Repeated to catch a watch that starts
-- asynchronously.
openActive :: Spec
openActive =
    it "a change right after openWatch returns is reported"
        $ withSystemTempDirectory "fsevent_dir" $ \fp -> do
            r <- Path.fromString fp
            forM_ [1 .. 50 :: Int] $ \i ->
                bracket
                    (Event.openWatch allEvents (r NonEmpty.:| []))
                    Event.closeWatch
                    $ \w -> do
                        let file = fp </> ("file" ++ show i)
                        createFile file
                        evs <- readUntil (isCreatedAt file) w
                        createdRoots file evs `shouldBe` [fp]

-- The reader thread is blocked reading the watch.
addWhileReading :: Spec
addWhileReading =
    it "addRoot does not block while another thread reads the events"
        $ withRootHandle id $ \root w -> do
            let root2 = takeDirectory root </> "root2"
                file = root2 </> "file"
            createDirectory root2
            p <- Path.fromString root2
            (r, evs) <- whileReading (isCreatedAt file) w $ do
                r <- timeout 2000000 $ Event.addRoot w p
                -- Unblock the reader
                createFile file
                return r
            r `shouldBe` Just ()
            createdRoots file evs `shouldBe` [root2]

-- The tree under the added root is watched, as for a root of
-- 'Event.openWatch'.
addRecursive :: Spec
addRecursive =
    it "addRoot in recursive mode watches the tree under the root"
        $ withRootHandle recursive $ \root w -> do
            let root2 = takeDirectory root </> "root2"
                sub = root2 </> "sub"
                file = sub </> "file"
            createDirectory root2
            createDirectory sub
            p <- Path.fromString root2
            (_, evs) <- whileReading (isCreatedAt file) w $ do
                Event.addRoot w p
                createFile file
            createdRoots file evs `shouldBe` [root2]

-- On macOS each addRoot and removeRoot replaces the FSEvents stream of the
-- watch, the events of the root that stays are not lost or duplicated.
--
-- TODO: Test the root changes with a root on a read-only volume on macOS.
-- A read-only volume has no FSEvents history, replacing the stream cannot
-- replay the events since the last event id and may report isEventsLost
-- instead, as documented for addRoot and removeRoot. No test covers this
-- case now: this test is marked pending when isEventsLost is reported, but
-- its roots are on a writable volume. A read-only volume can be made with
-- "hdiutil create -size 10m -fs APFS" and "hdiutil attach -readonly". Add a
-- directory in the image as a second root, add and remove other roots while
-- files are created in the writable root, and check that every event of
-- the writable root, or an isEventsLost event, is reported. Needs a CI
-- runner that allows attaching a disk image.
keepEventsWhileAdding :: Spec
keepEventsWhileAdding =
    it "a root reports all its events while other roots are added, removed"
        $ withRootHandle id $ \root w -> do
            let base = takeDirectory root
                files = [root </> ("file" ++ show i) | i <- [1 .. n]]
                end = root </> "end"
                dirs = [base </> ("dir" ++ show i) | i <- [1 .. adds]]
                writer = mapM_ createFile files >> createFile end
                changeRoots (i, dir) = do
                    p <- Path.fromString dir
                    Event.addRoot w p
                    when (even i) $ Event.removeRoot w p
            mapM_ createDirectory dirs
            done <- newEmptyMVar
            (_, evs) <- whileReading (isCreatedAt end) w $
                bracket
                    (forkIO (writer `finally` putMVar done ()))
                    killThread
                    $ \_ -> do
                        mapM_ changeRoots (zip [1 :: Int ..] dirs)
                        takeMVar done
            let ofRoot = filter ((== root) . rootPath) evs
            if any Event.isEventsLost evs
            then pendingWith "events were lost, reported by isEventsLost"
            else ofRoot `shouldCreate` (files ++ [end])

    where

    n = 2000 :: Int
    adds = 20 :: Int

-- A change made in the removed root after removeRoot returns is not
-- reported, before or after the isRootUnwatched event of the root.
removeStops :: Spec
removeStops =
    it "a removed root stops reporting its events"
        $ withRootHandle id $ \root w -> do
            let root2 = takeDirectory root </> "root2"
                file = root2 </> "file"
                marker = root </> "marker"
            createDirectory root2
            p <- Path.fromString root2
            Event.addRoot w p
            Event.removeRoot w p
            createFile file
            evs <- readUnwatched w root2 root2
            createFile marker
            evs1 <- readUntil (isCreatedAt marker) w
            filter (isUnder root2) (pathsOf isChange (evs ++ evs1))
                `shouldBe` []

-- On Linux the second removeRoot runs before the isRootUnwatched event is
-- read.
removeTwice :: Spec
removeTwice =
    it "removeRoot twice does not raise an exception"
        $ withTwoRoots $ \root1 root2 w -> do
            let file = root2 </> "file"
            p <- Path.fromString root1
            Event.removeRoot w p
            Event.removeRoot w p
            evs <- readUnwatched w root1 root1
            evs `shouldUnwatch` [(root1, root1)]
            createFile file
            evs1 <- readUntil (isCreatedAt file) w
            createdRoots file evs1 `shouldBe` [root2]

-- The reader thread is blocked reading the watch, the isRootUnwatched event
-- of the removed root ends the read. On macOS the event can follow the
-- event of the file in the other root.
removeWhileReading :: Spec
removeWhileReading =
    it "removeRoot does not block while another thread reads the events"
        $ withTwoRoots $ \root1 root2 w -> do
            let file = root2 </> "file"
            p <- Path.fromString root1
            (r, evs) <- whileReading isUnwatched w $ do
                r <- timeout 2000000 $ Event.removeRoot w p
                createFile file
                return r
            r `shouldBe` Just ()
            evs `shouldUnwatch` [(root1, root1)]

-- The directory @sub@ is watched through both the roots. After the inner
-- root is removed, the outer root reports the changes in @sub@.
overlapRemoveInner :: Spec
overlapRemoveInner =
    it "removing the inner root keeps the outer root watching"
        $ withNestedRoots $ \root sub w -> do
            let changeBoth i = do
                    let file1 = sub </> ("file" ++ show i)
                        file2 = root </> ("marker" ++ show i)
                    createFile file1
                    createFile file2
                    evs <- readUntil (isCreatedAt file2) w
                    return (file1, evs)
            Event.removeRoot w =<< Path.fromString sub
            evs <- readUnwatched w sub sub
            evs `shouldUnwatch` [(sub, sub)]
            (file1, evs1) <- changeBoth (0 :: Int)
            createdRoots file1 evs1 `shouldBe` [root]
            filter (== sub) (map rootPath evs1) `shouldBe` []

-- The events of the root after it is added again are reported. On Linux the
-- root is added again before its isRootUnwatched event is read.
removeAddAgain :: Spec
removeAddAgain =
    it "a root removed and added again reports the events after the add"
        $ withRootHandle id $ \root w -> do
            let file = root </> "file"
            p <- Path.fromString root
            Event.removeRoot w p
            Event.addRoot w p
            createFile file
            evs <- readUntil (isCreatedAt file) w
            evs `shouldUnwatch` [(root, root)]
            createdRoots file evs `shouldBe` [root]

-- The root parent/root2 is stale after it is moved away from its path, and
-- after a new parent/root2 is created at its path. removeRoot and addRoot
-- watch the new directory. Windows does not rename a parent of a watched
-- root, the root itself is renamed. On macOS a root is watched by its path
-- and is never stale.
staleRootReAdded :: Spec
staleRootReAdded =
    it "a root moved away from its path is stale until it is added again"
        $ withRootHandle id $ \root w -> do
            let parent = takeDirectory root </> "parent"
                root2 = parent </> "root2"
                file = root2 </> "file"
            createDirectory parent
            createDirectory root2
            p <- Path.fromString root2
            Event.addRoot w p
            stale w `shouldReturn` []
            moveAway parent root2
            stale w `shouldReturn` expected root2
            createDirectoryIfMissing True root2
            stale w `shouldReturn` expected root2
            Event.removeRoot w p
            Event.addRoot w p
            stale w `shouldReturn` []
            createFile file
            evs <- readUntil (isCreatedAt file) w
            createdRoots file evs `shouldBe` [root2]

    where

    stale w = map Path.toString <$> Event.staleRoots w

#if defined(CABAL_OS_WINDOWS)
    moveAway parent root2 = renameDirectory root2 (parent </> "moved")
#else
    moveAway parent _ =
        renameDirectory parent (takeDirectory parent </> "moved")
#endif

#if defined(CABAL_OS_DARWIN)
    expected _ = []
#else
    expected r = [r]
#endif

closedWatchOps :: Spec
closedWatchOps =
    it "addRoot raises an exception, removeRoot does nothing on a closed watch"
        $ withRootHandle id $ \root w -> do
            p <- Path.fromString root
            Event.closeWatch w
            r <- try (Event.addRoot w p)
            either (\(_ :: IOException) -> True) (const False) r
                `shouldBe` True
            Event.removeRoot w p
            Event.closeWatch w

#if defined(CABAL_OS_WINDOWS)
-- The watch keeps reporting the events of its other roots after the failed
-- add.
fileRootRaises :: Spec
fileRootRaises =
    it "addRoot raises an exception if the root is a regular file"
        $ withRootHandle id $ \root w -> do
            let file = root </> "file"
                file2 = root </> "file2"
            createFile file
            p <- Path.fromString file
            r <- try (Event.addRoot w p)
            either (Just . ioeGetErrorType) (const Nothing) r
                `shouldBe` Just InappropriateType
            createFile file2
            evs <- readUntil (isCreatedAt file2) w
            createdRoots file2 evs `shouldBe` [root]
#endif

#if !defined(CABAL_OS_WINDOWS)
-- Opening a FIFO for reading without O_NONBLOCK blocks until it has a
-- writer, the timeout fails the test if openWatch opens the FIFO that way.
-- System.IO opens the FIFO with O_NONBLOCK, the reader is opened first, an
-- open for writing without a reader fails with ENXIO.
--
-- FreeBSD 14.5 and 15.0 add the watch but do not report a write to the
-- FIFO, the test checks that no isModified event arrives in 3 seconds.
fifoRoot :: Spec
fifoRoot =
    it "a FIFO as a root is watched without opening it"
        $ withSystemTempDirectory "fsevent_dir" $ \fp -> do
            let fifo = fp </> "fifo"
            createNamedPipe fifo (ownerReadMode .|. ownerWriteMode)
            p <- Path.fromString fifo
            let open =
                    timeout 10000000 (Event.openWatch allEvents (p :| []))
                        >>= maybe (error "openWatch: timed out") return
            bracket open Event.closeWatch $ \w ->
                bracket (openFile fifo ReadMode) hClose $ \_ -> do
                    withFile fifo WriteMode (`hPutStr` "x")
#if defined(CABAL_OS_FREEBSD)
                    r <- timeout 3000000 (readUntil (isModifiedAt fifo) w)
                    fmap (const ()) r `shouldBe` Nothing
#else
                    evs <- readUntil (isModifiedAt fifo) w
                    map absPath (filter (isModifiedAt fifo) evs)
                        `shouldBe` [fifo]
#endif

    where

    isModifiedAt file ev = Event.isModified ev && absPath ev == file
#endif

-- The reader thread is blocked reading the watch. The stream ends when the
-- watch is closed, without any file system event.
closeWhileReading :: Spec
closeWhileReading =
    it "closeWatch does not block while another thread reads the events"
        $ withRootHandle id $ \_ w -> do
            mv <- newEmptyMVar
            let reader = Stream.fold Fold.drain (Event.eventStream w)
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

-- The events of the file created before 'Event.closeWatch' can still be in
-- the watch after it, the reads return them before 'Nothing'.
readEventAfterClose :: Spec
readEventAfterClose =
    it "readEvent returns Nothing after closeWatch, and on every later call"
        $ withRootHandle id $ \root w -> do
            let file = root </> "file"
            createFile file
            evs <- readUntil (isCreatedAt file) w
            createdRoots file evs `shouldBe` [root]
            Event.closeWatch w
            _ <- readEventsUntil (const False) w
            isNothing <$> Event.readEvent w `shouldReturn` True
            isNothing <$> Event.readEvent w `shouldReturn` True

    where

    isNothing = maybe True (const False)

-- A read interrupted by a timeout while it waits does not lose an event.
--
-- On Linux, with all the events enabled, 'Event.openWatch' opens and closes
-- the root after adding its watch, the watch reports the open and the close.
-- The reads before the first timeout return these events.
readTimeout :: Spec
readTimeout =
    it "a readEvent interrupted while waiting does not lose an event"
        $ withRootHandle id $ \root w -> do
            forM_ [1 .. 20 :: Int] $ \_ -> readUntilTimeout w
            let file = root </> "file"
            createFile file
            evs <- readUntil (isCreatedAt file) w
            createdRoots file evs `shouldBe` [root]

    where

    readUntilTimeout w = do
        r <- timeout 1000 (Event.readEvent w)
        case r of
            Nothing -> return ()
            Just Nothing -> expectationFailure "the watch is closed"
            Just (Just _) -> readUntilTimeout w

-- Each event is returned to one of the readers, on Linux an event returned
-- to both would fail 'shouldCreate'. The readers end when the watch is
-- closed. The events of the two readers interleave in any order, they are
-- sorted in the order of creation before the check.
twoReaders :: Spec
twoReaders =
    it "two threads reading a watch with readEvent get each event once"
        $ withRootHandle id $ \root w -> do
            let files = [root </> ("file" ++ show i) | i <- [1 .. n]]
            seen <- newIORef []
            let record ev =
                    when (Event.isCreated ev)
                        $ atomicModifyIORef' seen (\s -> (absPath ev : s, ()))
                loop acc = do
                    r <- Event.readEvent w
                    case r of
                        Nothing -> return (reverse acc)
                        Just ev -> record ev >> loop (ev : acc)
                allSeen = do
                    s <- readIORef seen
                    return (all (`elem` s) files)
            mv1 <- newEmptyMVar
            mv2 <- newEmptyMVar
            bracket
                (mapM (\mv -> forkIO (try (loop []) >>= putMVar mv))
                    [mv1, mv2])
                (mapM_ killThread)
                $ \_ -> do
                    mapM_ createFile files
                    waitUntil "twoReaders" allSeen
                    Event.closeWatch w
                    evs1 <- takeMVar mv1 >>= either rethrow return
                    evs2 <- takeMVar mv2 >>= either rethrow return
                    let byFile ev = elemIndex (absPath ev) files
                    sortOn byFile (evs1 ++ evs2) `shouldCreate` files

    where

    n = 200 :: Int
    rethrow (e :: SomeException) = throwIO e

handleTests :: Spec
handleTests = do
    openActive
    addWhileReading
    addRecursive
    keepEventsWhileAdding
    removeStops
    removeTwice
    removeWhileReading
    removeAddAgain
    staleRootReAdded
    closedWatchOps
#if defined(CABAL_OS_WINDOWS)
    fileRootRaises
#endif
    closeWhileReading
    readEventAfterClose
    readTimeout
    twoReaders

-------------------------------------------------------------------------------
-- Resources of a watch
-------------------------------------------------------------------------------

-- | The directory to be watched, and the count of the watch resources of
-- the process before the watch is opened.
data Probe = Probe FilePath Int

-- | Take the probe before a watch is opened on the directory.
newProbe :: FilePath -> IO Probe

-- | Whether a watch opened after the probe is open.
isWatchOpen :: Probe -> IO Bool

-- | Whether the watches opened after the probe are closed.
isWatchClosed :: Probe -> IO Bool

#if defined(CABAL_OS_LINUX)
-- | The number of inotify fds of the process, a watch has one. The test is
-- marked pending if @\/proc@ is not available.
inotifyFds :: IO Int
inotifyFds = do
    r <- try (listDirectory "/proc/self/fd")
    case r of
        Left (_ :: IOException) ->
            pendingWith "/proc is not available" >> return 0
        Right fds -> length . filter id <$> mapM isInotify fds

    where

    isInotify fd = do
        r <- try (getSymbolicLinkTarget ("/proc/self/fd" </> fd))
        return $ case r of
            Left (_ :: IOException) -> False
            Right target -> target == "anon_inode:inotify"

newProbe dir = Probe dir <$> inotifyFds
isWatchOpen (Probe _ n) = (== n + 1) <$> inotifyFds
isWatchClosed (Probe _ n) = (== n) <$> inotifyFds
#elif defined(CABAL_OS_FREEBSD)
foreign import capi "fcntl.h fcntl" c_fcntl :: CInt -> CInt -> IO CInt

foreign import capi "fcntl.h value F_GETFD" f_GETFD :: CInt

-- | The number of open fds of the process below 1024, a watch has one.
-- FreeBSD has no @\/proc\/self\/fd@, and @\/dev\/fd@ lists only the fds 0,
-- 1 and 2 unless fdescfs is mounted.
countFds :: IO Int
countFds =
    length . filter (/= -1) <$> mapM (`c_fcntl` f_GETFD) [0 .. 1023]

newProbe dir = Probe dir <$> countFds
isWatchOpen (Probe _ n) = (> n) <$> countFds
isWatchClosed (Probe _ n) = (<= n) <$> countFds
#elif defined(CABAL_OS_DARWIN)
-- | The number of open fds of the process, a watch has the two fds of its
-- pipe.
countFds :: IO Int
countFds = length <$> listDirectory "/dev/fd"

newProbe dir = Probe dir <$> countFds
isWatchOpen (Probe _ n) = (> n) <$> countFds
isWatchClosed (Probe _ n) = (<= n) <$> countFds
#else
-- | Whether the directory can be opened with no sharing, it cannot be if
-- any other handle with data access to the directory is open. A watch keeps
-- a handle of its root open.
canOpenExclusive :: FilePath -> IO Bool
canOpenExclusive dir = do
    r <- try $ do
        h <- Win32.createFile dir
            Win32.fILE_LIST_DIRECTORY
            Win32.fILE_SHARE_NONE
            Nothing
            Win32.oPEN_EXISTING
            Win32.fILE_FLAG_BACKUP_SEMANTICS
            Nothing
        Win32.closeHandle h
    return $ either (\(_ :: SomeException) -> False) (const True) r

newProbe dir = return (Probe dir 0)
-- An open with no sharing while the watch opens its root would make the
-- watch fail with a sharing violation, wait for the watch instead.
isWatchOpen _ = threadDelay 500000 >> return True
isWatchClosed (Probe dir _) = canOpenExclusive dir
#endif

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

-- | The watch must be open within 5 seconds.
waitOpen :: Probe -> IO ()
waitOpen probe = retryUntil (isWatchOpen probe) >>= (`shouldBe` True)

-- | The watches must be closed after garbage collection.
gcClosed :: Probe -> IO ()
gcClosed probe =
    retryUntil (performGC >> isWatchClosed probe) >>= (`shouldBe` True)

#if !defined(CABAL_OS_WINDOWS)
-- | The number of fds open in a child process. The child inherits the fds
-- that are not close-on-exec. 'forkProcess' is not used, in a threaded
-- program the forked child can fail before it runs the command.
childFds :: FilePath -> IO Int
childFds out = do
    callProcess "/bin/sh" ["-c", "ls /dev/fd > " ++ out]
    s <- readFile out
    return $! length (lines s)

-- The watch is open and reports events when the child is created. On Linux
-- the inotify fd is close-on-exec, on macOS the two fds of the pipe are.
childInherits :: Spec
childInherits =
    it "a child process does not inherit the fds of a watch"
        $ withSystemTempDirectory "fsevent_dir" $ \fp -> do
            let out = fp </> "fds"
                root = fp </> "root"
                file = root </> "file"
            createDirectory root
            n0 <- childFds out
            when (n0 == 0) $ pendingWith "/dev/fd cannot be listed"
            n1 <- withWatch id [root] $ \w -> do
                createFile file
                _ <- readUntil (isCreatedAt file) w
                childFds out
            n1 `shouldBe` n0
#endif

resourceTests :: Spec
resourceTests =
#if !defined(CABAL_OS_WINDOWS)
    childInherits
#else
    return ()
#endif

-------------------------------------------------------------------------------
-- Async exceptions
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
-- exception fails the test, for example, reaching a limit on the watches.
shouldBeKilled :: MVar (Either SomeException ()) -> Expectation
shouldBeKilled result = do
    r <- timeout 5000000 (takeMVar result)
    case r of
        Nothing -> expectationFailure "the thread did not end"
        Just (Right ()) -> expectationFailure "the thread was not killed"
        Just (Left e) ->
            when (fromException e /= Just ThreadKilled) $ throwIO e

-- | A recursive watch stream of the path.
recStream :: FilePath -> IO (Stream.Stream IO Event)
recStream root = do
    p <- Path.fromString root
    return $ Event.watchWith recursive (p NonEmpty.:| [])

-- An exception raised in the stream closes the watch without a garbage
-- collection.
killWaitingReader :: Spec
killWaitingReader =
    it "killing a thread waiting for events closes the watch at once"
        $ withSystemTempDirectory "fsevent_dir" $ \fp -> do
            probe <- newProbe fp
            stream <- recStream fp
            (tid, result) <- forkResult (Stream.fold Fold.drain stream)
            waitOpen probe
            -- Let the reader block in the wait for events
            threadDelay 200000
            killThread tid
            shouldBeKilled result
            isWatchClosed probe `shouldReturn` True

-- On Linux the scan of the tree runs in the masked allocation of the
-- bracket. The exception can be delivered when the allocation ends, outside
-- the stream, the watch is then closed by the garbage collection.
killStartingWatch :: Spec
killStartingWatch =
    it "killing a thread during the start of a watch closes the watch"
        $ withSystemTempDirectory "fsevent_dir" $ \fp -> do
            mapM_
                (\i -> createDirectory (fp </> show i))
                [1 .. 2000 :: Int]
            probe <- newProbe fp
            stream <- recStream fp
            let kill delay = do
                    (tid, result) <-
                        forkResult (Stream.fold Fold.drain stream)
                    threadDelay delay
                    killThread tid
                    shouldBeKilled result
                    gcClosed probe
            mapM_ kill [0, 100, 1000, 5000, 20000, 50000]

-- The delays cover the allocation, the first read and the wait. The garbage
-- collections keep the number of unclosed watches below the limits of the
-- system.
killRepeatedly :: Spec
killRepeatedly =
    it "killing a thread at any point leaves no watch and no blocked thread"
        $ withSystemTempDirectory "fsevent_dir" $ \fp -> do
            createDirectory (fp </> "sub")
            probe <- newProbe fp
            stream <- recStream fp
            let kill i = do
                    (tid, result) <-
                        forkResult (Stream.fold Fold.drain stream)
                    when (i `mod` 3 /= 0) $ threadDelay (i `mod` 400)
                    killThread tid
                    shouldBeKilled result
                    when (i `mod` 50 == 0) performGC
            mapM_ kill [1 .. 2000 :: Int]
            gcClosed probe

-- The consumer runs outside the stream, the stream does not see the
-- exception. The watch is closed by the garbage collection.
killConsumer :: Spec
killConsumer =
    it "killing a thread in the consumer closes the watch on GC"
        $ withSystemTempDirectory "fsevent_dir" $ \fp -> do
            probe <- newProbe fp
            stream <- recStream fp
            consuming <- newEmptyMVar
            let consume _ = do
                    _ <- tryPutMVar consuming ()
                    threadDelay 60000000
            (tid, result) <-
                forkResult (Stream.fold (Fold.drainMapM consume) stream)
            waitOpen probe
            createFile (fp </> "file")
            timeout 5000000 (takeMVar consuming) `shouldReturn` Just ()
            killThread tid
            shouldBeKilled result
            gcClosed probe

-- 'Stream.take' stops without draining the stream, the bracket of the
-- stream does not see the end. The watch is closed by the garbage
-- collection.
abandonStream :: Spec
abandonStream =
    it "a stream abandoned by the consumer closes the watch on GC"
        $ withSystemTempDirectory "fsevent_dir" $ \fp -> do
            probe <- newProbe fp
            stream <- recStream fp
            (_, result) <-
                forkResult (Stream.fold Fold.drain (Stream.take 1 stream))
            waitOpen probe
            createFile (fp </> "file")
            r <- timeout 5000000 (takeMVar result)
            fmap (either (const False) (const True)) r `shouldBe` Just True
            gcClosed probe

-- On Linux the read that times out reports dir and dir/file, reaches the
-- end of the scan of dir, which closes dir, and waits for an event.
readAfterTimeout :: String -> (Config -> Config) -> Spec
readAfterTimeout desc cfg =
    it ("reads again after reads that time out, " ++ desc)
        $ withRootCfg (recursive . cfg) $ \root w -> do
            let dir = root </> "dir"
                file = root </> "file"
                drain =
                    timeout 500000
                        $ Stream.fold Fold.drain (Event.eventStream w)
            createDirectory dir
            createFile (dir </> "file")
            drain `shouldReturn` Nothing
            drain `shouldReturn` Nothing
            createFile file
            evs <- readUntil (isCreatedAt file) w
            createdRoots file evs `shouldBe` [root]

-- The reads time out after 1 to 200 us. The reader masks async exceptions
-- around the read and the code that records the event. The exception of a
-- timeout is then raised only by the read, while it waits for an event or
-- before it takes one out of the watch, and no event is lost. The last read
-- collects the remaining events, up to the event of a file created after
-- the reads.
--
-- On macOS, with 2000 files created before the first read, the watch
-- reported about 1100 of them and then 'Event.isEventsLost'. The test
-- creates 300 files on macOS and Windows.
readTimeoutsNoLoss :: Spec
readTimeoutsNoLoss =
    it "does not lose an event when the reads time out repeatedly"
        $ withRoot recursive $ \root w -> do
            let n = if onInotify then 2000 else 300 :: Int
                files = [root </> ("file" ++ show i) | i <- [1 .. n]]
                end = root </> "end"
            ref <- newIORef []
            let collect ev = atomicModifyIORef' ref (\evs -> (ev : evs, ()))
                drain t =
                    timeout t
                        $ mask_
                        $ Stream.fold (Fold.drainMapM collect)
                        $ Event.eventStream w
            mapM_ createFile files
            mapM_ (\i -> drain (1 + i `mod` 200)) [1 .. 2000 :: Int]
            createFile end
            evs <- readUntilS (isCreatedAt end) w
            earlier <- readIORef ref
            let allEvs = reverse earlier ++ evs
            when (any Event.isEventsLost allEvs)
                $ pendingWith "events were lost, reported by isEventsLost"
            allEvs `shouldCreate` (files ++ [end])

-- The reader has async exceptions masked, and an exception is thrown to it
-- before it starts to read. The events of the files are in the watch by
-- then, the reads do not wait. The first read must raise the exception
-- without taking an event out of the watch.
pendingException :: Spec
pendingException =
    it "a read raises a pending async exception before it takes an event"
        $ withRootCfg id $ \root w -> do
            let files = [root </> ("file" ++ show i) | i <- [1 .. 100 :: Int]]
                end = root </> "end"
            ref <- newIORef []
            ready <- newEmptyMVar
            start <- newEmptyMVar
            result <- newEmptyMVar
            let record ev = atomicModifyIORef' ref (\evs -> (ev : evs, ()))
                loop = do
                    r <- Event.readEvent w
                    case r of
                        Nothing -> return ()
                        Just ev -> record ev >> loop
                reader = do
                    putMVar ready ()
                    uninterruptibleMask_ (takeMVar start)
                    loop
            mapM_ createFile files
            -- Let the events reach the watch
            threadDelay 500000
            tid <-
                mask $ \_ -> forkIO (try reader >>= putMVar result)
            takeMVar ready
            _ <- forkIO (killThread tid)
            -- Let the exception reach the reader
            threadDelay 200000
            putMVar start ()
            shouldBeKilled result
            taken <- readIORef ref
            length taken `shouldBe` 0
            createFile end
            evs <- readUntil (isCreatedAt end) w
            evs `shouldCreate` (files ++ [end])

asyncTests :: Spec
asyncTests = do
    pendingException
    readTimeoutsNoLoss
    killWaitingReader
    killStartingWatch
    killRepeatedly
    killConsumer
    abandonStream
    readAfterTimeout "all events" allEvents
    readAfterTimeout "default events" id

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
                relativeRoot
                normalizedRoots
#if !defined(CABAL_OS_WINDOWS)
                fifoRoot
#endif
                deletedRootsEndStream
            describe "overlapping roots" $ do
                overlapRemoveInner
                renameOutOfInnerRoot
            describe "recursive mode" $ do
                recSubdirMovedDeleted
                recManyNewDirs
                recDirReplacedByFile
                recDirMovedOutRecreatedRenamed
                recStartSymlinkCycles
            describe "recursive mode changes to the tree"
                $ mapM_ runTreeChange treeChanges
            describe "handle API" handleTests
            describe "resources" resourceTests
            describe "async exceptions" asyncTests

main :: IO ()
main = do
    let run = runTests moduleName "non-recursive" (failNested Event.watch)
    run DirType (nestedOpsTest : commonTests)
    run SymLinkOrigPath (nestedOpsTest : commonTests)

    let runRec = runTests moduleName "recursive" Event.watchRecursive
    runRec DirType (commonTests ++ commonRecTests)
    runRec SymLinkOrigPath (commonTests ++ commonRecTests)

#if !defined(CABAL_OS_WINDOWS)
    -- A regular file cannot be a watch root on Windows, see fileRootRaises.
    let fileRootTests = [fileModify "" (const [])]
    run FileType fileRootTests
    runRec FileType fileRootTests
#endif

    watchTests
