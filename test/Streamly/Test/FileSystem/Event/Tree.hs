-- |
-- Module      : Streamly.Test.FileSystem.Event.Tree
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Helpers for the tests of a recursive watch on a large or busy tree: the
-- limits on the size of a tree, a consumer that records the events, marker
-- files to synchronize with the consumer, a check that every directory of
-- the tree is watched, and a model of the tree built by the consumer from
-- the events.

module Streamly.Test.FileSystem.Event.Tree
    (
    -- * Platform
      platformName
    , isLinux
    , isWindows
    , createFile
    , absPath

    -- * Limits
    , queuedEventsLimit
    , Budget (..)
    , dirBudget
    , stressBudget

    -- * Tree
    , treePaths

    -- * Watch
    , Track (..)
    , Watch (..)
    , startWatch
    , stopWatch
    , sync
    , pause
    , resume
    , lostCount
    , settledLost
    , streamEnded

    -- * Probes
    , ProbeResult (..)
    , probeTree
    , probeOk

    -- * Model
    , ModelResult (..)
    , checkModel
    )
where

import Control.Concurrent
    ( Chan, MVar, ThreadId, forkIO, killThread, newChan, newEmptyMVar
    , putMVar, readChan, takeMVar, threadDelay, tryReadMVar, tryTakeMVar
    , writeChan
    )
import Control.Exception (IOException, SomeException, bracket, try)
#if defined(CABAL_OS_LINUX)
import Control.Exception (evaluate)
#endif
import Control.Monad (forM_, unless, when)
import Data.Function ((&))
import Data.IORef
    ( IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef
    , writeIORef
    )
import Data.List (isPrefixOf, sortOn)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Map.Strict (Map)
import Data.Set (Set)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath
    ((</>), makeRelative, pathSeparator, takeDirectory, takeFileName)
import System.Timeout (timeout)
import Streamly.FileSystem.Event (Config, Event)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.FileSystem.Event as Event
import qualified Streamly.FileSystem.Path as Path
import qualified Streamly.Internal.FS.Event as Handle
    (closeWatch, eventStream, openWatch)
#if defined(CABAL_OS_LINUX)
import qualified Streamly.FileSystem.Event.Linux as Linux
#endif

#if defined(CABAL_OS_WINDOWS)
import System.FilePath (takeDrive)
import System.Win32.File (getDiskFreeSpace)
#else
import System.Process (readProcess)
#endif

-------------------------------------------------------------------------------
-- Platform
-------------------------------------------------------------------------------

platformName :: String
#if defined(CABAL_OS_LINUX)
platformName = "Linux"
#elif defined(CABAL_OS_DARWIN)
platformName = "macOS"
#else
platformName = "Windows"
#endif

isLinux :: Bool
isLinux = platformName == "Linux"

isWindows :: Bool
isWindows = platformName == "Windows"

createFile :: FilePath -> IO ()
createFile path = writeFile path ""

absPath :: Event -> FilePath
absPath = Path.toString . Event.getAbsPath

-------------------------------------------------------------------------------
-- Limits
-------------------------------------------------------------------------------

-- | @max_queued_events@ on Linux. On the other platforms, a number of events
-- that overflows the event buffer of a watch if the consumer is blocked.
queuedEventsLimit :: IO Int
#if defined(CABAL_OS_LINUX)
queuedEventsLimit =
    read <$> readFile "/proc/sys/fs/inotify/max_queued_events"
#else
queuedEventsLimit = return 20000
#endif

#if defined(CABAL_OS_LINUX)
-- | The inotify watches in use by the processes of this user, counted from
-- the fdinfo of the inotify fds. Processes that cannot be read are skipped.
watchesInUse :: IO Int
watchesInUse = do
    pids <- filter (all (`elem` ['0'..'9'])) <$> listDirectory "/proc"
    sum <$> mapM countPid pids

    where

    countPid pid = do
        r <- try (listDirectory ("/proc" </> pid </> "fdinfo"))
        case r of
            Left (_ :: SomeException) -> return 0
            Right fds -> sum <$> mapM (countFd pid) fds

    -- readFile is lazy, the count is evaluated inside try so that an fd
    -- closed during the read is skipped.
    countFd pid fd = do
        r <- try $ do
            s <- readFile ("/proc" </> pid </> "fdinfo" </> fd)
            evaluate
                $ length (filter ("inotify wd:" `isPrefixOf`) (lines s))
        case r of
            Left (_ :: SomeException) -> return 0
            Right n -> return n

-- | The inotify watches that this user can still add, leaving 2000 free.
freeWatches :: IO Int
freeWatches = do
    limit <- read <$> readFile "/proc/sys/fs/inotify/max_user_watches"
    used <- watchesInUse
    putStrLn $ "max_user_watches " ++ show limit ++ ", in use " ++ show used
    return (limit - used - 2000)
#endif

#if !defined(CABAL_OS_WINDOWS)
-- | The fourth column of the @df -P@ output for the file system of a path.
dfAvail :: [String] -> FilePath -> IO Int
dfAvail opts dir = do
    out <- readProcess "df" (opts ++ ["-P", dir]) ""
    case lines out of
        [_, l] | (_ : _ : _ : avail : _) <- words l -> return (read avail)
        _ -> error ("dfAvail: cannot parse df output: " ++ out)
#endif

-- | The free inodes of the file system of a path. 'Nothing' if the file
-- system has no fixed number of inodes: on macOS (APFS) and on Windows.
freeInodes :: FilePath -> IO (Maybe Int)
#if defined(CABAL_OS_LINUX)
freeInodes dir = Just <$> dfAvail ["-i"] dir
#else
freeInodes _ = return Nothing
#endif

-- | The free KB of the file system of a path.
freeKBytes :: FilePath -> IO Int
#if defined(CABAL_OS_WINDOWS)
freeKBytes dir = do
    (sectors, bytes, clusters, _) <- getDiskFreeSpace (Just (takeDrive dir))
    return
        $ fromIntegral sectors * fromIntegral bytes * fromIntegral clusters
            `div` 1024
#else
freeKBytes = dfAvail ["-k"]
#endif

-- | The limits on the size of a tree created by a test.
data Budget = Budget
    { maxDirs :: Int
    -- | The inodes used by a directory and its files.
    , inodesPerDir :: Int
    -- | The inodes used by the files that are not in a directory of the
    -- tree.
    , fixedInodes :: Int
    -- | The bytes used by a directory and its files.
    , bytesPerDir :: Int
    -- | Use at most @1/diskFraction@ of the free inodes and space.
    , diskFraction :: Int
    -- | On Linux, use at most @1/watchFraction@ of the free inotify watches.
    , watchFraction :: Int
    }

-- | The number of directories, at most 'maxDirs', that a test can create in
-- a directory.
dirBudget :: Budget -> FilePath -> IO Int
dirBudget Budget{..} dir = do
    inodes <- freeInodes dir
    kbytes <- freeKBytes dir
#if defined(CABAL_OS_LINUX)
    watches <- freeWatches
    let byWatches = [watches `div` watchFraction]
#else
    let byWatches = []
#endif
    let perDir n = (n `div` diskFraction - fixedInodes) `div` inodesPerDir
        byInodes = maybe [] (\n -> [perDir n]) inodes
        bySpace = kbytes * 1024 `div` (diskFraction * bytesPerDir)
    putStrLn $ "free inodes " ++ maybe "unlimited" show inodes
        ++ ", free KB " ++ show kbytes
    return $ minimum ([maxDirs, bySpace] ++ byInodes ++ byWatches)

-- | @stressBudget dirs inodes fixed@ uses a tenth of the free inodes and
-- space, and half of the free inotify watches.
stressBudget :: Int -> Int -> Int -> Budget
stressBudget dirs inodes fixed =
    Budget
        { maxDirs = dirs
        , inodesPerDir = inodes
        , fixedInodes = fixed
        , bytesPerDir = 4096
        , diskFraction = 10
        , watchFraction = 2
        }

-------------------------------------------------------------------------------
-- Tree
-------------------------------------------------------------------------------

-- | The relative paths of a tree of @n@ directories, including the top
-- directory @top@, with 10 subdirectories in each directory, breadth first.
treePaths :: FilePath -> Int -> [FilePath]
treePaths top n = take n (go [top])

    where

    go [] = []
    go xs = xs ++ go [x </> ("d" ++ show i) | x <- xs, i <- [0 .. 9 :: Int]]

-------------------------------------------------------------------------------
-- Model of the tree
-------------------------------------------------------------------------------

-- The model is the set of the absolute paths of the entries under the root,
-- as a consumer learns them from the events. For each event the consumer
-- checks the path on the disk, the flags of the event are not used: on
-- macOS the flags of several changes are merged. On 'Event.isEventsLost' it
-- lists the tree under the path of the event again.

-- | The entries under @dir@, recursively. An entry removed during the
-- listing is skipped.
listTree :: FilePath -> IO [FilePath]
listTree dir = do
    r <- try (listDirectory dir)
    case r of
        Left (_ :: IOException) -> return []
        Right xs -> concat <$> mapM entry xs

    where

    entry x = do
        let path = dir </> x
        isDir <- doesDirectoryExist path
        (path :) <$> if isDir then listTree path else return []

-- | 'True' if @path@ is under @dir@.
isUnder :: FilePath -> FilePath -> Bool
isUnder dir path = (dir ++ [pathSeparator]) `isPrefixOf` path

-- | Remove the paths under @dir@. These paths are contiguous in the set.
removeUnder :: FilePath -> Set FilePath -> Set FilePath
removeUnder dir s =
    let prefix = dir ++ [pathSeparator]
        (before, after) = Set.spanAntitone (< prefix) s
     in Set.union before (Set.dropWhileAntitone (prefix `isPrefixOf`) after)

-- | Update the model for an event of @path@ under @top@. A directory not in
-- the model is listed, its entries may have been created before it was
-- moved into the tree.
updateModel :: IORef (Set FilePath) -> FilePath -> FilePath -> IO ()
updateModel ref top path =
    when (isUnder top path) $ do
        isDir <- doesDirectoryExist path
        isFile <- doesFileExist path
        known <- Set.member path <$> readIORef ref
        if isDir
        then unless known $ do
            xs <- listTree path
            modifyIORef' ref (Set.union (Set.fromList (path : xs)))
        else if isFile
        then modifyIORef' ref (Set.insert path)
        else modifyIORef' ref (Set.delete path . removeUnder path)

-- | List the tree under @path@ again, after an 'Event.isEventsLost' event.
rescanModel :: IORef (Set FilePath) -> FilePath -> FilePath -> IO ()
rescanModel ref top path = do
    modifyIORef' ref (Set.delete path . removeUnder path)
    if path == top
    then do
        xs <- listTree top
        modifyIORef' ref (Set.union (Set.fromList xs))
    else updateModel ref top path

-------------------------------------------------------------------------------
-- Watch
-------------------------------------------------------------------------------

-- | What the consumer records besides the counts and the probes.
data Track =
      TrackNothing
    -- | The paths reported by 'Event.isCreated'.
    | TrackCreated
    -- | A model of the tree, see 'checkModel'.
    | TrackModel
    deriving Eq

-- | A watch whose events are recorded by a consumer thread.
--
-- A file whose name starts with @sync-@ is a marker, the events before its
-- create event have been read when the consumer reads it. A file whose name
-- starts with @pause-@ blocks the consumer when its create event is read,
-- until 'resume'.
data Watch = Watch
    { root :: FilePath
    , lost :: IORef Int
    -- | Directories reported by the scan of a new directory, Linux only.
    , scanDirs :: IORef Int
    -- | The paths reported by 'Event.isCreated', if 'trackCreated'.
    , created :: IORef (Set FilePath)
    , trackCreated :: Bool
    -- | The model of the tree, if 'TrackModel'.
    , model :: Maybe (IORef (Set FilePath))
    -- | The number of 'Event.isCreated' events of each file named @probe@.
    , probes :: IORef (Map FilePath Int)
    -- | The names of the entries of the root that an event was reported
    -- for.
    , rootEntries :: IORef (Set FilePath)
    , markers :: Chan FilePath
    -- | The marker names already read, an event can be repeated on macOS.
    , seen :: IORef (Set FilePath)
    , paused :: MVar ()
    , resumed :: MVar ()
    , finished :: MVar (Either SomeException ())
    , thread :: ThreadId
    }

isScanDir :: Event -> Bool
#if defined(CABAL_OS_LINUX)
isScanDir ev = Linux.isScanEvent ev && Linux.isDir ev
#else
isScanDir _ = False
#endif

observe :: Watch -> Event -> IO ()
observe Watch{..} ev = do
    let path = absPath ev
        name = takeFileName path
        marker prefix = Event.isCreated ev && prefix `isPrefixOf` name
    when (Event.isEventsLost ev) $ modifyIORef' lost (+ 1)
    -- On Linux the stream ends after the event, the tree is listed again
    -- after a new watch is opened, see 'startWatch'.
    forM_ model $ \ref ->
        if Event.isEventsLost ev
        then unless isLinux $ rescanModel ref root path
        else updateModel ref root path
    when (takeDirectory path == root)
        $ modifyIORef' rootEntries (Set.insert name)
    when (Event.isCreated ev) $ do
        when (isScanDir ev) $ modifyIORef' scanDirs (+ 1)
        when trackCreated $ modifyIORef' created (Set.insert path)
        when (name == "probe")
            $ modifyIORef' probes (Map.insertWith (+) path 1)
    -- The consumer pauses once for all the markers of a 'pause', the markers
    -- differ in the number after the last "-".
    when (marker "sync-" || marker "pause-") $ do
        let isSync = "sync-" `isPrefixOf` name
            key =
                if isSync
                then name
                else reverse (drop 1 (dropWhile (/= '-') (reverse name)))
        new <- atomicModifyIORef' seen
            (\s -> (Set.insert key s, not (Set.member key s)))
        when new $
            if isSync
            then writeChan markers name
            else putMVar paused () >> takeMVar resumed

-- | The number of 'Event.isEventsLost' events read so far.
lostCount :: Watch -> IO Int
lostCount = readIORef . lost

-- | 'Just' the result if the stream has ended.
streamEnded :: Watch -> IO (Maybe (Either SomeException ()))
streamEnded = tryReadMVar . finished

-- | Create a marker file named @name-i@ in the root, every 100 ms, until the
-- consumer reads one of them. Raise an exception if the stream has ended or
-- if no marker has been read in 300 seconds.
sync :: Watch -> String -> IO ()
sync w@Watch{..} name = go (0 :: Int)

    where

    prefix = "sync-" ++ name ++ "-"

    wait = do
        n <- readChan markers
        unless (prefix `isPrefixOf` n) wait

    go i = do
        when (i >= 3000)
            $ error ("sync: no marker read for " ++ name)
        ended <- streamEnded w
        case ended of
            Just r -> error ("sync: the stream ended: " ++ show r)
            Nothing -> do
                createFile (root </> (prefix ++ show i))
                r <- timeout 100000 wait
                maybe (go (i + 1)) return r

-- | Block the consumer, it does not read events until 'resume'. A marker
-- file named @pause-name-i@ is created in the root every 100 ms, until the
-- consumer reads one of them; an overflow can drop a marker. Raise an
-- exception if no marker has been read in 60 seconds.
--
-- 'tryTakeMVar' is used instead of 'timeout' around 'takeMVar': the timeout
-- can interrupt the thread after 'takeMVar' has taken the value, the value is
-- lost and the consumer stays blocked.
pause :: Watch -> String -> IO ()
pause Watch{..} name = go (0 :: Int)

    where

    go i = do
        when (i >= 600)
            $ error ("pause: the consumer did not read " ++ name)
        createFile (root </> ("pause-" ++ name ++ "-" ++ show i))
        threadDelay 100000
        r <- tryTakeMVar paused
        maybe (go (i + 1)) return r

resume :: Watch -> IO ()
resume Watch{..} = putMVar resumed ()

-- | Start a watch of @dir@ and wait until the consumer reads events.
--
-- On Linux the stream ends after an overflow, the consumer then opens a new
-- watch, as an application does. With 'TrackModel' the consumer lists the
-- tree after it opens a watch.
startWatch :: (Config -> Config) -> Track -> FilePath -> IO Watch
startWatch cfg track dir = do
    lostRef <- newIORef 0
    scanRef <- newIORef 0
    createdRef <- newIORef Set.empty
    modelRef <-
        if track == TrackModel
        then Just <$> newIORef Set.empty
        else return Nothing
    probesRef <- newIORef Map.empty
    entriesRef <- newIORef Set.empty
    chan <- newChan
    seenRef <- newIORef Set.empty
    pausedVar <- newEmptyMVar
    resumedVar <- newEmptyMVar
    finishedVar <- newEmptyMVar
    started <- newEmptyMVar
    let mk tid = Watch
            { root = dir
            , lost = lostRef
            , scanDirs = scanRef
            , created = createdRef
            , trackCreated = track == TrackCreated
            , model = modelRef
            , probes = probesRef
            , rootEntries = entriesRef
            , markers = chan
            , seen = seenRef
            , paused = pausedVar
            , resumed = resumedVar
            , finished = finishedVar
            , thread = tid
            }
        consumer = do
            w <- mk <$> takeMVar started
            rootPath <- Path.fromString dir
            let open =
                    Handle.openWatch
                        (Event.setRecursiveMode True . cfg) (rootPath :| [])
                -- The tree is listed after the watch is open, a change made
                -- during the listing is reported by the watch.
                run = do
                    n0 <- lostCount w
                    bracket open Handle.closeWatch $ \h -> do
                        forM_ (model w) $ \ref -> rescanModel ref dir dir
                        Handle.eventStream h
                            & Stream.fold (Fold.drainMapM (observe w))
                    n1 <- lostCount w
                    when (isLinux && n1 > n0) run
            run
    tid <- forkIO (try consumer >>= putMVar finishedVar)
    putMVar started tid
    let w = mk tid
    sync w "start"
    return w

-- | Stop the consumer and wait for it to close the watch.
stopWatch :: Watch -> IO ()
stopWatch w = do
    killThread (thread w)
    _ <- takeMVar (finished w)
    return ()

-- | Sync, then wait until no 'Event.isEventsLost' event is read for 2
-- seconds. Returns the number of these events. Raise an exception if they
-- do not stop in 120 seconds.
settledLost :: Watch -> String -> IO Int
settledLost w name = go (0 :: Int)

    where

    go i = do
        when (i >= 60) $ error "settledLost: isEventsLost events do not stop"
        sync w (name ++ "-" ++ show i ++ "-a")
        n1 <- lostCount w
        threadDelay 2000000
        sync w (name ++ "-" ++ show i ++ "-b")
        n2 <- lostCount w
        if n1 == n2 then return n2 else go (i + 1)

-------------------------------------------------------------------------------
-- Probes
-------------------------------------------------------------------------------

data ProbeResult = ProbeResult
    { probeDirs :: Int
    , probeMissing :: Int
    , probeExtra :: Int
    , probeRepeated :: Int
    , probeLost :: Int -- ^ 'Event.isEventsLost' events during the probes.
    } deriving Show

-- | The batch of probe files that does not overflow the event buffer.
-- Windows has a 63 KB buffer for each watch root.
probeBatch :: Int
probeBatch = if isWindows then 200 else 2000

-- | Print up to 5 of the directories of the probe paths, relative to the
-- root, the shortest first. The shortest paths are the closest to the top of
-- a subtree whose probes are missing.
printProbes :: FilePath -> String -> [FilePath] -> IO ()
printProbes top label paths =
    unless (null paths) $
        putStrLn $ "  " ++ label ++ " probes in: "
            ++ show (take 5 (sortOn length (map rel paths)))

    where

    rel = makeRelative top . takeDirectory

-- | Create a file named @probe@ in every directory of the tree under the
-- root, and count the probes that are not reported once. The probes are
-- created in batches that do not overflow the event queue, a batch is
-- checked after its sync. Only the paths of its missing probes are kept. The
-- paths of the missing probes and of the probes reported at paths not probed
-- are printed, see 'printProbes'.
probeTree :: Watch -> String -> IO ProbeResult
probeTree w name = do
    lost0 <- lostCount w
    batchRef <- newIORef []
    missingRef <- newIORef []
    total <- newIORef (0 :: Int, 0 :: Int, 0 :: Int, 0 :: Int)
    let flush = do
            batch <- readIORef batchRef
            unless (null batch) $ do
                writeIORef batchRef []
                (n, _, _, _) <- readIORef total
                sync w (name ++ "-probe-" ++ show n)
                let keys = Set.fromList batch
                found <- atomicModifyIORef' (probes w) $ \m ->
                    (Map.withoutKeys m keys, Map.restrictKeys m keys)
                let missingKeys = Set.difference keys (Map.keysSet found)
                    missing = Set.size missingKeys
                    repeated = Map.size (Map.filter (/= 1) found)
                modifyIORef' missingRef (Set.toList missingKeys ++)
                (a, b, c, d) <- readIORef total
                let !a1 = a + 1
                    !b1 = b + length batch
                    !c1 = c + missing
                    !d1 = d + repeated
                writeIORef total (a1, b1, c1, d1)
        visit dir = do
            let probe = dir </> "probe"
            createFile probe
            modifyIORef' batchRef (probe :)
            n <- length <$> readIORef batchRef
            when (n >= probeBatch) flush
            xs <- listDirectory dir
            forM_ xs $ \x -> do
                let path = dir </> x
                isDir <- doesDirectoryExist path
                when isDir $ visit path
    visit (root w)
    flush
    lost1 <- lostCount w
    extras <- Map.keys <$> readIORef (probes w)
    readIORef missingRef >>= printProbes (root w) "missing"
    printProbes (root w) "extra" extras
    (_, dirs, missing, repeated) <- readIORef total
    return ProbeResult
        { probeDirs = dirs
        , probeMissing = missing
        , probeExtra = length extras
        , probeRepeated = repeated
        , probeLost = lost1 - lost0
        }

-- | Every directory reported its probe and no other probe was reported. On
-- macOS a later event for a path can repeat an earlier 'Event.isCreated'
-- flag, a probe reported more than once is allowed there.
probeOk :: ProbeResult -> Bool
probeOk ProbeResult{..} =
       probeMissing == 0
    && probeExtra == 0
    && probeLost == 0
    && (probeRepeated == 0 || platformName == "macOS")

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

data ModelResult = ModelResult
    { modelEntries :: Int
    -- | Entries on the disk that are not in the model.
    , modelMissing :: Int
    -- | Entries in the model that are not on the disk.
    , modelStale :: Int
    } deriving Show

-- | Sync, then compare the model of the tree with the tree on the disk.
-- 'Nothing' if the watch does not track a model. The marker files are not
-- compared, a marker created after the one that a 'sync' read may not have
-- been read yet. Up to 5 of the paths that differ are printed.
checkModel :: Watch -> String -> IO (Maybe ModelResult)
checkModel w name =
    case model w of
        Nothing -> return Nothing
        Just ref -> do
            sync w (name ++ "-model")
            m <- Set.filter keep <$> readIORef ref
            disk <- Set.filter keep . Set.fromList <$> listTree (root w)
            let missing = Set.toList (Set.difference disk m)
                stale = Set.toList (Set.difference m disk)
            printPaths "missing from the model" missing
            printPaths "stale in the model" stale
            return $ Just ModelResult
                { modelEntries = Set.size disk
                , modelMissing = length missing
                , modelStale = length stale
                }

    where

    keep path =
        let file = takeFileName path
         in not ("sync-" `isPrefixOf` file || "pause-" `isPrefixOf` file)

    printPaths label paths =
        unless (null paths) $
            putStrLn $ "  " ++ label ++ ": "
                ++ show (take 5 (sortOn length (map rel paths)))

    rel = makeRelative (root w)
