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
-- files to synchronize with the consumer, and a check that every directory
-- of the tree is watched.

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
    )
where

import Control.Concurrent
    ( Chan, MVar, ThreadId, forkIO, killThread, newChan, newEmptyMVar
    , putMVar, readChan, takeMVar, threadDelay, tryReadMVar, writeChan
    )
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, when)
import Data.Function ((&))
import Data.IORef
    ( IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef
    , writeIORef
    )
import Data.List (isPrefixOf)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Map.Strict (Map)
import Data.Set (Set)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeFileName)
import System.Timeout (timeout)
import Streamly.FileSystem.Event (Config, Event)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.FileSystem.Event as Event
import qualified Streamly.FileSystem.Path as Path
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

    countFd pid fd = do
        r <- try (readFile ("/proc" </> pid </> "fdinfo" </> fd))
        case r of
            Left (_ :: SomeException) -> return 0
            Right s -> do
                let n = length (filter ("inotify wd:" `isPrefixOf`) (lines s))
                n `seq` return n

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
-- Watch
-------------------------------------------------------------------------------

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
    -- | The number of 'Event.isCreated' events of each file named @probe@.
    , probes :: IORef (Map FilePath Int)
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
    when (Event.isCreated ev) $ do
        when (isScanDir ev) $ modifyIORef' scanDirs (+ 1)
        when trackCreated $ modifyIORef' created (Set.insert path)
        when (name == "probe")
            $ modifyIORef' probes (Map.insertWith (+) path 1)
    when (marker "sync-" || marker "pause-") $ do
        new <- atomicModifyIORef' seen
            (\s -> (Set.insert name s, not (Set.member name s)))
        when new $
            if "sync-" `isPrefixOf` name
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

-- | Block the consumer, it does not read events until 'resume'.
pause :: Watch -> String -> IO ()
pause Watch{..} name = do
    createFile (root </> ("pause-" ++ name))
    r <- timeout 60000000 (takeMVar paused)
    when (r == Nothing) $ error ("pause: the consumer did not read " ++ name)

resume :: Watch -> IO ()
resume Watch{..} = putMVar resumed ()

-- | Start a watch of @dir@ and wait until the consumer reads events. If
-- @track@ is true, the paths of the 'Event.isCreated' events are recorded.
startWatch :: (Config -> Config) -> Bool -> FilePath -> IO Watch
startWatch cfg track dir = do
    lostRef <- newIORef 0
    scanRef <- newIORef 0
    createdRef <- newIORef Set.empty
    probesRef <- newIORef Map.empty
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
            , trackCreated = track
            , probes = probesRef
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
            Event.watchWith
                (Event.setRecursiveMode True . cfg) (rootPath :| [])
                & Stream.fold (Fold.drainMapM (observe w))
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

-- | Create a file named @probe@ in every directory of the tree under the
-- root, and count the probes that are not reported once. The probes are
-- created in batches that do not overflow the event queue, a batch is
-- checked after its sync, and its paths are not kept after that.
probeTree :: Watch -> String -> IO ProbeResult
probeTree w name = do
    lost0 <- lostCount w
    batchRef <- newIORef []
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
                let missing = Set.size keys - Map.size found
                    repeated = Map.size (Map.filter (/= 1) found)
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
    extra <- Map.size <$> readIORef (probes w)
    (_, dirs, missing, repeated) <- readIORef total
    return ProbeResult
        { probeDirs = dirs
        , probeMissing = missing
        , probeExtra = extra
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
