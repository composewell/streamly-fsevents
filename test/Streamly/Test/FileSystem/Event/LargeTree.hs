-- |
-- Module      : Streamly.Test.FileSystem.Event.LargeTree
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- A recursive watch on the largest tree that the free space allows, up to
-- 'treeDirsCap' directories, and on Linux, the inotify watch limit and the
-- queue limit, see 'allowedTreeDirs'. Half of the tree is moved out of the
-- watched tree and back in, and renamed within it, several times. The test
-- checks that every directory is watched at the end, and prints the memory
-- used by the watch.
--
-- On Linux, the test also checks that the scan of the directory moved in
-- reports every directory under it, and prints the peak RSS. The live heap
-- is printed only if the RTS option @-T@ is given.


module Streamly.Test.FileSystem.Event.LargeTree (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, unless, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import GHC.Clock (getMonotonicTime)
import GHC.Stats
    (RTSStats(..), GCDetails(..), getRTSStats, getRTSStatsEnabled)
import System.Directory (createDirectory, listDirectory, renameDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import System.IO.Temp (withSystemTempDirectory)
import System.Mem (performMajorGC)
import Streamly.Test.FileSystem.Event.Tree

-------------------------------------------------------------------------------
-- Sizes and limits
-------------------------------------------------------------------------------

-- | The largest tree used, to bound the run time where the limits are high.
treeDirsCap :: Int
treeDirsCap = 100000

-- | The largest number of directories, at most 'treeDirsCap', that the
-- limits allow in a tree created under the given directory. The tree uses
-- at most half of the free inodes and space of the file system of that
-- directory, and on Linux, the free inotify watches leaving 2000 free. Each
-- directory has a probe file, so a directory needs two inodes.
--
-- On Linux, moving half of the tree out removes the watch of each of its
-- directories, and the kernel queues an IN_IGNORED event for each removed
-- watch. Half of the tree is kept 2048 directories below max_queued_events,
-- so that the move out does not overflow the queue.
allowedTreeDirs :: FilePath -> IO Int
allowedTreeDirs dir = do
    queued <- queuedEventsLimit
    let byQueue = if isLinux then 2 * (queued - 2048) else treeDirsCap
    dirBudget
        Budget
            { maxDirs = min treeDirsCap byQueue
            , inodesPerDir = 2
            , fixedInodes = 0
            , bytesPerDir = 4096
            , diskFraction = 2
            , watchFraction = 1
            }
        dir

-------------------------------------------------------------------------------
-- Tree
-------------------------------------------------------------------------------

-- | List every directory of the tree under @dir@, which has no files, and
-- print the time taken. The caches are warm after this.
listTree :: FilePath -> IO ()
listTree dir = do
    t0 <- getMonotonicTime
    n <- go dir
    t1 <- getMonotonicTime
    putStrLn $ "listing " ++ show n ++ " directories: " ++ show (t1 - t0)
        ++ " s"

    where

    go :: FilePath -> IO Int
    go d = do
        xs <- listDirectory d
        (+ 1) . sum <$> mapM (go . (d </>)) xs

-------------------------------------------------------------------------------
-- Memory
-------------------------------------------------------------------------------

-- | The peak resident set size of the process in KB since the last
-- 'resetPeak'. 'Nothing' if the platform does not report it.
peakRss :: IO (Maybe Int)
#if defined(CABAL_OS_LINUX)
peakRss = do
    s <- readFile "/proc/self/status"
    case [read v | l <- lines s, ("VmHWM:" : v : _) <- [words l]] of
        [kb] -> return (Just kb)
        _ -> error "peakRss: VmHWM not found"
#else
peakRss = return Nothing
#endif

resetPeak :: IO ()
#if defined(CABAL_OS_LINUX)
resetPeak = writeFile "/proc/self/clear_refs" "5"
#else
resetPeak = return ()
#endif

-- | Bytes live in the heap after a major GC.
liveBytes :: IO Int
liveBytes = do
    performMajorGC
    fromIntegral . gcdetails_live_bytes . gc <$> getRTSStats

maxLiveBytes :: IO Int
maxLiveBytes = fromIntegral . max_live_bytes <$> getRTSStats

mb :: Int -> String
mb b = show (fromIntegral b / (1024 * 1024) :: Double) ++ " MB"

-- | Print the peak RSS during a phase, and the live heap at its end.
report :: String -> IO ()
report phase = do
    enabled <- getRTSStatsEnabled
    heap <-
        if enabled
        then do
            live <- liveBytes
            maxLive <- maxLiveBytes
            return $ ", live " ++ mb live ++ ", max live so far " ++ mb maxLive
        else return ""
    rss <- peakRss
    putStrLn $ phase ++ ":"
        ++ maybe "" (\kb -> " peak RSS " ++ mb (kb * 1024)) rss ++ heap
    hFlush stdout
    resetPeak

-------------------------------------------------------------------------------
-- Test
-------------------------------------------------------------------------------

-- | Check a condition, print the result and record a failure.
check :: IORef Bool -> String -> Bool -> IO ()
check failed msg ok = do
    putStrLn $ (if ok then "PASS: " else "FAIL: ") ++ msg
    unless ok $ writeIORef failed True

-- | Run an action and print the time it took.
timed :: String -> IO a -> IO a
timed label action = do
    t0 <- getMonotonicTime
    r <- action
    t1 <- getMonotonicTime
    putStrLn $ label ++ " time: " ++ show (t1 - t0) ++ " s"
    return r

-- | Move half of the tree out of the watched tree and back in. On Linux,
-- check that the entries moved in are reported by the scan. macOS and
-- Windows do not report the entries of a directory moved in.
moveOutIn :: IORef Bool -> Watch -> FilePath -> Int -> Int -> IO ()
moveOutIn failed w out half i = do
    let root_ = root w
    renameDirectory (root_ </> "t2") (out </> "t2")
    sync w ("out-" ++ show i)
    report ("move out " ++ show i)
    writeIORef (scanDirs w) 0
    -- The scan of the moved directory is read before the sync event.
    timed ("move in " ++ show i) $ do
        renameDirectory (out </> "t2") (root_ </> "t2")
        sync w ("in-" ++ show i)
    n <- readIORef (scanDirs w)
    when isLinux
        $ check failed
            ("move in " ++ show i ++ ": " ++ show n ++ " of "
                ++ show (half - 1) ++ " directories reported by the scan")
            (n == half - 1)
    report ("move in " ++ show i)

-- | Rename half of the tree within the watched tree and back.
renameWithin :: Watch -> Int -> IO ()
renameWithin w i = do
    let root_ = root w
    timed ("rename " ++ show i) $ do
        renameDirectory (root_ </> "t2") (root_ </> "t2r")
        sync w ("rename-" ++ show i)
    timed ("rename back " ++ show i) $ do
        renameDirectory (root_ </> "t2r") (root_ </> "t2")
        sync w ("unrename-" ++ show i)
    report ("rename within " ++ show i)

runTest :: FilePath -> Int -> IO Bool
runTest base total = do
    let root_ = base </> "root"
        out = base </> "out"
        half = total `div` 2
        t1 = treePaths "t1" (total - half)
        t2 = treePaths "t2" half
    createDirectory root_
    createDirectory out
    forM_ (t1 ++ t2) $ \d -> createDirectory (root_ </> d)
    putStrLn $ "Tree: " ++ show total ++ " directories, " ++ show half
        ++ " of them moved"
    listTree root_
    listTree root_
    report "before the watch"

    failed <- newIORef False
    -- The watch is started when the create event of a sync file is read.
    -- The stream is read only after the tree has been added.
    w <- timed "watch start" (startWatch id False root_)
    report "watch started"

    forM_ [1 .. 3 :: Int] $ \i -> do
        moveOutIn failed w out half i
        renameWithin w i

    r <- probeTree w "probes"
    check failed ("probes: " ++ show r) (probeOk r)
    n <- lostCount w
    check failed ("isEventsLost events: " ++ show n) (n == 0)
    report "probes"
    stopWatch w
    readIORef failed

main :: IO ()
main =
    withSystemTempDirectory "fsevent_large" $ \base -> do
        allowed <- allowedTreeDirs base
        if allowed < 2000
        then putStrLn $ "SKIP: the limits allow only " ++ show allowed
                ++ " dirs, the test needs at least 2000"
        else do
            failed <- runTest base allowed
            -- Wait for the watch thread to close the fd before the removal.
            threadDelay 100000
            when failed exitFailure
