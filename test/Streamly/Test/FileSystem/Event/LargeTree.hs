-- |
-- Module      : Streamly.Test.FileSystem.Event.LargeTree
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- A recursive watch on a tree of 'defaultDirs' directories. @--dirs N,M@ or
-- the environment variable @LARGETREE_DIRS@ set to @N,M@ runs the test on a
-- tree of each size in turn. The default tree is the same on all platforms,
-- so that the times measured on different platforms are for the same work.
-- A size is skipped if the free space, or on Linux the inotify limits, do
-- not allow it, see 'allowedTreeDirs'.
--
-- Half of the tree is moved out of the watched tree and back in, and renamed
-- within it, three times. The half moved includes a chain of directories
-- with paths longer than 260 characters. An empty directory is then renamed
-- 2000 times. The test checks that each move out and in is reported, that
-- every directory is watched at the end and that no events are lost. On
-- Linux, it also checks that the scan of the directory moved in reports
-- every directory under it, and prints the peak RSS.
--
-- After each size the test prints a summary of the times and of the live
-- heap in markdown tables. The live heap is measured only if the RTS option
-- @-T@ is given. @--label TEXT@ sets the first column of the tables, the
-- platform name by default.

module Streamly.Test.FileSystem.Event.LargeTree (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM, forM_, unless, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (intercalate)
import Data.Version (showVersion)
import GHC.Clock (getMonotonicTime)
import GHC.Conc (getNumProcessors, numCapabilities)
import GHC.RTS.Flags (GCFlags(..), getGCFlags)
import GHC.Stats
    (RTSStats(..), GCDetails(..), getRTSStats, getRTSStatsEnabled)
import System.Directory (createDirectory, listDirectory, renameDirectory)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.Info (fullCompilerVersion)
import System.IO (hFlush, stdout)
import System.IO.Temp (withSystemTempDirectory)
import System.Mem (performMajorGC)
import Text.Printf (printf)
import Streamly.Test.FileSystem.Event.Tree

import qualified Data.Set as Set

#if !defined(CABAL_OS_WINDOWS)
import System.Process (readProcess)
#endif

-------------------------------------------------------------------------------
-- Options
-------------------------------------------------------------------------------

-- | The number of directories of the tree, if neither @--dirs@ nor
-- @LARGETREE_DIRS@ is given. Half of it, 10000, is below the 14336 that the
-- default @max_queued_events@ of 16384 allows on Linux, see
-- 'allowedTreeDirs'.
defaultDirs :: Int
defaultDirs = 20000

data Options = Options
    { optDirs :: [Int]
    , optLabel :: String
    }

-- | Parse a comma separated list of numbers of directories, each at least
-- 20.
parseDirs :: String -> Maybe [Int]
parseDirs = mapM one . splitComma

    where

    splitComma str =
        case break (== ',') str of
            (x, []) -> [x]
            (x, _ : rest) -> x : splitComma rest

    one n
        | [(v, "")] <- reads n, v >= 20 = Just v
        | otherwise = Nothing

-- | Parse the arguments, with the default numbers of directories given.
parseOptions :: [Int] -> [String] -> Either String Options
parseOptions dirs = go (Options dirs platformName)

    where

    go o [] = Right o
    go o ("--dirs" : n : xs)
        | Just v <- parseDirs n = go o { optDirs = v } xs
    go o ("--label" : l : xs) = go o { optLabel = l } xs
    go _ xs = Left $ "unknown arguments: " ++ unwords xs
        ++ "\nusage: [--dirs N[,M...]] [--label TEXT]"

-------------------------------------------------------------------------------
-- Sizes and limits
-------------------------------------------------------------------------------

-- | The number of renames of an empty directory.
renameCount :: Int
renameCount = 2000

-- | The renames are synced in batches of this size. A rename generates two
-- events, 1000 events of a batch fit in the 63 KB event buffer of a watch
-- root on Windows.
renameBatch :: Int
renameBatch = 500

-- | The largest number of directories, at most @n@, that the limits allow in
-- a tree created under the given directory. The tree uses at most half of
-- the free inodes and space of the file system of that directory, and on
-- Linux, the free inotify watches leaving 2000 free. Each directory has a
-- probe file, so a directory needs two inodes.
--
-- On Linux, moving half of the tree out removes the watch of each of its
-- directories, and the kernel queues an IN_IGNORED event for each removed
-- watch. Half of the tree is kept 2048 directories below max_queued_events,
-- so that the move out does not overflow the queue.
allowedTreeDirs :: Int -> FilePath -> IO Int
allowedTreeDirs n dir = do
    queued <- queuedEventsLimit
    let byQueue = if isLinux then 2 * (queued - 2048) else n
    dirBudget
        Budget
            { maxDirs = min n byQueue
            , inodesPerDir = 2
            , fixedInodes = 0
            , bytesPerDir = 4096
            , diskFraction = 2
            , watchFraction = 1
            }
        dir

-------------------------------------------------------------------------------
-- Formatting
-------------------------------------------------------------------------------

-- | A time in seconds, in milliseconds below 0.1 s.
fmtTime :: Double -> String
fmtTime t
    | t >= 0.1 = printf "%.2f s" t
    | otherwise = printf "%.1f ms" (t * 1000)

-- | The mean of the times and, if there is more than one, their range.
fmtTimes :: [Double] -> String
fmtTimes [t] = fmtTime t
fmtTimes ts =
    fmtTime (sum ts / fromIntegral (length ts))
        ++ " (" ++ fmtTime (minimum ts) ++ " to " ++ fmtTime (maximum ts)
        ++ ")"

mb :: Int -> String
mb b = printf "%.2f MB" (fromIntegral b / (1024 * 1024) :: Double)

-- | A markdown table row.
row :: [String] -> String
row xs = "| " ++ intercalate " | " xs ++ " |"

-------------------------------------------------------------------------------
-- Tree
-------------------------------------------------------------------------------

-- | A chain of 5 directories with names of 60 characters under @t2@. The
-- paths in the chain are longer than the 260 characters of @MAX_PATH@ on
-- Windows.
longChain :: [FilePath]
longChain = drop 1 (scanl (</>) "t2" (replicate 5 (replicate 60 'l')))

-- | List every directory of the tree under @dir@, which has no files, and
-- print the time taken. Returns the time.
listTree :: FilePath -> IO Double
listTree dir = do
    t0 <- getMonotonicTime
    n <- go dir
    t1 <- getMonotonicTime
    putStrLn $ "listing " ++ show n ++ " directories: " ++ show (t1 - t0)
        ++ " s"
    return (t1 - t0)

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

-- | Print the peak RSS during a phase, and the live heap at its end. Returns
-- the live heap, 'Nothing' without @+RTS -T@.
report :: String -> IO (Maybe Int)
report phase = do
    enabled <- getRTSStatsEnabled
    heap <-
        if enabled
        then do
            live <- liveBytes
            maxLive <- maxLiveBytes
            return (Just (live, maxLive))
        else return Nothing
    rss <- peakRss
    putStrLn $ phase ++ ":"
        ++ maybe "" (\kb -> " peak RSS " ++ mb (kb * 1024)) rss
        ++ maybe ""
            (\(l, m) -> ", live " ++ mb l ++ ", max live so far " ++ mb m)
            heap
    hFlush stdout
    resetPeak
    return (fst <$> heap)

-------------------------------------------------------------------------------
-- Test
-------------------------------------------------------------------------------

-- | Check a condition, print the result and record a failure.
check :: IORef Bool -> String -> Bool -> IO ()
check failed msg ok = do
    putStrLn $ (if ok then "PASS: " else "FAIL: ") ++ msg
    unless ok $ writeIORef failed True

-- | Run an action and print the time it took. Returns the time.
timed :: String -> IO () -> IO Double
timed label action = do
    t0 <- getMonotonicTime
    action
    t1 <- getMonotonicTime
    putStrLn $ label ++ " time: " ++ show (t1 - t0) ++ " s"
    return (t1 - t0)

-- | Check that an event was reported for the entry @t2@ of the root since
-- the last check.
checkMoved :: IORef Bool -> Watch -> String -> IO ()
checkMoved failed w label = do
    entries <- readIORef (rootEntries w)
    writeIORef (rootEntries w) Set.empty
    check failed (label ++ ": t2 reported") (Set.member "t2" entries)

-- | Move half of the tree out of the watched tree and back in, and check
-- that each move is reported. On Linux, check that the entries moved in are
-- reported by the scan. macOS and Windows do not report the entries of a
-- directory moved in. Returns the time of the move in.
moveOutIn :: IORef Bool -> Watch -> FilePath -> Int -> Int -> IO Double
moveOutIn failed w out half i = do
    let root_ = root w
    writeIORef (rootEntries w) Set.empty
    renameDirectory (root_ </> "t2") (out </> "t2")
    sync w ("out-" ++ show i)
    checkMoved failed w ("move out " ++ show i)
    _ <- report ("move out " ++ show i)
    writeIORef (scanDirs w) 0
    -- The scan of the moved directory is read before the sync event.
    t <- timed ("move in " ++ show i) $ do
        renameDirectory (out </> "t2") (root_ </> "t2")
        sync w ("in-" ++ show i)
    checkMoved failed w ("move in " ++ show i)
    n <- readIORef (scanDirs w)
    when isLinux
        $ check failed
            ("move in " ++ show i ++ ": " ++ show n ++ " of "
                ++ show (half - 1) ++ " directories reported by the scan")
            (n == half - 1)
    _ <- report ("move in " ++ show i)
    return t

-- | Rename half of the tree within the watched tree and back. Returns the
-- times of the two renames.
renameWithin :: Watch -> Int -> IO [Double]
renameWithin w i = do
    let root_ = root w
    t1 <- timed ("rename " ++ show i) $ do
        renameDirectory (root_ </> "t2") (root_ </> "t2r")
        sync w ("rename-" ++ show i)
    t2 <- timed ("rename back " ++ show i) $ do
        renameDirectory (root_ </> "t2r") (root_ </> "t2")
        sync w ("unrename-" ++ show i)
    _ <- report ("rename within " ++ show i)
    return [t1, t2]

-- | Rename an empty directory in the root 'renameCount' times, in batches of
-- 'renameBatch' each followed by a sync. Returns the time of all the
-- batches.
renameEmpty :: Watch -> IO Double
renameEmpty w = do
    let name i = root w </> ("e" ++ show i)
    createDirectory (name (0 :: Int))
    sync w "empty"
    t <- timed (show renameCount ++ " renames of an empty directory") $
        forM_ [0, renameBatch .. renameCount - 1] $ \b -> do
            forM_ [b .. b + renameBatch - 1] $ \i ->
                renameDirectory (name i) (name (i + 1))
            sync w ("empty-" ++ show b)
    _ <- report "renames of an empty directory"
    return t

-------------------------------------------------------------------------------
-- Summary
-------------------------------------------------------------------------------

data Result = Result
    { resDirs :: Int
    , resListTime :: Double
    , resStartTime :: Double
    , resMoveTimes :: [Double]
    , resRenameTimes :: [Double]
    , resEmptyTime :: Double
    -- | Live heap before the watch, after its start and after each cycle of
    -- moves and renames.
    , resLive :: Maybe (Int, Int, [Int])
    -- | The maximum live heap of the process before the watch, and before
    -- the probes. The probes are not included, their map is data of the
    -- test.
    , resMaxLive :: Maybe (Int, Int)
    }

-- | The kernel name and version, "unknown" on Windows.
osVersion :: IO String
#if defined(CABAL_OS_WINDOWS)
osVersion = return "unknown"
#else
osVersion = concat . lines <$> readProcess "uname" ["-sr"] ""
#endif

-- | Print the setup of the run and the results in markdown tables.
printSummary :: FilePath -> String -> Result -> IO ()
printSummary base label Result{..} = do
    os <- osVersion
    cpus <- getNumProcessors
    flags <- getGCFlags
    let half = resDirs `div` 2
        -- The allocation area is in blocks of 4 KB.
        areaKB = minAllocAreaSize flags * 4
        area
            | areaKB `mod` 1024 == 0 = show (areaKB `div` 1024) ++ "m"
            | otherwise = show areaKB ++ "k"
        gcOpts = "-G" ++ show (generations flags) ++ " -A" ++ area
    putStrLn ""
    putStrLn $ "## Summary, " ++ show resDirs ++ " directories"
    putStrLn ""
    putStrLn $ "* OS: " ++ platformName ++ ", " ++ os
    putStrLn $ "* CPUs: " ++ show cpus ++ ", capabilities: "
        ++ show numCapabilities
    putStrLn $ "* GHC: " ++ showVersion fullCompilerVersion
        ++ ", GC options: " ++ gcOpts
    putStrLn $ "* Tree: " ++ show resDirs
        ++ " directories, 10 subdirectories in each directory and a chain"
        ++ " of " ++ show (length longChain) ++ " with paths longer than 260"
        ++ " characters, under " ++ base
    putStrLn ""
    putStrLn $ row
        [ "Platform"
        , "Listing the tree"
        , "Start of the watch"
        , "Move in of " ++ show half ++ " directories"
        , "Rename of " ++ show half ++ " directories"
        , show renameCount ++ " renames of an empty directory"
        ]
    putStrLn $ row (replicate 6 "---")
    putStrLn $ row
        [ label
        , fmtTime resListTime
        , fmtTime resStartTime
        , fmtTimes resMoveTimes
        , fmtTimes resRenameTimes
        , fmtTime resEmptyTime ++ ", "
            ++ show (round (fromIntegral renameCount / resEmptyTime) :: Int)
            ++ " renames/s"
        ]
    putStrLn ""
    putStrLn $ "Listing: the second listing, with warm caches. Move in: "
        ++ "the mean and range of " ++ show (length resMoveTimes)
        ++ " moves. Rename: the mean and range of "
        ++ show (length resRenameTimes) ++ " renames, there and back."
    putStrLn ""
    case (resLive, resMaxLive) of
        (Just (before, start, cycles), Just (maxBefore, maxEnd)) -> do
            let perDir = (start - before) `div` resDirs
                cyc = map (subtract before) cycles
                range a b = if a == b then a else a ++ " to " ++ b
            putStrLn $ row
                [ "Platform"
                , "Live heap after the start of the watch"
                , "Live heap after each cycle of moves and renames"
                , "Maximum live heap, before the watch"
                , "Maximum live heap, at the end of the renames"
                ]
            putStrLn $ row (replicate 5 "---")
            putStrLn $ row
                [ label
                , mb (start - before) ++ ", " ++ show perDir
                    ++ " bytes per directory"
                , range (mb (minimum cyc)) (mb (maximum cyc))
                , mb maxBefore
                , mb maxEnd
                ]
            putStrLn ""
            putStrLn $ "The live heap after the start and after the cycles "
                ++ "excludes the " ++ mb before ++ " live before the start "
                ++ "of the watch, the data of the test. The maximum live "
                ++ "heap is of the process, it is measured at major GCs."
        _ -> putStrLn "Live heap: run with +RTS -T to measure it."
    hFlush stdout

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

runTest :: FilePath -> Int -> IO (Bool, Result)
runTest base total = do
    let root_ = base </> "root"
        out = base </> "out"
        half = total `div` 2
        t1 = treePaths "t1" (total - half)
        t2 = treePaths "t2" (half - length longChain) ++ longChain
    createDirectory root_
    createDirectory out
    forM_ (t1 ++ t2) $ \d -> createDirectory (root_ </> d)
    putStrLn $ "Tree: " ++ show total ++ " directories, " ++ show half
        ++ " of them moved"
    _ <- listTree root_
    listTime <- listTree root_
    before <- report "before the watch"
    maxBefore <- traverse (const maxLiveBytes) before

    failed <- newIORef False
    -- The watch is started when the create event of a sync file is read.
    -- The stream is read only after the tree has been added.
    s0 <- getMonotonicTime
    w <- startWatch id TrackNothing root_
    s1 <- getMonotonicTime
    putStrLn $ "watch start time: " ++ show (s1 - s0) ++ " s"
    start <- report "watch started"

    cycles <- forM [1 .. 3 :: Int] $ \i -> do
        tm <- moveOutIn failed w out half i
        tr <- renameWithin w i
        live <- report ("cycle " ++ show i)
        return (tm, tr, live)
    emptyTime <- renameEmpty w
    enabled <- getRTSStatsEnabled
    maxEnd <- if enabled then Just <$> maxLiveBytes else return Nothing

    r <- probeTree w "probes"
    check failed ("probes: " ++ show r) (probeOk r)
    n <- lostCount w
    check failed ("isEventsLost events: " ++ show n) (n == 0)
    _ <- report "probes"
    stopWatch w
    ok <- readIORef failed
    let lives = (,,) <$> before <*> start
                    <*> traverse (\(_, _, l) -> l) cycles
    return
        ( ok
        , Result
            { resDirs = total
            , resListTime = listTime
            , resStartTime = s1 - s0
            , resMoveTimes = [tm | (tm, _, _) <- cycles]
            , resRenameTimes = concat [tr | (_, tr, _) <- cycles]
            , resEmptyTime = emptyTime
            , resLive = lives
            , resMaxLive = (,) <$> maxBefore <*> maxEnd
            }
        )

main :: IO ()
main = do
    env <- lookupEnv "LARGETREE_DIRS"
    envDirs <- case env of
        Nothing -> return [defaultDirs]
        Just "" -> return [defaultDirs]
        Just n ->
            maybe
                (putStrLn ("LARGETREE_DIRS: invalid numbers " ++ n)
                    >> exitFailure)
                return
                (parseDirs n)
    args <- getArgs
    opts <- either (\e -> putStrLn e >> exitFailure) return
                (parseOptions envDirs args)
    results <- forM (optDirs opts) $ \dirs ->
        withSystemTempDirectory "fsevent_large" $ \base -> do
            allowed <- allowedTreeDirs dirs base
            if allowed < dirs
            then do
                putStrLn $ "SKIP: the limits allow only " ++ show allowed
                    ++ " dirs, the test needs " ++ show dirs
                    ++ ", use --dirs to set a smaller tree"
                return False
            else do
                (failed, res) <- runTest base dirs
                -- Wait for the watch thread to close the fd before the
                -- removal.
                threadDelay 100000
                printSummary base (optLabel opts) res
                return failed
    when (or results) exitFailure
