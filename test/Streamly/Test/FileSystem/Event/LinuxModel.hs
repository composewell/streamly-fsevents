-- |
-- Module      : Streamly.Test.FileSystem.Event.LinuxModel
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Exhaustive test of a recursive watch on Linux. Every valid sequence of file
-- system operations, up to a given length, over a small set of paths is run
-- against the library. Some operations run before the events are read, the
-- others after a given number of events is read, which covers the orderings
-- of the operations and the event reader. See "Exhaustive test" in
-- dev/design.linux.md for the checks.
--
-- Environment variables, for a deeper search:
--
-- * FSEVENT_MODEL_DEPTH: the maximum length of a sequence.
-- * FSEVENT_MODEL_SPLIT_DEPTH: the maximum length of a sequence that is also
--   run with its operations split around the reads of the events.
-- * FSEVENT_MODEL_MAX_FAILURES: the number of failures shown.
-- * FSEVENT_MODEL_OUT: append all the failures to this file.
--
module Streamly.Test.FileSystem.Event.LinuxModel (main) where

import Control.Concurrent
    ( forkIO, killThread, newEmptyMVar, putMVar, takeMVar, tryPutMVar )
import Control.Exception (SomeException, bracket, finally, try)
import Control.Monad (forM_, unless, void, when)
import Data.IORef
    (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (intercalate, isPrefixOf, sort, (\\))
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import System.Directory
    ( createDirectory, createDirectoryIfMissing, createFileLink
    , doesDirectoryExist, listDirectory, pathIsSymbolicLink
    , removeDirectory, removeFile, removePathForcibly, renamePath )
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.IO (IOMode(WriteMode), hClose, openFile)
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Hspec
import Text.Read (readMaybe)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.FileSystem.Path as Path
import qualified Streamly.Internal.FS.Event.Linux as Event

-------------------------------------------------------------------------------
-- Model of the file system
-------------------------------------------------------------------------------

-- | A symbolic link is a 'Link', a write to it is not generated.
data Kind = Dir | File | Link deriving (Eq, Show)

-- | The tree, the paths are relative to the base directory of a test case.
type Tree = Map FilePath Kind

data Op =
      Mkdir FilePath
    | Rmdir FilePath
    | Touch FilePath
    | Rm FilePath
    | Mv FilePath FilePath
    -- | @Ln target path@.
    | Ln FilePath FilePath
    -- | Append to a regular file in the roots.
    | Write FilePath

showOp :: Op -> String
showOp (Mkdir p) = "mkdir " ++ p
showOp (Rmdir p) = "rmdir " ++ p
showOp (Touch p) = "touch " ++ p
showOp (Rm p) = "rm " ++ p
showOp (Mv p q) = "mv " ++ p ++ " " ++ q
showOp (Ln t p) = "ln -s " ++ t ++ " " ++ p
showOp (Write p) = "echo >> " ++ p

-- | A path strictly under a directory.
isBelow :: FilePath -> FilePath -> Bool
isBelow dir path = (dir ++ "/") `isPrefixOf` path

-- | A set of watch roots, the initial tree and the paths used by the
-- operations. The roots are not used by the operations.
data World = World
    { wName :: String
    , wRoots :: [FilePath]
    , wTree :: Tree
    , wPaths :: [FilePath]
    -- | The target of the symbolic links created by the operations.
    , wLinkTarget :: FilePath
    , wConfig :: Event.Config
    -- | The consumer can apply the events to get the tree.
    , wCheckView :: Bool
    -- | The consumer lists a directory on its create and move to events, as a
    -- program does with 'Event.setScanNewDirs' 'False'.
    , wConsumerScans :: Bool
    -- | Check the create events of the probes, otherwise their open events.
    , wCreateEvents :: Bool
    }

hasChildren :: Tree -> FilePath -> Bool
hasChildren t dir = any (isBelow dir) (Map.keys t)

parentIsDir :: Tree -> FilePath -> Bool
parentIsDir t p = Map.lookup (takeDirectory p) t == Just Dir

-- | rename(2): the destination can be an empty directory if the source is a
-- directory, and a file if the source is a file.
canMove :: Tree -> FilePath -> FilePath -> Bool
canMove t p q =
    p /= q
        && not (isBelow p q)
        && parentIsDir t q
        && case (Map.lookup p t, Map.lookup q t) of
            (Nothing, _) -> False
            (Just _, Nothing) -> True
            (Just Dir, Just Dir) -> not (hasChildren t q)
            (Just k1, Just k2) -> k1 /= Dir && k2 /= Dir

-- | Which roots of the world reach a path.
rootsOf :: World -> FilePath -> [FilePath]
rootsOf w p = [r | r <- wRoots w, r == p || isBelow r p]

validOps :: World -> Tree -> [Op]
validOps w t =
       [Mkdir p | p <- ps, absent p, parentIsDir t p]
    ++ [Rmdir p | p <- ps, is Dir p, not (hasChildren t p)]
    ++ [Touch p | p <- ps, absent p, parentIsDir t p]
    ++ [Ln (wLinkTarget w) p | p <- ps, absent p, parentIsDir t p]
    ++ [Rm p | p <- ps, is File p || is Link p]
    ++ [Mv p q | p <- ps, q <- ps, canMove t p q]
    ++ [Write p | (p, File) <- Map.toList t, not (null (rootsOf w p))]

    where

    ps = wPaths w
    absent p = Map.notMember p t
    is k p = Map.lookup p t == Just k

applyOp :: Tree -> Op -> Tree
applyOp t (Mkdir p) = Map.insert p Dir t
applyOp t (Rmdir p) = Map.delete p t
applyOp t (Touch p) = Map.insert p File t
applyOp t (Ln _ p) = Map.insert p Link t
applyOp t (Write _) = t
applyOp t (Rm p) = Map.delete p t
applyOp t (Mv p q) =
    Map.fromList (map move (Map.toList (Map.delete q t)))

    where

    move (path, k)
        | path == p = (q, k)
        | isBelow p path = (q ++ drop (length p) path, k)
        | otherwise = (path, k)

createFile :: FilePath -> IO ()
createFile file = openFile file WriteMode >>= hClose

execOp :: FilePath -> Op -> IO ()
execOp base (Mkdir p) = createDirectory (base </> p)
execOp base (Rmdir p) = removeDirectory (base </> p)
execOp base (Touch p) = createFile (base </> p)
execOp base (Ln t p) = createFileLink (base </> t) (base </> p)
execOp base (Rm p) = removeFile (base </> p)
execOp base (Mv p q) = renamePath (base </> p) (base </> q)
execOp base (Write p) = appendFile (base </> p) "x"

-------------------------------------------------------------------------------
-- The view of a consumer that applies the events in order
-------------------------------------------------------------------------------

-- | The paths known to the consumer.
type View = Set FilePath

getAbsPath :: Event.Event -> FilePath
getAbsPath = Path.toString . Event.getAbsPath

-- | The sentinels and the probes are hidden files, they are not a part of the
-- tree.
isControl :: FilePath -> Bool
isControl = isPrefixOf "." . takeFileName

-- | A rename is applied as the tree leaving and coming back in, no cookie is
-- matched: 'Event.isMovedFrom' removes the directory and the paths under it,
-- the entries at the new path are reported by scan events after
-- 'Event.isMovedTo'. rmdir(2) needs an empty directory, a deleted directory
-- removes the paths under it too, a scan event reported before the deletion
-- of its directory is removed with it.
applyEvent :: View -> Event.Event -> View
applyEvent v ev
    | isControl path = v
    | Event.isMovedFrom ev || Event.isDeleted ev =
        Set.filter (\q -> q /= path && not (isBelow path q)) v
    | Event.isMovedTo ev || Event.isCreated ev = Set.insert path v
    | otherwise = v

    where

    path = getAbsPath ev

-------------------------------------------------------------------------------
-- Running a test case
-------------------------------------------------------------------------------

-- | An event read by the consumer, or the paths found when the consumer
-- listed a directory after reading its event.
data Logged = Ev Event.Event | Listed [FilePath]

applyLogged :: View -> Logged -> View
applyLogged v (Ev ev) = applyEvent v ev
applyLogged v (Listed ps) =
    foldr Set.insert v (filter (not . isControl) ps)

-- | The paths of all the entries under a directory, symbolic links are not
-- followed. A path that cannot be read has no entries.
listTree :: FilePath -> IO [FilePath]
listTree dir = do
    r <- try (listDirectory dir)
    case r of
        Left (_ :: SomeException) -> return []
        Right names -> concat <$> mapM entry names

    where

    entry name = do
        let path = dir </> name
        r <- try $ do
            isLink <- pathIsSymbolicLink path
            if isLink then return False else doesDirectoryExist path
        sub <-
            case r of
                Right True -> listTree path
                Right False -> return []
                Left (_ :: SomeException) -> return []
        return (path : sub)

data Outcome = Outcome
    { oFailures :: [String]
    -- | The number of events asked for was read before the operations ended.
    , oSplitReached :: Bool
    }

setupTree :: FilePath -> Tree -> IO ()
setupTree base t =
    forM_ (Map.toAscList t) $ \(p, k) ->
        case k of
            Dir -> createDirectoryIfMissing True (base </> p)
            File -> createFile (base </> p)
            Link -> error "setupTree: a symbolic link in the initial tree"

-- | The state of 'expectedWrites'. A directory of the initial tree in the
-- roots is identified by its number.
data WriteState = WriteState
    { wsTree :: Tree
    -- | The current paths of the directories that have not left the roots.
    , wsDirs :: Map FilePath Int
    -- | The path that a directory arrived at by its last move, and the number
    -- of the operation.
    , wsArrived :: Map Int (FilePath, Int)
    -- | A path that a directory arrived at by a move and then left, with the
    -- directory and the number of the operation that moved it there.
    , wsVacated :: Map FilePath (Int, Int)
    }

-- | The writes that must be reported by 'Event.isModified' events, as the
-- path of the file at the time of the write, once for each root that reaches
-- it. A write is expected if its directory was in the roots at the start and
-- has not left them since. A directory created or moved in later is watched
-- only after its event is read, its events before that are not reported.
--
-- A directory that arrives at a path by a move, leaves it, and whose path is
-- then taken by another directory, as in @mv f a; mv a b; mkdir a@, loses its
-- watch when IN_MOVED_TO of @a@ is read, see "Keeping the watch over two
-- renames" in dev/design.linux.md. The writes in its tree after it arrived
-- at the path are not expected.
expectedWrites :: World -> [Op] -> [FilePath]
expectedWrites w ops0 =
    [ p
    | (p, ids, t) <- writes
    , not (any (\i -> any (< t) (Map.findWithDefault [] i lost)) ids)
    ]

    where

    inRoots = not . null . rootsOf w
    initialDirs =
        [p | (p, Dir) <- Map.toList (wTree w), inRoots p]
    st0 =
        WriteState
            { wsTree = wTree w
            , wsDirs = Map.fromList (zip initialDirs [0 ..])
            , wsArrived = Map.empty
            , wsVacated = Map.empty
            }
    (writes, lost) = go 0 st0 ops0

    move p q d
        | d == p = q
        | isBelow p d = q ++ drop (length p) d
        | otherwise = d

    -- The path q is taken by a directory other than the one numbered @self@.
    reuse self q st =
        case Map.lookup q (wsVacated st) of
            Just (i, ta) | Just i /= self ->
                let dirs = wsDirs st
                    gone =
                        [ d
                        | (d0, j) <- Map.toList dirs
                        , j == i
                        , d <- Map.keys dirs
                        , d == d0 || isBelow d0 d
                        ]
                 in ( Just (i, ta)
                    , st { wsDirs = foldr Map.delete dirs gone
                         , wsVacated = Map.delete q (wsVacated st)
                         }
                    )
            Just _ ->
                (Nothing, st {wsVacated = Map.delete q (wsVacated st)})
            Nothing -> (Nothing, st)

    go ::
           Int
        -> WriteState
        -> [Op]
        -> ([(FilePath, [Int], Int)], Map Int [Int])
    go _ _ [] = ([], Map.empty)
    go t st (op : ops) =
        let (st1, ws, l) = step t st op
            (ws1, l1) = go (t + 1) st1 {wsTree = applyOp (wsTree st) op} ops
            addLost (i, ta) = Map.insertWith (++) i [ta]
         in (ws ++ ws1, maybe l1 (`addLost` l1) l)

    step t st op =
        case op of
            Write p
                | Map.member dir (wsDirs st) ->
                    let ids =
                            [ i
                            | (d, i) <- Map.toList (wsDirs st)
                            , d == dir || isBelow d dir
                            ]
                        n = length (rootsOf w p)
                     in (st, replicate n (p, ids, t), Nothing)
                where dir = takeDirectory p
            Mkdir q ->
                let (l, st1) = reuse Nothing q st in (st1, [], l)
            Rmdir p ->
                (st {wsDirs = Map.delete p (wsDirs st)}, [], Nothing)
            Mv p q
                | Map.lookup p (wsTree st) == Just Dir ->
                    let top = Map.lookup p (wsDirs st)
                        (l, st1) = reuse top q st
                        arrivedAt i = Map.lookup i (wsArrived st1)
                        vacated =
                            case top of
                                Just i
                                    | Just (a, ta) <- arrivedAt i
                                    , a == p ->
                                        Map.insert p (i, ta) (wsVacated st1)
                                _ -> wsVacated st1
                        dirs =
                            Map.filterWithKey (\d _ -> inRoots d)
                                $ Map.mapKeys (move p q)
                                $ Map.delete q (wsDirs st1)
                        arrived =
                            case top of
                                Just i | inRoots q -> Map.insert i (q, t)
                                _ -> id
                     in ( st1 { wsDirs = dirs
                              , wsArrived = arrived (wsArrived st1)
                              , wsVacated = vacated
                              }
                        , []
                        , l
                        )
            _ -> (st, [], Nothing)

failuresOf ::
       World -> FilePath -> Tree -> [Op] -> [Logged] -> [String]
failuresOf w base final ops logged =
       [ "probe missing: " ++ rel p | p <- expected \\ reported ]
    ++ [ "probe extra: " ++ rel p | p <- reported \\ expected ]
    ++ [ "probe no open: " ++ rel p | p <- expected \\ opened ]
    ++ [ "entry not reported: " ++ rel p
       | wCheckView w, p <- Set.toList lost ]
    ++ [ "write not reported: " ++ rel p | p <- writes \\ modified ]

    where

    rel = drop (length base + 1)
    evs = [ev | Ev ev <- logged]
    dirs = [p | (p, Dir) <- Map.toList final]
    expected =
        sort
            [ base </> d </> ".probe"
            | d <- dirs
            , _ <- rootsOf w d
            ]
    probes p = sort
        [ getAbsPath ev
        | ev <- evs
        , p ev
        , takeFileName (getAbsPath ev) == ".probe"
        ]
    reported = if wCreateEvents w then probes Event.isCreated else opened
    writes = sort [base </> p | p <- expectedWrites w ops]
    modified = sort [getAbsPath ev | ev <- evs, Event.isModified ev]
    opened = probes Event.isOpened
    inRoots t =
        Set.fromList [base </> p | p <- Map.keys t, not (null (rootsOf w p))]
    view0 = inRoots (wTree w)
    view = foldl applyLogged view0 logged
    lost = inRoots final `Set.difference` view

-- | Run @ops1@ with no event read, then read the events and run @ops2@ when
-- the event number @split@ is read, or after all the events of @ops1@.
runCase :: FilePath -> World -> [Op] -> Maybe Int -> [Op] -> IO Outcome
runCase base w ops1 split ops2 = do
    setupTree base (wTree w)
    roots <- mapM (Path.fromString . (base </>)) (wRoots w)
    bracket
        (Event.openWatch (wConfig w) (NonEmpty.fromList roots))
        Event.closeWatch
        go

    where

    cfg = wConfig w
    root = base </> head (wRoots w)
    sentinel n = root </> (".sentinel" ++ show (n :: Int))
    isSentinel n ev =
        (if wCreateEvents w then Event.isCreated ev else Event.isOpened ev)
            && getAbsPath ev == sentinel n
    final = foldl applyOp (foldl applyOp (wTree w) ops1) ops2
    dirs = [p | (p, Dir) <- Map.toList final]
    limit = 5000000
    consumerScans ev =
        wConsumerScans w
            && Event.isDir ev
            && (Event.isCreated ev || Event.isMovedTo ev)
            && not (Event.isScanEvent ev)
            && not (isControl (getAbsPath ev))

    go wtch = do
        evRef <- newIORef []
        countRef <- newIORef (0 :: Int)
        ops2Ref <- newIORef False
        splitRef <- newIORef False
        settled <- newEmptyMVar
        finished <- newEmptyMVar
        failed <- newEmptyMVar
        let runOps2 = do
                done <- readIORef ops2Ref
                unless done $ do
                    writeIORef ops2Ref True
                    mapM_ (execOp base) ops2
                    createFile (sentinel 2)
            onEvent ev = do
                modifyIORef' evRef (Ev ev :)
                when (consumerScans ev) $ do
                    found <- listTree (getAbsPath ev)
                    modifyIORef' evRef (Listed found :)
                modifyIORef' countRef (+ 1)
                n <- readIORef countRef
                when (Just n == split) $ do
                    done <- readIORef ops2Ref
                    unless done (writeIORef splitRef True)
                    runOps2
                when (isSentinel 1 ev) runOps2
                when (isSentinel 2 ev) (void (tryPutMVar settled ()))
                when (isSentinel 3 ev) (void (tryPutMVar finished ()))
            reader = do
                r <- try
                    $ Stream.fold (Fold.drainMapM onEvent)
                    $ Event.watchToStream cfg wtch
                case r of
                    Left (e :: SomeException) -> do
                        _ <- tryPutMVar failed ("exception: " ++ show e)
                        _ <- tryPutMVar settled ()
                        void (tryPutMVar finished ())
                    Right () -> return ()
        mapM_ (execOp base) ops1
        createFile (sentinel 1)
        tid <- forkIO reader
        (do
            r1 <- timeout limit (takeMVar settled)
            r2 <- case r1 of
                Nothing -> return Nothing
                Just () -> do
                    mapM_ (\d -> createFile (base </> d </> ".probe")) dirs
                    createFile (sentinel 3)
                    timeout limit (takeMVar finished)
            logged <- reverse <$> readIORef evRef
            reached <- readIORef splitRef
            err <- timeout 1 (takeMVar failed)
            let failures =
                    case (r1, r2) of
                        (Nothing, _) -> ["timeout before the sentinel"]
                        (_, Nothing) -> ["timeout before the probes"]
                        _ -> failuresOf w base final (ops1 ++ ops2) logged
            return
                Outcome
                    { oFailures = maybe failures (: failures) err
                    , oSplitReached = reached
                    }
            ) `finally` killThread tid

-------------------------------------------------------------------------------
-- Search
-------------------------------------------------------------------------------

data Failure = Failure String [String]

data Stats = Stats
    { sCases :: IORef Int
    , sFailures :: IORef [Failure]
    }

-- | Run one case in a new directory.
runOne ::
    FilePath -> Stats -> World -> [Op] -> Maybe Int -> [Op] -> IO Outcome
runOne tmp st w ops1 split ops2 = do
    n <- readIORef (sCases st)
    writeIORef (sCases st) (n + 1)
    let base = tmp </> "case"
    createDirectory base
    r <- runCase base w ops1 split ops2 `finally` removePathForcibly base
    unless (null (oFailures r))
        $ modifyIORef' (sFailures st)
            (Failure (describeCase ops1 split ops2) (oFailures r) :)
    return r

describeCase :: [Op] -> Maybe Int -> [Op] -> String
describeCase ops1 Nothing ops2 =
    intercalate "; " (map showOp (ops1 ++ ops2))
describeCase ops1 (Just k) ops2 =
    intercalate "; " (map showOp ops1)
        ++ " | after event " ++ show k ++ " | "
        ++ intercalate "; " (map showOp ops2)

-- | Run the sequence with the second part of it run after @k@ events, for all
-- the values of @k@ that are reached.
runSplits :: FilePath -> Stats -> World -> [Op] -> [Op] -> IO ()
runSplits tmp st w ops1 ops2 = go 1

    where

    go k = do
        r <- runOne tmp st w ops1 (Just k) ops2
        when (oSplitReached r) (go (k + 1))

-- | Depth first search. A sequence that fails is still extended, a longer
-- sequence can fail in a different way.
search :: FilePath -> Stats -> World -> Int -> Int -> IO ()
search tmp st w depth splitDepth = go [] (wTree w)

    where

    go done t =
        forM_ (validOps w t) $ \op -> do
            let ops = done ++ [op]
                len = length ops
            _ <- runOne tmp st w ops Nothing []
            when (len <= splitDepth)
                $ forM_ [1 .. len - 1]
                $ \i ->
                    let (ops1, ops2) = splitAt i ops
                     in runSplits tmp st w ops1 ops2
            when (len < depth) (go ops (applyOp t op))

-------------------------------------------------------------------------------
-- Worlds
-------------------------------------------------------------------------------

recCfg :: Event.Config
recCfg =
    Event.setRecursiveMode True (Event.setAllEvents True Event.defaultConfig)

-- | @r@ is the root, @r/f@ is a watched directory with a subdirectory, @o@ is
-- outside the tree, @o/m@ is a directory that can be moved in.
oneRoot :: World
oneRoot = World
    { wName = "one root"
    , wRoots = ["r"]
    , wTree = Map.fromList
        [ ("r", Dir), ("r/f", Dir), ("r/f/sub", Dir), ("r/f/file", File)
        , ("o", Dir), ("o/m", Dir), ("o/m/file", File)
        ]
    , wPaths = ["r/a", "r/b", "r/f", "r/f/sub", "r/a/c", "o/m", "o/x"]
    , wLinkTarget = "r/f"
    , wConfig = recCfg
    , wCheckView = True
    , wConsumerScans = False
    , wCreateEvents = True
    }

-- | Two disjoint roots of the same watch, @r/f@ and @s/h@ are watched.
twoRoots :: World
twoRoots = World
    { wName = "two roots"
    , wRoots = ["r", "s"]
    , wTree = Map.fromList
        [ ("r", Dir), ("r/f", Dir), ("r/f/sub", Dir), ("r/f/file", File)
        , ("s", Dir), ("s/h", Dir), ("s/h/file", File)
        ]
    , wPaths = ["r/a", "r/f", "r/f/sub", "s/a", "s/h"]
    , wLinkTarget = "r/f"
    , wConfig = recCfg
    , wCheckView = True
    , wConsumerScans = False
    , wCreateEvents = True
    }

-- | A root inside a root, @r/f@ is reported for both the roots.
nestedRoots :: World
nestedRoots = oneRoot
    { wName = "nested roots"
    , wRoots = ["r", "r/f"]
    , wPaths = ["r/a", "r/b", "r/f/sub", "r/f/x", "o/m"]
    }

-- | A path of the tree can be replaced by a symbolic link to a directory
-- outside the tree. @r/f/sub@ does not exist at the start, so that it can be
-- a new directory whose add is queued. The probe in @o/t/sub@ must not be
-- reported.
linkOut :: World
linkOut = oneRoot
    { wName = "symbolic link to outside"
    , wTree = Map.fromList
        [ ("r", Dir), ("r/f", Dir), ("r/f/file", File)
        , ("o", Dir), ("o/t", Dir), ("o/t/sub", Dir)
        ]
    , wPaths = ["r/a", "r/f", "r/f/sub", "o/x"]
    , wLinkTarget = "o/t"
    }

-- | A small set of paths, for a deeper search.
small :: World
small = oneRoot
    { wName = "small"
    , wPaths = ["r/a", "r/b", "r/f", "o/x"]
    }

noScan :: World
noScan = oneRoot
    { wName = "setScanNewDirs False"
    , wPaths = ["r/a", "r/b", "r/f", "o/m", "o/x"]
    , wConfig = Event.setScanNewDirs False recCfg
    , wConsumerScans = True
    }

noCreated :: World
noCreated = noScan
    { wName = "setCreated False"
    , wConfig = Event.setCreated False recCfg
    , wCheckView = False
    , wConsumerScans = False
    , wCreateEvents = False
    }

noMoves :: World
noMoves = noScan
    { wName = "setMovedFrom False, setMovedTo False"
    , wConfig = Event.setMovedFrom False (Event.setMovedTo False recCfg)
    , wCheckView = False
    , wConsumerScans = False
    }

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

envInt :: String -> Int -> IO Int
envInt name def = do
    v <- lookupEnv name
    return (fromMaybe def (v >>= readMaybe))

check :: Int -> Int -> Int -> World -> Spec
check depth splitDepth maxShown w =
    it ("all the sequences of operations: " ++ wName w) $
        withSystemTempDirectory "fsevent_model" $ \tmp -> do
            st <- Stats <$> newIORef 0 <*> newIORef []
            search tmp st w depth splitDepth
            n <- readIORef (sCases st)
            failures <- reverse <$> readIORef (sFailures st)
            putStrLn $ wName w ++ ": " ++ show n ++ " cases, "
                ++ show (length failures) ++ " failed"
            let showF (Failure c msgs) =
                    c ++ "\n    " ++ intercalate "\n    " msgs
            out <- lookupEnv "FSEVENT_MODEL_OUT"
            forM_ out $ \file ->
                appendFile file $ unlines
                    [ wName w ++ "\t" ++ c ++ "\t" ++ m
                    | Failure c msgs <- failures, m <- msgs
                    ]
            unless (null failures)
                $ expectationFailure
                $ unlines (map showF (take maxShown failures))

main :: IO ()
main = do
    depth <- envInt "FSEVENT_MODEL_DEPTH" 2
    splitDepth <- envInt "FSEVENT_MODEL_SPLIT_DEPTH" 0
    maxShown <- envInt "FSEVENT_MODEL_MAX_FAILURES" 20
    hspec $ describe "FS.Event.Linux recursive model" $ do
        mapM_ (check depth splitDepth maxShown)
            [ oneRoot, twoRoots, nestedRoots, linkOut, small
            , noScan, noCreated, noMoves
            ]
