-- |
-- Module      : Streamly.Test.FileSystem.Event.Darwin.Replace
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- A model test of "Streamly.Internal.FS.Event.Darwin.Replace". The model
-- simulates the file system events, the FSEvents streams, the pipe and the
-- reader, with the root changes made at random points.
--
-- A simulated stream reports the events of its roots: the replayed events
-- in the order of their ids, then a history done record, and the live
-- events in order, interleaved at random with the replayed ones. The
-- records of different streams are interleaved at random. A stopped stream
-- writes up to three more records, then its retired record.

module Streamly.Test.FileSystem.Event.Darwin.Replace (main) where

import Control.Monad (foldM, replicateM)
import Data.List (sort)
import Data.Map (Map)
import Data.Maybe (isJust, isNothing)
import Data.Sequence (Seq, ViewL(..), (|>))
import Data.Set (Set)
import Streamly.Internal.FS.Event.Darwin.Replace (EventId, Since(..))
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Streamly.Internal.FS.Event.Darwin.Replace as Replace

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

type Root = Int

-- | The generation of a stream
type SGen = Replace.Gen

data FsEvent = FsEvent
    { feId :: EventId
    , feRoot :: Root
    }
    deriving Show

data Rec =
      RecEvent SGen EventId Bool -- ^ The last field is history done
    | RecRetired SGen
    deriving Show

data MStream = MStream
    { msRoots :: Set Root
    , msReplay :: [FsEvent]
    , msLive :: Seq FsEvent
    , msHistoryDone :: Bool -- ^ The history done record is not written yet
      -- | The records still written after a stop, the last is the retired
      -- record. Nothing if the stream is not stopped.
    , msTail :: Maybe [Rec]
    }

data Sim = Sim
    { smNextId :: EventId
    , smHistory :: Seq FsEvent
    , smStreams :: Map SGen MStream
    , smPipe :: Seq Rec
    , smState :: Replace.State
    , smGen :: SGen
    , smRoots :: Set Root
      -- | The current event id when the root was last added
    , smAddedAt :: Map Root EventId
    , smOpenAt :: EventId
    , smReported :: [EventId]
    , smErrors :: [String]
    , smLog :: [String]
    }

numRoots :: Int
numRoots = 3

logMsg :: String -> Sim -> Sim
logMsg msg sim = sim {smLog = msg : smLog sim}

addError :: String -> Sim -> Sim
addError msg sim = sim {smErrors = msg : smErrors sim}

-- | A file system event in a random root, watched or not.
fsEvent :: Sim -> Gen Sim
fsEvent sim = do
    r <- choose (0, numRoots - 1)
    let ev = FsEvent (smNextId sim) r
        add s
            | isNothing (msTail s) && Set.member r (msRoots s) =
                s {msLive = msLive s |> ev}
            | otherwise = s
    return
        sim
            { smNextId = smNextId sim + 1
            , smHistory = smHistory sim |> ev
            , smStreams = Map.map add (smStreams sim)
            }

-- | The records that a running stream can write next, each with the
-- stream after it is written.
nextRecords :: SGen -> MStream -> [(Rec, MStream)]
nextRecords gen s =
    replayed ++ historyDone ++ live

    where

    replayed =
        case msReplay s of
            ev : evs -> [(RecEvent gen (feId ev) False, s {msReplay = evs})]
            [] -> []
    historyDone =
        [ (RecEvent gen 0 True, s {msHistoryDone = False})
        | null (msReplay s), msHistoryDone s
        ]
    live =
        case Seq.viewl (msLive s) of
            ev :< evs -> [(RecEvent gen (feId ev) False, s {msLive = evs})]
            EmptyL -> []

-- | Write one record of a random stream to the pipe. Returns Nothing if no
-- stream has a record to write.
deliver :: Sim -> Gen (Maybe Sim)
deliver sim =
    case concatMap choices (Map.toList (smStreams sim)) of
        [] -> return Nothing
        cs -> do
            (gen, rec, s) <- elements cs
            return
                $ Just
                    sim
                        { smStreams = Map.insert gen s (smStreams sim)
                        , smPipe = smPipe sim |> rec
                        }

    where

    choices (gen, s) =
        case msTail s of
            Just (rec : recs) -> [(gen, rec, s {msTail = Just recs})]
            Just [] -> []
            Nothing -> [(gen, rec, s1) | (rec, s1) <- nextRecords gen s]

-- | Stop a running stream, it writes up to three more records.
stopStream :: Sim -> SGen -> Gen Sim
stopStream sim gen =
    case Map.lookup gen (smStreams sim) of
        Just s | isNothing (msTail s) -> do
            k <- choose (0, 3 :: Int)
            recs <- takeRecs k s
            let s1 = s {msTail = Just (recs ++ [RecRetired gen])}
            return
                $ logMsg ("stop " ++ show gen)
                    sim {smStreams = Map.insert gen s1 (smStreams sim)}
        _ -> return $ addError ("stop of a stopped stream " ++ show gen) sim

    where

    takeRecs 0 _ = return []
    takeRecs k s =
        case nextRecords gen s of
            [] -> return []
            cs -> do
                (rec, s1) <- elements cs
                (rec :) <$> takeRecs (k - 1 :: Int) s1

-- | The reader processes the next record of the pipe. Returns Nothing if
-- the pipe is empty.
readRecord :: Sim -> Gen (Maybe Sim)
readRecord sim =
    case Seq.viewl (smPipe sim) of
        EmptyL -> return Nothing
        rec :< rest -> Just <$> process rec sim {smPipe = rest}

    where

    process (RecRetired gen) s =
        return
            s
                { smState = Replace.onRetired gen (smState s)
                , smStreams = Map.delete gen (smStreams s)
                }
    process rec@(RecEvent gen eid hd) s = do
        let (report, stops, st) = Replace.onEvent gen eid hd (smState s)
            s1 =
                logMsg (show rec ++ if report then " reported" else "")
                    s {smState = st}
            s2 =
                if report
                then s1 {smReported = eid : smReported s1}
                else s1
        foldM stopStream s2 stops

-- | Start a simulated stream on the roots.
startStream :: SGen -> Since -> Set Root -> Sim -> Sim
startStream gen since roots sim =
    sim {smStreams = Map.insert gen s (smStreams sim)}

    where

    replay =
        case since of
            SinceNow -> []
            SinceId b ->
                [ ev | ev <- foldr (:) [] (smHistory sim)
                , feId ev > b, Set.member (feRoot ev) roots
                ]
    s =
        MStream
            { msRoots = roots
            , msReplay = replay
            , msLive = Seq.empty
            , msHistoryDone = since /= SinceNow
            , msTail = Nothing
            }

-- | Set the roots of the watch, as 'addRoot' and 'removeRoot' do.
setRoots :: Set Root -> Sim -> Gen Sim
setRoots roots sim
    | Set.null roots = do
        let (stops, st) = Replace.stopAll (smState sim)
        foldM stopStream (logMsg "stopAll" (common st)) stops
    | otherwise = do
        let (since, stops, st) = Replace.replace gen now (smState sim)
            sim1 =
                startStream gen since roots
                    $ logMsg
                        ("start " ++ show gen ++ " " ++ show (Set.toList roots)
                            ++ " " ++ show since)
                    $ common st
        foldM stopStream sim1 stops

    where

    gen = smGen sim + 1
    now = smNextId sim - 1
    added = Set.difference roots (smRoots sim)
    common st =
        sim
            { smState = st
            , smGen = gen
            , smRoots = roots
            , smAddedAt =
                Map.union
                    (Map.fromSet (const now) added)
                    (Map.restrictKeys (smAddedAt sim) roots)
            }

-- | Add or remove a random root.
rootChange :: Sim -> Gen Sim
rootChange sim = do
    r <- choose (0, numRoots - 1)
    let roots =
            if Set.member r (smRoots sim)
            then Set.delete r (smRoots sim)
            else Set.insert r (smRoots sim)
    setRoots roots sim

-- | The history before the watch is opened, and the open.
openSim :: Gen Sim
openSim = do
    n <- choose (0, 5)
    roots <- Set.fromList <$> listOf1 (choose (0, numRoots - 1))
    let sim0 =
            Sim
                { smNextId = 1
                , smHistory = Seq.empty
                , smStreams = Map.empty
                , smPipe = Seq.empty
                , smState = Replace.initial
                , smGen = 0
                , smRoots = Set.empty
                , smAddedAt = Map.empty
                , smOpenAt = 0
                , smReported = []
                , smErrors = []
                , smLog = []
                }
    sim1 <- foldM (\s _ -> fsEvent s) sim0 [1 .. n :: Int]
    setRoots roots sim1 {smOpenAt = smNextId sim1 - 1}

-- | Run the steps in random order, then write and read all the records.
runSim :: Int -> Gen Sim
runSim steps = do
    -- The weights of file system events, deliveries, reads and root
    -- changes, different in each run.
    ws <- replicateM 4 (choose (1, 10 :: Int))
    sim0 <- openSim
    sim1 <- foldM (\s _ -> step ws s) sim0 [1 .. steps]
    drain sim1

    where

    step ws sim = do
        k <- frequency (zip ws (map pure [0 :: Int ..]))
        case k of
            0 -> fsEvent sim
            1 -> maybe sim id <$> deliver sim
            2 -> maybe sim id <$> readRecord sim
            _ -> rootChange sim

    drain sim = do
        d <- deliver sim
        r <- readRecord sim
        case (d, r) of
            (Nothing, Nothing) -> return sim
            (Just s, Nothing) -> drain s
            (Nothing, Just s) -> drain s
            (Just s1, Just s2) -> do
                s <- elements [s1, s2]
                drain s

-------------------------------------------------------------------------------
-- Checks
-------------------------------------------------------------------------------

duplicates :: [EventId] -> [EventId]
duplicates xs = [x | (x, y) <- zip sorted (drop 1 sorted), x == y]

    where

    sorted = sort xs

checkSim :: Sim -> Property
checkSim sim =
    counterexample (unlines (reverse (smLog sim)))
        $ conjoin
            [ counterexample ("errors: " ++ show (smErrors sim))
                (null (smErrors sim))
            , counterexample ("duplicates: " ++ show dups) (null dups)
            , counterexample ("missing: " ++ show missing) (null missing)
            , counterexample ("before open: " ++ show early) (null early)
            , counterexample "a stream is still replaying"
                (isNothing (Replace.pendingGen st))
            , counterexample ("running streams: " ++ show running)
                (running == [g | Just g <- [Replace.stableGen st]])
            , counterexample "the stable stream is missing"
                (isJust (Replace.stableGen st) == not (Set.null roots))
            ]

    where

    st = smState sim
    roots = smRoots sim
    reported = Set.fromList (smReported sim)
    dups = duplicates (smReported sim)
    early = filter (<= smOpenAt sim) (smReported sim)
    required =
        [ feId ev
        | ev <- foldr (:) [] (smHistory sim)
        , Just added <- [Map.lookup (feRoot ev) (smAddedAt sim)]
        , feId ev > added
        ]
    missing = filter (`Set.notMember` reported) required
    running = [g | (g, s) <- Map.toList (smStreams sim), isNothing (msTail s)]

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main =
    hspec
        $ describe "Streamly.Internal.FS.Event.Darwin.Replace"
        $ modifyMaxSuccess (const 5000)
        $ prop "no event of a root is lost or reported twice"
        $ forAllBlind (sized (\n -> runSim (4 * n))) checkSim
