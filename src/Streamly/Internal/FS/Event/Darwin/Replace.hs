-- |
-- Module      : Streamly.Internal.FS.Event.Darwin.Replace
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC
--
-- The replacement of the FSEvents stream of a watch when its roots change,
-- implemented as a pure state machine. "Streamly.Internal.FS.Event.Darwin"
-- starts and stops the streams as this module says. The module is free from
-- FFI, it is platform independent, its model test can run on all platforms.
--
-- The paths of an FSEvents stream cannot be changed after it is created. When
-- we change roots a new stream "pending" is started on the new roots, from the
-- event id "base". The FSEvents history is used to replay the events after
-- base, when its done it reports a history done event. During the replay the
-- pending stream reports live events too, its ids are not in order. The old
-- stream, "stable", keeps running until the reader reads the history done
-- event of the pending stream. Then the stable stream is stopped and the
-- pending stream becomes stable. The records of the old stable stream read
-- after "history done" event  are dropped: they are events of the removed
-- roots, or events that the new stable stream reports.
--
-- Each stream has a generation number. A stream reports the events of its
-- roots in the order of their ids, except during a replay. After its
-- replay, all the events of its roots up to the highest id read from it
-- have been read. This id, "stable max", is the base of the next
-- replacement. If the roots change again during a replay, the pending
-- stream is stopped and the new one starts from the same base.
--
-- While two streams overlap, an event can be reported by both. The ids
-- greater than base of the events reported are kept in the set "dedupe
-- ids", an event with an id in the set, or not greater than base, is
-- dropped as a duplicate. When the stable stream is the only stream whose
-- records are read, it reports the events in order, the ids not greater
-- than its last one are removed from the set, and the set is not used once
-- it is empty.

module Streamly.Internal.FS.Event.Darwin.Replace
    ( Gen
    , EventId
    , Since (..)
    , State
    , initial
    , stableGen
    , pendingGen
    , replace
    , stopAll
    , onEvent
    , onRetired
    )
where

import Data.Maybe (catMaybes, isNothing, maybeToList)
import Data.Set (Set)
import Data.Word (Word64)

import qualified Data.Set as Set

-- | The generation number of a stream.
type Gen = Word64

type EventId = Word64

-- | Where a new stream starts.
data Since =
      SinceNow
      -- | Replay the history after the id, with the full history flag.
    | SinceId !EventId
    deriving (Eq, Show)

data State = State
    { -- | The generation of the stable stream: the stream on the current
      -- roots whose history done event has been read, or which started
      -- without a replay. Nothing when the watch has no root.
      stStable :: !(Maybe Gen)
      -- | The generation of the pending stream: the stream started by the
      -- last root change, whose history done event has not been read yet.
      -- It replaces the stable stream when that event is read. Nothing
      -- when no stream is replaying.
    , stPending :: !(Maybe Gen)
      -- | The highest id read from the stable stream, or the id it started
      -- from. The events of the roots of the stable stream with an id up
      -- to this one have been read.
    , stStableMax :: !EventId
      -- | The highest id read from the pending stream, or 'stDedupeBase'
      -- if no higher id has been read from it.
    , stPendingMax :: !EventId
      -- | True from the first replacement until the stable stream is the
      -- only stream being read and 'stDedupeIds' is empty. When False no
      -- event is dropped as a duplicate, 'stDedupeBase' and
      -- 'stDedupeIds' are not used.
    , stDedupeActive :: !Bool
      -- | The id after which the pending stream replays the history. It is
      -- 'stStableMax' at the time of the replacement: all the events up
      -- to this id were reported before the pending stream started, so an
      -- event with an id up to this one is a duplicate.
    , stDedupeBase :: !EventId
      -- | The ids greater than 'stDedupeBase' of the events reported so
      -- far. Two overlapping streams both report an event that is under
      -- the roots of both, the second record of an id in this set is
      -- dropped.
    , stDedupeIds :: !(Set EventId)
      -- | Stopped due to root change, their records are read until their
      -- retired record.
    , stRetiring :: !(Set Gen)
      -- | The old stable streams, stopped when the history done event of
      -- the pending stream was read. A stopped stream can still have
      -- unread records, written before it stopped. These records are
      -- dropped without looking at their ids. A stream stays in this set
      -- until its retired record, its last record, is read.
    , stCut :: !(Set Gen)
    }
    deriving Show

-- | The state of a watch without any stream.
initial :: State
initial =
    State
        { stStable = Nothing
        , stPending = Nothing
        , stStableMax = 0
        , stPendingMax = 0
        , stDedupeActive = False
        , stDedupeBase = 0
        , stDedupeIds = Set.empty
        , stRetiring = Set.empty
        , stCut = Set.empty
        }

stableGen :: State -> Maybe Gen
stableGen = stStable

pendingGen :: State -> Maybe Gen
pendingGen = stPending

-- | @replace gen now st@ replaces the streams by a stream of generation
-- @gen@ on a non-empty list of roots. @now@ is the current event id of the
-- system. Returns where the new stream starts, the streams to stop and the
-- new state. Use the new state only if the new stream starts, stop the old
-- streams after it starts.
replace :: Gen -> EventId -> State -> (Since, [Gen], State)
replace gen now st =
    case stStable st of
        -- The ids of the new stream are greater than now, they are not in
        -- the dedupe ids.
        Nothing -> (SinceNow, [], st {stStable = Just gen, stStableMax = now})
        Just _ ->
            case stPending st of
                Just old ->
                    ( SinceId (stDedupeBase st)
                    , [old]
                    , st
                        { stPending = Just gen
                        , stPendingMax = stDedupeBase st
                        , stRetiring = Set.insert old (stRetiring st)
                        }
                    )
                Nothing ->
                    let base = stStableMax st
                    in ( SinceId base
                       , []
                       , st
                            { stPending = Just gen
                            , stPendingMax = base
                            , stDedupeActive = True
                            , stDedupeBase = base
                            -- The dedupe ids can be non-empty here: the ids
                            -- greater than base are of events that the stable
                            -- stream has not reported yet, the new stream
                            -- replays them too.
                            , stDedupeIds =
                                snd (Set.split base (stDedupeIds st))
                            }
                       )

-- | Stop all the streams, when the last root is removed or the watch is
-- closed. Returns the streams to stop.
stopAll :: State -> ([Gen], State)
stopAll st =
    ( gens
    , st
        { stStable = Nothing
        , stPending = Nothing
        , stRetiring = foldr Set.insert (stRetiring st) gens
        }
    )

    where

    gens = catMaybes [stPending st, stStable st]

-- | @onEvent gen eid historyDone st@ is called for each event record read,
-- in the order of the records. Returns whether the event is reported, the
-- streams to stop and the new state. A history done event is not reported.
onEvent :: Gen -> EventId -> Bool -> State -> (Bool, [Gen], State)
onEvent gen eid historyDone st
    | Set.member gen (stCut st) = (False, [], st)
    | historyDone =
        if Just gen == stPending st
        then
            let old = maybeToList (stStable st)
            in ( False
               , old
               , st
                    { stStable = Just gen
                    , stPending = Nothing
                    , stStableMax = stPendingMax st
                    , stCut = foldr Set.insert (stCut st) old
                    }
               )
        else (False, [], st)
    -- An event with id 0, e.g. a root change, is not replayed
    | eid == 0 = (True, [], st)
    | otherwise = (not dup, [], st1)

    where

    isStable = Just gen == stStable st
    isPending = Just gen == stPending st

    dup =
        stDedupeActive st
            && (eid <= stDedupeBase st || Set.member eid (stDedupeIds st))

    alone =
        isStable && isNothing (stPending st) && Set.null (stRetiring st)

    dedupeIds
        | not (stDedupeActive st) = stDedupeIds st
        | alone = snd (Set.split eid (stDedupeIds st))
        | dup = stDedupeIds st
        | otherwise = Set.insert eid (stDedupeIds st)

    st1 =
        st
            { stStableMax =
                if isStable
                then max eid (stStableMax st)
                else stStableMax st
            , stPendingMax =
                if isPending
                then max eid (stPendingMax st)
                else stPendingMax st
            , stDedupeActive =
                stDedupeActive st && not (alone && Set.null dedupeIds)
            , stDedupeIds = dedupeIds
            }

-- | Called for the retired record of a stream, the stream writes no more
-- records.
onRetired :: Gen -> State -> State
onRetired gen st =
    st
        { stRetiring = Set.delete gen (stRetiring st)
        , stCut = Set.delete gen (stCut st)
        }
