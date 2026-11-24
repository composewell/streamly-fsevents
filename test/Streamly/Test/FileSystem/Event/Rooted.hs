-- |
-- Module      : Streamly.Test.FileSystem.Event.Rooted
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- The event streams of "Streamly.Internal.FS.Event" that raise an exception
-- on an event that is not under any watch root. The tests import this
-- module with the same qualifier as the library module, hiding the library
-- functions, so that every event they read is checked.
--
-- On macOS the library matches the event paths against the resolved root
-- paths, an event that does not match any root is a bug in the library,
-- except for the events listed in 'isRootless'. On Linux and Windows every
-- event has a root.

module Streamly.Test.FileSystem.Event.Rooted
    ( hasNoRoot
    , isRootless
    , checkRoot
    , watch
    , watchWith
    , watchRecursive
    , eventStream
    , readEvent
    )
where

import Data.List.NonEmpty (NonEmpty)
import Streamly.Data.Stream (Stream)
import Streamly.Internal.FS.Event (Config, Event, Watch)
import Streamly.Internal.FileSystem.Path (Path)

import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.FS.Event as Event
#if defined(CABAL_OS_DARWIN)
import qualified Streamly.Internal.FS.Event.Darwin as Darwin
#endif

-- | Whether the event is not under any watch root. 'False' on Linux and
-- Windows.
hasNoRoot :: Event -> Bool
#if defined(CABAL_OS_DARWIN)
hasNoRoot = not . Darwin.hasRoot
#else
hasNoRoot _ = False
#endif

-- | Whether the event is not under any watch root and is not one of the
-- macOS events that can report a path outside the roots: the root path
-- event of a missing root reports its parent, a mount event reports the
-- mount point, and the events of the stream itself, e.g. a loss of events,
-- can report any path.
isRootless :: Event -> Bool
#if defined(CABAL_OS_DARWIN)
isRootless ev =
    hasNoRoot ev
        && not
            (any ($ ev)
                [ Darwin.isRootPathEvent
                , Darwin.isEventsLost
                , Darwin.isKernelDropped
                , Darwin.isUserDropped
                , Darwin.isHistoryDone
                , Darwin.isEventIdWrapped
                , Darwin.isMounted
                , Darwin.isUnmounted
                ])
#else
isRootless = hasNoRoot
#endif

-- | Raise an exception if the event is not under any watch root, see
-- 'isRootless'.
checkRoot :: Event -> IO Event
checkRoot ev
    | isRootless ev =
        ioError
            $ userError
            $ "event not under any watch root:\n" ++ Event.showEvent ev
    | otherwise = return ev

watchWith :: (Config -> Config) -> NonEmpty Path -> Stream IO Event
watchWith f paths = Stream.mapM checkRoot (Event.watchWith f paths)

watch :: NonEmpty Path -> Stream IO Event
watch paths = Stream.mapM checkRoot (Event.watch paths)

watchRecursive :: NonEmpty Path -> Stream IO Event
watchRecursive paths = Stream.mapM checkRoot (Event.watchRecursive paths)

eventStream :: Watch -> Stream IO Event
eventStream w = Stream.mapM checkRoot (Event.eventStream w)

readEvent :: Watch -> IO (Maybe Event)
readEvent w = Event.readEvent w >>= traverse checkRoot
