-- |
-- Module      : Streamly.Internal.FS.Event
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC
--
-- The internal modules of this package are named @Streamly.Internal.FS.*@
-- instead of @Streamly.Internal.FileSystem.*@ to avoid a conflict with the
-- modules of the same name in the @streamly@ package, which provides
-- @Streamly.Internal.FileSystem.Event@ and its platform specific modules.

module Streamly.Internal.FS.Event
    (
    -- * Creating a Watch

    -- ** Configuration
      Config
    , setRecursiveMode

    -- ** Watch APIs
    , watch
    , watchRecursive
    , watchWith

    -- ** Handle APIs
    -- TODO: When these are exported from Streamly.FileSystem.Event, add a
    -- "Handle API" section to docs/platform-differences.md: on macOS
    -- isRootUnwatched is reported only after removeRoot; no FSEvents
    -- history on read-only and network volumes, a root change may report
    -- isEventsLost; on Windows removeRoot drops the changes of the
    -- root not yet read from the system, and a deleted root is removed from
    -- the watch.
    , Watch
    , openWatch
    , closeWatch
    , readEvent
    , eventStream
    , addRoot
    , removeRoot
    , staleRoots

    -- * Handling Events
    , Event
    , getRoot
    , getRelPath
    , getAbsPath

    -- ** Item CRUD events
    , isCreated
    , isDeleted
    , isMoved
    , isModified

    -- ** Root events
    , isRootUnwatched

    -- ** Exception Conditions
    , isEventsLost

    -- ** Debugging
    , showEvent
    )
where

import Data.List.NonEmpty (NonEmpty)
import Streamly.Data.Stream (Stream)
import Streamly.FileSystem.Path (Path)

#if defined(CABAL_OS_DARWIN)
import Streamly.Internal.FS.Event.Darwin (Config, Event, Watch)
import qualified Streamly.Internal.FS.Event.Darwin as Event
#elif defined(CABAL_OS_LINUX)
import Streamly.Internal.FS.Event.Linux (Config, Event, Watch)
import qualified Streamly.Internal.FS.Event.Linux as Event
#elif defined(CABAL_OS_WINDOWS)
import Streamly.Internal.FS.Event.Windows (Config, Event, Watch)
import qualified Streamly.Internal.FS.Event.Windows as Event
#elif defined(CABAL_OS_FREEBSD)
import Streamly.Internal.FS.Event.FreeBSD (Config, Event, Watch)
import qualified Streamly.Internal.FS.Event.FreeBSD as Event
#else
#error "FS Events not supported on this platform"
#endif

-- The defaultConfig of all the platforms enables the create, delete, move and
-- modify events, and disables the recursive mode. The other settings are
-- platform specific.
--
-- XXX Ensure that equivalent events are generated on all platforms when same
-- operations are done. Document any differences in behavior on different
-- platforms. Any events that are not common to all platforms should be
-- filtered out.
--
-- XXX Only need to set common event types for macOS/Linux/Windows
-- XXX write a test, that generates all types of events that can possibly
-- occur on each platform. Then create a common test that only checks
-- for the common subset of those.

-------------------------------------------------------------------------------
-- Configuration
-------------------------------------------------------------------------------

-- | Watch the whole directory tree under a watch root recursively instead of
-- watching only the watch root directory.
--
-- The entries already in a directory moved into the tree are reported as
-- 'isCreated' events on Linux and FreeBSD, and are not reported on macOS and
-- Windows. A portable program scans such a directory on its 'isMoved' event,
-- and removes the duplicates on Linux and FreeBSD.
--
-- See @docs\/platform-differences.md@ in this package for the differences in
-- a recursive watch on each platform.
--
-- /default: False/
setRecursiveMode :: Bool -> Config -> Config
setRecursiveMode = Event.setRecursiveMode

-------------------------------------------------------------------------------
-- Creating a watch
-------------------------------------------------------------------------------

-- | Start monitoring a list of paths for file system events, with the
-- supplied modifications applied to the default 'Config'. The default
-- configuration enables the create, delete, move and modify events on all
-- platforms.
--
-- The 'Config' type is the same as the 'Config' type of the platform specific
-- module, therefore, the platform specific settings can also be applied when
-- the code is not meant to be portable:
--
-- @
-- watchWith (setRecursiveMode True . Linux.setFollowRootSymLinks False) paths
-- @
--
-- Each run of the stream opens a new watch and closes it when the stream ends.
-- The handle of the watch is not exposed, so the roots of the watch remain
-- fixed for as long as the stream runs. To add or remove roots while reading
-- the events, open the watch using 'openWatch', read it using 'eventStream'
-- and change the roots using 'addRoot' and 'removeRoot'.
watchWith :: (Config -> Config) -> NonEmpty Path -> Stream IO Event
watchWith = Event.watchWith

-- macOS and Windows seem to do a path level watch i.e. they watch the path
-- rather than the underlying inode. Linux watches the inode, the library
-- removes a root on its IN_MOVE_SELF event, so a renamed root is not watched
-- on Linux and macOS. A new object created at the path of a deleted or renamed
-- root is watched on macOS but not on Linux.
--
-- Also, to make the behavior of Linux same as macOS we need to set
-- setFollowRootSymLinks on Linux.

-- XXX Verify all the cases mentioned below using test cases.
-- XXX Make it fail if the watch root is a file. It works on macOS and Linux
-- but does not work on Windows.

-- | Start monitoring a list of directories or symbolic links to directories
-- for file system events.  Monitoring starts from the current time onwards.
--
-- If a watch root is a symbolic link then the target of the link is watched.
--
-- When a watch root is deleted, an 'isDeleted' event is reported for the
-- root. On Linux, FreeBSD and Windows a deleted root is not watched again,
-- and the stream ends when none of the roots is watched any more. On macOS
-- the watch continues, an object created at the path of the root later is
-- watched.
--
-- Do not rename, or delete and create again, a watch root in a portable
-- program, the result depends on the platform. See
-- @docs\/platform-differences.md@ in this package, which also describes
-- watching a path that does not exist or a path without permissions.
--
-- This API watches for changes in the watch root directory only, any changes
-- in the subdirectories of the watch root are not watched. Use
-- 'watchRecursive' to watch the whole directory tree.
--
-- The roots cannot be changed while the stream runs, see 'watchWith'.
--
-- >>> watch = watchWith id
watch :: NonEmpty Path -> Stream IO Event
watch = Event.watch

-- | Like 'watch' except that if a watched path is a directory the whole
-- directory tree under it is watched recursively. See 'setRecursiveMode'.
--
-- >>> watchRecursive = watchWith (setRecursiveMode True)
watchRecursive :: NonEmpty Path -> Stream IO Event
watchRecursive = Event.watchRecursive

-------------------------------------------------------------------------------
-- Handle APIs
-------------------------------------------------------------------------------

-- | Start monitoring the paths, with the supplied modifications applied to
-- the default 'Config', and return a handle to the watch. The watch is
-- active when the call returns: a change made after it is reported. Use
-- 'closeWatch' to close the watch.
--
-- Raise an exception if the watch cannot be started. On Windows the program
-- must be linked with the threaded RTS.
--
openWatch :: (Config -> Config) -> NonEmpty Path -> IO Watch
openWatch = Event.openWatch

-- | Close a watch. The streams reading the watch end, a thread waiting in
-- 'readEvent' returns 'Nothing'. The events that the watch has already read
-- from the system can still be returned before that. Closing a closed watch
-- does nothing. Can be called from any thread.
--
closeWatch :: Watch -> IO ()
closeWatch = Event.closeWatch

-- | Read the next event of a watch, wait for one if none is available.
-- Returns 'Nothing' when the watch is closed. Several threads can read the
-- same watch, each event is returned to one of them.
--
-- A read that is interrupted by an async exception, for example, by a
-- timeout, does not take an event out of the watch, the next read returns
-- it. An async exception that arrives after the read returns the event and
-- before the caller records it loses the event. To avoid that, call
-- 'readEvent' and the code that records the event with async exceptions
-- masked. The read can still be interrupted: while it waits for an event,
-- and before it takes an event that is available.
--
readEvent :: Watch -> IO (Maybe Event)
readEvent = Event.readEvent

-- | The events of a watch as a stream, using 'readEvent'. The stream ends
-- when the watch is closed, it does not close the watch.
--
-- The stream reports the events of the current roots of the watch: after
-- 'addRoot' it reports the events of the new root, after 'removeRoot' it
-- stops reporting the events of the removed root. See those functions for
-- the events reported while the roots change.
--
eventStream :: Watch -> Stream IO Event
eventStream = Event.eventStream

-- | Add a root to a watch. Can be called from any thread, including while
-- another thread reads the watch. The events of the root that occur before
-- the call returns may not be reported. Raise an exception if the watch is
-- closed.
--
-- A relative path is relative to the working directory at the time of the
-- call, a later change of the working directory does not affect the watch.
--
-- A path that is already a root of the watch is not added again. Case is
-- significant in the comparison of the paths, even on a case-insensitive
-- file system.
--
-- The events of the other roots of the watch are not lost or duplicated by
-- the call. On macOS this does not hold on a volume that does not keep a
-- history of file system events, e.g. a read-only or a network volume: the
-- events of the other roots that occur during the call may be lost, an
-- 'isEventsLost' event may be reported in their place.
--
addRoot :: Watch -> Path -> IO ()
addRoot = Event.addRoot

-- | Remove a root from a watch, the path must be the same as the one used to
-- add it, with the same case. The events of the root queued before the call
-- are reported. Does nothing if the watch is closed or the path is not a
-- root.
--
-- An 'isRootUnwatched' event for the root follows the events of the root.
-- On Linux and Windows the changes of the root that the watch has not yet
-- read from the system are not reported. On macOS the events of the root
-- that occur before the call are reported, and the 'isRootUnwatched' event
-- follows them, usually within a few milliseconds of the call.
--
-- The events of the other roots of the watch are not lost or duplicated by
-- the call. On macOS this does not hold on a volume that does not keep a
-- history of file system events, e.g. a read-only or a network volume: the
-- events of the other roots that occur during the call may be lost, an
-- 'isEventsLost' event may be reported in their place.
--
removeRoot :: Watch -> Path -> IO ()
removeRoot = Event.removeRoot

-- | The roots of a watch whose path no longer refers to the directory
-- watched via the root, as supplied to 'addRoot'. The watch is not changed,
-- to watch the directory now at the path, call 'removeRoot' and then
-- 'addRoot' with the path. The result can be out of date when the call
-- returns.
--
-- On Linux and FreeBSD a root becomes stale when a parent directory is
-- renamed, and on Windows when the root is renamed. The root is still
-- watched, its events report the old path. A root is stale if its path does
-- not exist or refers to another file. On macOS a root is watched by its path
-- and is never stale, the list is empty.
--
staleRoots :: Watch -> IO [Path]
staleRoots = Event.staleRoots

-------------------------------------------------------------------------------
-- Handling Events
-------------------------------------------------------------------------------

-- | Get the watch root to which the event belongs, as it was supplied to the
-- watch API. On Linux and macOS redundant separators and @.@ components are
-- removed from the path.
--
-- For an 'isEventsLost' event, this is the watch root whose events may have
-- been lost.
getRoot :: Event -> Path
getRoot = Event.getRoot

-- Why getRelPath returns a Maybe instead of "." for the watch root:
--
-- "." is a valid relative path only when the watch root is a directory. On
-- Linux (inotify) and macOS (FSEvents with file events) the watch root can be
-- a regular file. "." represents a directory, it would be misleading and
-- semantically wrong path for a file. Returning "." for a directory root and
-- 'Nothing' for a file root would give two forms for root events.

-- | Get the path of the file system object for which the event is generated,
-- relative to the watch root. Returns 'Nothing' if the event is for the watch
-- root itself, or if it is an 'isEventsLost' event for the whole watch root.
getRelPath :: Event -> Maybe Path
getRelPath = Event.getRelPath

-- XXX Ensure that on all paltforms the path has same conventions. That is do
-- not have a trailing path separator on one platfrom and not on another.

-- | Get the absolute path of the file system object for which the event is
-- generated.
--
-- If the watch root was supplied as a relative path, it is taken relative to
-- the working directory at the time the watch was started. The 'getRoot' path
-- joined with the 'getRelPath' path gives the path relative to that
-- directory.
--
-- When the watch root is a symlink, the absolute path returned is via the
-- original symlink and not through the resolved path.
--
-- If the event is for the watch root then this is the absolute path of the
-- watch root.
--
-- For an 'isEventsLost' event, this gives the path whose tree has to be
-- scanned again.
getAbsPath :: Event -> Path
getAbsPath = Event.getAbsPath

-- | Determine whether the event indicates creation of an object within the
-- monitored path. This event is generated when any file system object is
-- created.
--
-- Creating a hard link does not generate this event on all platforms. See
-- @docs\/platform-differences.md@ in this package.
--
-- In a recursive watch on Linux and FreeBSD, this event is also generated for
-- the entries of a directory moved into the tree, which may not be new. See
-- 'setRecursiveMode'.
isCreated :: Event -> Bool
isCreated = Event.isCreated

-- | Determine whether the event indicates deletion of an object within the
-- monitored path.
--
-- When a watch root itself is deleted, this event is reported for the root,
-- 'getRelPath' returns 'Nothing' for it. It is not reported when the root is
-- a symbolic link and the link is deleted. See
-- @docs\/platform-differences.md@ in this package for the events of the
-- entries in a deleted root, and for the watch after the root is deleted.
isDeleted :: Event -> Bool
isDeleted = Event.isDeleted

-- | Determine whether the event is for the old path or the new path of a
-- rename, when that path is under the watch root. A rename within the watched
-- tree reports two events, one for each path. A move out of the tree reports
-- one event, for the old path, and a move into the tree reports one event, for
-- the new path. Moving hard links is no different than other types of objects.
--
-- When a watch root itself is renamed, this event is reported for the root
-- on Linux, FreeBSD and macOS, 'getRelPath' returns 'Nothing' for it. On
-- Windows the rename of a watch root is not reported. See
-- @docs\/platform-differences.md@ in this package for the watch after the
-- root is renamed.
--
-- This event is reported for the moved directory, not for the entries in it.
-- In a recursive watch, see 'setRecursiveMode' for the entries of a directory
-- moved into the tree.
isMoved :: Event -> Bool
isMoved = Event.isMoved

-- XXX Make the Windows behavior consistent with other platforms by removing
-- the event from directories.
--
-- | Determine whether the event indicates modification of an object within the
-- monitored path. This event is generated on file modification on all
-- platforms.
--
-- Whether this event occurs for directories depends on the platform. See
-- @docs\/platform-differences.md@ in this package.
isModified :: Event -> Bool
isModified = Event.isModified

-- | An event that indicates that some events before this may have been lost,
-- therefore, we need to take some recovery action.
--
-- The tree under the path returned by 'getAbsPath' has to be scanned again to
-- know its current state. See @docs\/robustness.md@ in this package for the
-- path reported on each platform and for the recovery.
--
-- On Linux and FreeBSD the watch is closed and the stream ends after this
-- event, a new watch has to be opened to continue.
isEventsLost :: Event -> Bool
isEventsLost = Event.isEventsLost

-- | The root returned by 'getRoot' is no longer watched, no more events of
-- the root follow. It is reported after 'removeRoot' on all platforms. On
-- Linux, FreeBSD and Windows it is also reported when the watch of a root
-- ends because the root is deleted, and on Linux and FreeBSD when a root is
-- renamed. On macOS a deleted or renamed root stays watched.
--
-- The rename of a parent of a root is not reported. On Linux and FreeBSD the
-- root stays watched under the old path, but this event may be reported for
-- the root later, when the library finds that the root path no longer refers
-- to the root, for example, before it adds a directory created in a
-- recursive root.
--
-- On Linux and FreeBSD it is also reported for a directory of a recursive
-- watch that is no longer watched, 'getAbsPath' returns the directory.
isRootUnwatched :: Event -> Bool
isRootUnwatched = Event.isRootUnwatched

-------------------------------------------------------------------------------
-- Debugging
-------------------------------------------------------------------------------

-- | Convert an 'Event' record to a String representation. Note that the output
-- of this function may be different on different platforms because it may
-- contain platform specific details.
--
-- The format of the output is for debugging only, it cannot be relied upon and
-- can change at any time.
showEvent :: Event -> String
showEvent = Event.showEvent
