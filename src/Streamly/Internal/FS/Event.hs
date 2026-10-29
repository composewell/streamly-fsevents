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

-- XXX Need to ensure that the signatures of the exported APIs are same on all
-- platforms.

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
import Streamly.Internal.FS.Event.Darwin (Config, Event)
import qualified Streamly.Internal.FS.Event.Darwin as Event
#elif defined(CABAL_OS_LINUX)
import Streamly.Internal.FS.Event.Linux (Config, Event)
import qualified Streamly.Internal.FS.Event.Linux as Event
#elif defined(CABAL_OS_WINDOWS)
import Streamly.Internal.FS.Event.Windows (Config, Event)
import qualified Streamly.Internal.FS.Event.Windows as Event
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
-- macOS and Windows watch a directory tree natively. Linux inotify watches
-- only one directory, a recursive watch adds a watch for each directory in
-- the tree, and adds a watch for a new directory when its create event is
-- received:
--
-- * Starting a watch on a large tree takes time and memory proportional to
-- the number of directories in the tree.
-- * Creating a directory and adding a watch for it is not atomic, events that
-- occur in a new directory before its watch is added may be lost.
--
-- /default: False/
--
-- /Pre-release/
--
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
-- watchWith (setRecursiveMode True . Linux.setFollowSymLinks False) paths
-- @
--
-- /Pre-release/
--
watchWith :: (Config -> Config) -> NonEmpty Path -> Stream IO Event
watchWith = Event.watchWith

-- macOS and Windows seem to do a path level watch i.e. they watch the path
-- rather than the underlying inode, possibly they can map the inode back to a
-- path?.  On Linux we cannot keep watching a watch root if the inode it is
-- pointing to has moved. However, the inode continues to be watched in this
-- case instead of the path, which is not the case for macOS.
--
-- To make the behavior same for all platforms, for the Linux case we will have
-- to stop watching the inode if it moves from under the path, on the
-- IN_MOVE_SELF event of the root, no inotify flag does that.  And on macOS we
-- can stop watching  a path if the inode under it changes i.e. just filter out
-- events for that path.
--
-- Also, to make the behavior of Linux same as macOS we need to set
-- setFollowSymLinks on Linux.

-- XXX Verify all the cases mentioned below using test cases.
-- XXX Make it fail if the watch root is a file. It works on macOS and Linux
-- but does not work on Windows.

-- | Start monitoring a list of directories or symbolic links to directories
-- for file system events.  Monitoring starts from the current time onwards.
--
-- If a watch root is a symbolic link then the target of the link is watched.
-- Fails if the watched path does not exist. If the user does not have
-- permissions (read and execute?) on the watch root then no events are
-- generated.  No events are generated if the watch root itself is renamed or
-- deleted.
--
-- This API watches for changes in the watch root directory only, any changes
-- in the subdirectories of the watch root are not watched. Use
-- 'watchRecursive' to watch the whole directory tree.
--
-- On macOS FSEvents can only watch a directory tree recursively, the events
-- for paths more than one level below a watch root are dropped by this API.
-- The OS still generates and delivers those events to this process.
--
-- >>> watch = watchWith id
--
-- /Pre-release/
--
watch :: NonEmpty Path -> Stream IO Event
watch = Event.watch

-- | Like 'watch' except that if a watched path is a directory the whole
-- directory tree under it is watched recursively. See 'setRecursiveMode' for
-- the limitations on Linux.
--
-- >>> watchRecursive = watchWith (setRecursiveMode True)
--
-- /Pre-release/
--
watchRecursive :: NonEmpty Path -> Stream IO Event
watchRecursive = Event.watchRecursive

-------------------------------------------------------------------------------
-- Handling Events
-------------------------------------------------------------------------------

-- | Get the watch root to which the event belongs, as it was supplied to the
-- watch API.
--
-- /Pre-release/
--
getRoot :: Event -> Path
getRoot = Event.getRoot

-- | Get the path of the file system object for which the event is generated,
-- relative to the watch root. Returns 'Nothing' if the event is for the watch
-- root itself.
--
-- /Pre-release/
--
getRelPath :: Event -> Maybe Path
getRelPath = Event.getRelPath

-- XXX Ensure that on all paltforms the path has same conventions. That is do
-- not have a trailing path separator on one platfrom and not on another.
--
-- | Get the absolute path of the file system object for which the event is
-- generated.
--
-- When the watch root is a symlink, the absolute path returned is via the
-- original symlink and not through the resolved path.
--
-- /Pre-release/
--
getAbsPath :: Event -> Path
getAbsPath = Event.getAbsPath

-- | Determine whether the event indicates creation of an object within the
-- monitored path. This event is generated when any file system object is
-- created.
--
-- For hard links the behavior is different on different operating systems. On
-- macOS hard linking does not generate a create event, it generates an
-- 'Streamly.Internal.FS.Event.Darwin.isAttrsModified' event on the directory
-- instead. On Linux and Windows hard linking generates a create event.
--
-- /Pre-release/
--
isCreated :: Event -> Bool
isCreated = Event.isCreated

-- XXX To make the behavior consistent, can we block the event on watch root on
-- macOS?
--
-- | Determine whether the event indicates deletion of an object within the
-- monitored path. On Linux and Windows hard link deletion generates a delete
-- event.
--
-- On Linux and Windows, this event does not occur when the watch root itself
-- is deleted. On macOS it occurs on deleting the watch root when it is not a
-- symbolic link.
--
-- See also the 'Streamly.Internal.FS.Event.Linux.isRootDeleted' event on
-- Linux.
--
-- /Pre-release/
--
isDeleted :: Event -> Bool
isDeleted = Event.isDeleted

-- | Determine whether the event indicates rename of an object within the
-- monitored path. This event is generated when an object is renamed within the
-- watched directory or if it is moved out of or in the watched directory.
-- Moving hard links is no different than other types of objects.
--
-- /Pre-release/
--
isMoved :: Event -> Bool
isMoved = Event.isMoved

-- XXX Make the Windows behavior consistent with other platforms by removing
-- the event from directories.
--
-- | Determine whether the event indicates modification of an object within the
-- monitored path. This event is generated on file modification on all
-- platforms.
--
-- On Linux and macOS this event is never generated for directories.  On
-- Windows (in recursive watch mode) this event is generated for directories as
-- well when an object is created in or deleted from the directory.
--
-- /Pre-release/
--
isModified :: Event -> Bool
isModified = Event.isModified

-- | An event that indicates that some events before this may have been lost,
-- therefore, we need to take some recovery action.
--
-- 'getRoot' returns the watch root whose events may have been lost. On
-- Linux, where the kernel reports a single event for all the roots, one
-- event is generated for each watch root.
--
-- /Pre-release/
--
isEventsLost :: Event -> Bool
isEventsLost = Event.isEventsLost

-------------------------------------------------------------------------------
-- Debugging
-------------------------------------------------------------------------------

-- | Convert an 'Event' record to a String representation. Note that the output
-- of this function may be different on different platforms because it may
-- contain platform specific details.
--
-- /Internal/
--
showEvent :: Event -> String
showEvent = Event.showEvent
