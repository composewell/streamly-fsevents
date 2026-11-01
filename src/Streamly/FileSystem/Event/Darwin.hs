-- |
-- Module      : Streamly.FileSystem.Event.Darwin
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- File system event notification API for macOS, using the FSEvents API. See
-- "Streamly.FileSystem.Event" for the portable API.
--
-- The 'Config' and 'Event' types are the same as in
-- "Streamly.FileSystem.Event", the functions in this module can be used with
-- the portable API as well.
--
-- = Overview
--
-- Use 'watch' or 'watchRecursive' with a non-empty list of file system paths
-- you want to watch as argument. It returns a stream of 'Event' representing
-- the file system events occurring under the watched paths.
--
-- @
-- path <- Path.fromString "path"
-- Stream.mapM_ (putStrLn . Path.toString . 'getAbsPath')
--     $ 'watchRecursive' (path :| [])
-- @
--
-- 'Event' is an opaque type. Accessor functions (e.g. 'getAbsPath' above)
-- provided in this module are used to determine the attributes of the event.
--
-- = Watch Behavior
--
-- /Watch root:/ A watch is attached to the path of a watch root. From the
-- observed behavior, macOS watches the path, whatever object the path points
-- to at any given point of time.
--
-- /Watch root deletion:/ If the watch root is deleted and then recreated,
-- the newly created file or directory is automatically watched.
--
-- /Watch root moved:/ If the watch root is moved to a new path, the object
-- will no longer be watched unless the new path is also being watched and was
-- pointing to an existing file at the time of starting the watch (see notes
-- about non-existing paths below).
--
-- /Symbolic link watch root:/ If the path name to be watched is a symbolic
-- link then the target of the link is watched instead of the symbolic link
-- itself. It is equivalent to as if the target of the symbolic link itself was
-- directly added to the watch API. That is, the symbolic link is resolved at
-- the time of adding the watch. However, the paths in the events are reported
-- via the original symbolic link, see 'getAbsPath'.
--
-- Note that if a watched path is deleted and recreated as a symbolic link
-- pointing to another path then the symbolic link file itself is watched, it
-- won't be resolved. The symbolic link resolution happens only at the time of
-- adding the watch.
--
-- /Non-existing Paths:/ If a watch is started on a non-existing path then the
-- path is watched when it is created later (tested on macOS 26). The macOS
-- API does not fail for a non-existing path.
--
-- If a non-existing path is watched with 'setRootPathEvents' then an
-- 'isRootPathEvent' event is reported if the path is created later and the
-- "path" field in the event is set to the dirname of the path rather than the
-- full absolute path. This is the observed behavior on macOS 10.15.1.
--
-- /Coalescing:/ More than one event predicate can be true for the same event.
-- FSEvents can coalesce several changes of a path into one event. For example,
-- @touch x; rm x@ can report one event for which both 'isCreated' and
-- 'isDeleted' are true, and @touch x; mv x y@ one event for which both
-- 'isCreated' and 'isMoved' are true. The event does not give the order of the
-- changes. A program must test every predicate it is interested in, and read
-- the file system to find the current state of the path.
--
-- = Platform differences
--
-- For reference documentation see:
--
-- * "<https://developer.apple.com/library/archive/documentation/Darwin/Conceptual/FSEvents_ProgGuide/UsingtheFSEventsFramework/UsingtheFSEventsFramework.html Apple FS Events Programming Guide>"
-- * "<https://developer.apple.com/documentation/coreservices/file_system_events?language=objc Apple FS Events Reference>"
--
-- We try to keep the macOS\/Linux\/Windows event handling APIs and defaults
-- semantically and syntactically as close as possible.
--
-- See @docs\/platform-differences.md@ in this package for the differences
-- between the Linux, macOS and Windows APIs.

-- XXX Write behavior tests for all the points in Watch Behavior above.

#include <config.h>

#if HAVE_DECL_KFSEVENTSTREAMCREATEFLAGFILEEVENTS
module Streamly.FileSystem.Event.Darwin
    (
    -- * Creating a Watch

    -- ** Configuration
      Config
    , setRecursiveMode
    , BatchInfo (..)
    , setEventBatching

    -- ** Events of Interest
    , setRootPathEvents
    , setFileEvents
    , setIgnoreSelf
    , setFullHistory
    , setAllEvents

    -- ** Watch APIs
    , watch
    , watchRecursive
    , watchWith

    -- * Handling Events
    , Event
    , getEventId
    , getRoot
    , getRelPath
    , getAbsPath

    -- ** Root Level Events
    -- | Events that belong to the root path as a whole and not to specific
    -- items contained in it.
    , isMounted
    , isUnmounted
    , isHistoryDone
    , isRootPathEvent

    -- ** Item Level Metadata change
    , isAttrsModified
    , isSecurityModified
    , isXAttrsModified
    , isFinderInfoModified

    -- ** Item Level CRUD events
    , isCreated
    , isDeleted
    , isMoved
    , isModified
    , isCloned

    -- ** Item Path info
    , isDir
    , isFile
    , isSymLink
    , isHardLink
    , isLastHardLink

    -- ** Exception Conditions
    , isEventIdWrapped
    , isEventsLost
    , isKernelDropped
    , isUserDropped
    )
where

import Streamly.Internal.FS.Event.Darwin
#else
module Streamly.FileSystem.Event.Darwin () where
#endif
