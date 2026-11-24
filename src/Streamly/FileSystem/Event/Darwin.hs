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
-- /Watch root:/ A watch root is bound to the object that its path refers to
-- when the root is added. The watch of the root ends with 'isRootUnwatched'
-- and 'isRootStale' when the library finds that the path no longer refers to
-- that object, e.g. when the root is deleted or renamed, or when a symbolic
-- link in the root path is changed to point to another directory. See
-- 'allowMissingRootPath' and 'enableStaleRootCheck'.
--
-- /Symbolic link watch root:/ If the path name to be watched is a symbolic
-- link then the target of the link is watched instead of the symbolic link
-- itself. It is equivalent to as if the target of the symbolic link itself was
-- directly added to the watch API. That is, the symbolic link is resolved at
-- the time of adding the watch. However, the paths in the events are reported
-- via the original symbolic link, see 'getAbsPath'.
--
-- /Non-existing Paths:/ A watch on a non-existing path raises an exception.
-- With 'allowMissingRootPath' 'True' the path is watched, and the path is
-- watched when it is created later (tested on macOS 26). A deleted root then
-- stays watched, the object created at its path later is watched.
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
    , allowMissingRootPath
    , enableStaleRootCheck

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
    , showEvent

    -- ** Root Level Events
    -- | Events that belong to the root path as a whole and not to specific
    -- items contained in it.
    , isMounted
    , isUnmounted
    , isHistoryDone
    , isRootPathEvent
    , isRootUnwatched
    , isRootStale

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
