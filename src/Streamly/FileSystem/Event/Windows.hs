-- |
-- Module      : Streamly.FileSystem.Event.Windows
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- File system event notification API for Windows, using the
-- ReadDirectoryChangesW API. See "Streamly.FileSystem.Event" for the portable
-- API.
--
-- The 'Config' and 'Event' types are the same as in
-- "Streamly.FileSystem.Event", the functions in this module can be used with
-- the portable API as well.
--
-- = Overview
--
-- Use 'watchRecursive' or 'watch' with a non-empty list of file system dir
-- paths you want to watch as argument. It returns a stream of 'Event'
-- representing the file system events occurring under the watched paths.
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
-- A watch keeps its watch root open, deleting the watch root fails with a
-- permission error while it is watched.
--
-- When recursive mode is True, the whole directory tree under the path is
-- watched recursively.  When recursive mode is False, only the files and
-- directories directly under the watched directory are monitored, contents of
-- subdirectories are not monitored.
--
-- Monitoring starts from the current time onwards.
--
-- /Symbolic Links:/ If the pathname to be watched is a symbolic link then
-- watch the target of the symbolic link instead of the symbolic link itself.
-- Note that the path location in the events is through the original symbolic
-- link path rather than the resolved path.
--
-- = Platform differences
--
-- For reference documentation see:
--
-- * <https://docs.microsoft.com/en-us/windows/win32/api/winnt/ns-winnt-file_notify_information file notify information>
-- * <https://docs.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-readdirectorychangesw read directory changes>
--
-- As per the Windows reference docs, the fs event notification API is
-- available in:
--
-- * Minimum supported client: Windows XP [desktop apps | UWP apps]
-- * Minimum supported server: Windows Server 2003 [desktop apps | UWP apps]
--
-- We try to keep the macOS\/Linux\/Windows event handling APIs and defaults
-- semantically and syntactically as close as possible.
--
-- See @docs\/platform-differences.md@ in this package for the differences
-- between the Linux, macOS and Windows APIs.

module Streamly.FileSystem.Event.Windows
    (
    -- * Creating a Watch

    -- ** Configuration
      Config
    , setRecursiveMode

    -- ** Events of Interest
    , setFileNameEvents
    , setDirNameEvents
    , setAttrsModified
    , setSecurityModified
    , setSizeModified
    , setLastWriteTimeModified
    , setAllEvents

    -- ** Watch APIs
    , watch
    , watchRecursive
    , watchWith

    -- * Handling Events
    , Event
    , getRoot
    , getRelPath
    , getAbsPath
    , showEvent

    -- ** Item CRUD events
    , isCreated
    , isDeleted
    , isModified
    , isMoved
    , isMovedFrom
    , isMovedTo

    -- ** Exception Conditions
    , isEventsLost
    )
where

import Streamly.Internal.FS.Event.Windows
