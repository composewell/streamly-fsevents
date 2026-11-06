-- |
-- Module      : Streamly.FileSystem.Event.Linux
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- File system event notification API for Linux, using the inotify API. See
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
-- path <- Path.fromString "dir"
-- Stream.mapM_ (putStrLn . Path.toString . 'getAbsPath')
--     $ 'watch' (path :| [])
-- @
--
-- 'Event' is an opaque type. Accessor functions (e.g. 'getAbsPath' above)
-- provided in this module are used to determine the attributes of the event.
--
-- = Watch Behavior
--
-- An inotify watch is attached to the inode of a watch root, not to its path.
--
-- /Watch root:/
--
-- * If a watch root is deleted, its watch ends with an 'isRootUnwatched'
-- event.
-- * If a watch root is renamed, its watch ends with an 'isRootUnwatched'
-- event. The kernel does not report the new path of the root.
-- * An object created at the path of a removed root later, or renamed to it,
-- is not watched, including the root renamed back. Add the path to the watch
-- again to watch it.
--
-- Use 'setRootPathEvents' to receive the 'isRootDeleted' and 'isRootMoved'
-- events. Objects inside a watched directory do not have these limitations. A
-- file deleted and created again in a watched directory reports 'isDeleted'
-- and 'isCreated', and its later events are reported.
--
-- /Non-existing Paths:/ An exception is raised if a watch is started on a
-- non-existing path.
--
-- /Overlapping Paths:/ A path listed more than once is watched once, its
-- events are reported once. If two different paths reach the same file or
-- directory, for example, a recursive root and a root inside it, its events
-- are reported once for each path.
--
-- /Coalescing:/ Identical successive events may be coalesced into a single
-- event.
--
-- /Performance:/ Note that recursive watch on a large directory tree could be
-- expensive. When starting a watch, the whole tree must be read and watches
-- are started on each directory in the tree. The initial time to start the
-- watch as well as the memory required is proportional to the number of
-- directories in the tree. A directory moved into the tree is read the same
-- way when its move event is read, see 'setScanNewDirs'.
--
-- /Limitations:/ When new directories are created under the tree they are
-- added to the watch on receiving the directory create event. The creation of
-- a directory and adding a watch for it is not atomic. The implementation
-- makes sure that watches are added for all directories, but the directory may
-- change before its watch is added. Entries created in the directory in that
-- period are found by a scan, see 'setScanNewDirs'. Other events in that
-- period, for example, a modification or a deletion of an entry, are lost.
-- inotify provides no way to avoid this loss.
--
-- A directory moved into the tree, or renamed within it, is handled the same
-- way, its entries are reported by the scan even if they existed before the
-- move. A move event does not show where a directory came from, a rename is
-- reported as the tree leaving and coming back in, see 'setScanNewDirs'.
--
-- = Platform differences
--
-- For reference documentation see:
--
-- * <https://man7.org/linux/man-pages/man7/inotify.7.html inotify man page>
--
-- We try to keep the macOS\/Linux\/Windows event handling APIs and defaults
-- semantically and syntactically as close as possible.
--
-- See @docs\/platform-differences.md@ in this package for the differences
-- between the Linux, macOS and Windows APIs.

module Streamly.FileSystem.Event.Linux
    (
    -- * Creating a Watch

    -- ** Configuration
      Config
    , setRecursiveMode
    , setScanNewDirs
    , setRestartOnOverflow
    , setFollowSymLinks
    , setExcludeUnlinked
    , setOneShot
    , setOnlyDir

    -- ** Events of Interest
    -- *** Root Path Events
    , setRootDeleted
    , setRootMoved
    , setRootPathEvents

    -- *** Item Level Metadata change
    , setAttrsModified

    -- *** Item Level Access
    , setAccessed
    , setOpened
    , setWriteClosed
    , setNonWriteClosed

    -- *** Item CRUD events
    , setCreated
    , setDeleted
    , setMovedFrom
    , setMovedTo
    , setModified

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

    -- ** Root Level Events
    , isRootPathEvent
    , isRootUnwatched
    , isRootDeleted
    , isRootMoved
    , isRootUnmounted

    -- ** Item Level Metadata change
    , isAttrsModified

    -- ** Item Level Access
    , isAccessed
    , isOpened
    , isWriteClosed
    , isNonWriteClosed

    -- ** Item Level CRUD events
    , isCreated
    , isDeleted
    , isMovedFrom
    , isMovedTo
    , isMoved
    , isModified

    -- ** Item Path info
    , isDir
    , isScanEvent

    -- ** Exception Conditions
    , isEventsLost
    )
where

import Streamly.Internal.FS.Event.Linux
