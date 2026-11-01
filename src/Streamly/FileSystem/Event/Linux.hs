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
-- An inotify watch is attached to the inode of a watch root, not to its path:
--
-- * If a watch root is deleted, its watch ends with an 'isRootUnwatched'
-- event. A new object created at the same path is not watched.
-- * If a watch root is renamed, its watch continues and the events are
-- reported with the old path. In recursive mode, a directory created in the
-- renamed root is not watched, because the old path of the directory does not
-- exist.
--
-- Use 'setRootPathEvents' to receive the 'isRootDeleted' and 'isRootMoved'
-- events. Objects inside a watched directory do not have these limitations. A
-- file deleted and created again in a watched directory reports 'isDeleted'
-- and 'isCreated', and its later events are reported.
--
-- For reference documentation see:
--
-- * <https://man7.org/linux/man-pages/man7/inotify.7.html inotify man page>

#include "config.h"

#if HAVE_DECL_IN_EXCL_UNLINK
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
    , Cookie
    , getCookie

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
#else
module Streamly.FileSystem.Event.Linux () where
#endif
