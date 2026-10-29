-- |
-- Module      : Streamly.FileSystem.Event.FreeBSD
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- File system event notification API for FreeBSD, using the inotify API of
-- FreeBSD 14.5, 15.0 and later. FreeBSD 14.4 and earlier do not have
-- inotify, the package does not build on them. See
-- "Streamly.FileSystem.Event" for the portable API.
--
-- The 'Config' and 'Event' types are the same as in
-- "Streamly.FileSystem.Event", the functions in this module can be used with
-- the portable API as well.
--
-- The FreeBSD inotify API is compatible with the Linux inotify API. This
-- module exports the same functions as "Streamly.FileSystem.Event.Linux",
-- with the same defaults. The differences from Linux are:
--
-- * The events of a file are never reported after it is unlinked,
-- 'setExcludeUnlinked' 'False' has no effect.
-- * A link count change, by unlink(2), link(2) or a rename over an existing
-- file, reports 'isAttrsModified' only to a watch on the file itself, not to
-- a watch on its directory. rmdir(2) does not report 'isAttrsModified'.
-- * An access to a file through any of its hard links generates an event
-- in a watched directory that has a link to the file. A watched directory
-- that has two links to the same file reports each event once for each
-- link.
-- * Adding a watch on a directory reads the directory and looks up each of
-- its entries. The time taken is proportional to the number of entries. The
-- read generates an 'isAccessed' event for the directory. A directory that
-- is readable but not searchable cannot be watched, the watch fails with
-- @EACCES@.
-- * devfs does not report events. File systems that do not use the kernel
-- name cache, for example tmpfs mounted with @nonc@, may not report all the
-- events.
--
-- The limits are the sysctl variables @vfs.inotify.max_user_instances@,
-- @vfs.inotify.max_user_watches@ and @vfs.inotify.max_queued_events@.
--
-- For reference documentation see:
--
-- * inotify(2) man page:
--   <https://man.freebsd.org/cgi/man.cgi?query=inotify&sektion=2>

#include "config.h"

#if HAVE_DECL_IN_EXCL_UNLINK
module Streamly.FileSystem.Event.FreeBSD
    (
    -- * Creating a Watch

    -- ** Configuration
      Config
    , setRecursiveMode
    , setFollowSymLinks
    , setExcludeUnlinked
    , setOneShot
    , setOnlyDir
    , WhenExists (..)
    , setWhenExists

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

    -- ** Exception Conditions
    , isEventsLost
    )
where

import Streamly.Internal.FS.Event.FreeBSD
#else
#error "streamly-fsevents requires inotify, FreeBSD 14.5, 15.0 or later."
#endif
