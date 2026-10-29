-- |
-- Module      : Streamly.Internal.FS.Event.FreeBSD
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- File system event notification API for FreeBSD, using the inotify API of
-- FreeBSD 14.5, 15.0 and later. FreeBSD 14.4 and earlier do not have
-- inotify and are not supported.
--
-- The FreeBSD inotify API is compatible with the Linux inotify API, this
-- module is the same as "Streamly.Internal.FS.Event.Linux", see that module
-- for the documentation. The differences from Linux are listed in
-- "Streamly.FileSystem.Event.FreeBSD".
--
-- For reference documentation see:
--
-- * inotify(2) man page:
--   <https://man.freebsd.org/cgi/man.cgi?query=inotify&sektion=2>

#include "config.h"

#if HAVE_DECL_IN_EXCL_UNLINK
module Streamly.Internal.FS.Event.FreeBSD
    (
    -- * Subscribing to events

    -- ** Default configuration
      Config (..)
    , defaultConfig

    -- ** Watch Behavior
    , setRecursiveMode
    , setFollowSymLinks
    , setExcludeUnlinked
    , setUnwatchMoved
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

    -- Low level watch APIs
    , Watch
    , openWatch
    , closeWatch
    , watchToStream
    , addToWatch
    , removeFromWatch

    -- * Handling Events
    , Event(..)
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

    -- * Debugging
    , showEvent
    )
where

import Streamly.Internal.FS.Event.Linux
#else
#error "streamly-fsevents requires inotify, FreeBSD 14.5, 15.0 or later."
#endif
