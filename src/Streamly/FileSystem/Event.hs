-- |
-- Module      : Streamly.FileSystem.Event
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC
--
-- Portable file system event notification API for Linux, macOS, and Windows.
--
-- This module provides a common interface for file system event monitoring
-- across major platforms. In most cases, behavior is consistent across
-- operating systems, but certain platform-specific idiosyncrasies remain.
--
-- Compared to Linux inotify, the kernel event notification APIs on macOS and
-- Windows are more robust. In particular, recursive directory watching on
-- Linux must be emulated in user space, which introduces unavoidable race
-- conditions.
--
-- For this reason, this portable module does not expose a recursive watch API.
-- If you need recursive monitoring, you can use the platform-specific modules,
-- which provide their own recursive APIs. Recursive watches for macOS and
-- windows are robust.
--
-- On Linux, watches are tied to file handles. As a result:
--
-- * If a watched file is deleted and recreated, the new file is not watched.
-- * If a watched file is renamed, we can continue to monitor it.
--
-- On macOS, by contrast, watches are path-based.
--
-- For platform-specific APIs, see:
--
-- * "Streamly.Internal.FS.Event.Darwin"
-- * "Streamly.Internal.FS.Event.Linux"
-- * "Streamly.Internal.FS.Event.Windows"

module Streamly.FileSystem.Event
    (
    -- * Creating a Watch

      watch
    -- , watchRecursive

    -- * Handling Events
    , Event
    , getAbsPath

    -- ** Item CRUD events
    , isCreated
    , isDeleted
    , isMoved
    , isModified

    -- ** Exception Conditions
    , isEventsLost
    )
where

import Streamly.Internal.FS.Event
