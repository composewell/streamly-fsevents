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
-- Recursive directory watching is native on macOS and Windows. On Linux,
-- inotify watches only one directory, recursive watching is emulated in user
-- space by adding a watch for each new directory, and events in a new
-- directory may be lost before its watch is added. See 'setRecursiveMode'.
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
    )
where

import Streamly.Internal.FS.Event
