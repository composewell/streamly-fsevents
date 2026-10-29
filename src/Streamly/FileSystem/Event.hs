-- |
-- Module      : Streamly.FileSystem.Event
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : released
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
-- * "Streamly.FileSystem.Event.Darwin"
-- * "Streamly.FileSystem.Event.Linux"
-- * "Streamly.FileSystem.Event.Windows"
--
-- The 'Config' and 'Event' types of this module are the same as those of the
-- platform specific module, therefore, the platform specific settings and
-- event predicates can be used along with this module. For example, on Linux:
--
-- @
-- import qualified Streamly.FileSystem.Event as Event
-- import qualified Streamly.FileSystem.Event.Linux as Linux
--
-- Event.watchWith (Linux.setFollowSymLinks False) paths
--     & Stream.filter Linux.isDir
-- @
--
-- A platform specific module is available only on its platform, the Haddock
-- documentation of the module is built only on that platform.

module Streamly.FileSystem.Event
    (
    -- Design note: this module exports only the APIs available on all
    -- platforms. Each platform specific module exports the complete API for
    -- its platform, including these portable APIs. See "Portable vs Platform
    -- Specific Modules" in docs/Developer/module-organization.md in the
    -- streamly repository for the reasons.

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
