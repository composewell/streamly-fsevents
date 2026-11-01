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
-- See @docs\/platform-differences.md@ in this package for the differences that
-- a portable program has to handle, for example, when a watch root is deleted
-- or renamed.
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
