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
--
-- = Event Coalescing
--
-- More than one event predicate can be true for the same 'Event', because the
-- operating system can coalesce several changes of a path into one event. For
-- example, on macOS @touch x; rm x@ can report one event for which both
-- 'isCreated' and 'isDeleted' are true. The event does not give the order of
-- the changes, and @x@ may or may not exist after it.
--
-- A program must test every predicate it is interested in. A chain that stops
-- at the first predicate that is true misses the other changes reported by the
-- event:
--
-- @
-- handle ev = do
--     when ('isCreated' ev) (onCreate ev)
--     when ('isDeleted' ev) (onDelete ev)
-- @
--
-- A program that needs the current state of a path must read it from the file
-- system. The number of events cannot be used to count changes on any
-- platform. See @docs\/platform-differences.md@ in this package for what is
-- coalesced on each platform.

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
