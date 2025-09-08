-- |
-- Module      : Streamly.FileSystem.Event
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC
--
-- File system event notification API portable across Linux, macOS and Windows
-- platforms.
--
-- Note that recursive directory tree watch does not work reliably on Linux
-- (see notes in the Linux module), therefore, recursive watch API is not
-- provided in this module. However, you can use it from the platform specific
-- modules.
--
-- For platform specific APIs please see the following modules:
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
