-- |
-- Module      : Streamly.FileSystem.Event.Darwin
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC
--
-- File system event notification API for macOS, using the FSEvents API. See
-- "Streamly.FileSystem.Event" for the portable API.
--
-- The 'Config' and 'Event' types are the same as in
-- "Streamly.FileSystem.Event", the functions in this module can be used with
-- the portable API as well.
--
-- For reference documentation see:
--
-- * "<https://developer.apple.com/library/archive/documentation/Darwin/Conceptual/FSEvents_ProgGuide/UsingtheFSEventsFramework/UsingtheFSEventsFramework.html Apple FS Events Programming Guide>"
-- * "<https://developer.apple.com/documentation/coreservices/file_system_events?language=objc Apple FS Events Reference>"

#include <config.h>

#if HAVE_DECL_KFSEVENTSTREAMCREATEFLAGFILEEVENTS
module Streamly.FileSystem.Event.Darwin
    (
    -- * Creating a Watch

    -- ** Configuration
      Config
    , setRecursiveMode
    , BatchInfo (..)
    , setEventBatching

    -- ** Events of Interest
    , setRootPathEvents
    , setFileEvents
    , setIgnoreSelf
    , setFullHistory
    , setAllEvents

    -- ** Watch APIs
    , watch
    , watchRecursive
    , watchWith

    -- * Handling Events
    , Event
    , getEventId
    , getRoot
    , getRelPath
    , getAbsPath

    -- ** Root Level Events
    -- | Events that belong to the root path as a whole and not to specific
    -- items contained in it.
    , isMounted
    , isUnmounted
    , isHistoryDone
    , isRootPathEvent

    -- ** Item Level Metadata change
    , isAttrsModified
    , isSecurityModified
    , isXAttrsModified
    , isFinderInfoModified

    -- ** Item Level CRUD events
    , isCreated
    , isDeleted
    , isMoved
    , isModified
    , isCloned

    -- ** Item Path info
    , isDir
    , isFile
    , isSymLink
    , isHardLink
    , isLastHardLink

    -- ** Exception Conditions
    , isEventIdWrapped
    , isEventsLost
    , isKernelDropped
    , isUserDropped
    )
where

import Streamly.Internal.FS.Event.Darwin
#else
module Streamly.FileSystem.Event.Darwin () where
#endif
