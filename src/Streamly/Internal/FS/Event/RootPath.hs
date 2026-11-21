-- |
-- Module      : Streamly.Internal.FS.Event.RootPath
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC
--
-- The normalization of the watch root paths on the platforms with Posix
-- paths, Linux, FreeBSD and macOS. A root is identified by its normalized
-- path, see dev/design.md.

module Streamly.Internal.FS.Event.RootPath
    ( isDotPath
    , normalizeRoot
    )
where

import Data.Functor.Identity (runIdentity)
import Streamly.Internal.FileSystem.Path (Path)

import qualified Streamly.Data.Stream as S
import qualified Streamly.Internal.FileSystem.Path as Path

-- | Whether the path is @.@.
isDotPath :: Path -> Bool
isDotPath p = Path.eqPathBytes p (Path.fromString_ ".")

-- | Remove the redundant separators and the @.@ components of a root path,
-- @x\/\/y@, @x\/.\/y@ and @.\/x\/y@ become @x\/y@. A path that is only @.@
-- components becomes @.@. A trailing separator is kept, a symbolic link
-- named with a trailing separator is resolved by the system calls. @..@
-- components are kept, removing them would change the path if a component
-- before them is a symbolic link. Symbolic links are not resolved.
--
-- Two paths that differ only in redundant separators and @.@ components
-- have the same normalized path, the roots can be compared by the bytes of
-- their normalized paths.
--
-- TODO: use Path.normalise when streamly-core 0.4 is released, see
-- dev/upstream-issues.md.
normalizeRoot :: Path -> Path
normalizeRoot path =
    case runIdentity (S.toList (Path.splitPath_ path)) of
        [] -> path
        comps -> addSep (foldl1 Path.join (dropLeadingDot comps))

    where

    dropLeadingDot (c : cs@(_ : _)) | isDotPath c = cs
    dropLeadingDot cs = cs

    addSep p
        | Path.hasTrailingSeparator path
            && not (isDotPath p)
            && not (Path.hasTrailingSeparator p) =
            Path.addTrailingSeparator p
        | otherwise = p
