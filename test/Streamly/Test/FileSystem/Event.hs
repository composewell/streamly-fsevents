-- |
-- Module      : Streamly.Test.FileSystem.Event
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--

module Streamly.Test.FileSystem.Event (main) where

import qualified Streamly.Internal.FS.Event as Event
import Streamly.Test.FileSystem.Event.Common

moduleName :: String
moduleName = "FS.Event"

main :: IO ()
main = do
    let run = runTests moduleName "non-recursive" (failNested Event.watch)
    run DirType (nestedOpsTest : commonTests)
    run SymLinkOrigPath (nestedOpsTest : commonTests)
