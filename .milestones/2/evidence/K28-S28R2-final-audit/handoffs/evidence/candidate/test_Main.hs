{- |
Module      : Main
Description : Test suite entry point
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0
-}
module Main (main) where

import E2ESpec qualified
import FoldSpec qualified
import InvariantsSpec qualified
import JwkSpec qualified
import MultiClientSpec qualified
import S28AppApiSpec qualified
import ServerSpec qualified
import StoreInvariantsSpec qualified
import StoreSpec qualified
import Test.Hspec (hspec)
import TransitionInvariantsSpec qualified
import ValidateSpec qualified

main :: IO ()
main = hspec $ do
    InvariantsSpec.spec
    TransitionInvariantsSpec.spec
    FoldSpec.spec
    ValidateSpec.spec
    JwkSpec.spec
    StoreSpec.spec
    StoreInvariantsSpec.spec
    ServerSpec.spec
    E2ESpec.spec
    MultiClientSpec.spec
    S28AppApiSpec.spec
