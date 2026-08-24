{-# LANGUAGE ImportQualifiedPost #-}

module Main (
    main,
) where

import Blake3Spec qualified
import Ed25519Spec qualified
import EnvelopeSpec qualified
import Reactivegas.Core qualified as Core
import Test.Hspec (describe, hspec, it)
import VerifySpec qualified

main :: IO ()
main = hspec $ do
    describe "Reactivegas.Core" $
        it "declares the event-format contract version" $
            Core.coreVersion `seq`
                True
    describe "Blake3" Blake3Spec.spec
    describe "Ed25519" Ed25519Spec.spec
    describe "Envelope" EnvelopeSpec.spec
    describe "Verify" VerifySpec.spec
