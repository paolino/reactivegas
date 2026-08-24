{-# LANGUAGE ImportQualifiedPost #-}

module Main (
    main,
) where

import Test.Hspec (describe, hspec, it)

import Reactivegas.Core qualified as Core

main :: IO ()
main =
    hspec $
        describe "Reactivegas.Core" $
            it "declares the event-format contract version" $
                Core.coreVersion `seq`
                    True
