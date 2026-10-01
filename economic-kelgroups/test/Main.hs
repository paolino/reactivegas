{- | Permanent money custody suite: direct behavior, frozen-corpus replay
and compiled guard-mutant controls. The gate markers are printed only
after the named groups have actually passed; any failure exits nonzero.
-}
module Main (main) where

import Control.Monad (unless, when)
import System.Exit (exitFailure)
import Test.Hspec.Core.Runner (
    Summary (..),
    hspecResult,
 )

import CorpusSpec qualified
import MoneyCustodySpec qualified
import MutationSpec qualified

main :: IO ()
main = do
    money <- hspecResult MoneyCustodySpec.spec
    corpus <- hspecResult CorpusSpec.spec
    mutation <- hspecResult MutationSpec.spec
    let sane s = summaryFailures s == 0 && summaryExamples s > 0
        conformance = sane money && sane corpus
        controls = sane mutation
    putStrLn $
        "money-custody-summary conformance-examples="
            <> show (summaryExamples money + summaryExamples corpus)
            <> " control-examples="
            <> show (summaryExamples mutation)
            <> " failures="
            <> show (summaryFailures money + summaryFailures corpus + summaryFailures mutation)
    when conformance $ putStrLn "MONEY-CUSTODY-CONFORMANCE-OK"
    when controls $ putStrLn "MONEY-CUSTODY-CONTROLS-OK"
    unless (conformance && controls) exitFailure
