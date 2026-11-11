{- |
Module      : InvariantsSpec
Description : QuickCheck properties mirroring Lean invariants
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Each property corresponds to a proven Lean theorem in
@KelGroups.Invariants@. The Lean proofs guarantee
correctness for all inputs; these QuickCheck properties
test that the Haskell implementation matches.
-}
module InvariantsSpec (spec) where

import Data.Map.Strict qualified as Map
import Generators
    ( arbitraryGroupState
    , arbitraryWithAdmin
    , gsWithAdminCount
    )
import KelGroups.Bootstrap
    ( AuthMode (..)
    , authMode
    )
import KelGroups.State
    ( GroupState (..)
    , adminCount
    , majority
    )
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
    ( Arbitrary (..)
    , NonNegative (..)
    )

-- --------------------------------------------------------
-- Specs
-- --------------------------------------------------------

spec :: Spec
spec = do
    -- ==================================================
    -- Invariant 1: Bootstrap mode ↔ zero admins
    -- Lean: bootstrap_iff_zero_admins
    -- Lean: normal_iff_positive_admins
    -- ==================================================
    describe "Invariant 1: bootstrap ↔ zero admins" $ do
        prop
            "bootstrap mode implies zero admins"
            $ do
                gs <- arbitraryGroupState
                pure $
                    (authMode gs == Bootstrap)
                        == (adminCount gs == 0)

        prop
            "normal mode implies positive admins"
            $ do
                gs <- arbitraryGroupState
                pure $
                    (authMode gs == Normal)
                        == (adminCount gs /= 0)

    -- ==================================================
    -- Invariant 2: Empty state is in bootstrap mode
    -- Lean: empty_is_bootstrap
    -- ==================================================
    describe "Invariant 2: empty state" $ do
        it "empty state is in bootstrap mode" $
            authMode (emptyState' ()) `shouldBe` Bootstrap

    -- ==================================================
    -- Invariant 3: Majority properties
    -- Lean: majority_zero, majority_one, majority_two,
    --       majority_three, majority_le, majority_pos
    -- ==================================================
    describe "Invariant 3: majority properties" $ do
        it "majority of 0 admins is 0" $
            majority (gsWithAdminCount 0)
                `shouldBe` 0

        it "majority of 1 admin is 1" $
            majority (gsWithAdminCount 1)
                `shouldBe` 1

        it "majority of 2 admins is 1" $
            majority (gsWithAdminCount 2)
                `shouldBe` 1

        it "majority of 3 admins is 2" $
            majority (gsWithAdminCount 3)
                `shouldBe` 2

        prop "majority n ≤ n" $ do
            NonNegative n <- arbitrary
            let gs = gsWithAdminCount n
            pure $ majority gs <= adminCount gs

        prop "majority of positive admin count is positive" $
            do
                NonNegative n <- arbitrary
                let gs = gsWithAdminCount (n + 1)
                pure $ majority gs > 0

    -- ==================================================
    -- Invariant 4: Removing all members → bootstrap
    -- Lean: remove_all_triggers_bootstrap
    -- ==================================================
    describe "Invariant 4: remove all → bootstrap" $ do
        it "removing all members triggers bootstrap" $
            authMode
                GroupState
                    { members = Map.empty
                    , pendingProposals = Map.empty
                    , pendingBase = Map.empty
                    , appFold = ()
                    }
                `shouldBe` Bootstrap

    -- ==================================================
    -- Invariant 5: Admin member → normal mode
    -- Lean: admin_member_means_normal
    -- ==================================================
    describe "Invariant 5: admin member → normal" $ do
        prop
            "state with at least one admin is in normal mode"
            $ do
                gs <- arbitraryWithAdmin
                pure $ authMode gs == Normal

-- --------------------------------------------------------
-- Helpers
-- --------------------------------------------------------

-- | Empty state helper.
emptyState' :: () -> GroupState ()
emptyState' _ =
    GroupState
        { members = Map.empty
        , pendingProposals = Map.empty
        , pendingBase = Map.empty
        , appFold = ()
        }
