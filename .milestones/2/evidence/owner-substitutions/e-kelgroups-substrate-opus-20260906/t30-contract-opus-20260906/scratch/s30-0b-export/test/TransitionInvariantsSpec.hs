{- |
Module      : TransitionInvariantsSpec
Description : QuickCheck properties mirroring Lean transition invariants
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Each property corresponds to a proven Lean theorem in
@KelGroups.TransitionInvariants@. The Lean proofs guarantee
correctness for all inputs; these QuickCheck properties
test that the Haskell implementation matches.
-}
module TransitionInvariantsSpec (spec) where

import Data.Map.Strict qualified as Map
import Generators
    ( arbitraryAdminRoles
    , arbitraryGroupState
    , arbitraryKey
    , arbitraryNonAdminRoles
    , arbitraryWithTwoAdmins
    , gsWithAdminCount
    )
import KelGroups.Event (Proposal (..))
import KelGroups.Fold
    ( AppFold
    , applyPropose
    , enact
    , foldGroup
    )
import KelGroups.State
    ( GroupState (..)
    , adminCount
    , emptyState
    )
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
    ( Arbitrary (..)
    , elements
    , suchThat
    )

-- | Trivial app fold for testing.
trivialAppFold :: AppFold ()
trivialAppFold _ _ = ()

-- --------------------------------------------------------
-- Specs
-- --------------------------------------------------------

spec :: Spec
spec = do
    -- ==================================================
    -- Tier 1: Straightforward invariants
    -- ==================================================
    describe
        "Tier 1: enact_introduce_admin_exits_bootstrap"
        $ do
            prop
                "introducing admin makes adminCount > 0"
                $ do
                    gs <- arbitraryGroupState
                    key <- arbitraryKey
                    roles <- arbitraryAdminRoles
                    let gs' =
                            enact gs $
                                IntroduceMember
                                    key
                                    (key <> "@test.example")
                                    roles
                    pure $ adminCount gs' > 0

    describe "Tier 1: enact_introduce_admin_count" $ do
        prop
            "introducing admin increases adminCount by 1"
            $ do
                gs <- arbitraryGroupState
                key <-
                    arbitraryKey `suchThat` \k ->
                        not $ Map.member k (members gs)
                roles <- arbitraryAdminRoles
                let email = key <> "@test.example"
                    gs' =
                        enact gs $
                            IntroduceMember key email roles
                pure $
                    adminCount gs' == adminCount gs + 1

    describe
        "Tier 1: enact_introduce_nonadmin_count"
        $ do
            prop
                "introducing non-admin preserves adminCount"
                $ do
                    gs <- arbitraryGroupState
                    key <-
                        arbitraryKey `suchThat` \k ->
                            not $ Map.member k (members gs)
                    roles <- arbitraryNonAdminRoles
                    let gs' =
                            enact gs $
                                IntroduceMember
                                    key
                                    (key <> "@test.example")
                                    roles
                    pure $
                        adminCount gs' == adminCount gs

    describe
        "Tier 1: enact_preserves_pendingProposals"
        $ do
            prop
                "enact only touches members"
                $ do
                    gs <- arbitraryGroupState
                    proposal <- arbitrary
                    let gs' = enact gs proposal
                    pure $
                        pendingProposals gs'
                            == pendingProposals gs

    describe "Tier 1: foldGroup_nil" $ do
        it "folding empty list yields emptyState" $
            foldGroup trivialAppFold () []
                `shouldBe` emptyState ()

    -- ==================================================
    -- Tier 2: Majority + tryEnact
    -- ==================================================
    describe
        "Tier 2: bootstrap_proposal_immediately_enacted"
        $ do
            prop
                "bootstrap proposal has no pending after apply"
                $ do
                    signer <- arbitraryKey
                    proposal <- arbitrary
                    let gs' =
                            applyPropose
                                (emptyState ())
                                signer
                                proposal
                    pure $
                        Map.null (pendingProposals gs')

    describe
        "Tier 2: single_admin_proposal_enacted"
        $ do
            prop
                "single admin proposal is enacted immediately"
                $ do
                    let gs = gsWithAdminCount 1
                    signer <-
                        elements $
                            Map.keys (members gs)
                    proposal <- arbitrary
                    let gs' = applyPropose gs signer proposal
                    pure $
                        members gs'
                            == members
                                (enact gs proposal)

    -- ==================================================
    -- Tier 3: List induction — eraseP + filter
    -- ==================================================
    describe
        "Tier 3: enact_remove_preserves_normal"
        $ do
            prop
                "adminCount >= 2 and remove keeps adminCount >= 1"
                $ do
                    gs <- arbitraryWithTwoAdmins
                    key <- arbitraryKey
                    let gs' =
                            enact gs $ RemoveMember key
                    pure $ adminCount gs' >= 1
