{- |
Module      : FoldSpec
Description : QuickCheck properties mirroring Lean fold invariants
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Each property corresponds to a proven Lean theorem in
@KelGroups.FoldInvariants@. Tests app/base separation
and enact effects on member counts.
-}
module FoldSpec (spec) where

import Data.Map.Strict qualified as Map
import Generators
    ( arbitraryGroupState
    , arbitraryKey
    , arbitraryWithAdmin
    , freshKey
    )
import KelGroups.Event
    ( BaseEvent (..)
    , GroupEvent (..)
    , Proposal (..)
    )
import KelGroups.Fold
    ( AppFold
    , applyEvent
    , enact
    )
import KelGroups.State (GroupState (..))
import Test.Hspec (Spec, describe, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..))

-- | Trivial app fold for testing.
trivialAppFold :: AppFold ()
trivialAppFold _ _ = ()

spec :: Spec
spec = do
    -- ==================================================
    -- Lean: applyEvent_app_preserves_members
    -- Lean: applyEvent_app_preserves_pendingProposals
    -- ==================================================
    describe "App/base separation" $ do
        prop "app event preserves members" $ do
            gs <- arbitraryGroupState
            signer <- arbitraryKey
            let gs' =
                    applyEvent
                        trivialAppFold
                        gs
                        (signer, App ())
            pure $
                members gs' `shouldBe` members gs

        prop
            "app event preserves pendingProposals"
            $ do
                gs <- arbitraryGroupState
                signer <- arbitraryKey
                let gs' =
                        applyEvent
                            trivialAppFold
                            gs
                            (signer, App ())
                pure $
                    pendingProposals gs'
                        `shouldBe` pendingProposals gs

    -- ==================================================
    -- Lean: applyApprove_nonexistent_is_identity
    -- ==================================================
    describe "Approve identity" $ do
        prop "approve nonexistent is identity" $ do
            gs <- arbitraryGroupState
            signer <- arbitraryKey
            pid <- freshKey gs
            let gs' =
                    applyEvent
                        trivialAppFold
                        gs
                        (signer, Base $ Approve pid)
            pure $ gs' `shouldBe` gs

    -- ==================================================
    -- Lean: enact_changeRoles_preserves_member_count
    -- Lean: enact_introduce_increases_member_count
    -- ==================================================
    describe "Enact member count effects" $ do
        prop "changeRoles preserves member count" $ do
            gs <- arbitraryWithAdmin
            key <- arbitraryKey
            roles <- arbitrary
            let gs' = enact gs $ ChangeRoles key roles
            pure $
                Map.size (members gs')
                    `shouldBe` Map.size (members gs)

        prop "introduce increases member count" $ do
            gs <- arbitraryGroupState
            key <- freshKey gs
            roles <- arbitrary
            let gs' =
                    enact gs $
                        IntroduceMember
                            key
                            (key <> "@test.example")
                            roles
            pure $
                Map.size (members gs')
                    `shouldBe` Map.size (members gs)
                        + 1
