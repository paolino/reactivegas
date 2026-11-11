{- |
Module      : ValidateSpec
Description : QuickCheck properties mirroring Lean validation invariants
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Each property corresponds to a proven Lean theorem in
@KelGroups.ValidateInvariants@ or tests Haskell-specific
validation involving @GroupConfig@.
-}
module ValidateSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Generators
    ( adminKey
    , arbitraryAdminRoles
    , arbitraryKey
    , arbitraryNonAdminRoles
    , arbitraryWithAdmin
    , arbitraryWithNonAdmin
    , arbitraryWithPending
    , existingKey
    , freshKey
    , nonAdminMemberKey
    , trivialConfig
    )
import KelGroups.Event
    ( BaseEvent (..)
    , GroupEvent (..)
    , Proposal (..)
    )
import KelGroups.State
    ( GroupState (..)
    , PendingProposal (..)
    )
import KelGroups.Types (Admin (..), Role (..))
import KelGroups.Validate
    ( ValidationError (..)
    , validateEvent
    )
import Test.Hspec (Spec, describe, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (elements)

spec :: Spec
spec = do
    -- ==================================================
    -- Bootstrap mode
    -- Lean: bootstrap_accepts_admin_intro
    -- Lean: bootstrap_rejects_nonadmin_intro
    -- Lean: bootstrap_rejects_remove
    -- Lean: bootstrap_rejects_changeRoles
    -- ==================================================
    describe "Bootstrap validation" $ do
        prop "bootstrap accepts admin intro" $ do
            key <- arbitraryKey
            roles <- arbitraryAdminRoles
            let email = key <> "@test.example"
            pure $
                validateEvent
                    trivialConfig
                    emptyGS
                    key
                    (Base $ Propose $ IntroduceMember key email roles)
                    `shouldBe` Right ()

        prop "bootstrap rejects non-admin intro" $ do
            key <- arbitraryKey
            roles <- arbitraryNonAdminRoles
            let email = key <> "@test.example"
            pure $
                validateEvent
                    trivialConfig
                    emptyGS
                    key
                    (Base $ Propose $ IntroduceMember key email roles)
                    `shouldBe` Left BootstrapRequiresAdmin

        prop "bootstrap rejects remove" $ do
            key <- arbitraryKey
            target <- arbitraryKey
            pure $
                validateEvent
                    trivialConfig
                    emptyGS
                    key
                    (Base $ Propose $ RemoveMember target)
                    `shouldBe` Left BootstrapRequiresAdmin

        prop "bootstrap rejects changeRoles" $ do
            key <- arbitraryKey
            target <- arbitraryKey
            roles <- arbitraryAdminRoles
            pure $
                validateEvent
                    trivialConfig
                    emptyGS
                    key
                    (Base $ Propose $ ChangeRoles target roles)
                    `shouldBe` Left BootstrapRequiresAdmin

    -- ==================================================
    -- Normal mode: auth
    -- Lean: normal_proposal_requires_admin
    -- ==================================================
    describe "Normal mode: auth" $ do
        prop "non-member proposal rejected" $ do
            gs <- arbitraryWithAdmin
            signer <- freshKey gs
            roles <- arbitraryAdminRoles
            let email = signer <> "@test.example"
                evt =
                    Base $
                        Propose $
                            IntroduceMember signer email roles
            pure $
                validateEvent
                    trivialConfig
                    gs
                    signer
                    evt
                    `shouldBe` Left (NotAnAdmin signer)

        prop "non-admin proposal rejected" $ do
            gs <- arbitraryWithNonAdmin
            signer <- nonAdminMemberKey gs
            target <- freshKey gs
            let email = target <> "@test.example"
                evt =
                    Base $
                        Propose $
                            IntroduceMember
                                target
                                email
                                ( Set.singleton
                                    (AdminRole PublicAdmin)
                                )
            pure $
                validateEvent
                    trivialConfig
                    gs
                    signer
                    evt
                    `shouldBe` Left (NotAnAdmin signer)

        prop "non-member app event rejected" $ do
            gs <- arbitraryWithAdmin
            signer <- freshKey gs
            pure $
                validateEvent
                    trivialConfig
                    gs
                    signer
                    (App ())
                    `shouldBe` Left (NotAMember signer)

        prop "member app event accepted" $ do
            gs <- arbitraryWithAdmin
            signer <- existingKey gs
            pure $
                validateEvent
                    trivialConfig
                    gs
                    signer
                    (App ())
                    `shouldBe` Right ()

    -- ==================================================
    -- Normal mode: proposal constraints
    -- Lean: normal_introduce_existing_rejected
    -- Lean: normal_remove_nonmember_rejected
    -- Lean: normal_valid_admin_proposal_accepted
    -- ==================================================
    describe "Normal mode: proposal constraints" $ do
        prop "introduce existing rejected" $ do
            gs <- arbitraryWithAdmin
            signer <- adminKey gs
            target <- existingKey gs
            let email = target <> "@test.example"
                evt =
                    Base $
                        Propose $
                            IntroduceMember
                                target
                                email
                                ( Set.singleton
                                    (AdminRole PublicAdmin)
                                )
            pure $
                validateEvent
                    trivialConfig
                    gs
                    signer
                    evt
                    `shouldBe` Left
                        (MemberAlreadyExists target)

        prop "remove nonmember rejected" $ do
            gs <- arbitraryWithAdmin
            signer <- adminKey gs
            target <- freshKey gs
            let evt =
                    Base $
                        Propose $
                            RemoveMember target
            pure $
                validateEvent
                    trivialConfig
                    gs
                    signer
                    evt
                    `shouldBe` Left
                        (MemberNotFound target)

        prop "valid admin proposal accepted" $ do
            gs <- arbitraryWithAdmin
            signer <- adminKey gs
            target <- freshKey gs
            roles <- arbitraryAdminRoles
            let email = target <> "@test.example"
                evt =
                    Base $
                        Propose $
                            IntroduceMember target email roles
            pure $
                validateEvent
                    trivialConfig
                    gs
                    signer
                    evt
                    `shouldBe` Right ()

    -- ==================================================
    -- Normal mode: approval constraints
    -- Lean: normal_approve_missing_rejected
    -- Lean: normal_double_approve_rejected
    -- ==================================================
    describe "Normal mode: approval constraints" $ do
        prop "approve missing rejected" $ do
            gs <- arbitraryWithAdmin
            signer <- adminKey gs
            pid <- freshKey gs
            let evt = Base $ Approve pid
            pure $
                validateEvent
                    trivialConfig
                    gs
                    signer
                    evt
                    `shouldBe` Left
                        (ProposalNotFound pid)

        prop "double approve rejected" $ do
            gs <- arbitraryWithPending
            signer <- adminKey gs
            pid <-
                elements $
                    Map.keys (pendingProposals gs)
            let gs' =
                    gs
                        { pendingProposals =
                            Map.adjust
                                ( \pp ->
                                    pp
                                        { approvals =
                                            Set.insert
                                                signer
                                                (approvals pp)
                                        }
                                )
                                pid
                                (pendingProposals gs)
                        }
                evt = Base $ Approve pid
            pure $
                validateEvent
                    trivialConfig
                    gs'
                    signer
                    evt
                    `shouldBe` Left
                        (AlreadyApproved signer pid)

        prop "valid approval accepted" $ do
            gs <- arbitraryWithPending
            signer <- adminKey gs
            pid <-
                elements $
                    Map.keys (pendingProposals gs)
            let gs' =
                    gs
                        { pendingProposals =
                            Map.adjust
                                ( \pp ->
                                    pp
                                        { approvals =
                                            Set.delete
                                                signer
                                                (approvals pp)
                                        }
                                )
                                pid
                                (pendingProposals gs)
                        }
                evt = Base $ Approve pid
            pure $
                validateEvent
                    trivialConfig
                    gs'
                    signer
                    evt
                    `shouldBe` Right ()

-- | Empty group state for bootstrap tests.
emptyGS :: GroupState ()
emptyGS =
    GroupState
        { members = Map.empty
        , pendingProposals = Map.empty
        , pendingBase = Map.empty
        , appFold = ()
        }
