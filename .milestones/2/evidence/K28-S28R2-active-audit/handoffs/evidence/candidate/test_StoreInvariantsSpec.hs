{- |
Module      : StoreInvariantsSpec
Description : Store-through invariant properties mirroring Lean theorems
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Each property mirrors a Lean theorem, verifying that the
invariant holds after KERI event construction → SQLite
write → read → fold. Property names match Lean theorem
names.
-}
module StoreInvariantsSpec (spec) where

import Data.IORef (newIORef)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Generators
    ( arbitraryAdminRoles
    , arbitraryNonAdminRoles
    , freshKey
    )
import KelGroups.Bootstrap (AuthMode (..), authMode)
import KelGroups.Event
    ( BaseEvent (..)
    , GroupEvent (..)
    , Proposal (..)
    )
import KelGroups.Fold (enact)
import KelGroups.State
    ( GroupState (..)
    , adminCount
    , majority
    )
import KelGroups.Store (chainTip, readState)
import KelGroups.Types (Admin (..), Role (..))
import StoreTestDSL
    ( appendTestEvent
    , arbitraryHistory
    , onReachable
    , onReachableWhere
    , onReachableWith
    , replayHistory
    , withStore
    )
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Monadic
    ( assert
    , monadicIO
    , pick
    , run
    )

-- --------------------------------------------------------
-- Invariants.lean mirrors (6 properties)
-- --------------------------------------------------------

spec :: Spec
spec = describe "Store-through invariants (Lean mirrors)" $ do
    -- Lean: bootstrap_iff_zero_admins
    prop "bootstrap_iff_zero_admins" $
        onReachable $ \gs ->
            (authMode gs == Bootstrap)
                == (adminCount gs == 0)

    -- Lean: normal_iff_positive_admins
    prop "normal_iff_positive_admins" $
        onReachable $ \gs ->
            (authMode gs == Normal)
                == (adminCount gs > 0)

    -- Lean: empty_is_bootstrap
    it "empty_is_bootstrap" $ do
        gs <- replayHistory []
        authMode gs `shouldBe` Bootstrap

    -- Lean: majority_le
    prop "majority_le" $
        onReachable $ \gs ->
            majority gs <= adminCount gs

    -- Lean: majority_pos
    prop "majority_pos" $
        onReachableWhere
            (\gs -> adminCount gs > 0)
            (\gs -> majority gs > 0)

    -- Lean: admin_member_means_normal
    prop "admin_member_means_normal" $
        onReachableWhere
            (\gs -> adminCount gs > 0)
            (\gs -> authMode gs == Normal)

    -- --------------------------------------------------------
    -- TransitionInvariants.lean mirrors (5 properties)
    -- --------------------------------------------------------

    -- Lean: enact_introduce_admin_exits_bootstrap
    prop "enact_introduce_admin_exits_bootstrap" $
        onReachableWith arbitraryHistory $ \gs -> do
            key <- freshKey gs
            roles <- arbitraryAdminRoles
            let email = key <> "@test.example"
                gs' = enact gs $ IntroduceMember key email roles
            pure $ adminCount gs' > 0

    -- Lean: enact_introduce_admin_count
    prop "enact_introduce_admin_count" $
        onReachableWith arbitraryHistory $ \gs -> do
            key <- freshKey gs
            roles <- arbitraryAdminRoles
            let email = key <> "@test.example"
                gs' = enact gs $ IntroduceMember key email roles
            pure $
                adminCount gs' == adminCount gs + 1

    -- Lean: enact_introduce_nonadmin_count
    prop "enact_introduce_nonadmin_count" $
        onReachableWith arbitraryHistory $ \gs -> do
            key <- freshKey gs
            roles <- arbitraryNonAdminRoles
            let email = key <> "@test.example"
                gs' = enact gs $ IntroduceMember key email roles
            pure $
                adminCount gs' == adminCount gs

    -- Lean: enact_preserves_pendingProposals
    prop "enact_preserves_pendingProposals" $
        onReachableWith arbitraryHistory $ \gs -> do
            key <- freshKey gs
            roles <- arbitraryAdminRoles
            let email = key <> "@test.example"
                gs' = enact gs $ IntroduceMember key email roles
            pure $
                pendingProposals gs'
                    == pendingProposals gs

    -- Lean: enact_remove_preserves_normal
    prop "enact_remove_preserves_normal" $
        onReachableWhere
            (\gs -> adminCount gs >= 2)
            ( \gs ->
                let gs' = enact gs $ RemoveMember "any"
                in  adminCount gs' >= 1
            )

    -- --------------------------------------------------------
    -- Store-specific transition properties
    -- --------------------------------------------------------

    -- Bootstrap proposal is immediately enacted
    prop "bootstrap_proposal_immediately_enacted" $
        monadicIO $ do
            key <- pick $ freshKey (emptyGS ())
            roles <- pick arbitraryAdminRoles
            let email = key <> "@test.example"
            gs <- run $ withStore $ \store -> do
                tip <- chainTip store
                tipRef <- newIORef tip
                StoreTestDSL.appendTestEvent
                    store
                    tipRef
                    ( "bootstrap"
                    , Base $
                        Propose $
                            IntroduceMember key email roles
                    )
                readState store
            assert $ Map.null (pendingProposals gs)

    -- Single admin proposal is enacted immediately
    prop "single_admin_proposal_enacted" $
        monadicIO $ do
            key <- pick $ freshKey (emptyGS ())
            roles <- pick arbitraryAdminRoles
            let email = key <> "@test.example"
            gs <- run $ withStore $ \store -> do
                tip <- chainTip store
                tipRef <- newIORef tip
                StoreTestDSL.appendTestEvent
                    store
                    tipRef
                    ( "bootstrap"
                    , Base $
                        Propose $
                            IntroduceMember
                                "founder"
                                "founder@test.example"
                                ( Set.singleton
                                    (AdminRole PublicAdmin)
                                )
                    )
                StoreTestDSL.appendTestEvent
                    store
                    tipRef
                    ( "founder"
                    , Base $
                        Propose $
                            IntroduceMember key email roles
                    )
                readState store
            assert $ Map.null (pendingProposals gs)

-- | Empty group state helper.
emptyGS :: a -> GroupState a
emptyGS a =
    GroupState
        { members = Map.empty
        , pendingProposals = Map.empty
        , pendingBase = Map.empty
        , appFold = a
        }
