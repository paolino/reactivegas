{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : Generators
Description : Shared QuickCheck generators and orphan instances
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Shared generators for group states, members, roles, and
proposals. Uses real CESR-encoded Ed25519 public keys
via keri-hs for all key material.
-}
module Generators
    ( arbitraryKey
    , freshKey
    , existingKey
    , adminKey
    , nonAdminMemberKey
    , arbitraryGroupState
    , arbitraryWithAdmin
    , arbitraryWithTwoAdmins
    , arbitraryWithNonAdmin
    , arbitraryWithPending
    , arbitraryAdminRoles
    , arbitraryNonAdminRoles
    , gsWithAdminCount
    , trivialConfig
    , makeCesrKey
    ) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text, pack)
import Data.Word (Word8)
import KelGroups.Event (Proposal (..))
import KelGroups.State
    ( GroupState (..)
    , PendingProposal (..)
    , adminCount
    , isAdmin
    )
import KelGroups.Types
    ( Admin (..)
    , GroupConfig (..)
    , Member (..)
    , Role (..)
    )
import Keri.Cesr.DerivationCode (DerivationCode (..))
import Keri.Cesr.Encode qualified as Cesr
import Keri.Cesr.Primitive (Primitive (..))
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , chooseInt
    , elements
    , listOf
    , oneof
    , suchThat
    , vectorOf
    )

-- --------------------------------------------------------
-- CESR key helpers
-- --------------------------------------------------------

{- | Build a deterministic CESR-encoded Ed25519 public
key from a seed byte. Produces 32 bytes padded with
the seed, then CESR-encodes with code 'D'.
-}
makeCesrKey :: Word8 -> Text
makeCesrKey seed =
    Cesr.encode
        Primitive
            { code = Ed25519PubKey
            , raw = BS.pack (replicate 32 seed)
            }

-- --------------------------------------------------------
-- Arbitrary instances
-- --------------------------------------------------------

instance Arbitrary Admin where
    arbitrary :: Gen Admin
    arbitrary = elements [PublicAdmin, PrivateAdmin]

instance Arbitrary Role where
    arbitrary :: Gen Role
    arbitrary =
        oneof
            [ AdminRole <$> arbitrary
            , AppRole . pack . ("role" <>) . show
                <$> chooseInt (0, 9)
            ]

instance Arbitrary Member where
    arbitrary :: Gen Member
    arbitrary = do
        key <- arbitraryKey
        let email = key <> "@test.example"
        roles <- Set.fromList <$> listOf arbitrary
        pure
            Member
                { memberKey = key
                , memberEmail = email
                , memberRoles = roles
                }

instance Arbitrary Proposal where
    arbitrary :: Gen Proposal
    arbitrary = do
        key <- arbitraryKey
        let email = key <> "@test.example"
        oneof
            [ IntroduceMember key email . Set.fromList
                <$> listOf arbitrary
            , pure $ RemoveMember key
            , ChangeRoles key . Set.fromList
                <$> listOf arbitrary
            ]

-- --------------------------------------------------------
-- Key generators
-- --------------------------------------------------------

{- | Random CESR-encoded Ed25519 public key from a
pool of 100 deterministic keys.
-}
arbitraryKey :: Gen Text
arbitraryKey =
    makeCesrKey . fromIntegral <$> chooseInt (0, 99)

-- | A key not present in the given group state.
freshKey :: GroupState () -> Gen Text
freshKey gs =
    arbitraryKey `suchThat` \k ->
        not $ Map.member k (members gs)

-- --------------------------------------------------------
-- GroupState generators
-- --------------------------------------------------------

-- | Generate a GroupState () with random members.
arbitraryGroupState :: Gen (GroupState ())
arbitraryGroupState = do
    n <- chooseInt (0, 10)
    ms <- vectorOf n arbitrary
    let memberMap =
            Map.fromList
                [(memberKey m, m) | m <- ms]
    pure
        GroupState
            { members = memberMap
            , pendingProposals = Map.empty
            , pendingBase = Map.empty
            , appFold = ()
            }

-- | Generate a GroupState with at least one admin.
arbitraryWithAdmin :: Gen (GroupState ())
arbitraryWithAdmin =
    arbitraryGroupState `suchThat` \gs ->
        adminCount gs > 0

-- | Generate a GroupState with at least 2 admins.
arbitraryWithTwoAdmins :: Gen (GroupState ())
arbitraryWithTwoAdmins =
    arbitraryGroupState `suchThat` \gs ->
        adminCount gs >= 2

-- | An admin-bearing role set (always contains an AdminRole).
arbitraryAdminRoles :: Gen (Set.Set Role)
arbitraryAdminRoles = do
    adm <- arbitrary
    extras <- listOf arbitrary
    pure $ Set.insert (AdminRole adm) (Set.fromList extras)

-- | A non-admin role set (never contains AdminRole).
arbitraryNonAdminRoles :: Gen (Set.Set Role)
arbitraryNonAdminRoles = do
    n <- chooseInt (0, 5)
    roles <-
        vectorOf n $
            AppRole . pack . ("role" <>) . show
                <$> chooseInt (0, 9)
    pure $ Set.fromList roles

{- | Build a GroupState with exactly @n@ admin members.
Used to test majority properties directly.
-}
gsWithAdminCount :: Int -> GroupState ()
gsWithAdminCount n =
    GroupState
        { members =
            Map.fromList
                [ (key i, adminMember (key i))
                | i <- [1 .. n]
                ]
        , pendingProposals = Map.empty
        , pendingBase = Map.empty
        , appFold = ()
        }
  where
    key :: Int -> Text
    key i = makeCesrKey (fromIntegral i)
    adminMember :: Text -> Member
    adminMember k =
        Member
            { memberKey = k
            , memberEmail = k <> "@test.example"
            , memberRoles =
                Set.singleton
                    (AdminRole PublicAdmin)
            }

-- | Pick an existing member key from a group state.
existingKey :: GroupState () -> Gen Text
existingKey gs = elements $ Map.keys (members gs)

-- | Pick an admin key from a group state.
adminKey :: GroupState () -> Gen Text
adminKey gs =
    elements
        [ k
        | k <- Map.keys (members gs)
        , isAdmin k gs
        ]

{- | Pick a non-admin member key from a group state.
Requires at least one non-admin member.
-}
nonAdminMemberKey :: GroupState () -> Gen Text
nonAdminMemberKey gs =
    elements
        [ k
        | k <- Map.keys (members gs)
        , not $ isAdmin k gs
        ]

{- | GroupState with at least one admin and at least
one non-admin member.
-}
arbitraryWithNonAdmin :: Gen (GroupState ())
arbitraryWithNonAdmin =
    arbitraryGroupState `suchThat` \gs ->
        adminCount gs > 0
            && Map.size (members gs)
                > adminCount gs

{- | GroupState with at least one admin and at least
one pending proposal.
-}
arbitraryWithPending :: Gen (GroupState ())
arbitraryWithPending = do
    gs <- arbitraryWithAdmin
    pid <- arbitraryKey
    signer <- adminKey gs
    proposal' <- arbitrary
    pure
        gs
            { pendingProposals =
                Map.singleton
                    pid
                    PendingProposal
                        { proposal = proposal'
                        , proposer = signer
                        , approvals =
                            Set.singleton signer
                        }
            }

-- | Trivial group config with no role defs.
trivialConfig :: GroupConfig ()
trivialConfig = GroupConfig{roleDefs = Map.empty}
