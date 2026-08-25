{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Reactivegas.Core.Projection
Description : Semantic state projected from validated events (plan §6.3)
Copyright   : (c) 2026 Paolo Veronelli
License     : BSD3
Maintainer  : Paolo Veronelli <paolo.veronelli@gmail.com>
Stability   : experimental

The materialized state obtained by folding accepted envelopes. Every
balance movement nets to zero against treasury or reserve, so
'ledgerTotal' is a permanent conservation probe: it must be zero for
any projection reachable through 'Reactivegas.Core.Reduce.step'.
The 'ToJSON' rendering is part of the cross-language fixture contract
consumed by @vectors/reducer.json@.
-}
module Reactivegas.Core.Projection (
    -- * Members
    MemberStatus (..),
    MemberState (..),
    emptyMemberState,
    -- * Campaigns and commitments
    Phase (..),
    CommitmentStatus (..),
    CommitmentState (..),
    CampaignState (..),
    emptyCampaignState,
    -- * Accounts, catalog, governance
    AccountState (..),
    CatalogItem (..),
    CatalogState (..),
    GovernanceState (..),
    -- * Whole projection
    Projection (..),
    emptyProjection,
    defaultQuorum,
    acceptedTotalFor,
    ledgerTotal,
) where

import Data.Aeson (ToJSON (..), ToJSONKey (..), ToJSONKeyFunction (..), Value (..))
import Data.Aeson qualified as Aeson
import Data.ByteArray.Encoding (Base (..), convertToBase)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)

import Reactivegas.Core.Envelope (MemberId (..))
import Reactivegas.Core.Payload

data MemberStatus = ActiveMember | SuspendedMember
    deriving (Eq, Ord, Show, Generic)

instance ToJSON MemberStatus

data MemberState = MemberState
    { memberStatus :: MemberStatus
    , memberRoles :: Set Role
    }
    deriving (Eq, Show, Generic)

instance ToJSON MemberState

emptyMemberState :: MemberState
emptyMemberState = MemberState ActiveMember mempty

data Phase
    = CollectingCatalog
    | OpenForOrders
    | ClosedForOrders
    | Finalized
    | Aborted
    deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance ToJSON Phase

data CommitmentStatus = ProposedCommitment | AcceptedCommitment
    deriving (Eq, Ord, Show, Generic)

instance ToJSON CommitmentStatus

data CommitmentState = CommitmentState
    { commitmentCampaign :: CampaignId
    , commitmentPledger :: MemberId
    , commitmentCents :: EuroCent
    , commitmentStatus :: CommitmentStatus
    }
    deriving (Eq, Show, Generic)

instance ToJSON CommitmentState

data CampaignState = CampaignState
    { campaignPhase :: Phase
    , campaignCatalogRoot :: Maybe ByteString
    , campaignAllocations :: Map MemberId EuroCent
    }
    deriving (Eq, Show, Generic)

instance ToJSON CampaignState

emptyCampaignState :: CampaignState
emptyCampaignState =
    CampaignState CollectingCatalog Nothing mempty

newtype AccountState = AccountState {accountBalance :: EuroCent}
    deriving (Eq, Ord, Show, Generic)

instance ToJSON AccountState

data CatalogItem = CatalogItem
    { itemName :: Text
    , itemUnitPrice :: EuroCent
    }
    deriving (Eq, Ord, Show, Generic)

instance ToJSON CatalogItem

newtype CatalogState = CatalogState {catalogItems :: Map ProductId CatalogItem}
    deriving (Eq, Ord, Show, Generic)

instance ToJSON CatalogState

data GovernanceState = GovernanceState
    { governanceVotes :: Map ProposalId (Map MemberId Choice)
    , governanceCertified :: Set ProposalId
    }
    deriving (Eq, Show, Generic)

instance ToJSON GovernanceState

data Projection = Projection
    { projMembers :: Map MemberId MemberState
    , projAccounts :: Map MemberId AccountState
    , projTreasury :: EuroCent
    , projReserve :: EuroCent
    , projCampaigns :: Map CampaignId CampaignState
    , projCommitments :: Map CommitmentId CommitmentState
    , projCatalog :: CatalogState
    , projGovernance :: GovernanceState
    , projMovements :: Set MovementId
    , projQuorum :: Int
    }
    deriving (Eq, Show, Generic)

instance ToJSON Projection

-- | Ballots required by 'QuorumCertified' until policy events exist.
defaultQuorum :: Int
defaultQuorum = 1

emptyProjection :: Projection
emptyProjection =
    Projection
        { projMembers = mempty
        , projAccounts = mempty
        , projTreasury = EuroCent 0
        , projReserve = EuroCent 0
        , projCampaigns = mempty
        , projCommitments = mempty
        , projCatalog = CatalogState mempty
        , projGovernance = GovernanceState mempty mempty
        , projMovements = mempty
        , projQuorum = defaultQuorum
        }

-- | Sum of accepted commitment amounts of one campaign.
acceptedTotalFor :: Projection -> CampaignId -> EuroCent
acceptedTotalFor proj cid =
    EuroCent $
        sum
            [ unEuroCent (commitmentCents c)
            | c <- Map.elems (projCommitments proj)
            , commitmentCampaign c == cid
            , commitmentStatus c == AcceptedCommitment
            ]

{- | Conservation probe: member balances plus treasury plus reserve.
Zero for every projection reachable by 'step'; nonzero means an
accounting bug created or destroyed money.
-}
ledgerTotal :: Projection -> Int64
ledgerTotal p =
    sum (map (unEuroCent . accountBalance) (Map.elems (projAccounts p)))
        + unEuroCent (projTreasury p)
        + unEuroCent (projReserve p)

-- Hex rendering shared with the envelope fixture format.

instance ToJSONKey MemberId where
    toJSONKey = hexKey unMemberId

instance ToJSONKey ProductId where
    toJSONKey = hexKey unProductId

instance ToJSONKey ProposalId where
    toJSONKey = hexKey unProposalId

hexKey :: (a -> ByteString) -> ToJSONKeyFunction
hexKey extract =
    ToJSONKeyText
        (TE.decodeUtf8 . hexEncodeBytes . extract)
        (Aeson.toEncoding . Aeson.String . TE.decodeUtf8 . hexEncodeBytes . extract)

-- | Deterministic lowercase hex via the memory package's Base16 codec.
hexEncodeBytes :: ByteString -> ByteString
hexEncodeBytes = convertToBase Base16

instance ToJSON CampaignId where
    toJSON = idJson unCampaignId

instance ToJSON CommitmentId where
    toJSON = idJson unCommitmentId

instance ToJSON MovementId where
    toJSON = idJson unMovementId

instance ToJSON ProposalId where
    toJSON = idJson unProposalId

instance ToJSON ProductId where
    toJSON = idJson unProductId

instance ToJSON MemberId where
    toJSON = idJson unMemberId

idJson :: (a -> ByteString) -> a -> Value
idJson extract = Aeson.String . TE.decodeUtf8 . hexEncodeBytes . extract

instance ToJSON EuroCent where
    toJSON (EuroCent c) = Aeson.toJSON c

instance ToJSON Role where
    toJSON r = Aeson.String $ case r of
        RoleReferente -> "referente"
        RoleTreasurer -> "treasurer"
        RoleCatalogEditor -> "catalog-editor"

instance ToJSON Choice where
    toJSON c = Aeson.String $ case c of
        VoteYes -> "yes"
        VoteNo -> "no"
