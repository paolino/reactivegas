{- |
Module      : KelGroups.State
Description : Group condition derived from KEL fold
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

The group condition (configuration) is derived entirely
by folding the KEL. There is no mutable external state.
-}
module KelGroups.State
    ( GroupState (..)
    , PendingProposal (..)
    , PendingBase (..)
    , emptyState
    , adminCount
    , majority
    , isAdmin
    , isMember
    , groupView
    , lookupPendingBase
    ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text (Text)
import KelGroups.Event (BaseMutation, Proposal)
import KelGroups.Types
    ( GroupView (..)
    , Member (..)
    , ProposalId
    , hasAdmin
    )

{- | A voted base mutation awaiting its approval threshold. The
integrated pending store: its payload is 'BaseMutation', which cannot
express admission, so nothing here can ever insert a member.
-}
data PendingBase = PendingBase
    { pbMutation :: BaseMutation
    -- ^ The voted mutation (non-admitting-typed)
    , pbProposer :: Text
    -- ^ CESR public key of the proposing admin
    , pbApprovals :: Set Text
    -- ^ CESR public keys of admins who approved
    }
    deriving stock (Show, Eq)

{- | The group condition, derived from folding the KEL.
Parameterized by the application fold result @a@.
-}
data GroupState s = GroupState
    { members :: Map Text Member
    -- ^ All members, keyed by CESR public key
    , pendingProposals :: Map ProposalId PendingProposal
    -- ^ Proposals awaiting approval (HISTORICAL-NON-PRODUCTION store)
    , pendingBase :: Map ProposalId PendingBase
    -- ^ Voted base mutations awaiting approval (PRODUCTION store)
    , appFold :: s
    -- ^ Application-level fold result (the AppState)
    }
    deriving stock (Show, Eq)

-- | A proposal that has been submitted but not yet enacted.
data PendingProposal = PendingProposal
    { proposal :: Proposal
    -- ^ The proposed change
    , proposer :: Text
    -- ^ CESR public key of the proposing admin
    , approvals :: Set Text
    -- ^ CESR public keys of admins who approved
    }
    deriving stock (Show, Eq)

-- | Empty group condition with no members.
emptyState :: s -> GroupState s
emptyState = GroupState Map.empty Map.empty Map.empty

-- | Count current admins.
adminCount :: GroupState a -> Int
adminCount =
    Map.size
        . Map.filter (hasAdmin . memberRoles)
        . members

{- | Compute required majority for admin votes.
@ceil(numAdmins / 2)@. With zero admins, returns 0
(bootstrap mode).
-}
majority :: GroupState a -> Int
majority gs =
    let n = adminCount gs
    in  (n + 1) `div` 2

-- | Check if a public key belongs to an admin.
isAdmin :: Text -> GroupState a -> Bool
isAdmin pubKey gs =
    case Map.lookup pubKey (members gs) of
        Just m -> hasAdmin (memberRoles m)
        Nothing -> False

-- | Check if a public key belongs to a member.
isMember :: Text -> GroupState a -> Bool
isMember pubKey = Map.member pubKey . members

-- | The voted base mutation pending under an id, if any.
lookupPendingBase
    :: ProposalId -> GroupState s -> Maybe PendingBase
lookupPendingBase proposalId gs =
    Map.lookup proposalId (pendingBase gs)

{- | The canonical projection. The sole route from the writable
aggregate to the read-only view every app fold, base hook and vote
observation consumes.
-}
groupView :: GroupState s -> GroupView
groupView gs = GroupView (members gs)
