-- | Group state mirroring Haskell KelGroups.State.
module KelGroups.Client.State
  ( PendingProposal
  , GroupState
  , emptyState
  , adminCount
  , majority
  , isAdmin
  , isMember
  ) where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Set (Set)
import KelGroups.Client.Event (Proposal)
import KelGroups.Client.Types (Member, hasAdmin)

-- | A pending proposal awaiting admin majority.
type PendingProposal =
  { proposal :: Proposal
  , proposer :: String
  , approvals :: Set String
  }

-- | Group state parameterized by application state.
type GroupState a =
  { members :: Map String Member
  , pendingProposals :: Map String PendingProposal
  , appFold :: a
  }

-- | Empty group state with given initial app value.
emptyState :: forall a. a -> GroupState a
emptyState initial =
  { members: Map.empty
  , pendingProposals: Map.empty
  , appFold: initial
  }

-- | Count admins in the group.
adminCount :: forall a. GroupState a -> Int
adminCount gs =
  Map.size $ Map.filter hasAdminM gs.members
  where
  hasAdminM :: Member -> Boolean
  hasAdminM m = hasAdmin m.roles

-- | Majority threshold: ceiling(numAdmins / 2).
majority :: forall a. GroupState a -> Int
majority gs =
  let
    n = adminCount gs
  in
    (n + 1) / 2

-- | Check if a key belongs to an admin.
isAdmin :: forall a. String -> GroupState a -> Boolean
isAdmin key gs = case Map.lookup key gs.members of
  Nothing -> false
  Just m -> hasAdmin m.roles

-- | Check if a key belongs to a member.
isMember :: forall a. String -> GroupState a -> Boolean
isMember key gs = isJust (Map.lookup key gs.members)
