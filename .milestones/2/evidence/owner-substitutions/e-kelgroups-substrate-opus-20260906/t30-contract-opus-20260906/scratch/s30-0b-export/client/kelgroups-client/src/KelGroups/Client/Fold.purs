-- | Client-side fold mirroring Haskell KelGroups.Fold.
module KelGroups.Client.Fold
  ( AppFold
  , foldGroup
  , applyEvent
  , applyPropose
  , tryEnact
  , enact
  , proposalDigest
  ) where

import Prelude

import Data.Foldable (foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import KelGroups.Client.Event
  ( BaseEvent(..)
  , GroupEvent(..)
  , Proposal(..)
  )
import KelGroups.Client.State
  ( GroupState
  , PendingProposal
  , emptyState
  , majority
  )
import KelGroups.Client.Types (Member)

-- | Application fold function.
type AppFold a = a -> a -> a

-- | Fold a sequence of signed group events into state.
foldGroup
  :: forall a
   . AppFold a
  -> a
  -> Array (Tuple String (GroupEvent a))
  -> GroupState a
foldGroup appFoldFn initial =
  foldl (applyEvent appFoldFn) (emptyState initial)

-- | Apply a single event to group state. Assumes validated.
applyEvent
  :: forall a
   . AppFold a
  -> GroupState a
  -> Tuple String (GroupEvent a)
  -> GroupState a
applyEvent appFoldFn gs (Tuple signer evt) = case evt of
  Base baseEvt -> applyBase gs signer baseEvt
  App appEvt ->
    gs { appFold = appFoldFn gs.appFold appEvt }

applyBase
  :: forall a
   . GroupState a
  -> String
  -> BaseEvent
  -> GroupState a
applyBase gs signer = case _ of
  Propose proposal' -> applyPropose gs signer proposal'
  Approve proposalId -> applyApprove gs signer proposalId

-- | Apply a Propose event.
applyPropose
  :: forall a
   . GroupState a
  -> String
  -> Proposal
  -> GroupState a
applyPropose gs signer proposal' =
  let
    pid = proposalDigest proposal'

    pp :: PendingProposal
    pp =
      { proposal: proposal'
      , proposer: signer
      , approvals: Set.singleton signer
      }
    gs' = gs
      { pendingProposals =
          Map.insert pid pp gs.pendingProposals
      }
  in
    tryEnact gs' pid

applyApprove
  :: forall a
   . GroupState a
  -> String
  -> String
  -> GroupState a
applyApprove gs signer proposalId =
  case Map.lookup proposalId gs.pendingProposals of
    Nothing -> gs
    Just pp ->
      let
        pp' = pp
          { approvals = Set.insert signer pp.approvals }
        gs' = gs
          { pendingProposals =
              Map.insert proposalId pp' gs.pendingProposals
          }
      in
        tryEnact gs' proposalId

-- | Try to enact a proposal if it has reached majority.
tryEnact :: forall a. GroupState a -> String -> GroupState a
tryEnact gs proposalId =
  case Map.lookup proposalId gs.pendingProposals of
    Nothing -> gs
    Just pp
      | Set.size pp.approvals >= majority gs ->
          let
            gs' = enact gs pp.proposal
          in
            gs'
              { pendingProposals =
                  Map.delete proposalId gs'.pendingProposals
              }
      | otherwise -> gs

-- | Enact a proposal by modifying group state.
enact :: forall a. GroupState a -> Proposal -> GroupState a
enact gs = case _ of
  IntroduceMember pubKey email roles ->
    let
      m :: Member
      m = { key: pubKey, email, roles }
    in
      gs { members = Map.insert pubKey m gs.members }
  RemoveMember pubKey ->
    gs { members = Map.delete pubKey gs.members }
  ChangeRoles pubKey roles ->
    gs
      { members =
          Map.update
            (\m -> Just (m { roles = roles }))
            pubKey
            gs.members
      }

-- | Compute a proposal digest. Matches Haskell's placeholder.
proposalDigest :: Proposal -> String
proposalDigest p = "proposal:" <> show p
