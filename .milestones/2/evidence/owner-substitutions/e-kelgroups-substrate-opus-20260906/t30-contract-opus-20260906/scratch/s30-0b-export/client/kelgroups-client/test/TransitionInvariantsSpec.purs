-- | Transition invariant properties mirroring Lean
-- | TransitionInvariants.lean + bootstrap behavior.
module Test.TransitionInvariantsSpec (run) where

import Prelude

import Data.Map as Map
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Console (log)
import KelGroups.Client.Event (GroupEvent, Proposal(..))
import KelGroups.Client.Fold
  ( applyPropose
  , enact
  , foldGroup
  )
import KelGroups.Client.State
  ( adminCount
  , emptyState
  , isAdmin
  )
import Test.Generators
  ( adminKey
  , arbitraryAdminRoles
  , arbitraryNonAdminRoles
  , arbitraryProposal
  , arbitraryWithAdmin
  , arbitraryWithTwoAdmins
  , freshKey
  , gsWithAdminCount
  )
import Test.QuickCheck (quickCheck, (<?>))

run :: Effect Unit
run = do
  -- T1: enact introduce admin exits bootstrap
  log "  T1: enact introduce admin exits bootstrap"
  quickCheck do
    k <- freshKey (emptyState unit)
    roles <- arbitraryAdminRoles
    let
      gs' = enact (emptyState unit)
        (IntroduceMember k (k <> "@test.example") roles)
    pure $ adminCount gs' > 0
      <?> "should exit bootstrap after introducing admin"

  -- T2: enact introduce admin increments count
  log "  T2: enact introduce admin increments count"
  quickCheck do
    gs <- arbitraryWithAdmin
    k <- freshKey gs
    roles <- arbitraryAdminRoles
    let
      before = adminCount gs
      gs' = enact gs (IntroduceMember k (k <> "@test.example") roles)
    pure $ adminCount gs' == before + 1
      <?> "expected " <> show (before + 1)
        <> " got "
        <> show (adminCount gs')

  -- T3: enact introduce non-admin preserves count
  log "  T3: enact introduce non-admin preserves count"
  quickCheck do
    gs <- arbitraryWithAdmin
    k <- freshKey gs
    roles <- arbitraryNonAdminRoles
    let
      before = adminCount gs
      gs' = enact gs (IntroduceMember k (k <> "@test.example") roles)
    pure $ adminCount gs' == before
      <?> "expected " <> show before
        <> " got "
        <> show (adminCount gs')

  -- T4: enact preserves pendingProposals
  log "  T4: enact preserves pendingProposals"
  quickCheck do
    gs <- arbitraryWithAdmin
    p <- arbitraryProposal
    let gs' = enact gs p
    pure $ gs'.pendingProposals == gs.pendingProposals
      <?> "pendingProposals should be unchanged"

  -- T5: foldGroup [] = emptyState
  log "  T5: foldGroup [] = emptyState"
  quickCheck
    $
      let
        gs = foldGroup (\a _ -> a) unit
          ([] :: Array (Tuple String (GroupEvent Unit)))
        expected = emptyState unit
      in
        gs.members == expected.members
          && gs.pendingProposals
            == expected.pendingProposals
          && gs.appFold == expected.appFold
          <?> "foldGroup of empty should be emptyState"

  -- T6: bootstrap proposal immediately enacted
  log "  T6: bootstrap proposal immediately enacted"
  quickCheck do
    k <- freshKey (emptyState unit)
    roles <- arbitraryAdminRoles
    let
      gs' = applyPropose (emptyState unit) "signer"
        (IntroduceMember k (k <> "@test.example") roles)
    pure $ Map.isEmpty gs'.pendingProposals
      <?> "proposal should enact immediately in bootstrap"

  -- T7: single admin proposal enacted immediately
  log "  T7: single admin proposal enacted immediately"
  quickCheck do
    let gs = gsWithAdminCount 1
    k <- freshKey gs
    roles <- arbitraryAdminRoles
    let
      gs' = applyPropose gs "admin0"
        (IntroduceMember k (k <> "@test.example") roles)
    pure $ Map.isEmpty gs'.pendingProposals
      <?> "single admin proposal should enact immediately"

  -- T8: remove with >=2 admins keeps >=1
  log "  T8: remove with >=2 admins keeps >=1"
  quickCheck do
    gs <- arbitraryWithTwoAdmins
    k <- adminKey gs
    let gs' = enact gs (RemoveMember k)
    pure $ adminCount gs' >= 1
      <?> "should keep at least 1 admin after removal"

  -- B2: proposal in bootstrap auto-enacted
  log "  B2: proposal in bootstrap auto-enacted"
  quickCheck do
    p <- arbitraryProposal
    let gs' = applyPropose (emptyState unit) "signer" p
    pure $ Map.isEmpty gs'.pendingProposals
      <?> "all proposals auto-enacted in bootstrap"

  -- B3: single admin proposal auto-enacted
  log "  B3: single admin proposal auto-enacted"
  quickCheck do
    let gs = gsWithAdminCount 1
    p <- arbitraryProposal
    let gs' = applyPropose gs "admin0" p
    pure $ Map.isEmpty gs'.pendingProposals
      <?> "single admin proposal should auto-enact"

  -- B4: bootstrap intro creates admin member
  log "  B4: bootstrap intro creates admin member"
  quickCheck do
    k <- freshKey (emptyState unit)
    roles <- arbitraryAdminRoles
    let
      gs' = enact (emptyState unit)
        (IntroduceMember k (k <> "@test.example") roles)
    pure $ isAdmin k gs'
      <?> k <> " should be admin after introduce"
