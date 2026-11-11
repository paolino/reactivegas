-- | Fold invariant properties mirroring Lean FoldInvariants.lean.
module Test.FoldSpec (run) where

import Prelude

import Data.Map as Map
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import KelGroups.Client.Event
  ( BaseEvent(..)
  , GroupEvent(..)
  , Proposal(..)
  )
import KelGroups.Client.Fold (applyEvent, enact)
import Test.Generators
  ( arbitraryAdminRoles
  , arbitraryGroupState
  , arbitraryWithAdmin
  , freshKey
  )
import Test.QuickCheck (quickCheck, (<?>))

run :: Effect Unit
run = do
  -- F1: app event preserves members
  log "  F1: app event preserves members"
  quickCheck do
    gs <- arbitraryGroupState
    let
      gs' = applyEvent (\a _ -> a) gs
        (Tuple "signer" (App unit))
    pure $ gs'.members == gs.members
      <?> "app event should not change members"

  -- F2: app event preserves pendingProposals
  log "  F2: app event preserves pendingProposals"
  quickCheck do
    gs <- arbitraryGroupState
    let
      gs' = applyEvent (\a _ -> a) gs
        (Tuple "signer" (App unit))
    pure $ gs'.pendingProposals == gs.pendingProposals
      <?> "app event should not change pendingProposals"

  -- F3: approve nonexistent is identity
  log "  F3: approve nonexistent is identity"
  quickCheck do
    gs <- arbitraryGroupState
    let
      gs' = applyEvent (\a _ -> a) gs
        ( Tuple "signer"
            (Base (Approve "nonexistent-id"))
        )
    pure
      $
        gs'.members == gs.members
          && gs'.pendingProposals
            == gs.pendingProposals
          && gs'.appFold == gs.appFold
          <?> "approve nonexistent should be identity"

  -- F4: changeRoles preserves member count
  log "  F4: changeRoles preserves member count"
  quickCheck do
    gs <- arbitraryWithAdmin
    roles <- arbitraryAdminRoles
    let
      before = Map.size gs.members
      gs' = enact gs (ChangeRoles "admin0" roles)
    pure $ Map.size gs'.members == before
      <?> "changeRoles should preserve member count"

  -- F5: introduce increases member count
  log "  F5: introduce increases member count"
  quickCheck do
    gs <- arbitraryGroupState
    k <- freshKey gs
    roles <- arbitraryAdminRoles
    let
      before = Map.size gs.members
      gs' = enact gs (IntroduceMember k (k <> "@test.example") roles)
    pure $ Map.size gs'.members == before + 1
      <?> "expected " <> show (before + 1)
        <> " got "
        <> show (Map.size gs'.members)
