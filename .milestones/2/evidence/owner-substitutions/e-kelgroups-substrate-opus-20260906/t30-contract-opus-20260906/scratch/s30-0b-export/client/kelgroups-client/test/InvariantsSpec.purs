-- | Static invariant properties mirroring Lean Invariants.lean.
module Test.InvariantsSpec (run) where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import KelGroups.Client.State
  ( adminCount
  , emptyState
  , isAdmin
  , isMember
  , majority
  )
import Test.Generators
  ( adminKey
  , arbitraryWithAdmin
  , gsWithAdminCount
  , arbitraryGroupState
  )
import Test.QuickCheck (quickCheck, (<?>))
import Test.QuickCheck.Gen (chooseInt)

run :: Effect Unit
run = do
  -- S1: bootstrap iff zero admins
  log "  S1: bootstrap iff zero admins"
  quickCheck do
    gs <- arbitraryGroupState
    let
      ac = adminCount gs
      m = majority gs
    pure $ (ac == 0) == (m == 0)
      <?> "adminCount=" <> show ac
        <> " majority="
        <> show m

  -- S2: empty state is bootstrap
  log "  S2: empty state is bootstrap"
  quickCheck
    $ adminCount (emptyState unit) == 0
        <?> "emptyState should have 0 admins"

  -- S3: majority(0) = 0
  log "  S3: majority(0) = 0"
  quickCheck
    $ majority (gsWithAdminCount 0) == 0
        <?> "majority of 0 admins should be 0"

  -- S4: majority(1) = 1
  log "  S4: majority(1) = 1"
  quickCheck
    $ majority (gsWithAdminCount 1) == 1
        <?> "majority of 1 admin should be 1"

  -- S5: majority(n) <= n
  log "  S5: majority(n) <= n"
  quickCheck do
    n <- chooseInt 0 20
    let m = majority (gsWithAdminCount n)
    pure $ m <= n
      <?> "majority(" <> show n <> ")="
        <> show m
        <> " > "
        <> show n

  -- S6: majority(n) > 0 when n > 0
  log "  S6: majority(n) > 0 when n > 0"
  quickCheck do
    n <- chooseInt 1 20
    let m = majority (gsWithAdminCount n)
    pure $ m > 0
      <?> "majority(" <> show n <> ")="
        <> show m
        <> " should be > 0"

  -- S7: admin member implies normal member
  log "  S7: admin member implies member"
  quickCheck do
    gs <- arbitraryWithAdmin
    k <- adminKey gs
    pure $ isAdmin k gs && isMember k gs
      <?> k <> " should be both admin and member"

  -- B1: zero admins means bootstrap
  log "  B1: zero admins means bootstrap"
  quickCheck
    $ majority (emptyState unit) == 0
        <?> "empty state majority should be 0"
