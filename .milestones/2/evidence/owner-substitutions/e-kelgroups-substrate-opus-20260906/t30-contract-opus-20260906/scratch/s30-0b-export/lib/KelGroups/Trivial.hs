{- |
Module      : KelGroups.Trivial
Description : Trivial instance with no application semantics
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

The trivial instance uses @()@ as the application
event type and fold result. No application roles,
no application events — just the base system.
-}
module KelGroups.Trivial
    ( trivialConfig
    , trivialFold
    , trivialInitial
    ) where

import Data.Map.Strict qualified as Map
import KelGroups.Fold (AppFold)
import KelGroups.Types (GroupConfig (..))

-- | Trivial group config with no application roles.
trivialConfig :: GroupConfig ()
trivialConfig =
    GroupConfig{roleDefs = Map.empty}

-- | Trivial fold function — ignores all app events.
trivialFold :: AppFold ()
trivialFold _ _ = ()

-- | Trivial initial fold value.
trivialInitial :: ()
trivialInitial = ()
