{- |
Module      : KelGroups.Bootstrap
Description : Bootstrap mode detection and logic
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

When the KEL has zero admins (empty or all admins
removed), the system enters bootstrap mode. In this
mode, authentication is via passphrase challenge
rather than cryptographic signatures.
-}
module KelGroups.Bootstrap
    ( AuthMode (..)
    , authMode
    ) where

import KelGroups.State (GroupState, adminCount)

-- | The authentication mode of the system.
data AuthMode
    = -- | No admins — passphrase challenge required
      Bootstrap
    | -- | At least one admin — signature-based auth
      Normal
    deriving stock (Eq, Show)

{- | Determine the authentication mode from the
current group condition. Zero admins means bootstrap.
-}
authMode :: GroupState a -> AuthMode
authMode gs
    | adminCount gs == 0 = Bootstrap
    | otherwise = Normal
