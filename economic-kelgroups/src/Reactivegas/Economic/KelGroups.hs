{- |
Module      : Reactivegas.Economic.KelGroups
Description : Production GroupView binding for the custody core
Copyright   : (c) 2026 Paolo Veronelli
License     : BSD-3-Clause

Binds the pure custody core to the accepted KelGroups substrate: both
queries derive from one canonical GroupView and no membership or role data
store is created here. There is no duplicated transition policy:
stepInView is exactly the core's step under the view's queries, and the
view is read-only input that the transition can neither return nor change.
-}
module Reactivegas.Economic.KelGroups (
    queriesFromView,
    stepInView,
) where

import KelGroups.Types (
    GroupView,
    isAdminInView,
    isMemberInView,
 )

import Reactivegas.Economic.Core (
    CustodyEvent,
    Key,
    Queries (..),
    State,
    step,
 )

{- | Both custody queries derived read-only from the one canonical view:
membership and admin roles are read, never reconstructed or subset.
-}
queriesFromView :: GroupView -> Queries
queriesFromView view =
    Queries
        { memberQuery = (`isMemberInView` view)
        , adminQuery = (`isAdminInView` view)
        }

-- | The production custody transition under a canonical GroupView.
stepInView ::
    GroupView ->
    State frame ->
    Key ->
    CustodyEvent ->
    Maybe (State frame)
stepInView view state signer event =
    step (queriesFromView view) state signer event
