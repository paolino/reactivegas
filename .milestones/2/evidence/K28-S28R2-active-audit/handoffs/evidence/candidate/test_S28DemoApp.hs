{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
Module      : S28DemoApp
Description : Test-only demo instance for S28-1 integrated app-api
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Test-only nondegenerate application built on the integrated substrate.
Distinct state and event types, signer checked through the sole GroupView,
domain refusal before durable append, sealed atomic base hook.
Never shipped as a library or executable.
-}
module S28DemoApp
    ( DemoState (..)
    , DemoEvent (..)
    , DemoError (..)
    , DemoProposal (..)
    , demoProposalMutation
    , demoDigest
    , demoReserved
    , protectedKey
    , demoAppFold
    , demoBaseHook
    , demoIntegration
    , demoInitialState
    , foundingDemo
    , demoStep
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import KelGroups.Event
    ( BaseChange (..)
    , BaseMutation (..)
    , IntegratedEvent
    )
import KelGroups.Fold
    ( BaseHook
    , IntegratedAppFold
    , IntegratedError (..)
    , IntegratedResult (..)
    , Integration (..)
    , applyIntegratedEvent
    )
import KelGroups.Server.JSON ()
import KelGroups.State (GroupState (..), emptyState)
import KelGroups.Types
    ( Admin (..)
    , Member (..)
    , ProposalId
    , Role (..)
    , isAdminInView
    )

data DemoState = DemoState
    { demoCounter :: Int
    , demoLog :: [Text]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data DemoEvent
    = DemoAdd Int
    | DemoReset
    | DemoNoop
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data DemoError
    = DemoNotAdmin Text
    | DemoNegative Int
    | DemoHookRefused Text
    deriving stock (Eq, Show)

data DemoProposal
    = DemoRemove Text
    | DemoChangeRoles Text (Set Role)
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

demoProposalMutation :: DemoProposal -> BaseMutation
demoProposalMutation = \case
    DemoRemove key -> RemoveMemberVoted key
    DemoChangeRoles key roles -> ChangeRolesVoted key roles

demoDigest :: DemoProposal -> ProposalId
demoDigest proposal' = pack (show proposal')

demoReserved :: Text
demoReserved = "reserved-demo-key-do-not-admit"

protectedKey :: Text
protectedKey = "protected-demo-member-key"

demoAppFold :: IntegratedAppFold DemoState DemoEvent DemoError
demoAppFold signer preView _postView state event = case event of
    DemoAdd n ->
        if n < 0
            then Left (DemoNegative n)
            else
                Right
                    state
                        { demoCounter = demoCounter state + n
                        , demoLog =
                            demoLog state
                                <> ["add " <> pack (show n)]
                        }
    DemoReset ->
        if isAdminInView signer preView
            then
                Right
                    state
                        { demoCounter = 0
                        , demoLog = demoLog state <> ["reset"]
                        }
            else Left (DemoNotAdmin signer)
    DemoNoop -> Right state

demoBaseHook :: BaseHook DemoState DemoError
demoBaseHook change _preView _postView state = case change of
    MemberRemoved key ->
        if key == protectedKey
            then Left (DemoHookRefused key)
            else
                Right
                    state
                        { demoLog =
                            demoLog state
                                <> ["hook removed " <> key]
                        }
    MemberAdmitted key ->
        Right
            state
                { demoLog =
                    demoLog state <> ["hook admitted " <> key]
                }
    RolesChanged key ->
        Right
            state
                { demoLog =
                    demoLog state <> ["hook roles " <> key]
                }

demoIntegration
    :: Integration DemoState DemoEvent DemoProposal DemoError
demoIntegration =
    Integration
        { intReserved = demoReserved
        , intDigest = demoDigest
        , intProposalMutation = demoProposalMutation
        , intAppFold = demoAppFold
        , intBaseHook = demoBaseHook
        }

demoInitialState :: GroupState DemoState
demoInitialState = emptyState (DemoState 0 [])

foundingDemo :: GroupState DemoState
foundingDemo =
    GroupState
        { members =
            Map.singleton "admin-key-1" (foundingAdmin "admin-key-1")
        , pendingProposals = Map.empty
        , pendingBase = Map.empty
        , appFold = DemoState 0 []
        }
  where
    foundingAdmin key =
        Member
            { memberKey = key
            , memberEmail = key <> "@test.example"
            , memberRoles = Set.singleton (AdminRole PublicAdmin)
            }

demoStep
    :: GroupState DemoState
    -> Text
    -> IntegratedEvent DemoProposal DemoEvent
    -> Either (IntegratedError DemoError) (IntegratedResult DemoState)
demoStep gs signer event =
    applyIntegratedEvent demoIntegration gs signer event
