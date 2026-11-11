module View.Proposals
  ( proposalsComponent
  , Output(..)
  , Input
  ) where

import Prelude

import Data.Array (fromFoldable)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import KelGroups.Client.Event (Proposal(..))
import KelGroups.Client.State (GroupState, PendingProposal, isAdmin, majority)

data Output = SubmitApprove String

type Input =
  { groupState :: GroupState Unit
  , myKey :: Maybe String
  }

type State =
  { groupState :: GroupState Unit
  , myKey :: Maybe String
  }

data Action
  = Receive Input
  | DoApprove String

proposalsComponent
  :: forall q m. MonadAff m => H.Component q Input Output m
proposalsComponent = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }

initialState :: Input -> State
initialState input =
  { groupState: input.groupState
  , myKey: input.myKey
  }

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  let
    entries = fromFoldable
      ( Map.toUnfoldable st.groupState.pendingProposals
          :: Array (Tuple String PendingProposal)
      )
    maj = majority st.groupState
    amIAdmin = case st.myKey of
      Nothing -> false
      Just k -> isAdmin k st.groupState
  in
    HH.div [ HP.class_ (HH.ClassName "proposals") ]
      [ HH.h2_ [ HH.text "Pending Proposals" ]
      , if entries == [] then HH.p_ [ HH.text "No pending proposals." ]
        else HH.div_ (map (proposalCard amIAdmin maj st.myKey) entries)
      ]

proposalCard
  :: forall m
   . Boolean
  -> Int
  -> Maybe String
  -> Tuple String PendingProposal
  -> H.ComponentHTML Action () m
proposalCard amIAdmin maj myKey (Tuple pid pp) =
  let
    approvalCount = Set.size pp.approvals
    alreadyApproved = case myKey of
      Nothing -> true
      Just k -> Set.member k pp.approvals
  in
    HH.div [ HP.class_ (HH.ClassName "proposal-card") ]
      [ HH.h3_ [ HH.text (describeProposal pp.proposal) ]
      , HH.p_
          [ HH.text $
              "Proposed by: " <> truncateKey pp.proposer
          ]
      , HH.p_
          [ HH.text $
              "Approvals: " <> show approvalCount
                <> " / "
                <> show maj
                <> " needed"
          ]
      , if amIAdmin && not alreadyApproved then HH.button
          [ HE.onClick (const (DoApprove pid))
          , HP.class_ (HH.ClassName "btn-primary")
          ]
          [ HH.text "Approve" ]
        else HH.text ""
      ]

handleAction
  :: forall m
   . MonadAff m
  => Action
  -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive input ->
    H.modify_ _
      { groupState = input.groupState
      , myKey = input.myKey
      }
  DoApprove pid ->
    H.raise (SubmitApprove pid)

-- Helpers

describeProposal :: Proposal -> String
describeProposal (IntroduceMember key _ _) =
  "Introduce " <> truncateKey key
describeProposal (RemoveMember key) =
  "Remove " <> truncateKey key
describeProposal (ChangeRoles key _) =
  "Change roles for " <> truncateKey key

truncateKey :: String -> String
truncateKey s =
  if String.length s > 12 then String.take 12 s <> "..."
  else s
