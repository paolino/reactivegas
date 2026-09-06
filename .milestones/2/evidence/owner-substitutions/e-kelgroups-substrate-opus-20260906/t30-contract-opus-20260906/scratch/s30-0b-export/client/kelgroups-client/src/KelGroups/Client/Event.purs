-- | Event types mirroring Haskell KelGroups.Event.
module KelGroups.Client.Event
  ( Proposal(..)
  , BaseEvent(..)
  , GroupEvent(..)
  ) where

import Prelude

import Data.Set (Set)
import KelGroups.Client.Types (Role)

-- | A proposal for group membership changes.
data Proposal
  = IntroduceMember String String (Set Role)
  | RemoveMember String
  | ChangeRoles String (Set Role)

derive instance eqProposal :: Eq Proposal

instance showProposal :: Show Proposal where
  show (IntroduceMember k _ _) = "IntroduceMember " <> k
  show (RemoveMember k) = "RemoveMember " <> k
  show (ChangeRoles k _) = "ChangeRoles " <> k

-- | Base group event (membership management).
data BaseEvent
  = Propose Proposal
  | Approve String

derive instance eqBaseEvent :: Eq BaseEvent

-- | Group event parameterized by application event type.
data GroupEvent a
  = Base BaseEvent
  | App a

derive instance eqGroupEvent :: Eq a => Eq (GroupEvent a)
