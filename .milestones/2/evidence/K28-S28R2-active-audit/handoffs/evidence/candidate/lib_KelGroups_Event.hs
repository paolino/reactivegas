{- |
Module      : KelGroups.Event
Description : Base and group event types
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Events that can be appended to a group KEL. Base events
handle member management and role changes. Application
events are opaque to the base system.
-}
module KelGroups.Event
    ( GroupEvent (..)
    , BaseEvent (..)
    , Proposal (..)
    , DirectCommand (..)
    , BaseMutation (..)
    , BaseChange (..)
    , IntegratedEvent (..)
    ) where

import Data.Set (Set)
import Data.Text (Text)
import KelGroups.Types
    ( ProposalId
    , Role
    )

{- | A group event: either a base infrastructure event
or an application-specific event.
-}
data GroupEvent a
    = -- | Base system event (members, roles, voting)
      Base BaseEvent
    | -- | Application event (opaque to base system)
      App a
    deriving stock (Show, Eq)

{- | Base events for group management. All member and
role changes follow a proposal + approval pattern.
-}
data BaseEvent
    = -- | Propose a change (by an admin)
      Propose Proposal
    | -- | Approve a pending proposal (by an admin)
      Approve ProposalId
    deriving stock (Show, Eq)

{- | A proposal for a group change. Proposals require
admin majority to take effect.

HISTORICAL-NON-PRODUCTION: 'Proposal'/'BaseEvent'/'GroupEvent' keep the
accepted #54 evidence. The integrated production path below
('DirectCommand'/'BaseMutation'/'IntegratedEvent') never reads these;
they receive no new production responsibility in this slice.
-}
data Proposal
    = -- | Add a new member with initial roles
      IntroduceMember
        Text
        -- ^ CESR-encoded public key
        Text
        -- ^ Email address
        (Set Role)
        -- ^ Initial roles (must include admin during bootstrap)
    | -- | Remove a member entirely
      RemoveMember
        Text
        -- ^ CESR-encoded public key
    | -- | Change a member's role set
      ChangeRoles
        Text
        -- ^ CESR-encoded public key
        (Set Role)
        -- ^ New role set
    deriving stock (Show, Eq)

{- | The direct base-command vocabulary: exactly one constructor, member
admission. The signer is supplied separately by the integrated
transition, so no command carries a second author identity.
-}
data DirectCommand = AdmitMember Text Text (Set Role)
    deriving stock (Show, Eq)

{- | The voted base vocabulary: removal and role change, and nothing
else. Admission is not representable here, so no pending approval can
enact one. Constructor names carry a 'Voted' suffix because Haskell
shares one constructor namespace per module while Lean namespaces per
inductive (r5 D1 mapping); shapes match Lean 'BaseMutation' exactly.
Adding an admission constructor stops the exhaustive enactment matching
compiling.
-}
data BaseMutation
    = RemoveMemberVoted Text
    | ChangeRolesVoted Text (Set Role)
    deriving stock (Show, Eq)

{- | The observable base-change vocabulary. A committed substrate
membership or role effect is exactly one of these three, each naming the
affected key.
-}
data BaseChange
    = MemberAdmitted Text
    | MemberRemoved Text
    | RolesChanged Text
    deriving stock (Show, Eq)

{- | The closed integrated event vocabulary. Base proposal and app event
are distinct type parameters, so an app event cannot be a proposal and no
membership action can arrive dressed as app payload.
-}
data IntegratedEvent bp e
    = IEDirect DirectCommand
    | IEPropose bp
    | IEApprove ProposalId
    | IEApp e
    deriving stock (Show, Eq)
