{- |
Module      : KelGroups.Fold
Description : Fold a KEL into group condition
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

The group condition is computed by folding the sequence
of group events. Base events update members, roles, and
pending proposals. Application events are folded by a
user-supplied function.
-}
module KelGroups.Fold
    ( foldGroup
    , applyEvent
    , AppFold
    , enact
    , applyPropose
    , IntegratedAppFold
    , BaseHook
    , IntegratedError (..)
    , IntegratedResult (..)
    , Integration (..)
    , commitBaseChange
    , tryEnactBase
    , applyIntegratedEvent
    , foldIntegrated
    , foldIntegratedFrom
    , enactMutation
    , mutationChange
    , admitMemberInto
    ) where

import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, pack)
import Data.Text.Encoding qualified as TE
import KelGroups.Event
    ( BaseChange (..)
    , BaseEvent (..)
    , BaseMutation (..)
    , DirectCommand (..)
    , GroupEvent (..)
    , IntegratedEvent
    , Proposal (..)
    )
import KelGroups.Event qualified as Evt
import KelGroups.State
    ( GroupState (..)
    , PendingBase (..)
    , PendingProposal (..)
    , emptyState
    , groupView
    , lookupPendingBase
    , majority
    )
import KelGroups.Types
    ( GroupView
    , Member (..)
    , ProposalId
    , Role
    )
import KelGroups.Validate
    ( ValidationError (..)
    , validateBaseApproval
    , validateBaseMutation
    , validateDirectAdmission
    )
import Keri.Crypto.Digest (computeSaid)

{- | Application fold function. Given the current
application fold result and an application event,
produce the new fold result.

HISTORICAL-NON-PRODUCTION: 'AppFold'/'foldGroup'/'applyEvent' and the
base helpers below keep the accepted behavior and the historical
'pendingProposals' store. The integrated production fold
('applyIntegratedEvent'/'foldIntegrated') never calls these; they receive
no new production responsibility in this slice.
-}
type AppFold a = a -> a -> a

{- | Fold a sequence of signed group events into a
group condition. Each event is tagged with the signer's
CESR public key.
-}
foldGroup
    :: AppFold a
    -> a
    -- ^ Initial application fold value
    -> [(Text, GroupEvent a)]
    -- ^ Events with signer public keys
    -> GroupState a
foldGroup appFoldFn initial =
    foldl (applyEvent appFoldFn) (emptyState initial)

{- | Apply a single event to the group condition.
Does not validate — assumes the event has already
been validated.
-}
applyEvent
    :: AppFold a
    -> GroupState a
    -> (Text, GroupEvent a)
    -> GroupState a
applyEvent appFoldFn gs (signer, evt) = case evt of
    Base baseEvt -> applyBase gs signer baseEvt
    App appEvt ->
        gs{appFold = appFoldFn (appFold gs) appEvt}

applyBase
    :: GroupState a
    -> Text
    -> BaseEvent
    -> GroupState a
applyBase gs signer = \case
    Propose proposal' ->
        applyPropose gs signer proposal'
    Approve proposalId ->
        applyApprove gs signer proposalId

applyPropose
    :: GroupState a
    -> Text
    -> Proposal
    -> GroupState a
applyPropose gs signer proposal' =
    let pid = proposalDigest proposal'
        pp =
            PendingProposal
                { proposal = proposal'
                , proposer = signer
                , approvals = Set.singleton signer
                }
        gs' =
            gs
                { pendingProposals =
                    Map.insert
                        pid
                        pp
                        (pendingProposals gs)
                }
    in  tryEnact gs' pid

applyApprove
    :: GroupState a
    -> Text
    -> Text
    -> GroupState a
applyApprove gs signer proposalId =
    case Map.lookup
        proposalId
        (pendingProposals gs) of
        Nothing -> gs
        Just pp ->
            let pp' =
                    pp
                        { approvals =
                            Set.insert
                                signer
                                (approvals pp)
                        }
                gs' =
                    gs
                        { pendingProposals =
                            Map.insert
                                proposalId
                                pp'
                                (pendingProposals gs)
                        }
            in  tryEnact gs' proposalId

{- | Try to enact a proposal if it has reached
admin majority.
-}
tryEnact
    :: GroupState a -> Text -> GroupState a
tryEnact gs proposalId =
    case Map.lookup
        proposalId
        (pendingProposals gs) of
        Nothing -> gs
        Just pp
            | Set.size (approvals pp)
                >= majority gs ->
                let gs' = enact gs (proposal pp)
                in  gs'
                        { pendingProposals =
                            Map.delete
                                proposalId
                                (pendingProposals gs')
                        }
            | otherwise -> gs

-- | Enact a proposal by modifying the group condition.
enact :: GroupState a -> Proposal -> GroupState a
enact gs = \case
    IntroduceMember pubKey email roles ->
        gs
            { members =
                Map.insert
                    pubKey
                    Member
                        { memberKey = pubKey
                        , memberEmail = email
                        , memberRoles = roles
                        }
                    (members gs)
            }
    RemoveMember pubKey ->
        gs{members = Map.delete pubKey (members gs)}
    ChangeRoles pubKey roles ->
        gs
            { members =
                Map.adjust
                    (\m -> m{memberRoles = roles})
                    pubKey
                    (members gs)
            }

{- | Compute a proposal digest using SAID. The
proposal's 'show' representation is hashed via
keri-hs 'computeSaid' to produce a CESR-encoded
Blake2b-256 digest.
-}
proposalDigest :: Proposal -> Text
proposalDigest p =
    computeSaid $ TE.encodeUtf8 $ pack $ show p

-- --------------------------------------------------------
-- Integrated production fold
-- --------------------------------------------------------

{- | The application transition. It receives the signer, the canonical
pre- and post-transition views, the app payload and the app event, and
returns app payload or a rejection: never a group or a member list.
-}
type IntegratedAppFold s e err =
    Text -> GroupView -> GroupView -> s -> e -> Either err s

{- | The sealed post-base hook. It observes one committed base change
through its exact pre/post views and returns the corresponding app
payload or a rejection.
-}
type BaseHook s err =
    BaseChange -> GroupView -> GroupView -> s -> Either err s

{- | Rejection identities of the integrated boundary: a substrate
admissibility refusal, or the application's own.
-}
data IntegratedError err
    = IEValidation ValidationError
    | IEApp err
    deriving stock (Show, Eq)

{- | Success: the new aggregate, and the concrete base change it
committed if it committed one.
-}
data IntegratedResult s = IntegratedResult
    { irState :: GroupState s
    , irChange :: Maybe BaseChange
    }
    deriving stock (Show, Eq)

{- | The contract bundle an application supplies to the boundary. It
exposes no function from an unrestricted generic proposal into the voted
vocabulary: 'intProposalMutation' lands in 'BaseMutation', which cannot
admit.
-}
data Integration s e bp err = Integration
    { intReserved :: Text
    , intDigest :: bp -> ProposalId
    , intProposalMutation :: bp -> BaseMutation
    , intAppFold :: IntegratedAppFold s e err
    , intBaseHook :: BaseHook s err
    }

{- | The sole member insertion. Nothing else in this module writes a new
key into the members relation.
-}
admitMemberInto
    :: GroupState s -> Text -> Text -> Set Role -> GroupState s
admitMemberInto gs key email roles =
    gs
        { members =
            Map.insert
                key
                Member
                    { memberKey = key
                    , memberEmail = email
                    , memberRoles = roles
                    }
                (members gs)
        }

{- | The voted base effects. Exhaustive over 'BaseMutation'; neither arm
can introduce a key.
-}
enactMutation :: GroupState s -> BaseMutation -> GroupState s
enactMutation gs = \case
    RemoveMemberVoted key ->
        gs{members = Map.delete key (members gs)}
    ChangeRolesVoted key roles ->
        gs
            { members =
                Map.adjust (\m -> m{memberRoles = roles}) key (members gs)
            }

{- | The observable evidence a voted mutation commits. Kept separate from
the effect so a route cannot report one change while performing another.
-}
mutationChange :: BaseMutation -> BaseChange
mutationChange = \case
    RemoveMemberVoted key -> MemberRemoved key
    ChangeRolesVoted key _ -> RolesChanged key

{- | Commit a base change together with its consequences, or reject both.
The hook sees the exact pre and post canonical views and the
pre-transition payload; its output is the payload the caller observes.
-}
commitBaseChange
    :: Integration s e bp err
    -> GroupState s
    -> GroupState s
    -> BaseChange
    -> Either (IntegratedError err) (IntegratedResult s)
commitBaseChange integration pre post change =
    case intBaseHook
        integration
        change
        (groupView pre)
        (groupView post)
        (appFold pre) of
        Right appState ->
            Right
                IntegratedResult
                    { irState = post{appFold = appState}
                    , irChange = Just change
                    }
        Left err -> Left (IEApp err)

{- | Enact a pending base mutation once its approvals reach the majority
of the current franchise; otherwise leave it pending and report no
change.
-}
tryEnactBase
    :: Integration s e bp err
    -> GroupState s
    -> ProposalId
    -> Either (IntegratedError err) (IntegratedResult s)
tryEnactBase integration gs proposalId =
    case lookupPendingBase proposalId gs of
        Nothing ->
            Right
                IntegratedResult{irState = gs, irChange = Nothing}
        Just pending ->
            if Set.size (pbApprovals pending) >= majority gs
                then
                    let cleared =
                            gs
                                { pendingBase =
                                    Map.delete proposalId (pendingBase gs)
                                }
                        effected =
                            enactMutation cleared (pbMutation pending)
                    in  commitBaseChange
                            integration
                            gs
                            effected
                            (mutationChange (pbMutation pending))
                else
                    Right
                        IntegratedResult{irState = gs, irChange = Nothing}

{- | The one integrated transition. Validation dominates the effect on
every route: an app event from a non-member reaches no fold, and no base
route reaches an effect without its admissibility decision.
-}
applyIntegratedEvent
    :: Integration s e bp err
    -> GroupState s
    -> Text
    -> IntegratedEvent bp e
    -> Either (IntegratedError err) (IntegratedResult s)
applyIntegratedEvent integration gs signer event = case event of
    Evt.IEDirect (AdmitMember key email roles) ->
        case validateDirectAdmission
            (intReserved integration)
            gs
            signer
            key
            email
            roles of
            Left err -> Left (IEValidation err)
            Right () ->
                commitBaseChange
                    integration
                    gs
                    (admitMemberInto gs key email roles)
                    (MemberAdmitted key)
    Evt.IEPropose proposal ->
        let mutation = intProposalMutation integration proposal
        in  case validateBaseMutation gs signer mutation of
                Left err -> Left (IEValidation err)
                Right () ->
                    let proposalId = intDigest integration proposal
                        pending =
                            PendingBase
                                { pbMutation = mutation
                                , pbProposer = signer
                                , pbApprovals = Set.singleton signer
                                }
                    in  tryEnactBase
                            integration
                            gs
                                { pendingBase =
                                    Map.insert proposalId pending (pendingBase gs)
                                }
                            proposalId
    Evt.IEApprove proposalId ->
        case validateBaseApproval gs signer proposalId of
            Left err -> Left (IEValidation err)
            Right () ->
                case lookupPendingBase proposalId gs of
                    Nothing ->
                        Left (IEValidation (ProposalNotFound proposalId))
                    Just pending ->
                        let approved =
                                pending
                                    { pbApprovals =
                                        Set.insert signer (pbApprovals pending)
                                    }
                        in  tryEnactBase
                                integration
                                gs
                                    { pendingBase =
                                        Map.insert proposalId approved (pendingBase gs)
                                    }
                                proposalId
    Evt.IEApp appEvent ->
        let view = groupView gs
        in  if True
                then case intAppFold integration signer view view (appFold gs) appEvent of
                    Right appState ->
                        Right
                            IntegratedResult
                                { irState = gs{appFold = appState}
                                , irChange = Nothing
                                }
                    Left err -> Left (IEApp err)
                else Left (IEValidation (NotAMember signer))

{- | The integrated fold: every signed integrated event, in order, from
a starting app payload. A rejected event leaves the aggregate exactly as
it was, so a refusal cannot advance state.
-}
foldIntegrated
    :: Integration s e bp err
    -> s
    -> [(Text, IntegratedEvent bp e)]
    -> GroupState s
foldIntegrated integration initial =
    foldl
        ( \gs (signer, evt) ->
            case applyIntegratedEvent integration gs signer evt of
                Right result -> irState result
                Left _ -> gs
        )
        (emptyState initial)

{- | The replay primitive: the same shared step as 'foldIntegrated',
starting from a founding aggregate instead of an empty one. Used by the
integrated store to replay persisted rows over the stored founding. The
refusal arm matches 'foldIntegrated' by construction.
-}
foldIntegratedFrom
    :: Integration s e bp err
    -> GroupState s
    -> [(Text, IntegratedEvent bp e)]
    -> GroupState s
foldIntegratedFrom integration =
    foldl
        ( \gs (signer, evt) ->
            case applyIntegratedEvent integration gs signer evt of
                Right result -> irState result
                Left _ -> gs
        )
