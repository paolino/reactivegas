{- |
Module      : KelGroups.Validate
Description : Event validation against group condition
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Validates new events before they are appended to the
KEL. Checks authentication mode, admin majority,
and role preconditions.
-}
module KelGroups.Validate
    ( validateEvent
    , ValidationError (..)
    , validateDirectAdmission
    , validateBaseMutation
    , validateBaseApproval
    ) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import KelGroups.Bootstrap (AuthMode (..), authMode)
import KelGroups.Event
    ( BaseEvent (..)
    , BaseMutation (..)
    , GroupEvent (..)
    , Proposal (..)
    )
import KelGroups.State
    ( GroupState (..)
    , PendingBase (..)
    , PendingProposal (..)
    , isAdmin
    , isMember
    , lookupPendingBase
    )
import KelGroups.Types
    ( GroupConfig (..)
    , Member (..)
    , ProposalId
    , Role (..)
    , RoleDef (..)
    , RoleName
    , hasAdmin
    )
import Keri.Cesr (decode)
import Keri.Cesr.DerivationCode (DerivationCode (..))
import Keri.Cesr.Primitive (Primitive (..))

-- | Validation errors for group events.
data ValidationError
    = -- | Signer is not a known member
      NotAMember Text
    | -- | Signer is not an admin
      NotAnAdmin Text
    | -- | Bootstrap requires introducing a member with admin
      BootstrapRequiresAdmin
    | -- | Member already exists
      MemberAlreadyExists Text
    | -- | Member does not exist
      MemberNotFound Text
    | -- | Proposal not found
      ProposalNotFound Text
    | -- | Already approved this proposal
      AlreadyApproved Text Text
    | -- | Role precondition not met for addition
      RoleAddPrecondition RoleName
    | -- | Role precondition not met for removal
      RoleRemovePrecondition RoleName
    | -- | Key is not a valid CESR Ed25519 public key
      InvalidKey Text
    | -- | Key is reserved and may never become a member
      ReservedKey Text
    deriving stock (Show, Eq)

{- | Validate a group event against the current
condition. The signer is the CESR-encoded public key
of the event author.

HISTORICAL-NON-PRODUCTION: 'validateEvent' and the proposal/approval
validators below keep the accepted behavior. The integrated production
routes ('validateDirectAdmission'/'validateBaseMutation'/
'validateBaseApproval') never call these; they receive no new production
responsibility in this slice.
-}
validateEvent
    :: GroupConfig a
    -> GroupState a
    -> Text
    -- ^ Signer's CESR public key
    -> GroupEvent a
    -> Either ValidationError ()
validateEvent config gs signer = \case
    Base baseEvt ->
        validateBase config gs signer baseEvt
    App _ ->
        -- App events only need a valid member signature
        if isMember signer gs
            then Right ()
            else Left (NotAMember signer)

validateBase
    :: GroupConfig a
    -> GroupState a
    -> Text
    -> BaseEvent
    -> Either ValidationError ()
validateBase config gs signer = \case
    Propose proposal' ->
        validateProposal config gs signer proposal'
    Approve proposalId ->
        validateApproval gs signer proposalId

validateProposal
    :: GroupConfig a
    -> GroupState a
    -> Text
    -> Proposal
    -> Either ValidationError ()
validateProposal config gs signer proposal' =
    case authMode gs of
        Bootstrap ->
            validateBootstrapProposal proposal'
        Normal -> do
            requireAdmin signer gs
            validateNormalProposal config gs proposal'

validateBootstrapProposal
    :: Proposal -> Either ValidationError ()
validateBootstrapProposal = \case
    IntroduceMember key _ roles -> do
        requireValidCesrKey key
        if hasAdmin roles
            then Right ()
            else Left BootstrapRequiresAdmin
    _ -> Left BootstrapRequiresAdmin

validateNormalProposal
    :: GroupConfig a
    -> GroupState a
    -> Proposal
    -> Either ValidationError ()
validateNormalProposal config gs = \case
    IntroduceMember pubKey _email roles -> do
        requireValidCesrKey pubKey
        requireNotMember pubKey gs
        validateRoleAdditions config gs roles
    RemoveMember pubKey ->
        requireMember pubKey gs
    ChangeRoles pubKey roles -> do
        requireMember pubKey gs
        validateRoleChanges config gs pubKey roles

validateApproval
    :: GroupState a
    -> Text
    -> Text
    -> Either ValidationError ()
validateApproval gs signer proposalId = do
    requireAdmin signer gs
    case Map.lookup proposalId (pendingProposals gs) of
        Nothing -> Left (ProposalNotFound proposalId)
        Just pp
            | Set.member signer (approvals pp) ->
                Left (AlreadyApproved signer proposalId)
            | otherwise -> Right ()

-- | Check that roles being added pass preconditions.
validateRoleAdditions
    :: GroupConfig a
    -> GroupState a
    -> Set.Set Role
    -> Either ValidationError ()
validateRoleAdditions config gs roles =
    mapM_ checkRole (Set.toList roles)
  where
    checkRole (AdminRole _) = Right ()
    checkRole (AppRole name) =
        case Map.lookup name (roleDefs config) of
            Nothing -> Right ()
            Just rd
                | canAdd rd (appFold gs) -> Right ()
                | otherwise ->
                    Left (RoleAddPrecondition name)

-- | Check role changes including removal preconditions.
validateRoleChanges
    :: GroupConfig a
    -> GroupState a
    -> Text
    -> Set.Set Role
    -> Either ValidationError ()
validateRoleChanges config gs pubKey newRoles =
    case Map.lookup pubKey (members gs) of
        Nothing -> Left (MemberNotFound pubKey)
        Just member' -> do
            let oldRoles = memberRoles member'
                removed =
                    Set.difference oldRoles newRoles
            mapM_ (checkRemoval config gs) $
                Set.toList removed
            validateRoleAdditions config gs $
                Set.difference newRoles oldRoles

checkRemoval
    :: GroupConfig a
    -> GroupState a
    -> Role
    -> Either ValidationError ()
checkRemoval _ _ (AdminRole _) = Right ()
checkRemoval config gs (AppRole name) =
    case Map.lookup name (roleDefs config) of
        Nothing -> Right ()
        Just rd
            | canRemove rd (appFold gs) -> Right ()
            | otherwise ->
                Left (RoleRemovePrecondition name)

requireAdmin
    :: Text
    -> GroupState a
    -> Either ValidationError ()
requireAdmin signer gs
    | isAdmin signer gs = Right ()
    | otherwise = Left (NotAnAdmin signer)

requireMember
    :: Text
    -> GroupState a
    -> Either ValidationError ()
requireMember pubKey gs
    | isMember pubKey gs = Right ()
    | otherwise = Left (MemberNotFound pubKey)

requireNotMember
    :: Text
    -> GroupState a
    -> Either ValidationError ()
requireNotMember pubKey gs
    | isMember pubKey gs =
        Left (MemberAlreadyExists pubKey)
    | otherwise = Right ()

{- | Validate that a key is a CESR-encoded Ed25519
public key.
-}
requireValidCesrKey
    :: Text -> Either ValidationError ()
requireValidCesrKey key =
    case decode key of
        Right Primitive{code = Ed25519PubKey} ->
            Right ()
        _ -> Left (InvalidKey key)

{- | The sole member-insertion validator. Three guards in a fixed order
so the refusal identity is exact: a non-admin signer is 'NotAnAdmin',
the reserved key is 'ReservedKey', and an existing key is
'MemberAlreadyExists'. There is no bootstrap arm: a group with no admin
admits nobody; the founding admin arrives through the application's
guarded initial aggregate.
-}
validateDirectAdmission
    :: Text
    -> GroupState s
    -> Text
    -> Text
    -> Text
    -> Set.Set Role
    -> Either ValidationError ()
validateDirectAdmission reserved gs signer target _email _roles
    | not (isAdmin signer gs) = Left (NotAnAdmin signer)
    | target == reserved = Left (ReservedKey target)
    | isMember target gs = Left (MemberAlreadyExists target)
    | otherwise = Right ()

{- | Admissibility of a voted base mutation. Exhaustive over
'BaseMutation': an added constructor stops this compiling rather than
acquiring a default.
-}
validateBaseMutation
    :: GroupState s -> Text -> BaseMutation -> Either ValidationError ()
validateBaseMutation gs signer = \case
    RemoveMemberVoted key -> do
        requireAdmin signer gs
        requireMember key gs
    ChangeRolesVoted key _ -> do
        requireAdmin signer gs
        requireMember key gs

{- | Admissibility of an approval of a pending base mutation. Reads the
integrated 'pendingBase' store (never the historical one).
-}
validateBaseApproval
    :: GroupState s -> Text -> ProposalId -> Either ValidationError ()
validateBaseApproval gs signer proposalId = do
    requireAdmin signer gs
    case lookupPendingBase proposalId gs of
        Nothing -> Left (ProposalNotFound proposalId)
        Just pending
            | Set.member signer (pbApprovals pending) ->
                Left (AlreadyApproved signer proposalId)
            | otherwise -> Right ()
