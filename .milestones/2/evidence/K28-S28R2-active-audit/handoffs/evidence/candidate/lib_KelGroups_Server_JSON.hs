{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : KelGroups.Server.JSON
Description : Aeson instances for KEL types and HTTP types
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Orphan 'ToJSON' and 'FromJSON' instances for all event
and state types. Kept separate to avoid an @aeson@
dependency in the core modules. Also defines HTTP-specific
types: 'Submission', 'AppendResult', 'ServerError'.
-}
module KelGroups.Server.JSON
    ( Submission (..)
    , AppendResult (..)
    , ServerError (..)
    ) where

import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , Value (..)
    , object
    , withObject
    , withText
    , (.!=)
    , (.:)
    , (.:?)
    , (.=)
    )
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import KelGroups.Bootstrap (AuthMode (..))
import KelGroups.Event
    ( BaseChange (..)
    , BaseEvent (..)
    , BaseMutation (..)
    , DirectCommand (..)
    , GroupEvent (..)
    , IntegratedEvent (..)
    , Proposal (..)
    )
import KelGroups.State
    ( GroupState (..)
    , PendingBase (..)
    , PendingProposal (..)
    )
import KelGroups.Types
    ( Admin (..)
    , Member (..)
    , Role (..)
    )
import KelGroups.Validate (ValidationError (..))

-- --------------------------------------------------------
-- HTTP-specific types
-- --------------------------------------------------------

-- | A submission from a client to append an event.
data Submission a = Submission
    { subPassphrase :: Maybe Text
    -- ^ Required in bootstrap mode
    , subSigner :: Text
    -- ^ CESR-encoded public key
    , subSignature :: Text
    -- ^ CESR-encoded Ed25519 signature
    , subPriorDigest :: Maybe Text
    -- ^ SAID of last event (stale-tip detection)
    , subEvent :: GroupEvent a
    -- ^ The event to append
    }
    deriving stock (Show, Eq)

-- | Successful append result.
newtype AppendResult = AppendResult
    { sequenceNumber :: Int
    }
    deriving stock (Show, Eq)

-- | Server-level errors.
data ServerError
    = ValidationErr ValidationError
    | PassphraseRequired
    | WrongPassphrase
    | SignatureError Text
    | StaleTip Text Text
    | BadRequest Text
    deriving stock (Show, Eq)

-- --------------------------------------------------------
-- Admin
-- --------------------------------------------------------

instance ToJSON Admin where
    toJSON PublicAdmin = String "publicAdmin"
    toJSON PrivateAdmin = String "privateAdmin"

instance FromJSON Admin where
    parseJSON = withText "Admin" $ \case
        "publicAdmin" -> pure PublicAdmin
        "privateAdmin" -> pure PrivateAdmin
        t -> fail $ "unknown Admin: " <> show t

-- --------------------------------------------------------
-- Role
-- --------------------------------------------------------

instance ToJSON Role where
    toJSON (AdminRole adm) =
        object ["adminRole" .= adm]
    toJSON (AppRole name) =
        object ["appRole" .= name]

instance FromJSON Role where
    parseJSON = withObject "Role" $ \o -> do
        mAdmin <- o .:? "adminRole"
        mApp <- o .:? "appRole"
        case (mAdmin :: Maybe Admin, mApp :: Maybe Text) of
            (Just adm, _) -> pure (AdminRole adm)
            (_, Just name) -> pure (AppRole name)
            _ -> fail "unknown Role"

-- --------------------------------------------------------
-- Proposal
-- --------------------------------------------------------

instance ToJSON Proposal where
    toJSON (IntroduceMember key email roles) =
        object
            [ "tag" .= ("introduce" :: Text)
            , "key" .= key
            , "email" .= email
            , "roles" .= Set.toList roles
            ]
    toJSON (RemoveMember key) =
        object
            [ "tag" .= ("remove" :: Text)
            , "key" .= key
            ]
    toJSON (ChangeRoles key roles) =
        object
            [ "tag" .= ("changeRoles" :: Text)
            , "key" .= key
            , "roles" .= Set.toList roles
            ]

instance FromJSON Proposal where
    parseJSON = withObject "Proposal" $ \o -> do
        (tag :: Text) <- o .: "tag"
        case tag of
            "introduce" ->
                IntroduceMember
                    <$> o .: "key"
                    <*> o .: "email"
                    <*> (Set.fromList <$> o .: "roles")
            "remove" ->
                RemoveMember <$> o .: "key"
            "changeRoles" ->
                ChangeRoles
                    <$> o .: "key"
                    <*> (Set.fromList <$> o .: "roles")
            _ -> fail $ "unknown Proposal tag: " <> show tag

-- --------------------------------------------------------
-- BaseEvent
-- --------------------------------------------------------

instance ToJSON BaseEvent where
    toJSON (Propose p) =
        object
            [ "tag" .= ("propose" :: Text)
            , "proposal" .= p
            ]
    toJSON (Approve pid) =
        object
            [ "tag" .= ("approve" :: Text)
            , "proposalId" .= pid
            ]

instance FromJSON BaseEvent where
    parseJSON = withObject "BaseEvent" $ \o -> do
        (tag :: Text) <- o .: "tag"
        case tag of
            "propose" -> Propose <$> o .: "proposal"
            "approve" -> Approve <$> o .: "proposalId"
            _ ->
                fail $
                    "unknown BaseEvent tag: " <> show tag

-- --------------------------------------------------------
-- GroupEvent a
-- --------------------------------------------------------

instance (ToJSON a) => ToJSON (GroupEvent a) where
    toJSON (Base be) =
        object
            [ "tag" .= ("base" :: Text)
            , "event" .= be
            ]
    toJSON (App a) =
        object
            [ "tag" .= ("app" :: Text)
            , "event" .= a
            ]

instance (FromJSON a) => FromJSON (GroupEvent a) where
    parseJSON = withObject "GroupEvent" $ \o -> do
        (tag :: Text) <- o .: "tag"
        case tag of
            "base" -> Base <$> o .: "event"
            "app" -> App <$> o .: "event"
            _ ->
                fail $
                    "unknown GroupEvent tag: " <> show tag

-- --------------------------------------------------------
-- Member
-- --------------------------------------------------------

instance ToJSON Member where
    toJSON m =
        object
            [ "key" .= memberKey m
            , "email" .= memberEmail m
            , "roles" .= Set.toList (memberRoles m)
            ]

instance FromJSON Member where
    parseJSON = withObject "Member" $ \o ->
        Member
            <$> o .: "key"
            <*> o .: "email"
            <*> (Set.fromList <$> o .: "roles")

-- --------------------------------------------------------
-- PendingProposal
-- --------------------------------------------------------

instance ToJSON PendingProposal where
    toJSON pp =
        object
            [ "proposal" .= proposal pp
            , "proposer" .= proposer pp
            , "approvals" .= Set.toList (approvals pp)
            ]

instance FromJSON PendingProposal where
    parseJSON = withObject "PendingProposal" $ \o ->
        PendingProposal
            <$> o .: "proposal"
            <*> o .: "proposer"
            <*> (Set.fromList <$> o .: "approvals")

-- --------------------------------------------------------
-- GroupState a
-- --------------------------------------------------------

instance (ToJSON a) => ToJSON (GroupState a) where
    toJSON gs =
        object
            [ "members" .= Map.elems (members gs)
            , "pendingProposals"
                .= Map.toList (pendingProposals gs)
            , "pendingBase" .= Map.toList (pendingBase gs)
            , "appFold" .= appFold gs
            ]

instance (FromJSON a) => FromJSON (GroupState a) where
    parseJSON = withObject "GroupState" $ \o -> do
        (ms :: [Member]) <- o .: "members"
        let memberMap =
                Map.fromList
                    [(memberKey m, m) | m <- ms]
        pps <- o .: "pendingProposals"
        pbs <- o .:? "pendingBase" .!= []
        af <- o .: "appFold"
        pure
            GroupState
                { members = memberMap
                , pendingProposals = Map.fromList pps
                , pendingBase = Map.fromList pbs
                , appFold = af
                }

-- --------------------------------------------------------
-- AuthMode
-- --------------------------------------------------------

instance ToJSON AuthMode where
    toJSON Bootstrap = String "bootstrap"
    toJSON Normal = String "normal"

instance FromJSON AuthMode where
    parseJSON = withText "AuthMode" $ \case
        "bootstrap" -> pure Bootstrap
        "normal" -> pure Normal
        t -> fail $ "unknown AuthMode: " <> show t

-- --------------------------------------------------------
-- ValidationError
-- --------------------------------------------------------

instance ToJSON ValidationError where
    toJSON (NotAMember k) =
        object
            [ "error" .= ("notAMember" :: Text)
            , "key" .= k
            ]
    toJSON (NotAnAdmin k) =
        object
            [ "error" .= ("notAnAdmin" :: Text)
            , "key" .= k
            ]
    toJSON BootstrapRequiresAdmin =
        object
            ["error" .= ("bootstrapRequiresAdmin" :: Text)]
    toJSON (MemberAlreadyExists k) =
        object
            [ "error" .= ("memberAlreadyExists" :: Text)
            , "key" .= k
            ]
    toJSON (MemberNotFound k) =
        object
            [ "error" .= ("memberNotFound" :: Text)
            , "key" .= k
            ]
    toJSON (ProposalNotFound pid) =
        object
            [ "error" .= ("proposalNotFound" :: Text)
            , "proposalId" .= pid
            ]
    toJSON (AlreadyApproved k pid) =
        object
            [ "error" .= ("alreadyApproved" :: Text)
            , "key" .= k
            , "proposalId" .= pid
            ]
    toJSON (RoleAddPrecondition name) =
        object
            [ "error" .= ("roleAddPrecondition" :: Text)
            , "role" .= name
            ]
    toJSON (RoleRemovePrecondition name) =
        object
            [ "error" .= ("roleRemovePrecondition" :: Text)
            , "role" .= name
            ]
    toJSON (InvalidKey k) =
        object
            [ "error" .= ("invalidKey" :: Text)
            , "key" .= k
            ]
    toJSON (ReservedKey k) =
        object
            [ "error" .= ("reservedKey" :: Text)
            , "key" .= k
            ]

instance FromJSON ValidationError where
    parseJSON = withObject "ValidationError" $ \o -> do
        (tag :: Text) <- o .: "error"
        case tag of
            "notAMember" ->
                NotAMember <$> o .: "key"
            "notAnAdmin" ->
                NotAnAdmin <$> o .: "key"
            "bootstrapRequiresAdmin" ->
                pure BootstrapRequiresAdmin
            "memberAlreadyExists" ->
                MemberAlreadyExists <$> o .: "key"
            "memberNotFound" ->
                MemberNotFound <$> o .: "key"
            "proposalNotFound" ->
                ProposalNotFound <$> o .: "proposalId"
            "alreadyApproved" ->
                AlreadyApproved
                    <$> o .: "key"
                    <*> o .: "proposalId"
            "roleAddPrecondition" ->
                RoleAddPrecondition <$> o .: "role"
            "roleRemovePrecondition" ->
                RoleRemovePrecondition <$> o .: "role"
            "invalidKey" ->
                InvalidKey <$> o .: "key"
            "reservedKey" ->
                ReservedKey <$> o .: "key"
            _ ->
                fail $
                    "unknown ValidationError: "
                        <> show tag

-- --------------------------------------------------------
-- Submission
-- --------------------------------------------------------

instance (ToJSON a) => ToJSON (Submission a) where
    toJSON s =
        object
            [ "passphrase" .= subPassphrase s
            , "signer" .= subSigner s
            , "signature" .= subSignature s
            , "priorDigest" .= subPriorDigest s
            , "event" .= subEvent s
            ]

instance (FromJSON a) => FromJSON (Submission a) where
    parseJSON = withObject "Submission" $ \o ->
        Submission
            <$> o .:? "passphrase"
            <*> o .: "signer"
            <*> o .: "signature"
            <*> o .:? "priorDigest"
            <*> o .: "event"

-- --------------------------------------------------------
-- AppendResult
-- --------------------------------------------------------

instance ToJSON AppendResult where
    toJSON r =
        object
            ["sequenceNumber" .= sequenceNumber r]

instance FromJSON AppendResult where
    parseJSON = withObject "AppendResult" $ \o ->
        AppendResult <$> o .: "sequenceNumber"

-- --------------------------------------------------------
-- ServerError
-- --------------------------------------------------------

instance ToJSON ServerError where
    toJSON (ValidationErr ve) =
        object
            [ "error" .= ("validationError" :: Text)
            , "detail" .= ve
            ]
    toJSON PassphraseRequired =
        object
            ["error" .= ("passphraseRequired" :: Text)]
    toJSON WrongPassphrase =
        object
            ["error" .= ("wrongPassphrase" :: Text)]
    toJSON (SignatureError msg) =
        object
            [ "error" .= ("signatureError" :: Text)
            , "message" .= msg
            ]
    toJSON (StaleTip expected got) =
        object
            [ "error" .= ("staleTip" :: Text)
            , "expected" .= expected
            , "got" .= got
            ]
    toJSON (BadRequest msg) =
        object
            [ "error" .= ("badRequest" :: Text)
            , "message" .= msg
            ]

instance FromJSON ServerError where
    parseJSON = withObject "ServerError" $ \o -> do
        (tag :: Text) <- o .: "error"
        case tag of
            "validationError" ->
                ValidationErr <$> o .: "detail"
            "passphraseRequired" ->
                pure PassphraseRequired
            "wrongPassphrase" ->
                pure WrongPassphrase
            "signatureError" ->
                SignatureError <$> o .: "message"
            "staleTip" ->
                StaleTip
                    <$> o .: "expected"
                    <*> o .: "got"
            "badRequest" ->
                BadRequest <$> o .: "message"
            _ ->
                fail $
                    "unknown ServerError: " <> show tag

-- --------------------------------------------------------
-- Integrated vocabulary (S28-1 production codecs)
-- --------------------------------------------------------
-- Orphan instances for the integrated store persist/replay path
-- ('openIntegratedKEL'/'appendIntegratedEvent'). Tag shapes mirror the
-- historical codecs above; the integrated tags are distinct so the two
-- logs can never be confused.

instance ToJSON DirectCommand where
    toJSON (AdmitMember key email roles) =
        object
            [ "tag" .= ("admitMember" :: Text)
            , "key" .= key
            , "email" .= email
            , "roles" .= Set.toList roles
            ]

instance FromJSON DirectCommand where
    parseJSON = withObject "DirectCommand" $ \o -> do
        (tag :: Text) <- o .: "tag"
        case tag of
            "admitMember" ->
                AdmitMember
                    <$> o .: "key"
                    <*> o .: "email"
                    <*> (Set.fromList <$> o .: "roles")
            _ ->
                fail $
                    "unknown DirectCommand tag: " <> show tag

instance ToJSON BaseMutation where
    toJSON (RemoveMemberVoted key) =
        object
            [ "tag" .= ("removeMemberVoted" :: Text)
            , "key" .= key
            ]
    toJSON (ChangeRolesVoted key roles) =
        object
            [ "tag" .= ("changeRolesVoted" :: Text)
            , "key" .= key
            , "roles" .= Set.toList roles
            ]

instance FromJSON BaseMutation where
    parseJSON = withObject "BaseMutation" $ \o -> do
        (tag :: Text) <- o .: "tag"
        case tag of
            "removeMemberVoted" ->
                RemoveMemberVoted <$> o .: "key"
            "changeRolesVoted" ->
                ChangeRolesVoted
                    <$> o .: "key"
                    <*> (Set.fromList <$> o .: "roles")
            _ ->
                fail $
                    "unknown BaseMutation tag: " <> show tag

instance ToJSON BaseChange where
    toJSON (MemberAdmitted key) =
        object
            [ "tag" .= ("memberAdmitted" :: Text)
            , "key" .= key
            ]
    toJSON (MemberRemoved key) =
        object
            [ "tag" .= ("memberRemoved" :: Text)
            , "key" .= key
            ]
    toJSON (RolesChanged key) =
        object
            [ "tag" .= ("rolesChanged" :: Text)
            , "key" .= key
            ]

instance FromJSON BaseChange where
    parseJSON = withObject "BaseChange" $ \o -> do
        (tag :: Text) <- o .: "tag"
        case tag of
            "memberAdmitted" ->
                MemberAdmitted <$> o .: "key"
            "memberRemoved" ->
                MemberRemoved <$> o .: "key"
            "rolesChanged" ->
                RolesChanged <$> o .: "key"
            _ ->
                fail $
                    "unknown BaseChange tag: " <> show tag

instance (ToJSON bp, ToJSON e) => ToJSON (IntegratedEvent bp e) where
    toJSON (IEDirect cmd) =
        object
            [ "tag" .= ("direct" :: Text)
            , "command" .= cmd
            ]
    toJSON (IEPropose proposal) =
        object
            [ "tag" .= ("propose" :: Text)
            , "proposal" .= proposal
            ]
    toJSON (IEApprove proposalId) =
        object
            [ "tag" .= ("approve" :: Text)
            , "proposalId" .= proposalId
            ]
    toJSON (IEApp event) =
        object
            [ "tag" .= ("app" :: Text)
            , "event" .= event
            ]

instance (FromJSON bp, FromJSON e) => FromJSON (IntegratedEvent bp e) where
    parseJSON = withObject "IntegratedEvent" $ \o -> do
        (tag :: Text) <- o .: "tag"
        case tag of
            "direct" -> IEDirect <$> o .: "command"
            "propose" -> IEPropose <$> o .: "proposal"
            "approve" -> IEApprove <$> o .: "proposalId"
            "app" -> IEApp <$> o .: "event"
            _ ->
                fail $
                    "unknown IntegratedEvent tag: " <> show tag

instance ToJSON PendingBase where
    toJSON pp =
        object
            [ "mutation" .= pbMutation pp
            , "proposer" .= pbProposer pp
            , "approvals" .= Set.toList (pbApprovals pp)
            ]

instance FromJSON PendingBase where
    parseJSON = withObject "PendingBase" $ \o ->
        PendingBase
            <$> o .: "mutation"
            <*> o .: "proposer"
            <*> (Set.fromList <$> o .: "approvals")
