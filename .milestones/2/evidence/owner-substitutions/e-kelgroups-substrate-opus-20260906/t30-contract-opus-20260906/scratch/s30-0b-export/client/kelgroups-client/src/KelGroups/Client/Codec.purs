-- | Argonaut codecs matching Haskell KelGroups.Server.JSON.
module KelGroups.Client.Codec
  ( encodeAdmin
  , decodeAdmin
  , encodeRole
  , decodeRole
  , encodeProposal
  , decodeProposal
  , encodeBaseEvent
  , decodeBaseEvent
  , encodeGroupEvent
  , decodeGroupEvent
  , encodeMember
  , decodeMember
  , encodeSubmission
  , decodeAppendResult
  , decodeServerError
  , decodeInfoResponse
  ) where

import Prelude

import Data.Argonaut.Core (Json, jsonNull)
import Data.Argonaut.Decode
  ( JsonDecodeError(..)
  , decodeJson
  , (.:)
  , (.:?)
  )
import Data.Argonaut.Encode (encodeJson)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Traversable (traverse)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Foreign.Object as FO
import Data.Argonaut.Core as J
import KelGroups.Client.Event
  ( BaseEvent(..)
  , GroupEvent(..)
  , Proposal(..)
  )
import KelGroups.Client.Types (Admin(..), Member, Role(..))

-- --------------------------------------------------------
-- Admin
-- --------------------------------------------------------

encodeAdmin :: Admin -> Json
encodeAdmin PublicAdmin = encodeJson "publicAdmin"
encodeAdmin PrivateAdmin = encodeJson "privateAdmin"

decodeAdmin :: Json -> Either JsonDecodeError Admin
decodeAdmin json = case decodeJson json of
  Right ("publicAdmin" :: String) -> Right PublicAdmin
  Right ("privateAdmin" :: String) -> Right PrivateAdmin
  Right _ -> Left $ TypeMismatch "unknown admin string"
  Left e -> Left e

-- --------------------------------------------------------
-- Role
-- --------------------------------------------------------

encodeRole :: Role -> Json
encodeRole (AdminRole adm) =
  J.fromObject $ FO.fromFoldable
    [ Tuple "adminRole" (encodeAdmin adm) ]
encodeRole (AppRole name) =
  J.fromObject $ FO.fromFoldable
    [ Tuple "appRole" (encodeJson name) ]

decodeRole :: Json -> Either JsonDecodeError Role
decodeRole json = do
  obj <- decodeJson json
  let
    tryAdmin = do
      admJson <- obj .: "adminRole"
      adm <- decodeAdmin admJson
      pure (AdminRole adm)
    tryApp = do
      name <- obj .: "appRole"
      pure (AppRole name)
  case tryAdmin of
    Right r -> Right r
    Left _ -> tryApp

-- --------------------------------------------------------
-- Proposal
-- --------------------------------------------------------

encodeProposal :: Proposal -> Json
encodeProposal (IntroduceMember key email roles) =
  J.fromObject $ FO.fromFoldable
    [ Tuple "tag" (encodeJson "introduce")
    , Tuple "key" (encodeJson key)
    , Tuple "email" (encodeJson email)
    , Tuple "roles" (encodeJson (map encodeRole (Array.fromFoldable roles)))
    ]
encodeProposal (RemoveMember key) =
  J.fromObject $ FO.fromFoldable
    [ Tuple "tag" (encodeJson "remove")
    , Tuple "key" (encodeJson key)
    ]
encodeProposal (ChangeRoles key roles) =
  J.fromObject $ FO.fromFoldable
    [ Tuple "tag" (encodeJson "changeRoles")
    , Tuple "key" (encodeJson key)
    , Tuple "roles" (encodeJson (map encodeRole (Array.fromFoldable roles)))
    ]

decodeProposal :: Json -> Either JsonDecodeError Proposal
decodeProposal json = do
  obj <- decodeJson json
  tag :: String <- obj .: "tag"
  case tag of
    "introduce" -> do
      key <- obj .: "key"
      email <- obj .: "email"
      rolesArr :: Array Json <- obj .: "roles"
      roles <- traverse decodeRole rolesArr
      pure (IntroduceMember key email (Set.fromFoldable roles))
    "remove" -> do
      key <- obj .: "key"
      pure (RemoveMember key)
    "changeRoles" -> do
      key <- obj .: "key"
      rolesArr :: Array Json <- obj .: "roles"
      roles <- traverse decodeRole rolesArr
      pure (ChangeRoles key (Set.fromFoldable roles))
    _ -> Left $ TypeMismatch ("unknown Proposal tag: " <> tag)

-- --------------------------------------------------------
-- BaseEvent
-- --------------------------------------------------------

encodeBaseEvent :: BaseEvent -> Json
encodeBaseEvent (Propose p) =
  J.fromObject $ FO.fromFoldable
    [ Tuple "tag" (encodeJson "propose")
    , Tuple "proposal" (encodeProposal p)
    ]
encodeBaseEvent (Approve pid) =
  J.fromObject $ FO.fromFoldable
    [ Tuple "tag" (encodeJson "approve")
    , Tuple "proposalId" (encodeJson pid)
    ]

decodeBaseEvent :: Json -> Either JsonDecodeError BaseEvent
decodeBaseEvent json = do
  obj <- decodeJson json
  tag :: String <- obj .: "tag"
  case tag of
    "propose" -> do
      pJson <- obj .: "proposal"
      p <- decodeProposal pJson
      pure (Propose p)
    "approve" -> do
      pid <- obj .: "proposalId"
      pure (Approve pid)
    _ -> Left $ TypeMismatch ("unknown BaseEvent tag: " <> tag)

-- --------------------------------------------------------
-- GroupEvent a
-- --------------------------------------------------------

encodeGroupEvent
  :: forall a. (a -> Json) -> GroupEvent a -> Json
encodeGroupEvent _ (Base be) =
  J.fromObject $ FO.fromFoldable
    [ Tuple "tag" (encodeJson "base")
    , Tuple "event" (encodeBaseEvent be)
    ]
encodeGroupEvent encA (App a) =
  J.fromObject $ FO.fromFoldable
    [ Tuple "tag" (encodeJson "app")
    , Tuple "event" (encA a)
    ]

decodeGroupEvent
  :: forall a
   . (Json -> Either JsonDecodeError a)
  -> Json
  -> Either JsonDecodeError (GroupEvent a)
decodeGroupEvent decA json = do
  obj <- decodeJson json
  tag :: String <- obj .: "tag"
  case tag of
    "base" -> do
      evtJson <- obj .: "event"
      be <- decodeBaseEvent evtJson
      pure (Base be)
    "app" -> do
      evtJson <- obj .: "event"
      a <- decA evtJson
      pure (App a)
    _ -> Left $ TypeMismatch ("unknown GroupEvent tag: " <> tag)

-- --------------------------------------------------------
-- Member
-- --------------------------------------------------------

encodeMember :: Member -> Json
encodeMember m =
  J.fromObject $ FO.fromFoldable
    [ Tuple "key" (encodeJson m.key)
    , Tuple "email" (encodeJson m.email)
    , Tuple "roles" (encodeJson (map encodeRole (Array.fromFoldable m.roles)))
    ]

decodeMember :: Json -> Either JsonDecodeError Member
decodeMember json = do
  obj <- decodeJson json
  key <- obj .: "key"
  email <- obj .: "email"
  rolesArr :: Array Json <- obj .: "roles"
  roles <- traverse decodeRole rolesArr
  pure { key, email, roles: Set.fromFoldable roles }

-- --------------------------------------------------------
-- Submission (for POST /events)
-- --------------------------------------------------------

encodeSubmission
  :: forall a
   . (a -> Json)
  -> { passphrase :: Maybe String, signer :: String, signature :: String, event :: GroupEvent a }
  -> Json
encodeSubmission encA sub =
  J.fromObject $ FO.fromFoldable
    [ Tuple "passphrase"
        ( case sub.passphrase of
            Nothing -> jsonNull
            Just p -> encodeJson p
        )
    , Tuple "signer" (encodeJson sub.signer)
    , Tuple "signature" (encodeJson sub.signature)
    , Tuple "event" (encodeGroupEvent encA sub.event)
    ]

-- --------------------------------------------------------
-- AppendResult
-- --------------------------------------------------------

decodeAppendResult :: Json -> Either JsonDecodeError { sequenceNumber :: Int }
decodeAppendResult json = do
  obj <- decodeJson json
  sn <- obj .: "sequenceNumber"
  pure { sequenceNumber: sn }

-- --------------------------------------------------------
-- ServerError
-- --------------------------------------------------------

decodeServerError :: Json -> Either JsonDecodeError { error :: String, message :: Maybe String }
decodeServerError json = do
  obj <- decodeJson json
  err <- obj .: "error"
  msg <- obj .:? "message"
  pure { error: err, message: msg }

-- --------------------------------------------------------
-- InfoResponse (GET /info)
-- --------------------------------------------------------

decodeInfoResponse
  :: Json
  -> Either JsonDecodeError
       { publicAdminEmails :: Array String
       , pendingIntroduction :: Boolean
       }
decodeInfoResponse json = do
  obj <- decodeJson json
  emails <- obj .: "publicAdminEmails"
  pending <- obj .: "pendingIntroduction"
  pure { publicAdminEmails: emails, pendingIntroduction: pending }
