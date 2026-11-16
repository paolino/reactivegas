{- |
Module      : KelGroups.Server
Description : HTTP server for kelgroups (WAI application)
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

WAI application providing JSON endpoints for group
management and SSE notifications. Constructs KERI
events (inception for bootstrap, interaction for
normal operations) and verifies signatures against
serialized KERI event bytes.
-}
module KelGroups.Server
    ( ServerEnv (..)
    , mkApp
    , mkKeriEvent
    ) where

import Control.Concurrent.STM
    ( TChan
    , atomically
    , dupTChan
    , readTChan
    , writeTChan
    )
import Data.Aeson
    ( FromJSON
    , ToJSON (..)
    , decode
    , encode
    , object
    , (.=)
    )
import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as Builder
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import KelGroups.Bootstrap (AuthMode (..), authMode)
import KelGroups.Event (GroupEvent (..), Proposal (..))
import KelGroups.Fold (AppFold)
import KelGroups.Server.JSON
    ( AppendResult (..)
    , ServerError (..)
    , Submission (..)
    )
import KelGroups.State
    ( GroupState (..)
    , PendingProposal (..)
    , isMember
    )
import KelGroups.Store
    ( ChainTip (..)
    , KELStore (..)
    , StoredEvent (..)
    , appendEvent
    , chainTip
    , kelLength
    , readEventsFrom
    , readState
    )
import KelGroups.Types
    ( Admin (..)
    , GroupConfig
    , Member (..)
    , Role (..)
    )
import KelGroups.Validate (validateEvent)
import Keri.Cesr qualified as Cesr
import Keri.Cesr.DerivationCode (DerivationCode (..))
import Keri.Cesr.Primitive (Primitive (..))
import Keri.Crypto.Ed25519 qualified as Ed25519
import Keri.Event (Event)
import Keri.Event.Inception
    ( InceptionConfig (..)
    , mkInception
    )
import Keri.Event.Interaction
    ( InteractionConfig (..)
    , mkInteraction
    )
import Keri.Event.Serialize (serializeEvent)
import Network.HTTP.Types
    ( HeaderName
    , Status
    , hContentType
    , status200
    , status400
    , status401
    , status403
    , status404
    , status409
    , status422
    )
import Network.Wai
    ( Application
    , Request
    , Response
    , pathInfo
    , queryString
    , requestMethod
    , responseLBS
    , responseStream
    , strictRequestBody
    )

-- | Server environment shared across all handlers.
data ServerEnv a = ServerEnv
    { envStore :: KELStore a
    -- ^ Persistent event store
    , envConfig :: GroupConfig a
    -- ^ Group configuration (role defs)
    , envAppFold :: AppFold a
    -- ^ Application fold function
    , envPassphrase :: Text
    -- ^ Bootstrap passphrase
    , envBroadcast :: TChan Int
    -- ^ SSE broadcast channel
    }

{- | Build a WAI 'Application' from a 'ServerEnv'.
Unmatched routes are passed to the optional fallback
application, or return 404.
-}
mkApp
    :: (FromJSON a, ToJSON a)
    => ServerEnv a
    -> Maybe Application
    -- ^ Optional fallback for unmatched routes
    -> Application
mkApp env mFallback req respond =
    case (requestMethod req, pathInfo req) of
        ("GET", ["info"]) ->
            handleInfo env req respond
        ("GET", ["condition"]) ->
            requireMemberGuard env req respond $
                handleCondition env req respond
        ("GET", ["events"]) ->
            requireMemberGuard env req respond $
                handleGetEvent env req respond
        ("POST", ["events"]) ->
            handlePostEvent env req respond
        ("GET", ["stream"]) ->
            requireMemberGuard env req respond $
                handleStream env req respond
        _ -> case mFallback of
            Just fallback -> fallback req respond
            Nothing ->
                respond $
                    jsonResponse status404 $
                        BadRequest "not found"

-- --------------------------------------------------------
-- GET /condition
-- --------------------------------------------------------

handleCondition
    :: (ToJSON a)
    => ServerEnv a
    -> Application
handleCondition env _req respond = do
    gs <- readState (envStore env)
    respond $
        responseLBS
            status200
            jsonHeaders
            (encode $ conditionBody gs)

conditionBody
    :: GroupState a -> ConditionResponse a
conditionBody gs =
    ConditionResponse
        { crState = gs
        , crAuthMode = authMode gs
        }

-- | Internal type for the /condition response.
data ConditionResponse a = ConditionResponse
    { crState :: GroupState a
    , crAuthMode :: AuthMode
    }

instance (ToJSON a) => ToJSON (ConditionResponse a) where
    toJSON cr =
        object
            [ "state" .= crState cr
            , "authMode" .= crAuthMode cr
            ]

-- --------------------------------------------------------
-- GET /events?after=N
-- --------------------------------------------------------

handleGetEvent
    :: ServerEnv a
    -> Application
handleGetEvent env req respond =
    case parseAfter req of
        Nothing ->
            respond $
                jsonResponse status400 $
                    BadRequest "missing or invalid ?after=N"
        Just after -> do
            events <-
                readEventsFrom
                    (envStore env)
                    (after + 1)
            case events of
                [] ->
                    respond $
                        jsonResponse status404 $
                            BadRequest
                                "no event at position"
                (se : _) ->
                    respond $
                        responseLBS
                            status200
                            jsonHeaders
                            ( encode $
                                object
                                    [ "signer"
                                        .= seSigner se
                                    , "event"
                                        .= TE.decodeUtf8
                                            ( seEventBytes
                                                se
                                            )
                                    , "signature"
                                        .= seSignature se
                                    ]
                            )

-- | Parse the ?after=N query parameter.
parseAfter :: Request -> Maybe Int
parseAfter req =
    case lookup "after" (queryString req) of
        Just (Just bs) ->
            case TR.signed TR.decimal (TE.decodeUtf8 bs) of
                Right (n, _) -> Just n
                Left _ -> Nothing
        _ -> Nothing

-- --------------------------------------------------------
-- POST /events
-- --------------------------------------------------------

handlePostEvent
    :: (FromJSON a, ToJSON a)
    => ServerEnv a
    -> Application
handlePostEvent env req respond = do
    body <- strictRequestBody req
    case decode body of
        Nothing ->
            respond $
                jsonResponse status400 $
                    BadRequest "invalid JSON"
        Just sub -> do
            gs <- readState (envStore env)
            case authMode gs of
                Bootstrap ->
                    handleBootstrapPost env sub respond
                Normal ->
                    doAppend env sub respond

handleBootstrapPost
    :: (ToJSON a)
    => ServerEnv a
    -> Submission a
    -> (Response -> IO b)
    -> IO b
handleBootstrapPost env sub respond =
    case subPassphrase sub of
        Nothing ->
            respond $
                jsonResponse
                    status401
                    PassphraseRequired
        Just pass
            | pass /= envPassphrase env ->
                respond $
                    jsonResponse
                        status401
                        WrongPassphrase
            | otherwise ->
                doAppend env sub respond

{- | Validate and append the event. Constructs the
appropriate KERI event (inception or interaction),
verifies the signature against it, and appends.
-}
doAppend
    :: (ToJSON a)
    => ServerEnv a
    -> Submission a
    -> (Response -> IO b)
    -> IO b
doAppend env sub respond = do
    gs <- readState (envStore env)
    -- Business-rule validation
    case validateEvent
        (envConfig env)
        gs
        (subSigner sub)
        (subEvent sub) of
        Left ve ->
            respond $
                jsonResponse status422 $
                    ValidationErr ve
        Right () -> do
            tip <- chainTip (envStore env)
            -- Stale-tip check
            case checkStaleTip tip (subPriorDigest sub) of
                Left err ->
                    respond $ jsonResponse status409 err
                Right () -> do
                    -- Construct KERI event
                    let keriEvt =
                            mkKeriEvent
                                tip
                                (subSigner sub)
                                (subEvent sub)
                    -- Verify signature against KERI event
                    case verifySig
                        (subSigner sub)
                        (subSignature sub)
                        keriEvt of
                        Left err ->
                            respond
                                $ jsonResponse
                                    status401
                                $ SignatureError err
                        Right () -> do
                            appendEvent
                                (envStore env)
                                (envAppFold env)
                                (subSigner sub)
                                keriEvt
                                (subSignature sub)
                                (subEvent sub)
                            sn <-
                                kelLength (envStore env)
                            atomically $
                                writeTChan
                                    (envBroadcast env)
                                    sn
                            respond $
                                responseLBS
                                    status200
                                    jsonHeaders
                                    ( encode $
                                        AppendResult sn
                                    )

{- | Construct the KERI event for a submission.
First event (no tip) becomes an inception event;
subsequent events become interaction events with
the group event as anchor.
-}
mkKeriEvent
    :: (ToJSON a)
    => Maybe ChainTip
    -> Text
    -> GroupEvent a
    -> Event
mkKeriEvent Nothing signerKey groupEvt =
    mkInception
        InceptionConfig
            { icKeys = [signerKey]
            , icSigningThreshold = 1
            , icNextKeys = []
            , icNextThreshold = 0
            , icConfig = []
            , icAnchors = [toJSON groupEvt]
            }
mkKeriEvent (Just tip) _signer groupEvt =
    mkInteraction
        InteractionConfig
            { ixPrefix = tipPrefix tip
            , ixSequenceNumber = tipSeqNo tip + 1
            , ixPriorDigest = tipDigest tip
            , ixAnchors = [toJSON groupEvt]
            }

{- | Check stale-tip: if the client provides a
priorDigest, it must match the current tip's digest.
For the first event (no tip), priorDigest should be
absent.
-}
checkStaleTip
    :: Maybe ChainTip
    -> Maybe Text
    -> Either ServerError ()
checkStaleTip Nothing Nothing = Right ()
checkStaleTip Nothing (Just _) = Right ()
checkStaleTip (Just _) Nothing = Right ()
checkStaleTip (Just tip) (Just pd)
    | pd == tipDigest tip = Right ()
    | otherwise =
        Left $ StaleTip (tipDigest tip) pd

-- --------------------------------------------------------
-- GET /stream (SSE)
-- --------------------------------------------------------

handleStream
    :: ServerEnv a
    -> Application
handleStream env _req respond =
    respond $
        responseStream status200 sseHeaders $
            \write flush -> do
                ch <-
                    atomically $
                        dupTChan (envBroadcast env)
                let loop = do
                        sn <- atomically $ readTChan ch
                        write $
                            Builder.byteString
                                "event: new\ndata: {\"sn\":"
                                <> Builder.intDec sn
                                <> Builder.byteString
                                    "}\n\n"
                        flush
                        loop
                loop

-- --------------------------------------------------------
-- GET /info?key=K (open to anyone)
-- --------------------------------------------------------

handleInfo
    :: ServerEnv a
    -> Application
handleInfo env req respond =
    case parseKey req of
        Nothing ->
            respond $
                jsonResponse status400 $
                    BadRequest "missing ?key=K"
        Just key -> do
            gs <- readState (envStore env)
            tip <- chainTip (envStore env)
            let pubEmails = publicAdminEmails gs
                pending = hasPendingIntro key gs
                sKey =
                    serverCesrKey (envStore env)
                groupId = fmap tipPrefix tip
            respond $
                responseLBS
                    status200
                    jsonHeaders
                    ( encode $
                        object
                            [ "publicAdminEmails"
                                .= pubEmails
                            , "pendingIntroduction"
                                .= pending
                            , "serverKey"
                                .= sKey
                            , "groupId"
                                .= groupId
                            ]
                    )

-- | Emails of members with AdminRole PublicAdmin.
publicAdminEmails :: GroupState a -> [Text]
publicAdminEmails gs =
    [ memberEmail m
    | m <- Map.elems (members gs)
    , isPublicAdmin m
    ]
  where
    isPublicAdmin m =
        any
            ( \case
                AdminRole PublicAdmin -> True
                _ -> False
            )
            (memberRoles m)

-- | Check if any pending proposal introduces the key.
hasPendingIntro :: Text -> GroupState a -> Bool
hasPendingIntro key gs =
    any matchesKey $
        Map.elems (pendingProposals gs)
  where
    matchesKey pp = case proposal pp of
        IntroduceMember k _ _ -> k == key
        _ -> False

-- --------------------------------------------------------
-- Membership guard
-- --------------------------------------------------------

{- | Check that the request includes a valid member
key. In bootstrap mode, all guarded endpoints are
blocked (non-members should use /info instead).
-}
requireMemberGuard
    :: ServerEnv a
    -> Request
    -> (Response -> IO b)
    -> IO b
    -> IO b
requireMemberGuard env req respond onOk =
    case parseKey req of
        Nothing ->
            respond $
                jsonResponse status401 $
                    BadRequest "missing key"
        Just key -> do
            gs <- readState (envStore env)
            if isMember key gs
                then onOk
                else
                    respond $
                        jsonResponse status403 $
                            BadRequest "not a member"

-- | Parse the ?key=K query parameter.
parseKey :: Request -> Maybe Text
parseKey req =
    case lookup "key" (queryString req) of
        Just (Just bs) -> Just (TE.decodeUtf8 bs)
        _ -> Nothing

-- --------------------------------------------------------
-- Helpers
-- --------------------------------------------------------

jsonHeaders :: [(HeaderName, ByteString)]
jsonHeaders = [(hContentType, "application/json")]

sseHeaders :: [(HeaderName, ByteString)]
sseHeaders =
    [ (hContentType, "text/event-stream")
    , ("Cache-Control", "no-cache")
    ]

jsonResponse
    :: (ToJSON e)
    => Status
    -> e
    -> Response
jsonResponse status body =
    responseLBS status jsonHeaders (encode body)

{- | Verify the Ed25519 signature on a KERI event.
The signed message is the canonical serialization of
the KERI event (not the group event).
-}
verifySig
    :: Text
    -- ^ CESR-encoded signer public key
    -> Text
    -- ^ CESR-encoded Ed25519 signature
    -> Event
    -- ^ The KERI event
    -> Either Text ()
verifySig signerCesr sigCesr keriEvt = do
    pk <- decodePubKey signerCesr
    sig <- decodeSig sigCesr
    let msg = serializeEvent keriEvt
    if Ed25519.verify pk msg sig
        then Right ()
        else Left "signature verification failed"
  where
    decodePubKey t =
        case Cesr.decode t of
            Right Primitive{code = Ed25519PubKey, raw} ->
                case Ed25519.publicKeyFromBytes raw of
                    Right k -> Right k
                    Left e -> Left (T.pack e)
            Right _ ->
                Left "not an Ed25519 public key"
            Left e -> Left (T.pack e)
    decodeSig t =
        case Cesr.decode t of
            Right Primitive{code = Ed25519Sig, raw} ->
                Right raw
            Right _ ->
                Left "not an Ed25519 signature"
            Left e -> Left (T.pack e)
