module View.App
  ( appComponent
  ) where

import Prelude

import Data.Argonaut.Core (Json, jsonNull, stringify)
import Data.Argonaut.Decode
  ( decodeJson
  , printJsonDecodeError
  , (.:)
  )
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import FFI.Fetch as Fetch
import FFI.SSE as SSE
import FFI.Storage as Storage
import FFI.TextEncoder (encodeUtf8)
import FFI.TweetNaCl as NaCl
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Keri.Cesr.DerivationCode (DerivationCode(..))
import Keri.Cesr.Encode as Cesr
import Keri.Cesr.Primitive (mkPrimitive)
import KelGroups.Client.Codec
  ( decodeGroupEvent
  , decodeInfoResponse
  , encodeGroupEvent
  , encodeSubmission
  )
import KelGroups.Client.Event (BaseEvent(..), GroupEvent(..), Proposal(..))
import KelGroups.Client.Fold (applyEvent)
import KelGroups.Client.State (GroupState, emptyState, adminCount)
import KelGroups.Client.Types (Admin(..), Role(..))
import Type.Proxy (Proxy(..))
import View.Bootstrap as Bootstrap
import View.Members as Members
import View.Proposals as Proposals

type Identity =
  { keyPair :: NaCl.KeyPair
  , prefix :: String
  }

type InfoResponse =
  { publicAdminEmails :: Array String
  , pendingIntroduction :: Boolean
  }

data Screen
  = IdentityScreen
  | BootstrapScreen
  | NonMemberScreen InfoResponse
  | NormalScreen

type State =
  { screen :: Screen
  , identity :: Maybe Identity
  , groupState :: GroupState Unit
  , serverSeqNo :: Int
  , sse :: Maybe SSE.EventSource
  , error :: Maybe String
  }

data Action
  = Init
  | GenerateIdentity
  | HandleBootstrap Bootstrap.Output
  | HandleMembers Members.Output
  | HandleProposals Proposals.Output
  | SSEMessage String
  | CopyKey
  | ResetIdentity
  | Dismiss

type Slots =
  ( bootstrap :: H.Slot (Const Void) Bootstrap.Output Unit
  , members :: H.Slot (Const Void) Members.Output Unit
  , proposals :: H.Slot (Const Void) Proposals.Output Unit
  )

appComponent :: forall q i o m. MonadAff m => H.Component q i o m
appComponent = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval H.defaultEval
      { initialize = Just Init
      , handleAction = handleAction
      }
  }

initialState :: State
initialState =
  { screen: IdentityScreen
  , identity: Nothing
  , groupState: emptyState unit
  , serverSeqNo: 0
  , sse: Nothing
  , error: Nothing
  }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render st = HH.div [ HP.class_ (HH.ClassName "app") ]
  [ header st
  , case st.error of
      Just err ->
        HH.div
          [ HP.class_ (HH.ClassName "error-bar")
          , HE.onClick (const Dismiss)
          ]
          [ HH.text err ]
      Nothing -> HH.text ""
  , content st
  ]

header :: forall m. State -> H.ComponentHTML Action Slots m
header st = HH.nav [ HP.class_ (HH.ClassName "header") ]
  [ HH.h1_ [ HH.text "kelgroups" ]
  , case st.identity of
      Just ident -> HH.span
        [ HP.class_ (HH.ClassName "user-id") ]
        [ HH.text (take8 ident.prefix)
        , HH.button
            [ HE.onClick (const CopyKey)
            , HP.class_ (HH.ClassName "btn-copy")
            ]
            [ HH.text "Copy" ]
        , HH.button
            [ HE.onClick (const ResetIdentity)
            , HP.class_ (HH.ClassName "btn-reset")
            ]
            [ HH.text "Reset" ]
        ]
      Nothing -> HH.text ""
  ]
  where
  take8 s =
    if String.length s > 8 then String.take 8 s <> "..."
    else s

content :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
content st = case st.screen of
  IdentityScreen ->
    HH.div [ HP.class_ (HH.ClassName "identity-screen") ]
      [ HH.h2_ [ HH.text "Welcome to kelgroups" ]
      , HH.p_ [ HH.text "Generate a KERI identity to get started." ]
      , HH.button
          [ HE.onClick (const GenerateIdentity)
          , HP.class_ (HH.ClassName "btn-primary")
          ]
          [ HH.text "Generate Identity" ]
      ]

  BootstrapScreen ->
    HH.slot (Proxy :: _ "bootstrap") unit
      Bootstrap.bootstrapComponent
      (map _.prefix st.identity)
      HandleBootstrap

  NonMemberScreen info ->
    HH.div [ HP.class_ (HH.ClassName "non-member-screen") ]
      [ HH.h2_ [ HH.text "Not a member yet" ]
      , if info.pendingIntroduction then
          HH.p [ HP.class_ (HH.ClassName "pending-notice") ]
            [ HH.text
                "Your introduction is pending approval. \
                \Please wait for admin majority."
            ]
        else HH.text ""
      , if info.publicAdminEmails /= [] then
          HH.div_
            [ HH.p_ [ HH.text "Contact an admin to request introduction:" ]
            , HH.ul_ $ map
                ( \email ->
                    HH.li_ [ HH.text email ]
                )
                info.publicAdminEmails
            ]
        else if not info.pendingIntroduction then
          HH.p_
            [ HH.text
                "No public admins found. Ask someone with \
                \access to introduce you."
            ]
        else HH.text ""
      ]

  NormalScreen ->
    HH.div_
      [ HH.slot (Proxy :: _ "members") unit
          Members.membersComponent
          { groupState: st.groupState
          , myKey: map _.prefix st.identity
          }
          HandleMembers
      , HH.slot (Proxy :: _ "proposals") unit
          Proposals.proposalsComponent
          { groupState: st.groupState
          , myKey: map _.prefix st.identity
          }
          HandleProposals
      ]

baseUrl :: String
baseUrl = ""

handleAction
  :: forall o m
   . MonadAff m
  => Action
  -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  Init -> do
    mPrefix <- liftEffect $ Storage.getItem "kelgroups-prefix"
    case mPrefix of
      Just prefix -> do
        kp <- liftEffect NaCl.generateKeyPair
        let ident = { keyPair: kp, prefix }
        H.modify_ _ { identity = Just ident }
        checkMembershipAndLoad prefix
      Nothing -> pure unit

  GenerateIdentity -> do
    kp <- liftEffect NaCl.generateKeyPair
    case mkPrimitive Ed25519PubKey kp.publicKey of
      Left err ->
        H.modify_ _ { error = Just err }
      Right prim -> do
        let
          prefix = Cesr.encode prim
          ident = { keyPair: kp, prefix }
        liftEffect $ Storage.setItem "kelgroups-prefix" prefix
        H.modify_ _ { identity = Just ident, error = Nothing }
        checkMembershipAndLoad prefix

  HandleBootstrap output -> case output of
    Bootstrap.Submit passphrase key email -> do
      st <- H.get
      case st.identity of
        Just ident ->
          submitEvent
            (Just passphrase)
            ident
            ( Base
                ( Propose
                    ( IntroduceMember
                        key
                        email
                        ( Set.singleton
                            (AdminRole PublicAdmin)
                        )
                    )
                )
            )
        Nothing ->
          H.modify_ _
            { error = Just "No identity" }

  HandleMembers output -> case output of
    Members.SubmitPropose proposal' -> do
      st <- H.get
      case st.identity of
        Just ident ->
          submitEvent Nothing ident
            (Base (Propose proposal'))
        Nothing ->
          H.modify_ _ { error = Just "No identity" }

  HandleProposals output -> case output of
    Proposals.SubmitApprove proposalId -> do
      st <- H.get
      case st.identity of
        Just ident ->
          submitEvent Nothing ident
            (Base (Approve proposalId))
        Nothing ->
          H.modify_ _ { error = Just "No identity" }

  SSEMessage msgStr -> do
    case parseSseMessage msgStr of
      Left _ -> pure unit
      Right _ -> fetchNewEvents

  CopyKey -> do
    st <- H.get
    case st.identity of
      Just ident -> liftEffect $ Storage.copyToClipboard ident.prefix
      Nothing -> pure unit

  ResetIdentity -> do
    ok <- liftEffect $ Storage.confirm "Reset your identity? This cannot be undone."
    when ok do
      liftEffect $ Storage.removeItem "kelgroups-prefix"
      H.modify_ _ { identity = Nothing, screen = IdentityScreen }

  Dismiss ->
    H.modify_ _ { error = Nothing }

-- | Check membership via /info + /events, decide which screen.
checkMembershipAndLoad
  :: forall o m
   . MonadAff m
  => String
  -> H.HalogenM State Action Slots o m Unit
checkMembershipAndLoad key = do
  -- First try fetching events (will 403 if not a member)
  res <- liftAff $ Fetch.fetch
    (baseUrl <> "/events?after=-1&key=" <> key)
    { method: "GET", body: "" }
  case res.status of
    200 -> do
      -- We're a member, replay all events
      fetchAndReplay key
      startSSE key
    403 -> do
      -- Not a member — fetch /info to decide screen
      infoRes <- liftAff $ Fetch.fetch
        (baseUrl <> "/info?key=" <> key)
        { method: "GET", body: "" }
      case parseInfoResponse infoRes.body of
        Left _ ->
          -- Can't parse info, assume bootstrap
          H.modify_ _ { screen = BootstrapScreen }
        Right info ->
          if
            info.publicAdminEmails == []
              && not info.pendingIntroduction then
            H.modify_ _ { screen = BootstrapScreen }
          else
            H.modify_ _ { screen = NonMemberScreen info }
    404 -> do
      -- No events at all — bootstrap mode
      H.modify_ _ { screen = BootstrapScreen }
    _ ->
      H.modify_ _
        { error = Just ("Fetch failed: " <> show res.status) }

-- | Sign a group event and compute CESR signature.
signEvent
  :: Identity
  -> GroupEvent Unit
  -> Either String String
signEvent ident evt = do
  let
    evtJson = encodeGroupEvent (const jsonNull) evt
    msgBytes = encodeUtf8 (stringify evtJson)
    sigBytes = NaCl.sign msgBytes ident.keyPair.secretKey
  sigPrim <- mkPrimitive Ed25519Sig sigBytes
  pure (Cesr.encode sigPrim)

-- | Submit a group event to the server.
submitEvent
  :: forall o m
   . MonadAff m
  => Maybe String
  -> Identity
  -> GroupEvent Unit
  -> H.HalogenM State Action Slots o m Unit
submitEvent passphrase ident evt =
  case signEvent ident evt of
    Left err ->
      H.modify_ _
        { error = Just ("Signing failed: " <> err) }
    Right signature -> do
      let
        body = encodeSubmission
          (const jsonNull)
          { passphrase
          , signer: ident.prefix
          , signature
          , event: evt
          }
      res <- liftAff $ Fetch.fetch
        (baseUrl <> "/events")
        { method: "POST", body: stringify body }
      if res.status /= 200 then
        H.modify_ _
          { error =
              Just ("Submit failed: " <> res.body)
          }
      else do
        st <- H.get
        fetchNewEvents
        -- After bootstrap, re-check membership
        case st.screen of
          BootstrapScreen ->
            checkMembershipAndLoad ident.prefix
          _ -> pure unit

-- | Fetch all events from the beginning and rebuild state.
fetchAndReplay
  :: forall o m
   . MonadAff m
  => String
  -> H.HalogenM State Action Slots o m Unit
fetchAndReplay key = do
  let
    go seqNo gs = do
      res <- liftAff $ Fetch.fetch
        ( baseUrl <> "/events?after=" <> show seqNo
            <> "&key="
            <> key
        )
        { method: "GET", body: "" }
      case res.status of
        404 -> do
          let
            screen =
              if adminCount gs == 0 then BootstrapScreen
              else NormalScreen
          H.modify_ _
            { groupState = gs
            , serverSeqNo = seqNo + 1
            , screen = screen
            }
        200 ->
          case parseEventResponse res.body of
            Left err ->
              H.modify_ _
                { error = Just ("Decode error: " <> err) }
            Right { signer, event } ->
              case decodeGroupEvent (const (Right unit)) event of
                Left err ->
                  H.modify_ _
                    { error = Just
                        ("Event decode: " <> printJsonDecodeError err)
                    }
                Right evt -> do
                  let gs' = applyEvent trivialFold gs (Tuple signer evt)
                  go (seqNo + 1) gs'
        _ ->
          H.modify_ _ { error = Just ("Fetch failed: " <> show res.status) }
  go (-1) (emptyState unit)

-- | Fetch new events since our last known position.
fetchNewEvents
  :: forall o m
   . MonadAff m
  => H.HalogenM State Action Slots o m Unit
fetchNewEvents = do
  st <- H.get
  case st.identity of
    Nothing -> pure unit
    Just ident -> do
      let
        key = ident.prefix
        go seqNo gs = do
          res <- liftAff $ Fetch.fetch
            ( baseUrl <> "/events?after=" <> show seqNo
                <> "&key="
                <> key
            )
            { method: "GET", body: "" }
          case res.status of
            404 -> do
              let
                screen =
                  if adminCount gs == 0 then BootstrapScreen
                  else NormalScreen
              H.modify_ _
                { groupState = gs
                , serverSeqNo = seqNo + 1
                , screen = screen
                }
            200 ->
              case parseEventResponse res.body of
                Left _ -> H.modify_ _ { serverSeqNo = seqNo + 1 }
                Right { signer, event } ->
                  case decodeGroupEvent (const (Right unit)) event of
                    Left _ -> H.modify_ _ { serverSeqNo = seqNo + 1 }
                    Right evt -> do
                      let gs' = applyEvent trivialFold gs (Tuple signer evt)
                      go (seqNo + 1) gs'
            _ ->
              H.modify_ _ { error = Just ("Fetch failed: " <> show res.status) }
      go (st.serverSeqNo - 1) st.groupState

-- | Start SSE subscription.
startSSE
  :: forall o m
   . MonadAff m
  => String
  -> H.HalogenM State Action Slots o m Unit
startSSE key = do
  { emitter, listener } <- liftEffect HS.create
  void $ H.subscribe emitter
  es <- liftEffect do
    es <- SSE.create (baseUrl <> "/stream?key=" <> key)
    SSE.onMessage es \msg ->
      HS.notify listener (SSEMessage msg)
    pure es
  H.modify_ _ { sse = Just es }

trivialFold :: Unit -> Unit -> Unit
trivialFold _ _ = unit

-- | Parse SSE data: @{"sn":N}@
parseSseMessage :: String -> Either String Int
parseSseMessage s = do
  json <- lmap show (jsonParser s)
  lmap printJsonDecodeError do
    obj <- decodeJson json
    obj .: "sn"

-- | Parse a GET /events response body.
parseEventResponse
  :: String -> Either String { signer :: String, event :: Json }
parseEventResponse s = do
  json <- lmap show (jsonParser s)
  lmap printJsonDecodeError do
    obj <- decodeJson json
    signer <- obj .: "signer"
    event <- obj .: "event"
    pure { signer, event }

-- | Parse a GET /info response body.
parseInfoResponse
  :: String -> Either String InfoResponse
parseInfoResponse s = do
  json <- lmap show (jsonParser s)
  lmap printJsonDecodeError (decodeInfoResponse json)
