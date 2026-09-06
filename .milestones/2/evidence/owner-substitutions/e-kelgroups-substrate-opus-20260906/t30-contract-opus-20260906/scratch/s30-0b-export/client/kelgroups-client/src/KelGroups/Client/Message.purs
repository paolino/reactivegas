-- | Bridge between group events and KERI interaction events.
module KelGroups.Client.Message
  ( KelGroupMessage
  , mkGroupMessage
  , verifyGroupMessage
  , extractGroupEvent
  , serializeEnvelope
  , deserializeEnvelope
  ) where

import Prelude

import Data.Argonaut.Core
  ( Json
  , fromObject
  , fromString
  , stringify
  )
import Data.Argonaut.Decode
  ( JsonDecodeError
  , decodeJson
  , printJsonDecodeError
  , (.:)
  )
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import FFI.TextEncoder (encodeUtf8)
import FFI.TweetNaCl as NaCl
import Keri.Cesr.DerivationCode (DerivationCode(..))
import Keri.Cesr.Encode as CesrEncode
import Keri.Cesr.Primitive (mkPrimitive)
import Keri.Event (eventPrefix)
import Keri.Event.Interaction (InteractionConfig, mkInteraction)
import Keri.Event.Serialize (serializeEvent)
import Keri.KeyState (KeyState)
import Keri.KeyState.Verify (verifySignatures)
import Keri.Kel (SignedEvent) as Kel

-- | A KERI-wrapped group message.
type KelGroupMessage a =
  { keriEvent :: Kel.SignedEvent
  , groupEvent :: a
  }

-- | Create a signed KERI interaction event wrapping a group event.
mkGroupMessage
  :: forall a
   . (a -> Json)
  -> { prefix :: String
     , sequenceNumber :: Int
     , priorDigest :: String
     , keyPair :: NaCl.KeyPair
     , keyIndex :: Int
     }
  -> a
  -> Either String (KelGroupMessage a)
mkGroupMessage encodeEvt cfg groupEvent = do
  let
    canonical = stringify (encodeEvt groupEvent)
    anchor = fromString canonical

    ixnConfig :: InteractionConfig
    ixnConfig =
      { prefix: cfg.prefix
      , sequenceNumber: cfg.sequenceNumber
      , priorDigest: cfg.priorDigest
      , anchors: [ anchor ]
      }
    ixn = mkInteraction ixnConfig
    serialized = serializeEvent ixn
    msgBytes = encodeUtf8 serialized
    sigBytes = NaCl.sign msgBytes cfg.keyPair.secretKey
  sigPrim <- mkPrimitive Ed25519Sig sigBytes
  let
    sigCesr = CesrEncode.encode sigPrim
    keriEvent =
      { event: ixn
      , signatures:
          [ { index: cfg.keyIndex, signature: sigCesr } ]
      }
  pure { keriEvent, groupEvent }

-- | Verify a group message's KERI signature against key state.
verifyGroupMessage
  :: forall a. KeyState -> KelGroupMessage a -> Boolean
verifyGroupMessage ks msg =
  let
    serialized = serializeEvent msg.keriEvent.event
  in
    verifySignatures
      ks.keys
      ks.signingThreshold
      serialized
      msg.keriEvent.signatures

-- | Extract signer AID and group event from a verified message.
extractGroupEvent
  :: forall a
   . (Json -> Either JsonDecodeError a)
  -> KelGroupMessage a
  -> Either String (Tuple String a)
extractGroupEvent _ msg =
  let
    prefix = eventPrefix msg.keriEvent.event
  in
    Right (Tuple prefix msg.groupEvent)

-- | Serialize a KERI signed event to JSON string (for POST).
serializeEnvelope :: forall a. KelGroupMessage a -> String
serializeEnvelope msg =
  let
    serialized = serializeEvent msg.keriEvent.event
  in
    stringify $ fromObject $ Object.fromFoldable
      [ Tuple "event" (fromString serialized) ]

-- | Deserialize a KERI envelope payload from server.
deserializeEnvelope
  :: forall a
   . (Json -> Either JsonDecodeError a)
  -> String
  -> Either String { signer :: String, groupEvent :: a }
deserializeEnvelope decodeEvt s = do
  json <- lmap show (jsonParser s)
  lmap printJsonDecodeError do
    obj <- decodeJson json
    signer <- obj .: "signer"
    evtJson <- obj .: "event"
    evt <- decodeEvt evtJson
    pure { signer, groupEvent: evt }
