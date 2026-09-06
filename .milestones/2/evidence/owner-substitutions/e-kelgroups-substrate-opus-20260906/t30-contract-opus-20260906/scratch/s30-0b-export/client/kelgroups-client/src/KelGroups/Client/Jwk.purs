module KelGroups.Client.Jwk
  ( Jwk
  , keyPairToJwk
  , jwkToKeyPair
  , parseJwkJson
  , encodeJwkJson
  , b64urlEncodeInts
  , b64urlDecodeInts
  ) where

import Prelude

import Data.Argonaut.Core (Json, fromObject, fromString, stringify)
import Data.Argonaut.Decode
  ( JsonDecodeError
  , decodeJson
  , printJsonDecodeError
  , (.:)
  )
import Data.Argonaut.Parser (jsonParser)
import Data.Array (length, take, (!!))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Int.Bits (and, or, shl, shr)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..))
import FFI.KeyBytes (fromArray, fromSeed, toArray)
import FFI.TweetNaCl (KeyPair)
import Foreign.Object as Object

-- | An Ed25519 private-key JWK (RFC 7517 + RFC 8037).
type Jwk =
  { kty :: String
  , crv :: String
  , x :: String
  , d :: String
  }

-- | Export a signing key pair. The JWK `d` member is
-- the 32-byte seed (first half of the TweetNaCl secret
-- key).
keyPairToJwk :: KeyPair -> Jwk
keyPairToJwk kp =
  { kty: "OKP"
  , crv: "Ed25519"
  , x: b64urlEncodeInts (toArray kp.publicKey)
  , d: b64urlEncodeInts (take 32 (toArray kp.secretKey))
  }

-- | Import a private-key JWK with full validation:
-- key type and curve, strict unpadded base64url,
-- exactly-32-byte members and public/private
-- consistency. Errors never contain key material.
jwkToKeyPair :: Jwk -> Either String KeyPair
jwkToKeyPair j = do
  checkType j
  dBytes <- member32 "d" j.d
  xBytes <- member32 "x" j.x
  let kp = fromSeed (fromArray dBytes)
  if toArray kp.publicKey /= xBytes then
    Left "public key does not match private key"
  else
    Right kp

-- | Parse a JWK JSON document and import the key.
parseJwkJson :: String -> Either String KeyPair
parseJwkJson s = do
  json <- lmap show (jsonParser s)
  jwk <- lmap printJsonDecodeError (decodeJwk json)
  jwkToKeyPair jwk
  where
  decodeJwk :: Json -> Either JsonDecodeError Jwk
  decodeJwk json = do
    obj <- decodeJson json
    kty <- obj .: "kty"
    crv <- obj .: "crv"
    x <- obj .: "x"
    d <- obj .: "d"
    pure { kty, crv, x, d }

-- | Serialize a JWK to a JSON string.
encodeJwkJson :: Jwk -> String
encodeJwkJson j =
  stringify $ fromObject $ Object.fromFoldable
    [ Tuple "kty" (fromString j.kty)
    , Tuple "crv" (fromString j.crv)
    , Tuple "x" (fromString j.x)
    , Tuple "d" (fromString j.d)
    ]

checkType :: Jwk -> Either String Unit
checkType j
  | j.kty /= "OKP" = Left "unsupported kty: expected OKP"
  | j.crv /= "Ed25519" = Left "unsupported crv: expected Ed25519"
  | otherwise = Right unit

member32 :: String -> String -> Either String (Array Int)
member32 name raw = do
  bytes <- lmap (const ("invalid base64url in " <> name))
    (b64urlDecodeInts raw)
  if length bytes /= 32 then
    Left (name <> " must encode exactly 32 bytes")
  else
    Right bytes

-- --------------------------------------------------------
-- Unpadded base64url (RFC 7515 Section 2)
-- --------------------------------------------------------

alphabetArr :: Array Char
alphabetArr =
  SCU.toCharArray
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

b64urlEncodeInts :: Array Int -> String
b64urlEncodeInts bytes = go 0
  where
  n = length bytes

  at i = fromMaybe 0 (bytes !! i)

  go i
    | i + 3 <= n = chunk3 (at i) (at (i + 1)) (at (i + 2)) <> go (i + 3)
    | i + 2 == n = chunk2 (at i) (at (i + 1))
    | i + 1 == n = chunk1 (at i)
    | otherwise = ""

  chunk3 a b c =
    let
      v = a * 65536 + b * 256 + c
    in
      s4 v

  chunk2 a b =
    let
      v = a * 256 + b
    in
      ch (v `shr` 10) <> ch (v `shr` 4) <> ch (v `shl` 2)

  chunk1 a = ch (a `shr` 2) <> ch (a `shl` 4)

  s4 v = ch (v `shr` 18) <> ch (v `shr` 12) <> ch (v `shr` 6) <> ch v

  ch k = SCU.singleton (fromMaybe '?' (alphabetArr !! (k `and` 63)))

b64urlDecodeInts :: String -> Either String (Array Int)
b64urlDecodeInts s
  | s == "" = Left "empty base64url value"
  | otherwise = go 0
      where
      n = SCU.length s

      go i
        | i + 4 <= n = do
            va <- valAt i
            vb <- valAt (i + 1)
            vc <- valAt (i + 2)
            vd <- valAt (i + 3)
            rest <- go (i + 4)
            pure ([ b0 va vb, b1 vb vc, b2 vc vd ] <> rest)
        | i + 2 == n = do
            va <- valAt i
            vb <- valAt (i + 1)
            zeroBits vb 4
            pure [ b0 va vb ]
        | i + 3 == n = do
            va <- valAt i
            vb <- valAt (i + 1)
            vc <- valAt (i + 2)
            zeroBits vc 2
            pure [ b0 va vb, b1 vb vc ]
        | otherwise = Left "invalid base64url length"

      valAt i = do
        c <- case SCU.charAt i s of
          Just ch -> Right ch
          Nothing -> Left "invalid base64url length"
        case Array.findIndex (\ch -> ch == c) alphabetArr of
          Just v -> Right v
          Nothing -> Left "invalid base64url encoding"

      zeroBits v k =
        if (v `and` ((1 `shl` k) - 1)) /= 0 then
          Left "non-canonical base64url padding bits"
        else
          Right unit

      b0 va vb = (va `shl` 2) `or` (vb `shr` 4)

      b1 vb vc = ((vb `and` 15) `shl` 4) `or` (vc `shr` 2)

      b2 vc vd = ((vc `and` 3) `shl` 6) `or` vd
