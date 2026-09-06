{- |
Module      : KelGroups.Jwk
Description : Ed25519 private key export/import as JWK
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Standard JSON Web Key (RFC 7517) representation of
Ed25519 keys using the Octet Key Pair type and curve
defined in RFC 8037. A private-key JWK carries the
base64url-encoded @x@ (public) and @d@ (private)
members:

> { "kty": "OKP"
> , "crv": "Ed25519"
> , "x": "<base64url public key>"
> , "d": "<base64url private key>"
> }

Import validates the key type and curve, decodes both
members strictly (unpadded canonical base64url),
requires exactly 32-byte values, and rejects a public
key that does not match the private key. Error
messages never contain key material.
-}
module KelGroups.Jwk
    ( Jwk (..)
    , keyPairToJwk
    , jwkToKeyPair
    , encodeJwkJson
    , decodeJwkJson
    , encodeB64url
    , decodeB64url
    ) where

import Crypto.PubKey.Ed25519 qualified as Ed
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , eitherDecode
    , encode
    , object
    , withObject
    , (.:)
    , (.=)
    )
import Data.Aeson.Types (Parser)
import Data.Bits
    ( shiftL
    , shiftR
    , (.&.)
    )
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8)
import Keri.Crypto.Ed25519
    ( KeyPair (..)
    , publicKeyBytes
    , publicKeyFromBytes
    , secretKeyBytes
    , secretKeyFromBytes
    )

-- | An Ed25519 private-key JWK.
data Jwk = Jwk
    { jwkX :: Text
    -- ^ Base64url-encoded public key
    , jwkD :: Text
    -- ^ Base64url-encoded private key
    }
    deriving stock (Eq)

-- | Redacted 'Show': never renders key material.
instance Show Jwk where
    show _ = "Jwk<redacted>"

-- | Export a key pair as a private-key JWK.
keyPairToJwk :: KeyPair -> Jwk
keyPairToJwk kp =
    Jwk
        { jwkX = encodeB64url (publicKeyBytes (publicKey kp))
        , jwkD = encodeB64url (secretKeyBytes (secretKey kp))
        }

{- | Import a private-key JWK, validating structure
and key consistency.
-}
jwkToKeyPair :: Jwk -> Either String KeyPair
jwkToKeyPair Jwk{jwkD, jwkX} = do
    dBytes <- decodeMember "d" jwkD
    sk <- parseKey "d" secretKeyFromBytes dBytes
    xBytes <- decodeMember "x" jwkX
    pk <- parseKey "x" publicKeyFromBytes xBytes
    if publicKeyBytes (Ed.toPublic sk) /= xBytes
        then Left "public key does not match private key"
        else
            Right
                KeyPair
                    { secretKey = sk
                    , publicKey = pk
                    }
  where
    parseKey name fromBytes bytes =
        either
            (const (Left (name <> " is not a usable Ed25519 key")))
            Right
            (fromBytes bytes)

-- | Serialize a JWK to JSON.
encodeJwkJson :: Jwk -> LBS.ByteString
encodeJwkJson = encode

{- | Parse JSON and import the enclosed key pair,
applying full validation.
-}
decodeJwkJson :: LBS.ByteString -> Either String KeyPair
decodeJwkJson json =
    eitherDecode json >>= jwkToKeyPair

instance ToJSON Jwk where
    toJSON j =
        object
            [ "kty" .= ("OKP" :: Text)
            , "crv" .= ("Ed25519" :: Text)
            , "x" .= jwkX j
            , "d" .= jwkD j
            ]

instance FromJSON Jwk where
    parseJSON = withObject "Ed25519 OKP JWK" $ \o -> do
        kty <- o .: "kty" :: Parser Text
        crv <- o .: "crv" :: Parser Text
        x <- o .: "x"
        d <- o .: "d"
        let jwk = Jwk{jwkX = x, jwkD = d}
        case (kty, crv) of
            ("OKP", "Ed25519") -> pure jwk
            ("OKP", _) ->
                fail "unsupported crv: expected Ed25519"
            (_, "Ed25519") ->
                fail "unsupported kty: expected OKP"
            _ ->
                fail
                    "unsupported key type: expected OKP/Ed25519"

-- --------------------------------------------------------
-- Unpadded base64url (RFC 7515 Section 2)
-- --------------------------------------------------------

b64urlTable :: [(Char, Int)]
b64urlTable =
    zip
        (['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9'] <> "-_")
        [0 ..]

{- | Encode bytes as canonical unpadded base64url
text.
-}
encodeB64url :: ByteString -> Text
encodeB64url = T.pack . go . BS.unpack
  where
    go (a : b : c : rest) =
        let n = w a * 65536 + w b * 256 + w c
        in  ix (n `shiftR` 18)
                : ix (n `shiftR` 12)
                : ix (n `shiftR` 6)
                : ix n
                : go rest
    go [a, b] =
        let n = w a * 256 + w b
        in  [ix (n `shiftR` 10), ix (n `shiftR` 4), ix (n `shiftL` 2)]
    go [a] = [ix (w a `shiftR` 2), ix (w a `shiftL` 4)]
    go [] = []
    ix sh = fst (b64urlTable !! (sh .&. 63))
    w = fromIntegral :: Word8 -> Int

{- | Strictly decode unpadded base64url text. Rejects
foreign characters and non-canonical trailing bits.
Errors never echo the offending input.
-}
decodeB64url :: Text -> Either String ByteString
decodeB64url t
    | T.null t = Left "empty base64url value"
    | otherwise = BS.pack <$> go (T.unpack t)
  where
    go (a : b : c : d : rest) = do
        va <- valOf a
        vb <- valOf b
        vc <- valOf c
        vd <- valOf d
        rest' <- go rest
        pure (b0 va vb : b1 vb vc : b2 vc vd : rest')
    go [a, b] = do
        va <- valOf a
        vb <- valOf b
        requireZeroBits vb 4
        pure [b0 va vb]
    go [a, b, c] = do
        va <- valOf a
        vb <- valOf b
        vc <- valOf c
        requireZeroBits vc 2
        pure [b0 va vb, b1 vb vc]
    go [_] = Left "invalid base64url length"
    go [] = Right []
    valOf c = case lookup c b64urlTable of
        Just v -> Right v
        Nothing -> Left "invalid base64url encoding"
    requireZeroBits v n =
        if v .&. ((1 `shiftL` n) - 1) /= 0
            then Left "non-canonical base64url padding bits"
            else Right ()
    b0 va vb = fromIntegral (va * 4 + vb `div` 16)
    b1 vb vc = fromIntegral ((vb .&. 15) * 16 + vc `div` 4)
    b2 vc vd = fromIntegral ((vc .&. 3) * 64 + vd)

{- | Decode one JWK member: strict base64url of
exactly 32 bytes.
-}
decodeMember :: String -> Text -> Either String ByteString
decodeMember name raw = do
    bs <-
        either
            (const (Left ("invalid base64url in " <> name)))
            Right
            (decodeB64url raw)
    if BS.length bs /= 32
        then Left (name <> " must encode exactly 32 bytes")
        else Right bs
