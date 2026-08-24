{-# LANGUAGE ImportQualifiedPost #-}

{- |
Module      : Reactivegas.Core.Ed25519
Description : Ed25519 signing and verification (RFC 8032)
Copyright   : (c) 2026 Paolo Veronelli
License     : BSD3
Maintainer  : Paolo Veronelli <paolo.veronelli@gmail.com>
Stability   : experimental

Thin, total wrappers around @crypton@'s Ed25519 so the event core
never touches partial functions or raw crypto errors. Keys and
signatures cross the codec boundary as strict bytestrings.
-}
module Reactivegas.Core.Ed25519 (
    SecretKey,
    PublicKey,
    Signature,
    newSecretKey,
    derivePublicKey,
    signMessage,
    verifySignature,
    publicKeyBytes,
    parsePublicKey,
    signatureBytes,
    parseSignature,
) where

import Crypto.Error (CryptoFailable, eitherCryptoError)
import Crypto.PubKey.Ed25519 (
    PublicKey,
    SecretKey,
    Signature,
 )
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.ByteArray (convert)
import Data.ByteString (ByteString)

cryptoEither :: CryptoFailable a -> Either String a
cryptoEither = either (Left . show) Right . eitherCryptoError

-- | Parse a 32-byte seed into a secret key.
newSecretKey :: ByteString -> Either String SecretKey
newSecretKey = cryptoEither . Ed25519.secretKey

-- | Public key derived from a secret key.
derivePublicKey :: SecretKey -> PublicKey
derivePublicKey = Ed25519.toPublic

-- | Deterministic Ed25519 signature over the message bytes.
signMessage :: SecretKey -> PublicKey -> ByteString -> Signature
signMessage = Ed25519.sign

verifySignature :: PublicKey -> ByteString -> Signature -> Bool
verifySignature pk msg sig = Ed25519.verify pk msg sig

publicKeyBytes :: PublicKey -> ByteString
publicKeyBytes = convert

parsePublicKey :: ByteString -> Either String PublicKey
parsePublicKey = cryptoEither . Ed25519.publicKey

signatureBytes :: Signature -> ByteString
signatureBytes = convert

parseSignature :: ByteString -> Either String Signature
parseSignature = cryptoEither . Ed25519.signature
