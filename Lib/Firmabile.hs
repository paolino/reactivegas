{-# LANGUAGE TypeSynonymInstances #-}

{- |
Module      : Lib.Firmabile
Description : Cryptographic signing and verification utilities
Copyright   : (c) Paolo Veronelli, 2025
License     : BSD-3-Clause

Provides RSA-based cryptographic signing and verification,
along with AES encryption for secure key storage.
-}
module Lib.Firmabile
    ( -- * Type aliases
      Firma
    , Chiave
    , Segreto
    , Password
      -- * Hashing
    , hash
    , hashOver
    , bytestringInteger
      -- * Key management
    , cryptobox
      -- * Signing and verification
    , sign
    , verify
    ) where

import Codec.Crypto.RSA qualified as RSA
import Codec.Crypto.RSA.Pure qualified as RSAP
import Codec.Crypto.SimpleAES qualified as A (Mode (..), decryptMsg, encryptMsg')
import Crypto.Random (SystemRandom, newGenIO)
import Data.ByteString.Char8 qualified as B (ByteString, concat, pack, take)
import Data.ByteString.Lazy.Char8 qualified as BL
    ( ByteString
    , concat
    , foldr
    , pack
    , toChunks
    , unpack
    )
import Data.Char (ord)
import Data.Monoid (mappend)
import System.IO.Unsafe (unsafePerformIO)

-- | Digital signature (RSA signature as lazy ByteString)
type Firma = BL.ByteString

-- | Public key for verification (Italian: "chiave" = key)
type Chiave = RSA.PublicKey

-- | Encrypted private key (Italian: "segreto" = secret)
type Segreto = BL.ByteString

-- | Password for key encryption
type Password = String

-- | Compute SHA256 hash of a string
hash :: String -> BL.ByteString
hash = RSA.hashFunction RSA.hashSHA256 . BL.pack

-- | Compute SHA256 hash of a string combined with existing hash
hashOver :: String -> BL.ByteString -> BL.ByteString
hashOver str existing =
    RSA.hashFunction RSA.hashSHA256 $ BL.pack str `mappend` existing

-- | Convert a ByteString to an integer (base-256 encoding)
bytestringInteger :: BL.ByteString -> Int
bytestringInteger = BL.foldr (\c x -> ord c + 256 * x) 0

-- | Create a password-derived key of specified length
key :: Int -> String -> B.ByteString
key n = B.pack . take n . concat . repeat

-- | Generate a cryptographic key pair protected by password
-- Returns (public key, encrypted private key)
cryptobox :: Password -> (Chiave, Segreto)
cryptobox p = unsafePerformIO $ do
    let k = RSA.hashFunction RSA.hashSHA256 . BL.pack $ p
    gen <- newGenIO :: IO SystemRandom
    case RSAP.generateKeyPair gen 1024 of
        Left err -> error $ "RSA key generation failed: " ++ show err
        Right (publicKey, privateKey, _) ->
            return
                ( publicKey
                , A.encryptMsg'
                    A.CBC
                    (key 32 p)
                    (B.take 16 . B.concat . BL.toChunks $ k)
                    (BL.pack $ show privateKey)
                )

-- | Sign data using encrypted private key and password
-- Returns 'Nothing' if decryption fails (wrong password)
sign :: (Show a) => (Segreto, Password) -> a -> Maybe Firma
sign (encryptedKey, password) value =
    case reads (BL.unpack . A.decryptMsg A.CBC (key 32 password) $ encryptedKey) of
        [] -> Nothing
        [(privateKey, _)] -> Just . RSA.sign privateKey . BL.pack . show $ value

-- | Verify a signature using public key
verify :: (Show a) => Chiave -> a -> Firma -> Bool
verify publicKey value = RSA.verify publicKey (BL.pack . show $ value)
