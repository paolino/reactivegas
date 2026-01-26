{-# LANGUAGE TypeSynonymInstances #-}

module Lib.Firmabile where

import qualified Codec.Crypto.RSA as RSA
import qualified Codec.Crypto.RSA.Pure as RSAP
import qualified Codec.Crypto.SimpleAES as A (Mode (..), decryptMsg, encryptMsg')
import Crypto.Random (SystemRandom, newGenIO)
import qualified Data.ByteString.Char8 as B (ByteString, concat, pack, take)
import qualified Data.ByteString.Lazy.Char8 as BL (
    ByteString,
    concat,
    foldr,
    pack,
    toChunks,
    unpack,
 )
import Data.Char (ord)
import Data.Monoid (mappend)
import System.IO.Unsafe (unsafePerformIO)

type Firma = BL.ByteString
type Chiave = RSA.PublicKey
type Segreto = BL.ByteString
type Password = String

hash :: String -> BL.ByteString
hash = RSA.hashFunction RSA.hashSHA256 . BL.pack

hashOver :: String -> BL.ByteString -> BL.ByteString
hashOver x k = RSA.hashFunction RSA.hashSHA256 $ BL.pack x `mappend` k

bytestringInteger :: BL.ByteString -> Int
bytestringInteger = BL.foldr (\c x -> ord c + 256 * x) 0

key :: Int -> String -> B.ByteString
key n = B.pack . take n . concat . repeat

cryptobox :: Password -> (Chiave, Segreto)
cryptobox p = unsafePerformIO $ do
    let k = RSA.hashFunction RSA.hashSHA256 . BL.pack $ p
    gen <- newGenIO :: IO SystemRandom
    case RSAP.generateKeyPair gen 1024 of
        Left err -> error $ "RSA key generation failed: " ++ show err
        Right (pu, pr, _) ->
            return
                ( pu
                , A.encryptMsg'
                    A.CBC
                    (key 32 p)
                    (B.take 16 . B.concat . BL.toChunks $ k)
                    (BL.pack $ show $ pr)
                )

sign :: (Show a) => (Segreto, Password) -> a -> Maybe Firma
sign (s, p) y = case reads (BL.unpack . A.decryptMsg A.CBC (key 32 p) $ s) of
    [] -> Nothing
    [(x, _)] -> Just . RSA.sign x . BL.pack . show $ y

verify :: (Show a) => Chiave -> a -> Firma -> Bool
verify c x = RSA.verify c (BL.pack . show $ x)
