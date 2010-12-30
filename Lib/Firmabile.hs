{-# LANGUAGE StandaloneDeriving,DeriveDataTypeable,  TypeSynonymInstances #-}
module Lib.Firmabile 
	where
import Data.Monoid (mappend)
import qualified Codec.Crypto.RSA as RSA -- (hashFunction, ha_MD5,ha_SHA256, sign,verify,PrivateKey(..),PublicKey (..), generateKeyPair)
import qualified Codec.Crypto.SimpleAES as A  (decryptMsg,encryptMsg',Mode(..))
import qualified Data.ByteString.Char8 as B (ByteString, pack, concat, take)
import qualified Data.ByteString.Lazy.Char8 as BL (ByteString, pack, toChunks, unpack, foldr, concat)
import Data.Typeable (Typeable)
import System.Random (mkStdGen)
import Data.Char (ord)

deriving instance Read RSA.PublicKey
deriving instance Read RSA.PrivateKey

deriving instance Eq RSA.PublicKey
deriving instance Eq RSA.PrivateKey

deriving instance Typeable RSA.PublicKey
deriving instance Typeable RSA.PrivateKey
type Firma = BL.ByteString
type Chiave = RSA.PublicKey
type Segreto = BL.ByteString
type Password = String


hash = RSA.hashFunction RSA.ha_MD5 . BL.pack
hashOver x k = RSA.hashFunction RSA.ha_MD5 $ BL.pack x `mappend` k
bytestringInteger :: BL.ByteString -> Int 
bytestringInteger = BL.foldr (\c x -> ord c + 256 * x) 0 
key :: Int -> String -> B.ByteString
key n = B.pack . take n . concat . repeat

cryptobox :: Password -> (Chiave, Segreto)
cryptobox p = let 
	k = RSA.hashFunction RSA.ha_MD5 . BL.pack $ p
	g = mkStdGen . bytestringInteger $ k
	(pu,pr,_) = RSA.generateKeyPair g 1024
	in (pu, A.encryptMsg' A.CBC  ( key 32 p) ( B.take 16 . B.concat . BL.toChunks $ k)(BL.pack $ show $ pr))

sign :: Show a => (Segreto,Password) -> a -> Maybe Firma
sign (s,p) y = case reads (BL.unpack . A.decryptMsg A.CBC (key 32 p) $ s) of
	[] -> Nothing
	[(x,_)] -> Just . RSA.sign x . BL.pack . show $ y

verify :: Show a => Chiave -> a -> Firma -> Bool
verify c x = RSA.verify c (BL.pack . show $ x)


	
