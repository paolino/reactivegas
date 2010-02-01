module Lib.Firmabile 
	where

import qualified Codec.Crypto.RSA as RSA (sign,verify,PrivateKey,PublicKey)
import qualified Codec.Crypto.SimpleAes as A 
import qualified Data.ByteString.Lazy.Char8 as B (ByteString)


type Firma = B.ByteString
type Chiave = RSA.PublicKey
type Segreto = B.ByteString
type Password = String

mkResponsabile :: IO 

class Firmabile a where
	hash :: a -> B.ByteString

sign :: Firmabile a => Segreto -> a -> Firma
sign c = RSA.sign c . hash

verify :: Firmabile a => Chiave -> a -> Firma -> Bool
verify c x = RSA.verify c (hash x)

