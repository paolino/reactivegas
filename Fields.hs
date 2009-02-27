{-# LANGUAGE StandaloneDeriving #-}
module Fields where

import Data.List
import Text.ParserCombinators.ReadP as P
import Data.Char
import Control.Applicative
import Codec.Crypto.RSA
import Data.Digest.Pure.SHA

import Data.ByteString.Lazy.Char8 
import System.Time

type Responsabile = PublicKey
deriving instance Read PublicKey
deriving instance Ord PublicKey
deriving instance Eq PublicKey

newtype User = User String deriving (Eq,Ord)
instance Read User where
	readsPrec _ = P.readP_to_S  (P.skipSpaces >> User <$> P.munch1 (isAlphaNum))

instance Show User where
	show (User u) = u
newtype Bene = Bene String deriving (Eq,Ord)
instance Show Bene where
	show (Bene o) = o
instance Read Bene where
	readsPrec _ = P.readP_to_S  (P.skipSpaces >> Bene <$> P.munch1 (isAlphaNum))

type Valore = Double
