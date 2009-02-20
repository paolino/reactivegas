{-# LANGUAGE StandaloneDeriving #-}
module Crypto where

import Codec.Crypto.RSA
import Data.ByteString.Lazy.Char8 
import Data.List
import Text.ParserCombinators.ReadP as P
import System.Random
import System.Console.GetOpt
import System.Environment
import Data.Maybe 
import Data.Char
import Control.Applicative

import Fields

deriving instance Read PublicKey

newtype Firma = Firma ByteString
instance Show Firma where
	show (Firma bs) = "Firma "++ show (Data.List.map ord . unpack $ bs)
instance Read Firma where
	readsPrec _ = P.readP_to_S (string "Firma" >> P.skipSpaces >> Firma . pack . Data.List.map chr <$> P.readS_to_P reads) 


