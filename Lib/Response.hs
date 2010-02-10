{-# LANGUAGE StandaloneDeriving, ExistentialQuantification #-}

module Lib.Response where

import Data.Typeable
import Data.Maybe
import Codec.Crypto.RSA
import Text.PrettyPrint

deriving instance Typeable PublicKey

data Response 
	= forall a. (Typeable a, Show a) => ResponseOne a
	| forall a . Show a => ResponseMany [a]
	| forall a . Show a =>  ResponseAL [(String,a)]
	| Response [(String,Response)]

instance Show Response where
	show x = render $ render' x

render' (ResponseOne x) = text (case typeOf x == typeOf "" of True -> fromJust $ cast x ; False -> show x)
render' (ResponseMany xs) = vcat $ map (text . show) xs
render' (ResponseAL xs) = vcat $ map (\(x,y) -> hang (text (x ++ ":")) 3 (text . show$ y)) xs
render' (Response rs) = vcat $ map (\(s,r) -> hang (text (s ++ ":")) 3 (render' r)) rs


