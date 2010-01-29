{-# LANGUAGE ExistentialQuantification #-}

module Lib.Response where

import Text.PrettyPrint


data Response 
	= forall a. Show a => ResponseOne a
	| forall a . Show a => ResponseMany [a]
	| forall a . Show a =>  ResponseAL [(String,a)]
	| Response [(String,Response)]

instance Show Response where
	show x = render $ render' x

render' (ResponseOne x) = text (show x)
render' (ResponseMany xs) = vcat $ map (text . show) xs
render' (ResponseAL xs) = vcat $ map (\(x,y) -> hang (text (x ++ ":")) 3 (text . show$ y)) xs
render' (Response rs) = vcat $ map (\(s,r) -> hang (text (s ++ ":")) 3 (render' r)) rs


