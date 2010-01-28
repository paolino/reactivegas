{-# LANGUAGE ExistentialQuantification #-}

module Lib.Response where

import Text.PrettyPrint


data Response 
	= forall a. Show a => ResponseOne a
	| forall a . Show a => ResponseMany [a]
	| forall a . Show a =>  ResponseAL [(String,a)]
	| Response [(String,Response)]

renderResponse (ResponseOne x) = text (show x)
renderResponse (ResponseMany xs) = vcat $ map (text . show) xs
renderResponse (ResponseAL xs) = vcat $ map (\(x,y) -> hang (text (x ++ ":")) 3 (text . show $ y)) xs
renderResponse (Response rs) = vcat $ map (\(s,r) -> hang (text (s ++ ":")) 3 (renderResponse r)) rs


