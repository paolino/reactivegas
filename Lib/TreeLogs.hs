
module Lib.TreeLogs where

import Data.Function
import Codec.Binary.UTF8.String
import Text.PrettyPrint
import Control.Applicative
import Data.List

import Control.Arrow

--------------------- programma di stampa ------------------------------------------
data Show a => Tree a = Node (a,[Tree a]) | Leaf deriving Show

showTrees :: [[String]] -> String
showTrees =  render . vcat . map renderTree . passa  where
	renderTree Leaf = Text.PrettyPrint.empty
	renderTree (Node (x,ts)) = text x $$ nest 3 (vcat (map renderTree ts))

passa :: (Show a ,Eq a) => [[a]] -> [Tree a]
passa xs = let 	h = map ((head . head) &&& map tail) . groupBy ((==) `on` head) 
		in do 	y <- groupBy ((==) `on` null) xs  -- distinguiamo tra liste vuote e piene
			if null . head $ y then const Leaf <$> y -- una lista di liste vuote é una lista di Leaf
				else Node . second passa <$>  h y -- ogni sottosequenza di una lista  di liste piene che ha 
					-- la stessa testa é un nodo. ricorsivamente analizziamo il resto

eccoILogs :: [([(String, String)], String)] -> String
eccoILogs = decodeString . showTrees . map (\(as,b) -> (map (\(d,e) -> show d ++ ":" ++ e) as) ++ ["------> " ++ b])
{-
eil = map renderTree . passa . map (\(as,b) -> (map (\(d,e) -> show d ++ ":" ++ e) as) ++ ["------> " ++ b])
	where
		renderTree Leaf = ""
		renderTree (Node (x,ts)) = 
-}
