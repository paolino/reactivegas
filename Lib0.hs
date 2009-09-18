{-# LANGUAGE NoMonomorphismRestriction, TypeOperators, MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
module Lib0 where

import Control.Arrow
import Control.Monad.Maybe
import Control.Monad.RWS
import Data.Maybe
import qualified Aspetti
import Codec.Crypto.RSA


import Data.Function
import Codec.Binary.UTF8.String
import Text.PrettyPrint
import Control.Applicative
import Data.List





deriving instance Read PublicKey
deriving instance Ord PublicKey
deriving instance Eq PublicKey

deriving instance Read PrivateKey
deriving instance Ord PrivateKey
deriving instance Eq PrivateKey

-----------------------------------------

update k dv v kvs = case lookup k kvs of
	Nothing -> (k,dv v):kvs
	Just v -> (k,dv v):filter ((/=) k . fst) kvs
assente k kvs = case lookup k kvs of
	Nothing -> True
	Just _ -> False
elimina k kvs = filter ((/=) k . fst) kvs

secondM f (x,y) = f y >>= return . (,) x
updateM k dv v kvs = case lookup k kvs of
	Nothing -> dv v >>= \v' -> return $ (k,v'):kvs
	Just v -> dv v >>= \v' -> return $ (k,v'):filter ((/=) k . fst) kvs
xs ? (k,t) = maybe t id $ lookup k xs 
------------------------------------------
type Indice = Int
type Chiave = PublicKey


--------------------- programma di stampa ------------------------------------------
data Show a => Tree a = Node (a,[Tree a]) | Leaf 

showTrees =  render . vcat . map renderTree . passa  where
	renderTree Leaf = Text.PrettyPrint.empty
	renderTree (Node (x,ts)) = text x $$ nest 3 (vcat (map renderTree ts))

passa :: (Show a ,Eq a) => [[a]] -> [Tree a]
passa xs = let 	h = map ((head . head) &&& map tail) . groupBy ((==) `on` head) 
		in do 	y <- groupBy ((==) `on` null) xs  -- distinguiamo tra liste vuote e piene
			if null . head $ y then const Leaf <$> y -- una lista di liste vuote é una lista di Leaf
				else Node . second passa <$>  h y -- ogni sottosequenza di una lista  di liste piene che ha 
					-- la stessa testa é un nodo. ricorsivamente analizziamo il resto
stampaLogs = 	putStrLn . decodeString . showTrees . map (\(as,b) -> (map (\(d,e) -> show d ++ ":" ++ e) as) ++ ["------> " ++ b])
eccoILogs = decodeString . showTrees . map (\(as,b) -> (map (\(d,e) -> show d ++ ":" ++ e) as) ++ ["------> " ++ b])

