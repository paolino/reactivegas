module Lib.Database where

import Data.List (lookup)
import Data.Maybe (listToMaybe)
import Control.Arrow (second)
import Control.Applicative ((<$>))


-- | interfaccia di un DB
data DB a b = DB 
	{ query :: a -> Maybe b
	, lkey :: Maybe a
	, set :: (a,b) -> DB a b
	, forget :: a -> DB a b 
	, dbmap :: (b -> b) -> DB a b
	, dump :: [(a,b)]
	}

restoreDB l = foldr (flip set) (limitedDB l)

-- | un DB inefficiente a memoria limitata 
limitedDB :: (Show a, Eq a) 
	=> Int 	-- ^ massimo numero di elementi
	-> DB a b
limitedDB limit = let
	q xs x = lookup x xs
	l xs = fst <$> listToMaybe xs
	s xs (x,y) = mkdb . take limit $ (x,y) : xs 
	f xs x = mkdb . filter ((/=) x . fst) $ xs	
	m xs f = mkdb . map (second f) $ xs 
	mkdb xs = DB (q xs) (l xs) (s xs) (f xs) (m xs) xs
	
	in mkdb []

