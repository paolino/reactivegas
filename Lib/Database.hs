module Lib.Database where

import Data.List (lookup)
import Data.Maybe (listToMaybe, isJust)
import Control.Arrow (second)
import Control.Applicative ((<$>))


-- | interfaccia di un DB
data DB a b = DB 
	{ query :: a -> Maybe b
	, lkey :: Maybe a
	, select :: (a -> Bool) -> [(a,b)]
	, set :: (a,b) -> DB a b
	, forget :: a -> DB a b 
	, dbmap :: (b -> b) -> DB a b
	, purge :: (a -> Bool) -> DB a b
	, dump :: [(a,b)]
	, exists :: a -> Bool
	}

restoreDB l = foldr (flip set) (limitedDB l)

-- | un DB inefficiente a memoria limitata 
limitedDB :: (Show a, Eq a) 
	=> Int 	-- ^ massimo numero di elementi
	-> DB a b
limitedDB limit = let
	q xs x = lookup x xs
	l xs = fst <$> listToMaybe xs
	se xs f = filter (f . fst) xs
	s xs (x,y) = mkdb . take limit $ (x,y) : filter ((/=) x . fst) xs 
	f xs x = mkdb . filter ((/=) x . fst) $ xs	
	m xs f = mkdb . map (second f) $ xs
	p xs f = mkdb . filter (not . f . fst) $ xs  
	e xs = isJust . q xs 
	mkdb xs = DB (q xs) (l xs) (se xs) (s xs) (f xs) (m xs) (p xs) xs (e xs)
	
	in mkdb []

