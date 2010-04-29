{-# LANGUAGE NoMonomorphismRestriction #-}
module Lib.QInteger (makeQInteger, fromQInteger,QInteger) where
import Debug.Trace

import Data.List (unfoldr)
import Text.Printf

nq= 4

data QInteger = QInteger [Int] deriving (Eq,Ord)

instance Show QInteger where
	show (QInteger xs) = concatMap (printf ("%0" ++ show nq ++ "d ")) $ xs


readQ [] = ([],"")
readQ xs = let (ps,qs) =  splitAt (nq + 1) $ dropWhile (== ' ') xs in
		if length ps < (nq + 1) then
			([],xs) else
				case reads ps of
					[] -> ([],xs)
					[(i," ")] -> let (js,rs) = readQ qs in (i:js ,rs) 
					_ -> ([],xs)

instance Read QInteger where
	readsPrec _ x = case readQ x of
		([],xs) -> []
		(js,rs) -> [(QInteger $ js,rs)]

takeQ :: Integer -> (Int, Integer) 
takeQ x = let (a,b) = x `divMod` (10 ^ nq )	in (fromIntegral b,a)

makeQInteger :: Integer -> QInteger
makeQInteger = QInteger . unfoldr f
	where 	f 0 = Nothing
		f x = Just $ takeQ x

fromQInteger :: QInteger -> Integer
fromQInteger (QInteger xs) = foldr (\x y -> y * 10 ^ nq + fromIntegral x) 0 $ xs
