{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification #-}
-- | un modulo per il riordinamento di valori basato su una serie di priorita' ricavabili dal parsing delle loro rappresentazioni
module Lib.Prioriti (sortP, levelsP, R (..)) where

import Control.Arrow ((&&&))
import Control.Applicative  ((<$>))
import Control.Monad (msum)

import Data.Maybe (listToMaybe)
import Data.Ord (comparing)
import Data.List (sortBy)
import Data.Char (ord)

-- | una scatola per le priorita'
data R = forall a. Read a => R (a -> Int) 

-- | la funzione riordinante 
sortP :: Int -> [R] -> (a -> String) -> [a] -> [a]
sortP l rs f = map fst . takeWhile ((<=l) . snd) . sortBy (comparing snd) . map (id &&& maybe 0 id . lss rs)
	where
		lss ps x = msum $ map (ls x) ps
		ls x (R (p :: a -> Int)) = p <$> (read' :: String -> Maybe a) (f x) 
		read' x = fst <$> listToMaybe (reads x)

levelsP rs f = sortBy (comparing snd) . map (id &&& maybe 0 id . lss rs)
	where
		lss ps x = msum $ map (ls x) ps
		ls x (R (p :: a -> Int)) = p <$> (read' :: String -> Maybe a) (f x) 
		read' x = fst <$> listToMaybe (reads x)

test = sortP 800 [R id, R ord , R (length :: String -> Int)] id ["'a'","3","\"piox\""]
	== ["3","\"piox\"","'a'"]
