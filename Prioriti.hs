{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, MultiParamTypeClasses #-}

module Prioriti (sortP, R (..)) where

import Control.Arrow
import Control.Applicative 
import Control.Monad

import Data.Maybe (listToMaybe)
import Data.Ord (comparing)
import Data.List (sortBy)
import Data.Char (ord)

data R = forall a. Read a => R (a -> Int) 

sortP :: [R] -> (a -> String) -> [a] -> [a]
sortP rs f = map fst . sortBy (comparing snd) . map (id &&& maybe 0 id . lss rs)
	where
		lss ps x = msum $ map (ls x) ps
		ls x (R (p :: a -> Int)) = p <$> (read' :: String -> Maybe a) (f x) 
		read' x = fst <$> listToMaybe (reads x)

test = sortP [R id, R ord , R (length :: String -> Int)] id ["'a'","3","\"piox\""]
	== ["3","\"piox\"","'a'"]
