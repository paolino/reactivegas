{-# LANGUAGE ExistentialQuantification, NoMonomorphismRestriction, ScopedTypeVariables, Rank2Types, TypeSynonymInstances #-}
module Lib.States where

import Control.Arrow (first)

data ToPast = forall a . (Show a, Transition a) => ToPast a
data FromPast a = forall b . (Read b, Transition b) => FromPast (b -> a)

class Transition a where
	back :: a -> Maybe ToPast
	forth :: Maybe (FromPast a)


tryShowF :: (forall a . Show a =>  a -> Bool) -> ToPast -> Bool
tryShowF f (ToPast x) = f x || maybe False (tryShowF f) (back x)


tryRead = tryRead' (FromPast id) where
	tryRead' :: forall a . FromPast a -> ReadS a
	tryRead' (FromPast (f :: b -> a)) x = case map (first f) $ reads x of
		[] -> case forth :: Maybe (FromPast b) of
			Nothing -> []
			Just t -> map (first f) $ tryRead' t x
		q -> q


------------------------ example -----------------------

data Inside = Inside deriving (Show,Read)
data Outside = Outside () Inside deriving (Show,Read)


instance Transition Outside where
	back (Outside () Inside) = Just (ToPast Inside)
	forth = Just $ FromPast (Outside ())

instance Transition Inside where
	back Inside = Nothing
	forth = Nothing

main = (tryRead  :: ReadS Outside) "Inside"	
