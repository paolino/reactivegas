{-# LANGUAGE EmptyDataDecls , GADTs, MultiParamTypeClasses, ScopedTypeVariables, FlexibleInstances,FlexibleContexts #-}

module Core.Dichiarazioni where

import Control.Monad (msum)
import Control.Arrow ((&&&))
import Data.List (partition)
import Data.Typeable 
import Data.Maybe (isNothing, catMaybes,mapMaybe)
import Core.Types
import Core.Parsing

data Singola
data Composta
data Dichiarazione c  b where
	Singola :: (Show a, Read a, Parser c a) => a ->  Dichiarazione c  Singola
	Composta :: (Show a, Read a, Patch a, Parser c a, Typeable a) => [a] -> Dichiarazione c  Composta

data Dichiarazioni c  = Dichiarazioni {
	multiple 	:: [Dichiarazione c  Singola],
	uniche		:: [Dichiarazione c  Composta]	
	}

class Patch a where
	patch :: [a] -> a -> [a]
	nulla :: a -> Bool

parseSingola :: forall c . Evento -> Dichiarazione c  Singola -> Maybe (Dichiarazione c  Singola)
parseSingola y (Singola (w :: a)) = Singola `fmap` (valore :: c a -> a) `fmap` parser y

parseComposta :: forall c . Evento -> Dichiarazione c  Composta  ->  Maybe (Dichiarazione c  Composta)
parseComposta y (Composta (ws :: [a])) = Composta `fmap` patch ws `fmap` (valore :: c a -> a) `fmap`  parser y

toEventi :: forall  c . Dichiarazioni c  -> [Evento]
toEventi (Dichiarazioni xs ys) = map f xs ++ concatMap g ys where
	f :: Dichiarazione c Singola -> Evento
	f (Singola (x :: a)) = (serializza :: c a -> String) . boxer $ x
	g :: Dichiarazione c Composta -> [Evento]
	g (Composta (xs :: [a])) = mapMaybe (\x -> if nulla x then Nothing else Just . (serializza :: c a -> String) . boxer $ x)  xs

holing :: [a] -> [(a,[a])]
holing = holing' [] where
	holing' _ [] = []
	holing' ys (x:xs) = (x,reverse ys ++ xs):holing' (x:ys) xs

parseDichiarazione ::  [Dichiarazione c  Singola] -> Evento -> Dichiarazioni c  -> Dichiarazioni c 
parseDichiarazione ss x (Dichiarazioni ys zs) = case msum $ map (parseSingola x) ss of
	Just y -> Dichiarazioni (y:ys) zs
	Nothing -> case msum . map (\(z,zs') -> ((,) zs') `fmap` parseComposta x z) $ holing zs of
		Just (zs',z) -> Dichiarazioni ys (z:zs')
		Nothing -> error $ "dichiarazione inaccettabile: " ++ x
	
parseDichiarazioni :: [Dichiarazione c  Singola] -> [Dichiarazione c  Composta] -> [Evento] -> Dichiarazioni c 
parseDichiarazioni ss cs = foldr (parseDichiarazione ss ) (Dichiarazioni [] cs) 

aggiungi :: Dichiarazione c  Singola -> Dichiarazioni c  -> Dichiarazioni c 
aggiungi x (Dichiarazioni ms us) = Dichiarazioni (x:ms) us

elimina :: forall c  . Dichiarazioni c  -> [(String,Dichiarazioni c )]
elimina (Dichiarazioni ms us) = map f (holing ms) ++ concatMap g (holing us) where
	f :: (Dichiarazione c Singola,[Dichiarazione c Singola]) -> (Evento,Dichiarazioni c)
	f (Singola (x :: a),xs) = ((serializza :: c a -> String) . boxer $ x, Dichiarazioni xs us)
	g :: (Dichiarazione c Composta,[Dichiarazione c Composta]) -> [(Evento,Dichiarazioni c)]
	g (Composta (x :: [a]),xs) = map q $ holing x where
		q (y,ys) = ((serializza :: c a -> String) . boxer $ y, if null ys then Dichiarazioni ms xs else 
			Dichiarazioni ms (Composta ys:xs))

correggi :: forall c. Dichiarazione c  Composta -> Dichiarazioni c  -> Dichiarazioni c 
correggi (Composta y) (Dichiarazioni ms us) = Dichiarazioni ms $ case msum . map f $ holing us of
		Nothing -> (Composta y: us)
		Just (us,u) -> (u:us)
	where
	f :: (Dichiarazione c Composta, [Dichiarazione c Composta]) -> Maybe ([Dichiarazione c Composta], Dichiarazione c Composta)
	f (Composta x,us) = ((,) us) `fmap` Composta `fmap` foldr (flip patch) x `fmap` sequence (map cast y)
{-
-}
