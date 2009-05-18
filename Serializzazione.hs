{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction #-}
module Serializzazione (serializza, deserializza, SNodo , nodoVuoto) where

import Control.Monad.RWS

import Core (provaAccentratore, Contestuale, Nodo (..), runInserzione , TyReazione, Reazione (..), ParserConRead, Parser (..))

import Control.Monad.RWS
import Control.Monad.Writer
import Control.Applicative
import Control.Arrow
import Data.List
import Data.Maybe
import Data.Either
import Debug.Trace



-- nodo serializzato
data (Read s, Show s) => SNodo s d = SNodo 
	Bool -- stato di attivita della reazion
	[(Contestuale s d,[(Int,SNodo s d)])] -- struttura di deserializzazione  dipendente
	deriving (Read,Show)

nodoVuoto = SNodo True []
-- passa da una struttura priva di azioni monadiche ad una con, data la azione del nodo base 
deserializza :: (Read s, Show s) 
	=> SNodo s d -- il nodo da ricreare
	-> Reazione s c d -- la sua reazione
	-> Nodo s c d -- il nodo vivo ottenuto
deserializza (SNodo k rs) r@(Reazione (acc,f :: TyReazione a b d s c)) =	
	let  te :: (Contestuale s d,[(Int,SNodo s d)]) -> (Contestuale s d,[(Int,Nodo s c d)])
	     te ((ec@(Right (u,x)) , s),js) = ((ec,s),ns) where
		ns = case  (valore :: ParserConRead a -> a) <$> parser x of
			Nothing -> error "deserializzaione fallita nel parser"
			Just y -> let (Just (_,(qs,_)), _,_) = runInserzione (f (Right (u,y))) [] s in 
				map (\(i,d) -> (i,deserializza d (qs !! i))) js
	     te ((ec@(Left x) , s),js) = ((ec , s),ns) where
		ns = case maybe ((valore :: ParserConRead b -> b) <$> parser x) (provaAccentratore x) acc of
			Nothing -> error "deserializzaione fallita nel parser"
			Just y -> let (Just (_,(qs,_)), _, _) = runInserzione (f (Left y)) [] s in 
				map (\(i,d) -> (i,deserializza d (qs !! i))) js
	in Nodo (if k then Just r else Nothing) (map te rs)

-- passa da una struttura contenente azioni monadiche ad una priva
serializza :: (Read s, Show s) => Nodo s c d -> SNodo s d
serializza (Nodo k rs) = SNodo (isJust k) (map (second $ map (second serializza))  rs)	


