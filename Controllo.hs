{-# LANGUAGE ViewPatterns, ScopedTypeVariables, NoMonomorphismRestriction #-}
-- | un wrapper intorno al Core per facilitare il caricamento di un blocco di eventi e per accedere allo stato, partendo dalla serializzazione delle reazioni
--  Modulo per la serializzazione dell'albero delle reazioni. La serializzazione e' complicata dal fatto che i nodi contengono le procedure di reazione.
-- La soluzione e' ricreare le procedure . Per fare questo e' necessaro ricaricare  un set di eventi che essendo potenzialmente discontinui nel tempo vengono memorizzati ognuno con lo stato in cui deve essere caricato per riprodurre le reazioni desiderate. 
-- Da notare che lo stato dell'applicazion in se e' serializzabile. Il problema nasce dal fatto che le reazioni sono dinamiche , nascono a volte dal caricamento degli eventi
module Controllo where

import Control.Monad.RWS
import Control.Applicative
import Control.Arrow
import Data.List
import Data.Maybe
import Data.Either
import Debug.Trace

import Core 	
	-- (Nodo, runInserzione , inserimentoCompleto, Reazione, mkNodi, reattore, Motivato')
import Prioriti (R,sortP)
-- | nodo serializzato , una copia di una struttura Nodo non contenente la funzione di reazione del nodo stesso
data (Read s, Show s) => SNodo s d = SNodo 
	Bool 					-- ^ stato di attivita della reazione
	[(Appuntato s d,[(Int,SNodo s d)])] 	-- ^ struttura di deserializzazione  dipendente
	deriving (Read,Show)

-- | un SNodo vuoto
nodoVuoto = SNodo True []

-- | passa da una struttura SNodo a una Nodo con il contributo della reazione, le reazioni dei nodi seguenti sono costruite con l'inserimento degli eventi appositamente ricordati 
deserializza :: (Read s, Show s) 
	=> SNodo s d 		-- ^ il nodo serializzato da ricreare
	-> Reazione s c d 	-- ^ la sua reazione
	-> Nodo s c d 		-- ^ il nodo vivo ottenuto
deserializza (SNodo k rs) r@(Reazione (acc,f :: TyReazione a b d s c)) =	
	let  -- te :: (Contestuale s d,[(Int,SNodo s d)]) -> (Contestuale s d,[(Int,Nodo s c d)])
	     te ((ec@(Right (u,x)) , s),js) = ((ec,s),ns) where
		ns = case  (valore :: ParserConRead a -> a) <$> parser x of
			Nothing -> error "deserializzaione fallita nel parser"
			Just y -> let (Just (_,(qs,_)), _,_) = runInserzione (f (Right (u,y))) Boot s in 
				map (\(i,d) -> (i,deserializza d (qs !! i))) js
	     te ((ec@(Left x) , s),js) = ((ec , s),ns) where
		ns = case maybe ((valore :: ParserConRead b -> b) <$> parser x) (provaAccentratore x) acc of
			Nothing -> error "deserializzaione fallita nel parser"
			Just y -> let (Just (_,(qs,_)), _, _) = runInserzione (f (Left y)) Boot s in 
				map (\(i,d) -> (i,deserializza d (qs !! i))) js
	in Nodo (if k then Just r else Nothing) (map te rs)

-- | passa da una struttura Nodo a una SNodo, naturalmente la funzione reazione del nodo base deve essere la stessa quando verra' deserializzato
serializza :: (Read s, Show s) 
	=> Nodo s c d 	-- ^ il nodo vivo 
	-> SNodo s d	-- ^ il nodo serializzato
serializza (Nodo k rs) = SNodo (isJust k) (map (second $ map (second serializza))  rs)	

-- | programma di caricamento eventi, prevede il riordinamento per priorita
caricaEventi :: (Show d, Read s, Show s,Monad m) 
	=> [R] 			-- ^ i prioritizzatori
	-> [Reazione s c d] 	-- ^ le reazioni base
	-> [Esterno d] 		-- ^ gli eventi da caricare
	-> (s,[SNodo s d]) 	-- ^ lo stato e la serializzazione dell'albero reattivo
	-> m ((s,[SNodo s d]),[Contestualizzato d String])-- ^ nuovo stato e nuova  serializzazione dell'albero reattivo insieme ai log contestualizzati
caricaEventi ps rs xs (s,nss) = do
	let 	ns = map (uncurry deserializza) $ zip nss rs
		xs' = sortP ps snd xs
		(ns',s',ws) = runInserzione (foldM (\ns x -> inserimentoCompleto x ns) ns xs') Boot s
		nss' =  map serializza ns'
	return ((s',nss'),ws)

----------------------------------------
