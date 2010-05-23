{-# LANGUAGE ViewPatterns, ScopedTypeVariables#-}
-- | la colla che forma il Core per facilitare il caricamento di un blocco di eventi e per accedere allo stato, partendo dalla serializzazione delle reazioni
--  Modulo per la serializzazione dell'albero delle reazioni. La serializzazione e' complicata dal fatto che i nodi contengono le procedure di reazione.
-- La soluzione e' ricreare le procedure . Per fare questo e' necessaro ricaricare  un set di eventi che essendo potenzialmente discontinui nel tempo vengono memorizzati ognuno con lo stato in cui deve essere caricato per riprodurre le reazioni desiderate. 
-- Da notare che lo stato dell'applicazion in se e' serializzabile. Il problema nasce dal fatto che le reazioni sono dinamiche , nascono a volte dal caricamento degli eventi
module Core.Controllo where

import Data.Maybe (isJust)

import Control.Monad (foldM)
import Control.Applicative ((<$>))
import Control.Arrow (second)

import Lib.Missing (foldDeleteMb)
import Lib.Prioriti (R,sortP)

import Core.Types (Esterno)
import Core.Programmazione (Message,runInserzione, provaAccentratore, Reazione (..), TyReazione)
import Core.Nodo (Appuntato (..), Nodo (..))
import Core.Contesto (nuovoContesto,  Contestualizzato)
import Core.Parsing (valore,parser, ParserConRead)
import Core.Inserimento (inserimentoCompleto)

import Debug.Trace

-- | nodo serializzato , una copia di una struttura Nodo non contenente la funzione di reazione del nodo stesso
data SNodo s d = SNodo { 
	attivo :: Bool, 					-- ^ stato di attivita della reazione
	sottonodi :: [(Appuntato s d,[(Int,SNodo s d)])] 	-- ^ struttura di deserializzazione dipendente
	}
	deriving (Read,Show,Eq)

-- | un SNodo vuoto
nodoVuoto ::  SNodo s d
nodoVuoto = SNodo True []

-- | passa da una struttura SNodo a una Nodo con il contributo della reazione, le reazioni dei nodi seguenti sono costruite con l'inserimento degli eventi appositamente ricordati 
deserializza 
	:: SNodo s d 		-- ^ il nodo serializzato da ricreare
	-> Reazione s c d 	-- ^ la sua reazione
	-> Nodo s c d 		-- ^ il nodo vivo ottenuto
deserializza (SNodo k rs) r@(Reazione (acc,f :: TyReazione a b d s c)) =	
	let  -- te :: (Contestuale s d,[(Int,SNodo s d)]) -> (Contestuale s d,[(Int,Nodo s c d)])
	     te ((ec@(Right (u,x)) , s),js) = ((ec,s),ns) where
		ns = case  (valore :: ParserConRead a -> a) <$> parser x of
			Nothing -> error "deserializzaione fallita nel parser"
			Just y -> let (Just (_,(qs,_)), _,_) = runInserzione (f (Right (u,y))) nuovoContesto s in 
				map (\(i,d) -> (i,deserializza d (qs !! i))) js
	     te ((ec@(Left x) , s),js) = ((ec , s),ns) where
		ns = case maybe ((valore :: ParserConRead b -> b) <$> parser x) (provaAccentratore x) acc of
			Nothing -> error "deserializzaione fallita nel parser"
			Just y -> let (Just (_,(qs,_)), _, _) = runInserzione (f (Left y)) nuovoContesto s in 
				map (\(i,d) -> (i,deserializza d (qs !! i))) js
	in Nodo (if k then Just r else Nothing) (map te rs)

-- | passa da una struttura Nodo a una SNodo, naturalmente la funzione reazione del nodo base deve essere la stessa quando verra' deserializzato
serializza 
	:: Nodo s c d 	-- ^ il nodo vivo 
	-> SNodo s d	-- ^ il nodo serializzato
serializza (Nodo k rs) = SNodo (isJust k) (map (second $ map (second serializza))  rs)	

-- | programma di caricamento eventi, prevede il riordinamento per priorita
caricaEventi :: Show d 
	=> [R] 			-- ^ i prioritizzatori
	-> [Reazione s c d] 	-- ^ le reazioni base
	-> Int 			-- ^ livello di caricamento
	-> [Esterno d] 		-- ^ gli eventi da caricare
	-> (s,[SNodo s d]) 	-- ^ lo stato e la serializzazione dell'albero reattivo
	-> ((s,[SNodo s d]),[Contestualizzato d Message])-- ^ nuovo stato e nuova  serializzazione dell'albero reattivo insieme ai log contestualizzati
caricaEventi ps rs l xs (s,nss) = 
	let 	ns = map (uncurry deserializza) $ zip nss rs
		xs' = sortP l ps snd xs
		((ns',ahi),s',ws) = runInserzione (foldDeleteMb inserimentoCompleto ns xs') nuovoContesto s
		nss' =  map serializza ns'
	in ((s',nss'),ws)


----------------------------------------
