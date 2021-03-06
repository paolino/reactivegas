
{-# LANGUAGE FlexibleContexts, Rank2Types, ExistentialQuantification, ScopedTypeVariables, GeneralizedNewtypeDeriving, NoMonomorphismRestriction, ImplicitParams #-}
module UI.Console (interfaccia) where

import Data.Maybe (isJust , fromJust,catMaybes)
import Data.List (delete,find,(\\))

import Control.Arrow
import Control.Applicative
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import Debug.Trace

import Lib.Passo (Costruzione,mano, menu, rotonda ,rmenu, Passo,svolgi, HPasso ) 
import qualified Lib.Passo as P

import Lib.TreeLogs (eccoILogs)
import Lib.Firmabile (cryptobox, Chiave)
import Lib.Prioriti (R)
import Lib.Response (Response (..))


import Core.Types (Esterno, Evento, Utente, Responsabile)
import Core.Controllo (caricaEventi, SNodo (..))
import Core.Contesto (flatten)
import Core.Programmazione (Reazione)
import Core.Parsing (ParserConRead)
import Core.Patch ( Firmante (..),firmante, Patch, fromPatch)
import Core.Costruzione (runSupporto, Supporto)

import Eventi.Anagrafe
import Eventi.Accredito
import Eventi.Impegno
import Eventi.Acquisto

import Applicazioni.Reactivegas (QS,bianco, TS, sortEventi, levelsEventi, maxLevel)
import Applicazioni.Persistenza (Persistenza (..))
import Applicazioni.Sessione (Sessione (..))

import Lib.Console
import UI.Lib

wrapCostrActions 	
	:: (a -> Interfaccia ()) 
	-> [MEnv (SUtente,TS) -> (a -> Interfaccia ()) -> (String -> Interfaccia ()) -> [(String,Interfaccia ())]]
	-> [(String,Interfaccia ())]
wrapCostrActions g = concatMap (\f -> map (second (>> effetto)) $ f q g bocciato) where
	q = do 	s <- fst <$> statoSessione
		mu <- fmap fst <$> sel (readAccesso . snd)
		return (SUtente mu,s)


interrogazioni :: Interfaccia ()
interrogazioni = mano "interrogazioni sullo stato del gruppo" $ (wrapCostrActions P.output $ [
		costrQueryAnagrafe,
		costrQueryAccredito,
		costrQueryImpegni,
		costrQueryAssenso
		]) 

dichiarazioni k = onAccesso . const . mano "gestione dichiarazioni" $ concat  
		[wrapCostrActions addEvento [costrEventiAccredito]
		,wrapCostrActions addEvento [costrEventiAcquisto]
		,wrapCostrActions addEvento [costrEventiImpegno]
		,wrapCostrActions addEvento [costrEventiAnagrafe ,costrEventiResponsabili]
		,wrapCostrActions addEvento [costrEventiAssenso]
		] ++ [	 ("elimina delle dichiarazioni",eliminazioneEvento)
			,("modifica il livello di considerazione delle ultime dichiarazioni", eventLevelSelector)
			,("uscita",salvataggio >> k ())
			]

baseloop :: Interfaccia ()
baseloop = rotonda $ \k -> do 
	ms <- sel $ readStato . fst 
	case ms of 
		Nothing ->  P.errore $ ResponseOne "il gruppo non esiste ancora" 
		Just s ->  
			menu ("menu principale") 
				[("uscita",salvataggio >> k ())
				,("gestione dichiarazioni" ,  dichiarazioni k)
				,("effetto delle ultime dichiarazioni del gruppo", effetto)
				,("scarica nuove chiavi da responsabile", creaChiavi)
				,("digerisci tutte le dichiarazioni pubblicate (sincronizzatore)", sincronizza)
				]

interfaccia :: MEnv ()
interfaccia = svolgi baseloop >>= interazione 
