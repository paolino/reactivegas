
{-# LANGUAGE FlexibleContexts, Rank2Types, ExistentialQuantification, ScopedTypeVariables, GeneralizedNewtypeDeriving, NoMonomorphismRestriction, ImplicitParams #-}
module UI.Console (applicazione) where

import Data.Maybe (isJust , fromJust,catMaybes)
import Data.List (delete,find,(\\))

import Control.Arrow
import Control.Applicative
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import Debug.Trace


import Lib.Passo (Costruzione,mano, menu, rotonda ,rmenu, Passo) 
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

import Applicazioni.Reactivegas (QS,caricamento, TS, sortEventi, levelsEventi, maxLevel)
import Applicazioni.Persistenza (Persistenza (..))
import Applicazioni.Sessione (Sessione (..))

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
		costrQueryAcquisto,
		costrQueryImpegni,
		costrQueryAssenso
		]) 

dichiarazioni = onAccesso . const . mano "produci dichiarazioni" . concat $ 
		[wrapCostrActions addEvento [costrEventiAccredito]
		,wrapCostrActions addEvento [costrEventiAcquisto]
		,wrapCostrActions addEvento [costrEventiImpegno]
		,wrapCostrActions addEvento [costrEventiAnagrafe ,costrEventiResponsabili]
		,wrapCostrActions addEvento [costrEventiAssenso]
		]

applicazione :: Interfaccia ()
applicazione = rotonda $ \_ -> do 
	ms <- sel $ readStato . fst 
	case ms of 
		Nothing ->  P.errore $ ResponseOne "il gruppo non esiste ancora" 
		Just s ->  
			mano ("menu principale") 
				[("produci dichiarazioni" ,  dichiarazioni)
				,("pubblica le dichiarazioni in sessione",salvataggio)
				,("scarica un aggiornamento individuale",scaricaAggiornamentoIndividuale)
				,("elimina delle dichiarazioni",eliminazioneEvento)

				 
				,("responsabile autore delle dichiarazioni", accesso >> return ())
				,("modifica il livello di considerazione delle ultime dichiarazioni", eventLevelSelector)
				,("scarica nuove chiavi da responsabile", creaChiavi)
				,("digerisci tutte le dichiarazioni pubblicate", sincronizza)
				,("carica un aggiornamento individuale", caricaAggiornamentoIndividuale )
				,("carica un aggiornamento di gruppo",caricaAggiornamentoDiGruppo)
				,("scarica un aggiornamento di gruppo", scaricaAggiornamentoDiGruppo)
				,("effetto delle ultime dichiarazioni", effetto)
				,("descrizione sessione", descrizione)
				,("interrogazione sullo stato del gruppo", interrogazioni)
				]

