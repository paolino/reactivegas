
{-# LANGUAGE FlexibleContexts, Rank2Types, ExistentialQuantification, ScopedTypeVariables, GeneralizedNewtypeDeriving, NoMonomorphismRestriction, ImplicitParams #-}
module UI.Server where

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
wrapCostrActions g = concatMap (\f -> f q g bocciato) where
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

dichiarazioni = concat $ 
		[wrapCostrActions addEvento [costrEventiAccredito]
		,wrapCostrActions addEvento [costrEventiAcquisto]
		,wrapCostrActions addEvento [costrEventiImpegno]
		,wrapCostrActions addEvento $ 
			[costrEventiAnagrafe 
			,costrEventiResponsabili
			]
		,wrapCostrActions addEvento [costrEventiAssenso]

		,	[("----------",return ())
			,("pubblica le dichiarazioni in sessione",salvataggio)
			,("elimina delle dichiarazioni",eliminazioneEvento)
			]

		]
amministrazione :: Interfaccia ()
amministrazione = do
	


	mano "amministrazione" $ 
			[("responsabile autore delle dichiarazioni", accesso >> return ())
			,("livello di considerazione delle ultime dichiarazioni", eventLevelSelector)
			,("digerisci tutte le dichiarazioni pubblicate", sincronizza )
			,("scarica nuove chiavi da responsabile", creaChiavi)
			,("accesso indiretto" , mano "accesso indiretto" 
				[("carica un aggiornamento individuale", caricaAggiornamentoIndividuale )
				,("carica un aggiornamento di gruppo", caricaAggiornamentoDiGruppo)
				,("scarica gli aggiornamenti individuali", 
					sel (readUPatches . fst) >>= \ (n,ups) -> P.download ("patches." ++ show n) ups)
				,("scarica un aggiornamento di gruppo",scaricaAggiornamentoDiGruppo)
				,("scarica lo stato", sel (readStato . fst) >>= maybe (bocciato "stato non presente") 
					(\ (n,s) -> P.download ("stato." ++ show n) s))
				])
			]


applicazione :: Costruzione MEnv () ()
applicazione = rotonda $ \_ -> do 
	ms <- sel $ readStato . fst 
	case ms of 
		Nothing ->    -- un bel po rotto
			mano "il gruppo non esiste ancora" 
				[("creazione nuova identita' di responsable", bootChiavi)
				,("preparazione stato iniziale di gruppo", bootGruppo)
				]
		Just s ->  
			mano ("menu principale") [
				("effetto delle ultime dichiarazioni", do
					c <- sel (readCaricamento . snd) 
					P.output . Response $ 
						[("effetto delle ultime dichiarazioni",  c)]),

				("gestione dichiarazioni" , onAccesso . const $ mano "gestione dichiarazioni" $ dichiarazioni), 
				("descrizione sessione", descrizione),
				("interrogazione sullo stato del gruppo", interrogazioni),
				("amministrazione",amministrazione)
				]

{-
-}
