
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
import Lib.Tokens
import Lib.Modify


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

import UI.Lib

wrapCostrActions 	
	:: (a -> Interfaccia ()) 
	-> [MEnv (SUtente,TS) -> (a -> Interfaccia ()) -> (String -> Interfaccia ()) -> [(String,Interfaccia ())]]
	-> [(String,Interfaccia ())]
wrapCostrActions g = concatMap (\f -> f q g bocciato) where
	q = do 	s <- fst <$> statoSessione
		mu <- fmap fst <$> ses readAccesso
		return (SUtente mu,s)


interrogazioni :: Interfaccia ()
interrogazioni = rotonda $ \_ -> menu "interrogazioni sullo stato del gruppo" $ (wrapCostrActions P.output $ [
		costrQueryAnagrafe,
		costrQueryAccredito,
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
-- amministrazione :: Interfaccia () -> Interfaccia ()
amministrazione ammenu = rotonda $ \k -> do 
	let 	
		sempre = [("selezione del gruppo di acquisto", cambiaGruppo),
				("amministrazione del servizio",mano "amministrazione del servizio" ammenu)] 
		inizio = [("costruzione del gruppo",ensureGruppo $  bootGruppo)] 
		normale = 
			[("scelta del responsabile autore delle dichiarazioni", ensureStato accesso )
			,("modifica della considerazione delle dichiarazioni", ensureGruppo eventLevelSelector)
			,("digestione di tutte le dichiarazioni pubblicate", ensureStato sincronizza )
			,("richiesta nuove chiavi da responsabile", ensureStato creaChiavi)
			,("accesso indiretto" , mano "accesso indiretto" 
				[("carica un aggiornamento individuale", ensureStato caricaAggiornamentoIndividuale )
				,("carica un aggiornamento di gruppo", ensureStato caricaAggiornamentoDiGruppo)
				,("scarica gli aggiornamenti individuali", ensureStato $ 
				  sepU readUPatches >>= \ (n,ups) -> P.download ("patches." ++ show n) ups)
					,("scarica un aggiornamento di gruppo",ensureStato $ scaricaAggiornamentoDiGruppo)
				,("scarica lo stato", ensureStato $ sepU readStato >>= maybe (bocciato "stato non presente") 
						 (\ (n,s) -> P.download ("stato." ++ show n) s))
				])
			]
	costr <- sep $ maybe (return []) (fmap (maybe inizio (const normale)) . readStato) 
	menu "amministrazione" $ sempre ++ costr
			
-- bootGruppo :: Interfaccia ()
bootGruppo = do
	tok <- sepU (return . modTokens) :: Interfaccia (PeekPoke (Tokenizer Responsabile Utente))
	mano "costruzione del gruppo" $ uiTokenizer tok "elenco responsabili raccolti" 
		(P.libero $ "nome del responsabile associato al nuovo token")
		(\next -> next $ \(_,u) -> do
			p <- P.password "password per il nuovo responsabile"
			return (u, cryptobox p))
		(\c -> sepU $ ($c) . forceBoot)

cambiaGruppo = do
	gs <- ses (queryGruppi) 
	g <- P.scelte (("<nessuno>",Nothing): zip gs (map Just gs)) "seleziona il gruppo di acquisto" 
	ses $ ($g) . writeGruppo

ensureGruppo f = do
	g <- ses readGruppo
	case g of 
		Nothing -> bocciato "manca la selezione del gruppo"
		Just _ -> f 

ensureStato f = ensureGruppo $ do 
	s <- sep $ maybe (return Nothing) (fmap Just . readStato)
	case s of 
		Nothing -> bocciato "manca la selezione del gruppo"
		Just Nothing -> bocciato "manca la costruzione del gruppo"
		Just (Just _) -> f

ensureResponsabile f = ensureStato  $ do
	r <- ses readAccesso
	case r of 
		Nothing -> bocciato "manca la selezione del responsabile"
		Just _ -> f

-- applicazione :: Costruzione MEnv () ()
applicazione ammenu = rotonda $ \_ -> do
	menu "menu principale" $ 
				[
				("gestione dichiarazioni" , ensureResponsabile $ rotonda $ \_ -> menu "gestione dichiarazioni" $ dichiarazioni),	
				("descrizione della sessione",descrizione),
				("effetto delle ultime dichiarazioni", ensureStato $ do
					c <- fromJust <$> ses readCaricamento 
					P.output . Response $ 
						[("effetto delle ultime dichiarazioni",  c)]),

				("interrogazione sullo stato del gruppo", ensureStato  interrogazioni),
				("amministrazione", amministrazione ammenu)
				]


