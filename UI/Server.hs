
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
amministrazione :: Interfaccia ()
amministrazione = rotonda $ \_ ->
	menu "amministrazione" $ 
			[("selezione del gruppo di acquisto", cambiaGruppo)
			,("scelta del responsabile autore delle dichiarazioni", ensureStato accesso )
			,("modifica della considerazione delle dichiarazioni", ensureGruppo eventLevelSelector)
			,("digestione di tutte le dichiarazioni pubblicate", ensureStato sincronizza )
			,("richiesta nuove chiavi da responsabile", creaChiavi)
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

bootGruppo :: Interfaccia ()
bootGruppo = mano "preparazione stato iniziale del gruppo" $ 
			[("elenco delle chiavi responsabile già inserite", do
				xs <- sepU readBoot 
				P.output $ Response 
					[("elenco chiavi responsabile già inserite",
						ResponseMany $ map (ResponseOne . fst) xs)])
			,("inserimento di un responsabile nel gruppo iniziale (interattivo)", do
				t <- P.libero "inserisci il tuo token"
				u <- P.libero "scegli il tuo nomignolo di utente e responsabile"
				xs <- sepU readBoot
				if u `elem` map fst xs then bocciato "nome utente già utilizzato"
					else do
						mr <- nuovoResponsabile u
						case mr of 
							Nothing -> bocciato "errore di digitazione"
							Just r -> do 
								b <- sepU $ ($ r) . ($t) . assignToken 
								when (not b) $ bocciato 
									"token invalido o già utilizzato"
				)
			,("inserimento di un responsabile nel gruppo iniziale (da remoto)", do
				t <- P.libero "inserisci il tuo token"
				r@(u,_) <- P.upload "carica le tue chiavi"
				xs <- sepU readBoot 
				if u `elem` map fst xs then bocciato "nome utente già utilizzato"
					else do
						b <- sepU $ ($ r) . ($t) . assignToken 
						when (not b) $ bocciato "token invalido o già utilizzato"
				)
			,("gestione tokens",mano "gestione tokens" [
				("tokens non ancora assegnati", do
				t <- P.password "password lettura tokens"
				mxs <- sepU $ ($t) . readTokens 
				case mxs of 
					Just xs -> P.output $ ResponseMany $ map ResponseOne xs
					Nothing -> bocciato $ "password errata"
				),
				("fine forzata della fase di boot", do 
				t <- P.password "password lettura tokens"
				m <- sepU $ ($t) . forceBoot 
				case m of 
					Just xs -> return ()
					Nothing -> bocciato $ "password errata"

				),
				("richiesta di nuovi tokens", do
				n <- P.libero "numero di tokens da aggiungere"
				t <- P.password "password lettura tokens"
				mts <- sepU $ ($n). ($t) . moreTokens 
				case mts of 
					Just () -> return ()
					Nothing -> bocciato $ "password errata"

				)])
			]


cambiaGruppo = do
	gs <- ses (return . queryGruppi) 
	g <- P.scelte (("<nessuno>",Nothing): zip gs (map Just gs)) "seleziona il gruppo di acquisto" 
	ses $ ($g) . writeGruppo

ensureGruppo f = do
	g <- ses readGruppo
	case g of 
		Nothing -> bocciato "manca la selezione del gruppo"
		Just _ -> f 

ensureStato f = ensureGruppo $ do 
	s <- sepU readStato
	case s of 
		Nothing -> bocciato "manca la costruzione del gruppo"
		Just _ -> f

ensureResponsabile f = ensureStato  $ do
	r <- ses readAccesso
	case r of 
		Nothing -> bocciato "manca la selezione del responsabile"
		Just _ -> f

applicazione :: Costruzione MEnv () ()
applicazione = rotonda $ \_ -> do
	menu "menu principale" $ 
				[
				("gestione dichiarazioni" , ensureResponsabile $ rotonda $ \_ -> menu "gestione dichiarazioni" $ dichiarazioni), 
				("interrogazioni sulle attivita'", ensureGruppo $ rotonda $ \_ -> mano "interrogazioni sulle attivita'" 
					[("descrizione della sessione",descrizione),
					("effetto delle ultime dichiarazioni", ensureStato $ do
					c <- fromJust <$> ses readCaricamento 
					P.output . Response $ 
						[("effetto delle ultime dichiarazioni",  c)])]),


				("interrogazione sullo stato del gruppo", ensureStato  interrogazioni),
				("amministrazione", amministrazione)
				]


