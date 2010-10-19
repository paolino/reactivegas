
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
import Applicazioni.Amministratore (Amministratore (..))

import Lib.Tokenizer
import UI.Lib

wrapCostrActions 	
	:: (a -> Interfaccia ()) 
	-> [MEnv (SUtente,TS) -> (a -> Interfaccia ()) -> (String -> Interfaccia ()) -> [(String,Interfaccia ())]]
	-> [(String,Interfaccia ())]
wrapCostrActions g = concatMap (\f -> f q g (bocciato "costruzione di una dichiarazione")) where
	q = do 	s <- fst <$> statoSessione
		mu <- fmap fst <$> ses readAccesso
		return (SUtente mu,s)


interrogazioni :: Interfaccia ()
interrogazioni = rotonda $ \_ -> menu "interrogazioni sullo stato del gruppo" $ (wrapCostrActions (P.output True) $ [
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
			,("pubblica le dichiarazioni in sessione",salvataggio "pubblica le dichiarazioni in sessione")
			,("elimina delle dichiarazioni",eliminazioneEvento "elimina delle dichiarazioni")
			]

		]
richiesta_nuovo_gruppo :: Interfaccia ()
richiesta_nuovo_gruppo = do 
	n <- P.libero "nome del nuovo gruppo"
	t <- sea $ ($n) . controlla_nome 
	if t then do
		(m :: String) <- P.libero "nome del primo responsabile"
		p <- P.password "password del primo responsabile"
		P.download (n ++ ".richiesta") "scarica file di richiesta inserimento nuovo gruppo" (n,(m, cryptobox p))
		else bocciato "nome del nuovo gruppo" "nome non disponibile"
accettazione_nuovo_gruppo :: Interfaccia ()
accettazione_nuovo_gruppo = do
	p <- P.password "password di amministrazione"
	t <- sea $ return . ($p) . controlla_password
	if t then do
		r <- P.upload "carica la richiesta"
		l <- sea $ ($r) . boot_nuovo_gruppo 
		case l of 
			True -> return ()
			False -> bocciato "nome del nuovo gruppo" "nome non disponibile"

		else bocciato "password di amministrazione" "password do amministrazione non riconosciuta"
cambiaGruppo = do
	gs <- sea elenco_gruppi 
	g <- P.scelte (("<nessuno>",Nothing): zip gs (map Just gs)) "seleziona il gruppo di acquisto" 
	ses $ ($g) . writeGruppo

ensureGruppo s f = do
	g <- ses readGruppo
	case g of 
		Nothing -> bocciatoS s "manca la selezione del gruppo"
		Just _ -> f 

ensureResponsabile s f = ensureGruppo s $ do
	r <- ses readAccesso
	case r of 
		Nothing -> bocciatoS s "manca la selezione del responsabile"
		Just _ -> f

wname s f x = (s,f s x)

amministrazione :: Interfaccia ()
amministrazione = rotonda $ \k -> do
	g <- ses readGruppo
	let grs = case g of 
		Nothing -> []
		Just _ -> [
			(,) "scelta del responsabile autore delle dichiarazioni"  accesso,
			(,) "modifica della considerazione delle dichiarazioni"   eventLevelSelector,
			(,) "ricezione nuove chiavi da responsabile"  creaChiavi
			]
	r <- ses readAccesso
	let res = case r of 
		Nothing -> []
		Just _ -> [(,) "digestione di tutte le dichiarazioni pubblicate"  sincronizza]
	menu "operazioni amministrative" $ 
		[("selezione del gruppo di acquisto", cambiaGruppo)]
		++ grs ++ res ++ [
			("richiesta inserimento nuovo gruppo",richiesta_nuovo_gruppo) ,
			("accettazione richiesta nuovo gruppo", accettazione_nuovo_gruppo)
			]


indiretto =  wname  "accesso indiretto"  mano
				[
				wname "carica un aggiornamento individuale"  ensureGruppo caricaAggiornamentoIndividuale ,
				wname "carica un aggiornamento di gruppo"  ensureGruppo caricaAggiornamentoDiGruppo,
				wname "scarica gli aggiornamenti individuali"  ensureGruppo $ 
				  sepU readUPatches >>= \ (n,ups) -> P.download ("patches." ++ show n) "scarica gli aggiornamenti individuali" ups,
				wname "scarica un aggiornamento di gruppo"  ensureGruppo scaricaAggiornamentoDiGruppo,
				wname "scarica lo stato"  ensureGruppo $ sepU readStato >>= \(n,s) -> 
					P.download ("stato." ++ show n) "scaricamento dello stato" s
				]



-- applicazione :: Costruzione MEnv () ()
applicazione = rotonda $ \_ -> do
	menu "menu principale" $ 
				[
				wname "gestione dichiarazioni" ensureResponsabile $ rotonda $ \_ -> menu "gestione dichiarazioni" $ dichiarazioni,	
				("descrizione della sessione",descrizione),
				wname "effetto delle ultime dichiarazioni" ensureGruppo $ do
					c <- fromJust <$> ses readCaricamento 
					P.output False . Response $ 
						[("effetto delle ultime dichiarazioni",  c)],

				wname "interrogazione sullo stato del gruppo" ensureGruppo  interrogazioni,
				("amministrazione", amministrazione)
				]

