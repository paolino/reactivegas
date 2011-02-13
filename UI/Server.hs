
{-# LANGUAGE FlexibleContexts, Rank2Types, ExistentialQuantification, ScopedTypeVariables, GeneralizedNewtypeDeriving, NoMonomorphismRestriction, ImplicitParams , OverlappingInstances#-}
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
import Eventi.Voci

import Applicazioni.Reactivegas (QS (..) ,bianco, TS, sortEventi, levelsEventi, maxLevel)
import Applicazioni.Persistenza (Persistenza (..))
import Applicazioni.Sessione (Sessione (..))
import Applicazioni.Amministratore (Amministratore (..))

import UI.Lib

wrapCostrActions 	
	:: (a -> Interfaccia ()) 
	-> [MEnv (SUtente,TS) -> (a -> Interfaccia ()) -> (String -> Interfaccia ()) -> [(String,Interfaccia ())]]
	-> [(String,Interfaccia ())]
wrapCostrActions g = concatMap (\f -> f q g (bocciato "costruzione di una dichiarazione")) where
	q = do 	s <- fst <$> unQS <$>statoSessione
		mu <- fmap fst  <$> ses readAccesso
		return (SUtente mu,s)

wrapCostrActions 	
	:: (a -> Interfaccia ()) 
	-> [MEnv (SUtente,TS) -> (a -> Interfaccia ()) -> (String -> Interfaccia ()) -> [(String,Interfaccia ())]]
	-> [(String,Interfaccia ())]

wrapCostrActionsC g = concatMap (\f -> f q g (bocciato "costruzione di una dichiarazione")) where
	q = do 	s <- fst <$> unQS <$> statoSessione
		return (ses id,s)


interrogazioni :: Interfaccia ()
interrogazioni = rotonda $ \_ -> menu (ResponseOne "interrogazioni sullo stato del gruppo")  $ (wrapCostrActions (P.output True) $ [
		costrQueryAnagrafe,
		costrQueryAccredito,
		costrQueryImpegni,
		costrQueryAssenso,
		costrQueryVoci
		]) 

{-
dichiarazioni = concat $ 
		[wrapCostrActions addEvento [costrEventiAccredito]
		,wrapCostrActions addEvento [costrEventiAcquisto]
		,wrapCostrActions addEvento [costrEventiImpegno]
		,wrapCostrActions addEvento $ 
			[costrEventiAnagrafe 
			,costrEventiResponsabili
			]
		,wrapCostrActions addEvento [costrEventiAssenso]

		-- ,wrapCostrActions addEventoC [costrEventiVoci]
		,	[("----------",return ())
			,("pubblica le dichiarazioni in sessione",salvataggio "pubblica le dichiarazioni in sessione")
			,("elimina delle dichiarazioni",eliminazioneEvento "elimina delle dichiarazioni"),
			(,) "modifica della considerazione delle dichiarazioni"   eventLevelSelector
			]

		]
richiesta_nuovo_gruppo :: Interfaccia ()
richiesta_nuovo_gruppo = do 
	n <- P.libero  $ ResponseOne "nome del nuovo gruppo"
	t <- sea $ ($n) . controlla_nome 
	if t then do
		(m :: String) <- P.libero  $ ResponseOne "nome del primo responsabile"
		p1 <- P.password  "una password responsabile (12 caratteri)"
		p2 <- P.password  "reimmetti la password"
		if p1 == p2 then 
			P.download (n ++ ".richiesta") "scarica file di richiesta inserimento nuovo gruppo" (n,(m, cryptobox p1))
			else bocciato "immissione password" "digitazione errata"
		else bocciato "nome del nuovo gruppo" "nome non disponibile"
accettazione_nuovo_gruppo :: Interfaccia ()
accettazione_nuovo_gruppo = do
	p <- P.password  "password di amministrazione"
	t <- sea $ return . ($p) . controlla_password
	if t then do
		r <- P.upload  "carica la richiesta"
		l <- sea $ ($r) . boot_nuovo_gruppo 
		case l of 
			True -> return ()
			False -> bocciato "nome del nuovo gruppo" "nome non disponibile"

		else bocciato "password di amministrazione" "password do amministrazione non riconosciuta"
cambiaGruppo = rotonda $ \_ -> do
	gs <- sea elenco_gruppi 
	let 	cg g = ses $ ($g) . writeGruppo
		ngs = map (second cg) $ ("<nessuno>",Nothing): zip gs (map Just gs)
	menu (ResponseOne "gruppo di acquisto")  ngs

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
			(,) "ricezione nuove chiavi da responsabile"  creaChiavi
			]
	r <- ses readAccesso
	let res = case r of 
		Nothing -> []
		Just _ -> [(,) "digestione di tutte le dichiarazioni pubblicate"  sincronizza]
	menu (ResponseOne "amministrazione")  $ grs ++ res ++ [	
		("richiesta inserimento nuovo gruppo",richiesta_nuovo_gruppo) ,
		("accettazione richiesta nuovo gruppo", accettazione_nuovo_gruppo)
		]


descrizione = do
	r <- ses readAccesso
	evs <- ses readEventi 
	evsp <- case r of
		Nothing -> return []
		Just (u,_) -> let ps (_,us) = case lookup u us of
						Nothing -> []
						Just (_,_,es) -> es 
				in sep $ maybe (return []) (fmap ps . readUPatches)

	l <- ses getConservative
	g <- ses readGruppo
	mv <- case g of 
		Nothing -> return Nothing 
		Just _ -> fmap Just $ sepU readVersion
	P.output False . Response $ 
		[("gruppo selezionato", ResponseOne $ maybe "<nessuno>" id g) 
		,("responsabile della sessione" , ResponseOne $ case r of 
			Nothing -> "<anonimo>"
			Just (u,_) -> u)
		] 
		++ case mv of 
			Nothing -> []
			Just v -> [("versione corrente dello stato", ResponseOne v)]
		++ [("livello di considerazione dichiarazioni",if l == maxLevel then
			ResponseOne "completo" else ResponseOne ("modificato: " ++ show l))]
		++ case r of 
			Nothing -> []
			Just _ -> [
				("dichiarazioni in sessione" , ResponseMany $ map ResponseOne (sortEventi evs)),
				("dichiarazioni pubblicate", ResponseMany $ map ResponseOne (sortEventi evsp))
				]
{-
indiretto =  ("accesso indiretto", mano (ResponseOne "accesso indiretto") 
				[
				wname "carica un aggiornamento individuale"  ensureGruppo caricaAggiornamentoIndividuale ,
				wname "carica un aggiornamento di gruppo"  ensureGruppo caricaAggiornamentoDiGruppo,
				wname "scarica gli aggiornamenti individuali"  ensureGruppo $ 
				  sepU readUPatches >>= \ (n,ups) -> P.download ("patches." ++ show n) "scarica gli aggiornamenti individuali" ups,
				wname "scarica un aggiornamento di gruppo"  ensureGruppo scaricaAggiornamentoDiGruppo,
				wname "scarica lo stato"  ensureGruppo $ sepU readStato >>= \(n,s) -> 
					P.download ("stato." ++ show n) "scaricamento dello stato" s
				])

-}

-- applicazione :: Costruzione MEnv () ()
applicazione = rotonda $ \_ -> do
	menu (ResponseOne "menu principale")  $ 
				[
				wname "responsabile autore"  ensureGruppo (rotonda $ const accesso),

				("gruppo di acquisto", cambiaGruppo),
				wname "gestione dichiarazioni" ensureResponsabile $ rotonda $ \_ -> menu 
					(ResponseOne "gestione dichiarazioni")  $ dichiarazioni,	
				("descrizione della sessione",descrizione),
				wname "effetto delle ultime dichiarazioni" ensureGruppo $ do
					c <- fromJust <$> ses readCaricamento 
					P.output False . Response $ 
						[("effetto delle ultime dichiarazioni",  c)],

				wname "interrogazione sullo stato del gruppo" ensureGruppo  interrogazioni,
				("amministrazione", amministrazione)
				]
-}
