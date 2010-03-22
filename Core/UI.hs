
{-# LANGUAGE FlexibleContexts, Rank2Types, ExistentialQuantification, ScopedTypeVariables, GeneralizedNewtypeDeriving, NoMonomorphismRestriction, ImplicitParams #-}
module Core.UI where

import Data.Maybe (isJust , fromJust,catMaybes)
import Data.List (delete,find)

import Control.Arrow
import Control.Applicative
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import System.Console.Haskeline (MonadException)
import Debug.Trace


import Lib.Passo (Costruzione,mano, menu, rotonda ,rmenu, Passo) 
import qualified Lib.Passo as P

import Lib.TreeLogs (eccoILogs)
import Lib.Firmabile (cryptobox)
import Lib.Prioriti (R)
import Lib.Response (Response (..))


import Core.Types (Esterno, Evento)
import Core.Controllo (caricaEventi, SNodo (..))
import Core.Contesto (flatten)
import Core.Programmazione (Reazione)
import Core.Parsing (ParserConRead)
import Core.Patch ( Firmante (..),firmante, Patch)
import Core.Costruzione (runSupporto, Supporto)
import Core.Persistenza (Persistenza (..))
import Core.Sessione (Sessione (..))
import Core.Applicazione (QS,caricamento, TS, sortEventi)

import Eventi.Anagrafe
import Eventi.Accredito
import Eventi.Impegno
import Eventi.Ordine
-- sel :: (MonadReader (Persistenza QS, Sessione)  m, MonadIO m) => ((Persistenza QS, Sessione) -> IO b) -> m b
sel f = asks f >>= liftIO 

letturaStato' :: (Functor m, MonadReader (Persistenza QS, Sessione (Maybe QS) Response) m,MonadIO m) => m QS
letturaStato' = fmap fromJust . sel $ readStato . fst

fromUPatches :: ([Patch],TS) -> [(Utente,[Evento])]
fromUPatches  (ups,s) =  
	let (rs,_) = responsabili s
	in catMaybes $ map (\(c,_,es) -> second (const es) <$> find (\(_,(c',_)) -> c == c') rs) ups


letturaStato = letturaStato' -- fmap fromJust . sel $ readStatoSessione . snd  
-- | semplifica il running dei Supporto

conStato :: (String -> Interfaccia a) -> (b -> Interfaccia a) -> Supporto MEnv TS () b ->  Interfaccia a
conStato x y z = runSupporto (fst <$> letturaStato) x y z

-- | comunica che c'è un errore logico nella richiesta
bocciato :: String -> Interfaccia ()
bocciato x =  P.errore . Response $ [("Incoerenza", ResponseOne x)] 

accesso :: Interfaccia ()
accesso = let k r = sel $ ($r) . writeAccesso . snd in do
	(rs,_) <- responsabili . fst <$> letturaStato 
	mano "responsabile autore" $ ("anonimo",k Nothing):map (fst &&& k . Just) rs

onAccesso k = sel (readAccesso . snd) >>= maybe (accesso >> onAccesso k) k 

-- | la monade dove gira il programma. Mantiene in lettura lo stato del gruppo insieme alle operazioni di IO. Nello stato la lista degli eventi aspiranti un posto nella patch
type MEnv  = ReaderT (Persistenza QS, Sessione (Maybe QS) Response) IO 

type Interfaccia a = Costruzione MEnv () a

bootGruppo :: Interfaccia ()
bootGruppo = rotonda $ \k ->  
		mano "preparazione stato iniziale" $ 
			[("elenco chiavi responsabile già inserite", do
				xs <- sel $ readBoot .fst 
				P.output $ Response 
					[("elenco chiavi responsabile già inserite",
						ResponseMany $ map (ResponseOne . fst) xs)])
			,("inserimento di una chiave responsabile", P.upload "chiave" >>= \x -> do
				xs <- sel $ readBoot . fst
				sel $ ($ x: delete x xs) . writeBoot . fst)
			,("creazione dello stato iniziale dalle chiavi inserite", 
				sel (writeStato . fst) >> k ())
			]
	
bootChiavi :: Interfaccia ()
bootChiavi = do
	u <- P.libero "scegli il tuo nome di utente"
	p <- P.libero "immetti una password, una frase , lunga almeno 12 caratteri"
	P.download (u ++ ".chiavi") (u,cryptobox p)

wrapCostrActions 	
	:: (a -> Interfaccia ()) 
	-> [MEnv TS -> (a -> Interfaccia ()) -> (String -> Interfaccia ()) -> [(String,Interfaccia ())]]
	-> [(String,Interfaccia ())]
wrapCostrActions g = concatMap (\f -> f (fst <$> letturaStato) g bocciato)

interrogazioni :: Interfaccia ()
interrogazioni = mano "interrogazione dello stato del gruppo" $ (wrapCostrActions P.output $ [
		costrQueryAccredito,
		costrQueryAnagrafe,
		costrQueryOrdine,
		costrQueryAssenso,
		costrQueryImpegni
		]) 
aggiornamentiIndividuali = ("interroga aggiornamenti in attesa", do
	us <- sel $ readUPatches . fst
	(s,_) <- letturaStato
	let ps = fromUPatches (us,s)
	if null ps then bocciato "non ci sono aggiornamenti individuali in attesa" else do 
		(u,es) <- P.scelte (map (fst &&& id) ps) "scegli aggiornamento da visionare" 
		P.output $ Response [("aggiornamento da parte di " ++ u, ResponseMany $ map ResponseOne (sortEventi es))]
	)
letturaEventi ::  Interfaccia [Evento]
letturaEventi = sel $ readEventi . snd

svuotaEventi :: Interfaccia ()
svuotaEventi = sel $ ($[]). writeEventi . snd



correzioneEventi  :: ([Evento] -> [Evento]) -> Interfaccia ()
correzioneEventi devs  = do
	evs <- letturaEventi
	sel $ ($ devs evs) . writeEventi . snd 


eliminazioneEvento :: Interfaccia ()
eliminazioneEvento = do
	es <- letturaEventi
	if null es then bocciato "non ci sono dichiarazioni da eliminare" 
		else let 
		k x = letturaEventi >>= correzioneEventi . const . delete x 
		in mano  "seleziona una dichiarazione da eliminare" (zip es $ map k es)




addEvento x = correzioneEventi (show x:)

anagrafica :: Interfaccia ()
anagrafica = mano "dichiarazioni anagrafiche" . wrapCostrActions addEvento $ [
		costrEventiResponsabili,
		costrEventiAnagrafe 
		]


economia  :: Interfaccia () 
economia = mano "dichiarazioni economiche" . concat $ 
		[wrapCostrActions addEvento [costrEventiAccredito]
		,wrapCostrActions addEvento [costrEventiImpegno]
		,wrapCostrActions addEvento [costrEventiOrdine]
		]

votazioni :: Interfaccia ()
votazioni = onAccesso $ \(u,_) -> do
	
	n <- conStato (const $ return 0) (return . length) (assensiFiltrati u) 
	mano ("dichiarazioni democratiche (" ++ show n ++ " questioni aperte)") . wrapCostrActions addEvento $ [
		costrEventiAssenso u
		]

sincronizza  aggiornamento aggiornamenti = onAccesso $ \(r@(u,_)) -> do  
	rs <- aggiornamenti
	case rs of 
		[] -> bocciato $ "nessun aggiornamento individale per lo stato attuale"
		xs -> do
			let k (Firmante f)  = (fst <$> letturaStato') >>= \s -> aggiornamento (f s xs)
			runSupporto (fst <$> letturaStato') bocciato k $ firmante r

salvataggio = do
	evs <- letturaEventi
	if null evs then bocciato "non ci sono dichiarazioni da firmare" else onAccesso $ \(r@(u,_)) -> do
		let 	p up = sel $ ($up) . ($u) . writeUPatch . fst
		 	k (Firmante f) = do 
				evs <- letturaEventi
				(fst <$> letturaStato') >>= \s -> p (f s evs) >> svuotaEventi
		runSupporto (fst <$> letturaStato') bocciato k $ firmante r

-- | importa gli eventuali eventi già presenti

importa :: Interfaccia ()
importa = onAccesso $ \r@(u,(c,_)) -> do 
	ps <- filter (\(c',_,_) -> c == c') <$>  sel (readUPatches . fst)
	if null ps then	bocciato $ "non ci sono aggiornamenti individuali in attesa per il responsabile " ++ u

		else do 
		(_,f,es) <- case ps of
			[r] -> return r
			_ -> P.scelte (zip (map show [1..]) ps) $ "aggiornamenti di " ++  u ++ " in attesa"
		let k _ = do
			correzioneEventi (es ++) 
			sel (($f) . deleteUPatch . fst)	
		runSupporto (fst <$> letturaStato') bocciato k $ firmante r
	

amministrazione :: Interfaccia ()
amministrazione = do
	
	let	aggiornamenti = sel $ readUPatches .fst
		aggiornamento g = sel $ ($g). writeGPatch .fst


	mano "amministrazione" $ [
			("firma un aggiornamento individuale",salvataggio),
			("modifica un aggiornamento individuale", importa),
			("firma un aggiornamento di gruppo", sincronizza aggiornamento aggiornamenti),
			("creazione nuove chiavi di responsabile", bootChiavi),
			aggiornamentiIndividuali,
			("accesso remoto", mano "accesso remoto" 

				[("scarica le dichiarazioni prodotte", letturaEventi >>= P.download "dichiarazioni.txt" )
				,("carica aggiornamento individuale", P.errore $ ResponseOne "non implementato")
				,("scarica gli aggiornamenti individuali", 
					aggiornamenti >>= P.download "aggiornamenti.txt")
				,("carica aggiornamento di gruppo",P.upload "aggiornamento di gruppo" >>= aggiornamento)
				,("scarica stato", sel (readStato . fst) >>= maybe (bocciato "stato non presente") (P.download "stato"))
				])
			]
applicazione :: Costruzione MEnv () ()
applicazione = rotonda $ \_ -> do 
	ms <- sel $ readStato . fst 
	case ms of 
		Nothing ->    
			mano "il gruppo non esiste ancora" 
				[("creazione nuove chiavi di responsable", bootChiavi)
				,("preparazione stato iniziale di gruppo", bootGruppo)
				]
		Just s ->  
			mano ("menu principale") [
				("responsabile autore", accesso >> return ()),
				("produzione dichiarazioni" , mano "produzione dichiarazioni" $ 
					[("dichiarazioni democratiche",votazioni)
					,("dichiarazioni economiche",economia)
					,("dichiarazioni anagrafiche",anagrafica)				
					,("effetto dell'inserimento delle dichiarazioni", do
						c <- sel (readCaricamento . snd) 
						P.output . Response $ [("effetto dell'inserimento delle dichiarazioni",  c)])
					,("correzione delle dichiarazioni",eliminazioneEvento)
					]),
				("descrizione sessione", do
					r <- sel $ readAccesso . snd
					evs <- sel $ readEventi . snd
					P.output . Response $ 
						[("responsabile autore" , ResponseOne $ case r of 
							Nothing -> "anonimo"
							Just (u,_) -> u)
						,("dichiarazioni prodotte" , ResponseMany $ map ResponseOne (sortEventi evs))
						]
					),
				("interrogazione", interrogazioni),
				("amministrazione",amministrazione)
				]

{-
-}
