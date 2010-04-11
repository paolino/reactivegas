
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
import Core.Applicazione (QS,caricamento, TS, sortEventi, levelsEventi)

import Eventi.Anagrafe
import Eventi.Accredito
import Eventi.Impegno
import Eventi.Acquisto
-- sel :: (MonadReader (Persistenza QS, Sessione)  m, MonadIO m) => ((Persistenza QS, Sessione) -> IO b) -> m b
sel f = asks f >>= liftIO 

statoPersistenza :: (Functor m, MonadReader (Persistenza QS, Sessione (Maybe QS) Response) m,MonadIO m) => m QS
statoPersistenza = fmap fromJust . sel $ readStato . fst

fromUPatches :: ([Patch],TS) -> [(Utente,[Evento])]
fromUPatches  (ups,s) =  
	let (rs,_) = responsabili s
	in catMaybes $ map (\(c,_,es) -> second (const es) <$> find (\(_,(c',_)) -> c == c') rs) ups


statoSessione =  fmap fromJust . sel $ readStatoSessione . snd   

-- | comunica che c'è un errore logico nella richiesta
bocciato :: String -> Interfaccia ()
bocciato x =  P.errore . Response $ [("Incoerenza", ResponseOne x)] 

accesso :: Interfaccia ()
accesso = let k r = sel $ ($r) . writeAccesso . snd in do
	(rs,_) <- responsabili . fst <$> statoSessione 
	mano "responsabile autore delle nuove dichiarazioni" $ ("anonimo",k Nothing):map (fst &&& k . Just) rs

onAccesso k = sel (readAccesso . snd) >>= maybe (accesso >> onAccesso k) k 

-- | la monade dove gira il programma. Mantiene in lettura lo stato del gruppo insieme alle operazioni di IO. Nello stato la lista degli eventi aspiranti un posto nella patch
type MEnv  = ReaderT (Persistenza QS, Sessione (Maybe QS) Response) IO 

type Interfaccia a = Costruzione MEnv () a

bootGruppo :: Interfaccia ()
bootGruppo = rotonda $ \k ->  
		mano "preparazione stato iniziale del gruppo" $ 
			[("elenco delle chiavi responsabile già inserite", do
				xs <- sel $ readBoot .fst 
				P.output $ Response 
					[("elenco chiavi responsabile già inserite",
						ResponseMany $ map (ResponseOne . fst) xs)])
			,("inserimento di un responsabile del gruppo iniziale", P.upload "chiave" >>= \x -> do
				xs <- sel $ readBoot . fst
				sel $ ($ x: delete x xs) . writeBoot . fst)
			,("creazione dello stato iniziale del gruppo", 
				sel (writeStato . fst) >> k ())
			]
	
bootChiavi :: Interfaccia ()
bootChiavi = do
	u <- P.libero "scegli il tuo nomignolo di utente e responsabile"
	p <- P.libero "immetti una password, una frase , lunga almeno 12 caratteri"
	P.download (u ++ ".chiavi") (u,cryptobox p)

creaChiavi :: Interfaccia ()
creaChiavi = do
	us <- utenti <$> fst <$> statoSessione
	u <- P.scelte  (zip us us) "nomignolo dell'utente pr il quale creare le chiavi"
	p <- P.libero $ "la password per le chiavi di " ++ u ++ ", una frase , lunga almeno 12 caratteri"
	P.download (u ++ ".chiavi") (u,cryptobox p)

wrapCostrActions 	
	:: (a -> Interfaccia ()) 
	-> [MEnv (SUtente,TS) -> (a -> Interfaccia ()) -> (String -> Interfaccia ()) -> [(String,Interfaccia ())]]
	-> [(String,Interfaccia ())]
wrapCostrActions g = concatMap (\f -> f q g bocciato) where
	q = do 	s <- fst <$> statoSessione
		mu <- fmap fst <$> sel (readAccesso . snd)
		return (SUtente mu,s)


interrogazioni :: Interfaccia ()
interrogazioni = mano "interrogazioni sulla conoscenza del gruppo" $ (wrapCostrActions P.output $ [
		costrQueryAnagrafe,
		costrQueryAccredito,
		costrQueryAcquisto,
		costrQueryImpegni,
		costrQueryAssenso
		]) 
aggiornamentiIndividuali = ("esamina gli aggiornamenti individuali presenti", do
	us <- sel $ readUPatches . fst
	(s,_) <- statoPersistenza
	let ps = fromUPatches (us,s)
	if null ps then bocciato "non ci sono aggiornamenti individuali in attesa" else do 
		(u,es) <- P.scelte (map (fst &&& id) ps) "scegli aggiornamento da esaminare" 
		P.output $ Response [("aggiornamento da parte di " ++ u, ResponseMany $ map ResponseOne (sortEventi es))]
	)
eventLevelSelector = do 
	us <- sel $ readUPatches . fst
	(s,_) <- statoPersistenza
	es' <- letturaEventi
	let es = levelsEventi . (es' ++) . concatMap snd $ fromUPatches (us,s)
	return $ case es of
		[] -> Nothing  
		es -> Just $ (const "<nessuno>" *** (subtract 1)) (head es) : es ++ [("<tutti>",100)]
	

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
anagrafica = mano "dichiarazioni anagrafiche" . wrapCostrActions addEvento $ 
		[costrEventiAnagrafe 
		,costrEventiResponsabili
		]


economia  :: Interfaccia () 
economia = mano "dichiarazioni economiche" . concat $ 
		[wrapCostrActions addEvento [costrEventiAccredito]
		,wrapCostrActions addEvento [costrEventiAcquisto]
		,wrapCostrActions addEvento [costrEventiImpegno]
		]

votazioni :: Interfaccia ()
votazioni = mano ("dichiarazioni di assenso") $ wrapCostrActions addEvento [costrEventiAssenso]

sincronizza  aggiornamento aggiornamenti = onAccesso $ \(r@(u,_)) -> do  
	rs <- aggiornamenti
	case rs of 
		[] -> bocciato $ "nessun aggiornamento individale per lo stato attuale"
		xs -> do
			let k (Firmante f)  = (fst <$> statoPersistenza) >>= \s -> aggiornamento (f s xs)
			runSupporto (fst <$> statoPersistenza) bocciato k $ firmante r

salvataggio = do
	evs <- letturaEventi
	if null evs then bocciato "non ci sono dichiarazioni da pubblicare" else onAccesso $ \(r@(u,_)) -> do
		let 	p up = sel $ ($up) . ($u) . writeUPatch . fst
		 	k (Firmante f) = do 
				evs <- letturaEventi
				(fst <$> statoPersistenza) >>= \s -> p (f s evs) >> svuotaEventi
		runSupporto (fst <$> statoPersistenza) bocciato k $ firmante r

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
		runSupporto (fst <$> statoPersistenza) bocciato k $ firmante r
	

amministrazione :: Interfaccia ()
amministrazione = do
	
	let	aggiornamenti = sel $ readUPatches .fst
		aggiornamento g = sel $ ($g). writeGPatch .fst


	mano "amministrazione" $ 
			[("applica tutte le dichiarazioni pubblicate alla conoscenza", sincronizza aggiornamento aggiornamenti)
			,("scarica nuove chiavi da responsabile", creaChiavi)
			,("porta sul retro", mano "porta sul retro" 

				[("scarica le dichiarazioni prodotte", letturaEventi >>= P.download "dichiarazioni.txt" )
				,("carica un aggiornamento individuale", P.errore $ ResponseOne "non implementato")
				,("scarica gli aggiornamenti individuali", 
					aggiornamenti >>= P.download "aggiornamenti.txt")
				,("carica un aggiornamento di gruppo",P.upload "aggiornamento di gruppo" >>= aggiornamento)
				,("scarica lo stato", sel (readStato . fst) >>= maybe (bocciato "stato non presente") (P.download "stato"))
				])
			]
applicazione :: Costruzione MEnv () ()
applicazione = rotonda $ \_ -> do 
	ms <- sel $ readStato . fst 
	case ms of 
		Nothing ->    -- un bel po rotto
			mano "il gruppo non esiste ancora" 
				[("creazione nuova identità di responsable", bootChiavi)
				,("preparazione stato iniziale di gruppo", bootGruppo)
				]
		Just s ->  
			mano ("menu principale") [
				("effetto delle nuove dichiarazioni", do
					c <- sel (readCaricamento . snd) 
					P.output . Response $ 
						[("effetto delle nuove dichiarazioni",  c)]),

				("responsabile autore delle nuove dichiarazioni", accesso >> return ()),
				("nuove dichiarazioni" , onAccesso . const $ mano "nuove dichiarazioni" $ 
					[("pubblica le dichiarazioni in sessione",salvataggio)
					,("dichiarazioni di assenso",votazioni)
					,("dichiarazioni economiche",economia)
					,("dichiarazioni anagrafiche",anagrafica)				
					,("elimina le dichiarazioni",eliminazioneEvento)
					,("modifica delle dichiarazioni gia' pubblicate", importa)
					,("regola il livello di caricamento dichiarazioni pubblicate", do 
						rs <- eventLevelSelector 
						case rs of 
							Nothing -> P.errore $ ResponseOne "nessuna dichiarazione pubblicata"
							Just rs -> do 
								r <- P.scelte rs "livello di caricamento"	
								sel (($r). setConservative . snd))
					]),
				("descrizione sessione", do
					r <- sel $ readAccesso . snd
					evs <- sel $ readEventi . snd
					P.output . Response $ 
						[("responsabile autore delle dichiarazioni in sessione" , ResponseOne $ case r of 
							Nothing -> "anonimo"
							Just (u,_) -> u)
						,("dichiarazioni in sessione (da pubblicare)" , ResponseMany $ map ResponseOne (sortEventi evs))
						]
					),
				("interrogazione della conoscenza", interrogazioni),
				("amministrazione",amministrazione)
				]

{-
-}
