
{-# LANGUAGE FlexibleContexts, Rank2Types, ExistentialQuantification, ScopedTypeVariables, GeneralizedNewtypeDeriving, NoMonomorphismRestriction, ImplicitParams #-}
module Core.UI where

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
-- sel :: (MonadReader (Persistenza QS, Sessione)  m, MonadIO m) => ((Persistenza QS, Sessione) -> IO b) -> m b
sel f = asks f >>= liftIO 

statoPersistenza :: (Functor m, MonadReader (Persistenza QS Response, Sessione (Maybe QS) Response) m,MonadIO m) => m QS
statoPersistenza = fmap (snd . fromJust) . sel $ readStato . fst

daChiave :: Chiave -> [Responsabile] -> Maybe Responsabile
daChiave c = find (\(_,(c',_)) -> c == c') 

fromUPatches :: ([Patch],TS) -> [(Utente,[Evento])]
fromUPatches  (ups,s) =  
	let (rs,_) = responsabili s
	in catMaybes $ map (\(c,_,es) -> second (const es) <$> daChiave c rs) ups


statoSessione =  fmap fromJust . sel $ readStatoSessione . snd   

-- | comunica che c'è un errore logico nella richiesta
bocciato :: String -> Interfaccia ()
bocciato x =  P.errore . Response $ [("Incoerenza", ResponseOne x)] 

accesso :: Interfaccia ()
accesso = do 
	let k r = do
		sel $ ($r) . writeAccesso . snd
		case r of 
			Just r -> do
				mes <- sel $ ($ fst r) . readUPatch . fst
				let es = case mes of
					Nothing -> []
					Just (_,_,es) -> es
				sel $ ($es) . writeEventi . snd
			Nothing -> sel $ ($ []) . writeEventi . snd
	(rs,_) <- responsabili . fst <$> statoPersistenza 
	mano "responsabile autore delle dichiarazioni" $ ("anonimo",k Nothing):map (fst &&& k . Just) rs

onAccesso k = sel (readAccesso . snd) >>= maybe (accesso >> onAccesso k) k 

-- | la monade dove gira il programma. Mantiene in lettura lo stato del gruppo insieme alle operazioni di IO. Nello stato la lista degli eventi aspiranti un posto nella patch
type MEnv  = ReaderT (Persistenza QS Response, Sessione (Maybe QS) Response) IO 

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
	(rs,rs') <- responsabili <$> fst <$> statoSessione
	let es = us \\ (map fst $ rs ++ rs')
	if null es then P.errore $ ResponseOne "nessun utente che non sia già responsabile" else do
		u <- P.scelte  (zip es es) "nomignolo dell'utente per il quale creare le chiavi"
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
interrogazioni = mano "interrogazioni sullo stato del gruppo" $ (wrapCostrActions P.output $ [
		costrQueryAnagrafe,
		costrQueryAccredito,
		costrQueryAcquisto,
		costrQueryImpegni,
		costrQueryAssenso
		]) 
aggiornamentiIndividuali = ("esamina gli aggiornamenti individuali presenti", do
	(_,us) <- sel $ readUPatches . fst
	(s,_) <- statoPersistenza
	let ps = fromUPatches (us,s)
	if null ps then bocciato "non ci sono aggiornamenti individuali in attesa" else do 
		(u,es) <- P.scelte (map (fst &&& id) ps) "scegli aggiornamento da esaminare" 
		P.output $ Response [("aggiornamento da parte di " ++ u, ResponseMany $ map ResponseOne (sortEventi es))]
	)
eventLevelSelector = do 
	(_,us) <- sel $ readUPatches . fst
	(s,_) <- statoPersistenza
	es' <- letturaEventi
	mu <- sel $ readAccesso . snd
	let es = levelsEventi . (es' ++) . concatMap snd . maybe id (\(u,_) -> filter ((/=) u . fst)) mu $ fromUPatches (us,s)
	let rs = case es of
		[] -> Nothing  
		es -> Just $ (const "<nessuno>" *** (subtract 1)) (head es) : es ++ [("<tutti>",maxLevel)]
	case rs of 
		Nothing -> P.errore $ ResponseOne "nessuna dichiarazione presente"
		Just rs -> mano "livello di considerazione delle ultime dichiarazioni" $ map (\(x,l) ->
				(x, sel (($l). setConservative . snd))) rs


letturaEventi ::  Interfaccia [Evento]
letturaEventi = sel $ readEventi . snd

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

votazioni :: Interfaccia ()
votazioni = mano "dichiarazioni di assenso" $ wrapCostrActions addEvento [costrEventiAssenso]

sincronizza  aggiornamento aggiornamenti = onAccesso $ \(r@(u,_)) -> do  
	(_,rs) <- aggiornamenti
	case rs of 
		[] -> bocciato $ "nessun aggiornamento individale per lo stato attuale"
		xs -> do
			let k (Firmante f)  = (fst <$> statoPersistenza) >>= \s -> aggiornamento (f s xs)
			runSupporto (fst <$> statoPersistenza) bocciato k $ firmante r
			sel $ ($ Nothing) . writeAccesso . snd

salvataggio = do
	evs <- letturaEventi
	onAccesso $ \(r@(u,_)) -> do
		let 	p up = sel $ ($up) . ($u) . writeUPatch . fst
		 	k (Firmante f) = do 
				evs <- letturaEventi
				(fst <$> statoPersistenza) >>= \s -> p (f s evs) 
		runSupporto (fst <$> statoPersistenza) bocciato k $ firmante r

-- | importa gli eventuali eventi già presenti

	
getPatch :: Patch -> Interfaccia ()
getPatch p@(c,_,_) = do 
	s <- fst <$> statoPersistenza
	rs <- runErrorT . flip runReaderT s $ fromPatch (fst . responsabili) p
	case rs of 
		Left prob -> P.errore $ ResponseOne prob
		Right _ -> do 
			let Just (u,_) = daChiave c (fst $ responsabili s)
			sel $ ($p) . ($u) . writeUPatch . fst

amministrazione :: Interfaccia ()
amministrazione = do
	
	let	aggiornamenti = sel $ readUPatches .fst
		aggiornamento g = sel $ ($g). writeGPatch .fst


	mano "amministrazione" $ 
			[
			("responsabile autore delle dichiarazioni", accesso >> return ())
			,("livello di considerazione delle ultime dichiarazioni", eventLevelSelector)
			,("scarica nuove chiavi da responsabile", creaChiavi)
			,("priveè", mano "priveè" 

				[("scarica le dichiarazioni prodotte", letturaEventi >>= P.download "dichiarazioni" )
				,("digerisci tutte le dichiarazioni pubblicate", sincronizza aggiornamento aggiornamenti)
				,("carica un aggiornamento individuale", P.upload "aggiornamento individuale" >>= getPatch )
				,("scarica gli aggiornamenti individuali", 
					aggiornamenti >>= \ (n,ups) -> P.download ("patches." ++ show n) ups)
				,("carica un aggiornamento di gruppo",P.upload "aggiornamento di gruppo" >>= aggiornamento)
				,("scarica un aggiornamento di gruppo",do
					n <- P.libero "indice dell'aggiornamento richiesto"
					mg <- sel $ ($n) . readGPatch . fst
					case mg of
						Nothing -> P.errore $ ResponseOne "aggiornamento di gruppo non trovato"
						Just g -> P.download ("groups." ++ show n) g)
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
				("descrizione sessione", do
					r <- sel $ readAccesso . snd
					evs <- sel $ readEventi . snd
					evsp <- case r of
						Nothing -> return []
						Just (u,_) -> do
							mevsp <- sel $ ($ u) . readUPatch . fst
							return $ case mevsp of
								Nothing -> []
								Just (_,_,es) -> es
					l <- sel $ getConservative . snd
					v <- sel $ readVersion . fst
					P.output . Response $ 
						[("versione attuale dello stato", ResponseOne $ v)
						,("responsabile della sessione" , ResponseOne $ case r of 
							Nothing -> "anonimo"
							Just (u,_) -> u)
						,("livello di considerazione dichiarazioni",if l == maxLevel then
							ResponseOne "massimo" else ResponseOne ("modificato: " ++ show l)) 
						,("dichiarazioni in sessione" , ResponseMany $ map ResponseOne (sortEventi evs))
						,("dichiarazioni pubblicate", ResponseMany $ map ResponseOne (sortEventi evsp))
						]
					),
				("interrogazione sullo stato del gruppo", interrogazioni),
				("amministrazione",amministrazione)
				]

{-
-}
