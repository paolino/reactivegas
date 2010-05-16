

{-# LANGUAGE FlexibleContexts, Rank2Types, ExistentialQuantification, ScopedTypeVariables, GeneralizedNewtypeDeriving, NoMonomorphismRestriction, ImplicitParams #-}
module UI.Lib where

import Data.Maybe (isJust , fromJust,catMaybes)
import Data.List (delete,find,(\\), lookup)

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


daChiave :: Chiave -> [Responsabile] -> Maybe Responsabile
daChiave c = find (\(_,(c',_)) -> c == c') 

eventi :: [(Utente,Patch)] -> [(Utente,[Evento])]
eventi = map (second $ \(_,_,es) -> es) 


-- sel :: (MonadReader (Persistenza QS, Sessione)  m, MonadIO m) => ((Persistenza QS, Sessione) -> IO b) -> m b
sel f = asks f >>= liftIO 

statoPersistenza :: (Functor m, MonadReader (Persistenza QS Response, Sessione (Maybe QS) Response) m,MonadIO m) => m QS
statoPersistenza = fmap (snd . fromJust) . sel $ readStato . fst

statoSessione =  fmap fromJust . sel $ readStatoSessione . snd   

 

-- | la monade dove gira il programma. Mantiene in lettura lo stato del gruppo insieme alle operazioni di IO. Nello stato la lista degli eventi aspiranti un posto nella patch
type MEnv  = ReaderT (Persistenza QS Response, Sessione (Maybe QS) Response) IO 

type Interfaccia a = Costruzione MEnv () a

-- | comunica che c'è un errore logico nella richiesta
bocciato :: String -> Interfaccia ()
bocciato x =  P.errore . Response $ [("Incoerenza", ResponseOne x)] 

accesso :: Interfaccia ()
accesso = do 
	let k r = sel $ ($r) . writeAccesso . snd
	(rs,_) <- responsabili . fst <$> statoPersistenza 
	mano "responsabile autore delle dichiarazioni" $ ("anonimo",k Nothing):map (fst &&& k . Just) rs

onAccesso k = sel (readAccesso . snd) >>= maybe (accesso >> onAccesso k) k

	

nuovoResponsabile :: Utente -> Interfaccia (Maybe Responsabile)
nuovoResponsabile u = do 
	p1 <- P.password "immetti una password, una frase , lunga almeno 12 caratteri"
	p2 <- P.password "reimmetti la password per controllare la digitazione"
	return $ if p1 == p2 then Just (u,cryptobox p1) else Nothing


creaChiavi :: Interfaccia ()
creaChiavi = do
	us <- utenti <$> fst <$> statoSessione
	(rs,rs') <- responsabili <$> fst <$> statoSessione
	let es = us \\ (map fst $ rs ++ rs')
	if null es then P.errore $ ResponseOne "nessun utente che non sia già responsabile" else do
		u <- P.scelte  (zip es es) "nomignolo dell'utente per il quale creare le chiavi"
		mr <- nuovoResponsabile u
		case mr of 
			Just r -> P.download (u ++ ".chiavi") r
			Nothing -> P.errore $ ResponseOne "errore di digitazione"
		

letturaEventi ::  Interfaccia [Evento]
letturaEventi = sel $ readEventi . snd

correzioneEventi  :: ([Evento] -> [Evento]) -> Interfaccia ()
correzioneEventi devs  = do
	evs <- letturaEventi
	sel $ ($ devs evs) . writeEventi . snd 

addEvento x = correzioneEventi (show x:)

eventLevelSelector = do 
	(_,us) <- sel $ readUPatches . fst
	es' <- letturaEventi
	mu <- sel $ readAccesso . snd
	let es = levelsEventi . (es' ++) . concatMap snd . maybe id (\(u,_) -> filter ((/=) u . fst)) mu $ eventi us
	let rs = case es of
		[] -> Nothing  
		es -> Just $ (const "<nessuno>" *** (subtract 1)) (head es) : es ++ [("<tutti>",maxLevel)]
	case rs of 
		Nothing -> P.errore $ ResponseOne "nessuna dichiarazione presente"
		Just rs -> mano "livello di considerazione delle ultime dichiarazioni" $ map (\(x,l) ->
				(x, sel (($l). setConservative . snd))) rs

eliminazioneEvento :: Interfaccia ()
eliminazioneEvento = do
	es <- letturaEventi
	if null es then bocciato "non ci sono dichiarazioni da eliminare" 
		else let 
		k x = letturaEventi >>= correzioneEventi . const . delete x 
		in mano  "seleziona una dichiarazione da eliminare" (zip es $ map k es)


sincronizza = onAccesso $ \(r@(u,_)) -> do  
	(_,rs) <-  second (map snd) <$> sel (readUPatches .fst)
	case rs of 
		[] -> bocciato $ "nessun aggiornamento individale per lo stato attuale"
		xs -> do
			let k (Firmante f)  = (fst <$> statoPersistenza) >>= \s -> sel $ ($ f s xs). writeGPatch .fst
			runSupporto (fst <$> statoPersistenza) bocciato k $ firmante r

salvataggio = do
	evs <- letturaEventi
	onAccesso $ \(r@(u,_)) -> do
		let 	p up = sel $ ($up) . ($u) . writeUPatch . fst
		 	k (Firmante f) = do 
				evs <- letturaEventi
				(fst <$> statoPersistenza) >>= \s -> p (f s evs) 
		runSupporto (fst <$> statoPersistenza) bocciato k $ firmante r

-- | importa gli eventuali eventi già presenti

	
caricaAggiornamentoIndividuale :: Interfaccia ()
caricaAggiornamentoIndividuale = do 
	p@(c,_,_) <- P.upload "aggiornamento individuale"
	s <- fst <$> statoPersistenza
	rs <- runErrorT . flip runReaderT s $ fromPatch (fst . responsabili) p
	case rs of 
		Left prob -> P.errore $ ResponseOne prob
		Right _ -> do 
			let Just (u,_) = daChiave c (fst $ responsabili s)
			sel $ ($p) . ($u) . writeUPatch . fst

scaricaAggiornamentoIndividuale :: Interfaccia ()
scaricaAggiornamentoIndividuale = do 
	(_,us) <- sel $ readUPatches . fst
	(u,p) <- P.scelte (map (fst &&& id) us) $ "aggiornamenti utente presenti"
	v <- sel $ readVersion . fst
	P.download (u ++ "." ++ show v) p

caricaAggiornamentoDiGruppo :: Interfaccia ()
caricaAggiornamentoDiGruppo = P.upload "aggiornamento di gruppo" >>= \g -> sel $ ($g). writeGPatch .fst

scaricaAggiornamentoDiGruppo :: Interfaccia ()
scaricaAggiornamentoDiGruppo = do
	n <- P.libero "indice dell'aggiornamento richiesto"
	mg <- sel $ ($n) . readGPatch . fst
	case mg of
		Nothing -> P.errore $ ResponseOne "aggiornamento di gruppo non trovato"
		Just g -> P.download ("group." ++ show n) g

effetto = do
	c <- sel $ readCaricamento . snd
	P.output . Response $ [("effetto delle ultime dichiarazioni",  c)]


descrizione = do
	r <- sel $ readAccesso . snd
	evs <- sel $ readEventi . snd
	evsp <- case r of
		Nothing -> return []
		Just (u,_) -> do 
			(_,us) <- sel $ readUPatches . fst
			return $ maybe [] (\(_,_,es) -> es) $ lookup u us
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
					
