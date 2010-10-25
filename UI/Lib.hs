



{-# LANGUAGE FlexibleContexts, Rank2Types, ExistentialQuantification, ScopedTypeVariables, GeneralizedNewtypeDeriving, NoMonomorphismRestriction, ImplicitParams #-}
module UI.Lib where

import Data.Maybe (isJust , fromJust,catMaybes)
import Data.List (delete,find,(\\), lookup)

import Control.Arrow
import Control.Applicative
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent.STM
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

import Applicazioni.Reactivegas (QS,bianco, TS, sortEventi, levelsEventi, maxLevel, Effetti)
import Applicazioni.Persistenza (Persistenza (..))
import Applicazioni.Sessione (Sessione (..))
import Applicazioni.Amministratore (Amministratore, valore_di_gruppo)

fJ y x = case x of {Nothing -> error (show y) ; Just x -> x}

daChiave :: Chiave -> [Responsabile] -> Maybe Responsabile
daChiave c = find (\(_,(c',_)) -> c == c') 

eventi :: [(Utente,Patch)] -> [(Utente,[Evento])]
eventi = map (second $ \(_,_,es) -> es) 


-- sel :: (MonadReader (Persistenza QS, Sessione)  m, MonadIO m) => ((Persistenza QS, Sessione) -> IO b) -> m b
sel f = asks f >>= liftIO 

type Name = String 

type Environment = (
	Amministratore (Persistenza QS Effetti Response),
	Sessione QS Response)
	

statoPersistenza :: (Functor m, MonadReader Environment m,MonadIO m) => m QS

statoPersistenza = snd  `fmap` sepU readStato 

statoSessione =  fmap (fJ "stato sessione") . sel $ readStatoSessione . snd   

ses f = sel $ f . snd
sepU f = sel $ \(pe,se) -> do
		g <- readGruppo se
		pe' <- maybe (return Nothing) (\g -> atomically (valore_di_gruppo pe g)) g
		f . fromJust $ pe'
sep f =  sel $ \(pe,se) -> do
		g <- readGruppo se
		pe' <- maybe (return Nothing) (\g -> atomically (valore_di_gruppo pe g)) g
		f  (pe')
sea f = sel $ f . fst 
-- | la monade dove gira il programma. Mantiene in lettura lo stato del gruppo insieme alle operazioni di IO. Nello stato la lista degli eventi aspiranti un posto nella patch
type MEnv  = ReaderT Environment IO 

type Interfaccia a = Costruzione MEnv () a

-- | comunica che c'è un errore logico nella richiesta
bocciato :: String -> String -> Interfaccia ()
bocciato s x =  P.errore True . Response $ [(s , ResponseOne x)] 

bocciatoS s x =  P.errore False . Response $ [(s , ResponseOne x)] 

accesso :: Interfaccia ()
accesso = do 
	(rs,_) <- responsabili . fst <$> statoPersistenza 
	r <- P.scelte (("<anonimo>",Nothing):map (fst &&& Just) rs) "responsabile autore delle dichiarazioni" 
	ses $ ($r) . writeAccesso

onAccesso k = ses readAccesso  >>= maybe (accesso >> onAccesso k) k


	



creaChiavi = do 
	p1 <- P.password "immetti una password, una frase , lunga almeno 12 caratteri"
	p2 <- P.password "reimmetti la password per controllare la digitazione"
	if p1 == p2 then 
		P.download  "nuove.chiavi" "scarica delle nuove chiavi" $ cryptobox p1
		else bocciato "creazione chiavi" "errore di digitazione password"
		
letturaEventi ::  Interfaccia [Evento]
letturaEventi = ses readEventi 

correzioneEventi  :: ([Evento] -> [Evento]) -> Interfaccia ()
correzioneEventi devs  = do
	evs <- letturaEventi
	ses $ ($ devs evs) . writeEventi 

addEvento x = correzioneEventi (show x:)

eventLevelSelector = do 
	
	(_,us) <- sepU readUPatches 
	es' <- letturaEventi
	mu <- ses readAccesso 
	let es = levelsEventi . (es' ++) . concatMap snd . maybe id (\(u,_) -> filter ((/=) u . fst)) mu $ eventi us
	let rs = case es of
		[] -> Nothing  
		es -> Just $ (const "<nessuno>" *** (subtract 1)) (head es) : es ++ [("<tutti>",maxLevel)]
	case rs of 
		Nothing -> bocciato "selezione livello di considerazione" "nessuna dichiarazione presente"
		Just rs -> mano "livello di considerazione delle ultime dichiarazioni" $ map (\(x,l) ->
				(x, ses (($l). setConservative ))) rs

eliminazioneEvento :: String -> Interfaccia ()
eliminazioneEvento s = do
	es <- letturaEventi
	if null es then bocciato s "non ci sono dichiarazioni da eliminare" 
		else let 
		k x = do
			es <- letturaEventi 
			correzioneEventi . const . delete x $ es
		in do 
			es <- letturaEventi
			mano  "seleziona una dichiarazione da eliminare" (zip es $ map k es)


sincronizza = onAccesso $ \(r@(u,_)) -> do  
	(_,rs) <-  second (map snd) <$> sepU readUPatches
	case rs of 
		[] -> bocciato "sincronizzazione gruppo" "nessun aggiornamento individale per lo stato attuale"
		xs -> do
			let k (Firmante f)  = (fst <$> statoPersistenza) >>= \s -> sepU $ ($ f s xs). writeGPatch
			runSupporto (fst <$> statoPersistenza) (bocciato "sincronizzazione gruppo") k $ firmante r
salvataggio s = do
	evs <- letturaEventi
	onAccesso $ \(r@(u,_)) -> do
		let 	p up = sepU $ ($up) . ($u) . writeUPatch 
		 	k (Firmante f) = do 
				evs <- letturaEventi
				(fst <$> statoPersistenza) >>= \s -> p (f s evs) 
		runSupporto (fst <$> statoPersistenza) (bocciato s) k $ firmante r

-- | importa gli eventuali eventi già presenti

	
caricaAggiornamentoIndividuale :: Interfaccia ()
caricaAggiornamentoIndividuale = do 
	p@(c,_,_) <- P.upload "aggiornamento individuale"
	s <- fst <$> statoPersistenza
	rs <- runErrorT . flip runReaderT s $ fromPatch (fst . responsabili) p
	case rs of 
		Left prob -> bocciato "caricamento aggiornamento individuale" prob
		Right _ -> do 
			let Just (u,_) = daChiave c (fst $ responsabili s)
			sepU $ ($p) . ($u) . writeUPatch

scaricaAggiornamentoIndividuale :: Interfaccia ()
scaricaAggiornamentoIndividuale = do 
	(_,us) <- sepU readUPatches
	(u,p) <- P.scelte (map (fst &&& id) us) $ "aggiornamenti utente presenti"
	v <- sepU readVersion
	P.download  (u ++ "." ++ show v) "scarica un aggiornamento individuale" p

caricaAggiornamentoDiGruppo :: Interfaccia ()
caricaAggiornamentoDiGruppo = P.upload "aggiornamento di gruppo" >>= \g -> sepU $ ($g). writeGPatch

scaricaAggiornamentoDiGruppo :: Interfaccia ()
scaricaAggiornamentoDiGruppo = do
	n <- P.libero "indice dell'aggiornamento richiesto"
	mg <- sepU $ ($n) . readGPatch
	case mg of
		Nothing -> bocciato "scaricamento aggiornamento di gruppo"  "aggiornamento di gruppo non trovato"
		Just g -> P.download  ("group." ++ show n) "scarica un aggiornamento individuale" g

effetto = do
	c <- fJ "6.UI.Server" <$> ses readCaricamento
	P.output False . Response $ [("effetto delle ultime dichiarazioni",  c)]


				
