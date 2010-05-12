-- | gestione della sessione, del modello falso in cui l'utente si trova ad operare in bianco prima di fare persistere il proprio lavoro
module Applicazioni.Sessione (Sessione (..), mkSessione, Update) where

import Data.List (union, (\\))
import Control.Applicative ((<$>))
import Control.Monad (forever, when)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (STM,TVar,TChan,newTVar,newTChan,readTVar,readTChan,writeTVar,writeTChan,orElse,atomically)

import Core.Types (Evento,Responsabile, Utente)

import Applicazioni.Persistenza (Change (..))

import Debug.Trace
-- | interfaccia concorrente per una sessione di interazione
data Sessione a b = Sessione 
	{readEventi :: IO [Evento]			-- ^ legge gli eventi in memoria		
	,writeEventi :: [Evento] -> IO ()		-- ^ scrive gli eventi in memoria
	,readAccesso :: IO (Maybe Responsabile)		-- ^ legge il responsabile in azione
	,writeAccesso :: Maybe Responsabile -> IO ()	-- ^ scrive il responsabile in azione
	,readCaricamento :: IO b	-- ^ legge l'effetto dell'ultimo caricamento dichiarazioni
	,readStatoSessione :: IO a	-- ^ legge lo stato modificato dagli eventi in memoria prodotti dal responsabile in memoria
	,setConservative :: Int -> IO () 	-- ^ imposta il livello di caricamento
	,getConservative :: IO Int		-- ^ legge il livello di caricamento
	}

-- | eventi che scatenano la ricomputazione dello stato modificato
data Triggers = TResponsabile (Maybe Responsabile) | TEventi [Evento] | TConservative Int

-- | modello di ricomputazione che deve essere fornito
type Update a b = Int -> Maybe Responsabile -> [Evento] -> STM (a,b)

-- | azione di modifica di uno di eventi o responsabile
update 	:: Update a b -- produce uno stato modificato
	-> Int -- ^ livello standard di caricamento eventi
	-> 	(TVar [Evento]
		,TVar (Maybe Responsabile)
		,TVar Int
		,TVar a
		,TVar b
		,TChan Triggers
		,STM Change -- una condizione esterna che deve fare scattare il rinnovamento
		,(Maybe Utente -> STM [Evento]) -- gli eventi pubblicati per un utente
		) -- ^ memoria condivisa
	-> STM ()
update f l (eventi, accesso, conservative, stato, caricamento, triggers,signal, publ) = 
	let 	-- | update causato da modifica utente
		interna = do 	t 	<- readTChan triggers 
				case t of 	TResponsabile mr 	->  do
							writeTVar accesso mr
							publ (fst <$> mr) >>= writeTVar eventi
						TEventi es 		->  writeTVar eventi es
						TConservative l 	->  writeTVar conservative l
		-- | update causato da condizione esterna, butta via tutto tranne il responsabile
		esterna = do 	s <- signal
				ess <- readTVar eventi
				mr <- fmap fst <$>readTVar accesso
				case s of
					Boot -> return ()
					GPatch digested orphans ->  do	
						let 	ofs = maybe [] id $ mr >>= \u -> lookup u orphans 
						 	dgs = maybe [] id $ mr >>= \u -> lookup u digested
						writeTVar eventi $ (ess `union` ofs) \\ dgs
						writeTVar conservative l

					UPatch u esp ->  do
						when (Just u == mr) $ writeTVar eventi (esp `union` ess)
	in do 	
		interna `orElse` esterna
		mr 	<- readTVar accesso
		evs 	<- readTVar eventi
		l' 	<- readTVar conservative
		-- effettua il caricamento rispettoso dell condizioni di sessione 
		(s,c) 	<- f l' mr evs 	
		writeTVar stato s
		writeTVar caricamento c

-- | costruisce l'interfaccia di sessione a partire da un modificatore di stato in STM
mkSessione 	:: Update a  b		-- ^ modificatore di stato
		-> Int 			-- ^ livello di caricamento di base
		-> IO (STM Change)		-- ^ segnale di aggiornamento stato
		-> (Maybe Utente -> STM [Evento])  -- ^ query sugli eventi pubblicati per un utente
		-> IO (Sessione a b)	
mkSessione f l mkSignal publ = do
	(s,c) 		<- atomically $ f l Nothing [] -- uno stato iniziale
	caricamento 	<- atomically $ newTVar c
	stato 		<- atomically $ newTVar s
	eventi 		<- atomically $ newTVar []
	accesso 	<- atomically $ newTVar Nothing
	triggers 	<- atomically $ newTChan
	conservative 	<- atomically $ newTVar l
	signal 		<- mkSignal
	let	memoria = (eventi, accesso, conservative, stato, caricamento ,triggers, signal, publ)
		checkUpdate q  = (update f l memoria >> q) `orElse` q
		write f = atomically . writeTChan triggers . f
		read t = atomically . checkUpdate $ readTVar t
	return $ Sessione 
		(read eventi) 
		(write TEventi)
		(read accesso) 
		(write TResponsabile)
		(read caricamento)
		(read stato)
		(write TConservative)
		(read conservative)

