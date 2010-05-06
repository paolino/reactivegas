-- | gestione della sessione, del modello falso in cui l'utente si trova ad operare in bianco prima di fare persistere il proprio lavoro
module Applicazioni.Sessione (Sessione (..), mkSessione, Update) where

import Control.Monad (forever)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (STM,TVar,TChan,newTVar,newTChan,readTVar,readTChan,writeTVar,writeTChan,orElse,atomically)

import Core.Types (Evento,Responsabile)

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
		,TChan () -- una condizione esterna che deve fare scattare il rinnovamento
		) -- ^ memoria condivisa
	-> STM ()
update f l (eventi, accesso, conservative, stato, caricamento, triggers,signal) = 
	let 	-- | update causato da modifica utente
		interna = do 	t 	<- readTChan triggers 
				case t of 	TResponsabile mr 	-> writeTVar accesso mr
						TEventi es 		-> writeTVar eventi es
						TConservative l 	-> writeTVar conservative l
		-- | update causato da condizione esterna, butta via tutto tranne il responsabile
		esterna = do 	readTChan signal
				writeTVar eventi []
				writeTVar conservative l
					
	in do 	interna  `orElse` esterna
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
		-> TChan ()		-- ^ segnale di aggiornamento stato
		-> IO (Sessione a b)	
mkSessione f l signal = do
	(s,c) 		<- atomically $ f l Nothing [] -- uno stato iniziale
	caricamento 	<- atomically $ newTVar c
	stato 		<- atomically $ newTVar s
	eventi 		<- atomically $ newTVar []
	accesso 	<- atomically $ newTVar Nothing
	triggers 	<- atomically $ newTChan
	conservative 	<- atomically $ newTVar l
	let	memoria = (eventi, accesso, conservative, stato, caricamento ,triggers, signal)
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

