module Core.Sessione (Sessione (..), mkSessione) where

import Control.Monad (forever)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM

import Core.Types (Evento)
import Eventi.Anagrafe (Responsabile)
import Debug.Trace
-- | interfaccia concorrente per una sessione di interazione
data Sessione a = Sessione 
	{readEventi :: IO [Evento]			-- ^ legge gli eventi in memoria		
	,writeEventi :: [Evento] -> IO ()		-- ^ scrive gli eventi in memoria
	,readAccesso :: IO (Maybe Responsabile)		-- ^ legge il responsabile in azione
	,writeAccesso :: Maybe Responsabile -> IO ()	-- ^ scrive il responsabile in azione
	-- | legge lo stato modificato dagli eventi in memoria prodotti dal responsabile in memoria
	,readStatoSessione :: IO a			
	}

-- memoria condivisa
data Board a = Board
	{ eventi :: TVar [Evento] 
	, accesso :: TVar (Maybe Responsabile)
	, stato :: TVar a
	, triggers :: TChan (Either (Maybe Responsabile) [Evento]) -- il canale al quale inviare le modifiche
	}


update f (eventi,accesso,stato) = do
	mr <- readTVar accesso
	evs <- readTVar eventi
	trace "update" $ f mr evs >>= writeTVar stato 

-- azione di modifica di uno degli eventi o responsabile
triggering 	:: (Maybe Responsabile -> [Evento] -> STM a)  -- produce uno stato modificato
		-> 	(TVar [Evento]
			,TVar (Maybe Responsabile)
			,TVar a
			,TChan (Either (Maybe Responsabile) [Evento])
			,TChan ()
			) 
		-> STM ()
triggering f (eventi,accesso,stato,triggers,cs) = do
	(readTChan triggers >>= either (writeTVar accesso) (writeTVar eventi))
		`orElse` (readTChan cs >> trace "ops" (return ()))
	update f (eventi,accesso,stato)

-- | costruisce l'interfaccia di sessione a partire da un modificatore di stato in STM
mkSessione 	:: (Maybe Responsabile -> [Evento] -> STM a)  -- ^ modificatore di stato
		-> TChan ()					-- ^ segnale di aggiornamento stato
		-> IO (Sessione a)	
mkSessione f cs = do
	print "wwooha"
	s <- atomically $ f Nothing [] -- uno stato iniziale, dovrebbe corrispondere allo stato non modificato
	stato <- atomically $ newTVar s
	eventi <- atomically $ newTVar []
	accesso <- atomically $ newTVar Nothing
	triggers <- atomically $ newTChan

	let t = atomically . before (triggering f (eventi,accesso,stato,triggers,cs))
	return $ Sessione 
		(t $ readTVar eventi) 
		(atomically . writeTChan triggers . Right)
		(t $ readTVar accesso) 
		(atomically . writeTChan triggers . Left)
		(t $ readTVar stato)

before :: STM a -> STM b -> STM b 
before a b = (a >> b) `orElse` b
