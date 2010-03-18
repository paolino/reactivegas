module Core.Sessione (Sessione (..), mkSessione) where

import Control.Monad (forever)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM

import Text.XHtml
import Core.Types (Evento)
import Eventi.Anagrafe (Responsabile)
import Debug.Trace
-- | interfaccia concorrente per una sessione di interazione
data Sessione a b = Sessione 
	{readEventi :: IO [Evento]			-- ^ legge gli eventi in memoria		
	,writeEventi :: [Evento] -> IO ()		-- ^ scrive gli eventi in memoria
	,readAccesso :: IO (Maybe Responsabile)		-- ^ legge il responsabile in azione
	,writeAccesso :: Maybe Responsabile -> IO ()	-- ^ scrive il responsabile in azione
	, readCaricamento :: IO b
	-- | legge lo stato modificato dagli eventi in memoria prodotti dal responsabile in memoria
	,readStatoSessione :: IO a			
	}

-- memoria condivisa
data Board a = Board
	{ eventi :: TVar [Evento] 
	, accesso :: TVar (Maybe Responsabile)
	, stato :: TVar a
	, caricatura :: TVar String
	, triggers :: TChan (Either (Maybe Responsabile) [Evento]) -- il canale al quale inviare le modifiche
	}

type Update a b = Maybe Responsabile -> [Evento] -> STM (a,b)

update :: Update a b -> (TVar [Evento], TVar (Maybe Responsabile),  TVar a, TVar b) -> STM ()
update f (eventi,accesso,stato, caricatura) = do
	mr <- readTVar accesso
	evs <- readTVar eventi
	(s,c) <- f mr evs 
	writeTVar stato s
	writeTVar caricatura c

-- azione di modifica di uno di eventi o responsabile
triggering 	:: Update a b -- produce uno stato modificato
		-> 	(TVar [Evento]
			,TVar (Maybe Responsabile)
			,TVar a
			,TVar b
			,TChan (Either (Maybe Responsabile) [Evento])
			,TChan ()
			) 
		-> STM ()
triggering f (eventi,accesso,stato,caricatura,triggers,cs) = do
	(readTChan triggers >>= either (writeTVar accesso) (writeTVar eventi))
		`orElse` (readTChan cs >> return ())
	update f (eventi,accesso,stato,caricatura)

-- | costruisce l'interfaccia di sessione a partire da un modificatore di stato in STM
mkSessione 	:: Update a  b		-- ^ modificatore di stato
		-> TChan ()		-- ^ segnale di aggiornamento stato
		-> IO (Sessione a b)	
mkSessione f cs = do
	(s,c) <- atomically $ f Nothing [] -- uno stato iniziale, dovrebbe corrispondere allo stato non modificato
	stato <- atomically $ newTVar s
	eventi <- atomically $ newTVar []
	accesso <- atomically $ newTVar Nothing
	caricatura <- atomically $ newTVar c
	triggers <- atomically $ newTChan

	let t = atomically . before (triggering f (eventi,accesso,stato,caricatura , triggers, cs))
	return $ Sessione 
		(t $ readTVar eventi) 
		(atomically . writeTChan triggers . Right)
		(t $ readTVar accesso) 
		(atomically . writeTChan triggers . Left)
		(t $ readTVar caricatura)
		(t $ readTVar stato)

before :: STM a -> STM b -> STM b 
before a b = (a >> b) `orElse` b
