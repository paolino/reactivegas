module Core.Sessione where


import Control.Concurrent.STM

import Core.Types
import Eventi.Anagrafe (Responsabile)

data Sessione = Sessione 
	{readEventi :: IO [Evento]
	,writeEventi :: [Evento] -> IO ()
	,readAccesso :: IO (Maybe Responsabile)
	,writeAccesso :: Maybe Responsabile -> IO ()
	}

mkSessione :: IO Sessione
mkSessione = do
	evs <- atomically $ newTVar []
	accesso <- atomically $ newTVar Nothing
	return $ Sessione (atomically $ readTVar evs) (atomically . writeTVar evs)
		(atomically $ readTVar accesso) (atomically . writeTVar accesso)
		
