module Core.Sessione where


import Control.Concurrent.STM

import Core.Types

data Sessione = Sessione 
	{readEventi :: IO [Evento]
	,writeEventi :: [Evento] -> IO ()
	}

mkSessione :: IO Sessione
mkSessione = do
	evs <- atomically $ newTVar []
	return $ Sessione (atomically $ readTVar evs) (atomically . writeTVar evs)
