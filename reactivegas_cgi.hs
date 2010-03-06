-- {-# LANGUAGE  #-}

import Control.Applicative ((<$>))
import Control.Concurrent.STM (readTChan,newTChan,atomically)
import Control.Concurrent (forkIO)
import Control.Monad (forever)


import Applicazioni.Server (singleSessionServer)

import Core.Persistenza (mkGroupSystem, startGroupSystem, readStato)
import Core.Sessione (mkSessione, readEventi , readAccesso)
import Core.UI (applicazione)
import Core.Applicazione (loader, caricamento, nuovoStato) 


main = do
	c <- atomically newTChan
	forkIO . forever $ (atomically (readTChan c) >>= putStrLn)
	pe <- mkGroupSystem loader nuovoStato c "tarogas" >>= startGroupSystem 10000000
	singleSessionServer 5000 100  applicazione ((,) pe <$> mkSessione) 
	
