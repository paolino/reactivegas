-- {-# LANGUAGE  #-}

import Control.Applicative ((<$>))
import Control.Concurrent.STM (readTChan,newTChan,atomically)
import Control.Concurrent (forkIO)
import Control.Monad (forever)


import Applicazioni.Server (singleSessionServer)

import Core.Persistenza (mkGroupSystem, startGroupSystem, readStato)
import Core.Sessione (mkSessione, readEventi , readAccesso)
import Core.UI (applicazione)
import Core.Applicazione (loader, caricamento) 

caricando (pe,se) = do 
	let 	modifica _ Nothing = return Nothing
		modifica se (Just s) = do  
			evs <- readEventi se
			mr <- readAccesso se
			case mr of 
				Nothing -> return $ Just s
				Just (u,_) -> do
					let (s',logs) =  caricamento (map ((,) u) evs) $ s
					print logs
					return . Just $ s'
	return (pe{readStato = readStato pe >>= modifica se},se) 

main = do
	c <- atomically newTChan
	forkIO . forever $ (atomically (readTChan c) >>= putStrLn)
	pe <- mkGroupSystem loader c "tarogas" >>= startGroupSystem 10000000
	singleSessionServer 5000 100  applicazione ((,) pe <$> mkSessione >>= caricando) 
	
