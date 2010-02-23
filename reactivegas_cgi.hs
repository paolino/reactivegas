-- {-# LANGUAGE  #-}


import Control.Concurrent.STM (readTChan,newTChan,atomically)
import Control.Concurrent (forkIO)
import Control.Monad (forever)


import Lib.ServerHTTP (server)

import Core.Persistenza (mkGroupSystem, startGroupSystem)
import Core.Sessione (mkSessione)
import Core.UI (applicazione)
import Core.Applicazione (loader) 

main = do
	c <- atomically newTChan
	forkIO . forever $ (atomically (readTChan c) >>= putStrLn)
	pe <- mkGroupSystem loader c "tarogas" >>= startGroupSystem 10000000
	server mkSessione pe applicazione	
	
