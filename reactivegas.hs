-- {-# LANGUAGE  #-}


import Control.Applicative ((<$>))
import Control.Concurrent.STM (readTChan,newTChan,atomically)
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Monad.Reader (runReaderT)

import Debug.Trace
-------------------------------------------------------
import Lib.Console (interazione)

import Core.Types (Esterno,Utente)

import Applicazioni.Reactivegas (QS,loader, caricamento, nuovoStato, maxLevel) 
import Applicazioni.Server (sessionServer)
import Applicazioni.Persistenza (mkPersistenza , Persistenza (readLogs,caricamentoBianco,updateSignal,queryUtente))
import Applicazioni.Sessione (mkSessione)

import UI.Console (applicazione)
	
main = do
	pe <- mkPersistenza loader caricamento nuovoStato "tarogas" 20
	forkIO . forever $ readLogs pe >>= putStrLn
	se <-mkSessione (caricamentoBianco pe) maxLevel (updateSignal pe) (queryUtente pe)
	runReaderT (interazione applicazione) (pe,se)
	
	
	
