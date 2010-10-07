-- {-# LANGUAGE  #-}


import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Monad.Reader (runReaderT)

import System.Environment
import Debug.Trace

-------------------------------------------------------
import Eventi.Anagrafe (responsabili)
import Applicazioni.Reactivegas (loader, bianco, nuovoStato, maxLevel) 
import Applicazioni.Persistenza (mkPersistenza , Persistenza (readLogs,caricamentoBianco,updateSignal,queryUtente))
import Applicazioni.Sessione (mkSessione)
import Applicazioni.Aggiornamento

import UI.Console (interfaccia)

	
main = do
	args <- getArgs
	(server,dir) <- case args of
		(x:[]) -> return (x,".")
		(x:y:_) -> return (x,y)
	(pe,boot) <- mkPersistenza "" loader bianco nuovoStato (fst . responsabili) fst dir 
	forkIO . forever $ readLogs pe >>= putStrLn
	boot
	se <- mkSessione (caricamentoBianco pe) maxLevel (updateSignal pe) (queryUtente pe) (return ()) Nothing 
	(aggiorna,sincronizza) <- clientAggiornamento pe server
	aggiorna 
	runReaderT interfaccia (pe,se)
	sincronizza

	
	
	
