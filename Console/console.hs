-- {-# LANGUAGE  #-}


import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Monad.Reader (runReaderT)

import System.Environment
import Debug.Trace

-------------------------------------------------------
import Lib.Console (interazione)

import Applicazioni.Reactivegas (loader, caricamento, nuovoStato, maxLevel) 
import Applicazioni.Persistenza (mkPersistenza , Persistenza (readLogs,caricamentoBianco,updateSignal,queryUtente))
import Applicazioni.Sessione (mkSessione)

import UI.Console (applicazione)
	
main = do
	args <- getArgs
	dir <- case args of
		[] -> return "."
		(x:_) -> return x
	pe <- mkPersistenza ("",[]) loader caricamento nuovoStato dir 1
	forkIO . forever $ readLogs pe >>= putStrLn
	se <- mkSessione (caricamentoBianco pe) maxLevel (updateSignal pe) (queryUtente pe) (return ()) Nothing 
	runReaderT (interazione applicazione) (pe,se)
	
	
	
