-- {-# LANGUAGE  #-}

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Monad (forever)

import Network.SCGI (output)

import Lib.Server.Server (server)

import Applicazioni.Reactivegas (loader, caricamento, nuovoStato, maxLevel) 
import Applicazioni.Persistenza (mkPersistenza , Persistenza (readLogs,caricamentoBianco,updateSignal,queryUtente))
import Applicazioni.Sessione (mkSessione)

import UI.Server (applicazione)

import Server.Opzioni (parseArgs, Argomenti (Argomenti))
import Server.Layout (layout, pagina)

main = do
	Argomenti dir port lagg lsess lrem <- parseArgs $ Argomenti "." 5000 20 10 20
	pe <- mkPersistenza loader caricamento nuovoStato dir lagg
	forkIO . forever $ readLogs pe >>= putStrLn
	server port lsess lrem applicazione (output . pagina) layout $
		(,) pe <$> mkSessione (caricamentoBianco pe) maxLevel (updateSignal pe) (queryUtente pe)

	

