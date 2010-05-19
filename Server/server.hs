-- {-# LANGUAGE  #-}

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Monad (forever)

import Network.SCGI (output)
import System.FilePath ((</>))


import Lib.Server.Server (server)

import Applicazioni.Reactivegas (loader, caricamento, nuovoStato, maxLevel) 
import Applicazioni.Persistenza (mkPersistenza , Persistenza (writeGPatch, readStato, readLogs,caricamentoBianco,updateSignal,queryUtente))
import Applicazioni.Sessione (mkSessione, Sessione (backup))
import Applicazioni.Report (report)
import UI.Server (applicazione)

import Server.Opzioni (parseArgs, Argomenti (Argomenti))
import Server.Layout (layout, pagina)

main = do
	Argomenti dir port lagg lsess lrem tokpass <- parseArgs $ Argomenti "." 5000 20 10 20 "" 
	pe <- mkPersistenza tokpass loader caricamento nuovoStato dir lagg
	let report' pe =  readStato pe >>= report (dir </> "report.html") .fmap (fst . snd)
	report' pe
	pe <- return pe{ writeGPatch = \g -> (writeGPatch pe g >> report' pe)}
	forkIO . forever $ readLogs pe >>= putStrLn
	server dir port lsess lrem applicazione (output . pagina) layout $ \signal ms -> do
		se <- mkSessione (caricamentoBianco pe) maxLevel (updateSignal pe) (queryUtente pe) signal ms
		return ((pe,se),backup se)

	

