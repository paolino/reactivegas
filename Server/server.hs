-- {-# LANGUAGE  #-}

import Control.Applicative ((<$>))
import Control.Concurrent.STM (atomically)
import Control.Concurrent (forkIO)
import Control.Monad (forever)

import Network.SCGI (output)
import System.FilePath ((</>))


import Lib.Server.Server (server)

import Eventi.Anagrafe (responsabili)
import Applicazioni.Reactivegas (Effetti, QS,loader, bianco, nuovoStato, maxLevel) 
import Applicazioni.Persistenza (Change (GPatch), mkPersistenza , Persistenza (readStato, readLogs, caricamentoBianco,updateSignal,queryUtente))
import Applicazioni.Sessione (mkSessione, Sessione (backup))
import Applicazioni.Report (mkReporter)
import UI.Server (applicazione)

import Server.Opzioni (parseArgs, Argomenti (Argomenti))
import Server.Layout (layout, pagina)
import Applicazioni.Aggiornamento (serverAggiornamento)



main = do
	Argomenti dir port lmov lsess lrem tokpass <- parseArgs $ Argomenti "." 5000 15 10 20 "" 
	putStrLn "** Inizio report"
	report <- mkReporter dir (dir </> "static" </> "report.html") lmov 
	putStrLn "** Inizio persistenza"
	(pe,boot) <- mkPersistenza tokpass loader bianco nuovoStato (fst . responsabili) fst dir 
	--- report thread --------------------------------
	forkIO $ do
		w <- updateSignal pe -- una copia del canale di segnalazione update della persistenza
		forever $ do
			c <- atomically w -- aspetta un segnale
			case c of
				GPatch _ _ (ls,x) -> report (ls,Just x) -- arrivata una GPatch 
				_ -> return ()
	------- logs ----------------------
	forkIO . forever $ readLogs pe >>= putStrLn
	-------- server ----------------------
	boot
	server dir port lsess lrem applicazione (serverAggiornamento pe) 
		(output . pagina) layout $ \signal ms -> do
			se <- mkSessione (caricamentoBianco pe) maxLevel (updateSignal pe) (queryUtente pe) signal ms
			return ((pe,se),backup se)
	
	

