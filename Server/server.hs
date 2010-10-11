-- {-# LANGUAGE  #-}
import Data.List (lookup)
import Control.Applicative ((<$>))
import Control.Concurrent.STM (atomically)
import Control.Concurrent (forkIO)
import Control.Monad (forever, forM)

import Network.SCGI (output)
import System.FilePath ((</>))


import Lib.Server.Server (server)
import Lib.Response
import Lib.Missing ((>$>))
import Eventi.Anagrafe (responsabili)
import Applicazioni.Reactivegas (Effetti, QS,loader, bianco, nuovoStato, maxLevel) 
import Applicazioni.Persistenza (Change (GPatch), mkPersistenza , Persistenza (readStato, readLogs, caricamentoBianco,updateSignal,queryUtente))
import Applicazioni.Sessione (mkSessione, Sessione (backup))
import Applicazioni.Report (mkReporter)
import UI.Server (applicazione)
import UI.Lib

import Server.Opzioni (parseArgs, Argomenti (Argomenti))
import Server.Layout (layout, pagina)
import Applicazioni.Aggiornamento (serverAggiornamento)



main = do
	Argomenti dir port lmov lsess lrem tokpass <- parseArgs $ Argomenti "." 5000 15 10 20 "" 
	putStrLn "** Inizio report"
	let dirs = [dir]
	pes <- forM dirs $ \dir -> do
		report <- mkReporter dir (dir </> "static" </> "report.html") lmov 
		putStrLn "** Inizio persistenza"
		(pe,boot) <- mkPersistenza tokpass loader bianco nuovoStato (fst . responsabili) fst dir 
		--- report thread --------------------------------
		forkIO $ do
			w <- atomically (updateSignal pe) -- una copia del canale di segnalazione update della persistenza
			forever $ do
				c <- atomically w -- aspetta un segnale
				case c of
					GPatch _ _ (ls,x) -> report (ls,Just x) -- arrivata una GPatch 
					_ -> return ()
		------- logs ----------------------
		forkIO . forever $ readLogs pe >>= putStrLn . take 100
		-------- server ----------------------
		boot
		return pe
	let pe = flip lookup $ zip dirs pes :: String -> Maybe (Persistenza QS Effetti Response)
	server "." port lsess lrem applicazione (serverAggiornamento pe) 
		(output . pagina) layout $ \signal ms -> do
			se <- mkSessione (fmap caricamentoBianco . pe ) 
				maxLevel 
				(fmap updateSignal . pe) 
				(fmap queryUtente . pe) 
				signal 
				ms
				dirs
			return ((pe,se),backup se)
	
	

