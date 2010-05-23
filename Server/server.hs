-- {-# LANGUAGE  #-}

import Control.Applicative ((<$>))
import Control.Concurrent.STM (atomically)
import Control.Concurrent (forkIO)
import Control.Monad (forever)

import Network.SCGI (output)
import System.FilePath ((</>))


import Lib.Server.Server (server)

import Applicazioni.Reactivegas (Effetti, QS,loader, bianco, nuovoStato, maxLevel) 
import Applicazioni.Persistenza (Change (GPatch), mkPersistenza , Persistenza (readStato, readLogs, caricamentoBianco,updateSignal,queryUtente))
import Applicazioni.Sessione (mkSessione, Sessione (backup))
import Applicazioni.Report (mkReporter)
import UI.Server (applicazione)

import Server.Opzioni (parseArgs, Argomenti (Argomenti))
import Server.Layout (layout, pagina)


main = do
	Argomenti dir port lagg lsess lrem tokpass <- parseArgs $ Argomenti "." 5000 20 10 20 "" 
	pe <- mkPersistenza tokpass loader bianco nuovoStato dir lagg
	-- report stuff -----------------------------
	v <- maybe (-1) fst <$> readStato pe
	report <- mkReporter dir (dir </> "static" </> "report.html") 10 v
	readStato pe >>= report . (,) [] . fmap snd
	forkIO $ do
		w <- updateSignal pe
		forever $ do
			c <- atomically w 
			case c of
				GPatch _ _ (ls,x) -> report (ls,Just x)
				_ -> return ()
	------- logs ----------------------
	forkIO . forever $ readLogs pe >>= putStrLn
	-------- server ----------------------
	server dir port lsess lrem applicazione (output . pagina) layout $ \signal ms -> do
		se <- mkSessione (caricamentoBianco pe) maxLevel (updateSignal pe) (queryUtente pe) signal ms
		return ((pe,se),backup se)

	

