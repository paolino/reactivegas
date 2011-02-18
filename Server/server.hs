-- {-# LANGUAGE  #-}
import Data.List (lookup)
import Control.Applicative ((<$>))
import Control.Concurrent.STM (atomically)
import Control.Concurrent (forkIO)
import Control.Monad (forever, forM, liftM2)

import Network.SCGI (output)
import System.FilePath ((</>))


import Lib.Server.Server (server)
import Lib.Response
import Lib.Missing ((>$>))
import Lib.Tokens (Token (..))
import Eventi.Anagrafe (responsabili)
import Applicazioni.Reactivegas (Effetti, QS (..) ,loader, bianco, nuovoStato, maxLevel, mkDichiarazioni) 
import Applicazioni.Persistenza (Change (GPatch), mkPersistenza , Persistenza (readStato, readLogs, caricamentoBianco,updateSignal,queryUtente))
import Applicazioni.Sessione (mkSessione, Sessione (backup,readGruppo))
import Applicazioni.Report (mkReporter)
import UI.Server (applicazione)
import UI.Lib

import Server.Opzioni (parseArgs, Argomenti (Argomenti))
import Server.Layout (layout, pagina)
-- import Applicazioni.Aggiornamento (serverAggiornamento)
import Applicazioni.Amministratore



runGruppo lmov (dir,name,mr0,signal)  = do
		putStrLn $ "** Inizio persistenza di \"" ++ name ++ "\""
		(pe,boot,cond) 
			<- mkPersistenza signal loader bianco (nuovoStato $ maybe [] return mr0) (fst . responsabili) (fst . unQS)  dir
		s <- readStato pe
		putStrLn $  "** Inizio report di \"" ++ name ++ "\"" 
		report <- mkReporter dir (dir </> "static" </> "report.html") lmov cond
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
		boot 		--------------------------------	
		

		return pe

main = do
	Argomenti dirs port lmov lsess lrem tokpass <- parseArgs $ Argomenti [] 5000 15 200 20 (Token 123) 
	amm@(Amministratore acs _ _ _ _ query) <- mkAmministratore tokpass (runGruppo lmov) "gruppi"
		
	let 
	    newEnvironment signal ms = do
		se <- mkSessione 
			(mkDichiarazioni)
			(fmap (fmap caricamentoBianco) . query ) 
			maxLevel 
			(fmap (fmap updateSignal) . query) 
			(fmap (fmap queryUtente) . query) 
			signal 
			ms
		acss <- acs	
		return ((amm,se),acss,backup se)
	server "." port lsess lrem applicazione (return Nothing) (output . pagina) layout newEnvironment
		(\(mn,_,_,_) -> mn)
			
	

