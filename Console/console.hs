-- {-# LANGUAGE  #-}

-------------------------------------------------------

import Applicazioni.Aggiornamento
import Applicazioni.Persistenza (Persistence (whiteLoad, queryUser, readLogs, updateSignal), mkPersistence)
import Applicazioni.Reactivegas (bianco, loader, maxLevel, nuovoStato)
import Applicazioni.Sessione (mkSessione)
import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Monad.Reader (runReaderT)
import Debug.Trace
import Eventi.Anagrafe (responsabili)
import System.Environment
import UI.Console (interfaccia)

main = do
    args <- getArgs
    (server, dir) <- case args of
        (x : []) -> return (x, ".")
        (x : y : _) -> return (x, y)
    (pe, boot, _) <- mkPersistence "" loader bianco nuovoStato (fst . responsabili) fst dir
    forkIO . forever $ readLogs pe >>= putStrLn
    boot
    se <- mkSessione (whiteLoad pe) maxLevel (updateSignal pe) (queryUser pe) (return ()) Nothing
    (aggiorna, sincronizza) <- clientAggiornamento pe server
    aggiorna
    runReaderT interfaccia (pe, se)
    sincronizza
