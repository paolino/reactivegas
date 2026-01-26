{-# LANGUAGE FlexibleContexts #-}

module Server.Service where

-- import Applicazioni.Aggiornamento (serverAggiornamento)
import Applicazioni.Amministratore
import Applicazioni.Persistenza (Change (GPatch), Persistence (whiteLoad, queryUser, readLogs, readState, updateSignal), mkPersistence)
import Applicazioni.Reactivegas (Effetti, QS (..), bianco, loader, maxLevel, mkDichiarazioni, nuovoStato)
import Applicazioni.Report (mkReporter)
import Applicazioni.Sessione (Sessione (backup, readGruppo), mkSessione)
import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Monad (forM, forever, liftM2)
import Data.List (lookup)
import Eventi.Anagrafe (responsabili)
import Lib.Missing ((>$>))
import Lib.Response
import Lib.SCGI (output)
import Lib.Server.Server (server)
import Lib.Tokens (Token (..))
import Server.Layout (layout, pagina)
import Server.Opzioni (Argomenti (Argomenti), parseArgs)
import System.FilePath ((</>))
import UI.Lib
import UI.Server (applicazione)

runGruppo lmov (dir, name, mr0, signal) = do
    putStrLn $ "** Inizio persistenza di \"" ++ name ++ "\""
    (pe, boot, cond) <-
        mkPersistence signal loader bianco (nuovoStato $ maybe [] return mr0) (fst . responsabili) (fst . unQS) dir
    s <- readState pe
    putStrLn $ "** Inizio report di \"" ++ name ++ "\""
    report <- mkReporter dir (dir </> "static" </> "report.html") lmov cond
    forkIO $ do
        w <- atomically (updateSignal pe) -- una copia del canale di segnalazione update della persistenza
        forever $ do
            c <- atomically w -- aspetta un segnale
            case c of
                GPatch _ _ (ls, x) -> report (ls, Just x) -- arrivata una GPatch
                _ -> return ()

    ------- logs ----------------------
    forkIO . forever $ readLogs pe >>= putStrLn . take 100

    -------- server ----------------------
    boot --------------------------------
    return pe

main = do
    Argomenti dirs port lmov lsess lrem tokpass <- parseArgs
    amm@(Administrator acs _ _ _ _ query) <- mkAdministrator tokpass (runGruppo lmov) dirs

    let newEnvironment signal ms = do
            se <-
                mkSessione
                    (mkDichiarazioni)
                    (fmap (fmap whiteLoad) . query)
                    maxLevel
                    (fmap (fmap updateSignal) . query)
                    (fmap (fmap queryUser) . query)
                    signal
                    ms
            acss <- acs
            return ((amm, se), acss, backup se)
    server
        "."
        port
        lsess
        lrem
        applicazione
        (return Nothing)
        (output . pagina)
        layout
        newEnvironment
        (\(mn, _, _, _) -> mn)
