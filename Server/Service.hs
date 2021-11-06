{-# LANGUAGE FlexibleContexts #-}

module Server.Service where

-- import Applicazioni.Aggiornamento (serverAggiornamento)
import Applicazioni.Amministratore
import Applicazioni.Persistenza (Change (GPatch), Persistenza (caricamentoBianco, queryUtente, readLogs, readStato, updateSignal), mkPersistenza)
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
import Lib.Server.Server (server)
import Lib.Tokens (Token (..))
import Network.SCGI (output)
import Server.Layout (layout, pagina)
import Server.Opzioni (Argomenti (Argomenti), parseArgs)
import System.FilePath ((</>))
import UI.Lib
import UI.Server (applicazione)

runGruppo lmov (dir, name, mr0, signal) = do
  putStrLn $ "** Inizio persistenza di \"" ++ name ++ "\""
  (pe, boot, cond) <-
    mkPersistenza signal loader bianco (nuovoStato $ maybe [] return mr0) (fst . responsabili) (fst . unQS) dir
  s <- readStato pe
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
  Argomenti dirs port lmov lsess lrem tokpass <- parseArgs $ Argomenti [] 5000 15 200 20 (Token "abc")
  amm@(Amministratore acs _ _ _ _ query) <- mkAmministratore tokpass (runGruppo lmov) dirs

  let newEnvironment signal ms = do
        se <-
          mkSessione
            (mkDichiarazioni)
            (fmap (fmap caricamentoBianco) . query)
            maxLevel
            (fmap (fmap updateSignal) . query)
            (fmap (fmap queryUtente) . query)
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
