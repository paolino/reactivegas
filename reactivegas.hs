{-# LANGUAGE FlexibleContexts, ExistentialQuantification, ScopedTypeVariables, GeneralizedNewtypeDeriving, NoMonomorphismRestriction #-}


import Control.Monad.Error (runErrorT)
import Control.Monad.Writer (Writer, tell)
import Control.Monad.Cont
import Control.Arrow
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent.STM
import Control.Concurrent
import Debug.Trace

import Lib.Passo
-- import Lib.Console
import Lib.ServerHTTP (server)
import Lib.TreeLogs
import Lib.Prioriti

import Core.Patch (fromGroup, Group)
import Core.Persistenza
import Core.Sessione
import Core.UI

import Eventi.Anagrafe (Utente)

import Core.Types (Esterno, Evento)
import Core.Controllo (caricaEventi, SNodo (..))
import Core.Contesto (flatten)
import Core.Programmazione (Reazione)
import Core.Parsing (ParserConRead)
import Core.Applicazione (loader) 

import Eventi.Anagrafe
import Eventi.Accredito
import Eventi.Impegno
import Eventi.Ordine



main = do
	se <- mkSessione
	c <- atomically newTChan
	forkIO . forever $ (atomically (readTChan c) >>= putStrLn)
	pe <- mkGroupSystem loader c "tarogas" >>= startGroupSystem 10000000

	interazione <- runReaderT (svolgi applicazione) (pe,se)
	server (flip runReaderT (pe,se)) interazione 	
	
