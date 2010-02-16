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
import Lib.HTTP


import Core.Patch (fromGroup, Group)
import Core.Persistenza
import Core.UI

import Eventi.Anagrafe (Utente)

loader ::  QS -> Group -> Writer [String] (Either String QS)
loader (qs@(s,_)) g = runErrorT $ do
			(_,es) <- runReaderT (fromGroup g) s
			let (qs',ef) = caricamento es qs
			tell [ef]
			return qs'

main = do
	p <- wake loader Nothing 
	t <- atomically . newTVar $ (svolgi (applicazione p),flip runReaderT p)
	server t	
