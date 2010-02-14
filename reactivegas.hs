{-# LANGUAGE FlexibleContexts, ExistentialQuantification, ScopedTypeVariables, GeneralizedNewtypeDeriving, NoMonomorphismRestriction #-}


import Control.Arrow
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent.STM
import Control.Concurrent
import Debug.Trace

import Lib.Passo
import Lib.Console
import Lib.HTTP

import Core.Aggiornamento
import Core.UI


aggiornamentoConcurrent = aggiornamento

checkDir = do
	mq <- aggiornamentoConcurrent Nothing loader
	return $ case mq of
		b@(Boot _ _) -> (svolgi boot,error "nessuno stato", flip runReaderT b)
		fl -> (svolgi flow, [], flip runReaderT fl)


main :: IO ()
main = do
	t <- checkDir >>= atomically . newTVar 
	forkIO . forever $ threadDelay 1000000 >> checkDir >>= atomically . writeTVar t
	server t
