{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,GeneralizedNewtypeDeriving #-}
module CState where

import Control.Concurrent.STM
import Control.Monad.State
import Control.Monad.Reader

newtype CState s a = CState (ReaderT (TVar s) IO a) deriving (Monad,Functor,MonadReader (TVar s),MonadIO)

instance MonadState s (CState s) where
	get = CState $ ask >>= \r -> liftIO (atomically $ readTVar r)
	put x = CState $ ask >>= \r -> liftIO (atomically $ writeTVar r x)

runCState :: TVar s -> CState s a -> IO a
runCState ts (CState c) = runReaderT c ts
