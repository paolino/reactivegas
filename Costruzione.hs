{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving, FlexibleContexts, ScopedTypeVariables, ViewPatterns #-}

module Costruzione where

import Aspetti
import Control.Monad.Cont
import Control.Monad.Reader

data SceltaOLibero a = Scelta String [(String,a)] | Libero String

type Continuazione m r b a = a -> m (Costruzione m r b) -- una continuazione monadica
data Monad m => Costruzione m r b 
	= forall a . (Show a,Read a) => 
		Costruzione (SceltaOLibero a) (Continuazione m r b a)

newtype Monad m => Svolgimento r b m a = Svolgimento (ContT (Costruzione m r b) (ReaderT r m) a)  deriving 
	(Functor, Monad, MonadReader r, MonadCont)

instance MonadTrans (Svolgimento r b) where
	lift k = Svolgimento (lift . lift $  k) 
instance Show (Svolgimento r b m a)
instance Read (Svolgimento r b m a)

parametro :: (Monad m, Show a,Read a) => SceltaOLibero a -> Svolgimento r b m a 
parametro scelte = Svolgimento (ContT $ \k -> ask >>= \r -> return (Costruzione scelte (\a -> runReaderT (k a) r)))

svolgi :: Monad m => Svolgimento r b m b  -> r -> m (Costruzione m r b)
svolgi (Svolgimento c) r = runReaderT (runContT c undefined) r


