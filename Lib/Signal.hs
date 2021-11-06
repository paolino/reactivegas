{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances#-}
-- | A monad isomorphic to  Writer [()], useful to signal if something has happened inside an action
module Lib.Signal  where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader

-- | the class for the monad and monadtransformer
class MonadSignal m where
	happened :: m () 		-- ^ signal that the thing happened
	intercept :: m a -> m (a,Bool)	-- ^ execute an action and report if the thing happened

newtype SignalT m a = SignalT {runSignalT :: m (a,Bool)} 
	
instance Monad m => MonadSignal (SignalT m) where
	happened = SignalT $  return  ((),True)
	intercept f = SignalT $ do
		(x,b) <- runSignalT f
		return ((x,b),b)

instance Monad m => Applicative (SignalT m) where
	pure = return 
	(<*>)  = ap


instance (Monad m) => Monad (SignalT m) where
	g >>= k = SignalT $ do
		(x,b) <- runSignalT g
		(x',b') <- runSignalT (k x)
		return $ (x',b || b')
	return x = lift (return x)

instance MonadFail m => MonadFail (SignalT m) where 
	fail x = SignalT $ fail x
	
instance MonadTrans SignalT where
	lift m = SignalT (m >>= return . flip (,) False)

instance (Functor t, Monad t) => Functor (SignalT t) where
	f `fmap` SignalT m = SignalT $ do 
		(x,b) <- m
		return (f x,b)

instance (Monad m, MonadReader r m) => MonadReader r (SignalT m) where
	ask = lift ask
	local f (SignalT m) = SignalT $ local f m

instance (Monad m, MonadWriter w m) => MonadWriter w (SignalT m) where
	tell w = lift (tell w)
	listen (SignalT m) = SignalT $ do
		((x,b),w) <- listen m 
		return ((x,w),b)
	pass (SignalT m) = SignalT . pass $ do
		((x,f),b) <- m
		return ((x,b),f)

instance (Monad m, MonadState s m) => MonadState s (SignalT m) where
	get = lift get
	put = lift . put

