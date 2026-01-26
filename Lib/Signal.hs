{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Lib.Signal
Description : Signal monad for tracking if something happened
Copyright   : (c) Paolo Veronelli, 2025
License     : BSD-3-Clause

A monad isomorphic to @Writer [()]@, useful to signal if something
has happened inside an action. This is used to track whether any
reaction occurred during event processing.
-}
module Lib.Signal
    ( MonadSignal (..)
    , SignalT (..)
    ) where

import Control.Monad (ap)
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Writer (MonadWriter (..))

-- | Type class for signal monads
class MonadSignal m where
    -- | Signal that the thing happened
    happened :: m ()

    -- | Execute an action and report if the thing happened
    intercept :: m a -> m (a, Bool)

-- | Signal monad transformer
newtype SignalT m a = SignalT
    { runSignalT :: m (a, Bool)
    -- ^ Run the signal computation, returning value and whether signal fired
    }

instance (Monad m) => MonadSignal (SignalT m) where
    happened = SignalT $ return ((), True)
    intercept f = SignalT $ do
        (x, b) <- runSignalT f
        return ((x, b), b)

instance (Monad m) => Applicative (SignalT m) where
    pure = return
    (<*>) = ap

instance (Monad m) => Monad (SignalT m) where
    g >>= k = SignalT $ do
        (x, b) <- runSignalT g
        (x', b') <- runSignalT (k x)
        return (x', b || b')
    return x = lift (return x)

instance (MonadFail m) => MonadFail (SignalT m) where
    fail x = SignalT $ fail x

instance MonadTrans SignalT where
    lift m = SignalT $ (,False) <$> m

instance (Functor t, Monad t) => Functor (SignalT t) where
    fmap f (SignalT m) = SignalT $ do
        (x, b) <- m
        return (f x, b)

instance (Monad m, MonadReader r m) => MonadReader r (SignalT m) where
    ask = lift ask
    local f (SignalT m) = SignalT $ local f m

instance (Monad m, MonadWriter w m) => MonadWriter w (SignalT m) where
    tell w = lift (tell w)
    listen (SignalT m) = SignalT $ do
        ((x, b), w) <- listen m
        return ((x, w), b)
    pass (SignalT m) = SignalT . pass $ do
        ((x, f), b) <- m
        return ((x, b), f)

instance (Monad m, MonadState s m) => MonadState s (SignalT m) where
    get = lift get
    put = lift . put
