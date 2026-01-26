{- |
Module      : Lib.Modify
Description : STM-based mutable state utilities
Copyright   : (c) Paolo Veronelli, 2025
License     : BSD-3-Clause

Provides utilities for creating and managing mutable state
using STM (Software Transactional Memory).
-}
module Lib.Modify
    ( PeekPoke (..)
    , modifyT
    , mkPeekPoke
    , Modify
    , writeModify
    , readModify
    , mkModify
    , mkModifyAssocList
    ) where

import Control.Concurrent.STM
    ( atomically
    , newTVarIO
    , readTVar
    , writeTVar
    )
import Control.Monad.Trans (MonadIO, liftIO)

-- | Interface for peeking, poking, and modifying a mutable value
data PeekPoke a = PeekPoke
    { peek :: IO a
    -- ^ read current value
    , poke :: a -> IO ()
    -- ^ write new value
    , modify :: (a -> IO a) -> IO ()
    -- ^ modify value with IO action
    }

-- | Modify a PeekPoke value using a monadic function
-- The function can return 'Nothing' to skip the update
modifyT :: (MonadIO t) => PeekPoke a -> (a -> t (Maybe a)) -> t ()
modifyT pp f = do
    y <- liftIO (peek pp)
    x <- f y
    case x of
        Just x' -> liftIO . poke pp $ x'
        Nothing -> return ()

-- | Create a PeekPoke backed by a TVar with a notification action
mkPeekPoke
    :: a
    -- ^ initial value
    -> IO ()
    -- ^ notification action on change
    -> IO (PeekPoke a)
mkPeekPoke x notify = do
    tt <- newTVarIO x
    return $
        PeekPoke
            (atomically $ readTVar tt)
            (\v -> atomically (writeTVar tt v) >> notify)
            (\f -> do
                v <- atomically $ readTVar tt
                v' <- f v
                atomically (writeTVar tt v') >> notify
            )

-- | Type alias for a modification function
type Modify m a = (a -> m (Maybe a)) -> m ()

-- | Write a value using a Modify function
writeModify :: (Monad m) => Modify m a -> a -> m ()
writeModify f x = f $ \_ -> return $ Just x

-- | Read a value using a Modify function (without changing it)
readModify :: (Monad m) => Modify m a -> (a -> m ()) -> m ()
readModify f g = f $ \x -> g x >> return Nothing

-- | Create a Modify function backed by a TVar
mkModify
    :: (MonadIO m)
    => a
    -- ^ initial value
    -> m ()
    -- ^ notification action on change
    -> IO (Modify m a)
mkModify x notify = do
    tt <- newTVarIO x
    return $ \f -> do
        mt <- (liftIO . atomically $ readTVar tt) >>= f
        case mt of
            Nothing -> return ()
            Just t -> do
                liftIO . atomically $ writeTVar tt t
                notify

-- | Create a Modify function for an association list with dynamic creation
mkModifyAssocList
    :: (MonadIO m)
    => (k -> IO v)
    -- ^ create value for new key
    -> [(k, v)]
    -- ^ initial associations
    -> m ()
    -- ^ notification action on change
    -> IO (Modify m [k])
mkModifyAssocList mkValue xs notify = do
    gs <- newTVarIO xs
    return $ \f -> do
        currentList <- liftIO . atomically $ readTVar gs
        mNewKeys <- f $ map fst currentList
        case mNewKeys of
            Nothing -> return ()
            Just newKeys -> do
                let addedKeys = filter (`notElem` map fst currentList) newKeys
                newValues <- mapM mkValue addedKeys
                liftIO . atomically $
                    writeTVar gs (currentList ++ zip addedKeys newValues)
                notify
