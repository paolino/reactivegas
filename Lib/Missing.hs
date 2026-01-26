{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Lib.Missing
Description : Miscellaneous utility functions
Copyright   : (c) Paolo Veronelli, 2025
License     : BSD-3-Clause

Various utility functions that are missing from standard libraries.
-}
module Lib.Missing
    ( sortLower
    , sortByLower
    , secondM
    , firstM
    , deleteM
    , deleteMb
    , foldDeleteMb
    , onNothing
    , (>$>)
    , catchRead
    , untilNothing
    ) where

import Control.Arrow (second)
import Control.Monad.Except (ExceptT, throwError)
import Data.Char (toLower)
import Data.Function (on)
import Data.List (sortBy)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, typeRep)

-- | Sort strings case-insensitively
sortLower :: [String] -> [String]
sortLower = sortByLower id

-- | Sort by a string field, case-insensitively
sortByLower :: (a -> String) -> [a] -> [a]
sortByLower f = sortBy (compare `on` (map toLower . f))

-- | Apply a monadic function to the second element of a pair
secondM :: (Monad m) => (a -> m b) -> (c, a) -> m (c, b)
secondM f (x, y) = do
    y' <- f y
    return (x, y')

-- | Apply a monadic function to the first element of a pair
firstM :: (Monad m) => (a -> m b) -> (a, c) -> m (b, c)
firstM f (x, y) = do
    x' <- f x
    return (x', y)

-- | Delete the first element matching a monadic predicate
deleteM
    :: (Functor m, Monad m)
    => (a -> m Bool)
    -- ^ predicate function
    -> [a]
    -- ^ list to filter
    -> m (Maybe [a])
    -- ^ 'Just' remaining list if element deleted, 'Nothing' otherwise
deleteM _ [] = return Nothing
deleteM predicate (x : xs) = do
    matches <- predicate x
    if matches
        then return $ Just xs
        else fmap (x :) <$> deleteM predicate xs

-- | Delete first matching element with stateful predicate
deleteMb
    :: (Functor m, Monad m)
    => (b -> a -> m (Maybe b))
    -- ^ state update and match function
    -> b
    -- ^ initial state
    -> [a]
    -- ^ list to process
    -> m (Maybe (b, [a]))
    -- ^ 'Just' (new state, remaining list) if match found
deleteMb _ _ [] = return Nothing
deleteMb f state (x : xs) = do
    result <- f state x
    case result of
        Nothing -> fmap (second (x :)) <$> deleteMb f state xs
        Just state' -> return $ Just (state', xs)

-- | Fold over list, deleting matching elements with stateful predicate
foldDeleteMb
    :: (Functor m, Monad m)
    => (b -> a -> m (Maybe b))
    -- ^ state update and match function
    -> b
    -- ^ initial state
    -> [a]
    -- ^ list to process
    -> m (b, [a])
    -- ^ (final state, unmatched elements)
foldDeleteMb f state xs = do
    result <- deleteMb f state xs
    case result of
        Nothing -> return (state, xs)
        Just (state', xs') -> foldDeleteMb f state' xs'

-- | Throw error on 'Nothing', return value on 'Just'
onNothing :: (Monad m) => String -> Maybe a -> ExceptT String m a
onNothing errMsg = maybe (throwError errMsg) return

-- | Compose a function with fmap
infixr 8 >$>
(>$>) :: (Functor f) => (a -> b) -> (c -> f a) -> c -> f b
(>$>) = (.) . (<$>)

-- | Read with informative error message on failure
catchRead :: forall a. (Read a, Typeable a) => String -> String -> a
catchRead context token = case reads token of
    [] -> err
    xs -> case last xs of
        (x, "") -> x
        _ -> err
  where
    err =
        error $
            context
                ++ " reading "
                ++ show (take 20 token)
                ++ " as type "
                ++ show (typeRep (Proxy :: Proxy a))

-- | Repeatedly apply function until it returns 'Nothing'
untilNothing
    :: (Monad m, Functor m)
    => b
    -- ^ initial state
    -> (b -> m (Maybe a, b))
    -- ^ step function returning optional value and new state
    -> m [a]
    -- ^ collected values
untilNothing state step = do
    (maybeValue, state') <- step state
    case maybeValue of
        Just value -> (value :) <$> untilNothing state' step
        Nothing -> return []
