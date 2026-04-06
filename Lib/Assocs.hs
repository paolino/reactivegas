{- |
Module      : Lib.Assocs
Description : Utility functions for association lists
Copyright   : (c) Paolo Veronelli, 2025
License     : BSD-3-Clause

Convenient functions for managing association lists (key-value pairs).
-}
module Lib.Assocs
    ( update
    , set
    , absent
    , delete
    , updateM
    , (?)
    ) where

import Data.List (lookup)
import Data.Maybe (fromMaybe, isNothing)

-- | Update the element at the given key, using a default if missing
update
    :: (Eq a)
    => a
    -- ^ key
    -> (b -> b)
    -- ^ modifier function
    -> b
    -- ^ default value if key is missing
    -> [(a, b)]
    -- ^ initial association list
    -> [(a, b)]
    -- ^ modified association list
update k dv v kvs = case lookup k kvs of
    Nothing -> (k, dv v) : kvs
    Just v' -> (k, dv v') : filter ((/=) k . fst) kvs

-- | Set the value at the given key
set
    :: (Eq a)
    => a
    -- ^ key
    -> b
    -- ^ value to set
    -> [(a, b)]
    -- ^ initial association list
    -> [(a, b)]
    -- ^ modified association list
set x y = update x (const y) undefined

-- | Check if a key is absent from the association list
absent
    :: (Eq a)
    => a
    -- ^ key to check
    -> [(a, b)]
    -- ^ association list
    -> Bool
    -- ^ 'True' if key is not present
absent x = isNothing . lookup x

-- | Delete the element at the given key
delete
    :: (Eq a)
    => a
    -- ^ key to delete
    -> [(a, b)]
    -- ^ initial association list
    -> [(a, b)]
    -- ^ modified association list
delete k kvs =
    let (xs, ys) = break ((== k) . fst) kvs
    in  xs ++ if null ys then ys else tail ys

-- | Monadic update of an element
updateM
    :: (Monad m, Eq a)
    => a
    -- ^ key
    -> (b -> m b)
    -- ^ monadic modifier function
    -> b
    -- ^ default value if key is missing
    -> [(a, b)]
    -- ^ initial association list
    -> m [(a, b)]
    -- ^ modified association list in monad
updateM k dv v kvs = case lookup k kvs of
    Nothing -> do
        v' <- dv v
        return $ (k, v') : kvs
    Just v' -> do
        v'' <- dv v'
        return $ (k, v'') : filter ((/=) k . fst) kvs

-- | Lookup with default value
(?)
    :: (Eq a)
    => [(a, b)]
    -- ^ association list to search
    -> (a, b)
    -- ^ key paired with default value
    -> b
    -- ^ found value or default
xs ? (k, t) = fromMaybe t $ lookup k xs
