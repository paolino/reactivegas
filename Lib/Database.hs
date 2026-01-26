{- |
Module      : Lib.Database
Description : Simple in-memory database interface
Copyright   : (c) Paolo Veronelli, 2025
License     : BSD-3-Clause

Provides a simple in-memory key-value database with limited capacity.
-}
module Lib.Database
    ( DB (..)
    , restoreDB
    , limitedDB
    ) where

import Control.Arrow (second)
import Data.List (lookup)
import Data.Maybe (isJust, listToMaybe)

-- | Database interface
data DB a b = DB
    { query :: a -> Maybe b
    -- ^ lookup a value by key
    , lkey :: Maybe a
    -- ^ get the most recent key
    , select :: (a -> Bool) -> [(a, b)]
    -- ^ select entries matching predicate
    , set :: (a, b) -> DB a b
    -- ^ insert or update an entry
    , forget :: a -> DB a b
    -- ^ remove an entry by key
    , dbmap :: (b -> b) -> DB a b
    -- ^ apply function to all values
    , purge :: (a -> Bool) -> DB a b
    -- ^ remove entries matching predicate
    , dump :: [(a, b)]
    -- ^ get all entries
    , exists :: a -> Bool
    -- ^ check if key exists
    }

-- | Restore a database from a list of entries
restoreDB :: (Show a, Eq a) => Int -> [(a, b)] -> DB a b
restoreDB limit = foldr (flip set) (limitedDB limit)

-- | Create an inefficient memory-limited database
limitedDB
    :: (Show a, Eq a)
    => Int
    -- ^ maximum number of elements
    -> DB a b
limitedDB limit = mkdb []
  where
    mkdb xs =
        DB
            { query = (`lookup` xs)
            , lkey = fst <$> listToMaybe xs
            , select = \f -> filter (f . fst) xs
            , set = \(x, y) ->
                mkdb . take limit $ (x, y) : filter ((/=) x . fst) xs
            , forget = \x -> mkdb . filter ((/=) x . fst) $ xs
            , dbmap = \f -> mkdb . map (second f) $ xs
            , purge = \f -> mkdb . filter (not . f . fst) $ xs
            , dump = xs
            , exists = \x -> isJust $ lookup x xs
            }
