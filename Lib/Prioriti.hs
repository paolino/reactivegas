{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Lib.Prioriti
Description : Priority-based sorting using parseable representations
Copyright   : (c) Paolo Veronelli, 2025
License     : BSD-3-Clause

A module for reordering values based on priorities derived from
parsing their string representations.
-}
module Lib.Prioriti
    ( sortP
    , levelsP
    , R (..)
    ) where

import Control.Arrow ((&&&))
import Control.Monad (msum)
import Data.Char (ord)
import Data.List (sortBy)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (comparing)

-- | Existential wrapper for priority functions
data R = forall a. (Read a) => R (a -> Int)

-- | Sort values by computed priority, filtering by maximum level
sortP
    :: Int
    -- ^ maximum priority level to include
    -> [R]
    -- ^ list of priority extractors
    -> (a -> String)
    -- ^ function to get string representation
    -> [a]
    -- ^ values to sort
    -> [a]
    -- ^ sorted values up to max level
sortP maxLevel extractors toStr =
    map fst
        . takeWhile ((<= maxLevel) . snd)
        . sortBy (comparing snd)
        . map (id &&& computeLevel extractors)
  where
    computeLevel ps x = fromMaybe 0 $ msum $ map (extractLevel x) ps
    extractLevel x (R (priority :: b -> Int)) =
        priority <$> safeRead (toStr x)
    safeRead :: forall b. (Read b) => String -> Maybe b
    safeRead s = fst <$> listToMaybe (reads s)

-- | Compute priority levels for all values
levelsP
    :: [R]
    -- ^ list of priority extractors
    -> (a -> String)
    -- ^ function to get string representation
    -> [a]
    -- ^ values to analyze
    -> [(a, Int)]
    -- ^ values paired with their levels
levelsP extractors toStr =
    sortBy (comparing snd) . map (id &&& computeLevel extractors)
  where
    computeLevel ps x = fromMaybe 0 $ msum $ map (extractLevel x) ps
    extractLevel x (R (priority :: b -> Int)) =
        priority <$> safeRead (toStr x)
    safeRead :: forall b. (Read b) => String -> Maybe b
    safeRead s = fst <$> listToMaybe (reads s)
