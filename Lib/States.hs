{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Lib.States
Description : State transition utilities for versioned data
Copyright   : (c) Paolo Veronelli, 2025
License     : BSD-3-Clause

Provides existential wrappers and type class for versioned state
transitions, enabling forward and backward compatibility when
reading serialized state.
-}
module Lib.States
    ( ToPast (..)
    , FromPast (..)
    , Transition (..)
    , tryShowF
    , tryRead
    ) where

import Control.Arrow (first)

-- | Existential wrapper for converting current state to past format
data ToPast = forall a. (Show a, Transition a) => ToPast a

-- | Existential wrapper for converting past format to current state
data FromPast a = forall b. (Read b, Transition b) => FromPast (b -> a)

-- | Type class for state transitions between versions
class Transition a where
    -- | Convert to previous version format (if possible)
    back :: a -> Maybe ToPast

    -- | Convert from previous version format (if possible)
    forth :: Maybe (FromPast a)

-- | Try applying a function to a 'ToPast' value, recursively
-- checking older versions if needed
tryShowF
    :: (forall a. (Show a) => a -> Bool)
    -> ToPast
    -> Bool
tryShowF f (ToPast x) = f x || maybe False (tryShowF f) (back x)

-- | Try to read a value, attempting older formats if current fails
tryRead :: forall a. (Read a, Transition a) => ReadS a
tryRead = tryRead' (FromPast id)
  where
    tryRead'
        :: forall b. FromPast b -> ReadS b
    tryRead' (FromPast (f :: c -> b)) x =
        case map (first f) $ reads x of
            [] -> case forth :: Maybe (FromPast c) of
                Nothing -> []
                Just t -> map (first f) $ tryRead' t x
            q -> q
