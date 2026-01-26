{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : Lib.Aspetti
Description : Heterogeneous record implementation
Copyright   : (c) Paolo Veronelli, 2025
License     : BSD-3-Clause

Implementation of extensible records, freely inspired by Oleg's HList.
Contribution by Saizan.

The state is a 2-tuple nested in the second element. All first element
types are distinct, and element selection occurs through type specification.
-}
module Lib.Aspetti
    ( ParteDi (..)
    , seeset
    , (.<)
    , type (:*:)
    ) where

-- | Type class for extensible records
-- First parameter is the selected type, second is the containing structure
class ParteDi l ls where
    -- | Extract the element of type @l@
    see :: ls -> l

    -- | Replace the element of type @l@
    set :: l -> ls -> ls

-- Base case: element is the first in the tuple
instance {-# OVERLAPPING #-} ParteDi l (l, ls) where
    see (l, _) = l
    set l (_, ls) = (l, ls)

-- Recursive case: search in the rest of the tuple
instance (ParteDi l ls) => ParteDi l (l', ls) where
    see (_, ls) = see ls
    set l (l', ls) = (l', set l ls)

-- Identity case: single element
instance {-# OVERLAPPABLE #-} ParteDi l l where
    see l = l
    set l _ = l

-- | Modify the element of type @l@ using a function
seeset
    :: (ParteDi l ls)
    => (l -> l)
    -- ^ modifier function
    -> ls
    -- ^ initial structure
    -> ls
    -- ^ modified structure
seeset f x = set (f $ see x) x

infixr 8 .<

-- | Structure composer: @x .< y == (x, y)@
(.<) :: l -> ls -> (l, ls)
(.<) = (,)

infixr 8 :*:

-- | Type-level structure composer
type a :*: b = (a, b)
