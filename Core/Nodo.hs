{- |
Module      : Core.Nodo
Description : Reactive tree node structure
Copyright   : (c) Paolo Veronelli, 2025
License     : BSD-3-Clause

Defines the tree structure for reactive event processing.
Each node contains an optional reaction and a list of children,
where children are contextualized by the events that created them.
-}
module Core.Nodo
    ( Appuntato
    , Nodo (..)
    , mkNodi
    , pruner
    ) where

import Control.Arrow (second)
import Data.Maybe (isJust)

import Core.Programmazione (Reazione)
import Core.Types (Esterno, Interno)

-- | Serialization of an event contextualized to its input state
-- We distinguish between internal and external events.
-- Internal events cannot have an associated @d@ value.
type Appuntato s d = (Either Interno (Esterno d), s)

-- | A node contains an optional reaction (reactions can have limited lifetime)
-- and a list of children. Each child contains the contextualized event
-- that created it and a list of indexed nodes.
data Nodo s c d = Nodo
    { reattore :: Maybe (Reazione s c d)
    -- ^ optional reaction at this node
    , seguenti :: [(Appuntato s d, [(Int, Nodo s c d)])]
    -- ^ children indexed by their creating event
    }

-- | Wrap a list of reactions as nodes with empty children
mkNodi :: [Reazione s c d] -> [Nodo s c d]
mkNodi = map (flip Nodo [] . Just)

-- | Prune dead branches (nodes without reactions and without children)
pruner :: Nodo s c d -> Nodo s c d
pruner (Nodo k rs) =
    Nodo k
        . filter (not . null . snd)
        -- Remove registered events that contain no reactions
        . map
            ( second $
                filter (\(_, Nodo k' rs') -> isJust k' || not (null rs'))
                    -- Remove dead indexed nodes
                    . map (second pruner)
                -- Recursively prune inner nodes
            )
        $ rs
