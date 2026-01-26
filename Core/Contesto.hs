{- |
Module      : Core.Contesto
Description : Event context tracking
Copyright   : (c) Paolo Veronelli, 2025
License     : BSD-3-Clause

Tracks the chain of events that led to the current reaction.
The chain always starts with an external event, followed by
any number of internal events.
-}
module Core.Contesto
    ( esterno
    , Contesto (..)
    , nuovoContesto
    , Contestualizzato
    , motiva
    , flatten
    ) where

import Core.Types (Esterno, Interno)

-- | Event context: tracks the chain of events we've reacted to
-- before reaching the current event
data Contesto d
    = -- | Initial state (no events yet)
      Boot
    | -- | First external event received
      Primo (Esterno d)
    | -- | External event followed by internal events
      Oltre (Esterno d) [Interno]
    deriving (Show, Eq)

-- | Extract the external event from a context (if any)
esterno :: Contesto d -> Maybe (Esterno d)
esterno Boot = Nothing
esterno (Primo e) = Just e
esterno (Oltre e _) = Just e

-- | Create a new empty context
nuovoContesto :: Contesto d
nuovoContesto = Boot

-- | Extend context with a new event
-- Protocol: external event first, then any number of internal events
motiva
    :: (Show d)
    => Either Interno (Esterno d)
    -- ^ new event (Left = internal, Right = external)
    -> Contesto d
    -- ^ current context
    -> Contesto d
    -- ^ updated context
motiva (Right x) Boot = Primo x
motiva (Left x) (Primo y) = Oltre y [x]
motiva (Left x) (Oltre y xs) = Oltre y (xs ++ [x])
motiva r q = error $ "context update failed: " ++ show (r, q)

-- | Values that only make sense together with their context
type Contestualizzato d r = (Contesto d, r)

-- | Flatten a string context to a list of labeled events
flatten :: Contesto String -> [(String, String)]
flatten Boot = []
flatten (Primo y) = [y]
flatten (Oltre y xs) = y : map ("Internal",) xs
