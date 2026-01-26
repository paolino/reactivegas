{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{- |
Module      : Core.Programmazione
Description : Reaction programming monad
Copyright   : (c) Paolo Veronelli, 2025
License     : BSD-3-Clause

Provides the core monad for programming reactions to events.
The monad tracks state changes, event context, and log messages.
-}
module Core.Programmazione
    ( -- * Message types
      Message (..)
    , Fallimento (..)
    , estrai
    , estraiFallimenti
    , lascia
      -- * Insertion monad
    , Inserzione (..)
    , logInserimento
    , runInserzione
      -- * Event and reaction types
    , EventoInterno (..)
    , Effetti
    , nessunEffetto
    , EfReazione
    , TyReazione
    , Reazione (..)
    , soloEsterna
      -- * Event routing
    , Deviatore (..)
    , Accentratore
    , provaDeviatore
    , provaAccentratore
    ) where

import Control.Monad (msum)
import Control.Monad.RWS
    ( MonadReader
    , MonadState
    , MonadWriter
    , RWS
    , ask
    , runRWS
    , tell
    )
import Data.Typeable (Typeable, cast, typeOf)

import Core.Contesto (Contesto, Contestualizzato)
import Core.Parsing (Parser, ParserConRead, parser, valore)
import Core.Types (Interno)

-- | Existential wrapper for log messages
data Message = forall a. (Show a, Typeable a) => Message a

instance Show Message where
    show (Message x) = show x

instance Eq Message where
    x == y = show x == show y

-- | Wrapper for failure messages
newtype Fallimento a = Fallimento a
    deriving (Typeable)

instance (Show a) => Show (Fallimento a) where
    show (Fallimento x) = "Failure:" ++ show x

-- | Extract messages of a specific type from contextualized messages
estrai
    :: (Typeable a)
    => [Contestualizzato d Message]
    -> ([a], [Contestualizzato d Message])
    -- ^ (extracted messages, remaining messages)
estrai [] = ([], [])
estrai (cm@(_, Message x) : xs) =
    let (ms, rs) = estrai xs
    in  case cast x of
            Just z -> (z : ms, rs)
            Nothing -> (ms, cm : rs)

-- | Extract failure messages with their context
estraiFallimenti
    :: (Typeable a)
    => [Contestualizzato d Message]
    -> ([Contestualizzato d a], [Contestualizzato d Message])
estraiFallimenti [] = ([], [])
estraiFallimenti (cm@(ctx, Message x) : xs) =
    let (ms, rs) = estraiFallimenti xs
    in  case cast x of
            Just z -> ((ctx, z) : ms, rs)
            Nothing -> (ms, cm : rs)

-- | Keep only messages of a specific type
lascia
    :: (Typeable a)
    => a
    -- ^ type witness
    -> [Contestualizzato d Message]
    -> [Contestualizzato d Message]
lascia _ [] = []
lascia u (cm@(_, Message x) : xs) =
    let rs = lascia u xs
    in  if typeOf x == typeOf u then cm : rs else rs

{- | The insertion monad

- State @s@: application-specific state containing event effects
- @c@: parser chosen by the application
- @d@: free parameter accompanying events (e.g., user identifier)
- Reader: maintains the causes leading to current state (events that
  occurred but whose effects aren't yet serializable)
- Writer: logs indexed by their causing event set
-}
newtype Inserzione s (c :: * -> *) d b = Inserzione
    (RWS (Contesto d) [Contestualizzato d Message] s b)
    deriving
        ( Applicative
        , Functor
        , Monad
        , MonadState s
        , MonadReader (Contesto d)
        , MonadWriter [Contestualizzato d Message]
        )

-- | Log a message associated with the current event context
logInserimento
    :: (MonadReader (Contesto d) m, MonadWriter [Contestualizzato d t] m)
    => t
    -> m ()
logInserimento x = ask >>= tell . return . (,x)

-- | Run an insertion computation
runInserzione
    :: Inserzione s c d b
    -> Contesto d
    -> s
    -> (b, s, [Contestualizzato d Message])
runInserzione (Inserzione f) = runRWS f

-- | Box for internal events, allows reactors to be polymorphic
-- in the events they produce
data EventoInterno = forall b. (Parser ParserConRead b) => EventoInterno b

-- | Effects of an insertion: new reactions and internal events
-- The first list adds new nodes to the reaction tree
-- The second list contains immediately produced events
type Effetti s c d = ([Reazione s c d], [EventoInterno])

-- | Empty effects (no new reactions, no internal events)
nessunEffetto :: Effetti s c d
nessunEffetto = ([], [])

-- | Result of a reaction: action returning Nothing on disinterest,
-- otherwise a boolean for reaction persistence and effects
type EfReazione s c d = Inserzione s c d (Maybe (Bool, Effetti s c d))

-- | Type of a reaction parameterized on external/internal events
-- A TyReazione is a function from either an internal or external event
-- to a possible insertion with persistence flag and child effects
type TyReazione a b d s c = Either b (d, a) -> EfReazione s c d

-- | Wrap a simple external-only action as a reaction
--
-- Uses @()@ as the internal event type since external-only reactions
-- don't process internal events.
soloEsterna
    :: (Show d, Read d, Parser c a)
    => ((d, a) -> EfReazione s c d)
    -> Reazione s c d
soloEsterna f =
    Reazione
        ( Nothing :: Maybe (Accentratore ())
        , either (const $ return Nothing) f
        )

-- | Existential box for event transformation
data Deviatore c b = forall a. (Parser c a) => Deviatore (a -> Maybe b)

-- | Type for concentrating multiple deviators on the same internal event
type Accentratore b = [Deviatore ParserConRead b]

-- | Check if a deviator accepts an event and transforms it
provaDeviatore :: forall b c. Interno -> Deviatore c b -> Maybe b
provaDeviatore x (Deviatore (f :: a -> Maybe b)) =
    (valore :: c a -> a) <$> parser x >>= f

-- | Try if any concentrator succeeds
provaAccentratore :: Interno -> [Deviatore c a] -> Maybe a
provaAccentratore x = msum . map (provaDeviatore x)

-- | A reaction encapsulates internal and external events in alternation
-- For internal events, an optional concentrator can be provided
data Reazione s (c :: * -> *) d
    = forall a b.
        (Parser c a, Parser ParserConRead b) =>
        Reazione (Maybe (Accentratore b), TyReazione a b d s c)
