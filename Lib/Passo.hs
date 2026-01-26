{-# LANGUAGE ExistentialQuantification #-}

{- |
Module      : Lib.Passo
Description : Controlled value construction monad
Copyright   : (c) Paolo Veronelli, 2025
License     : BSD-3-Clause

A module for controlled construction of values through interactive steps.

The possible steps for value creation are limited to the 'Passo' datatype.
Using 'Cont' monad with 'Passo' enables a sequential language for value
construction.

Example usage:

@
interaction :: Costruzione IO Int Int
interaction = do
    x <- libero "first number:"
    y <- libero "second number:"
    op <- scelte [("sum", (+)), ("difference", (-))] "operation:"
    return $ op x y
@

The 'svolgi' function converts from the monadic language to the 'Passo'
structure by running the monad. The resulting 'Passo' values are then
processed by drivers appropriate for the interaction medium.
-}
module Lib.Passo
    ( -- * Core types
      Passo (..)
    , HPasso
    , Costruzione
      -- * Running constructions
    , svolgi
      -- * Step constructors
    , libero
    , password
    , output
    , errore
    , upload
    , download
    , scelte
      -- * Menu utilities
    , menu
    , rotonda
    , mano
    , rmenu
    ) where

import Control.Arrow (first)
import Control.Monad (forever, join)
import Control.Monad.Cont (ContT (..), callCC, runContT)
import Control.Monad.State (StateT (..), get, put, runStateT)
import Lib.Response (Response)

-- | Possible developments of a construction
data Passo m b
    = -- | Choice constrained to a list of possibilities
      forall a. Scelta Response [(String, a)] (a -> m (HPasso m b))
    | -- | Free input parsed from a string
      forall a. (Read a) => Libero Response (a -> m (HPasso m b))
    | -- | File upload
      forall a. (Read a) => Upload String (a -> m (HPasso m b))
    | -- | Output display (with optional continuation)
      Output Response (Maybe (m (HPasso m b)))
    | -- | Error display (with optional continuation)
      Errore Response (Maybe (m (HPasso m b)))
    | -- | File download
      forall a. (Show a) => Download String String a (m (HPasso m b))
    | -- | Password input
      forall a. (Read a) => Password String (a -> m (HPasso m b))
    | -- | Completed construction with final value
      Costruito b

-- | Historied Passo: a step paired with history of previous steps
type HPasso m b = (Passo m b, [m (Passo m b)])

-- | Construction monad for building values interactively
type Costruzione m b = StateT [m (Passo m b)] (ContT (HPasso m b) m)

-- | Wrap a step constructor into the construction monad
wrap
    :: (Monad m)
    => ((a -> m (HPasso m b)) -> Passo m b)
    -> Costruzione m b a
wrap f = StateT $ \ns -> ContT $ \k ->
    return $
        let c a = k (a, (fst <$> c a) : ns)
        in  (f c, ns)

-- | Run a construction to produce a step
svolgi :: (Monad m) => Costruzione m b b -> m (HPasso m b)
svolgi = flip runContT (return . first Costruito) . flip runStateT []

-- | Create a free input step (parsed from string)
libero :: (Read a, Monad m) => Response -> Costruzione m b a
libero prompt = wrap $ Libero prompt

-- | Create a password input step
password :: (Read a, Monad m) => String -> Costruzione m b a
password prompt = wrap $ Password prompt

-- | Create an output display step
output
    :: (Monad m)
    => Bool
    -- ^ whether to continue after display
    -> Response
    -- ^ content to display
    -> Costruzione m b ()
output continue s = wrap $ \c -> Output s $ if continue then Just (c ()) else Nothing

-- | Create an error display step
errore
    :: (Monad m)
    => Bool
    -- ^ whether to continue after display
    -> Response
    -- ^ error content to display
    -> Costruzione m b ()
errore continue s = wrap $ \c -> Errore s $ if continue then Just (c ()) else Nothing

-- | Create a file upload step
upload :: (Read a, Monad m) => String -> Costruzione m b a
upload prompt = wrap $ Upload prompt

-- | Create a file download step
download :: (Show a, Monad m) => String -> String -> a -> Costruzione m b ()
download s f x = wrap $ \c -> Download s f x $ c ()

-- | Create a choice selection step
scelte
    :: (Monad m)
    => [(String, a)]
    -- ^ list of (label, value) choices
    -> Response
    -- ^ prompt
    -> Costruzione m b a
scelte xs prompt = wrap $ \c -> Scelta prompt xs c

-- | Present a menu of operational choices
menu
    :: (Functor m, Monad m)
    => Response
    -- ^ description
    -> [(String, Costruzione m b a)]
    -- ^ menu items (label, action)
    -> Costruzione m b a
menu prompt = join . flip scelte prompt

-- | Create a looping menu that restores state on each iteration
rotonda
    :: (Monad m)
    => ((a -> Costruzione m b a) -> Costruzione m b a)
    -> Costruzione m b a
rotonda f = callCC $ \k -> forever $ get >>= \c -> f k >> put c

-- | Create a hand menu with exit option
mano
    :: (Functor m, Monad m)
    => Response
    -> [(String, Costruzione m b ())]
    -> Costruzione m b ()
mano s xs = rotonda (rmenu s xs)

-- | Menu with exit option
rmenu
    :: (Functor m, Monad m)
    => Response
    -> [(String, Costruzione m b ())]
    -> (() -> Costruzione m b ())
    -> Costruzione m b ()
rmenu s xs k = menu s $ ("<exit>", k ()) : xs
