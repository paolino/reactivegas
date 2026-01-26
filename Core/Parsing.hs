{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Module      : Core.Parsing
Description : Selectable parser type class
Copyright   : (c) Paolo Veronelli, 2025
License     : BSD-3-Clause

Provides a type class for parsers that can parse events from
strings, with support for prioritization and serialization.
-}
module Core.Parsing
    ( Parser (..)
    , ParserConRead (..)
    ) where

-- | A selectable parser type class
class (Show a, Read a) => Parser c a where
    -- | Parse a string to produce a potential tagged value
    parser
        :: String
        -> Maybe (c a)

    -- | Extract the value from the tag
    valore :: c a -> a

    -- | Wrap a value in the parser tag
    boxer :: a -> c a

    -- | Get the priority of an event during parsing phase
    priorita :: c a -> Int

    -- | Serialize a tagged value back to string
    serializza :: c a -> String

-- | Standard parser using Read/Show instances
-- Used for deserialization of control state
-- Note: Requires bijective show/read relationship for all events
data ParserConRead a = ParserConRead a

-- | Like 'listToMaybe' but returns the last element
listToMaybe' :: [a] -> Maybe a
listToMaybe' [] = Nothing
listToMaybe' xs = Just $ last xs

-- | Standard parser instance using read and show
-- Valid for any event type with Read/Show instances
instance (Read a, Show a) => Parser ParserConRead a where
    parser s = ParserConRead <$> fst <$> listToMaybe' (reads s)
    valore (ParserConRead a) = a
    priorita (ParserConRead _) = 0
    boxer = ParserConRead
    serializza (ParserConRead a) = show a
