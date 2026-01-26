{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Core.Parsing where

import Control.Applicative ((<$>))

-- | Un parser selezionabile
class (Show a, Read a) => Parser c a where -- richiediamo Show qui per comoditá ....
    parser ::
        String ->
        -- | dato un evento di tipo e si produce un potenziale valore taggato dal parser
        Maybe (c a)
    valore ::
        c a ->
        -- | estrazione del valore dal tag
        a
    boxer :: a -> c a
    priorita ::
        c a ->
        -- | priorita' di un evento nel fase di parsing
        Int
    serializza :: c a -> String

{- |  parser interno utilizzato per la deserializzazione dello stato di controllo
 attenzione alla relazione biiettiva show read in tutti gli eventi introdotti in tutti i plugin !!!!
-}
data ParserConRead a = ParserConRead a

listToMaybe' [] = Nothing
listToMaybe' xs = Just $ last xs

-- | il parser standard che utilizza read e show, valido per qualsiasi evento di tipo String
instance (Read a, Show a) => Parser ParserConRead a where
    parser s = ParserConRead <$> fst <$> listToMaybe' (reads s)
    valore (ParserConRead a) = a
    priorita (ParserConRead a) = 0
    boxer = ParserConRead
    serializza (ParserConRead a) = show a
