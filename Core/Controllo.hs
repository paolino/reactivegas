{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{- | Core glue to facilitate loading a block of events and accessing state,
starting from reaction serialization. Module for serializing the reaction tree.
Serialization is complicated by the fact that nodes contain reaction procedures.
The solution is to recreate the procedures. To do this, we need to reload a set
of events that, being potentially discontinuous in time, are each stored with
the state in which they must be loaded to reproduce the desired reactions.
Note that the application state itself is serializable. The problem arises from
the fact that reactions are dynamic - they sometimes arise from event loading.
-}
module Core.Controllo where

import Control.Applicative ((<$>))
import Control.Arrow (second)
import Control.Monad (foldM, msum, zipWithM)
import Data.Function (on)
import Data.List (nubBy, tails)
import Data.Maybe (isJust)

import Lib.Missing (foldDeleteMb)
import Lib.Prioriti (R, sortP)

import Core.Contesto (Contestualizzato, esterno, nuovoContesto)
import Core.Inserimento (conFallimento, inserimentoCompleto)
import Core.Nodo (Appuntato (..), Nodo (..))
import Core.Parsing (ParserConRead, parser, valore)
import Core.Programmazione (Message, Reazione (..), TyReazione, provaAccentratore, runInserzione)
import Core.Types (Esterno)

import Debug.Trace

-- | Serialized node, a copy of a Nodo structure not containing the node's reaction function
data SNodo s d = SNodo
    { attivo :: Bool
    -- ^ reaction activity state
    , sottonodi :: [(Appuntato s d, [(Int, SNodo s d)])]
    -- ^ dependent deserialization structure
    }
    deriving (Read, Show, Eq)

-- amendSNodo :: (s -> s') -> Nodo s c d -> Nodo s' c d
amendSNodo f (SNodo x ys) = SNodo x $ map (\((e, s), zs) -> ((e, f s), map (second $ amendSNodo f) zs)) ys

-- | An empty SNodo
nodoVuoto :: SNodo s d
nodoVuoto = SNodo True []

-- | Converts an SNodo structure to a Nodo with the reaction contribution.
-- Reactions of subsequent nodes are built by inserting specially remembered events.
deserializza ::
    -- | the serialized node to recreate
    SNodo s d ->
    -- | its reaction
    Reazione s c d ->
    -- | the resulting live node
    Maybe (Nodo s c d)
deserializza (SNodo k rs) r@(Reazione (acc, f :: TyReazione a b d s c)) =
    let
        -- te :: (Contestuale s d,[(Int,SNodo s d)]) -> (Contestuale s d,[(Int,Nodo s c d)])
        te ((ec@(Right (u, x)), s), js) = ((ec, s),) `fmap` ns
          where
            ns = case (valore :: ParserConRead a -> a) <$> parser x of
                Nothing -> Nothing
                Just y ->
                    let (Just (_, (qs, _)), _, _) = runInserzione (f (Right (u, y))) nuovoContesto s
                     in mapM (\(i, d) -> (i,) `fmap` deserializza d (qs !! i)) js
        te ((ec@(Left x), s), js) = ((ec, s),) `fmap` ns
          where
            ns = case maybe ((valore :: ParserConRead b -> b) <$> parser x) (provaAccentratore x) acc of
                Nothing -> Nothing
                Just y ->
                    let (Just (_, (qs, _)), _, _) = runInserzione (f (Left y)) nuovoContesto s
                     in mapM (\(i, d) -> (i,) `fmap` deserializza d (qs !! i)) js
     in
        Nodo (if k then Just r else Nothing) `fmap` mapM te rs

-- | Converts a Nodo structure to an SNodo. The base node's reaction function
-- must be the same when deserialized.
serializza ::
    -- | the live node
    Nodo s c d ->
    -- | the serialized node
    SNodo s d
serializza (Nodo k rs) = SNodo (isJust k) (map (second $ map (second serializza)) rs)

-- | Event loading program with priority reordering
caricaEventi ::
    (Show d, Eq d, Show s) =>
    -- | prioritizers
    [R] ->
    -- | base reactions
    [Reazione s c d] ->
    -- | loading level
    Int ->
    -- | events to load
    [Esterno d] ->
    -- | state and reactive tree serialization
    (s, [SNodo s d]) ->
    -- | new state, new reactive tree serialization with contextualized logs
    ((s, [SNodo s d]), [Contestualizzato d Message])
caricaEventi ps rs l xs (s, nss) =
    let ns = case zipWithM deserializza nss rs of
            Nothing -> error $ "deserializzazione fallita" ++ show (length nss) ++ show s
            Just ns -> ns
        xs' = sortP l ps snd xs
        ((ns', ahi), s', ws) = runInserzione (foldDeleteMb inserimentoCompleto ns xs') nuovoContesto s
        nss' = map serializza ns'
     in ((s', nss'), ws)

-- | Event loading program with priority reordering (live nodes version)
caricaEventi' ::
    (Show d, Eq d, Show s) =>
    -- | prioritizers
    [R] ->
    -- | loading level
    Int ->
    -- | events to load
    [Esterno d] ->
    -- | state and reactive tree
    (s, [Nodo s c d]) ->
    -- | new state, new reactive tree with contextualized logs
    ((s, [Nodo s c d]), [Contestualizzato d Message])
caricaEventi' ps l xs (s, ns) =
    let xs' = sortP l ps snd xs
        ((ns', ahi), s', ws) = runInserzione (foldDeleteMb inserimentoCompleto ns xs') nuovoContesto s
     in ((s', ns'), ws)

----------------------------------------
