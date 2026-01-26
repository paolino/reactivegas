{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

{- | la colla che forma il Core per facilitare il caricamento di un blocco di eventi e per accedere allo stato, partendo dalla serializzazione delle reazioni
 Modulo per la serializzazione dell'albero delle reazioni. La serializzazione e' complicata dal fatto che i nodi contengono le procedure di reazione.
La soluzione e' ricreare le procedure . Per fare questo e' necessaro ricaricare  un set di eventi che essendo potenzialmente discontinui nel tempo vengono memorizzati ognuno con lo stato in cui deve essere caricato per riprodurre le reazioni desiderate.
Da notare che lo stato dell'applicazion in se e' serializzabile. Il problema nasce dal fatto che le reazioni sono dinamiche , nascono a volte dal caricamento degli eventi
-}
module Core.Controllo where

import Control.Applicative ((<$>))
import Control.Arrow (second)
import Control.Monad (foldM, msum)
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

-- | nodo serializzato , una copia di una struttura Nodo non contenente la funzione di reazione del nodo stesso
data SNodo s d = SNodo
    { attivo :: Bool
    -- ^ stato di attivita della reazione
    , sottonodi :: [(Appuntato s d, [(Int, SNodo s d)])]
    -- ^ struttura di deserializzazione dipendente
    }
    deriving (Read, Show, Eq)

-- amendSNodo :: (s -> s') -> Nodo s c d -> Nodo s' c d
amendSNodo f (SNodo x ys) = SNodo x $ map (\((e, s), zs) -> ((e, f s), map (second $ amendSNodo f) zs)) ys

-- | un SNodo vuoto
nodoVuoto :: SNodo s d
nodoVuoto = SNodo True []

-- | passa da una struttura SNodo a una Nodo con il contributo della reazione, le reazioni dei nodi seguenti sono costruite con l'inserimento degli eventi appositamente ricordati
deserializza ::
    -- | il nodo serializzato da ricreare
    SNodo s d ->
    -- | la sua reazione
    Reazione s c d ->
    -- | il nodo vivo ottenuto
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
                     in sequence $ map (\(i, d) -> (i,) `fmap` deserializza d (qs !! i)) js
        te ((ec@(Left x), s), js) = ((ec, s),) `fmap` ns
          where
            ns = case maybe ((valore :: ParserConRead b -> b) <$> parser x) (provaAccentratore x) acc of
                Nothing -> Nothing
                Just y ->
                    let (Just (_, (qs, _)), _, _) = runInserzione (f (Left y)) nuovoContesto s
                     in sequence $ map (\(i, d) -> (i,) `fmap` deserializza d (qs !! i)) js
     in
        Nodo (if k then Just r else Nothing) `fmap` (sequence $ map te rs)

-- | passa da una struttura Nodo a una SNodo, naturalmente la funzione reazione del nodo base deve essere la stessa quando verra' deserializzato
serializza ::
    -- | il nodo vivo
    Nodo s c d ->
    -- | il nodo serializzato
    SNodo s d
serializza (Nodo k rs) = SNodo (isJust k) (map (second $ map (second serializza)) rs)

-- | programma di caricamento eventi, prevede il riordinamento per priorita
caricaEventi ::
    (Show d, Eq d, Show s) =>
    -- | i prioritizzatori
    [R] ->
    -- | le reazioni base
    [Reazione s c d] ->
    -- | livello di caricamento
    Int ->
    -- | gli eventi da caricare
    [Esterno d] ->
    -- | lo stato e la serializzazione dell'albero reattivo
    (s, [SNodo s d]) ->
    -- | nuovo stato e nuova  serializzazione dell'albero reattivo insieme ai log contestualizzati
    ((s, [SNodo s d]), ([Contestualizzato d Message]))
caricaEventi ps rs l xs (s, nss) =
    let ns = case sequence . map (uncurry deserializza) . zip nss $ rs of
            Nothing -> error $ "deserializzazione fallita" ++ show (length nss) ++ show s
            Just ns -> ns
        xs' = sortP l ps snd xs
        ((ns', ahi), s', ws) = runInserzione (foldDeleteMb inserimentoCompleto ns xs') nuovoContesto s
        nss' = map serializza ns'
     in ((s', nss'), ws)

-- | programma di caricamento eventi, prevede il riordinamento per priorita
caricaEventi' ::
    (Show d, Eq d, Show s) =>
    -- | i prioritizzatori
    [R] ->
    -- | livello di caricamento
    Int ->
    -- | gli eventi da caricare
    [Esterno d] ->
    -- | lo stato e la serializzazione dell'albero reattivo
    (s, [Nodo s c d]) ->
    -- | nuovo stato e nuova  serializzazione dell'albero reattivo insieme ai log contestualizzati
    ((s, [Nodo s c d]), ([Contestualizzato d Message]))
caricaEventi' ps l xs (s, ns) =
    let xs' = sortP l ps snd xs
        ((ns', ahi), s', ws) = runInserzione (foldDeleteMb inserimentoCompleto ns xs') nuovoContesto s
     in ((s', ns'), ws)

----------------------------------------
