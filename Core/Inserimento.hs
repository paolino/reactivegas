{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Core.Inserimento
Description : State transformation through event processing
Copyright   : (c) Paolo Veronelli, 2025
License     : BSD-3-Clause

Module for transforming program state. Events are expressed as strings
while the state is typed. Provides the machinery for inserting events
into the reactive tree and propagating internal events.
-}
module Core.Inserimento
    ( -- * Types
      Inserimento
    , MTInserzione
    , CoreEvents (..)
    , UString (..)
      -- * Event processing
    , runInserimento
    , inserimento
    , inserimentoCompleto
    , eventoRifiutato
      -- * MTInserzione utilities
    , fallimento
    , loggamus
    , logga
    , osserva
    , modifica
    , conFallimento
    , mus
    ) where

import Codec.Binary.UTF8.String (encodeString)
import Control.Arrow (second)
import Control.Monad (foldM, mzero, when)
import Control.Monad.RWS (get, gets, lift, local, modify, put)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Writer (WriterT, listen, runWriterT, tell)
import Data.Typeable (Typeable)

import Core.Contesto (motiva)
import Core.Nodo (Nodo (..), mkNodi, pruner)
import Core.Parsing (ParserConRead, parser, valore)
import Core.Programmazione
    ( EventoInterno (..)
    , Fallimento (..)
    , Inserzione
    , Message (..)
    , Reazione (..)
    , TyReazione
    , logInserimento
    , provaAccentratore
    , runInserzione
    )
import Core.Types (Esterno, Interno)
import Lib.Aspetti (ParteDi, see, seeset)
import Lib.Signal (SignalT, happened, intercept, runSignalT)

-- | Temporary monad for event insertion
-- The Writer accumulates internal events contextualized to their causes
-- The SignalT declares that at least one reaction occurred
type Inserimento s c d = SignalT (WriterT [EventoInterno] (Inserzione s c d))

-- | Run an insertion computation
runInserimento
    :: Inserimento s c d a
    -> WriterT [EventoInterno] (Inserzione s c d) (a, Bool)
runInserimento = runSignalT

-- | Insert an event into the state, attempting the reaction in a Node
-- and all contained Nodes, producing a list of internal events
inserimento
    :: (Show d)
    => Either Interno (Esterno d)
    -- ^ internal or external event to process
    -> Nodo s c d
    -- ^ reactive branch
    -> Inserimento s c d (Nodo s c d)
    -- ^ updated reactive branch
inserimento x (Nodo Nothing rs) =
    Nodo Nothing <$> mapM (secondM (mapM (secondM $ inserimento x))) rs
  where
    secondM f (a, b) = (a,) <$> f b
inserimento x n@(Nodo k@(Just (Reazione (acc, f :: TyReazione a b d s c))) _) = do
    -- First execute insertion in children simulating a dead node
    Nodo mreat rs <- inserimento x n{reattore = Nothing}
    case mreat of
        Just _ -> error "impossible insertion state"
        Nothing -> do
            -- Record state for potential rollback or contextualization
            s' <- get
            let complete v = do
                    result <- lift . lift $ f v
                    case result of
                        Just (t, (zip [0 ..] . mkNodi -> ns, nevs)) -> do
                            -- Signal that at least one reaction occurred
                            happened
                            -- Log any created internal events
                            tell nevs
                            -- Return updated node, checking if reaction is finished
                            return . Nodo (if t then k else Nothing) $ ((x, s'), ns) : rs
                        Nothing -> put s' >> reject
                reject = return (n{seguenti = rs})
            case x of
                Right (u, y) ->
                    -- External event
                    case (valore :: c a -> a) <$> parser y of
                        Nothing -> reject
                        Just v -> complete (Right (u, v))
                Left y ->
                    -- Internal event
                    case maybe
                        ((valore :: ParserConRead b -> b) <$> parser y)
                        (provaAccentratore y)
                        acc of
                        Nothing -> reject
                        Just v -> complete (Left v)

-- | Core internal event signaling that no reactor accepted the event
data CoreEvents = Rifiuto
    deriving (Read, Show)

-- | Pattern match helper for rejection events
-- We don't export the constructor to prevent other modules from
-- producing this event, only matching on it
eventoRifiutato :: CoreEvents -> Maybe ()
eventoRifiutato Rifiuto = Just ()

-- | Completely insert an event, re-inserting any internal events
-- created during the insertion itself
inserimentoCompleto
    :: (Show d)
    => [Nodo s c d]
    -> Esterno d
    -> Inserzione s c d (Maybe [Nodo s c d])
inserimentoCompleto ns x = fmap (fst . fst) . runWriterT . runInserimento $ do
    (ns', t) <- intercept $ consuma ns (Right x)
    if not t
        then return Nothing
        else return $ Just ns'
  where
    -- Execute insertion on branches with dead branch cleanup
    inserimentoAlbero
        :: (Show d)
        => Either Interno (Esterno d)
        -> [Nodo s c d]
        -> Inserimento s c d [Nodo s c d]
    inserimentoAlbero ev = mapM (fmap pruner . inserimento ev)

    -- Consume an external event or a list of internal events
    consuma
        :: (Show d)
        => [Nodo s c d]
        -> Either [Interno] (Esterno d)
        -> Inserimento s c d [Nodo s c d]
    consuma nodes (Left xs) = foldM step nodes xs
      where
        step nodes' ev = local (motiva $ Left $ encodeString ev) $ do
            (nodes'', map (\(EventoInterno e) -> show e) -> xs') <-
                listen $ inserimentoAlbero (Left ev) nodes'
            if null xs' then return nodes'' else consuma nodes'' (Left xs')
    consuma nodes (Right e) = local (motiva $ Right $ second encodeString e) $ do
        (nodes', map (\(EventoInterno e') -> show e') -> xs) <-
            listen $ inserimentoAlbero (Right e) nodes
        if null xs then return nodes' else consuma nodes' (Left xs)

-- | Insertion monad with failure handling
type MTInserzione s c d = MaybeT (Inserzione s c d)

-- | UTF-8 encoded string wrapper
newtype UString = UString String
    deriving (Typeable)

instance Show UString where
    show (UString s) = encodeString s

-- | Create a message from a string
mus :: String -> Message
mus = Message . UString

-- | Handle a failure by logging the reason
fallimento
    :: Bool
    -- ^ condition (fail if True)
    -> String
    -- ^ failure reason
    -> MTInserzione s c d ()
fallimento condition reason =
    when condition $ logga (Message . Fallimento . UString $ reason) >> mzero

-- | Log a user message
loggamus :: String -> MTInserzione s c d ()
loggamus = logga . mus

-- | Log a message
logga :: Message -> MTInserzione s c d ()
logga s = lift (logInserimento s)

-- | Read a value of type @a@ from the state
osserva :: (ParteDi a s) => MTInserzione s c d a
osserva = lift $ gets see

-- | Modify the value of type @a@ in the state
modifica :: (ParteDi a s) => (a -> a) -> MTInserzione s c d ()
modifica = lift . modify . seeset

-- | Run the failure handling layer
conFallimento :: MTInserzione s c d a -> Inserzione s c d (Maybe a)
conFallimento = runMaybeT
