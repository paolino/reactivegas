{-# LANGUAGE LambdaCase #-}

-- | Session management, handling the draft model where the user operates
-- before persisting their work
module Applicazioni.Sessione (Sessione (..), mkSessione, Update) where

import Control.Applicative ((<$>))
import Control.Arrow (second)
import Control.Monad (forever, liftM2, when)
import Data.List (union, (\\))
import Data.Maybe (fromMaybe, listToMaybe)

-- import System.Random
import Control.Concurrent (forkIO)
import Control.Concurrent.STM

import Core.Types (Evento, Responsabile, Utente)
import Lib.QInteger
import Lib.STM (condSignal)

import Applicazioni.Persistenza (Change (..))

import Core.Dichiarazioni
import Debug.Trace

type Indice = QInteger
type Name = String

-- | Concurrent interface for an interaction session
data Sessione a b p = Sessione
    { readGruppo :: IO (Maybe Name)
    -- ^ selected group
    , writeGruppo :: Maybe Name -> IO ()
    -- ^ change group
    , readEventi :: IO [Evento]
    -- ^ reads events in memory
    , aggiungiEvento :: Dichiarazione p Singola -> IO ()
    , eliminaEvento :: IO [(Evento, IO ())]
    , correggiEvento :: Dichiarazione p Composta -> IO ()
    , readAccesso :: IO (Maybe Responsabile)
    -- ^ reads the responsible in action
    , writeAccesso :: Maybe Responsabile -> IO ()
    -- ^ writes the responsible in action
    , readAcquisto :: IO (Maybe Indice)
    , writeAquisto :: Maybe Indice -> IO ()
    , readOrdinante :: IO (Maybe Utente)
    -- ^ reads the ordering user
    , writeOrdinante :: Maybe Utente -> IO ()
    -- ^ writes the ordering user
    , readCaricamento :: IO (Maybe b)
    -- ^ reads the effect of the last declaration loading
    , readStatoSessione :: IO (Maybe a)
    -- ^ reads the state modified by in-memory events produced by the in-memory responsible
    , setConservative :: Int -> IO ()
    -- ^ sets the loading level
    , getConservative :: IO Int
    -- ^ reads the loading level
    , backup :: IO (Maybe Name, [Evento], Maybe Responsabile, Int)
    }

-- | Events that trigger recomputation of the modified state
data Triggers p
    = TResponsabile (Maybe Responsabile)
    | TAcquisto (Maybe Indice)
    | TOrdinante (Maybe Utente)
    | TEventi (Dichiarazioni p)
    | TConservative Int
    | TGruppo (Maybe Name)

-- | Recomputation model that must be provided
type Update a b = Name -> STM (Maybe (Int -> Maybe Responsabile -> [Evento] -> STM (a, b)))

type Parse p = [Evento] -> Dichiarazioni p

-- | Action to modify events or responsible
update ::
    Parse p -> -- declaration reproduction
    Update a b -> -- produces a modified state

    -- | standard event loading level
    Int ->
    -- | shared memory
    ( TVar (Dichiarazioni p)
    , TVar (Maybe Responsabile)
    , TVar (Maybe Indice)
    , TVar (Maybe Utente)
    , TVar Int
    , TVar (Maybe a) -- last calculated state
    , TVar (Maybe b) -- last calculated effects
    , TVar (Maybe Name) -- group name, if selected
    , TChan (Triggers p)
    , TVar (Maybe (STM (Change c d)))
    , Name -> STM (Maybe (STM (STM (Change c d)))) -- external condition that triggers renewal
    , Name -> STM (Maybe (Maybe Utente -> STM [Evento])) -- events published for a user
    ) ->
    STM ()
update pa f l (eventi, accesso, acquisto, ordinante, conservative, stato, caricamento, gruppo, triggers, signalbox, newsignal, publ) = do
    let
        -- update caused by user modification
        interna = do
            t <- readTChan triggers
            case t of
                TAcquisto mr -> writeTVar acquisto mr
                TOrdinante mr -> writeTVar ordinante mr
                TResponsabile mr -> do
                    mg <- readTVar gruppo
                    case mg of
                        Nothing -> return ()
                        Just g -> do
                            writeTVar accesso mr
                            publ g >>= \case
                                Nothing -> return ()
                                Just k -> k (fst <$> mr) >>= writeTVar eventi . pa
                TEventi ds -> writeTVar eventi ds
                TConservative l -> writeTVar conservative l
                TGruppo n -> case n of
                    Just g ->
                        newsignal g >>= \case
                            Just ns -> do
                                signal <- ns
                                writeTVar signalbox (Just signal)
                                writeTVar accesso Nothing
                                writeTVar eventi (pa [])
                                writeTVar conservative l
                                writeTVar gruppo n
                            Nothing -> return ()
                    Nothing -> do
                        writeTVar signalbox Nothing
                        writeTVar gruppo Nothing
                        writeTVar accesso Nothing
                        writeTVar eventi (pa [])
                        writeTVar conservative l
                        writeTVar gruppo n
        esterna = do
            g <- readTVar gruppo
            case g of
                Just g -> do
                    msignal <- readTVar signalbox
                    case msignal of
                        Just signal -> do
                            s <- signal
                            ess <- readTVar eventi
                            mr <- fmap fst <$> readTVar accesso
                            case s of
                                Boot _ -> return ()
                                GPatch digested orphans _ -> do
                                    let ofs =
                                            fromMaybe [] $
                                                mr
                                                    >>= \u -> lookup u orphans
                                        dgs =
                                            fromMaybe [] $
                                                mr
                                                    >>= \u -> lookup u digested
                                    -- TODO !!!
                                    writeTVar eventi $ pa [] -- (ess `union` ofs) \\ dgs
                                    writeTVar conservative l
                                UPatch u esp -> when (Just u == mr) $ writeTVar eventi $ pa esp
                        Nothing -> retry
                Nothing -> retry
    interna `orElse` esterna
    g <- readTVar gruppo
    case g of
        Just g -> do
            mr <- readTVar accesso
            evs <- readTVar eventi
            l' <- readTVar conservative
            -- perform loading respecting session conditions
            mu <- f g
            case mu of
                Nothing -> do
                    writeTVar stato Nothing
                    writeTVar caricamento Nothing
                Just k -> do
                    (s, c) <- k l' mr (toEventi evs)
                    writeTVar stato (Just s)
                    writeTVar caricamento (Just c)
        Nothing -> do
            writeTVar stato Nothing
            writeTVar caricamento Nothing

-- | Constructs the session interface from an STM state modifier
mkSessione ::
    -- | event reader
    Parse p ->
    -- | state modifier
    Update a b ->
    -- | base loading level
    Int ->
    -- | state update signal
    (Name -> STM (Maybe (STM (STM (Change c d))))) ->
    -- | query for events published for a user
    (Name -> STM (Maybe (Maybe Utente -> STM [Evento]))) ->
    -- | session modification signal
    STM () ->
    Maybe (Maybe Name, [Evento], Maybe Responsabile, Int) ->
    IO (Sessione a b p)
mkSessione pa f l signal publ exsignal ms = do
    (stato, caricamento) <- atomically $ do
        msc <- case ms of
            Nothing -> return Nothing
            Just (Nothing, _, _, _) -> return Nothing
            Just (Just g, es, mr, cl) ->
                f g >>= \case
                    Just k -> Just <$> k cl mr es
                    Nothing -> return Nothing
        liftM2 (,) (newTVar $ fst <$> msc) (newTVar $ snd <$> msc)
    eventi <- newTVarIO $ maybe (Dichiarazioni [] []) (\(_, es, _, _) -> pa es) ms
    accesso <- newTVarIO $ ms >>= \(_, _, mr, _) -> mr
    acquisto <- newTVarIO Nothing
    ordinante <- newTVarIO Nothing
    accesso <- newTVarIO $ ms >>= \(_, _, mr, _) -> mr
    triggers <- newTChanIO
    conservative <- newTVarIO $ maybe l (\(_, _, _, cl) -> cl) ms
    gruppo <- newTVarIO $ ms >>= \(mg, _, _, _) -> mg
    signalbox <- atomically $ case ms of
        Nothing -> newTVar Nothing
        Just (mg, _, _, _) -> case mg of
            Nothing -> newTVar Nothing
            Just g ->
                signal g >>= \case
                    Nothing -> newTVar Nothing
                    Just mks -> mks >>= newTVar . Just
    let memoria = (eventi, accesso, acquisto, ordinante, conservative, stato, caricamento, gruppo, triggers, signalbox, signal, publ)
        checkUpdate q = (update pa f l memoria >> q) `orElse` q
        write f x = atomically $ writeTChan triggers (f x) >> exsignal
        read t = atomically . checkUpdate $ readTVar t

    return $
        Sessione
            (read gruppo)
            (write TGruppo)
            (toEventi `fmap` read eventi)
            (\d -> read eventi >>= write TEventi . aggiungi d)
            (map (second $ write TEventi) . elimina <$> read eventi)
            (\d -> read eventi >>= write TEventi . correggi d)
            (read accesso)
            (write TResponsabile)
            (read acquisto)
            (write TAcquisto)
            (read ordinante)
            (write TOrdinante)
            (read caricamento)
            (read stato)
            (write TConservative)
            (read conservative)
            ( atomically $ do
                mg <- readTVar gruppo
                es <- toEventi <$> readTVar eventi
                cl <- readTVar conservative
                mr <- readTVar accesso
                return (mg, es, mr, cl) -- TODO: serialize acquisto and ordinante (but session restore doesn't work)
            )
