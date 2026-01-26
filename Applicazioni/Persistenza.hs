{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : Applicazioni.Persistenza
Description : Program persistence module
Copyright   : (c) Paolo Veronelli, 2025
License     : BSD-3-Clause

Handles persistence with two aspects:
- Current data that persists to files as-is
- Historical data that persists to files through SQLite
-}
module Applicazioni.Persistenza
    ( -- * Types
      GroupName
    , Change (..)
    , WhiteLoadResult
    , Persistence (..)

      -- * Persistence operations
    , mkPersistence
    ) where

import Control.Arrow (second, (&&&))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
    ( STM
    , TChan
    , TVar
    , atomically
    , dupTChan
    , newTChan
    , newTVarIO
    , readTChan
    , readTVar
    , readTVarIO
    , writeTChan
    , writeTVar
    )
import Control.Monad (MonadPlus (mplus), forever, liftM2, when)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (groupBy, sortBy, union, (\\))
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import System.IO (hFlush, stdout)

import Applicazioni.Database.GPatch (GPatches (..), mkGPatches)
import Core.Patch (Group, Patch, fromGroup, fromPatch)
import Core.Types (Evento, Responsabile, Utente)
import Lib.Filesystem (groupUnwrite, groupWrite)
import Lib.STM (condSignal)
import Lib.States (FromPast (FromPast), ToPast (ToPast), Transition (..))

-- | Helper for extracting values from Maybe with error on Nothing
extractOrError :: (Show a) => a -> Maybe p -> p
extractOrError context = \case
    Nothing -> error (show context)
    Just value -> value

-----------------------------------------------------------------------------------------

-- | Group name identifier
type GroupName = String

instance (Transition a) => Transition (Int, a) where
    back (n, x) = case back x of
        Nothing -> Nothing
        Just (ToPast y) -> Just $ ToPast (n, y)
    forth = case forth of
        Nothing -> Nothing
        Just (FromPast f) -> Just $ FromPast $ second f

-- | Persistence operation
--
-- Writes a snapshot of (state, patches) or tokens when triggered
persistence
    :: (Show a)
    => (forall b. (Show b) => String -> Int -> b -> IO ())
    -- ^ write operation
    -> TVar Int
    -- ^ state version
    -> TVar [(Utente, Patch)]
    -- ^ user -> individual patch association
    -> TVar [(Utente, [Evento])]
    -- ^ orphan events
    -> TChan String
    -- ^ logger
    -> TVar a
    -- ^ state
    -> TChan ()
    -- ^ persistence trigger
    -> IO ()
persistence write versionVar patchVar orphanVar _logChan stateVar trigger = do
    (version, patches, orphans, state) <- atomically $ do
        readTChan trigger
        version <- readTVar versionVar
        patches <- readTVar patchVar
        orphans <- readTVar orphanVar
        state <- readTVar stateVar
        return (version, patches, orphans, state)
    write "patches" version patches
    write "orfani" version orphans
    write "stato.corrente" version state

{- | Recovery operation

Recreates all group data from filesystem.
Returns True if recalculation from updates is necessary,
useful for clearing derived information from GPatch.
The returned action performs the actual state computation.
-}
recovery
    :: (Read a, Show a)
    => (forall b. (Show b) => String -> Int -> b -> IO ())
    -- ^ write operation
    -> (forall b. (Read b) => String -> IO (Maybe (Int, b)))
    -- ^ read operation
    -> String
    -- ^ group name
    -> Persistence a b d
    -- ^ persistence interface
    -> TVar Int
    -- ^ version var
    -> TVar [(Utente, Patch)]
    -- ^ patches var
    -> TVar [(Utente, [Evento])]
    -- ^ orphan events
    -> TChan String
    -- ^ logger
    -> Int
    -- ^ expected version
    -> TVar a
    -- ^ state var
    -> IO (Bool, IO ())
recovery write unwrite groupName pers versionVar patchVar orphanVar _logChan expectedVersion stateVar = do
    let complete = do
            patchesResult <- unwrite "patches"
            orphansResult <- unwrite "orfani"
            atomically $ do
                v <- readTVar versionVar
                case patchesResult of
                    Nothing -> return ()
                    Just (v', ps') ->
                        when (v' == v) $ writeTVar patchVar ps'
                case orphansResult of
                    Nothing -> return ()
                    Just (v', os') ->
                        when (v' == v) $ writeTVar orphanVar os'
                writeTVar versionVar v

    let reload =
            ( True
            , do
                bootState <- groupUnwrite groupName "stato.boot"
                case bootState of
                    Just (0, s) -> do
                        atomically $ writeTVar stateVar s
                        atomically $ writeTVar versionVar 0
                        putStr "aggiornamenti:"
                        autoFeed pers
                        s' <- readTVarIO stateVar
                        v' <- readTVarIO versionVar
                        write "stato.corrente" v' s'
                        putStrLn "\n"
                    Nothing -> do
                        s <- readTVarIO stateVar
                        groupWrite groupName "stato.boot" 0 s
                        putStrLn "stato iniziale scritto"
            )

    currentState <- groupUnwrite groupName "stato.corrente"
    result <- case currentState of
        Just (vc, s) -> do
            putStrLn $
                last (show s)
                    `seq` "rilevato file di stato corrente "
                    ++ show vc
            if vc == expectedVersion
                then
                    return
                        ( False
                        , atomically $
                            writeTVar versionVar vc >> writeTVar stateVar s
                        )
                else return reload
        Nothing -> return reload

    return $ second (>> complete) result

-- | Change notification messages
data Change a b
    = -- | Boot message with initial state
      Boot a
    | -- | Group patch arrived: digested events, orphan events, (effects, state)
      GPatch [(Utente, [Evento])] [(Utente, [Evento])] (b, a)
    | -- | User patch arrived: author and events
      UPatch Utente [Evento]
    deriving (Show)

-- | Group declarations by responsible user
groupUp :: [(Utente, Evento)] -> [(Utente, [Evento])]
groupUp =
    map (fst . head &&& map snd)
        . groupBy ((==) `on` fst)
        . sortBy (comparing fst)

-- | Flatten grouped declarations
groupDown :: [(Utente, [Evento])] -> [[(Utente, Evento)]]
groupDown = map (\(u, es) -> map (u,) es)

-- | Update transaction triggered by a group patch arrival
updateTransaction
    :: (a -> [(Utente, Evento)] -> Either String (a, b))
    -- ^ update attempt
    -> TVar Int
    -- ^ version
    -> TVar a
    -- ^ state
    -> TVar [(Utente, Patch)]
    -- ^ individual updates
    -> TVar [(Utente, [Evento])]
    -- ^ orphan events
    -> TChan String
    -- ^ log
    -> [(Utente, Evento)]
    -- ^ offered group update
    -> (Change a b -> STM ())
    -- ^ callback
    -> STM ()
updateTransaction load versionVar stateVar patchVar orphanVar logChan evs callback = do
    s <- readTVar stateVar
    let result = load s evs
        digested = groupUp evs
    case result of
        Left e -> writeTChan logChan e -- loading problem
        Right (s', effects) -> seq s' $ do
            v <- (+ 1) <$> readTVar versionVar
            writeTVar stateVar s' -- write new state
            waitingEvents <-
                concatMap (\(u, (_, _, es)) -> map (u,) es)
                    <$> readTVar patchVar
            existingOrphans <- concat . groupDown <$> readTVar orphanVar
            let orphans = groupUp $ (existingOrphans `union` waitingEvents) \\ evs
            writeTVar orphanVar orphans -- update orphans
            writeTVar patchVar [] -- clear individual updates
            writeTVar versionVar v -- write version
            callback $ GPatch digested orphans (effects, s')

-- | White modification interface type
--
-- A white load on the current state
type WhiteLoadResult a d =
    Int
    -- ^ loading level
    -> Maybe Responsabile
    -- ^ responsible author or anonymous
    -> [Evento]
    -- ^ author's declarations
    -> STM (a, d)
    -- ^ modified state with loading effects

-- | Produce the white modification interface
mkWhiteLoad
    :: (Int -> a -> [(Utente, Evento)] -> (a, d))
    -> TVar a
    -> TVar [(Utente, Patch)]
    -> WhiteLoadResult a d
mkWhiteLoad f stateVar patchVar level maybeResp events = do
    s <- readTVar stateVar
    let
        -- Map individual updates to a pool of events with author
        extractEvents =
            concatMap (\(u, (_, _, es)) -> map (u,) es)
                <$> readTVar patchVar
        authorEvents u = (++ map (u,) events) . filter ((/=) u . fst)
    -- Compute modification with or without new events depending on author presence
    f level s . maybe id (authorEvents . fst) maybeResp <$> extractEvents

-- | Functional persistence layer
--
-- Note: in-memory state is dangerous...
data Persistence a b d = Persistence
    { persReloadCond :: IO (STM Bool)
    -- ^ Condition for reload signal
    , readState :: IO (Int, a)
    -- ^ Read current state with version
    , writeUserPatch :: Utente -> Patch -> IO ()
    -- ^ Write a user patch
    , readUserPatches :: IO (Int, [(Utente, Patch)])
    -- ^ Read all user patches with version
    , writeGroupPatch :: Group -> IO ()
    -- ^ Write a group patch
    , readGroupPatch :: Int -> IO (Maybe Group)
    -- ^ Read a group patch by version
    , readVersion :: IO Int
    -- ^ Current state version
    , readLogs :: IO String
    -- ^ Read log messages
    , updateSignal :: STM (STM (Change a b))
    -- ^ Signal for state changes
    , queryUser :: Maybe Utente -> STM [Evento]
    -- ^ Collect events for a user from persistence
    , whiteLoad :: WhiteLoadResult a d
    -- ^ White modification interface
    }

-- | Prepare a virgin group state
--
-- @a@ is the state type, @b@ the effects type
mkPersistence
    :: (Show c, Read a, Show a, Show b)
    => STM ()
    -- ^ signal any change
    -> (a -> [(Utente, Evento)] -> Either String (a, b))
    -- ^ specific loader for @a@
    -> (Int -> a -> [(Utente, Evento)] -> (a, d))
    -- ^ direct event inserter for @a@
    -> a
    -- ^ group initializer
    -> (c -> [Responsabile])
    -- ^ get responsibles from context
    -> (a -> c)
    -- ^ get context from state
    -> GroupName
    -- ^ group name
    -> IO (Persistence a b d, IO (), Bool)
mkPersistence signal load modify boot getResponsibles getContext groupName = do
    -- Instantiate shared memory
    trigger <- atomically newTChan
    versionVar <- newTVarIO 0
    stateVar <- newTVarIO boot
    patchVar <- newTVarIO []
    orphanVar <- newTVarIO []
    logChan <- atomically newTChan
    changeChan <- atomically newTChan
    groupPatches <- mkGPatches groupName
    lastVersion <- fromInteger `fmap` ultimaGPatch groupPatches

    let atomicallyWithPersist f =
            atomically $
                writeTChan trigger ()
                    >> signal
                    >> f (writeTChan changeChan)

        readState' = atomically $ do
            s <- readTVar stateVar
            v <- readTVar versionVar
            return (v, s)

        writeUserPatch' user patch@(_, _, events) = do
            s <- readTVarIO stateVar
            result <-
                runExceptT $
                    runReaderT (fromPatch getResponsibles patch) (getContext s)
            case result of
                Right _ -> do
                    atomicallyWithPersist $ \notify -> do
                        existingPatches <- readTVar patchVar
                        writeTVar patchVar
                            . (++ [(user, patch) | not (null events)])
                            . filter ((/=) user . fst)
                            $ existingPatches
                        existingOrphans <- readTVar orphanVar
                        writeTVar orphanVar
                            . (++ [(user, [])])
                            . filter ((/=) user . fst)
                            $ existingOrphans
                        notify $ UPatch user events
                Left errMsg -> atomically $ writeTChan logChan errMsg

        readUserPatches' =
            atomically $ liftM2 (,) (readTVar versionVar) $ readTVar patchVar

        writeGroupPatch' group = do
            s <- readTVarIO stateVar
            result <-
                runExceptT $
                    runReaderT (fromGroup getResponsibles group) (getContext s)
            case result of
                Right (_, events) -> do
                    v <- atomicallyWithPersist $ \notify ->
                        updateTransaction
                            load
                            versionVar
                            stateVar
                            patchVar
                            orphanVar
                            logChan
                            events
                            notify
                            >> readTVar versionVar
                    nuovaGPatch groupPatches (fromIntegral v) group
                Left errMsg -> atomically $ writeTChan logChan errMsg

        readGroupPatch' = vecchiaGPatch groupPatches . fromIntegral

        readVersion' = readTVarIO versionVar

        readLogs' = atomically . readTChan $ logChan

        updateSignal' = dupTChan changeChan <&> readTChan

        queryUser' (Just user) = do
            patchEvents <-
                fmap (\(_, _, es) -> es) . lookup user <$> readTVar patchVar
            orphanEvents <- lookup user <$> readTVar orphanVar
            return . fromMaybe [] $ patchEvents `mplus` orphanEvents
        queryUser' Nothing = return []

        whiteLoad' = mkWhiteLoad modify stateVar patchVar

        pers =
            Persistence
                (condSignal trigger)
                readState'
                writeUserPatch'
                readUserPatches'
                writeGroupPatch'
                readGroupPatch'
                readVersion'
                readLogs'
                updateSignal'
                queryUser'
                whiteLoad'

        -- Persistence thread attached to trigger
        close = do
            forkIO
                . forever
                $ persistence
                    (groupWrite groupName)
                    versionVar
                    patchVar
                    orphanVar
                    logChan
                    stateVar
                    trigger
            forkIO . forever $ atomically (readTChan changeChan)
            putStrLn "persistenza attivata"

    (needsRecalc, bootAction) <-
        recovery
            (groupWrite groupName)
            (groupUnwrite groupName)
            groupName
            pers
            versionVar
            patchVar
            orphanVar
            logChan
            lastVersion
            stateVar
    return (pers, bootAction >> close, needsRecalc)

-- | Automatically feed patches from history
autoFeed :: Persistence a b d -> IO ()
autoFeed pers = do
    currentVersion <- readVersion pers
    maybeGroup <- readGroupPatch pers currentVersion
    case maybeGroup of
        Nothing -> return ()
        Just group -> do
            putStr $ show currentVersion ++ ","
            hFlush stdout
            writeGroupPatch pers group
            autoFeed pers
