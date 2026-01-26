{-# LANGUAGE ScopedTypeVariables #-}

module Lib.Server.Session where

import Control.Exception (IOException, catch)

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (STM, TVar, atomically, newTChan, newTVar, readTChan, readTVar, writeTChan, writeTVar)
import Control.Monad (forever)
import Control.Monad.Trans (lift)
import Data.Time
import Debug.Trace
import Lib.Database (DB, dump, forget, limitedDB, query, restoreDB, set)
import Lib.Missing (secondM)
import Lib.SCGI (CGI, Cookie (..), getCookie, newCookie, setCookie)
import System.FilePath ((</>))
import System.Random (randomIO)
import System.Time (addToClockTime, getClockTime, noTimeDiff, tdMonth, toCalendarTime)

type Droppable a = CGI (a, IO ())

-- | we should treat the unwilling cookie browser (reject for now, at least), those now just trash our cookie db !
sessioning ::
    forall a b.
    (Read b, Show b) =>
    -- | cartella di lavoro
    FilePath ->
    -- | limit for remebering sessions
    Int ->
    -- | segnale per persistere
    STM () ->
    -- | creation of default value or restoring
    (Maybe b -> IO (a, IO b)) ->
    -- | (recall last value for cookie if present, reset anyway)
    IO (Droppable a, Droppable a)
sessioning path l signal rs = do
    os <- catch (readFile (path </> "sessioni") >>= \os -> putStrLn "rilevata persistenza delle sessioni" >> return os) (\(_ :: IOException) -> return "[]")
    qs <- seq (last os) . mapM (secondM $ rs . Just) $ case reads os of
        [] -> []
        [(x, _)] -> x
    tcs <- atomically . newTVar $ restoreDB l qs
    forkIO . forever $ do
        ss <- atomically (signal >> (dump <$> readTVar tcs)) >>= mapM (\(c, (_, ios)) -> fmap ((,) c) ios)
        writeFile (path </> "sessioni") $ show ss
    let sex = "reactivegas_sessione"
    let
        -- new cookie
        new c = do
            sbk@(s, _) <- rs Nothing
            atomically $ do
                cs <- readTVar tcs
                writeTVar tcs (set cs (c, sbk))
            return s
        -- get cookie
        gc = do
            mc <- getCookie sex
            -- lift $ print mc -- debug
            case mc of
                Just c -> return c
                Nothing -> do
                    cn <- lift $ show <$> (randomIO :: IO Int)
                    t <- lift $ getCurrentTime >>= return . (addUTCTime $ 31 * 12 * 60 * 60) -- >>= toCalendarTime
                    let c = newCookie sex cn
                    setCookie $ c{cookieExpires = Just t}
                    return cn
        yes = do
            c <- gc
            r <- lift $ do
                ms <- atomically $ flip query c <$> readTVar tcs
                maybe (new c) (return . fst) ms
            return (r, droppa c)
        no = do
            c <- gc
            r <- lift . new $ c
            return (r, droppa c)
        droppa c = atomically $ readTVar tcs >>= writeTVar tcs . flip forget c

    return (yes, no)
