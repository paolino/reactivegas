{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Lib.Server.Server (server) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (foldM, forM, liftM2, void)
import Control.Monad.Cont
import Control.Monad.Reader
import Data.List (tails)
import Data.List.Split (splitOneOf)
import Lib.SCGI (CGI, CGIResult, getVars, handleErrors, runSCGIConcurrent')
import Text.XHtml

import Lib.HTTP
import Lib.Missing ((>$>))
import Lib.Passo
import qualified Lib.Passo as P
import Lib.Response
import Lib.STM
import Lib.Server.CGI
import Lib.Server.Core (Form (Form), Value, mkServer, restore)
import Lib.Server.Session

-- | la monade interna all'interazione che ammettiamo.
type Running e = ReaderT e IO

type RPasso e = Passo (Running e) ()

type HRPasso e = HPasso (Running e) ()

rzip :: [a] -> [b] -> [(a, b)]
rzip xs ys = reverse $ zip (reverse xs) (reverse ys)

-- | istanza di Form computata da un HPasso con environment. L'elaborazione prevede il passaggio del passo di radice, quindi, un passo senza storia.
fromHPasso :: forall e. HRPasso e -> e -> Form e Html Link
fromHPasso (p, []) e0 = fromHPasso' ((p, e0), [])
  where
    fromHPasso' :: ((RPasso e, e), [(Value, Running e (RPasso e))]) -> Form e Html Link
    fromHPasso' ((p, e), qs) =
        let
            (h, ml, cont) = runPasso p -- trasformata html
            vs = map fst qs
            pass v =
                cont v >>= \mhp -> return $ do
                    ((p', ps), e') <- runReaderT (liftM2 (,) mhp ask) e
                    return . fromHPasso' $ ((p', e'), rzip (v : vs) ps) -- postulato sul comportamento di Passo.cont
            reload =
                let
                    check k (e, _) vmps@((_, mp) : _) = do
                        (p, e') <- lift $ runReaderT (liftM2 (,) mp ask) e
                        let result = ((p, e'), vmps)
                        case p of
                            Errore _ _ -> k result
                            _ -> return (e', result)
                 in
                    fromHPasso' >$> flip runContT return . callCC $ \k ->
                        fmap snd . foldM (check k) (e0, ((p, e0), [])) . tail . reverse . tails $ qs
         in
            Form
                pass
                reload
                (map fst qs)
                ( \enk fok mb ma ->
                    h (show enk) (show fok) (fmap show mb) (fmap show ma)
                )
                ml
fromHPasso _ _ = error "inizializzazione con contesto non implementata"

checkReset :: CGI a -> CGI a -> CGI a
checkReset reset k = do
    vs <- getVars

    case lookup "REQUEST_URI" vs of
        Just x ->
            let xs = tail $ splitOneOf "/?" x
             in case tail xs of
                    [""] -> reset
                    [] -> reset
                    _ -> k
        _ -> k

server ::
    forall e b k.
    (Read b, Show b) =>
    -- | cartella di lavoro
    FilePath ->
    -- | porta del server scgi
    Int ->
    -- | numero massimo di ricordi per sessione
    Int ->
    -- | numero massimo di sessioni simultanee
    Int ->
    -- | applicazione
    Costruzione (Running e) () () ->
    -- | preserver
    CGI (Maybe CGIResult) ->
    -- | gestore del response
    ([Html] -> CGI CGIResult) ->
    -- | serializzazione delle form di default
    [([Value], Int)] ->
    -- | produzione e restore di evironment per sessione
    (STM () -> Maybe b -> IO (e, k -> IO Bool, IO b)) ->
    (b -> Maybe k) ->
    -- | aloa
    IO ()
server
    path
    (fromIntegral -> port)
    limitR
    limitS
    applicazione
    preServer
    responseHandler
    defaultForms
    newEnvironment
    sessionKey = do
        -- definizione di nuova sessione
        persistSessionChan <- atomically newTChan
        let newSession s = do
                reloadChan <- atomically newTChan
                reloadCond <- condSignal reloadChan
                -- ogni sessione ha la possibilità di avere il suo environment
                (en, cks, ben) <-
                    newEnvironment
                        (writeTChan reloadChan () >> writeTChan persistSessionChan ())
                        s
                -- esplicitazione della definizione di applicazione (runContT)
                (hp :: HRPasso e) <- runReaderT (svolgi applicazione) en
                -- computazione delle forms
                (fs :: [(Form e Html Link, Int)]) <- forM defaultForms $ \(vs, i) -> (, i) <$> restore (fromHPasso hp en) vs
                let reloadAllCond f g = do
                        mk <- sessionKey `fmap` ben
                        t1 <- maybe (return False) cks mk
                        t2 <- atomically reloadCond
                        return $ if t1 || t2 then f else g
                -- boot di un nuovo servizio
                s <- mkServer limitR (reloadAllCond (\y -> thediv ! [strAttr "reload" ""] << y) id) fs
                return (s, ben)

        (run, reset) <- sessioning path limitS (readTChan persistSessionChan) newSession
        putStrLn "** Server attivo"
        runSCGIConcurrent' (void . forkIO) 1000 port . handleErrors $ do
            b <- preServer
            case b of
                Nothing -> checkReset reset run >>= cgiFromServer responseHandler
                Just result -> return result
