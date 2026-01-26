module Applicazioni.Aggiornamento
    ( serverAggiornamento
    , clientAggiornamento
    ) where

import Control.Concurrent.STM
    ( atomically
    , newTVarIO
    , readTVarIO
    , writeTVar
    )
import Control.Monad (void)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.List.Split (splitOneOf)
import Data.Maybe (fromJust)
import Lib.SCGI
    ( CGI
    , CGIResult
    , getInput
    , getVars
    , output
    )
import Network.Browser (Form (..), formToRequest)
import Network.HTTP
    ( RequestMethod (..)
    , getResponseBody
    , simpleHTTP
    )
import Network.HTTP.Base (getRequest)
import Network.URI (parseURI)
import System.IO (hFlush, stdout)

import Applicazioni.Persistenza (Persistenza (..))
import Core.Patch (Group, Patch)
import Core.Types (Utente)
import Lib.Missing (catchRead, untilNothing)

-- | Contenuto della comunicazione, una lista di aggiornamenti
-- di gruppo e le patch utente pendenti per il prossimo aggiornamento
type Sync = ([Group], [(Utente, Patch)])

-- | Nome del gruppo
type Name = String

-- | Legge gli aggiornamenti dal server a partire da una versione
aggiorna
    :: Persistenza a b d
    -> Int
    -> IO Sync
aggiorna pe n = do
    let f m = do
            x <- readGPatch pe m
            return (x, m + 1)
    xs <- untilNothing n f
    (_, ys) <- readUPatches pe
    return (xs, ys)

-- | Applica una sincronizzazione alla persistenza
sincronizza
    :: Persistenza a b d
    -> Sync
    -> IO ()
sincronizza pe (xs, ys) = do
    mapM_
        ( \x ->
            writeGPatch pe x
                >> readVersion pe
                >>= putStr . ("," ++) . show
                >> hFlush stdout
        )
        xs
    mapM_ (uncurry (writeUPatch pe)) ys
    putStrLn "\n"

-- | Parser per richieste server
read'S :: (Read a) => String -> a
read'S = catchRead "on module Aggiornamento (Server, CGI)"

-- | Uno strato Server CGI intorno alle operazioni di aggiornamento
serverAggiornamento :: (Name -> IO (Maybe (Persistenza a b d))) -> CGI (Maybe CGIResult)
serverAggiornamento persistenze = do
    vs <- getVars
    case lookup "REQUEST_URI" vs of
        Just x ->
            let xs = tail $ splitOneOf "/?" x
             in case xs of
                    ("remote" : "aggiorna" : _) -> do
                        n <- maybe 0 read'S <$> getInput "limite"
                        runMaybeT $ do
                            g <- MaybeT $ getInput "gruppo"
                            pe <- MaybeT $ persistenze g
                            lift (lift (aggiorna pe n) >>= output . show)
                    ("remote" : "sincronizza" : _) -> do
                        s <- maybe ([], []) read'S <$> getInput "valore"
                        runMaybeT $ do
                            g <- MaybeT (getInput "gruppo")
                            pe <- MaybeT $ persistenze g
                            lift (lift (sincronizza pe s) >>= output . show)
                    _ -> return Nothing
        _ -> return Nothing

-- | Parser per richieste client
read'C :: (Read a) => String -> a
read'C = catchRead "on module Aggiornamento (Client, HTTP)"

-- | Uno strato Client HTTP intorno alle operazioni di aggiornamento.
-- Vale solo per un ciclo di aggiornamento, sicronizzazione
clientAggiornamento
    :: Persistenza a b d
    -- ^ operazioni di persistenza
    -> String
    -- ^ URL server
    -> IO (IO (), IO ())
    -- ^ le due operazioni di Aggiornamento mappate sul server
clientAggiornamento pe s = do
    v <- readVersion pe
    t <- newTVarIO v
    let agg = do
            putStrLn $
                "aggiornamento web da "
                    ++ s
                    ++ " dalla versione "
                    ++ show v
            rsp <- simpleHTTP (getRequest $ s ++ "/remote/aggiorna?limite=" ++ show v)
            getResponseBody rsp >>= sincronizza pe . read'C
            readVersion pe >>= atomically . writeTVar t
        sinc = do
            v' <- readTVarIO t
            si <- aggiorna pe v'
            let fo =
                    Form
                        POST
                        (fromJust . parseURI $ s ++ "/remote/sincronizza")
                        [("valore", show si)]
            void $ simpleHTTP (formToRequest fo)
    return (agg, sinc)
