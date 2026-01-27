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

import Applicazioni.Persistenza (Persistence (..))
import Core.Patch (Group, Patch)
import Core.Types (Utente)
import Lib.Missing (catchRead, untilNothing)

-- | Communication content: a list of group updates
-- and pending user patches for the next update
type Sync = ([Group], [(Utente, Patch)])

-- | Group name identifier
type Name = String

-- | Reads updates from the server starting from a given version
aggiorna
    :: Persistence a b d
    -> Int
    -> IO Sync
aggiorna pe n = do
    let f m = do
            x <- readGroupPatch pe m
            return (x, m + 1)
    xs <- untilNothing n f
    (_, ys) <- readUserPatches pe
    return (xs, ys)

-- | Applies a synchronization to the persistence layer
sincronizza
    :: Persistence a b d
    -> Sync
    -> IO ()
sincronizza pe (xs, ys) = do
    mapM_
        ( \x ->
            writeGroupPatch pe x
                >> readVersion pe
                >>= putStr . ("," ++) . show
                >> hFlush stdout
        )
        xs
    mapM_ (uncurry (writeUserPatch pe)) ys
    putStrLn "\n"

-- | Parser for server requests
read'S :: (Read a) => String -> a
read'S = catchRead "on module Aggiornamento (Server, CGI)"

-- | Server CGI layer wrapping the update operations
serverAggiornamento :: (Name -> IO (Maybe (Persistence a b d))) -> CGI (Maybe CGIResult)
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

-- | Parser for client requests
read'C :: (Read a) => String -> a
read'C = catchRead "on module Aggiornamento (Client, HTTP)"

-- | Client HTTP layer wrapping the update operations.
-- Valid only for a single update/synchronization cycle
clientAggiornamento
    :: Persistence a b d
    -- ^ persistence operations
    -> String
    -- ^ server URL
    -> IO (IO (), IO ())
    -- ^ the two update operations mapped to the server
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
