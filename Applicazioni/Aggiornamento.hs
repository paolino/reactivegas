{-# LANGUAGE ViewPatterns #-}

module Applicazioni.Aggiornamento (Aggiornamento (..), serverAggiornamento, clientAggiornamento) where

import Network.HTTP
import Network.Browser
import Network.URI
import Control.Applicative
import Control.Arrow
import Data.List.Split
import Data.Maybe
import Control.Monad.Trans
import Network.SCGI
import System.IO
import Control.Concurrent.STM


import Lib.Missing (untilNothing, catchRead)
import Applicazioni.Persistenza (Persistenza (..))
import Core.Patch (Group,Patch)
import Core.Types (Utente)

-- | contenuto della comunicazione, una lista di aggiornamenti di gruppo e le patch utente pendenti per il prossimo aggiornamento di gruppo
type Sync = ([Group],[(Utente,Patch)])

-- | operazioni esportate dal modulo
data Aggiornamento = Aggiornamento	
	{	aggiorna :: Int -> IO Sync	-- ^ produce il Sync dall'indice richiesto
	,	sincronizza :: Sync -> IO () 	-- ^ accettazione di un Sync
	} 

-- | un Aggiornamento e' costruito intorno ad una Persistenza, entrambe le operazioni risultano deboli mancando il lock sulle varie parti della persistenza. Andrebbero implementate in STM, anzi tutto il modulo dovrebbe essere esposto in Persistenza.Ma ci sono altri scazzi, tanto che writeGPatch e writeUPatch sono già in IO. Super buggy.
mkAggiornamento :: Persistenza a b d -> Aggiornamento
mkAggiornamento pe = Aggiornamento aggiorna sincronizza where
	aggiorna n = do 
		let f n = do  
			x <- readGPatch pe n
			return (x, n + 1)
		xs <- untilNothing n f
		(_,ys) <- readUPatches pe
		return (xs,ys)
	sincronizza (xs,ys) = do
		mapM_ (\x -> writeGPatch pe x >> readVersion pe >>= putStr . ("," ++) . show >> hFlush stdout) xs
		mapM_ (uncurry (writeUPatch pe)) ys
		putStrLn "\n"

read'S x = catchRead "on module Aggiornamento (Server, CGI)" x

-- | Uno strato Server CGI intorno alle operazioni di aggiornamento
serverAggiornamento :: Persistenza a b d -> CGI (Maybe CGIResult)
serverAggiornamento (mkAggiornamento -> Aggiornamento aggiorna sincronizza) = do
	vs <- getVars
	case lookup "REQUEST_URI" vs of
		Just x -> let xs = tail $ splitOneOf "/?" x in
			case xs of
				("remote":"aggiorna":_) -> do
					n <- maybe 0 read'S <$> getInput "limite"
					Just <$> (lift (aggiorna n) >>= output . show)
				("remote":"sincronizza":_) -> do
					s <- maybe ([],[]) read'S <$> getInput "valore"
					Just <$> (lift (sincronizza s) >>= output . show)
				_ -> return Nothing
		_ -> return Nothing

read'C x = catchRead "on module Aggiornamento (Client, HTTP)" x

-- | Uno strato Client HTTP intorno alle operazioni di aggiornamento. Vale solo per un ciclo di aggiornamento, sicronizzazione
clientAggiornamento 	:: Persistenza a b d 	-- ^ operazioni di persistenza
			-> String		-- ^ URL server 
			-> IO (IO (),IO ())	-- ^ le due operazioni di Aggiornamento mappate sul server
clientAggiornamento pe s = do
		let Aggiornamento aggiorna sincronizza = mkAggiornamento pe
		v <- readVersion pe 
		t <- atomically $  newTVar  v
		let 	agg = do
				putStrLn $ "aggiornamento web da " ++ s ++ " dalla versione " ++ show v
				rsp <- simpleHTTP (getRequest $  s ++ "/remote/aggiorna?limite=" ++ show v)
				read'C <$> getResponseBody rsp >>= sincronizza
				readVersion pe >>= atomically . writeTVar t -- ricorda la versione iniziale
			sinc = do
				v <- atomically $ readTVar t
				si <- aggiorna v
				let fo = Form POST (fromJust . parseURI $ s ++ "/remote/sincronizza") [("valore", show si)]
				simpleHTTP $ formToRequest fo
				return ()
			
		return (agg,sinc)
