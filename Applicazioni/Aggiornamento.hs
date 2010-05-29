module Applicazioni.Aggiornamento (Aggiornamento (..), mkAggiornamento, checkAggiornamento, getAggiornamento) where

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
import Applicazioni.Persistenza (Persistenza (..))
import Core.Patch
import Core.Types

type Sync = ([Group],[(Utente,Patch)])
data Aggiornamento = Aggiornamento	
	{	aggiorna :: Int -> IO Sync	-- ^ dal limite inferiore degli aggiornamenti di gruppo 
	,	sincronizza :: Sync -> IO ()
	} 
untilNothing :: (Monad m, Functor m) =>  b -> (b -> m (Maybe a, b)) -> m [a]
untilNothing r f = do
	(x,r') <- f r
	case x of
		Just y -> (y:) `fmap` untilNothing r' f
		Nothing -> return []

mkAggiornamento :: Persistenza a b -> Aggiornamento
mkAggiornamento pe = Aggiornamento {
	aggiorna = \n -> do 
		let f n = do  
			x <- readGPatch pe n
			return (x, n + 1)
		xs <- untilNothing n f
		(_,ys) <- readUPatches pe
		return (xs,ys)
	,sincronizza = \(xs,ys) -> do
		mapM_ (\x -> writeGPatch pe x >> readVersion pe >>= putStr . ("," ++) . show >> hFlush stdout) xs
		mapM_ (uncurry (writeUPatch pe)) ys
		putStrLn "\n"
		
	}

checkAggiornamento :: Aggiornamento -> CGI (Maybe CGIResult)
checkAggiornamento (Aggiornamento aggiorna sincronizza)= do
	vs <- getVars
	case lookup "REQUEST_URI" vs of
		Just x -> let xs = tail $ splitOneOf "/?" x in
			case xs of
				("remote":"aggiorna":_) -> do
					n <- maybe 0 read <$> getInput "limite"
					Just <$> (lift (aggiorna n) >>= output . show)
				("remote":"sincronizza":_) -> do
					s <- maybe ([],[]) read <$> getInput "valore"
					Just <$> (lift (sincronizza s) >>= output . show)
				_ -> return Nothing
		_ -> return Nothing

getAggiornamento :: Persistenza a b  -> String -> IO (IO (),IO ())
getAggiornamento pe s = do
		let Aggiornamento aggiorna sincronizza = mkAggiornamento pe
		v <- readVersion pe 
		t <- atomically $  newTVar  v
		let 	agg = do
				putStrLn $ "aggiornamento web da " ++ s ++ " dalla versione " ++ show v
				rsp <- simpleHTTP (getRequest $  s ++ "/remote/aggiorna?limite=" ++ show v)
				read <$> getResponseBody rsp >>= sincronizza
				readVersion pe >>= atomically . writeTVar t 
			sinc = do
				v <- atomically $ readTVar t
				si <- aggiorna v
				let fo = Form POST (fromJust . parseURI $ s ++ "/remote/sincronizza") [("valore", show si)]
				simpleHTTP $ formToRequest fo
				return ()
			
		return (agg,sinc)
