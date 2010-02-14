{-# LANGUAGE ScopedTypeVariables #-}
module Lib.HTTP where
import Network.SCGI
import Network
import Text.XHtml
import Debug.Trace
import qualified Lib.Passo as P 
import Lib.Response
import qualified Data.ByteString.Lazy as BS
 
import Control.Concurrent.STM
import Control.Monad.State
import Control.Monad (liftM, forM)
import Data.Maybe (fromJust)
import Data.IORef
import System.Random
import Data.List
 



runPasso :: (Monad m , Show b) => P.Passo m b -> (String -> String ->  Html, String -> m (P.Passo m b))

runPasso (P.Output x c) = let
	k y z = thediv ! [theclass "passobox"] << 
		(thediv ! [theclass "response"] << show x +++ 
			form ! [method "post", action z] << [hidden "chiave" y,  hidden "valore" "undefined", submit "" "Continua .."])
	in (k,\_ -> c )

runPasso (P.Costruito x) =
	(\_ _ -> thediv ! [theclass "passobox"] << 
		(thediv ! [theclass "response"] << (show x +++ anchor ! [href "/costruzione/"] << "riparti")),\_ -> return (P.Costruito x))

runPasso (P.Libero q c) = let 
	k y z = thediv ! [theclass "passobox"] << 
		(thediv ! [theclass "response"] << q +++ 
			form ! [method "post", action z] << 
				( [textfield "valore"] +++ [hidden "chiave" y, submit "" "Continua .."]) )
	parse x = case reads x of
		[] -> case reads $ "\"" ++ x ++ "\"" of 
			[] -> return $ P.Output (ResponseOne "valore non accettabile") (return $ P.Libero q c)
			[(x',_)] -> c x'
		[(x',_)] -> c x'
	in (k,parse)
runPasso (P.Scelta q xs c) = let 
	k y z = thediv ! [theclass "passobox"] << 
		(thediv ! [theclass "response"] << q +++ 
			form ! [method "post", action z] << 
				( map (\(x,_) -> radio' "valore" x << x +++ br) xs +++ 
					[hidden "chiave" y, submit "" "Continua .."]) )
	resp x = case lookup x xs of
			Nothing -> return $ P.Output (ResponseOne "errore interno, riprova") (return $ P.Scelta q xs c)
			Just y -> c y
	in (k,resp)
	
runPasso (P.DaFile q c) = let
	k y z = thediv ! [theclass "passobox"] << 
		(thediv ! [theclass "response"] << q +++ 
			form ! [method "post", enctype "multipart/form-data"] << 
				( [afile "valore"] +++ [hidden "chiave" y, submit "" "Continua .."]) )
	parse x = case reads x of
		[] -> return $ P.Output (ResponseOne "valore non accettabile") (return $ P.Libero q c)
		[(x',_)] -> c x'
	in (k,parse)

radio' n v = tag "input" ! [thetype "radio",value v,identifier n,name n]
 
-- cgiMain :: TVar [(String, TVar [(String , String -> IO (P.Passo IO ()))])] -> CGI CGIResult
cgiMain
  :: (Monad m) 
		=>	TVar [(String, TVar (s, [(String, String -> m (P.Passo m ()))]))]
		-> TVar (m (P.Passo m ()),s, m (P.Passo m ()) -> StateT s IO (P.Passo m ()))
		-> CGI CGIResult

cgiMain s t = do
	(i0,s0,z) <- liftIO . atomically $ readTVar t
	getInputs >>= liftIO . print
	getVars >>= liftIO . print
	u <- fromJust `fmap` getVar "REQUEST_URI" 
	if  "/costruzione/" `isPrefixOf` u then do 
		let sessione = drop (length "/costruzione/") u
		mn <- getInput "chiave"
		p <- show `fmap` (liftIO $ (randomIO :: IO Int))
		case mn of 
			Nothing -> do
				key <- liftIO $ show `fmap` (randomIO :: IO Int)
				((o,c),s1) <- liftIO . flip runStateT s0 $ runPasso `fmap` z i0
				liftIO $ atomically $ do 
					ls <- readTVar s
					tc <- newTVar (s1,[(p,c)])
					writeTVar s (take 1000 $ (key,tc) : ls)
				output $ prettyHtml $ o key ("/costruzione/" ++ p)
			Just key -> do
				q  <- getInput "valore"
				case q of 
					Nothing -> outputNotFound "valore non reperito"
					Just q ->  do 
						mc <- liftIO . atomically $ do
							ls <- readTVar s
							case lookup key ls of
								Nothing -> return Nothing
								Just tc -> Just `fmap` readTVar tc
						case mc of 
							Nothing -> outputNotFound "chiave non reperita"
							Just (s1,cs) -> do
								case lookup sessione cs of 
									Nothing ->  outputNotFound "sessione non reperita"
									Just c -> do 
										((o,c),s2) <- liftIO . flip runStateT s1 $ runPasso `fmap` z (c q)
										liftIO $ atomically $ do 
											ls <- readTVar s
											tc <- newTVar $ (s2,take 1000 $ (p,c):dropWhile ((/=sessione) . fst) cs)
											writeTVar s (take 1000 $ (key,tc) : filter ((/=) key . fst) ls)
										output $ prettyHtml $ o key ("/costruzione/" ++ p)
 		else outputNotFound "l'applicazione non è stata trovata"

server t = do
	s <- atomically (newTVar [])
	runSCGI  (PortNumber $ fromIntegral 5000)  $ handleErrors (cgiMain s t)
