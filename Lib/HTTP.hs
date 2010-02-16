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
 

renderResponse k x@(ResponseOne _) =   thediv ! [theclass k] << show x
renderResponse k (ResponseMany xs) = thediv ! [theclass k] <<  ulist << concatHtml (map  ((li <<) .  show ) xs) 
renderResponse k (ResponseAL xs) = thediv ! [theclass k] << dlist 
		<< concatHtml (map  (\(x,y) -> dterm << x +++ ddef << show y) xs) 
renderResponse k (Response xs) = thediv ! [theclass k] << dlist << 
		concatHtml (map  (\(x,y) -> dterm << x +++ ddef << renderResponse k y) xs)

runPasso :: (Monad m , Show b) => P.Passo m b -> (String -> String ->  Html, [(String,(String,String))] , String -> m (P.Passo m b))

runPasso (P.Output x c) = let
	k y z = thediv ! [theclass "passobox"] << 
		(renderResponse "responso" x +++ form ! [method "post", action ("/costruzione/" ++ z)] << [hidden "chiave" y,  hidden "valore" "undefined", submit "" "Continua .."])
	in (k, [],\_ -> c )

runPasso (P.Errore x c) = let
	k y z = thediv ! [theclass "passobox"] << 
		(renderResponse "errore" x +++ 
			form ! [method "post", action ("/costruzione/" ++ z)] << [hidden "chiave" y,  hidden "valore" "undefined", submit "" "Continua .."])
	in (k, [],\_ -> c )

runPasso (P.Costruito x) =
	(\_ _ -> thediv ! [theclass "passobox"] << 
		(thediv ! [theclass "response"] << (show x +++ anchor ! [href "/costruzione/"] << "riparti"))
		, []
		,\_ -> return (P.Costruito x))

runPasso (P.Libero q c) = let 
	k y z = thediv ! [theclass "passobox"] << 
		(thediv ! [theclass "response"] << q +++ 
			form ! [method "post", action ("/costruzione/" ++ z)] << 
				( [textfield "valore"] +++ [hidden "chiave" y, submit "" "Continua .."]) )
	parse x = case reads x of
		[] -> case reads $ "\"" ++ x ++ "\"" of 
			[] -> return $ P.Errore (ResponseOne "valore non accettabile") (return $ P.Libero q c)
			[(x',_)] -> c x'
		[(x',_)] -> c x'
	in (k, [],parse)
runPasso (P.Scelta q xs c) = let 
	k y z = thediv ! [theclass "passobox"] << 
		(thediv ! [theclass "response"] << q +++ 
			form ! [method "post", action ("/costruzione/" ++ z)] << 
				( map (\(x,_) -> radio' "valore" x << x +++ br) xs +++ 
					[hidden "chiave" y, submit "" "Continua .."]) )
	resp x = case lookup x xs of
			Nothing -> return $ P.Errore (ResponseOne "errore interno, riprova") (return $ P.Scelta q xs c)
			Just y -> c y
	in (k, [],resp)
	
runPasso (P.Upload q c) = let
	k y z = thediv ! [theclass "passobox"] << 
		(thediv ! [theclass "response"] << q +++ 
			form ! [method "post", action ("/costruzione/" ++ z), enctype "multipart/form-data"] << 
				( [afile "valore"] +++ [hidden "chiave" y, submit "" "Continua .."]) )
	parse x = case reads x of
		[] -> return $ P.Errore (ResponseOne "valore non accettabile") (return $ P.Libero q c)
		[(x',_)] -> c x'
	in (k, [],parse)


runPasso (P.Download q x c) = let
	k y z = thediv ! [theclass "passobox"] << 
		(thediv ! [theclass "download"] << [anchor ! [href ("/costruzione/download/" ++ q)] << "Scarica il dato"] +++ 
				form ! [method "post", action ("/costruzione/" ++ z)] 
					<< [hidden "chiave" y,hidden "valore" "undefined", submit "" "Continua .."]) 
	in (k,[(q,(show x,"application/chiavi"))],\_ -> c)
-------------------------------------------------------------------------------------------------------
radio' n v = tag "input" ! [thetype "radio",value v,identifier n,name n]
--------------------------------------------------------------------------------------------------
-- cgiMain :: TVar [(String, TVar [(String , String -> IO (P.Passo IO ()))])] -> CGI CGIResult
cgiMain
  :: (Monad m) 
		=> TVar [(String, TVar [(String, String -> m (P.Passo m ()))])]
		-> TVar (m (P.Passo m ()), m (P.Passo m ()) -> IO (P.Passo m ()))
		-> TVar [(String,(String,String))]
		-> CGI CGIResult

-- pagina :: String -> Html
cgiMain s t l = do
	(i0,z) <- liftIO . atomically $ readTVar t
	-- getVars >>= liftIO . print
	ls <- liftIO . atomically $ readTVar l
	u <- (urlDecode .  fromJust) `fmap` getVar "REQUEST_URI" 
	liftIO . putStrLn $ "\n" ++ u
	-- getInputs >>= liftIO . print
	if  "/costruzione/" `isPrefixOf` u then do 
		let sessione = drop (length "/costruzione/") u
		if "download/" `isPrefixOf` sessione then do
			let di = drop (length "download/") sessione
			case lookup di ls of
				Nothing -> trace (show ls) $ outputNotFound "download di un valore inesistente"
				Just (sv,tv) -> do
					setHeader "Content-type" tv
					output sv
			else do
				mn <- getInput "chiave" -- selettore di form
				p <- show `fmap` (liftIO $ (randomIO :: IO Int))
				case mn of 
					Nothing -> do
						key <- liftIO $ show `fmap` (randomIO :: IO Int)
						(o,ls,c) <- liftIO  $ runPasso `fmap` z i0
						liftIO $ atomically $ do 
							(take 100 . (ls ++)) `fmap` readTVar l >>= writeTVar l 
							tc <- newTVar [(p,c)]
							qs <- readTVar s
							writeTVar s (take 1000 $ (key,tc) : qs)
						output $ prettyHtml $ o key p
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
									Just cs -> do
										case lookup sessione cs of 
											Nothing ->  outputNotFound "sessione non reperita"
											Just c -> do 
												(o,ls,c) <- liftIO $ runPasso `fmap` z (c q)
												liftIO $ atomically $ do 
													(take 100 . (ls ++)) `fmap` readTVar l >>= writeTVar l 
													ls <- readTVar s
													tc <- newTVar $ (take 1000 $ (p,c):dropWhile ((/=sessione) . fst) cs)
													writeTVar s (take 1000 $ (key,tc) : filter ((/=) key . fst) ls)
												output $ prettyHtml $ o key p
 				else outputNotFound "l'applicazione non è stata trovata"

server
  :: (Monad m) =>
     TVar
       (m (P.Passo m ()),   -- ^ interazione
        m (P.Passo m ()) -> IO (P.Passo m ())) -- ^ desugarer
     -> IO ()

server t = do
	s <- atomically (newTVar []) -- forms
	l <- atomically (newTVar []) -- downloads
	runSCGI  (PortNumber $ fromIntegral 5000)  $ handleErrors (cgiMain s t l)
---------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
interazione :: P.Costruzione IO () () 
interazione = P.rotonda $ \k -> do
		P.output (ResponseOne "benvenuto")
		(x :: Int) <- P.upload "un file contenente lo show di un numero" 
		P.output (ResponseOne ("bel numero :" ++ show x))
		P.download "il numero uno.txt" x
		t <- P.scelte [("si",True),("no",False)] "fine?"
		if t then k () else return ()

prova = let 
		in do
		t <- atomically $ newTVar (P.svolgi interazione, id)
		server t


