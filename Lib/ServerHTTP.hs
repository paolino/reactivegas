{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}
module Lib.ServerHTTP  where
import Data.List (splitAt)
import Data.Maybe (fromJust, isNothing, listToMaybe)
import qualified Data.Array as A ((!)) 
import Control.Concurrent.STM
import Control.Monad.Cont (lift, runContT,callCC)
import Control.Applicative ((<$>))
import Control.Arrow (second)
import Control.Monad (liftM, forM, foldM)
import Control.Monad.Error (MonadError, ErrorT, throwError, runErrorT)
import Control.Monad.Maybe
import Control.Monad.Reader
import System.Random
import Network.SCGI
import Network
import Text.XHtml hiding (link)
import qualified Text.XHtml as H
import Debug.Trace
import Control.Monad
import Lib.Assocs (secondM)
import Lib.HTTP
import Lib.Passo hiding (output)
import Lib.Response
import qualified Lib.Passo as P

-------------------------------- library ------------------------------------
onNothing :: MonadError e m => e -> Maybe a -> m a
onNothing e x = if isNothing x then throwError e else return . fromJust $ x
------------------------------------------------------------------------------

type HistoryKey = String
type FormKey = String  -- chiave di selezione delle form di interazione
type Value = String

type Env a = ReaderT a IO


data FormPoint a = FormPoint 	
	{continuazione :: Value -> Maybe (IO (HistoryKey,FormPoint a ))
	,context :: [Env a (Passo (Env a) ())]
	,environment :: a
	,prompt :: FormKey -> Html
	,link :: Maybe Link
	}
instance Show (FormPoint a)where
	show _ = "FormPoint"

mkFormPoint :: (HPasso (Env a) (),a) -> IO (HistoryKey,FormPoint a)
mkFormPoint ((p,ctx),env) = let
	(h,ml,cont) = runPasso p -- trasformata html
	pass = cont >=> \a -> return $ runReaderT (liftM2 (,) a ask) env >>= mkFormPoint 
	in do	key <- show <$> (randomIO  :: IO Int)
		return (key,FormPoint pass ctx env (h key) ml)
reloadFormPoint :: a -> FormPoint a -> IO (HistoryKey, FormPoint a)
reloadFormPoint env (FormPoint _ ctx _ _ _) = do
	(p,env') <- foldM (\(_,env) ma -> runReaderT (liftM2 (,) ma ask) env) (undefined,env) $ reverse ctx
	mkFormPoint ((p,ctx),env')

data Form a = Form 
	{history :: [(HistoryKey, FormPoint a)]
	,limit :: Int
	} deriving Show

type Risposta = (HistoryKey, Value) -- risposta di una interazione

applica' :: Risposta -> TVar (Form a) -> ErrorT String IO (FormKey -> Html)
applica' (i,v) tps =  do
		Form hs l <- lift . atomically $ readTVar tps
		liftIO . print $ map (length . context . snd) hs
		
		(continuazione -> c) <- onNothing "indice della form fuori dai limiti" $ lookup i hs
		(k,fp) <- onNothing  "il valore risposto non è accettabile" (c v) >>= lift
		lift . atomically . writeTVar tps $ Form (take l $ (k,fp) : hs) l
		return (prompt fp) 		

serve' :: HistoryKey -> TVar (Form a) -> ErrorT String IO Link
serve' i tps = do
	Form hs l <- lift . atomically $ readTVar tps
	(link -> l) <- onNothing "indice della form fuori dai limiti" $ lookup i hs
	onNothing "questo indice non ha un valore da scaricare associato" l

data Sessione a = Sessione
	{	base  :: (HPasso (Env a) (),a)
	,	forms :: TVar [(FormKey, TVar (Form a))]
	}
mkSessione :: a -> HPasso (Env a) () -> IO (Sessione a)
mkSessione env hp = do
	forms <- atomically $ newTVar []
	let s = Sessione (hp,env) forms 
	newForm s Nothing	
	return s
newForm :: Sessione a -> Maybe FormKey -> IO ()
newForm (Sessione hpa tfs) Nothing = do
	key <- show <$> (randomIO  :: IO Int)
	fp <- mkFormPoint hpa
	atomically $ do
		fs <- readTVar tfs
		tfp <- newTVar $ Form [fp] 30
		writeTVar tfs $ (key,tfp) : fs

newForm s@(Sessione _ tfs) (Just clone) = do
	key <- show <$> (randomIO  :: IO Int)
	c <- atomically $ do
		fs <- readTVar tfs
		case lookup clone fs of
			Just tfp -> do
				fp <- readTVar tfp
				tfp' <- newTVar fp
				writeTVar tfs . take 10 $ (key,tfp') : fs
				return False
			Nothing -> return True
	when c $ newForm s Nothing 

deleteForm s@(Sessione _ tfs) fk = atomically $ readTVar tfs >>= 
	\fs -> when (not $ null fs) . writeTVar tfs . filter ((/=) fk . fst) $ fs

dropPoint s@(Sessione _ tfs) fk = atomically $ do 
	fs <- readTVar tfs 
	case lookup fk fs of
		Just tfp -> do
			Form hs l <- readTVar tfp
			when (length hs > 1) . writeTVar tfp $ Form (tail hs) l
		Nothing -> return ()

ricarica  s@(Sessione (_,env) tfs) fk hk = join . atomically $ do
	fs <- readTVar tfs 
	case lookup fk fs of
		Just tfp -> do
			Form hs l <- readTVar tfp
			return $ case  lookup hk hs of
				Nothing -> return () 
				Just point -> case context point of 
					[] -> return ()
					_ ->  do 
						hfp <- reloadFormPoint env point 
						atomically . writeTVar tfp $ 
							Form (take 30 $ hfp : hs) l
				Nothing -> return ()
		Nothing -> return (return ())
					
pickForm :: Sessione a -> FormKey -> ErrorT String IO (TVar (Form a))
pickForm (Sessione _ tfs) fk = 
	(lift . atomically) (readTVar tfs) >>= onNothing "chiave dell'interazione non trovata" . lookup fk  

applica :: Sessione a -> FormKey -> Maybe Risposta -> ErrorT String IO Html
applica s  fk res =  do 
	form <- pickForm s fk 	
	($fk) <$> case res of 
		Nothing -> prompt <$> snd <$> head <$> history <$> lift (atomically $ readTVar form) 
		Just iv -> applica' iv form

serve :: Sessione a -> FormKey -> HistoryKey -> ErrorT String IO Link
serve s fk i = pickForm s fk >>= serve' i 


runServe :: ErrorT String IO Link -> CGI CGIResult
runServe k = lift (runErrorT k) >>= either outputNotFound r where
	r (Link n x m) = do
		setHeader "Content-type" m
		setHeader "Content-Disposition" "attachment"
		setHeader "Content-Disposition" $ "filename=" ++ show n
		output x 

readHKey :: ErrorT String (CGIT IO) HistoryKey
readHKey = lift (getInput "hkey") >>= onNothing "form corrotta, manca la chiave di punto"
readFKey :: ErrorT String (CGIT IO) FormKey
readFKey =  lift (getInput "fkey") >>= onNothing "form corrotta, manca la chiave di interazione"
readValore :: ErrorT String (CGIT IO) Value
readValore = lift (getInput "valore") >>= onNothing "form corrotta, manca la risposta"


serveSessione :: forall a . Sessione a -> 	CGI CGIResult
serveSessione s@(Sessione _ tfs) = do
	(fps :: [(FormKey, FormKey -> Html)]) <- lift . atomically $ do
		fs <- readTVar tfs >>= mapM (secondM readTVar) :: STM [(FormKey,Form a)]
		return $ map (second (prompt . snd . head . history)) fs
	output . prettyHtml $ 
		header << (thelink ! [rel "stylesheet", href "/style.css", thetype "text/css"] << noHtml 
			+++ script ! [strAttr "language" "Javascript", src "/function.js"] << noHtml)
				+++ (body ! [strAttr "onload" "rewrite()"] << 
					map (\(k,h) -> thediv ! [theclass "interazione"] << h k) fps)

		
runApplica :: Sessione a -> ErrorT String IO Html -> CGI CGIResult
runApplica s k = lift (runErrorT k) >>= either outputNotFound (\_ -> serveSessione s)

cookieing :: IO b -> a -> HPasso (Env (a,b)) () -> TVar [(String,Sessione (a,b))] -> CGI (Sessione (a,b))
cookieing senv env hp ms = do
	mc <- getCookie "sessione"
	c <- case mc of 
		Just c -> do	lift $ print c 
				return c
		Nothing -> do 
			cn <- lift $ show <$> (randomIO :: IO Int)
			setCookie $ newCookie "sessione" cn
			return cn
	lift $ do
		b <- senv
		s0 <- mkSessione (env,b) hp
		atomically $  do
			ms' <- readTVar ms
			case lookup c ms' of
				Just s -> return s
				Nothing -> writeTVar ms (take 50 $ (c,s0):ms') >> return s0

mkServer ::  IO b -> a -> HPasso (Env (a,b)) () -> IO (CGI CGIResult)
mkServer senv env hp = do
	ms <- atomically $ newTVar [] -- le sessioni	
	return $ do 
		s <- cookieing senv env hp ms 
		vs <- getVars 
		r <- runErrorT $ do
			case lookup "REQUEST_URI"  vs of 
				Just "/style.css" -> do
					css <- liftIO $ readFile "style.css"   
					lift $ setHeader "Content-type" "text/css"
					lift $ output css
				Just "/function.js" -> do
					css <- liftIO $ readFile "function.js"   
					lift $ setHeader "Content-type" "text/js"
					lift $ output css
				Just "/interazione" -> do 
					hk <- readHKey
					fk <- readFKey
					v <- readValore
					lift . runApplica s . applica s fk $ Just (hk,v)
				Just "/download" -> do
					hk <- readHKey
					fk <- readFKey
					lift . runServe $ serve s fk hk
				Just "/menu" -> do
					hk <- readHKey
					fk <- readFKey
					v <- readValore
					lift . lift $ case v of 
						"chiudi" -> deleteForm s fk	
						"clona" -> newForm s (Just fk)
						"indietro" -> dropPoint s fk  
						"ricarica" -> ricarica s fk hk
					lift $ serveSessione s
				_ -> lift $ serveSessione s 
		either (\_ -> serveSessione s) return r



server :: IO b -> a -> Costruzione (Env (a,b)) () () -> IO ()

server senv env applicazione = do
	b <- senv
	interazione <- runReaderT (svolgi applicazione) (env,b)
	service <- mkServer senv env interazione
	runSCGI  (PortNumber $ fromIntegral 5000)  $ handleErrors service


interazione :: Costruzione (Env ((),())) () () 
interazione = rotonda $ \k -> do
		P.output (ResponseOne "benvenuto")
		(x :: Int) <- upload "un file contenente lo show di un numero" 
		P.output (ResponseOne ("bel numero :" ++ show x))
		download "il numero uno.txt" x
		t <- scelte [("si",True),("no",False)] "fine?"
		if t then k () else return ()
