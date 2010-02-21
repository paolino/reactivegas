{-# LANGUAGE ScopedTypeVariables #-}
module Lib.ServerHTTP  where
import Data.List (splitAt)
import Data.Maybe (fromJust, isNothing, listToMaybe)
import qualified Data.Array as A ((!)) 
import Control.Concurrent.STM
import Control.Monad.Cont (lift, runContT,callCC)
import Control.Applicative ((<$>))
import Control.Monad (liftM, forM, foldM)
import Control.Monad.Error (MonadError, ErrorT, throwError, runErrorT)
import Control.Monad.Maybe
import Control.Monad.Reader
import System.Random
import Network.SCGI
import Network
import Text.XHtml

import Debug.Trace
import Control.Monad
import Lib.HTTP
import Lib.Passo hiding (output)
import Lib.Response
import Lib.LimitedTree
import qualified Lib.Passo as P

-------------------------------- library ------------------------------------
onNothing :: MonadError e m => e -> Maybe a -> m a
onNothing e x = if isNothing x then throwError e else return . fromJust $ x
------------------------------------------------------------------------------

type HistoryKey = String
type FormKey = String  -- chiave di selezione delle form di interazione
type Value = String

data FormPoint m = FormPoint 	
	{continuazione :: Value -> Maybe (IO (HistoryKey,FormPoint m ))
	,context :: [m (Passo m ())]
	,prompt :: FormKey -> Html
	,link :: Maybe Link
	}
instance Show (FormPoint m)where
	show _ = "FormPoint"

mkFormPoint :: Monad m => (m (HPasso m ()) -> IO (HPasso m ())) -> HPasso m () -> IO (HistoryKey,FormPoint m)
mkFormPoint f  (p,ctx) = let
	(h,ml,cont) = runPasso p -- trasformata html
	pass = cont >=> \a -> return $ f a >>= mkFormPoint f 
	in do 
		key <- show <$> (randomIO  :: IO Int)
		return (key,FormPoint pass ctx (h key) ml)
				
data Form m = Form 
	{history :: [(HistoryKey, FormPoint m)]
	,limit :: Int
	} deriving Show
type Risposta = (HistoryKey, Value) -- risposta di una interazione

applica' :: Risposta -> TVar (Form m) -> ErrorT String IO (FormKey -> Html)
applica' (i,v) tps =  do
		Form hs l <- lift . atomically $ readTVar tps
		liftIO . print $ map (length . context . snd) hs
		
		FormPoint c _ _ _ <- onNothing "indice della form fuori dai limiti" $ lookup i hs
		(k,fp) <- onNothing  "il valore risposto non è accettabile" (c v) >>= lift
		lift . atomically . writeTVar tps $ Form (take l $ (k,fp) : hs) l
		return (prompt fp) 		

serve' :: HistoryKey -> TVar (Form m) -> ErrorT String IO Link
serve' i tps = do
	Form hs l <- lift . atomically $ readTVar tps
	FormPoint _ _ _ l <- onNothing "indice della form fuori dai limiti" $ lookup i hs
	onNothing "questo indice non ha un valore da scaricare associato" l

data Sessione m = Sessione
	{	azione :: HPasso m ()
	,	forms :: TVar [(FormKey, TVar (Form m))]
	}
mkSessione :: HPasso m () -> IO (Sessione m)
mkSessione hp = do
	forms <- atomically $ newTVar []
	return $ Sessione hp forms 

newForm ::Monad m => (m (HPasso m ()) -> IO (HPasso m ())) ->  Sessione m -> Maybe FormKey -> IO FormKey
newForm f (Sessione hp tfs) Nothing = do
	key <- show <$> (randomIO  :: IO Int)
	fp <- mkFormPoint f hp

	atomically $ do
		fs <- readTVar tfs
		tfp <- newTVar $ Form [fp] 30
		writeTVar tfs $ (key,tfp) : fs
	return key

newForm f s@(Sessione hp tfs) (Just clone) = do
	key <- show <$> (randomIO  :: IO Int)
	c <- atomically $ do
		fs <- readTVar tfs
		case lookup clone fs of
			Just tfp -> do
				fp <- readTVar tfp
				tfp' <- newTVar fp
				writeTVar tfs $ (key,tfp') : fs
				return False
			Nothing -> return True
	if c then newForm f s Nothing else return key

pickForm :: Sessione m -> FormKey -> ErrorT String IO (TVar (Form m))
pickForm (Sessione hp tfs) fk = 
	(lift . atomically) (readTVar tfs) >>= onNothing "chiave dell'interazione non trovata" . lookup fk  

applica :: Sessione m -> FormKey -> Maybe Risposta -> ErrorT String IO Html
applica s  fk res =  do 
	form <- pickForm s fk 	
	($fk) <$> case res of 
		Nothing -> prompt <$> snd <$> head <$> history <$> lift (atomically $ readTVar form) 
		Just iv -> applica' iv form

runApplica :: ErrorT String IO Html -> CGI CGIResult
runApplica k = lift (runErrorT k) >>= either outputNotFound (output . prettyHtml)

serve :: Sessione m -> FormKey -> HistoryKey -> ErrorT String IO Link
serve s fk i = pickForm s fk >>= serve' i 

runServe :: ErrorT String IO Link -> CGI CGIResult
runServe k = lift (runErrorT k) >>= either outputNotFound r where
	r (Link n x m) = do
		setHeader "Content-type" m
		setHeader "Content-Disposition" "attachment"
		setHeader "Content-Disposition" $ "filename=" ++ show n
		output x 

readInputs :: ErrorT String (CGIT IO) (HistoryKey,Value)
readInputs = do 
	hf <- lift (getInput "hkey") >>= onNothing "form corrotta, manca la chiave di punto"
	v <- lift (getInput "valore") >>= onNothing "form corrotta, manca la risposta"
	return (hf,v)


				

singleFormServer :: Monad m => (m (HPasso m ()) -> IO (HPasso m ())) -> HPasso m () -> IO (CGI CGIResult)
singleFormServer f hp = do
	s@(Sessione _ tfp) <- mkSessione hp
	fk <- newForm f s Nothing
	return $ do 
		r <- runErrorT $ do
			vs <- lift $ getVars 
			case lookup "REQUEST_URI"  vs of 
				Just "/interazione" -> do 
					(i,v) <- readInputs
					lift . runApplica . applica s fk $ Just (i,v)
				Just "/download" -> do
					(i,_) <- readInputs
					lift . runServe $ serve s fk i
				_ -> lift . runApplica $ applica s fk Nothing
		either outputNotFound return r

 
		
		



server
  :: (Monad m) =>
     (m (HPasso m ()) -> IO (HPasso m ())) -> HPasso m () -> IO ()

server f interazione = do
	service <- singleFormServer f interazione
	runSCGI  (PortNumber $ fromIntegral 5000)  $ handleErrors service


unIterazione = flip runReaderT ()
interazione :: Costruzione (ReaderT () IO) () () 
interazione = rotonda $ \k -> do
		P.output (ResponseOne "benvenuto")
		(x :: Int) <- upload "un file contenente lo show di un numero" 
		P.output (ResponseOne ("bel numero :" ++ show x))
		download "il numero uno.txt" x
		t <- scelte [("si",True),("no",False)] "fine?"
		if t then k () else return ()

{-
-}
