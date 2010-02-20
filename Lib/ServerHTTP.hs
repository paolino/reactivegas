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


type FormKey = String  -- chiave di selezione delle form di interazione
type Value = String

data FormPoint = FormPoint 	
	{continuazione :: Value -> Maybe (IO (Int,FormPoint))
	,prompt :: FormKey -> Html
	,link :: Maybe Link
	}
instance Show FormPoint where
	show _ = "FormPoint"

mkFormPoint :: HPasso IO () -> (Int,FormPoint)
mkFormPoint (p,i) = let
	(h,ml,cont) = runPasso p -- trasformata html
	pass = cont >=> \a -> return $ mkFormPoint <$> a
	in (i,FormPoint pass (h (show i)) ml)
				

data Form = Form 
	{tree :: MGraph FormPoint
	,remap :: [(Int,Int)]
	} deriving Show

type Risposta = (Int, Value) -- risposta di una interazione

applica' :: Risposta -> TVar Form -> ErrorT String IO (FormKey -> Html)
applica' (i,v) tps =  do
		fo@(Form mg rm) <- lift . atomically $ readTVar tps
		lift $ print ((i,v),fo)
		j <- onNothing "indice della form fuori dai limiti" $ lookup i rm
		let FormPoint c _ _ = load mg A.! j
		lift $ print j	
		(k,fp) <- onNothing  "il valore risposto non è accettabile" (c v) >>= lift
		(mg',l) <- onNothing "la memoria dell'interazione è esaurita" $ update mg (j,fp)
		lift . atomically . writeTVar tps $ Form mg' $ (k,l):(filter ((/=) k . fst) $ rm)
		return $ prompt fp		
serve' :: Int -> TVar Form -> ErrorT String IO Link
serve' i tps = do
	Form mg rm <- lift . atomically $ readTVar tps
	j <- onNothing "indice della form fuori dai limiti" $ lookup i rm
	let FormPoint _ _ ml = load mg A.! j
	onNothing "questo indice non ha un valore da scaricare associato" ml


data Sessione = Sessione
	{	azione :: HPasso IO ()
	,	forms :: TVar [(FormKey, TVar Form)]
	}

mkSessione :: HPasso IO () -> IO Sessione
mkSessione hp = do
	forms <- atomically $ newTVar []
	return $ Sessione hp forms 

newForm :: Sessione -> Maybe FormKey -> IO FormKey
newForm (Sessione hp tfs) Nothing = do
	let (0,fp) = mkFormPoint hp
	key <- show <$> (randomIO  :: IO Int)
	atomically $ do
		fs <- readTVar tfs
		tfp <- newTVar $ Form (mkMGraph 30 fp) [(0,0)]
		writeTVar tfs $ (key,tfp) : fs
	return key

newForm s@(Sessione hp tfs) (Just clone) = do
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
	if c then newForm s Nothing else return key

pickForm :: Sessione -> FormKey -> ErrorT String IO (TVar Form)
pickForm (Sessione hp tfs) fk = 
	(lift . atomically) (readTVar tfs) >>= onNothing "chiave dell'interazione non trovata" . lookup fk  

applica :: Sessione -> FormKey -> Maybe Risposta -> ErrorT String IO Html
applica s  fk res =  do 
	form <- pickForm s fk 	
	($fk) <$> case res of 
		Nothing -> prompt <$> (flip (A.!) 0) <$> load <$> tree  <$> lift (atomically $ readTVar form) 
		Just iv -> applica' iv form 

runApplica :: ErrorT String IO Html -> CGI CGIResult
runApplica k = lift (runErrorT k) >>= either outputNotFound (output . prettyHtml)

serve :: Sessione -> FormKey -> Int -> ErrorT String IO Link
serve s fk i = pickForm s fk >>= serve' i 

runServe :: ErrorT String IO Link -> CGI CGIResult
runServe k = lift (runErrorT k) >>= either outputNotFound r where
	r (Link n x m) = do
		setHeader "Content-type" m
		setHeader "Content-Disposition" "attachment"
		setHeader "Content-Disposition" $ "filename=" ++ show n
		output x 

readInputs :: ErrorT String (CGIT IO) (Int,String)
readInputs = do 
	hf <- lift (getInput "hkey") >>= onNothing "form corrotta, manca la chiave di punto"
	(i,_) <- onNothing "form corrotta, la chiave di punto non è un numero" . listToMaybe $ reads hf
	v <- lift (getInput "valore") >>= onNothing "form corrotta, manca la risposta"
	return (i,v)


				
singleFormServer :: HPasso IO () -> IO (CGI CGIResult)
singleFormServer hp = do
	s@(Sessione _ tfp) <- mkSessione hp
	fk <- newForm s Nothing
	return $ do 
		r <- runErrorT $ do
			vs <- lift $ getVars 
			lift $ getInputs >>= liftIO . print
			case lookup "REQUEST_URI"  vs of 
				Just "/interazione" -> do 
					(i,v) <- readInputs
					lift . runApplica . applica s fk $ Just (i,v)
				Just "/download" -> do
					(i,_) <- readInputs
					lift . runServe $ serve s fk i
				_ -> lift . runApplica $ applica s fk Nothing
		either outputNotFound return r

 
		
		




server interazione = do
	service <- singleFormServer interazione
	runSCGI  (PortNumber $ fromIntegral 5000)  $ handleErrors service

interazione :: Costruzione IO () () 
interazione = rotonda $ \k -> do
		P.output (ResponseOne "benvenuto")
		(x :: Int) <- upload "un file contenente lo show di un numero" 
		P.output (ResponseOne ("bel numero :" ++ show x))
		download "il numero uno.txt" x
		t <- scelte [("si",True),("no",False)] "fine?"
		if t then k () else return ()

