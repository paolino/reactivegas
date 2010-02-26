{-# LANGUAGE ScopedTypeVariables #-}
import Data.Maybe (listToMaybe)
import Data.List (splitAt,lookup)

import Control.Applicative ((<$>))
import Control.Concurrent.STM (newTVar, TVar, readTVar, writeTVar,atomically)
import Control.Monad (join)
import Control.Monad.Error (ErrorT, throwError, runErrorT,lift)
import Control.Arrow ((***),(&&&), first, second)

import System.Random (randomIO)
import qualified Data.IntMap as M (IntMap,assocs,insert,delete,adjust,fromList,lookup,singleton)

import Lib.Assocs (secondM)
import Lib.Passo
import Text.XHtml
------------------------------------ library -----------------------------------------
-- | interfaccia di un DB
data DB a b = DB {query :: a -> Maybe b, set :: (a,b) -> DB a b, dbmap :: (b -> b) -> DB a b}

-- | un DB inefficiente a memoria limitata 
limitedDB :: Eq a 
	=> Int 	-- ^ massimo numero di elementi
	-> DB a b
limitedDB limit = let
	q xs x = lookup x xs
	s xs (x,y) = mkdb . take limit $ (x,y) : xs 
	m xs f = mkdb . map (second f) $ xs 
	mkdb xs = DB (q xs) (s xs) (m xs)
	in mkdb []

-- | erroring on Nothing
onNothing :: Monad m => String -> Maybe a -> ErrorT String m a
onNothing x = maybe (throwError x) return  

infixr 8 >$>
(>$>) :: Functor f => (a -> b) -> (c -> f a) -> c -> f b
(>$>) = (.) . (<$>)
-------------------------------------------------------------------------------------

-- | la chiave di environment, indica una situazione dell'interfaccia untente. Le richieste la portano con se, per ricontestualizzare la risposta alla situazione "attuale" che l'utente fronteggia
type Enk = String

-- | il valore della risposta dell'utente
type Value = String

-- | una cella interattiva embrionale polimorfa nelle risposte 'b' e 'c', che puo' avanzare con la continuazione o essere ricaricata
data Form e b c = Form
	{ continuazione :: e -> Value -> Maybe (IO (Maybe e, Form e b c)) -- ^ produzione di un nuovo environment
	, ricarica :: e -> IO (Form e b c ,Enk -> Fok ->  b) 
	, scarica :: Maybe c
	}

-- | chiave di cella
type Fok = Int

-- | i valori delle richieste possibili, tutte portano con se la chiave che individua la cella di emissione
data Req 
	= Continua 	Value
	| Scarica 	
	| Clona 
	| Chiudi 	

-- | tutte le richieste portano con se la chiave di environment, e la chiave di cella
type Request = (Enk,Fok,Req)

-- | una mappatura tra chiavi di environment e insiemi di celle
type Servizio e b c = DB Enk (e,M.IntMap (Form e b c))

-- | il server come reattore a richieste
type Server e b c = Request -> ErrorT String IO (Either c [b])

-- | costruisce un server a partire dalla form base
mkServer 	:: Int 			-- ^ limite per il numero di environments
		-> e 			-- ^ environment iniziale
		-> Form e b c 		-- ^ form di base
		-> IO (Server e b c)	-- ^ il reattore 

mkServer limit env0 base = do
	dbe <- atomically . newTVar $ set (limitedDB limit) ("0",(env0, M.singleton 0 base))
	let reset = atomically $ readTVar dbe >>= writeTVar dbe . flip dbmap (first $ const env0)
	return $ \(enk,fok,q) -> do
		-- restituisce le celle riferite alla chiave di environment
		mio <- lift $ atomically (($enk) . query <$> readTVar dbe)
		(env,fos) <- onNothing "chiave temporale non trovata" $ mio
		fo  <- onNothing "indice di cella non trovato" $ fok `M.lookup` fos
		-- nuova chiave di environment
		enk' <- lift $ show <$> (randomIO :: IO Int)
		let 	ricarica' env xs = Right 
					>$> (,) env 
					>$> first M.fromList >$> unzip
					>$> mapM (lift . ricarica1 env) $ M.assocs xs
			ricarica1  env (fok,fo) = ((,) fok *** ($fok). ($enk')) <$> ricarica fo env0
			esegui' (Continua v) = do 
				(env', fo') <- join . onNothing "continuazione fallita, il valore non è stato compreso" 
					. fmap lift $ continuazione fo env v	
				env'' <- case env' of 	
					Nothing -> lift reset >> return env0
					Just e -> return e
				ricarica' env'' $ M.adjust (const fo') fok fos
			esegui' Scarica = fmap Left . 		
					onNothing "richiesta insensata, la form non contiene un valore da scaricare" 
					$ scarica fo
			esegui' Clona  = do
				fok' <- lift $ (randomIO :: IO Int)
				ricarica' env $ M.insert fok' fo fos
			esegui' Chiudi = ricarica' env $ M.delete fok fos 
			update (env',(fos',bs)) = do
				lift . atomically $ do 
					enks <- readTVar dbe
					writeTVar dbe $ set enks (enk', (env',fos'))
				return bs
		esegui' q >>= either (return . Left) (Right >$> update) 
	
{-
fromHPasso :: HPasso (ReaderT e IO) () -> Form e Html Link
fromHPasso (ctx,p) = let 
	(h,ml,cont) = runPasso p -- trasformata html
	pass env = cont >=> \a -> return $ runReaderT (liftM2 (,) ask a) env >>= secondM fromHPasso
	reload env enk fok = do
		
	in do	key <- show <$> (randomIO  :: IO Int)
		return (key,FormPoint pass ctx env (h key) ml)
-}

