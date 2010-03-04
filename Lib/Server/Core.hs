{-# LANGUAGE ScopedTypeVariables #-}
module Lib.Server.Core where

import Data.List (lookup)

import Control.Applicative ((<$>))
import Control.Concurrent.STM (newTVar,  readTVar, writeTVar,atomically)
import Control.Monad (join, (>=>), foldM)
import Control.Monad.Error (ErrorT, throwError, lift)
import Control.Arrow ((***),(&&&),  second)

import System.Random (randomIO)
import qualified Data.IntMap as M (IntMap,assocs,insert,delete,adjust,fromList,lookup,singleton)

import Debug.Trace
import Lib.Assocs (secondM)

------------------------------------ library -----------------------------------------
-- | interfaccia di un DB
data DB a b = DB {query :: a -> Maybe b, set :: (a,b) -> DB a b, dbmap :: (b -> b) -> DB a b}

-- | un DB inefficiente a memoria limitata 
limitedDB :: (Show a, Eq a) 
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

-- | chiave di form. Indica una form all'interno si una sessione
type Fok = Int

-- | il valore della risposta dell'utente
type Value = String

-- | una cella interattiva embrionale polimorfa nelle risposte 'b' e 'c', che puo' avanzare con la continuazione o essere ricaricata.In generale la continuazione non è ricomputabile dalla ricarica, in quanto la ricarica potrebbe logicamente produrre una form più basilare dell'attuale, e quindi fallire inspiegabilmente il parsing del value.
data Form e b c = Form
	{ continuazione :: Value -> Maybe (IO (Form e b c)) -- ^ passaggio a nuova form
	, ricarica :: IO (Form e b c) -- ^ ricomputazione della form, per inglobare eventuali cambiamenti logici
	, form :: Enk -> Fok -> b	-- ^ modulistica di interazione
	, scarica :: Maybe c	-- ^ eventuale valore servibile
	}

-- | i valori delle richieste possibili, scevri dai valori comuni
data Req 
	= Continua 	Value
	| Scarica 	
	| Clona 
	| Chiudi 	

-- | tutte le richieste portano con se la chiave di environment, e la chiave di cella
type Request = (Enk,Fok,Req)

-- | una mappatura tra chiavi di environment e insiemi di celle
type Servizio e b c = DB Enk (M.IntMap (Form e b c))

-- | il server come reattore a richieste
type Server e b c = (b,Request -> ErrorT String IO (Either c [b]))

-- | costruisce un server a partire dalla form base
mkServer 	:: forall e b c . Int 			-- ^ limite per il numero di environments
		-> Form e b c 		-- ^ form di base
		-> IO (Server e b c)	-- ^ il reattore 

mkServer limit base = do
	dbe <- atomically . newTVar $ set (limitedDB limit) ("0",(M.singleton 0 base))
	(,) (form base "0" 0) >$> return $ \(enk,fok,q) -> do
		-- restituisce le celle riferite alla chiave di environment
		lift $ print ("chiave temporale",enk)
		fos <- lift (atomically (($enk) . query <$> readTVar dbe)) >>= onNothing "chiave temporale non trovata" 
		fo  <- onNothing "indice di cella non trovato" $ fok `M.lookup` fos
		enk' <- lift $ show <$> (randomIO :: IO Int)
		lift $ print ("nuova chiave temporale",enk)
		let 	ricarica' :: M.IntMap (Form e b c) ->ErrorT String IO (Either c (M.IntMap (Form e b c)))
			ricarica' xs = Right 
					>$> M.fromList 
					>$> mapM (lift . secondM ricarica) $ M.assocs xs
			esegui' (Continua v) = do 
				fo' <- join . onNothing "il valore non è stato compreso" 
					. fmap lift $ continuazione fo v	
				ricarica' $ M.adjust (const fo') fok fos
			esegui' Scarica = 
				fmap Left . onNothing "la form non contiene un valore da scaricare" $ scarica fo
			esegui' Clona  = do
				fok' <- lift $ (randomIO :: IO Int)
				ricarica' $ M.insert fok' fo fos
			esegui' Chiudi = ricarica' $ M.delete fok fos 
			update fos' = do
				
				lift . atomically $ do 
					enks <- readTVar dbe
					writeTVar dbe $ set enks (enk', fos')
				return . map (\(fok,fo) -> form fo enk' fok) $ M.assocs fos'
		esegui' q >>= either (return . Left) (Right >$> update) 
	

