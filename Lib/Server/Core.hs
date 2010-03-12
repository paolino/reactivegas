{-# LANGUAGE ScopedTypeVariables #-}
module Lib.Server.Core where

import Data.Maybe (listToMaybe)
import Data.List (lookup)

import Control.Applicative ((<$>))
import Control.Concurrent.STM (newTVar,  readTVar, writeTVar,atomically)
import Control.Monad (join, (>=>), foldM)
import Control.Monad.Error (ErrorT, throwError, lift)
import Control.Arrow ((***),(&&&),  second, first)

import System.Random (randomIO)
import qualified Data.IntMap as M 

import Debug.Trace
import Lib.Assocs (secondM)

------------------------------------ library -----------------------------------------
-- | interfaccia di un DB
data DB a b = DB 
	{ query :: a -> Maybe b
	, lkey :: Maybe a
	, set :: (a,b) -> DB a b
	, forget :: a -> DB a b 
	, dbmap :: (b -> b) -> DB a b
	}

-- | un DB inefficiente a memoria limitata 
limitedDB :: (Show a, Eq a) 
	=> Int 	-- ^ massimo numero di elementi
	-> DB a b
limitedDB limit = let
	q xs x = lookup x xs
	l xs = fst <$> listToMaybe xs
	s xs (x,y) = mkdb . take limit $ (x,y) : xs 
	f xs x = mkdb . filter ((/=) x . fst) $ xs	
	m xs f = mkdb . map (second f) $ xs 
	mkdb xs = DB (q xs) (l xs) (s xs) (f xs) (m xs)
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
	, serializzazione :: [Value]  -- ^ insieme minimo di valori per raggiungere lo stato attuale della form
	, form :: Enk -> Fok -> b	-- ^ modulistica di interazione
	, scarica :: Maybe c	-- ^ eventuale valore servibile
	}

-- | i valori delle richieste possibili, scevri dai valori comuni
data Req 
	= Continua 	Value
	| Scarica 	
	| Clona 
	| Chiudi 
	| Inchioda	

-- | tutte le richieste portano con se la chiave di environment, e la chiave di cella
type Request = (Enk,Fok,Req)

type NailedForm e b c = (Maybe Int,Form e b c)
type IM e b c = M.IntMap (NailedForm e b c)
-- | una mappatura tra chiavi di environment e insiemi di celle
type Servizio e b c = DB Enk (IM e b c)

-- | il server come reattore a richieste
type Server e b c = (IO [b],Request -> ErrorT String IO (Either c [b]))

-- | costruisce un server a partire dalla form base
mkServer 	:: forall e b c . Int 			-- ^ limite per il numero di environments
		-> [NailedForm e b c] 		-- ^ forms di base
		-> ([[Value]] -> IO ()) -- ^ persistenza
		-> IO (Server e b c)	-- ^ il reattore 

mkServer limit bs pers = do
	let ibs = zip [0..] bs
	dbe <- atomically . newTVar $ set (limitedDB limit) ("0",M.fromList ibs)
	let
	  def = atomically $ do 
			Just enk <- lkey <$> readTVar dbe 
			Just fos <- ($enk) . query <$> readTVar dbe
			return . map (\(i,(_,fo)) -> form fo enk i) $ M.assocs fos 
	  req (enk,fok,q) = do
		-- restituisce le celle riferite alla chiave di environment
		fos <- lift (atomically (($enk) . query <$> readTVar dbe)) >>= onNothing "chiave temporale non trovata" 
		foi@(mi,fo)  <- onNothing "indice di cella non trovato" $ fok `M.lookup` fos
		enk' <- lift $ show <$> (`mod` 10000000) <$> abs <$> (randomIO :: IO Int)
		let 	ricarica' :: IM e b c ->ErrorT String IO (Either c (IM e b c))
			ricarica' xs = Right 
					>$> M.fromList 
					>$> mapM (lift . secondM (secondM ricarica)) $ M.assocs xs
			esegui' (Continua v) = do 
				fo' <- join . onNothing "il valore non è stato compreso" 
					. fmap lift $ continuazione fo v	
				let fo'' = case mi of
					Just l -> if length (serializzazione fo') >= l then fo' else fo
					Nothing -> fo'
				ricarica' $ M.adjust (const (mi,fo'')) fok fos 
			esegui' Scarica = 
				fmap Left . onNothing "la form non contiene un valore da scaricare" $ scarica fo
			esegui' Clona  = do
				let fok' = last (M.keys fos) + 1
				ricarica' $ M.insert fok' foi fos
			esegui' Inchioda = do
				ricarica' $ M.adjust (first (const . Just . length . serializzazione $ fo)) fok fos
			esegui' Chiudi = ricarica' $ if M.size fos > 1 then M.delete fok fos else fos
			update fos' = do
				lift . pers $ map (serializzazione . snd) $ M.elems fos'
				lift . atomically $ do 
					enks <- readTVar dbe
					writeTVar dbe $ set enks (enk', fos')
				return . map (\(fok,(_,fo)) -> form fo enk' fok) $ M.assocs fos'
		esegui' q >>= either (return . Left) (Right >$> update) 
	return (def,req)

restore :: forall e b c. Form e b c -> [Value] -> IO (Form e b c)
restore base hs = foldM k (Right base) (reverse hs) >>= either return return
	where 	k :: Either (Form e b c) (Form e b c) -> Value -> IO (Either (Form e b c) (Form e b c))
		k (Right f) v = do
			let mr = continuazione f v
			case mr of 
				Nothing -> return (Left f)
				Just r -> Right <$> r 
		k lf _ = return lf
	
