{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module Lib.Server.Core where

import Data.Maybe (listToMaybe)
import Data.List (lookup,partition,minimumBy)
import Data.Ord (comparing)

import Control.Applicative ((<$>))
import Control.Concurrent.STM (newTVar,  readTVar, writeTVar,atomically, TVar)
import Control.Monad (join, (>=>), foldM, mplus)
import Control.Monad.Error (ErrorT, throwError, lift, MonadIO)
import Control.Arrow ((***),(&&&),  second, first)

import System.Random (randomIO)
import qualified Data.Map as M 

import Debug.Trace
import Lib.Missing (onNothing, (>$>), firstM, secondM)
import Lib.Database (DB, limitedDB, query, lkey, set, exists, restoreDB, select, purge,dump)


-- | la chiave di environment, indica una situazione dell'interfaccia untente. Le richieste la portano con se, per ricontestualizzare la risposta alla situazione "attuale" che l'utente fronteggia
newtype TimeKey = TimeKey Int deriving (Num, Eq, Show,Read, Ord)

timeKey (TimeKey t) = t
-- | chiave di form. Indica una form all'interno si una sessione
newtype FormKey = FormKey Int deriving (Eq,Show,Read,Ord)
formKey (FormKey t) = t
-- | il valore della risposta dell'utente
type Value = String

-- | una cella interattiva embrionale polimorfa nelle risposte 'b' e 'c', che puo' avanzare con la continuazione o essere ricaricata.In generale la continuazione non è ricomputabile dalla ricarica, in quanto la ricarica potrebbe logicamente produrre una form più basilare dell'attuale, e quindi fallire inspiegabilmente il parsing del value.
data Form e b c = Form
	{ continuazione :: Value -> Maybe (IO (Form e b c)) -- ^ passaggio a nuova form
	, ricarica :: IO (Form e b c) -- ^ ricomputazione della form, per inglobare eventuali cambiamenti logici
	, serializzazione :: [Value]
	, form :: TimeKey -> FormKey -> Maybe TimeKey -> Maybe TimeKey -> b	-- ^ modulistica di interazione
	, scarica :: Maybe c	-- ^ eventuale valore servibile
	}

-- | ricostruisce una form a partire dalla form di base , applicando i valori alle continuazioni , sinche' vengono accettati
restore :: forall e b c. Form e b c -> [Value] -> IO (Form e b c)
restore base hs = foldM k (Right base) (reverse hs) >>= either return return
	where 	k :: Either (Form e b c) (Form e b c) -> Value -> IO (Either (Form e b c) (Form e b c))
		k (Right f) v = do
			let mr = continuazione f v
			case mr of 
				Nothing -> return (Left f)
				Just r ->   Right <$> r 
		k lf _ = return lf

-- | i valori delle richieste possibili, scevri dai valori comuni
data Req 
	= ContinuaT 	Value
	| ContinuaS 	Value
	| RicaricaS
	| ScaricaD
	| ClonaS
	| ResetS

-- | tutte le richieste portano con se la chiave di environment, e la chiave di cella
type Request = (TimeKey,FormKey,Req)

-- | Un identificativo dato dall'esterno
type FormId = Int

-- | un insieme di form, usiamo una mappa per semplificare la definizione delle updates nel caso di gruppo
type FormGroup e b c = M.Map FormKey (Form e b c)

type FormDB e b c = DB (TimeKey,FormKey) (Form e b c)
type FormGroupDB e b c = DB  TimeKey (FormGroup e b c)

-- estrae i valori di modulistica da un insieme di form e la sua chiave
renderS 	:: FormDB e b c -- database del tempo
		-> TimeKey -- chiave di tempo attuale
		-> FormKey -- chiave di form
		-> Form e b c -- form
		-> (b,FormId) -- rendering
renderS db enk fok fo = (form fo enk fok enkB enkF, formKey fok) where
	enkB = if exists db (enk - 1,fok) then Just (enk - 1) else Nothing
	enkF = if exists db (enk + 1,fok) then Just (enk + 1) else Nothing

renderT 	:: FormGroupDB e b c -- database del tempo
		-> TimeKey -- chiave di tempo attuale
		-> FormGroup e b c -- form
		-> [(b,FormId)] -- rendering
renderT db enk = map (\(fok,fo) -> (form fo enk fok enkB enkF ,formKey fok)) . M.assocs  where
	enkB = if exists db (enk - 1) then Just (enk - 1) else Nothing
	enkF = if exists db (enk + 1) then Just (enk + 1) else Nothing

eseguiContinuaT v fok fos = do 
	fo <- onNothing "chiave di form non trovata" $ fok `M.lookup` fos
	fo' <- join . onNothing "il valore non è stato compreso" . fmap lift $ continuazione fo v	
	return $ M.adjust (const fo') fok fos 

eseguiContinuaS v fo = do 
	fo' <- join . onNothing "il valore non è stato compreso" . fmap lift $ continuazione fo v 
	-- lift $ ricarica fo'
	return fo'

eseguiRicaricaS	fo = lift $ ricarica fo

eseguiScarica fo = do
	onNothing "la form non contiene un valore da scaricare" $ scarica fo

-- | il server come reattore a richieste. Il nome delle form filtra nelle uscite
-- il tipo 'e' filtra per inchiodare il tipo delle form
data Server e b c = Server {
	apertura :: [(b,FormId)], -- ^ il risultato di una non richiesta
	servizio :: Request -> ErrorT String IO (Either c [(b,FormId)]) -- ^ reazione ad una richiesta
	}

mkTimeKey :: IO TimeKey
mkTimeKey = TimeKey <$> (`mod` (10000000 :: Int)) <$> abs <$> randomIO

mkFokKey = FormKey <$> (`mod` (10000000 :: Int)) <$> abs <$> randomIO

correctS 	:: TVar (FormDB e b c) 
		-> (Form e b c -> ErrorT String IO (Form e b c))
		-> FormKey
		-> TimeKey
		-> TimeKey
		->  ErrorT String IO (Form e b c)
correctS dbe f fok enk nenk = do
	db <- lift . atomically $ readTVar dbe
	fo <- onNothing ("chiave temporale non trovata 1") $ query db (enk,fok)
	fo' <- f fo
	lift . atomically $ readTVar dbe >>= 
		writeTVar dbe . flip set ((nenk,fok),fo')
	return fo'

-- | costruisce un server a partire dalla form base
mkServer 	:: Int 			-- ^ limite per il numero di insiemi gestiti
		-> IO (b -> b)		-- ^ condizione di ricarica
		-> [(Form e b c,Int)] 	-- ^ forms di partenza
		-> IO (Server e b c)	-- ^ il reattore 

mkServer limit reload bs = do
	-- una chiave per l'insieme di form iniziale
	enk <- mkTimeKey
	-- apriamo un database in memoria (String -> Either (Form e b c) (FormGroup e b c)) e assegnamo
	--  alla chiave "0" l'insieme iniziale
	-- il database è condiviso alle chiamate, quindi va in retry in caso di update contemporaneo
	let 	dbg0 = restoreDB limit $ [(enk,fos0)]
	 	db0 = restoreDB limit $ map (\(fok,fo) -> ((enk,fok),fo)) (M.toList fos0)
		fos0 = M.fromList $ map (FormKey . snd &&& fst) bs
		db1 = restoreDB limit $ M.toList fos0
		apertura = renderT dbg0 enk fos0
	dbe <- atomically . newTVar $ db0
	dbge <- atomically . newTVar $ dbg0
	dbr <-  atomically . newTVar $ db1
	let	servizio (enk,fok,q) = do
			
			case q of
				ClonaS -> do
					db <- lift . atomically $ readTVar dbe
					fo <- onNothing "chiave temporale non trovata" $ query db (enk,fok)
					fok' <- lift $ mkFokKey
					lift $ atomically $ do 
						db <- readTVar dbe
						writeTVar dbe $ set db ((enk,fok'),fo)
					lift . atomically $ do
						db <- readTVar dbr
						writeTVar dbr $ set db (fok',fo)
					return . Right . return . first id  $ renderS db enk fok' fo
				ResetS -> do 
					enk' <- lift mkTimeKey
					dbf0 <- lift . atomically $ readTVar dbr
					lfo0 <- onNothing "chiave di form reset non trovata" $ query dbf0 fok
					fo <- lift . ricarica $ lfo0
					db <- lift . atomically  $ do 
						db <- readTVar dbe 
						let 	db' = purge db (\(e,f) -> f == fok)
							db'' = set db' ((enk',fok),fo)
						writeTVar dbe db''
						return db''
					return . Right . return . first id  $ renderS db enk' fok fo
				RicaricaS -> do
					fo <- correctS dbe eseguiRicaricaS fok enk enk
					db <- lift . atomically $ readTVar dbe 
					c <- lift reload
					return . Right . return . first c  $ renderS db enk fok fo
				ContinuaS v -> do
					let enk' = enk + 1
					fo <- correctS dbe (eseguiContinuaS v) fok enk enk'
					c <- lift reload
					db <- lift . atomically $ do 
						db <- readTVar dbe 
						let db' = purge db (\(e,f) -> f == fok && e > enk') 
						writeTVar dbe db'
						return db'
					return . Right . return . first c $ renderS db enk' fok fo
				ScaricaD -> do
					db <- lift . atomically $ readTVar dbe
					dbg <-  lift . atomically $ readTVar dbge
					fo <- onNothing "chiave temporale non trovata" (query db (enk,fok) `mplus` (query dbg enk >>= M.lookup fok))
					Left <$> eseguiScarica fo
				ContinuaT v -> do			
					dbg <-  lift . atomically $ readTVar dbge
					fos <-  onNothing "chiave temporale non trovata" $ query dbg enk 
					fos' <- eseguiContinuaT v fok fos
					fos'' <- M.fromList <$> mapM (\(k,f) -> ((,) k) <$> lift (ricarica f)) 
								(M.assocs fos')
					let dbg' = set dbg (enk + 1, fos'')
					lift . atomically $ writeTVar dbge dbg'
					return . Right $ renderT dbg' (enk + 1) fos''
	return $ Server apertura servizio
