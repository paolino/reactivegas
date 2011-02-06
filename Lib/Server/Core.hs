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
import Lib.Database (DB, limitedDB, query, lkey, set, exists, restoreDB, select)


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

type FormDB e b c = DB (Either (TimeKey,FormKey) TimeKey) (Either (Form e b c) (FormGroup e b c))

-- estrae i valori di modulistica da un insieme di form e la sua chiave
renderS 	:: FormDB e b c -- database del tempo
		-> TimeKey -- chiave di tempo attuale
		-> FormKey -- chiave di form
		-> Form e b c -- form
		-> (b,FormId) -- rendering
renderS db enk fok fo = (form fo enk fok enkB enkF, formKey fok) where
	enkB = if exists db (Left (enk - 1,fok)) then Just (enk - 1) else Nothing
	enkF = if exists db (Left (enk + 1,fok)) then Just (enk + 1) else Nothing

renderT 	:: FormDB e b c -- database del tempo
		-> TimeKey -- chiave di tempo attuale
		-> FormGroup e b c -- form
		-> [(b,FormId)] -- rendering
renderT db enk = map (\(fok,fo) -> (form fo enk fok enkB enkF ,formKey fok)) . M.assocs  where
	enkB = if exists db (Right (enk - 1)) then Just (enk - 1) else Nothing
	enkF = if exists db (Right (enk + 1)) then Just (enk + 1) else Nothing

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
	efosfo <- onNothing "chiave temporale non trovata" $ 
		query db (Left (enk,fok)) `mplus` query db (Right enk)
	case efosfo of
		Right fos -> do
			fo <- onNothing "chiave di form non trovata" $ 
				fok `M.lookup` fos
			fo' <- f fo
			lift . atomically $ readTVar dbe >>= writeTVar dbe . 
					flip set (Left (nenk,fok), Left fo')
			return fo'
		Left fo -> do
			fo' <- f fo
			lift . atomically $ readTVar dbe >>= 
				writeTVar dbe . flip set (Left (nenk,fok), Left fo')
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
	let 	db0 = restoreDB limit $ (Right enk,Right fos0) : map (\(fok,fo) -> (Left (enk,fok),Left fo)) (M.toList fos0)
		fos0 = M.fromList $ map (FormKey . snd &&& fst) bs
		apertura = renderT db0 enk fos0
	dbe <- atomically . newTVar $ db0
	let	servizio (enk,fok,q) = do
			
			db <- lift . atomically $ readTVar dbe 
			efosfo <- onNothing "chiave temporale non trovata" $ 
				query db (Left (enk,fok)) `mplus` query db (Right enk)
			case q of
				ClonaS -> do
					fo <- case efosfo of
						Right fos -> onNothing "chiave di form non trovata" $ 
								fok `M.lookup` fos
						Left fo -> return fo
					fok' <- lift $ mkFokKey
					lift $ atomically $ do 
						db <- readTVar dbe
						writeTVar dbe $ set db (Left (enk,fok'), Left fo)
					return . Right . return . first id  $ renderS db enk fok' fo
				ResetS -> do 
					fo <- case efosfo of
						Right fos -> onNothing "chiave di form non trovata" $ 
								fok `M.lookup` fos
						Left fo -> return fo
					db <- lift $ atomically $ readTVar dbe
					let 	es =  select db (either ((==) fok . snd) (const False)) 
 						(Left (enk',_),Left fo') = minimumBy (comparing $ \(Left (e,_),_) -> e) es
					return . Right . return . first id  $ renderS db enk' fok fo'
				RicaricaS -> do
					fo <- correctS dbe eseguiRicaricaS fok enk enk
					db <- lift . atomically $ readTVar dbe 
					c <- lift reload
					return . Right . return . first c  $ renderS db enk fok fo
				ContinuaS v -> do
					fo <- correctS dbe (eseguiContinuaS v) fok enk (enk + 1)
					c <- lift reload
					db <- lift . atomically $ readTVar dbe 
					return . Right . return . first c $ renderS db (enk + 1) fok fo
				ScaricaD -> do
					fo <- case efosfo of
						Right fos -> onNothing "chiave di form non trovata" $ 
								fok `M.lookup` fos
						Left fo -> return fo
					Left <$> eseguiScarica fo
				ContinuaT v -> do			
					fos <- case  efosfo of
						Left _ -> throwError 
							"chiave temporale riferita ad una form"
						Right fos -> return fos
					fos' <- eseguiContinuaT v fok fos
					fos'' <- M.fromList <$> mapM (\(k,f) -> ((,) k) <$> lift (ricarica f)) 
								(M.assocs fos')
					let db' = set db (Right $ enk + 1, Right fos'')
					lift . atomically $ writeTVar dbe db'
					return . Right $ renderT db' (enk + 1) fos''
	return $ Server apertura servizio
