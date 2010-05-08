{-# LANGUAGE ScopedTypeVariables #-}
module Lib.Server.Core where

import Data.Maybe (listToMaybe)
import Data.List (lookup,partition)

import Control.Applicative ((<$>))
import Control.Concurrent.STM (newTVar,  readTVar, writeTVar,atomically)
import Control.Monad (join, (>=>), foldM)
import Control.Monad.Error (ErrorT, throwError, lift, MonadIO)
import Control.Arrow ((***),(&&&),  second, first)

import System.Random (randomIO)
import qualified Data.IntMap as M 

import Debug.Trace
import Lib.Assocs (secondM, firstM)
import Lib.Missing (onNothing, (>$>))
import Lib.Database (limitedDB, query, lkey, set)


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
	, serializzazione :: [Value]
	, form :: Enk -> Fok -> b	-- ^ modulistica di interazione
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
				Just r -> Right <$> r 
		k lf _ = return lf

-- | i valori delle richieste possibili, scevri dai valori comuni
data Req 
	= Continua 	Value
	| Scarica 	
	| Clona Id
	| Chiudi 

-- | tutte le richieste portano con se la chiave di environment, e la chiave di cella
type Request = (Enk,Fok,Req)

-- | Un identificativo dato dall'esterno
type Id = Int

-- | una form assieme al suo nome
type IdedForm e b c = (Form e b c, Id)

-- | un insieme di form, usiamo una mappa per semplificare la definizione dellM.fromListe updates
type IM e b c = M.IntMap (IdedForm e b c)

-- estrae i valori di modulistica da un insieme di form e la sua chiave
render :: Enk -> IM e b c -> [(b,Id)]
render enk = map (\(i,(fo,j)) -> (form fo enk i,j)) . M.assocs  

-- | esegue il protocollo Req
esegui 	:: Req 			-- ^ richiesta da soddisfare
	-> IM e b c 		-- ^ insieme di form
	-> Fok 			-- ^ chiave della form richiedente
	-> IdedForm e b c 	-- ^ form richiedente
	-> ErrorT String IO (Either c (IM e b c))	-- ^ o lo scaricabile oppure il nuovo insieme di form
esegui (Continua v) fos fok (fo,j) = do 
	fo' <- join . onNothing "il valore non è stato compreso" . fmap lift $ continuazione fo v	
	return . Right $ M.adjust (const (fo',j)) fok fos 
esegui Scarica _ _ (fo,j) = fmap Left . onNothing "la form non contiene un valore da scaricare" $ scarica fo
esegui (Clona j) fos fok (fo,_) = let 	(afos,bfos) = partition ((< fok) . fst) $ M.assocs fos
	in return . Right  $ M.fromList $ afos ++ (fok,(fo,j)): map (first (+1)) bfos
esegui Chiudi fos fok _  = return . Right  $ if M.size fos > 1 then M.delete fok fos else fos



-- | il server come reattore a richieste. Il nome delle form filtra nelle uscite
-- il tipo 'e' filtra per inchiodare il tipo delle form
data Server e b c = Server {
	apertura :: IO [(b,Id)], -- ^ il risultato di una non richiesta
	servizio :: Request -> ErrorT String IO (Either c [(b,Id)]) -- ^ reazione ad una richiesta
	}

mkEnk :: IO Enk
mkEnk = show <$> (`mod` (100000 :: Int)) <$> abs <$> randomIO

-- | costruisce un server a partire dalla form base
mkServer 	:: forall e b c 
		. Int 			-- ^ limite per il numero di insiemi gestiti
		-> [IdedForm e b c] 	-- ^ forms di partenza
		-> IO (Server e b c)	-- ^ il reattore 

mkServer limit bs = do
	-- una chiave per l'insieme di form iniziale
	enk <- mkEnk
	-- apriamo un database in memoria (String -> IM e b c) e assegnamo alla chiave "0" l'insieme iniziale
	-- il database è condiviso alle chiamate, quindi va in retry in caso di update contemporaneo
	dbe <- atomically . newTVar $ set (limitedDB limit) (enk,M.fromList $ zip [0..] bs)
	let	apertura = atomically $ do -- carica l'ultima chiave inserita
			Just enk <- lkey <$> readTVar dbe 
			-- carica l'insieme di form relativo
			Just fos <- ($enk) . query <$> readTVar dbe
			-- estrae l'uscita dalla forma e gli associa il nome (j::Id)
			return $ render enk fos 
		servizio (enk,fok,q) = do
			-- estrae l'insieme di form dal db
			fos <- (lift . atomically) (($enk) . query <$> readTVar dbe) >>= onNothing "chiave temporale non trovata" 
			-- estrae la form con il suo nome
			foj  <- onNothing "chiave di form non trovata" $ fok `M.lookup` fos
			r <- esegui q fos fok foj
			case r of
				Left x -> return $ Left x
				Right fos' -> do
					-- riforma l'insieme di form ricaricandole
					fos'' <- M.fromList >$> lift . mapM (secondM . firstM $ ricarica) $ M.assocs fos'
					-- crea una nuova chiave storica (non serve il random ?)
					enk' <- lift mkEnk				
					-- aggiunge il nuovo insieme di form al db
					lift . atomically $ readTVar dbe >>= writeTVar dbe . flip set (enk', fos'')
					return . Right $ render enk' fos'' 
	return $ Server apertura servizio

	
