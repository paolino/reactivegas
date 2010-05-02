module Core.Sessione (Sessione (..), mkSessione) where

import Control.Monad (forever)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM

import Text.XHtml
import Core.Types (Evento)
import Eventi.Anagrafe (Responsabile)
import Debug.Trace
-- | interfaccia concorrente per una sessione di interazione
data Sessione a b = Sessione 
	{readEventi :: IO [Evento]			-- ^ legge gli eventi in memoria		
	,writeEventi :: [Evento] -> IO ()		-- ^ scrive gli eventi in memoria
	,readAccesso :: IO (Maybe Responsabile)		-- ^ legge il responsabile in azione
	,writeAccesso :: Maybe Responsabile -> IO ()	-- ^ scrive il responsabile in azione
	, readCaricamento :: IO b
	-- | legge lo stato modificato dagli eventi in memoria prodotti dal responsabile in memoria
	,readStatoSessione :: IO a
	,setConservative :: Int -> IO ()
	,getConservative :: IO (Int)
	}

data Triggers = TResponsabile (Maybe Responsabile) | TEventi [Evento] | TConservative Int

-- memoria condivisa
data Board a = Board
	{ eventi :: TVar [Evento] 
	, accesso :: TVar (Maybe Responsabile)
	, stato :: TVar a
	, caricatura :: TVar String
	, triggers :: TChan Triggers -- il canale al quale inviare le modifiche
	, conservative :: TVar Int
	}

type Update a b = Int -> Maybe Responsabile -> [Evento] -> STM (a,b)

update :: Update a b -> Int -> (TVar [Evento], TVar (Maybe Responsabile),  TVar Int , TVar a, TVar b) -> STM ()
update f l (eventi,accesso,conservative, stato, caricatura) = do
	mr <- readTVar accesso
	evs <- readTVar eventi
	l' <- readTVar conservative
	(s,c) <- f l' mr evs 
	writeTVar stato s
	c' <- if l == l' then return c else fmap snd $ f l mr evs
	writeTVar caricatura c

-- azione di modifica di uno di eventi o responsabile
triggering 	:: Update a b -- produce uno stato modificato
		-> Int -- ^ livello di caricamento completo
		-> 	(TVar [Evento]
			,TVar (Maybe Responsabile)
			,TVar Int
			,TVar a
			,TVar b
			,TChan Triggers
			,TChan ()
			) 
		-> STM ()
triggering f l (eventi,accesso,conservative, stato,caricatura,triggers,cs) = do
	(do 	t <- readTChan triggers 
		case t of 	TResponsabile mr -> writeTVar accesso mr
				TEventi es -> writeTVar eventi es
				TConservative l -> writeTVar conservative l
	
	 `orElse` (readTChan cs >> return ()))
	update f l (eventi,accesso,conservative, stato,caricatura)

-- | costruisce l'interfaccia di sessione a partire da un modificatore di stato in STM
mkSessione 	:: Update a  b		-- ^ modificatore di stato
		-> Int 			-- ^ livello di caricamento di base
		-> TChan ()		-- ^ segnale di aggiornamento stato
		-> IO (Sessione a b)	
mkSessione f l cs = do
	(s,c) <- atomically $ f l Nothing [] -- uno stato iniziale, dovrebbe corrispondere allo stato non modificato
	stato <- atomically $ newTVar s
	eventi <- atomically $ newTVar []
	accesso <- atomically $ newTVar Nothing
	caricatura <- atomically $ newTVar c
	triggers <- atomically $ newTChan
	conservative <- atomically $ newTVar l
	let t = atomically . before (triggering f l (eventi,accesso,conservative, stato,caricatura , triggers, cs))
	return $ Sessione 
		(t $ readTVar eventi) 
		(atomically . writeTChan triggers . TEventi)
		(t $ readTVar accesso) 
		(atomically . writeTChan triggers . TResponsabile)
		(t $ readTVar caricatura)
		(t $ readTVar stato)
		(atomically . writeTChan triggers . TConservative)
		(atomically $ readTVar conservative)

before :: STM a -> STM b -> STM b 
before a b = (a >> b) `orElse` b
