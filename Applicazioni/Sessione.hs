-- | gestione della sessione, del modello falso in cui l'utente si trova ad operare in bianco prima di fare persistere il proprio lavoro
module Applicazioni.Sessione (Sessione (..), mkSessione, Update) where

import Data.Maybe (listToMaybe)
import Data.List (union, (\\))
import Control.Applicative ((<$>))
import Control.Monad (forever, when, liftM2)
import System.Random
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (STM,TVar,retry,TChan,newTVar,newTChan,readTVar,readTChan,writeTVar,writeTChan,orElse,atomically)

import Core.Types (Evento,Responsabile, Utente)

import Applicazioni.Persistenza (Change (..))


import Debug.Trace

type Name = String 
-- | interfaccia concorrente per una sessione di interazione
data Sessione a b = Sessione 
	{
	readGruppo :: IO (Maybe Name)			-- ^ gruppo selezionato
	,writeGruppo :: Maybe Name -> IO ()		-- ^ cambia gruppo
	,readEventi :: IO [Evento]			-- ^ legge gli eventi in memoria		
	,writeEventi :: [Evento] -> IO ()		-- ^ scrive gli eventi in memoria
	,readAccesso :: IO (Maybe Responsabile)		-- ^ legge il responsabile in azione
	,writeAccesso :: Maybe Responsabile -> IO ()	-- ^ scrive il responsabile in azione
	,readCaricamento :: IO (Maybe b)	-- ^ legge l'effetto dell'ultimo caricamento dichiarazioni
	,readStatoSessione :: IO (Maybe a)	-- ^ legge lo stato modificato dagli eventi in memoria prodotti dal responsabile in memoria
	,setConservative :: Int -> IO () 	-- ^ imposta il livello di caricamento
	,getConservative :: IO Int		-- ^ legge il livello di caricamento
	,backup		:: IO (Maybe Name, [Evento],Maybe Responsabile,Int)
	}

-- | eventi che scatenano la ricomputazione dello stato modificato
data Triggers = TResponsabile (Maybe Responsabile) | TEventi [Evento] | TConservative Int | TGruppo (Maybe Name)
	deriving Show

-- | modello di ricomputazione che deve essere fornito
type Update a b = Name -> STM (Maybe (Int -> Maybe Responsabile -> [Evento] -> STM (a,b)))

-- | azione di modifica di uno di eventi o responsabile
update 	
	:: Int 
	-> Update a b -- produce uno stato modificato
	-> Int -- ^ livello standard di caricamento eventi
	-> 	(TVar [Evento]
		,TVar (Maybe Responsabile)
		,TVar Int
		,TVar (Maybe a)
		,TVar (Maybe b)
		,TVar (Maybe Name)
		,TChan Triggers
		,TVar (Maybe (STM (Change c d)))
		,(Name -> STM (Maybe (STM (STM (Change c d))))) -- una condizione esterna che deve fare scattare il rinnovamento
		,(Name -> STM (Maybe (Maybe Utente -> STM [Evento]))) -- gli eventi pubblicati per un utente
		) -- ^ memoria condivisa
	-> STM ()
update z f l (eventi, accesso, conservative, stato, caricamento, gruppo, triggers, signalbox, newsignal, publ) =  do
	let 	-- | update causato da modifica utente
		interna = do 	
				t 	<- readTChan triggers 
				case t of 	
						TResponsabile mr 	->  do
							mg <- readTVar gruppo
							case mg of 
								Nothing -> return ()
								Just g -> do 
									writeTVar accesso mr
									publ g >>= \mp -> case mp of
										Nothing -> return ()
										Just k -> k (fst <$> mr) >>= writeTVar eventi
						TEventi es 		->  writeTVar eventi es
						TConservative l 	->  writeTVar conservative l
						TGruppo n 		-> case n of
							Just g -> newsignal g >>= \mns -> case mns of 
								Just ns -> do
									signal <- ns
									writeTVar signalbox (Just signal)
									writeTVar accesso Nothing
									writeTVar eventi []
									writeTVar conservative l
									writeTVar gruppo n
								Nothing -> return ()
							Nothing -> do	
								writeTVar signalbox Nothing
								writeTVar gruppo Nothing
								writeTVar accesso Nothing
								writeTVar eventi []
								writeTVar conservative l
								writeTVar gruppo n
		esterna = do 	
			g <- readTVar gruppo
			case g of
				Just g -> do 
					msignal <- readTVar signalbox
					case msignal of 
						Just signal -> do 
							s <- signal 
							ess <- readTVar eventi
							mr <- fmap fst <$> readTVar accesso
							case s of
								Boot _ -> return ()
								GPatch digested orphans _ ->  do	
									let 	ofs = maybe [] id $ mr >>= 
											\u -> lookup u orphans 
										dgs = maybe [] id $ mr >>= 
											\u -> lookup u digested
									writeTVar eventi $ (ess `union` ofs) \\ dgs
									writeTVar conservative l
								UPatch u esp ->  do when (Just u == mr) $ 
											writeTVar eventi (esp `union` ess)
						Nothing -> retry
				Nothing -> retry
	interna `orElse` esterna
	g <- readTVar gruppo
	case g of 
		Just g ->  do
			mr 	<- readTVar accesso
			evs 	<- readTVar eventi
			l' 	<- readTVar conservative
			-- effettua il caricamento rispettoso dell condizioni di sessione 
			mu <- f g
			case mu of 
				Nothing -> do
					writeTVar stato Nothing
					writeTVar caricamento Nothing
				Just k -> do 
					(s,c) <- k l' mr evs 	
					writeTVar stato (Just s)
					writeTVar caricamento (Just c)
		Nothing -> do 
			writeTVar stato Nothing
			writeTVar caricamento Nothing

-- | costruisce l'interfaccia di sessione a partire da un modificatore di stato in STM
mkSessione 	:: Update a  b		-- ^ modificatore di stato
		-> Int 			-- ^ livello di caricamento di base
		-> (Name -> STM (Maybe (STM (STM (Change c d)))))		-- ^ segnale di aggiornamento stato
		-> (Name -> STM (Maybe (Maybe Utente -> STM [Evento])))  -- ^ query sugli eventi pubblicati per un utente
		-> STM ()			-- ^ segnale di modifica sessione
		-> Maybe (Maybe Name,[Evento],Maybe Responsabile,Int)
		-> IO (Sessione a b)	
mkSessione f l signal publ exsignal ms =  do
	z <- randomRIO (0,100000) :: IO Int
	(stato,caricamento) 	<- atomically $ do 
					msc <- case ms of
						Nothing -> return Nothing
						Just (Nothing,_, _ ,_) -> return Nothing
						Just (Just g,es,mr,cl) -> f g >>= \x -> case x of 
							Just k -> Just <$> k cl mr es 
							Nothing -> return Nothing
					liftM2 (,) (newTVar $ fst <$> msc ) (newTVar $ snd <$> msc)
	eventi 		<- atomically $ newTVar $ maybe [] (\(_,es,_,_) -> es) ms
	accesso 	<- atomically $ newTVar $ ms >>= \(_,_,mr,_) -> mr
	triggers 	<- atomically $ newTChan
	conservative 	<- atomically $ newTVar $ maybe l (\(_,_,_,cl) -> cl) ms
	gruppo 		<- atomically $ newTVar $ ms >>= \(mg,_,_,_) -> mg
	signalbox 	<- atomically $ case ms of 
		Nothing -> newTVar Nothing
		Just (mg,_,_,_) -> case mg of
			Nothing -> newTVar Nothing 
			Just g -> signal g >>= \ms -> case ms of
				Nothing -> newTVar Nothing
				Just mks -> mks >>= newTVar . Just			
	let	memoria = (eventi, accesso, conservative, stato, caricamento ,gruppo,triggers, signalbox , signal, publ)
		checkUpdate q  = (update z f l memoria >> exsignal >> q) `orElse` q
		write f = atomically . writeTChan triggers . f 
		read t = atomically . checkUpdate $ readTVar t
	return $ Sessione 
		(read gruppo) 
		(write TGruppo)
		(read eventi)
		(write TEventi)
		(read accesso) 
		(write TResponsabile)
		(read caricamento)
		(read stato)
		(write TConservative)
		(read conservative) 
		(atomically $ do
			mg <- readTVar gruppo
			es <- readTVar eventi
			cl <- readTVar conservative
			mr <- readTVar accesso
			return (mg,es,mr,cl))

	
		
	
