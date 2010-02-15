

module Core.Persistenza (Persistenza (..), aggiornamento) where


import Data.List (sort,partition)

import Control.Monad.Writer
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Applicative ((<$>))
import Control.Arrow ((&&&))

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM

import System.Directory (getCurrentDirectory)
import System.FilePath (takeExtension,replaceExtension, (</>))


import Lib.Valuedfiles  (Valuedfile (..), ext, maybeParse, getValuedfiles)
import Lib.Assocs (secondM)

import Core.Patch (Patch,Group)
import Core.Types (Evento)

import Eventi.Anagrafe (Utente)
-----------------------------------------------------------------------------------------

-- | strato funzionale di persistenza
data Persistenza a = Persistenza
		{ 	readStato :: IO a,									-- ^ lettura dello stato
			readEventi :: Utente -> IO [Evento],			-- ^ lettura degli eventi non firmati per un utente
			writeEventi :: Utente -> [Evento] -> IO (),	-- ^ scrittura degli eventi non firmati per un utente
			writeUPatch :: Utente -> Patch -> IO (),		-- ^ scrittura di una patch utente
			readUPatches :: IO [Patch],						-- ^ lettura delle patch utente
			writeGPatch :: Group -> IO (),					-- ^ scrittura di una patch di gruppo
			readLog :: IO String									-- ^ lettura bloccante del prossimo log 
		}

-- | thread di persistenza. Scrive un istantanea dello stato, eventi e patch utente su disco ogni minuto
persistenza 	:: Show a 
					=> FilePath 
					-> TVar Int 
					-> TVar a
					-> TVar [(Utente,TVar [Evento])]
					-> TVar [(Utente,Patch)]
					 -> IO ()
persistenza dir tversion tstato teventi tupatch = forever $ do 
	(version,stato,eventi,patches) <- atomically $ do
		version <- readTVar tversion
		stato <-  readTVar tstato
		eventi <- readTVar teventi >>= mapM (secondM readTVar)
		patches <- readTVar tupatch
		return (version,stato,eventi,patches)
	writeFile (dir </> "stato." ++ show version) $ show stato
	writeFile (dir </> "eventi." ++ show version) $ show eventi
	writeFile (dir </> "patches." ++ show version) $ show patches
	threadDelay 60000000

-- | thread di aggiornamento. Aspetta una patch di gruppo e aggiorna stato, eventi e patch utente quando arriva
aggiornamento 
			:: (a -> Group -> Writer [String] (Either String a)) 
			-> TVar Int
			-> TVar a
			-> TVar [(Utente,TVar [Evento])]
			-> TVar [(Utente,Patch)] 
			-> TChan Group 
			-> TChan String 
			-> IO ()
aggiornamento load tversion tstato teventi tupatch tgpatch tlog = 
	atomically . forever $ do 
		g@(_,_,ps) <- readTChan tgpatch -- aspetta la patch di gruppo e binda le patches	
		s <- readTVar tstato -- stato attuale
		let (eg',ls) = runWriter $ load s g -- prova l'aggiornamento dello stato
		mapM (writeTChan tlog)  ls -- logga l'esecuzione
		case eg' of 
			Left e -> writeTChan tlog e -- logga l'errore, l'aggiornamento e' fallito
			Right s' -> do
				readTVar tversion >>= writeTVar tversion . (+ 1) -- passa a versione sucessiva
				writeTVar tstato s' -- nuovo stato
				(_,os) <- partition ((`elem` ps) . snd ) `fmap` readTVar tupatch -- seleziona le patch individuali incluse
				-- corregge gli eventi di un utente aggiungendo gli esclusi di una patch
				let k evs (u,(_,_,es)) = case lookup u evs of
					Nothing -> do -- l'utente non ha eventi registrati
						n <- newTVar es 
						return ((u,n):evs) -- aggiunge il record per l'utente
					Just tes' -> do -- l'utente ha un registro
						es' <- readTVar tes'
						writeTVar tes' (es ++ es') -- aggiunge gli eventi nuovi ai vecchi
						return evs
				eventi <- readTVar teventi
				foldM k eventi os -- cicla sulle patch escluse
				writeTVar tupatch [] -- annulla il registro patch individuali

-- | thread di logging s console
logger :: TChan String -> IO ()
logger tlog = forever $ atomically (readTChan tlog) >>= putStrLn

-- | costruisce una persistenza a partire dallo stato e dalla versione, spawna i thread di persistenza e aggiornamento
mkPersistenza 	:: Show a
					=> (a -> Group -> Writer [String] (Either String a)) 
					-> FilePath 
					-> TChan String
					-> Int 
					-> a 
					-> IO (Persistenza a)
mkPersistenza load wd tlog ver stato = do
	let log = atomically . writeTChan tlog  --  helper per il log

	tstato <- atomically $ newTVar stato
	log  " Caricato il file di stato "

	tversion <- atomically $ newTVar ver
	log $ " Versione dello stato :" ++ show ver

	ps <- map value . filter ((==) (ver + 1) . ext) <$> liftIO (getValuedfiles maybeParse "patches" wd)
	ps' <- case ps of 
		[] -> do
	 		log  " Non rilevati aggiornamenti individuali "
	 		return []
		(x:_) ->  do
 			log  " Rilevati aggiornamenti individuali "
 			return x
	tupatch <- atomically . newTVar $ ps'

	rs <- map value . filter ((==) (ver + 1) . ext) <$> liftIO (getValuedfiles maybeParse "eventi" wd)
	rs' <- case rs of 
		[] -> do
			log " Non rilevati eventi "
			return []
		(x:_) ->  do
			log " Rilevati eventi "
			atomically . mapM (secondM newTVar) $ x
	teventi <- atomically . newTVar $ rs'

	tgpatch <- atomically newTChan

	forkIO $ aggiornamento load tversion tstato teventi tupatch tgpatch tlog 
	forkIO $ persistenza wd tversion tstato teventi tupatch 

	return $ Persistenza
		(atomically $ readTVar tstato)
		(\u -> atomically $ readTVar teventi >>= maybe (return []) readTVar . lookup u)
		(\u es -> atomically $ do 
			ues <- readTVar teventi 
			ues' <- case lookup u ues of
				Nothing -> do
					n <- newTVar es
					return $ (u,n) : ues 
				Just tes -> do
					writeTVar tes es
					return ues
			writeTVar teventi ues'
			)
		(\u p -> atomically $ readTVar tupatch >>= \ups -> writeTVar tupatch (ups ++ [(u,p)]))
		(atomically $ map snd `fmap` readTVar tupatch)
		(atomically . writeTChan tgpatch)
		(atomically $ readTChan tlog)
				  
-- | l'operazione di wake puo equivalere ad un boot se non trova il file di stato o ad un resume se lo stato esiste
data Wake a	
	= Boot (a -> IO (Persistenza a)) 	-- ^ serve ancora lo stato iniziale
	| Resume (Persistenza a)				-- ^ tutto in ordine
					
-- | operazione di risveglio
wake ::  (Show a, Read a)
			=> (a -> Group -> Writer [String] (Either String a))  -- ^ la funzione che aggiorna uno stato
			-> Maybe FilePath	-- ^ la directory di lavoro
			-> IO (Wake a) -- ^ strato di persistenza

wake load mf = do
	-- crea un canale duplicato per il logging
	(tlog,tlog') <- atomically $ do
			t1 <- newTChan
			t2 <- dupTChan t1
			return (t1,t2)

	forkIO $ logger tlog' -- logga in console

	let log = atomically . writeTChan tlog  --  helper per il log

	wd <- maybe getCurrentDirectory return mf -- impone la directory corrente se non è stata fornita una
	log $ " Cartella di lavoro: " ++ wd

	-- colleziona i file di stato
	stati <- reverse . sort <$> getValuedfiles maybeParse "stato" wd

	if null stati then do
		log " Non rilevato alcun stato"
		return . Boot $ mkPersistenza load wd tlog 0 
		else Resume `fmap` mkPersistenza load wd tlog (ext $ head stati) (value $ head stati)


		

