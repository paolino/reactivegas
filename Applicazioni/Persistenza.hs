{-# LANGUAGE Rank2Types, TupleSections #-}

-- | Modulo di persistenza del programma.
-- La persistenza ha 2 facce. Ci sono i dati attuali che persistono su file cosi' come sono e i dati storici che persistono su file attraverso sqlite.
module Applicazioni.Persistenza where -- (Persistenza (..), mkPersistenza, Change (..), Modificato) where

import Data.Maybe (fromJust,isJust,isNothing)
import Data.List (sort,find, partition,delete, sortBy, groupBy, union, (\\))
import Data.Ord (comparing)
import Data.Function (on)

import Control.Monad (when, liftM2, forever,mplus, join)
import Control.Monad.Writer (runWriter, Writer)
import Control.Monad.Reader
import Control.Monad.Error
import Control.Applicative ((<$>))
import Control.Arrow ((&&&), (***),second, first)
import Control.Exception (tryJust, SomeException (..))
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM (STM,TVar,TChan,newTVar,newTChan,readTVar,readTChan,writeTVar,writeTChan,dupTChan,retry,atomically)

import System.Random (getStdGen,randomRs)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>), addExtension)
import System.IO (hFlush,stdout)

import Core.Patch (Patch,firma,fromPatch, Group, fromGroup)
import Core.Types (Evento,Utente,Esterno,Responsabile)
import Core.Contesto (Contestualizzato)
import Core.Programmazione (Message)
import Applicazioni.Database.GPatch (mkGPatches, GPatches (..))
import Lib.Modify ( PeekPoke (peek,poke), mkPeekPoke)
import Debug.Trace
import Lib.Filesystem

fJ y x = case x of {Nothing -> error (show y) ; Just x -> x}
-----------------------------------------------------------------------------------------

-- | nome del gruppo
type GName = String



-- | Operazione di persistenza. Scrive un istantanea di (stato,patches) oppure i tokens, quando richiesto dal trigger
persistenza :: Show a	
	=> (forall b . Show b => String -> Int -> b -> IO ())		-- ^ operazione di persistenza
	-> TVar Int 		-- ^ versione dello stato
	-> TVar [(Utente,Patch)]-- ^ associazione utente -> patch individuale
	-> TVar [(Utente,[Evento])] -- ^ eventi orfani
	-> TChan String		-- ^ logger
	-> TVar a		-- ^ stato
	-> TChan () 		-- ^ trigger di persistenza
	-> IO ()

persistenza write tversion tupatch torfani tlog tstato trigger = do
	(version,patches,orfani,stato) <- atomically $ do 
		readTChan trigger 
		version 	<- readTVar tversion
		patches 	<- readTVar tupatch
		orfani 		<- readTVar torfani
		stato		<- readTVar tstato
		return (version,patches,orfani,stato)
	write "patches" version patches
	write "orfani" version orfani
	write "stato.corrente" version stato

-- | Operazione di ripristino. Legge lo stato del gruppo 
ripristino :: (Read a, Show a) 
	=> (forall b . Read b => String -> IO (Maybe (Int, b)))
	-> String 
	-> Persistenza a b d
	-> TVar Int
	-> TVar [(Utente,Patch)]
	-> TVar [(Utente,[Evento])] -- ^ eventi orfani
	-> TChan String
	-> Int 
	-> TVar a
	-> IO ()

ripristino unwrite gname p tversion tupatch torfani tlog uv ts = do					
	let reload = do 
		ms <- groupUnwrite gname "stato.boot"
		case ms of 
			Just (0,s) -> do 
				atomically $ writeTVar ts s
				putStr "aggiornamenti:"
				autofeed p	
				putStrLn "\n"
			Nothing -> do
				s <- atomically $ readTVar ts
				groupWrite gname "stato.boot" 0 s
				putStrLn "stato iniziale scritto"

	ms <- groupUnwrite gname "stato.corrente" 	
	case ms of 
		Just (vc,s) -> do 
			putStrLn $ "rilevato file di stato corrente " ++ show vc
			if vc == uv then 
				atomically $ writeTVar tversion vc >> writeTVar ts s
				else do
					putStrLn $ "incoerenza con aggiornamenti " ++ show (uv,vc)
					reload
		Nothing -> reload

	ps <- unwrite "patches" 
	os <- unwrite "orfani" 
	atomically $ do
		v <- readTVar tversion
		case ps of
			Nothing -> return ()
			Just (v',ps) -> when (v' == v) $ writeTVar tupatch ps
		case os of 
			Nothing -> return ()
			Just (v',os) -> when (v' == v) $ writeTVar torfani os
		writeTVar tversion v

-- | messaggi di avvenuta persistenza
data Change a b	= Boot 	a					-- ^ messaggio di nuovo stato
		-- | arrivato un aggiornamento di gruppo, comunica gli eventi digeriti e quelli orfani
		| GPatch [(Utente,[Evento])] [(Utente,[Evento])] (b, a)
		-- | arrivato un aggiornamento utente, comunica l'autore e gli eventi
		| UPatch Utente [Evento] deriving Show

-- | raggruppa le dichiarazioni per responsabile
groupUp :: [(Utente, Evento)] -> [(Utente, [Evento])]
groupUp = map (fst . head &&& map snd) . groupBy ((==) `on` fst) . sortBy (comparing fst)

-- | appiattisce le dichiarazioni raggruppate per responsabile
groupDown :: [(Utente, [Evento])] -> [[(Utente, Evento)]]
groupDown = map (\(u,es) -> map ((,) u) es)

-- | transazione di aggiornamento provocata dall'arrivo di una patch di gruppo
aggiornamento 
	:: (a -> [(Utente,Evento)] ->  Either String (a, b))  -- ^ tentativo di aggiornamento
	-> TVar Int			-- ^ versione
	-> TVar a		-- ^ stato
	-> TVar [(Utente,Patch)]	-- ^ aggiornamenti individuali
	-> TVar [(Utente,[Evento])] 	-- ^ eventi orfani
	-> TChan String	-- ^ log 
	-> [(Utente,Evento)]	-- ^ aggiornamento di gruppo offerto
	-> (Change a b  -> STM ())
	-> STM ()	-- ^ transazione 

aggiornamento  load  tv ts tp to tl evs k = do
	s <- readTVar ts
	let 	es = load s evs
		digested = groupUp evs
	case es of
		Left e -> writeTChan tl e -- problema di caricamento
		Right (s',ls) -> seq s' $ do
			v <- (+1) <$> readTVar tv
			writeTVar ts s' -- scrive il nuovo stato
			wes <- concatMap (\(u,(_,_,es)) -> map ((,) u) es) <$> readTVar tp 
			os <- concat <$> groupDown <$> readTVar to
			let orphans = groupUp $ (os `union` wes) \\ evs
			writeTVar to orphans -- aggiorna gli orfani
			writeTVar tp []	-- annulla gli aggiornamenti individuali (o erano in 'g' o sono in 'os')
			writeTVar tv v	-- scrive la versione
			k $ GPatch digested orphans $ (ls ,s') 
-- | come esportiamo l'interfaccia di modifica in bianco. Un caricamento in bianco sullo stato attuale 
type Modificato a d 	= Int 			-- ^ livello di caricamento
			-> Maybe Responsabile 	-- ^ responsabile autore o anonimo
			-> [Evento] 		-- ^ dichiarazioni dell'autore
			-> STM (a, d)	-- ^ in STM , lo stato modificato se esiste insieme agli effetti di caricamento


-- | produce l'interfaccia di modifica in bianco	
mkModificato 	:: (Int -> a -> [(Utente,Evento)] -> (a,d)) 
		-> TVar a 
		-> TVar [(Utente,Patch)]
		-> Modificato a d
mkModificato f ts tp l mr es = do
	s <-  readTVar ts
	let 	-- | mappa gli aggiornamenti individuali in una pozza di eventi con autore ((Utente,Evento))
		estraiEventi = concatMap (\(u,(_,_,es)) -> map ((,) u) es)  <$> readTVar tp
		-- | elimina le dichiarazioni dell'autore e inserisce le sue nuove
		eventiAutore u = (++ map ((,) u) es) . filter ((/=) u . fst)
		-- computa la modifica con o senza gli eventi nuovi a seconda della presenza dell'autore
	f l s <$> maybe id (eventiAutore . fst ) mr <$> estraiEventi

-- | strato funzionale di persistenza, lo stato in memoria è pericoloso .....
data Persistenza a b d = Persistenza
		{ 	readStato 	:: IO (Int,a),		-- ^ lettura dello stato
			writeUPatch 	:: Utente -> Patch -> IO (),	-- ^ scrittura di una patch utente
			readUPatches 	:: IO (Int,[(Utente,Patch)]),			-- ^ lettura delle patch utente
			writeGPatch 	:: Group -> IO (),		-- ^ scrittura di una patch di gruppo
			readGPatch	:: Int -> IO (Maybe Group),	-- ^ lettura di una patch di gruppo
			readVersion 	:: IO Int,			-- ^ versione dello stato attuale
			readLogs	:: IO String,
			updateSignal	:: STM (STM (Change a b)),			-- ^ segnala un avvenuto cambiamento di stato
			-- | raccoglie gli eventi relativi ad un utente che sono da attribuire alla persistenza
			queryUtente	:: Maybe Utente -> STM [Evento], 
			caricamentoBianco :: Modificato a d
		}

-- | prepara uno stato vergine di un gruppo, a e' il tipo dello stato, b il tipo degli effetti
mkPersistenza :: (Show c, Eq a, Read a, Show a, Show b) 
	=> (a -> [(Utente,Evento)] -> Either String (a,b)) 	-- ^ loader specifico per a
	-> (Int -> a -> [(Utente,Evento)] -> (a,d))		-- ^ insertore diretto di eventi per a 
	-> a							-- ^ inizializzatore di gruppo
	-> (c -> [Responsabile])
	-> (a -> c)
	-> GName 							-- ^ nome del gruppo
	-> IO (Persistenza a b d,IO ())					

mkPersistenza load modif boot resps ctx gname = do 
	-- istanzia la memoria condivisa
	trigger <- atomically $ newTChan 	
	tv <- atomically $ newTVar 0
	ts <- atomically $ newTVar boot
	tp <- atomically $ newTVar []
	to <- atomically $ newTVar []
	cl <- atomically $ newTChan
	cs <- atomically $ newTChan
	gp <- mkGPatches gname
	uv <- fromInteger`fmap` ultimaGPatch gp
	let	atomicallyP f = atomically $ writeTChan trigger () >> f (writeTChan cs)  
		readStato' 	= atomically $ do 
					s <- readTVar ts
					v <- readTVar tv 
					return (v,s)
		writeUPatch' u p@(_,_,es) = do
			s <- atomically $ readTVar ts
			rl <- runErrorT $ runReaderT (fromPatch resps p) (ctx s)
			case rl of
				Right _ -> do 
					atomicallyP $ \k -> do
						up <- readTVar tp 
						writeTVar tp . (++ if null es then [] else [(u,p)]) . filter ((/=) u . fst) $ up
						or <- readTVar to
						writeTVar to . (++ [(u,[])]) . filter ((/=) u .fst) $ or
						k $ UPatch u es
				Left gname -> atomically $ writeTChan cl  gname
		readUPatches' 	= atomically $ liftM2 (,) (readTVar tv) $ readTVar tp
		writeGPatch' g 	= do 
			s <- atomically $ readTVar ts
			rl <- runErrorT $ runReaderT (fromGroup resps g) (ctx s)
			case rl of
				Right (_,es) -> do 
					v <- atomicallyP $ \f ->  
						aggiornamento load  tv ts tp to cl es f >> readTVar tv
					nuovaGPatch gp (fromIntegral v) g
				Left gname -> atomically $ writeTChan cl gname
		readGPatch' 	= vecchiaGPatch gp . fromIntegral
		readVersion' 	= atomically $ readTVar tv
		readLogs' 	= atomically . readTChan $ cl
		updateSignal'	= dupTChan cs >>= return . readTChan  
		queryUtente (Just u)	= do
			es0 <- fmap (\(_,_,es) -> es) <$> lookup u <$> readTVar tp
			es1 <- lookup u <$> readTVar to
			return . maybe [] id $ es0 `mplus` es1
		queryUtente Nothing = return []
		caricamentoBianco' 	= mkModificato modif ts tp		
		p = Persistenza readStato'  writeUPatch'  
			readUPatches' writeGPatch' readGPatch' readVersion' readLogs' updateSignal' 
			queryUtente caricamentoBianco'
		boot' = do 	
			ripristino (groupUnwrite gname)  gname p tv tp to cl uv ts
			-- thread di persistenza appeso a trigger
			forkIO . forever $ persistenza (groupWrite gname) tv tp to cl ts trigger
			forkIO . forever $ atomically (readTChan cs) -- tiene vuoto cs 
			putStrLn "persistenza attivata"
	return (p,boot')


autofeed :: Persistenza a b d -> IO ()
autofeed p = do
	i <- readVersion p
	mn <- readGPatch p i
	case mn of
		Nothing -> return ()
		Just g -> do
			putStr $ show i ++ ","
			hFlush stdout
			writeGPatch p g
			autofeed p


