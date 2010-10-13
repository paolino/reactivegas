{-# LANGUAGE Rank2Types #-}

-- | Modulo di persistenza del programma.
-- La persistenza ha 2 facce. Ci sono i dati attuali che persistono su file cosi' come sono e i dati storici che persistono su file attraverso sqlite.
module Applicazioni.Persistenza (Persistenza (..), mkPersistenza, Change (..), Modificato) where

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

import Debug.Trace

fJ y x = case x of {Nothing -> error (show y) ; Just x -> x}
-----------------------------------------------------------------------------------------

-- | nome del gruppo
type GName = String


---------------------- filesystem ll -------------------------------------------------------------------------------
-- | scrive un dato riguardante un gruppo
groupWrite :: Show a => GName -> String -> Int -> a -> IO ()
groupWrite x y v t = do 
	r <- tryJust (\(SomeException x) -> Just $ show x) (writeFile (x </> y) . show $ (v,t)) 
	either putStrLn return r

-- | legge un dato riguardante un gruppo
groupUnwrite :: Read a => GName -> String -> IO (Maybe (Int,a))
groupUnwrite x y  = do
	r <- tryJust (\(SomeException x) -> Just $ show x) (readFile (x </> y))
	return $ case r of 
		Left _ -> Nothing
		Right z -> case reads z of 
			[(q,_)] -> last z `seq` Just q
			_ -> Nothing
-------------------------------------------------------------------------------------------------------------------

type Token = String

-- | Operazione di persistenza. Scrive un istantanea di (stato,patches) oppure i tokens, quando richiesto dal trigger
persistenza 	:: Show a
	=> (forall b . Show b => String -> Int -> b -> IO ())		-- ^ operazione di persistenza
	-> TVar Int 		-- ^ versione dello stato
	-> TVar (Maybe a)
	-> TVar [(Utente,Patch)]-- ^ associazione utente -> patch individuale
	-> TVar [(Utente,[Evento])] -- ^ eventi orfani
	-> TChan String		-- ^ logger
	-> TVar (Token,[Token],[Responsabile])	-- ^ tokens di boot
	-> TChan () 		-- ^ trigger di persistenza
	-> IO ()

persistenza write tversion tstato tupatch torfani tlog ttokens trigger = do
	(stato,version,patches,orfani,tokens) <- atomically $ do 
		readTChan trigger 
		version 	<- readTVar tversion
		patches 	<- readTVar tupatch
		orfani 		<- readTVar torfani
		tokens		<- readTVar ttokens
		stato 		<- readTVar tstato
		return (stato,version,patches,orfani,tokens)
	when (isJust stato) $ do 
		write "patches" version patches
		write "orfani" version orfani
	when (isNothing stato) $ do
		write "tokens" 0 tokens
	

-- | Operazione di ripristino. Legge lo stato del gruppo 
ripristino
	:: (Eq a, Read a, Show a) =>  (forall b . Read b => String -> IO (Maybe (Int, b)))
	-> TVar Int
	-> TVar (Maybe a)
	-> TVar [(Utente,Patch)]
	-> TVar [(Utente,[Evento])] -- ^ eventi orfani
	-> TChan String
	-> TVar (Token, [Token],[Responsabile])	-- ^ tokens di boot
	-> IO ()

ripristino unwrite tversion tstato tupatch torfani tlog ttokens = do
	mt <- unwrite "tokens" 
	ps <- unwrite "patches" 
	os <- unwrite "orfani" 
	atomically $ do 
		ms <- readTVar tstato
		case ms of
			Nothing -> case mt of
					Nothing -> return () 
					Just (_,(p,x,y)) -> writeTVar ttokens (p,x,y)
			Just x -> do
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
	-> TVar (Maybe a)		-- ^ stato
	-> TVar [(Utente,Patch)]	-- ^ aggiornamenti individuali
	-> TVar [(Utente,[Evento])] 	-- ^ eventi orfani
	-> TChan String	-- ^ log 
	-> [(Utente,Evento)]	-- ^ aggiornamento di gruppo offerto
	-> (Change a b  -> STM ())
	-> STM ()	-- ^ transazione 

aggiornamento  load  tv ts tp to tl evs k = do
	s' <- readTVar ts
	when (isNothing s') retry -- fallisce finche' non esiste uno stato
	let 	Just s = s'
	 	es = load s evs
		digested = groupUp evs
	case es of
		Left e -> writeTChan tl e -- problema di caricamento
		Right (s',ls) -> seq s' $ do
			v <- (+1) <$> readTVar tv
			writeTVar ts $ Just s' -- scrive il nuovo stato
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
			-> STM (Maybe a, d)	-- ^ in STM , lo stato modificato se esiste insieme agli effetti di caricamento


-- | produce l'interfaccia di modifica in bianco	
mkModificato 	:: (Int -> a -> [(Utente,Evento)] -> (a,d)) 
		-> TVar (Maybe a) 
		-> TVar [(Utente,Patch)]
		-> Modificato a d
mkModificato f ts tp l mr es = do
	ms <-  readTVar ts
	let 	-- | mappa gli aggiornamenti individuali in una pozza di eventi con autore ((Utente,Evento))
		estraiEventi = concatMap (\(u,(_,_,es)) -> map ((,) u) es)  <$> readTVar tp
		-- | elimina le dichiarazioni dell'autore e inserisce le sue nuove
		eventiAutore u = (++ map ((,) u) es) . filter ((/=) u . fst)
	case ms of
		Nothing -> return (Nothing, error "nessuno stato") -- richiesta di modifica in bianco 	di uno stato inesistente
		-- lo stato esiste , computa la modifica con o senza gli eventi nuovi a seconda della presenza dell'autore
		Just s -> first Just <$> f l s <$> maybe id (eventiAutore . fst ) mr <$> estraiEventi

-- | strato funzionale di persistenza, lo stato in memoria è pericoloso .....
data Persistenza a b d = Persistenza
		{ 	readStato 	:: IO (Maybe (Int,a)),		-- ^ lettura dello stato
			assignToken	:: Token -> Responsabile -> IO Bool, -- ^ tenta l'assegnamento di un token
			readBoot	:: IO [Responsabile],	-- ^ elenco dei responsabili di boot
			moreTokens	:: Token -> Int -> IO (Maybe ()),	-- ^ richiede altri tokens
			readTokens	:: Token -> IO (Maybe [Token]), -- ^ elenco tokens mancanti
			forceBoot	:: Token -> IO (Maybe ()),	-- ^ forza la chiusura della fase di boot
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


-- | prepara uno stato vergine di un gruppo
mkPersistenza :: (Eq a, Read a, Show a, Show c, Show b) 
	=> Token						-- ^ token di amministrazione fase di boot
	-> (a -> [(Utente,Evento)] -> Either String (a,b)) 	-- ^ loader specifico per a
	-> (Int -> a -> [(Utente,Evento)] -> (a,d))		-- ^ insertore diretto di eventi per a 
	-> ([Responsabile] -> a)				-- ^ inizializzatore di gruppo
	-> (c -> [Responsabile])
	-> (a -> c)
	-> GName 							-- ^ nome del gruppo
	-> IO (Persistenza a b d,IO ())					

mkPersistenza pass load modif boot resps ctx x = do 
	-- istanzia la memoria condivisa
	trigger <- atomically $ newTChan 	
	tb <- atomically $ newTVar (pass,[],[])
	tv <- atomically $ newTVar 0
	ts <- atomically $ newTVar Nothing
	tp <- atomically $ newTVar []
	to <- atomically $ newTVar []
	cl <- atomically $ newTChan
	cs <- atomically $ newTChan
	gp <- mkGPatches x
	let	atomicallyP f = atomically $ writeTChan trigger () >> f (writeTChan cs)  
		readStato' 	= atomically $ do 
					s <- readTVar ts
					case s of 
						Nothing -> return Nothing
						Just s -> readTVar tv >>= \v -> return (Just (v,s))
		assignToken' t  r	= atomicallyP $ \k -> do
			(p,us,as) <- readTVar tb 
			if t `elem` us then do
				let 	us' = delete t us
					as' = r:as
				writeTVar tb (p,us',as')
				when (null us') $ do
					let s = boot as'
					writeTVar ts $ Just s
					k (Boot s)
				return True
				else return False

		forceBoot' t = atomicallyP $ \k -> do
			(p,_,as) <- readTVar tb 
			if t == p then do 
				let s = boot as
				writeTVar ts $ Just s
				k (Boot s)
				return (Just ())
				else return Nothing
		moreTokens' t n = do 
			g <- getStdGen 
			atomically $ do
				(p,us,as) <- readTVar tb 
				if t == p then do 
					let ts = map show . take n $ randomRs (0,1000000000::Int) g
					writeTVar tb (p, us ++ ts,as)  
					return $ Just ()
					else return Nothing

		readBoot' 	= atomically $ do 
					(_,_,rs) <- readTVar tb
					return rs
		readTokens' t	 = atomically $ do
					(p,ts,_) <- readTVar tb
					return $ if t == p then Just ts else Nothing
		writeUPatch' u p@(_,_,es) = do
			s <- atomically $ fJ "Persistenza" <$> readTVar ts
			rl <- runErrorT $ runReaderT (fromPatch resps p) (ctx s)
			case rl of
				Right _ -> do 
					atomicallyP $ \k -> do
						up <- readTVar tp 
						writeTVar tp . (++ if null es then [] else [(u,p)]) . filter ((/=) u . fst) $ up
						or <- readTVar to
						writeTVar to . (++ [(u,[])]) . filter ((/=) u .fst) $ or
						k $ UPatch u es
				Left x -> atomically $ writeTChan cl  x
		readUPatches' 	= atomically $ liftM2 (,) (readTVar tv) $ readTVar tp
		writeGPatch' g 	= do 
			s <- atomically $ fJ "Persistenza" <$> readTVar ts
			rl <- runErrorT $ runReaderT (fromGroup resps g) (ctx s)
			case rl of
				Right (_,es) -> do 
					v <- atomicallyP $ \f ->  
						aggiornamento load  tv ts tp to cl es f >> readTVar tv
					nuovaGPatch gp (fromIntegral v) g
				Left x -> atomically $ writeTChan cl x
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
		p = Persistenza readStato' assignToken' readBoot' moreTokens' readTokens'  forceBoot' writeUPatch'  
			readUPatches' writeGPatch' readGPatch' readVersion' readLogs' updateSignal' 
			queryUtente caricamentoBianco'
		boot' = do 	
			ms <- groupUnwrite x "stato.boot" 	
			case ms of 
				Just (_,s) -> 	do 
					putStrLn "rilevato file di stato iniziale"
					atomically $ writeTVar ts $ Just s
					putStr "aggiornamenti:"
					autofeed p	
					putStrLn "\n"
					ripristino (groupUnwrite x) tv ts tp to cl tb
				Nothing -> do 
					putStrLn "stato iniziale assente"
					forkIO $ do 
						r <- atomically updateSignal' >>= atomically
						case r of
							Boot s -> do
								groupWrite x "stato.boot" 0  s
								putStrLn "stato iniziale scritto"
							_ -> print "absurd"			 
					return ()
			-- thread di persistenza appeso a trigger
			forkIO . forever $ persistenza (groupWrite x) tv ts tp to cl tb trigger
			forkIO . forever $ atomically (readTChan cs) 
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



		

