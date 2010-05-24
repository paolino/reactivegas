{-# LANGUAGE Rank2Types #-}

-- | modulo di gestione del filesystem
module Applicazioni.Persistenza (Persistenza (..), mkPersistenza, Change (..), Modificato) where

import Data.Maybe (fromJust,isJust,isNothing)
import Data.List (sort,find, partition,delete)

import Control.Monad (when, liftM2, forever,mplus)
import Control.Monad.Writer (runWriter, Writer)
import Control.Applicative ((<$>))
import Control.Arrow ((&&&), (***),second, first)
import Control.Exception (tryJust, SomeException (..))
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM (STM,TVar,TChan,newTVar,newTChan,readTVar,readTChan,writeTVar,writeTChan,dupTChan,retry,atomically)

import System.Random (getStdGen,randomRs)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>), addExtension)
import System.IO

import Text.XHtml hiding ((</>),value)

import Lib.Assocs (secondM)
import Lib.Firmabile (Firma)

import Core.Patch (Patch,firma,Group)
import Core.Types (Evento,Utente,Esterno,Responsabile)
import Core.Contesto (Contestualizzato)
import Core.Programmazione (Message)
import Applicazioni.Database.GPatch (mkGPatches, GPatches (..))

import Debug.Trace
-----------------------------------------------------------------------------------------

-- | nome del gruppo
type GK = String


---------------------- filesystem ll -------------------------------------------------------------------------------
-- | scrive un dato riguardante un gruppo
groupWrite :: Show a => GK -> String -> Int -> a -> IO ()
groupWrite x y v t = do 
	r <- tryJust (\(SomeException x) -> Just $ show x) (writeFile (x </> y) . show $ (v,t)) 
	either putStrLn return r

-- | legge un dato riguardante un gruppo
groupUnwrite :: Read a => GK -> String -> IO (Maybe (Int,a))
groupUnwrite x y  = do
	r <- tryJust (\(SomeException x) -> Just $ show x) (readFile (x </> y))
	return $ case r of 
		Left _ -> Nothing
		Right z -> last z `seq` case reads z of 
			[(q,_)] -> Just q
			_ -> Nothing
-------------------------------------------------------------------------------------------------------------------


type Token = String

-- | Operazione di persistenza. Scrive un istantanea dello stato, patch utente 
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
	atomically $ readTChan trigger 
	(version,patches,orfani,tokens) <- atomically $ do
		version 	<- readTVar tversion
		patches 	<- readTVar tupatch
		orfani 		<- readTVar torfani
		tokens		<- readTVar ttokens
		return (version,patches,orfani,tokens)
	stato <- atomically $ readTVar tstato
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
	ms <- atomically $ readTVar tstato
	case ms of
		Nothing -> do 
			mt <- unwrite "tokens" 
			atomically $ case mt of
				Nothing -> return () 
				Just (_,(p,x,y)) -> writeTVar ttokens (p,x,y)
		Just x -> do
			v <- atomically $ readTVar tversion
			ps <- unwrite "patches" 
			os <- unwrite "orfani" 
			atomically $ do
				case ps of
					Nothing -> return ()
					Just (v',ps) -> when (v' == v) $ writeTVar tupatch ps
				case os of 
					Nothing -> return ()
					Just (v',os) -> when (v' == v) $ writeTVar torfani os
				writeTVar tversion v

type Effetti = [Contestualizzato Utente Message]

data Change a	= Boot 	a					-- ^ messaggio di nuovo stato
		-- | arrivato un aggiornamento di gruppo, comunica gli eventi digeriti e quelli orfani
		| GPatch [(Utente,[Evento])] [(Utente,[Evento])] (Effetti, a)
		-- | arrivato un aggiornamento utente, comunica l'autore e gli eventi
		| UPatch Utente [Evento]


-- | forse non serve ad una mazza
mix :: [(Utente,[Evento])] -> [(Utente,[Evento])] -> [(Utente,[Evento])] 
mix [] [] = []
mix [] xs = xs
mix ((u,y):ys) xs = case lookup u xs of 
	Nothing -> mix ys $ (u,y):xs
	Just _ -> mix ys $ (u,y): filter ((/=) u . fst) xs

-- | transazione di aggiornamento provocata dall'arrivo di una patch di gruppo
aggiornamento 
	:: (a -> Group ->  Either String (a, Effetti))  -- ^ tentativo di aggiornamento
	-> TVar Int			-- ^ versione
	-> TVar (Maybe a)		-- ^ stato
	-> TVar [(Utente,Patch)]	-- ^ aggiornamenti individuali
	-> TVar [(Utente,[Evento])] 	-- ^ eventi orfani
	-> TChan String	-- ^ log 
	-> Group	-- ^ aggiornamento di gruppo offerto
	-> (Change a  -> STM ())
	-> STM ()	-- ^ transazione 

aggiornamento  load  tv ts tp to tl g@(_,_,ps) k = do
	s' <- readTVar ts
	when (isNothing s') retry -- fallisce finche non esiste uno stato
	let 	Just s = s'
	 	es = load s g
	case es of
		Left e -> 	writeTChan tl e -- problema di caricamento
		Right (s',ls) -> do
			v <- (+1) <$> readTVar tv
			writeTVar ts $ Just s' -- scrive il nuovo stato
			(pos,dps) <- partition (not . (`elem` ps) . snd) <$> readTVar tp -- cerca gli aggiornamenti individuali mancanti
			os <- readTVar to
			let 	orphans =  map (second $ \(_,_,evs) -> evs) pos `mix` os
			 	digested = map (second $ \(_,_,evs) -> evs) dps
			writeTVar to $ filter (not . null . snd) orphans -- aggiorna gli orfani
			writeTVar tp []	-- annulla gli aggiornamenti individuali (o erano in 'g' o sono in 'os')
			writeTVar tv v	-- scrive la versione
			k $ GPatch digested orphans $ (ls ,s') 
-- | come esportiamo l'interfaccia di modifica in bianco. Un caricamento in bianco sullo stato attuale 
type Modificato a b 	= Int 			-- ^ livello di caricamento
			-> Maybe Responsabile 	-- ^ responsabile autore o anonimo
			-> [Evento] 		-- ^ dichiarazioni dell'autore
			-> STM (Maybe a, b)	-- ^ in STM , lo stato modificato se esiste insieme agli effetti di caricamento


-- | produce l'interfaccia di modifica in bianco	
mkModificato 	:: (Int -> a -> [Esterno Utente] -> (a,b)) 
		-> TVar (Maybe a) 
		-> TVar [(Utente,Patch)]
		-> Modificato a b
mkModificato f ts tp l mr es = do
	ms <-  readTVar ts
	let 	-- | mappa gli aggiornamenti individuali in una pozza di eventi con autore (Esterno Utente)
		estraiEventi = concatMap (\(u,(_,_,es)) -> map ((,) u) es)  <$> readTVar tp
		-- | elimina le dichiarazioni dell'autore e inserisce le sue nuove
		eventiAutore u = (++ map ((,) u) es) . filter ((/=) u . fst)
	case ms of
		Nothing -> return (Nothing, error "nessuno stato") -- richiesta di modifica in bianco 	di uno stato inesistente
		-- lo stato esiste , computa la modifica con o senza gli eventi nuovi a seconda della presenza dell'autore
		Just s -> first Just <$> f l s <$> maybe id (eventiAutore . fst ) mr <$> estraiEventi

-- | strato funzionale di persistenza, lo stato in memoria è pericoloso .....
data Persistenza a b = Persistenza
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
			updateSignal	:: IO (STM (Change a)),			-- ^ segnala un avvenuto cambiamento di stato
			-- | raccoglie gli eventi relativi ad un utente che sono da attribuire alla persistenza
			queryUtente	:: Maybe Utente -> STM [Evento], 
			caricamentoBianco :: Modificato a b
		}


-- | prepara uno stato vergine di un gruppo
mkPersistenza :: (Eq a, Read a, Show a) 
	=> Token						-- ^ token di amministrazione fase di boot
	-> (a -> Group -> Either String (a, Effetti)) 	-- ^ loader specifico per a
	-> (Int -> a -> [Esterno Utente] -> (a,b))		-- ^ insertore diretto di eventi per a 
	-> ([Responsabile] -> a)				-- ^ inizializzatore di gruppo
	-> GK 							-- ^ nome del gruppo
	-> IO (Persistenza a b,IO ())					

mkPersistenza pass load modif boot x = do 
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
	let		-- scatena la persistenza in chiusura della transazione
			atomicallyP f = atomically $ writeTChan trigger () >> f (writeTChan cs)  
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
			writeUPatch' u p@(_,_,es) = atomicallyP $ \k -> do
				up <- readTVar tp 
				writeTVar tp . (++ if null es then [] else [(u,p)]) . filter ((/=) u . fst) $ up
				or <- readTVar to
				writeTVar to . (++ [(u,[])]) . filter ((/=) u .fst) $ or
				k $ UPatch u es
			readUPatches' 	= atomically $ liftM2 (,) (readTVar tv) $ readTVar tp
			writeGPatch' g 	= do 
				v <- atomicallyP $ \f -> aggiornamento load  tv ts tp to cl g f >> readTVar tv
				nuovaGPatch gp (fromIntegral v) g
			readGPatch' 	= vecchiaGPatch gp . fromIntegral
			readVersion' 	= atomically $ readTVar tv
			readLogs' 	= atomically . readTChan $ cl
			updateSignal'	= atomically $ dupTChan cs >>= return . readTChan  
			queryUtente (Just u)	= do
				es0 <- fmap (\(_,_,es) -> es) <$> lookup u <$> readTVar tp
				es1 <- lookup u <$> readTVar to
				return . maybe [] id $ es0 `mplus` es1
			queryUtente Nothing = return []
			caricamentoBianco' 	= mkModificato modif ts tp
			
	let 	p = Persistenza 	readStato' assignToken' readBoot' moreTokens' readTokens'  forceBoot'
				writeUPatch' readUPatches' writeGPatch' 
				readGPatch' readVersion' readLogs' updateSignal' queryUtente caricamentoBianco'
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
						r <- atomically $ dupTChan cs >>= readTChan
						case r of
							Boot s -> do
								groupWrite x "stato.boot" 0  s
								putStrLn "stato iniziale scritto"
							_ -> return ()
					return ()
			-- thread di persistenza appeso a trigger
			forkIO $ do
				forever $ persistenza (groupWrite x) tv ts tp to cl tb trigger
			putStrLn "persistenza attivata"
	return (p,boot')


autofeed :: Persistenza a b -> IO ()
autofeed p = do
	i <- readVersion p
	mn <- readGPatch p i
	case mn of
		Nothing -> return ()
		Just g -> do
			putStr $ show i ++ ","
			hFlush stdout
			writeGPatch p g >> autofeed p



		

