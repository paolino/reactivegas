{-# LANGUAGE Rank2Types #-}

-- | modulo di gestione del filesystem
module Applicazioni.Persistenza (Persistenza (..), mkPersistenza, Change (..), Modificato) where

import Data.Maybe (fromJust,isJust,isNothing)
import Data.List (sort,find, partition)

import Control.Monad (when, liftM2, forever,mplus)
import Control.Monad.Writer (runWriter, Writer)
import Control.Applicative ((<$>))
import Control.Arrow ((&&&), (***),second, first)
import Control.Exception (tryJust, SomeException (..))
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM (STM,TVar,TChan,newTVar,newTChan,readTVar,readTChan,writeTVar,writeTChan,dupTChan,retry,atomically)


import System.Directory (getCurrentDirectory)
import System.FilePath ((</>), addExtension)

import Text.XHtml hiding ((</>),value)

import Lib.Valuedfiles  (Valuedfile (..), maybeParse, getValuedfiles, keepSpecific, keepNewest)
import Lib.Assocs (secondM)
import Lib.Firmabile (Firma)

import Core.Patch (Patch,firma,Group)
import Core.Types (Evento,Utente,Esterno,Responsabile)



import Debug.Trace
-----------------------------------------------------------------------------------------

-- | nome del gruppo
type GK = String


---------------------- filesystem ll -------------------------------------------------------------------------------
-- | scrive un dato riguardante un gruppo
groupWrite :: Show a => GK -> String -> Int -> a -> IO ()
groupWrite x y v t = do 
	r <- tryJust (\(SomeException x) -> Just $ show x) (writeFile (x </> addExtension y (show v)) . show $ t) 
	either putStrLn return r

extValue vf = case vf of 
	Nothing -> return Nothing
	Just vf -> do
		v <- value vf
		return . Just $ (ext vf ,v)
-- | legge un dato riguardante un gruppo
groupUnwrite :: Read a => GK -> String -> Maybe Int -> IO (Maybe (Int,a))
groupUnwrite x y mv = do
		stati <-  getValuedfiles maybeParse y x
		maybe (keepNewest stati) (keepSpecific stati) mv >>= extValue 
-------------------------------------------------------------------------------------------------------------------



-- | Operazione di persistenza. Scrive un istantanea dello stato, patch utente 
persistenza 	:: Show a
	=> (forall b . Show b => String -> Int -> b -> IO ())		-- ^ operazione di persistenza
	-> TVar Int 		-- ^ versione dello stato
	-> TVar (Maybe a)	-- ^ stato
	-> TVar [(Utente,Patch)]-- ^ associazione utente -> patch individuale
	-> TVar [(Utente,[Evento])] -- ^ eventi orfani
	-> TVar [(Int,Group)]		-- ^ ultimi aggiornamenti di gruppo
	-> TChan String		-- ^ logger
	-> TChan () 		-- ^ trigger di persistenza
	-> IO ()

persistenza write tversion tstato tupatch torfani tgroup tlog trigger = do
	atomically $ readTChan trigger 
	(version,stato,patches,groups,orfani) <- atomically $ do
		version 	<- readTVar tversion
		stato 		<- readTVar tstato
		patches 	<- readTVar tupatch
		orfani 		<- readTVar torfani
		groups 		<- readTVar tgroup
		return (version,stato,patches, groups,orfani)
	when (isJust stato) $ do 
		write "stato" version (fromJust stato)
		write "patches" version patches
		write "orfani" version orfani
		write "groups" version groups
	

-- | Operazione di ripristino. Legge lo stato del gruppo 
ripristino
	:: (Eq a, Read a, Show a) =>  (forall b . Read b => String -> Maybe Int -> IO (Maybe (Int, b)))
	-> TVar Int
	-> TVar (Maybe a)
	-> TVar [(Utente,Patch)]
	-> TVar [(Utente,[Evento])] -- ^ eventi orfani
	-> TVar [(Int,Group)]
	-> TChan String
	-> IO ()

ripristino unwrite tversion tstato tupatch torfani tgroup tlog = do
	ms <- unwrite "stato" Nothing
	case ms of
		Nothing -> atomically $ do
			writeTChan tlog "nessuno stato per questo gruppo"
			writeTVar tstato  Nothing
			writeTVar tupatch []
			writeTVar torfani []
			writeTVar tgroup []
			writeTVar tversion 0
		Just (v,x) -> do
			ps <- unwrite "patches" (Just v)
			os <- unwrite "orfani" (Just v)
			gs <- unwrite "groups" (Just v)
			atomically $ do
				writeTChan tlog $ "rilevato stato " ++ show v ++ " per questo gruppo"
				writeTVar tstato . seq (x == x) $ Just x
				when (isJust ps) $ do
					writeTVar tupatch . snd . fromJust $ ps
					writeTChan tlog $ "rilevati aggiornamenti utente " ++ show (length . snd . fromJust $ ps)
				when (isJust os) $ do
					writeTVar torfani . snd . fromJust $ os
					writeTChan tlog $ "rilevati eventi orfani  " ++ show (length . snd . fromJust $ os)
				when (isJust gs) $ do
					writeTVar tgroup . snd . fromJust $ gs
					writeTChan tlog $ "rilevato storico aggiornamenti di gruppo " ++ show (length . snd . fromJust $ gs)

				writeTVar tversion v

data Change 	= Boot 						-- ^ messaggio di nuovo stato
		-- | arrivato un aggiornamento di gruppo, comunica gli eventi digeriti e quelli orfani
		| GPatch [(Utente,[Evento])] [(Utente,[Evento])]  
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
	:: Int 	-- ^ dimensione della coda di aggiornamenti
	-> (a -> Group -> Writer [String] (Either String a))  -- ^ tentativo di aggiornamento
	-> TVar Int			-- ^ versione
	-> TVar (Maybe a)		-- ^ stato
	-> TVar [(Utente,Patch)]	-- ^ aggiornamenti individuali
	-> TVar [(Utente,[Evento])] 	-- ^ eventi orfani
	-> TVar [(Int,Group)]		-- ^ storico
	-> TChan String	-- ^ log 
	-> Group	-- ^ aggiornamento di gruppo offerto
	-> (Change -> STM ())
	-> STM ()	-- ^ transazione 

aggiornamento n load  tv ts tp to tg tl g@(_,_,ps) k = do
	s' <- readTVar ts
	when (isNothing s') retry -- fallisce finche non esiste uno stato
	let 	Just s = s'
	 	(es,ls) = runWriter $ load s g
	case es of
		Left e -> 	writeTChan tl e -- problema di caricamento
		Right s' -> do
			v <- (+1) <$> readTVar tv
			gs <- readTVar tg
			writeTVar tg (take n $ (v - 1,g) : gs) -- inserisce l'aggiornamento nello storico
			writeTVar ts $ Just s' -- scrive il nuovo stato
			(pos,dps) <- partition (not . (`elem` ps) . snd) <$> readTVar tp -- cerca gli aggiornamenti individuali mancanti
			os <- readTVar to
			let 	orphans =  map (second $ \(_,_,evs) -> evs) pos `mix` os
			 	digested = map (second $ \(_,_,evs) -> evs) dps
			writeTVar to orphans -- aggiorna gli orfani
			writeTVar tp []	-- annulla gli aggiornamenti individuali (o erano in 'g' o sono in 'os')
			writeTVar tv v	-- scrive la versione
			k $ GPatch digested orphans
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
			writeStato 	:: IO (),			-- ^ scrittura dello stato
			readBoot	:: IO [Responsabile],
			writeBoot	:: [Responsabile] -> IO (),
			writeUPatch 	:: Utente -> Patch -> IO (),	-- ^ scrittura di una patch utente
			readUPatches 	:: IO (Int,[(Utente,Patch)]),			-- ^ lettura delle patch utente
			writeGPatch 	:: Group -> IO (),		-- ^ scrittura di una patch di gruppo
			readGPatch	:: Int -> IO (Maybe Group),	-- ^ lettura di una patch di gruppo
			readVersion 	:: IO Int,			-- ^ versione dello stato attuale
			readLogs	:: IO String,
			updateSignal	:: IO (STM Change),			-- ^ segnala un avvenuto cambiamento di stato
			-- | raccoglie gli eventi relativi ad un utente che sono da attribuire alla persistenza
			queryUtente	:: Maybe Utente -> STM [Evento], 
			caricamentoBianco :: Modificato a b
		}

-- | prepara uno stato vergine di un gruppo
mkPersistenza :: (Eq a, Read a, Show a) 
	=> (a -> Group -> Writer [String] (Either String a)) 	-- ^ loader specifico per a
	-> (Int -> a -> [Esterno Utente] -> (a,b))		-- ^ insertore diretto di eventi per a 
	-> ([Responsabile] -> a)				-- ^ inizializzatore di gruppo
	-> GK 							-- ^ nome del gruppo
	-> Int 							-- ^ coda di aggiornamenti di gruppo
	-> IO (Persistenza a b)					

mkPersistenza load modif boot x n = do 
	-- istanzia la memoria condivisa
	trigger <- atomically $ newTChan 	
	tb <- atomically $ newTVar []
	tv <- atomically $ newTVar 0
	ts <- atomically $ newTVar Nothing
	tp <- atomically $ newTVar []
	to <- atomically $ newTVar []
	tg <- atomically $ newTVar [] 
	cl <- atomically $ newTChan
	cs <- atomically $ newTChan
	-- esegue il ripristino
	ripristino (groupUnwrite x) tv ts tp to tg cl
	-- thread di persistenza appeso a trigger
	forkIO . forever $ persistenza (groupWrite x) tv ts tp to tg cl trigger
	-- azioni del record di persistenza
	
	let		-- scatena la persistenza in chiusura della transazione
			atomicallyP f = atomically $ f (writeTChan cs) >> writeTChan trigger () 
			readStato' 	= atomically $ do 
						s <- readTVar ts
						case s of 
							Nothing -> return Nothing
							Just s -> readTVar tv >>= \v -> return (Just (v,s))
			writeStato' 	= atomicallyP $ \k -> readTVar tb >>= writeTVar ts . Just . boot >> k Boot
			readBoot' 	= atomically $ readTVar tb
			writeBoot' 	= atomically . writeTVar tb
			writeUPatch' u p@(_,_,es) = atomicallyP $ \k -> do
				up <- readTVar tp 
				writeTVar tp . (++ if null es then [] else [(u,p)]) . filter ((/=) u . fst) $ up
				or <- readTVar to
				writeTVar to . (++ [(u,[])]) . filter ((/=) u .fst) $ or
				k $ UPatch u es
			readUPatches' 	= atomically $ liftM2 (,) (readTVar tv) $ readTVar tp
			writeGPatch' g 	= atomicallyP $ aggiornamento n load  tv ts tp to tg cl g
			readGPatch' i 	= atomically $ lookup i <$> readTVar tg
			readVersion' 	= atomically $ readTVar tv
			readLogs' 	= atomically . readTChan $ cl
			updateSignal'	= atomically $ dupTChan cs >>= return . readTChan  
			queryUtente (Just u)	= do
				es0 <- fmap (\(_,_,es) -> es) <$> lookup u <$> readTVar tp
				es1 <- lookup u <$> readTVar to
				return . maybe [] id $ es0 `mplus` es1
			queryUtente Nothing = return []
			caricamentoBianco' 	= mkModificato modif ts tp
			
	return $ Persistenza 	readStato' writeStato' readBoot' writeBoot'  
				writeUPatch' readUPatches' writeGPatch' 
				readGPatch' readVersion' readLogs' updateSignal' queryUtente caricamentoBianco'

--- TODO 
{-
integrare gli orfani con la sessione !
-}



		

