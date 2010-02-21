{-# LANGUAGE Rank2Types, ExistentialQuantification #-}

module Core.Persistenza where

import Data.Maybe (fromJust,isJust,isNothing)
import Data.List (sort,partition,find)

import Control.Monad.Writer
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Applicative ((<$>))
import Control.Arrow ((&&&), (***),second)
import Control.Exception (tryJust, SomeException (..))
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM

import System.Directory (getCurrentDirectory)
import System.FilePath ((</>), addExtension)


import Lib.Valuedfiles  (Valuedfile (..), maybeParse, getValuedfiles)
import Lib.Assocs (secondM)

import Core.Patch (Patch,Group)
import Core.Types (Evento,Utente)


import Debug.Trace
-----------------------------------------------------------------------------------------
type GK = String

-- | strato funzionale di persistenza, lo stato in memoria è pericoloso .....
data Persistenza a = Persistenza
		{ 	readStato 	:: IO (Maybe a),		-- ^ lettura dello stato
			writeStato 	:: a -> IO (),			-- ^ scrittura dello stato
			readOrfani 	:: Utente -> IO [Evento], 	-- ^ eventi di patch utente invalidate
			writeOrfani 	:: Utente -> [Evento] -> IO (),	-- ^ settaggio orfani
			writeUPatch 	:: Utente -> Patch -> IO (),	-- ^ scrittura di una patch utente
			readUPatches 	:: IO [Patch],			-- ^ lettura delle patch utente
			writeGPatch 	:: Group -> IO (),		-- ^ scrittura di una patch di gruppo
			readLogs	:: IO (IO String)
		}

-- | scrive un dato riguardante un gruppo
groupWrite :: Show a => GK -> String -> Int -> a -> IO ()
groupWrite x y v = writeFile (x </> addExtension y (show v)) . show
	
-- | legge un dato riguardante un gruppo
groupUnwrite :: Read a => GK -> String -> Maybe Int -> IO (Maybe (Int,a))
groupUnwrite x y mv = do
		stati <-  getValuedfiles maybeParse y x
		return $ case mv of
			Just v -> find ((==) v . ext) stati >>= Just . (ext &&& value)
			Nothing -> case sort stati of 
				[] -> Nothing
				xs -> Just . (ext &&& value) $ last xs

-- | Operazione di persistenza. Scrive un istantanea dello stato, patch utente 
persistenza 	:: Show a
	=> (forall b . Show b => String -> Int -> b -> IO ())		-- ^ operazione di persistenza
	-> TVar Int 		-- ^ versione dello stato
	-> TVar (Maybe a)	-- ^ stato
	-> TVar [(Utente,Patch)]-- ^ associazione utente -> patch individuale
	-> TVar [(Utente,[Evento])] -- ^ eventi orfani
	-> TChan String		-- ^ logger
	-> IO ()

persistenza write tversion tstato tupatch torfani tlog = do 
	(version,stato,patches) <- atomically $ do
		version <- readTVar tversion
		stato <-  readTVar tstato
		patches <- readTVar tupatch
		orfani <- readTVar torfani
		return (version,stato,patches)
	when (isJust stato) $ do 
		write "stato" version (fromJust stato)
		write "patches" version patches
		write "orfani" version patches
		atomically . writeTChan tlog $ "persistenza: " ++ show (version,map fst patches)

-- | Operazione di ripristino. Legge lo stato del gruppo 
ripristino
	:: Read a =>  (forall b . Read b => String -> Maybe Int -> IO (Maybe (Int, b)))
	-> TVar Int
	-> TVar (Maybe a)
	-> TVar [(Utente,Patch)]
	-> TVar [(Utente,[Evento])] -- ^ eventi orfani
	-> TChan String
	-> IO ()

ripristino unwrite tversion tstato tupatch torfani tlog = do
	ms <- unwrite "stato" Nothing
	case ms of
		Nothing -> atomically $ do
			writeTChan tlog "nessuno stato per questo gruppo"
			writeTVar tstato  Nothing
			writeTVar tupatch []
			writeTVar torfani []
			writeTVar tversion 0
		Just (v,x) -> do
			ps <- unwrite "patches" (Just v)
			os <- unwrite "orfani" (Just v)
			atomically $ do
				writeTChan tlog "nessuno stato per questo gruppo"
				writeTVar tstato  $ Just x
				when (isJust ps) . writeTVar tupatch . snd . fromJust $ ps
				when (isJust os) . writeTVar torfani . snd . fromJust $ os
				writeTVar tversion v

-- | stato concorrente di un gruppo
type GroupState a = 
	(TVar Int 		-- ^ versione dello stato
	,TVar (Maybe a)		-- ^ stato
	,TVar [(Utente,Patch)]	-- ^ associazione utente -> patch individuale
	,TVar [(Utente,[Evento])] -- ^ eventi orfani dell'ultimo aggiornamento di gruppo
	,TChan Group		-- ^ canale di aggiornamento di gruppo
	,TChan String 		-- ^ log del gruppo
	)

-- | prepara un stato vergine
mkGroup :: IO (GroupState a)
mkGroup = do
	tv <- atomically $ newTVar 0
	ts <- atomically $ newTVar Nothing
	tp <- atomically $ newTVar []
	to <- atomically $ newTVar []
	cg <- atomically $ newTChan 
	cl <- atomically $ newTChan
	return (tv,ts,tp,to,cg,cl)

		

mix :: [(Utente,[Evento])] -> [(Utente,[Evento])] -> [(Utente,[Evento])] 
mix [] [] = []
mix [] xs = xs
mix ((u,y):ys) xs = case lookup u xs of 
	Nothing -> mix ys $ (u,y):xs
	Just es -> mix ys $ (u,y ++ es): filter ((/=) u . fst) xs

-- | transazione di aggiornamento provocata dall'arrivo di una patch di gruppo
aggiornamento 
	:: (a -> Group -> Writer [String] (Either String a))  -- ^ tentativo di aggiornamento
	-> TVar Int
	-> TVar (Maybe a)
	-> TVar [(Utente,Patch)]
	-> TVar [(Utente,[Evento])] -- ^ eventi orfani
	-> TChan Group
	-> TChan String
	-> IO ()	-- ^ transazione 
aggiornamento load  tv ts tp to cg tl = atomically $ do
	s' <- readTVar ts
	when (isNothing s') retry
	let Just s = s'
	g@(_,_,ps) <- readTChan cg
	let (es,ls) = runWriter $ load s g
	mapM (writeTChan tl) ls
	case es of
		Left e -> writeTChan tl e
		Right s' -> do
			writeTVar ts $ Just s'
			pos <- filter (not . (`elem` ps) . snd) <$> readTVar tp
			os <- readTVar to
			writeTVar to $ map (second $ \(_,_,evs) -> evs) pos `mix` os
			writeTVar tp []
			readTVar tv >>= writeTVar tv . (+1)
	
-- | il sistema gruppo , con il suo stato e le operazioni di persistenza
type GroupSystem a = 
	(Persistenza a	 		-- ^ stato complessivo del gruppo
	,IO ()				-- ^ azione di aggiornamento
	,IO ()				-- ^ azione di ripristino
	,IO ()				-- ^ azione di persistenza
	)

-- | prepara uno stato vergine di un gruppo
mkGroupSystem :: ( Read a, Show a) 
	=> (a -> Group -> Writer [String] (Either String a)) 	-- ^ loader specifico per a
	-> TChan String 					-- ^ log di persistenza
	-> GK 							-- ^ nome del gruppo
	-> IO (GroupSystem a)					

mkGroupSystem loader ptl x = do 
		g@(tv,ts,tp,to,cg,tl) <- mkGroup 
		return 	(mkPersistenza g
			,aggiornamento loader tv ts tp to cg tl 
			,ripristino (groupUnwrite x) tv ts tp to ptl 
			,persistenza (groupWrite x) tv ts tp to ptl
			)

startGroupSystem :: (Read a, Show a) => Int -> GroupSystem a -> IO (Persistenza a)
startGroupSystem t (g,agg,rip,pers) = do
	forkIO $ forever agg
	forkIO $ forever (threadDelay t >> pers)
	rip
	return g

mkPersistenza :: GroupState a -> Persistenza a
mkPersistenza (tv,ts,tp,to,cg,tl) = let
	readStato' = atomically $ readTVar ts
	writeStato' s = atomically $ writeTVar ts (Just s)
	readOrfani' u = atomically $ maybe [] id . lookup u <$> readTVar to
	writeOrfani' u es = atomically $ readTVar to >>= writeTVar to . ((u,es) :) .  filter ((/=) u . fst)  
	writeUPatch' u p = atomically $ readTVar tp >>= writeTVar tp . ((u,p) :)
	readUPatches' = atomically $ map snd <$> readTVar tp
	writeGPatch' g = atomically $ writeTChan cg g
	readLogs' = atomically (dupTChan tl) >>= return . atomically . readTChan
	in Persistenza readStato' writeStato' readOrfani' 
		writeOrfani' writeUPatch' readUPatches' writeGPatch' readLogs'

oneService :: (Read a, Show a) => (a -> Group -> Writer [String] (Either String a)) -> FilePath ->  IO (Persistenza a)
oneService load name = do
	c <- atomically newTChan
	forkIO . forever $ (atomically (readTChan c) >>= putStrLn)
	mkGroupSystem load c name >>= startGroupSystem 10000000
	
-- | prepara lo strato persistente per l'applicativo

		

