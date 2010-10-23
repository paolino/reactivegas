
{-# LANGUAGE Rank2Types, ScopedTypeVariables, TupleSections #-}

module Applicazioni.Amministratore where
import Control.Monad (when)
import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import Control.Arrow (first,(&&&))
import Lib.Passo

import Data.List (lookup)
import Data.Maybe (catMaybes, isNothing)
import Lib.Tokens 
import Control.Monad (liftM2)
import Control.Monad.Trans (liftIO, MonadIO)
import Core.Types
import Lib.Modify (PeekPoke)
import Lib.Response
import System.FilePath
import System.FilePath.Find
import System.Directory
import Lib.Filesystem
import Lib.Modify

import Applicazioni.Persistenza

type Gruppo = String


data Amministratore a = Amministratore {
	controlla_nome :: Gruppo -> IO Bool,
	controlla_password :: Token -> Bool,
	boot_nuovo_gruppo :: (Gruppo,Responsabile) -> IO Bool,
	elenco_gruppi :: IO [Gruppo],
	valore_di_gruppo :: Gruppo -> STM (Maybe a)
	}

mkAmministratore	
	-- | password di amministratore 
	:: Token 
	-- | creazione o lettura di gruppo		
	-> ((FilePath,Gruppo,Maybe Responsabile) -> IO a) 
	-- | directory di lavoro
	-> FilePath 
	-- | l'amministratore risultante
	-> IO (Amministratore a)
mkAmministratore pass readA dir = do
	-- analisi directories
	ms <- tail `fmap` find 
		((== 0) `fmap` depth) 
		(liftM2 (&&) ((== Directory) `fmap` fileType) ((/= "static") `fmap` fileName)) 
		dir
	-- gruppi e valori
	let ns = map takeFileName ms
	gs <-  zip ns `fmap` mapM readA (zip3 ms ns $ repeat Nothing)

	putStrLn "** Recuperati i seguenti gruppi:"
	mapM_ (\(x,_) -> putStrLn $ "\t" ++ x) gs 

	-- mappatura variabile
	pes <- atomically $ newTVar gs

	let 	new g r0 = do 	
			pe <- readA (dir </> g, g,Just r0)
			liftIO . atomically $ readTVar pes >>= writeTVar pes . ((g,pe):)
		create g = do
			let dg = dir </> g
			t <- doesDirectoryExist $ dg
			when t $ renameDirectory dg ( dg </> addExtension g "copia")
			createDirectoryIfMissing True (dg </> "static")
			putStrLn $ "created " ++ dg
		controlla_nome' x = (isNothing . lookup x) `fmap` atomically (readTVar pes)
		controlla_password' = (== pass)
		boot_nuovo_gruppo' (g,r) = do 
			t <- atomically $ do
				gs <- readTVar pes
				case lookup g gs of 
					Nothing -> return (g /= "static")
					Just _ -> return False
			when t $ do
				create g 
				new g r
			return t
		elenco_gruppi' = map fst `fmap` atomically (readTVar pes)
		valore_di_gruppo' g = lookup g `fmap` readTVar pes
	putStrLn "** Amministrazione attiva"
	return $ Amministratore controlla_nome' controlla_password' boot_nuovo_gruppo' elenco_gruppi' valore_di_gruppo'

