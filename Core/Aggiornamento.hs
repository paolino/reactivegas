
module Core.Aggiornamento (Aggiornamento (..), aggiornamento) where


import Data.List (sort)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Applicative ((<$>))
import Control.Arrow ((&&&))

import System.Directory (removeFile, getCurrentDirectory)
import System.FilePath (takeExtension,replaceExtension, (</>))


import Lib.Valuedfiles  (Valuedfile (..), ext, path , maybeParse, getValuedfiles)

import Core.Patch (Patch,Group)

import Eventi.Anagrafe (Responsabile)
-----------------------------------------------------------------------------------------

consumaM :: (MonadIO m , Show a) => ((Int,a) -> b -> m a) -> Valuedfile a Int -> [Valuedfile b Int] -> m (Valuedfile a Int)
consumaM agg y [] = return y
consumaM agg y@(Valuedfile n p s) (x@(Valuedfile m q d) :xs) 
	| m <= n = do
		liftIO $ do 	removeFile q
				putStrLn $ "Warn: eliminato un file di stato vecchio" ++ q
		consumaM agg y xs
	| m == n + 1 = do
		s' <- agg (m,s) d
		let 	np = replaceExtension p (takeExtension q)
		s' `seq` liftIO $ do 	writeFile np (show s') 
				 	putStrLn $ " Caricato l'aggiornamento " ++ show m
					removeFile q
					removeFile p
		consumaM agg (Valuedfile m np s') xs
	| m > n + 1 = liftIO $ do
			putStrLn $ "Warn: Rilevato l'aggiornamento " ++ show m ++ ", manca l'aggiornamento " ++ show (n + 1)
			return y


data Aggiornamento a 	
	= Boot { setStato :: a -> IO (), nuovires :: [Responsabile]}
	| Flow { stato :: (Int,a) , setAggiornamento :: ([Patch] -> Group) -> IO ()}
		
aggiornamento :: (Show a, Read a, MonadIO m, Functor m) => Maybe FilePath -> ((Int,a) -> Group -> m a) -> m (Aggiornamento a)
aggiornamento mf aggiorna = do
	(wd,msa) <- liftIO $ do 	
			putStrLn "\n\n *************** Inizio aggiornamento ***********"
			wd <- maybe getCurrentDirectory return mf 
			putStrLn $ " Aggiornamento di lavoro: " ++ wd
			stati <- reverse . sort <$> getValuedfiles maybeParse "stato" wd
			if  not $ null stati then do 
				let (stato:elimina) = stati
				putStrLn $ " Rilevato il file di stato " ++ show (ext stato)
				mapM (removeFile . path) elimina 
				(elimina,aggiornamenti) <- break ((> ext stato) . ext) <$> sort <$> getValuedfiles maybeParse "aggiornamento" wd
				putStrLn $ " Rilevati gli aggiornamenti di gruppo " ++ show (map ext aggiornamenti)	
				mapM (removeFile . path) elimina
				return (wd,Just (stato,aggiornamenti)) 
				else return (wd,Nothing)

	cs <- map (ext &&& value) <$> liftIO (getValuedfiles return "chiavi" wd)
	liftIO . putStrLn $ " Rilevate chiavi responsabile " ++ show (map fst cs)

	
	case msa of 
		Nothing -> do
			liftIO $ putStrLn $ " ********* Fine aggiornamento (stato inesistente) ********\n\n"
			return $ Boot (writeFile  (wd </> "stato.0") . show) cs
		Just (stato,aggiornamenti) -> do 
			Valuedfile n p s <- consumaM aggiorna stato aggiornamenti

			as <- map value . filter ((==) (n + 1) . ext) <$> liftIO (getValuedfiles maybeParse "aggiornamento" wd)
			liftIO $ putStrLn $ " Rilevati " ++ show (length as) ++ " aggiornamenti individuali "

			liftIO $ putStrLn $ " ********* Fine aggiornamento (stato " ++ show n ++ "********\n\n"
			return $ Flow (n,s) (writeFile (wd </> "aggiornamento." ++ show (n + 1)) . show . ($as))

-- -}
