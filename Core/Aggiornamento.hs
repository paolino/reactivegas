
module Core.Aggiornamento (Aggiornamento (..), aggiornamento) where


import Data.List (sort)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Applicative ((<$>))
import Control.Arrow ((&&&))

import System.Directory (removeFile, getCurrentDirectory)
import System.FilePath (takeExtension,replaceExtension, (</>))


import Lib.Valuedfiles  (Valuedfile (..), ext, path , maybeParse, getValuedfiles)

import Core.Patch (Patch,Group)

import Eventi.Anagrafe (Responsabile, Utente)
-----------------------------------------------------------------------------------------

consumaM :: (Show a) => (a -> b -> IO a) -> Valuedfile a Int -> [Valuedfile b Int] -> IO (Valuedfile a Int)
consumaM agg y [] = return y
consumaM agg y@(Valuedfile n p s) (x@(Valuedfile m q d) :xs) 
	| m <= n = do
		do 	removeFile q
			putStrLn $ "Warn: eliminato un file di stato vecchio" ++ q
		consumaM agg y xs
	| m == n + 1 = do
		s' <- agg s d
		let 	np = replaceExtension p (takeExtension q)
		s' `seq` do 	writeFile np (show s') 
				putStrLn $ " Caricato l'aggiornamento " ++ show m
				removeFile q
				removeFile p
		consumaM agg (Valuedfile m np s') xs
	| m > n + 1 = do
			putStrLn $ "Warn: Rilevato l'aggiornamento " ++ show m ++ ", manca l'aggiornamento " ++ show (n + 1)
			return y


data Aggiornamento a 	
	= Boot { responsabiliBoot :: [Responsabile], publishStato :: a -> IO (), publishChiavi :: Responsabile -> IO ()}
	| Flow { stato :: a , publishUPatch :: Utente -> Patch -> IO (), publishGPatch :: Maybe (([Patch] -> Group) -> IO ())}
		
aggiornamento :: (Show a, Read a) => Maybe FilePath -> (a -> Group -> IO a) -> IO (Aggiornamento a)
aggiornamento mf aggiorna = do
	(wd,msa) <-  do 	
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

	cs <- map (ext &&& value) <$> (getValuedfiles return "chiavi" wd)
	putStrLn $ " Rilevate chiavi responsabile " ++ show (map fst cs)

	
	case msa of 
		Nothing -> do
			putStrLn $ " ********* Fine aggiornamento (stato inesistente) ********\n\n"
			return $ Boot 
				cs 
				(writeFile  (wd </> "stato.0") . show) 
				(\(r,c) -> writeFile (wd </> "chiavi." ++ r) . show $ c) 
		Just (stato,aggiornamenti) -> do 
			Valuedfile n p s <- consumaM aggiorna stato aggiornamenti

			as <- map value . filter ((==) (n + 1) . ext) <$> liftIO (getValuedfiles maybeParse "aggiornamento" wd)
			putStrLn $ " Rilevati " ++ show (length as) ++ " aggiornamenti individuali "

			putStrLn $ " ********* Fine aggiornamento (stato " ++ show n ++ "********\n\n"
			return $ Flow 
				s
				(\u -> writeFile (wd </> "aggiornamento." ++ u ++ "." ++ show (n + 1)) . show)
				$ case as of
					[] -> Nothing
					as -> Just $ writeFile (wd </> "aggiornamento." ++ show (n + 1)) . show . ($as)

-- -}
