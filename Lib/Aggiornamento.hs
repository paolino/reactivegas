module Lib.Aggiornamento where

import Data.Maybe
import Control.Arrow 
import Control.Monad.Writer
import Control.Monad.Trans
import Control.Applicative ((<$>))
import Control.Monad.Maybe
import System.Directory
import System.FilePath.FindCompat
import Data.List (sortBy, sort)
import Data.Ord (comparing)
import System.FilePath (replaceExtension, takeExtension , splitExtension)
import Debug.Trace

getNumber :: Monad m => String -> MaybeT m Int
getNumber s = case reads (tail s) of
	[] -> mzero 
	[(n,_)] -> return n

guardM :: (Monad m, MonadPlus m) => m Bool -> m ()
guardM x = x >>= guard  

data Verfile a = Verfile {index :: Int , path :: FilePath, value :: a } deriving (Eq,Show)

instance Eq a => Ord (Verfile a) where
	(Verfile n _ _) `compare` (Verfile m _ _) = n `compare` m

mkVerfile :: Read a => (Int,FilePath) -> IO (Maybe (Verfile a))
mkVerfile (i,x) = do	r <- readFile x 
			return $ case reads r of
				[] -> Nothing
				[(r,_)] -> Just $ Verfile i x r

getVerfiles :: (Read a) => [Char] -> FilePath -> IO [Verfile a]
getVerfiles n x = let
	 	accum ss f = maybe ss (:ss) . flip evalClause f . runMaybeT $ do
			guardM . lift $ fileType ==? RegularFile
			guardM . lift $ fileName ~~? (n ++ ".*")
			liftM2 (,) (lift extension >>= getNumber) $ lift filePath
		in do 	fs <- fold (depth ==? 0) accum [] x >>= mapM mkVerfile
			return $ catMaybes fs



consumaM :: Show a => (a -> b -> (a,String)) -> Verfile a -> [Verfile b] -> WriterT [String] IO (Verfile a)
consumaM agg y [] = return y
consumaM agg y@(Verfile n p s) (x@(Verfile m q d) :xs) 
	| m <= n = do
		liftIO $ do 	removeFile q
				putStrLn $ "Warn: eliminato un file di stato vecchio" ++ q
		consumaM agg y xs
	| m == n + 1 = do
		let 	(s',log) = agg s d
			np = replaceExtension p (takeExtension q)
		s' `seq` liftIO $ writeFile np (show s') 
		tell [log]
		liftIO $ do 	putStrLn $ "Ok: Caricato l'aggiornamento " ++ show m
				removeFile q
				removeFile p
		consumaM agg (Verfile m np s') xs
	| m > n + 1 = do
		liftIO . putStrLn $ 
			"Warn: Rilevato l'aggiornamento " ++ show m ++ ", manca l'aggiornamento " ++ show (n + 1)
		return y
	
aggiornamento :: (Eq b, Read b, Show a, Eq a, Read a) => Maybe FilePath -> (a -> b -> (a, String)) -> IO (a,[String])
aggiornamento mf aggiorna = do
	putStrLn "Ok: *************** Aggiornamento ***********"
	wd <- maybe getCurrentDirectory return mf 
	putStrLn $ "Ok: Cartella di lavoro " ++ wd
	stati <- reverse . sort <$> getVerfiles "stato" wd
	when (null stati) $ error $ 
		"Abort: Il file di stato è non presente o non integro nella cartella " ++ wd
	let (stato:elimina) = stati
	mapM (removeFile . path) elimina 
	(elimina,aggiornamenti) <- break ((> index stato) . index) <$> sort <$> getVerfiles "aggiornamento" wd	
	mapM (removeFile . path) elimina 
	(Verfile n p s,rs) <- runWriterT $ consumaM aggiorna stato aggiornamenti
	putStrLn $ "Ok: Stato attuale all'aggiornamento " ++ show n
	return (s,rs)	
-- -}
