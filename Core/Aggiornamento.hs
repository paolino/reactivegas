module Core.Aggiornamento where

import Data.Maybe
import Control.Arrow 
import Control.Monad.Writer
import Control.Monad.Trans
import Control.Applicative ((<$>))
import Control.Monad.Maybe
import System.Directory
import System.FilePath.Find
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

data Basfile a = Basfile {bext :: String , bpath :: FilePath, bvalue :: a}

mkBasfile :: Read a => (String , FilePath) -> IO (Maybe (Basfile a))
mkBasfile (e,x) = do	r <- readFile x 
			return $ case reads r of
				[] -> Nothing
				[(r,_)] -> Just $ Basfile e x r

getBasfiles :: (Read a) => [Char] -> FilePath -> IO [Basfile a]
getBasfiles n x = let
	 	accum ss f = maybe ss (:ss) . flip evalClause f . runMaybeT $ do
			guardM . lift $ fileType ==? RegularFile
			guardM . lift $ fileName ~~? (n ++ ".*")
			liftM2 (,) (tail <$> lift extension)  $ lift filePath
		in do 	fs <- fold (depth ==? 0) accum [] x >>= mapM mkBasfile
			return $ catMaybes fs


data Verfile a = Verfile {index :: Int , path :: FilePath, value :: a } deriving Show

instance Ord (Verfile a) where
	(Verfile n _ _) `compare` (Verfile m _ _) = n `compare` m
instance Eq (Verfile a) where
	(Verfile n _ _) == (Verfile m _ _) = n == m

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


consumaM :: (MonadIO m , Show a) => ((Int,a) -> b -> m a) -> Verfile a -> [Verfile b] -> m (Verfile a)
consumaM agg y [] = return y
consumaM agg y@(Verfile n p s) (x@(Verfile m q d) :xs) 
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
		consumaM agg (Verfile m np s') xs
	| m > n + 1 = liftIO $ do
			putStrLn $ "Warn: Rilevato l'aggiornamento " ++ show m ++ ", manca l'aggiornamento " ++ show (n + 1)
			return y
	
aggiornamento :: (Eq b, Read b, Show a, Read a, MonadIO m) => Maybe FilePath -> ((Int,a) -> b -> m a) -> m (FilePath, Maybe (Int,a))

aggiornamento mf aggiorna = do
	(wd,msa) <- liftIO $ do 	
			putStrLn "\n\n *************** Inizio aggiornamento ***********"
			wd <- maybe getCurrentDirectory return mf 
			putStrLn $ " Cartella di lavoro " ++ wd
			stati <- reverse . sort <$> getVerfiles "stato" wd
			if  not $ null stati then do 
				let (stato:elimina) = stati
				putStrLn $ " Rilevato il file di stato " ++ show (index stato)
				mapM (removeFile . path) elimina 
				(elimina,aggiornamenti) <- break ((> index stato) . index) <$> sort <$> getVerfiles "aggiornamento" wd
				putStrLn $ " Rilevati gli aggiornamenti di gruppo " ++ show (map index aggiornamenti)	
				mapM (removeFile . path) elimina
				return (wd,Just (stato,aggiornamenti)) 
				else return (wd,Nothing)
	case msa of 
		Nothing -> return (wd,Nothing)
		Just (stato,aggiornamenti) -> do 
			Verfile n p s <- consumaM aggiorna stato aggiornamenti
			liftIO $ putStrLn $ " ********* Fine aggiornamento (stato " ++ show n ++ ") ********\n\n"
			return (wd, Just (n,s))	


-- -}
