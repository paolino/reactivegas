{-# LANGUAGE ScopedTypeVariables #-}
module Lib.Valuedfiles where

import Data.Maybe (catMaybes)
import Control.Monad.Trans (lift)
import Control.Applicative ((<$>))
import Control.Monad (mzero,guard,liftM2, MonadPlus)
import Control.Monad.Maybe (MaybeT,runMaybeT)
import System.FilePath.FindCompat (filePath, fileName , fileType, (==?), extension, 
	evalClause , depth, fold, liftOp, FileType (RegularFile),FindClause)
import System.FilePath.GlobPattern ((~~))
import System.IO (openFile, hGetContents, hSeek, SeekMode (AbsoluteSeek), hClose, IOMode (ReadMode))

import Debug.Trace
--- buggy filemanipcompat ------
(~~?) = liftOp (~~)
-------------------------------


maybeParse :: (Read a, Monad m) => String -> MaybeT m a
maybeParse s = case reads s of
	[] -> mzero 
	[(n,_)] -> return n

guardM :: (Monad m, MonadPlus m) => m Bool -> m ()
guardM x = x >>= guard  

data Valuedfile a b = Valuedfile {ext :: b , path :: FilePath, value :: IO a} 
instance Ord b => Ord (Valuedfile a b) where
	(Valuedfile n _ _) `compare` (Valuedfile m _ _) = n `compare` m
instance Eq b => Eq (Valuedfile a b) where
	(Valuedfile n _ _) == (Valuedfile m _ _) = n == m

trace' x = trace x x

mkValuedfile :: forall a b. Read a => (b,FilePath) -> IO (Maybe (Valuedfile a b))
mkValuedfile (i,x) = do	h1 <- openFile x ReadMode
			r <- hGetContents h1
			case reads r of
				[] -> do
					hClose h1
					return Nothing
				[((r::a),_)] -> do
					h2 <- openFile x ReadMode
					hClose h1
					return . Just . Valuedfile i x $ do 
						[(r',_)] <- reads <$> hGetContents h2
						hClose h2 
						return r'


getValuedfiles :: Read a => (String -> MaybeT FindClause b) -> String -> FilePath -> IO [Valuedfile a b]
getValuedfiles g n x = let
	 	accum ss f = maybe ss (:ss) . flip evalClause f . runMaybeT $ do
			guardM . lift $ fileType ==? RegularFile
			guardM . lift $ fileName ~~? (n ++ ".*")
			liftM2 (,) (lift extension >>= g . tail) $ lift filePath
		in do 	fs <- fold (depth ==? 0) accum [] x >>= mapM mkValuedfile
			return $ catMaybes fs

