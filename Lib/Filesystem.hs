{-# LANGUAGE MonoLocalBinds #-}
module Lib.Filesystem where

import System.Directory
import System.FilePath
import Data.Maybe
import Control.Exception

onFS :: IO a -> (String -> IO b) -> (a -> IO b) -> IO b
onFS f z w =  tryJust (\(SomeException x) -> Just $ show x) f >>= either z w 


-- | scrive un dato riguardante un gruppo
groupWrite :: Show a 	=> FilePath -> String -> Int -> a -> IO ()
groupWrite x y v t = onFS (writeFile (x </> y) (show (v,t))) error return

-- | legge un dato riguardante un gruppo
groupUnwrite :: Read a => FilePath -> String -> IO (Maybe (Int,a))
groupUnwrite x y  = onFS (readFile (x </> y)) (\_ -> return Nothing) (return . seqit)
	where seqit z = case reads z of 
		[(q,_)] -> Just q
		_ -> Nothing

-- | legge un dato riguardante un gruppo
groupUnwriteF ::  ReadS (Int,a) -> FilePath -> String -> IO (Maybe (Int,a))
groupUnwriteF f x y  = onFS (readFile (x </> y)) (\_ -> return Nothing) (return . seqit)
	where seqit z = case f z of 
		[(q,_)] -> last z `seq` Just q
		_ -> Nothing

-------------------------------------------------------------------------------------------------------------------


