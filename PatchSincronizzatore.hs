{-# LANGUAGE TypeOperators, ViewPatterns, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction #-}


import Control.Monad
import System.Environment

import Control.Applicative ((<$>))
import System.Directory
import Codec.Crypto.RSA
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe
import System.FilePath
import Rete
import Network
import Applicazione
import Anagrafe
-----------------------------------------


main =  do	[x,z] <- getArgs
		s  <- readFile "stato"
		prk <- read <$> readFile "sincronizzatore.priv"					
		(puk :: PublicKey) <- read <$> readFile "sincronizzatore.publ"					
		ps <-  query x (read z) (show $ UPS)
		let 	ps' = filter (\((pu,firma,es)::UP) -> pu `elem` responsabiliQ s && 
				verify pu (B.pack (s ++ concat es)) firma) ps
			f0 = sign prk (B.pack $ s ++ show ps') 
		s' <- aggiornaStato puk s [(f0 ,ps')]
		let 	ws = responsabiliQ $ s'
			f1 = sign prk (B.pack $ s ++ show ws)
		r <- query x (read z) (show $ GroupPatch (s',f0 ,ws, f1)) 
		putStrLn r
		writeFile "stato" s'

		

