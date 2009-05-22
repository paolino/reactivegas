{-# LANGUAGE TypeOperators, ViewPatterns, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction #-}


import Control.Monad
import System.Environment

import Control.Applicative ((<$>))
import System.Directory
import Codec.Crypto.RSA
import Data.Digest.Pure.SHA
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
		let h = showDigest $ sha512 $ B.pack s
		prk <- read <$> readFile "sincronizzatore.priv"					
		(puk :: PublicKey) <- read <$> readFile "sincronizzatore.publ"					
		ps <-  query x (read z) (show $ UPS)
		let 	ps' = filter (\((pu,firma,es)::UP) -> pu `elem` responsabiliQ s && 
				verify pu (B.pack (h ++ concat es)) firma) ps
			f0 = sign prk (B.pack $ h ++ show ps') 
		s' <- aggiornaStato puk s [(f0 ,ps')]
		let 	ws = responsabiliQ $ s'
			f1 = sign prk (B.pack $ h ++ show ws)
		let h = showDigest $ sha512 $ B.pack s'
		r <- query x (read z) (show $ GroupPatch (h,f0 ,ws, f1)) 
		putStrLn r
		writeFile "stato" s'

		

