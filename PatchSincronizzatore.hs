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


main =  do	ls <- getArgs
		s  <- readFile "stato"
		prk <- read <$> readFile "sincronizzatore.priv"					
		(puk :: PublicKey) <- read <$> readFile "sincronizzatore.publ"					
		ps <- case ls of
			[x,z] -> query x (read z) (show $ UPS)
		let 	ps' = filter (\((pu,firma,es)::UP) -> pu `elem` map snd (responsabiliQ s) && 
				verify pu (B.pack (s ++ concat es)) firma) ps
			f = sign prk (B.pack $ s ++ show ps')
		case ls of
			[x,z] -> do
				s' <- aggiornaStato puk s [(f,ps')]
				let ws = map snd . responsabiliQ $ s'
				r <- query x (read z) (show $ GroupPatch (s',f,ps',ws)) 
				putStrLn r
				writeFile "stato" s'

		

