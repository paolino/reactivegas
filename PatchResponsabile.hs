{-# LANGUAGE TypeOperators, ViewPatterns, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction #-}
import Controllo 
import System.Environment
import Control.Applicative ((<$>))
import System.IO
import Codec.Crypto.RSA
import Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad 

import MakePatch 
import Rete
import Applicazione
-----------------------------------------


main =  do	
	s  <- readFile "stato"
	let h = showDigest $ sha512 $ B.pack s
	g <- read <$> readFile "sincronizzatore.publ"
	[x,z] <- getArgs
	ps <- query x (read z) (show $ Aggiornamento h)	
	s' <- aggiornaStato g s ps 
	vs <- query x (read z) (show $ Validi)
	when (responsabiliQ s' /= vs) $ error "Il sicronizzatore sta truffando sui responsabili validi"
	writeFile "stato" s'
	hSetBuffering stdout NoBuffering 
	(u,prk,reverse -> es) <- runBuildingPatch priorities makers stampaLogs reattori s' 
	pu <- read <$> (readFile $ u ++ ".publ")
	let h = showDigest $ sha512 $ B.pack s'
	let r = (pu,sign prk (B.pack $ h ++ concat es),es)
	s <- query x (read z) $ show (Patch r) 
	putStrLn s
