{-# LANGUAGE TypeOperators, ViewPatterns, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction #-}
import Controllo 
import System.Environment
import Control.Applicative ((<$>))
import System.IO
import Codec.Crypto.RSA
import qualified Data.ByteString.Lazy.Char8 as B

import MakePatch 
import Rete
import Applicazione
-----------------------------------------


main =  do	
	s  <- readFile "stato" 
	g <- read <$> readFile "sincronizzatore.publ"
	ls <- getArgs
	ps <- case ls of 
		[x,z] -> query x (read z) (show $ Aggiornamento s)				
	s' <- aggiornaStato g s ps 
	writeFile "stato" s'
	ls <- getArgs
	hSetBuffering stdout NoBuffering 
	(u,prk,reverse -> es) <- runBuildingPatch priorities makers stampaLogs reattori s' 
	pu <- read <$> (readFile $ u ++ ".publ")
	let r = (pu,sign prk (B.pack $ s ++ concat es),es)
	() <- case ls of
		[x,z] -> query x (read z) $ show (Patch r) 
	return ()
