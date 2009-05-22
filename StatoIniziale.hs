{-# LANGUAGE TypeOperators, ViewPatterns, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction #-}

import MakePatch 
import Applicazione
import Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy.Char8 as B

main =  do	putStrLn "nome del primo utente (la sua chiave pubblica deve essere nella directory): "
		l <- getLine
		puk  <- readFile $ l ++ ".publ"
		let 	s = show $ s0 (l,read puk)
		writeFile "stato" s
		writeFile "stato.rif" $ showDigest . sha512 $ B.pack s

