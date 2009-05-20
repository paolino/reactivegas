{-# LANGUAGE TypeOperators, ViewPatterns, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction #-}

import MakePatch 
import Applicazione

main =  do	putStrLn "nome del primo utente (la sua chiave pubblica deve essere nella directory): "
		l <- getLine
		s  <- readFile $ l ++ ".publ"
		writeFile "stato" (show $ s0 (l,read s))
