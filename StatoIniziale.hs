{-# LANGUAGE TypeOperators, ViewPatterns, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction #-}

import MakePatch 
import Applicazione
import Data.Digest.Pure.SHA
import Codec.Crypto.RSA
import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as B

main =  do	putStrLn "nome del primo utente (la sua chiave pubblica deve essere nella directory): "
		l <- getLine
		puk  <- readFile $ l ++ ".publ"
		let 	s = show $ s0 (l,read puk)
		writeFile "stato" s
		p <- read <$> readFile "sincronizzatore.publ"
		writeFile "prova.gruppo" $ show (p :: PublicKey , ([]::[()],(showDigest . sha512 $ B.pack s,[]::[()]), [read puk :: PublicKey]))

