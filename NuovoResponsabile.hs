
{-# LANGUAGE TypeOperators, ViewPatterns, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction #-}

import System.Random (newStdGen)
import Codec.Crypto.RSA (generateKeyPair)
import Control.Applicative
import Codec.Binary.UTF8.String

main =  do	putStr "nome:"
		l <- getLine
		let (p1,p2) = (l ++ ".priv", l ++ ".publ")
		(pu,pr,_) <- flip generateKeyPair 512 <$> newStdGen
		writeFile p1 (show pr)
		writeFile p2 (show pu)
		putStrLn $ encodeString $ "spedisci la tua chiave pubblica (il file " ++ show p2 ++ ") al responsabile che ti introdurrá nel gruppo"
		putStrLn $ encodeString $ "la presenza del file " ++ show p1 ++ " nella directory é necessaria affinché tu possa produrre eventi nel gruppo. Non perderlo"
