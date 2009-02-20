{-# LANGUAGE StandaloneDeriving #-}
import Codec.Crypto.RSA
import System.Random
import System.Console.GetOpt
import System.Environment
import Data.Maybe 

main = do
	names <- getArgs 
	g <- newStdGen
	let (pu,pr,_) = generateKeyPair g 512
	writeFile (head names ++ ".priv") $ show pr
	writeFile (head names ++ ".publ") $ show pu

