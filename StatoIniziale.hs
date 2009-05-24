{-# LANGUAGE TypeOperators, ViewPatterns, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction #-}

import MakePatch 
import Applicazione
import Data.Digest.Pure.SHA
import Codec.Crypto.RSA
import Control.Applicative
import Control.Monad
import System.Directory
import System.FilePath
import System.IO

import qualified Data.ByteString.Lazy.Char8 as B
import Rete
main =  do	ls <- getDirectoryContents "."
		hSetBuffering stdout NoBuffering
		putStr "Nome del gruppo:"
		l <- getLine
		let 	rs = filter ((/=) "sincronizzatore.publ") . filter ((==) ".publ". takeExtension) $ ls
			ns = map takeBaseName rs
		puks  <- forM rs $ \r -> do
			read <$> readFile r

		let 	rs = zip ns puks
			s = show $ s0 rs
		writeFile "stato" s
		p <- read <$> readFile "sincronizzatore.publ"
		writeFile (l ++ ".gruppo") $ show (p :: PublicKey , mkBoardValue (showDigest . sha512 $ B.pack s) puks)

