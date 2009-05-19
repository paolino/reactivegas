{-# LANGUAGE TypeOperators, ViewPatterns, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction #-}

import Core 
import Serializzazione
import Controllo

import Anagrafe 
import Accredito
import Servizio
import Impegno
import Ordine

import Aspetti 


import System.Environment
import Data.List
import Control.Applicative ((<$>))
import System.IO
import Codec.Binary.UTF8.String
import Control.Monad.Error
import System.Directory
import Codec.Crypto.RSA
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe
import System.FilePath

-----------------------------------------

type T = Servizio Impegni :*: StatoOrdini :*: Conti :*: Saldi :*: Servizio Assensi :*: Anagrafe :*: Responsabili :*: ()
type Q = (T,[SNodo T Utente])

main =  do	s  <- readFile "stato"
		us <- filter ((==) ".patch" . takeExtension) <$> getDirectoryContents "."
		ps::[(PublicKey,B.ByteString,[String])] <- fmap catMaybes . forM us $ 
			\x -> (\x ->  fst <$> (listToMaybe . reads $ x)) <$> readFile x 
		let ps' = filter (\(pu,firma,es) -> pu `elem` map snd (responsabili . fst $ (read s :: Q)) && 
					verify pu (B.pack (s ++ concat es)) firma) ps
		prk <- read <$> readFile "sincronizzatore.priv"					
		writeFile "gpatch" $ show (sign prk (B.pack $ s ++ show ps'),ps')
		

