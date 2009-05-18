{-# LANGUAGE TypeOperators, ViewPatterns, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction #-}

import Core 
import Serializzazione
import Controllo

import Anagrafe 
import Accredito 
import Ordine 
import Servizio 
import Impegno 
import Logger

-- import Lib
import Aspetti 
import Costruzione 


import System.Environment
import Data.List
import Control.Applicative ((<$>))
import System.IO
import Codec.Binary.UTF8.String
import Control.Monad.Error
import System.Directory
import Codec.Crypto.RSA
import qualified Data.ByteString.Lazy.Char8 as B

import MakePatch 
-----------------------------------------

priorities = [priorityAnagrafe,priorityAnagrafeI,priorityImpegnoI,priorityImpegno,priorityOrdine,priorityAccredito]
makers = [makeAperturaOrdine,makeAccredito,makeEventiImpegno,makeEventiAssenso,makeEventiAnagrafe]


type T = Servizio Impegni :*: StatoOrdini :*: Conti :*: Saldi :*: Servizio Assensi :*: Anagrafe :*: Responsabili :*: ()
reattori = [reazioneAnagrafe :: Reazione T ParserConRead Utente,reazioneAccredito,reazioneOrdine,reazioneLogger] 

s0 responsabilediboot = (
	statoInizialeServizio . 
	statoInizialeOrdini . 
	statoInizialeAccredito . 
	statoInizialeServizio . 
	statoInizialeAnagrafe responsabilediboot $ (), 
	replicate (length reattori) $ nodoVuoto
	) :: (T,[SNodo T Utente])

main =  do	s  <- readFile "stato" -- leggiamo lo stato relativo alla patch n
		hSetBuffering stdout NoBuffering 
		(u,prk,reverse -> es) <- runBuildingPatch priorities makers stampaLogs reattori s 
		(read -> pu :: PublicKey)  <- readFile $ u ++ ".publ"
		writeFile (u ++ ".patch") (show (pu,sign prk (B.pack $ s ++ concat es),es))

