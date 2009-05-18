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

main =  do	putStrLn "nome del primo utente (la sua chiave pubblica deve essere nella directory): "
		l <- getLine
		s  <- readFile $ l ++ ".publ" -- leggiamo lo stato relativo alla patch n
		writeFile "stato" (show $ s0 (l,read s))
