
{-# LANGUAGE TypeOperators, ViewPatterns, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction #-}
module Applicazione where

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
import Data.Maybe
import Control.Arrow
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

type Q = (T,[SNodo T Utente])
s0 responsabilediboot = (
	statoInizialeServizio . 
	statoInizialeOrdini . 
	statoInizialeAccredito . 
	statoInizialeServizio . 
	statoInizialeAnagrafe responsabilediboot $ (), 
	replicate (length reattori) $ nodoVuoto
	) :: (T,[SNodo T Utente])

responsabiliQ s = responsabili . fst $ (read s :: Q)
aggiornaStato g = let
	r stato (firma,ps) = do
		when (not $ verify g (B.pack $ stato ++ show ps) firma) $ error "errore di integrita della patch di gruppo" 
		let 	(t,_):: (T,[SNodo T Utente]) = read stato
			rs = map (snd &&& fst) $ responsabili t 
			zs = map (\(puk,_,es) -> zip (repeat (fromJust $ lookup puk rs)) es) ps
		when (not $ all (\(puk,_,_) -> puk `elem` map fst rs) ps ) $ error "la patch di gruppo contiene eventi da un utente sconosciuto"
		when (not $ all (\(puk,firma,es) -> verify puk (B.pack $ stato ++ concat es) firma) ps) $ 
			error "la patch di gruppo contiene una patch di responsabile non integra"
		(_,stato',logs) <- runProgramma reattori stato (caricaEventi priorities (concat zs))
		stampaLogs logs
		return stato'
	in foldM r		


