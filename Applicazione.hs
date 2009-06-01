
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
import Control.Monad.Writer
import System.Directory
import Codec.Crypto.RSA
import Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy.Char8 as B
import Anagrafe
import MakePatch 
-----------------------------------------

priorities = [priorityAnagrafe,priorityAnagrafeI,priorityImpegnoI,priorityImpegno,priorityOrdine,priorityAccredito]
makers = concat [makeAperturaOrdine,makeAccredito,makeEventiImpegno,makeEventiAssenso,makeEventiAnagrafe]


type T = Servizio Impegni :*: StatoOrdini :*: Conti :*: Saldi :*: Servizio Assensi :*: Anagrafe :*: Responsabili :*: ()
reattori = [reazioneAnagrafe :: Reazione T ParserConRead Utente,reazioneAccredito,reazioneOrdine,reazioneLogger] 

type Q = (T,[SNodo T Utente])
s0 responsabilidiboot = (
	statoInizialeServizio . 
	statoInizialeOrdini . 
	statoInizialeAccredito . 
	statoInizialeServizio . 
	statoInizialeAnagrafe responsabilidiboot $ (), 
	replicate (length reattori) $ nodoVuoto
	) :: (T,[SNodo T Utente])

responsabiliQ :: Q -> [Chiave]
responsabiliQ s = map snd . responsabili . fst $ s

aggiornaStato :: (MonadError [Char] m) =>
                 PublicKey
                 -> (T, [SNodo T Utente])
                 -> [(B.ByteString, [(Chiave, B.ByteString, [[Char]])])]
                 -> m ((T, [SNodo T Utente]), Log Utente)

aggiornaStato g x = let
	r stato (firma,ps) = do
		let h = showDigest . sha512 $ B.pack $ show stato
		when (not $ verify g (B.pack $ h ++ show ps) firma) $ throwError "throwErrore di integrita della patch di gruppo" 
		let 	rs = map (snd &&& fst) $ responsabili (fst stato)
			zs = map (\(puk,_,es) -> zip (repeat (fromJust $ lookup puk rs)) es) ps
		when (not $ all (\(puk,_,_) -> puk `elem` map fst rs) ps ) $ throwError "la patch di gruppo contiene eventi da un utente sconosciuto"
		when (not $ all (\(puk,firma,es) -> verify puk (B.pack $ h ++ concat es) firma) ps) $ 
			throwError "la patch di gruppo contiene una patch di responsabile non integra"
		(_,stato',logs) <- runProgramma reattori stato (caricaEventi priorities (concat zs))
		tell logs
		return stato'
	in runWriterT . foldM r x


