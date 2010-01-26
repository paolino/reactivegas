
{-# LANGUAGE TypeOperators, ViewPatterns, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction #-}
module Applicazione where

import Data.Maybe
import Control.Arrow
import System.Environment
import Data.List
import Control.Applicative ((<$>))
import System.IO
import Codec.Binary.UTF8.String
import Control.Monad.Error
import Control.Monad.Writer
import Control.Monad.Reader
import System.Directory
import Codec.Crypto.RSA
import Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy.Char8 as B

import Core.Inserimento
import Core.Controllo
import Core.Programmazione
import Core.Parsing
import Core.Contesto

import Eventi.Anagrafe 
import Eventi.Accredito 
import Eventi.Ordine 
import Eventi.Servizio 
import Eventi.Impegno 
-- import Logger

import Lib.Aspetti 
import Lib.Costruzione 
-----------------------------------------

priorities = [priorityAnagrafe,priorityAnagrafeI,priorityImpegnoI,priorityImpegno,priorityOrdine,priorityAccredito]
makers = concat [makeAperturaOrdine,makeAccredito,makeEventiImpegno,makeEventiAssenso,makeEventiAnagrafe]
queriers = concat [queryAccredito, queryAnagrafe,queryAssenso,queryOrdine,queryImpegni]

type T = Servizio Impegni :*: StatoOrdini :*: Conti :*: Saldi :*: Servizio Assensi :*: Anagrafe :*: Responsabili :*: ()

reattori = [reazioneAnagrafe :: Reazione T ParserConRead Utente,reazioneAccredito,reazioneOrdine] 
addServizio = (,) servizio0
type Q = (T,[SNodo T Utente])

s0 :: [(Utente, Chiave)] -> Q
s0 responsabilidiboot = (
	addServizio . 
	statoInizialeOrdini . 
	statoInizialeAccredito . 
	addServizio . 
	statoInizialeAnagrafe responsabilidiboot $ (), 
	replicate (length reattori) $ nodoVuoto
	) :: (T,[SNodo T Utente])

responsabiliQ :: Q -> [(Utente,PublicKey)]
responsabiliQ s = runReader (responsabili error) . fst $ s

aggiornaStato :: MonadError [Char] m 
	=> Chiave
	-> (T, [SNodo T Utente])
	-> [(Firma, [(Chiave, Firma, [String])])]
	-> m ((T, [SNodo T Utente]), [Contestualizzato Utente String] )
aggiornaStato g x = let
	r stato (firma,ps) = do
		let h = showDigest . sha512 $ B.pack $ show stato
		when (not $ verify g (B.pack $ h ++ show ps) firma) $ throwError "throwErrore di integrita della patch di gruppo" 
		let 	rs = map (snd &&& fst) $ responsabiliQ stato
			zs = map (\(puk,_,es) -> zip (repeat (fromJust $ lookup puk rs)) es) ps
		when (not $ all (\(puk,_,_) -> puk `elem` map fst rs) ps ) $ throwError "la patch di gruppo contiene eventi da un utente sconosciuto"
		when (not $ all (\(puk,firma,es) -> verify puk (B.pack $ h ++ concat es) firma) ps) $ 
			throwError "la patch di gruppo contiene una patch di responsabile non integra"
		(stato',logs) <- caricaEventi priorities reattori (concat zs) stato
		tell logs
		return stato'
	in runWriterT . foldM r x

provaStato :: Utente -> IO (Q,PublicKey,(Utente,
