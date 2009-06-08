{-# LANGUAGE ScopedTypeVariables, ViewPatterns, NoMonomorphismRestriction #-}
-- | modulo per la gestione dei conti utente e responsabile
module Accredito (Accredito (..), Conti, Saldi, preleva, accredita, salda, reazioneAccredito , statoInizialeAccredito ,makeAccredito,priorityAccredito) where

import Codec.Binary.UTF8.String
import Core
import Lib0
import Lib1
import Costruzione
import Anagrafe
import Data.Maybe
import Control.Monad.Reader
import Control.Arrow
import Aspetti ((.<))
import Prioriti

data Accredito = Accredito Utente Float | Saldo Utente Float deriving (Show, Read)
priorityAccredito = R k where
	k (Accredito _ _) = - 15
	k (Saldo _ _) = -15

data Conti = Conti [(Utente,Float)] deriving (Read, Show)
data Saldi = Saldi [(Utente,Float)] deriving (Read, Show)

statoInizialeAccredito x = Conti [] .< Saldi [] .< x

preleva u dv = do
	fallimento (dv <= 0) "tentato un prelievo negativo o nullo"
	esistenzaUtente u 
	Conti us <- osserva
	fallimento (us ? (u,0) < dv) $ encodeString "il credito non é sufficiente per la richiesta" 
	aggiornaCredito u (subtract dv) 

accredita u dv = do
	fallimento (dv <= 0) "tentato un accredito negativo o nullo"
	esistenzaUtente u 
	Conti us <- osserva
	aggiornaCredito u (+ dv) 

salda u dv = esistenzaResponsabile u >> aggiornaSaldo u dv

aggiornaCredito u dv = modifica $ \(Conti us) -> Conti (update u dv 0 us)
aggiornaSaldo u dv = modifica $ \(Saldi us) -> Saldi (update u dv 0 us)

reazioneAccredito = soloEsterna reattoreAccredito

reattoreAccredito (eventoValidato -> (wrap,Accredito u dv)) = wrap $ \r -> do
	accredita u dv
	logga $ "accreditate " ++ show dv ++ " euro a " ++ show u
	return (True,nessunEffetto)	
reattoreAccredito (eventoValidato -> (wrap ,Saldo u dv)) = wrap $ \r -> do
	esistenzaResponsabile u
	fallimento (dv <= 0) "tentato un saldo negativo o nullo"
	modifica $ \(Saldi us) -> Saldi (update r (+ dv) 0 (update u (subtract dv) 0 us))
	logga $ "spostati " ++ show dv ++ " euro dal saldo di " ++ show u ++ " al saldo di " ++ show r
	return (True,nessunEffetto)

makeAccredito = [eventoAccredito , eventoSaldo] where

	eventoAccredito k = (,) "evento di accredito" $ \s -> do
	    when (null $ utenti s) $ k "nessun utente disponibile"
	    u <- parametro (Scelta "selezione utente". map (id &&& id) . utenti  $ s)
	    n <- parametro (Libero "la somma da accreditare")
	    return $ show (Accredito u n)

	eventoSaldo k =  (,) "evento di saldo" $ \s -> do
	    when (null $ utenti s) $ k "nessun responsabile disponibile"
	    u <- parametro (Scelta "selezione responsabile" . map (fst &&& id) . responsabili $ s)
	    n <- parametro (Libero "la somma ricevuta")
	    return $ show (Saldo (fst u) n)

