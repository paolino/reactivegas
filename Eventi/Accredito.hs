{-# LANGUAGE ScopedTypeVariables, ViewPatterns, NoMonomorphismRestriction, FlexibleContexts #-}
-- | modulo per la gestione dei conti utente e responsabile
module Eventi.Accredito (
--	Accredito,
	Conti,
	Saldi,
	preleva,
	accredita,
	salda,
	reazioneAccredito ,
	statoInizialeAccredito ,
	makeAccredito,
	priorityAccredito,
	queryAccredito
	) where

import Control.Monad.Reader (MonadReader, asks)
import Control.Monad (when)
import Control.Arrow (second, (&&&))

import Core.Programmazione (soloEsterna, nessunEffetto)
import Core.Inserimento (fallimento, osserva, modifica, logga)
import Lib.Costruzione (parametro, Svolgimento, Response (..), SceltaOLibero (..))
import Eventi.Anagrafe (Anagrafe, Utente, esistenzaUtente, utenti, Responsabili, 
	esistenzaResponsabile, responsabili, eventoValidato)
import Lib.Aspetti ((.<), see, ParteDi)
import Lib.Prioriti (R (..))
import Lib.Assocs (update , (?))

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
	fallimento (us ? (u,0) < dv) "il credito non é sufficiente per la richiesta" 
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
	salda r (+dv)
	logga $ "accreditate " ++ show dv ++ " euro a " ++ show u
	return (True,nessunEffetto)	
reattoreAccredito (eventoValidato -> (wrap ,Saldo u dv)) = wrap $ \r -> do
	esistenzaResponsabile u
	fallimento (dv <= 0) "tentato un saldo negativo o nullo"
	modifica $ \(Saldi us) -> Saldi (update r (+ dv) 0 (update u (subtract dv) 0 us))
	logga $ "spostati " ++ show dv ++ " euro dal saldo di " ++ show u ++ " al saldo di " ++ show r
	return (True,nessunEffetto)

makeAccredito :: (
	ParteDi Responsabili s, 
	ParteDi Anagrafe s,
	MonadReader s m) =>
	[(String -> Svolgimento b m ()) -> (String, Svolgimento b m String)]

makeAccredito = [eventoAccredito , eventoSaldo] where

	eventoAccredito k = (,) "attribuzione accredito per un utente" $ do
		us <- utenti k 
		u <- parametro (Scelta "selezione utente". map (id &&& id) $ us)
		n <- parametro (Libero "la somma da accreditare")
		return $ show (Accredito u n)

	eventoSaldo k =  (,) "ricezione saldo da un responsabile" $ do
		rs <- responsabili k
		u <- parametro (Scelta "selezione responsabile" . map (fst &&& id) $ rs)
		n <- parametro (Libero "la somma ricevuta")
		return $ show (Saldo (fst u) n)
	    
queryAccredito :: (
	Conti `ParteDi` s, 
	Saldi `ParteDi` s, 
	MonadReader s m) =>
	[(String -> Svolgimento b m a) -> (String, Svolgimento b m Response)]
    
queryAccredito = [queryUtente, queryResponsabile] where
	queryUtente k = (,) "accrediti degli utenti" $ do
		Conti us <- asks see 
		return $ Response [("accrediti degli utenti", if null us then 
			ResponseOne "nessun utente possiede un accredito" else ResponseAL us)]
	queryResponsabile k = (,) "saldi dei responsabili" $ do
		Saldi rs <- asks see 
		return $ Response [("saldi dei responsabili" , if null rs then 
			ResponseOne "nessun responsabile ha un saldo" else ResponseAL rs)]
