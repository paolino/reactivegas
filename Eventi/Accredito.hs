{-# LANGUAGE ScopedTypeVariables, ViewPatterns, NoMonomorphismRestriction, FlexibleContexts #-}
-- | modulo per la gestione dei conti utente e responsabile, detti accredito e saldo
module Eventi.Accredito {-(
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
	)-} where

import Control.Monad.Reader (MonadReader, asks)
import Control.Monad (when)
import Control.Arrow (second, (&&&), first)

import Core.Programmazione (Reazione, soloEsterna, nessunEffetto)
import Core.Inserimento (MTInserzione, fallimento, osserva, modifica, logga)
import Core.Costruzione (libero, scelte, CostrAction, runSupporto)
import Core.Parsing (Parser)
import Lib.Costruzione (Costruzione)
import Eventi.Anagrafe (Anagrafe, Utente, esistenzaUtente, utenti, Responsabili, 
	esistenzaResponsabile, responsabili, validante)
import Lib.Aspetti ((.<), see, ParteDi)
import Lib.Prioriti (R (..))
import Lib.Assocs (update , (?))
import Lib.Response (Response (..))

-- | evento esterno che interessa il controllo del credito o del saldo
data EsternoAccredito = Accredito Utente Float | Saldo Utente Float deriving (Show, Read)

-- | priorita' per gli eventi del modulo
priorityAccredito = R k where
	k (Accredito _ _) = - 15
	k (Saldo _ _) = -15

-- | stato degli accrediti utente
data Conti = Conti [(Utente,Float)] deriving (Read, Show)

-- | stato dei saldi responsabile
data Saldi = Saldi [(Utente,Float)] deriving (Read, Show)

-- | tipo aggiunto dello stato necessario al modulo
type TyAccredito a = (Conti , (Saldi , a))

-- | aggiunge lo stato del modulo allo stato passato
bootAccredito :: a -> TyAccredito a
bootAccredito x = Conti [] .< Saldi [] .< x

-- | esegue un prelievo da un conto utente
preleva :: (Anagrafe `ParteDi` s, Conti `ParteDi` s) => Utente -> Float -> MTInserzione s c Utente ()
preleva u dv = do
	fallimento (dv <= 0) "tentato un prelievo negativo o nullo"
	esistenzaUtente u 
	Conti us <- osserva
	fallimento (us ? (u,0) < dv) "il credito non é sufficiente per la richiesta" 
	aggiornaCredito u (subtract dv) 

-- | esegue un accredito su un conto utente
accredita :: (Anagrafe `ParteDi` s, Conti `ParteDi` s) => Utente -> Float -> MTInserzione s c Utente ()
accredita u dv = do
	fallimento (dv <= 0) "tentato un accredito negativo o nullo"
	esistenzaUtente u 
	Conti us <- osserva
	aggiornaCredito u (+ dv) 
-- | modifica il saldo di un responsabile
salda :: (Anagrafe `ParteDi` s, Responsabili `ParteDi` s, Saldi `ParteDi` s) => Utente -> (Float -> Float) -> MTInserzione s c Utente ()
salda u dv = esistenzaResponsabile u >> aggiornaSaldo u dv

-- | LL : modifica un credito
aggiornaCredito :: (Anagrafe `ParteDi` s, Conti `ParteDi` s) => Utente -> (Float -> Float) -> MTInserzione s c Utente ()
aggiornaCredito u dv = modifica $ \(Conti us) -> Conti (update u dv 0 us)
-- | LL : modifica un saldo
aggiornaSaldo :: (Anagrafe `ParteDi` s, Responsabili `ParteDi` s, Saldi `ParteDi` s) => Utente -> (Float -> Float) -> MTInserzione s c Utente ()
aggiornaSaldo u dv = modifica $ \(Saldi us) -> Saldi (update u dv 0 us)

-- | il caricatore di eventi per questo modulo
reazioneAccredito :: (
	Responsabili `ParteDi` s
	, Saldi `ParteDi` s
	, Anagrafe `ParteDi` s
	, Conti `ParteDi` s
	, Parser c EsternoAccredito
	) => Reazione s c Utente
reazioneAccredito = soloEsterna reattoreAccredito where
	reattoreAccredito (first validante -> (wrap,Accredito u dv)) = wrap $ \r -> do
		accredita u dv
		salda r (+dv)
		logga $ "accreditate " ++ show dv ++ " euro a " ++ show u
		return (True,nessunEffetto)	
	reattoreAccredito (first validante -> (wrap ,Saldo u dv)) = wrap $ \r -> do
		esistenzaResponsabile u
		fallimento (dv <= 0) "tentato un saldo negativo o nullo"
		modifica $ \(Saldi us) -> Saldi (update r (+ dv) 0 (update u (subtract dv) 0 us))
		logga $ "spostati " ++ show dv ++ " euro dal saldo di " ++ show u ++ " al saldo di " ++ show r
		return (True,nessunEffetto)

-- | costruttore di eventi per il modulo di accredito
costrEventiAccredito :: (ParteDi Responsabili s, ParteDi Anagrafe s) => CostrAction c EsternoAccredito s
costrEventiAccredito s kp kn = 	[("attribuzione accredito per un utente",eventoAccredito) 
				,("ricezione saldo da un responsabile", eventoSaldo)
				] 
	where
	run = runSupporto s kn kp
	eventoAccredito = run $ do
		us <- asks utenti 
		u <- scelte (map (id &&& id) us) "selezione utente"
		n <- libero "la somma da accreditare"
		return $ Accredito u n
	eventoSaldo = run $ do
		(rs,_) <- asks responsabili 
		u <- scelte (map (fst &&& id) rs) "selezione responsabile"
		n <- libero "la somma ricevuta"
		return $ Saldo (fst u) n
	    
-- | costruttore interrogazioni sul modulo accrediti
costrQueryAccredito :: (Conti `ParteDi` s, Saldi `ParteDi` s) => CostrAction c Response s
costrQueryAccredito s kp kn = 	[("accrediti degli utenti", queryUtente)
				,("saldi dei responsabili", queryResponsabile)
				] 
	where
	run = runSupporto s kn kp
	queryUtente = run $ do
		Conti us <- asks see 
		return $ Response [("accrediti degli utenti", if null us then 
			ResponseOne "nessun utente possiede un accredito" else ResponseAL us)]
	queryResponsabile = run $ do
		Saldi rs <- asks see 
		return $ Response [("saldi dei responsabili" , if null rs then 
			ResponseOne "nessun responsabile ha un saldo" else ResponseAL rs)]
