{-# LANGUAGE ScopedTypeVariables, ViewPatterns, NoMonomorphismRestriction, FlexibleContexts, DeriveDataTypeable #-}
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
import Data.Maybe (fromJust, isNothing)
import Data.Typeable (Typeable, cast)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad (when)
import Control.Monad.Error (throwError)
import Control.Arrow (second, (&&&), first, (***))

import Core.Types (Utente)
import Core.Programmazione (Reazione, soloEsterna, nessunEffetto, Message (..))
import Core.Inserimento (MTInserzione, fallimento, osserva, modifica, logga, loggamus)
import Core.Costruzione (libero, scelte, CostrAction, runSupporto)
import Core.Parsing (Parser)
import Lib.Costruzione (Costruzione)
import Eventi.Anagrafe (Anagrafe, esistenzaUtente, utenti, Responsabili, 
	esistenzaResponsabile, responsabili, validante, SUtente (..))
import Lib.Aspetti ((.<), see, ParteDi)
import Lib.Prioriti (R (..))
import Lib.Assocs (update , (?), upset)
import Lib.Response (Response (..))
import Lib.ShowRead
import Lib.Euro


data Movimento = MovimentoU Utente DEuro String | MovimentoR Utente DEuro String  deriving Typeable

instance Show Movimento where
	show (MovimentoU u e s) = "movimento di " ++ show e ++ " sul conto di " ++ u ++ " per " ++ s 
	show (MovimentoR u e s) = "movimento di " ++ show e ++ " sulla cassa di " ++ u ++ " per " ++ s


-- | evento esterno che interessa il controllo del credito o del saldo
data EsternoAccredito = Accredito Utente DEuro | Addebito Utente String DEuro | Saldo Utente DEuro 


instance Show EsternoAccredito where
	show (Accredito u f) = "accredito a " ++ quote u ++ " di " ++ show f
	show (Saldo u f) = "movimento dalla cassa di " ++ quote u ++ " di " ++ show f
	show (Addebito u s f) = "addebito a " ++ quote u ++ " di " ++ show f ++ " per " ++ quote s
instance Read EsternoAccredito where
	readPrec = let 
		acc = do
			string "accredito a "
			u <- phrase
			string " di "
			f <- reads'
			return $ Accredito u f
		add = do 
			string "addebito a "
			u <- phrase
			string " di "
			f <- reads'
			string " per "
			s <- phrase
			return $ Addebito u s f
		sal = do
			string "movimento dalla cassa di "
			u <- phrase
			string " di "
			f <- reads'
			return $ Saldo u f
		in lift $ acc <++ sal <++ add
-- | priorita' per gli eventi del modulo
priorityAccredito = R k where
	k (Accredito _ _) = -35
	k (Saldo _ _) = -35
	k (Addebito _ _ _) = -34
-- | stato degli accrediti utente
data Conti = Conti [(Utente,Euro)] deriving (Read, Show,Eq)

-- | stato dei saldi responsabile
data Saldi = Saldi [(Utente,Euro)] deriving (Read, Show,Eq)

reportCrediti :: (Conti `ParteDi` s) => s -> [(Utente,Euro)]
reportCrediti = (\(Conti xs) -> xs) . see 

reportCasse :: (Saldi `ParteDi` s) => s -> [(Utente,Euro)]
reportCasse = (\(Saldi xs) -> xs) . see 
-- | tipo aggiunto dello stato necessario al modulo
type TyAccredito a = (Conti , (Saldi , a))

-- | aggiunge lo stato del modulo allo stato passato
bootAccredito :: a -> TyAccredito a
bootAccredito x = Conti [] .< Saldi [] .< x

-- | esegue un accredito su un conto utente
accredita :: (Anagrafe `ParteDi` s, Conti `ParteDi` s) => Utente -> DEuro -> String -> MTInserzione s c Utente ()
accredita u dv s = do
	esistenzaUtente u 
	Conti us <- osserva
	let r = dv $^ (us ? (u,0)) 
	fallimento (r < 0) "conti in rosso non ammessi" 
	modifica $ \(Conti us) -> Conti (upset u r us)
	logga . Message $ MovimentoU u dv s
	
-- | modifica il saldo di un responsabile
salda :: (Anagrafe `ParteDi` s, Responsabili `ParteDi` s, Saldi `ParteDi` s) => Utente -> DEuro 
	-> String -> MTInserzione s c Utente ()
salda u dv s = do 
	esistenzaResponsabile u 
	modifica $ \(Saldi us) -> Saldi (update u (dv $^) 0 us)
	logga . Message $ MovimentoR u dv s

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
		fallimento (r == u) "aggiornamento del proprio credito di utente"
		fallimento (dv $^ 0 <= 0) "accredito negativo"
		accredita u dv $ "versamento presso il cassiere " ++ quote r
		salda r dv $ "funzione di cassiere per " ++ quote u
		loggamus $ "accreditate " ++ show dv ++ " a " ++ quote u
		return (True,nessunEffetto)	
	reattoreAccredito (first validante -> (wrap,Addebito u s dv)) = wrap $ \r -> do
		fallimento (r == u) "aggiornamento del proprio credito di utente"
		fallimento (dv $^ 0 <= 0) "prelievo negativo o nullo"
		accredita u (opposite dv) $ "prelievo attraverso il cassiere " ++ quote r ++ " per " ++ quote s
		salda r (opposite dv) $ "funzione di cassiere per " ++ quote u
		loggamus $ "prelevate " ++ show dv ++ " a " ++ quote u ++ " per " ++ s
		return (True,nessunEffetto)
	reattoreAccredito (first validante -> (wrap ,Saldo u dv)) = wrap $ \r -> do
		esistenzaResponsabile u
		fallimento (u == r) "movimento di denaro riferito ad una cassa sola"
		fallimento (dv $^ 0 <= 0) "saldo negativo o nullo"
		salda r dv $ "trasferiti dalla cassa di " ++ quote u
		salda u (opposite dv) $ "trasferiti alla cassa di " ++ quote r
		loggamus $ "spostati " ++ show dv ++ " dalla cassa di " ++ quote u ++ " alla cassa di " ++ quote r
		return (True,nessunEffetto)

-- | costruttore di eventi per il modulo di accredito
costrEventiAccredito :: (Monad m, ParteDi Responsabili s, SUtente `ParteDi` s, ParteDi Anagrafe s) => CostrAction m c EsternoAccredito s
costrEventiAccredito s kp kn = 	[("versamento sul conto di un utente",eventoAccredito) 
				,("prelievo motivato dal conto di un utente",eventoAddebito) 
				,("ricezione saldo da un responsabile", eventoSaldo)
				] 
	where
	run = runSupporto s kn kp
	eventoAccredito = run $ do
		us <- asks utenti 
		SUtente un <- asks see
		when (isNothing un) $ throwError "manca la scelta del responsabile autore"
		let us' = map (id &&& id) $ filter ((/=) $ fromJust un)  us
		when (null us') $ throwError "non ci sono altri utenti"
		u <- scelte us' "utente interessato dall'aggiornamento"
		n <- libero $ "somma da accreditare sul conto di " ++ u
		return $ Accredito u n	
	eventoAddebito = run $ do
		us <- asks utenti 
		SUtente un <- asks see
		when (isNothing un) $ throwError "manca la scelta del responsabile autore"
		let us' = map (id &&& id) $ filter ((/=) $ fromJust un)  us
		when (null us') $ throwError "non ci sono altri utenti"
		u <- scelte us' "utente interessato dall'aggiornamento"
		n <- libero $ "somma da prelevare dal conto di " ++ u
		s <- libero $ "motivazione del prelievo"
		return $ Addebito u s n 
	eventoSaldo = run $ do
		(rs,_) <- asks responsabili 
		SUtente un <- asks see
		when  (isNothing un) $ throwError "manca la scelta del responsabile autore"
		let rs' = map (fst &&& id) (filter ((/=) (fromJust un) . fst) rs)
		when (null rs') $ throwError "non ci sono altri responsabili"
		u <- scelte rs' "responsabile che ha dato il denaro"
		n <- libero $ "somma ricevuta dal responsabile " ++ fst u
		return $ Saldo (fst u) n
	    
-- | costruttore interrogazioni sul modulo accrediti
costrQueryAccredito :: (Monad m, Conti `ParteDi` s, Saldi `ParteDi` s) => CostrAction m c Response s
costrQueryAccredito s kp kn = 	[("crediti degli utenti", queryUtente)
				,("casse dei responsabili", queryResponsabile)
				] 
	where
	run = runSupporto s kn kp
	queryUtente = run $ do
		Conti us <- asks see 
		return $ Response [("crediti degli utenti", if null us then 
			ResponseOne "nessun utente possiede un credito" else ResponseMany (map (ResponseOne *** id) us))]
	queryResponsabile = run $ do
		Saldi rs <- asks see 
		return $ Response [("casse dei responsabili" , if null rs then 
			ResponseOne "nessun responsabile possiede una cassa attiva" else ResponseMany (map (ResponseOne *** id) rs))]
