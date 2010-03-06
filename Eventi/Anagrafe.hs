{-# LANGUAGE NoMonomorphismRestriction, StandaloneDeriving, ViewPatterns, FlexibleContexts, ScopedTypeVariables, ExistentialQuantification #-}
-- | Modulo di supporto anagrafico. Prevede un insiemde di eventi per la gestione dell'anagrafe utenti e dell'anagrafe responsabili, inoltre si esportano gli eventi di gestione assenso.
module Eventi.Anagrafe {-(
	eliminazioneResponsabile
	, esistenzaUtente 
	, Anagrafe
	, Responsabili
	, Responsabile
	, statoInizialeAnagrafe
	, esistenzaResponsabile
	, reazioneAnagrafe
	, programmazioneAssenso 
	, Chiave
	, Assensi
	, eventoValidato
	, Utente
	, makeEventiAssenso
	, makeEventiAnagrafe
	, responsabili 
	, utenti
	, priorityAnagrafeI
	, priorityAnagrafe 
	, queryAnagrafe
	, queryAssenso) -}
	where

import Data.List ((\\))
import Control.Applicative ((<$>))
import Control.Arrow ((***),first, (&&&))
import Control.Monad (when, mzero)
import Control.Monad.Reader (asks, MonadReader)
import Control.Monad.Error (throwError)
import Codec.Crypto.RSA (PublicKey (..))

import Lib.Aspetti ((.<),ParteDi,see)
import Lib.Assocs (assente,(?),updateM,elimina)
import Lib.Passo (Costruzione)
import Lib.Response (Response (..))
import Lib.Prioriti (R (..))

import Core.Inserimento (logga, conFallimento, MTInserzione, osserva, modifica, fallimento)
import Core.Programmazione (Inserzione, EventoInterno (..), soloEsterna, nessunEffetto, Reazione (..),Effetti)
import Core.Types (Message)
import Core.Costruzione (Supporto,libero,upload,scelte,runSupporto,CostrAction)
import Core.Parsing (ParserConRead, Parser)
import Lib.Firmabile (Segreto, Chiave)

import Eventi.Servizio 

deriving instance Eq PublicKey
deriving instance Read PublicKey

-- | chiave pubblica di un responsabile
-- type Segreto = 
type Indice = Int
-- | nome di un utente
type Utente = String

-- | il pezzo di stato necessario all'anagrafe per funzionare
type TyAnagrafe a = (Anagrafe , (Responsabili , (Servizio Assensi, a)))

-- | fornisce una hof validante sull'esistenza del responsabile
validante ::(ParteDi Anagrafe s, ParteDi Responsabili s) 
	=> Utente 
	-> (Utente -> MTInserzione s c d b) 
	-> Inserzione s c d (Maybe b)
validante r f = conFallimento $ esistenzaResponsabile r >> f r

-- | la lista di utenti 
data Anagrafe = Anagrafe [Utente] deriving (Show,Read)

utenti = (\(Anagrafe us) -> us) . see
-- | aggiunge la parte anagrafica dello stato allo stato iniziale
bootAnagrafe :: [Responsabile] ->  a -> TyAnagrafe a
bootAnagrafe unos x = Anagrafe (map fst unos) .< Responsabili unos [] .< servizio0 .< x

-- | un utente con chiave pubblica
type Responsabile = (Utente,(Chiave,Segreto))

-- | la lista dei responsabili eletti e in elezione
data Responsabili = Responsabili {eletti::[Responsabile], inodore ::[(Indice,Responsabile)]} deriving (Show,Read)

responsabili = (\(Responsabili es is) -> (es, map snd is)) . see
-- | mappa di priorita' per gli eventi di questo modulo
priorityAnagrafe = R k where
	k (NuovoUtente _) = -20
	k (EliminazioneResponsabile _) = -19
	k (ElezioneResponsabile _) = -18
	
priorityAnagrafeI = R k where
	k (EventoEliminazioneResponsabile _ _) = 20

-- | eventi provenienti dall'esterno
data EsternoAnagrafico 
	= NuovoUtente String -- ^ inserimento di un nuovo utente 
	| ElezioneResponsabile Responsabile -- ^ richiesta di promozione a responsabile per un utente
	| EliminazioneResponsabile String -- ^ richiesta di dimissioni da responsabile per un responsabile
	deriving (Read,Show)

-- | eventi prodotti all'interno
data InternoAnagrafico 
	= EventoEliminazioneResponsabile Utente Utente -- ^ messaggio di avvenuta eliminazione di un responsabile
	deriving (Show, Read)

-- | matcher per evento interno  di avvenuta eliminazione di un responsabile
eliminazioneResponsabile (EventoEliminazioneResponsabile w r) = Just (w,r)
eliminazioneResponsabile _ = Nothing

-- | controlla l'esistenza di un utente
esistenzaUtente ::  (Anagrafe `ParteDi` s) => Utente -> MTInserzione s c d ()
esistenzaUtente u  = do 
	Anagrafe us <- osserva 
	fallimento (u `notElem` us) $ "utente " ++ show u ++ " sconosciuto" 

-- | controlla l'esistenza di un responsabile
esistenzaResponsabile :: (Responsabili `ParteDi` s, Anagrafe `ParteDi` s) => Utente -> MTInserzione s c d ()
esistenzaResponsabile u = do
	esistenzaUtente u 
	Responsabili us _ <- osserva 
	fallimento (u `assente` us) $ "responsabile " ++ show u ++ " non eletto"


-- | la reazione agli eventi anagrafici
reazioneAnagrafe :: (
	ParteDi (Servizio Assensi) s,
	ParteDi Anagrafe s,
	ParteDi Responsabili s,
	Parser c EsternoAssenso,
	Parser c EsternoAnagrafico) =>
	Reazione s c Utente

reazioneAnagrafe = soloEsterna reattoreAnagrafe' where
	reattoreAnagrafe' (first validante -> (w,NuovoUtente u)) = w $ \r -> do
		Anagrafe us <- osserva
		fallimento (u `elem` us) "utente gia' presente" 
		modifica . const $ Anagrafe (u:us)
		logga $ "accettato nuovo utente " ++ show u
		return (True, nessunEffetto)
	reattoreAnagrafe' (first validante -> (w,ElezioneResponsabile u)) = w $ \r -> do
		Anagrafe us <- osserva
		esistenzaUtente (fst u)
		Responsabili us ls <- osserva 
		fallimento (u `elem` us) "utente gia' eletto" 
		fallimento (u `elem` map snd ls) "utente gia' in elezione" 
		(l,reaz) <- programmazioneAssenso ("elezione dell'utente " ++ show u) r maggioranza (chiudibene u) (chiudimale u)
		modifica $ \(Responsabili us ls) -> Responsabili us ((l,u):ls)
		return (True,([reaz],[])) 
		where
		chiudibene u l = do
			modifica $ \(Responsabili us ls) -> Responsabili (u:us) (filter ((/=) l . fst) ls)
			logga $ "nuovo responsabile " ++ show u
			return nessunEffetto
		chiudimale u l = do
			modifica $ \(Responsabili us ls) -> Responsabili us (filter ((/=) l . fst) ls)
			logga $ "elezione del responsabile " ++ show u ++ " fallita"
			return nessunEffetto

	reattoreAnagrafe' (first validante -> (w,EliminazioneResponsabile u)) = w $ \r -> do
		Anagrafe us <- osserva
		esistenzaUtente u
		esistenzaResponsabile u
		Responsabili us ls <- osserva 
		fallimento (u `elem` map (fst . snd) ls) "responsabile gia' in eliminazione" 
		(l,reaz) <- programmazioneAssenso ("eliminazione del responsabile "++ show u) r maggioranza (chiudibene u r) (chiudimale u r)
		modifica $ \(Responsabili us ls) -> (Responsabili us ((l,(u,us ? (u,undefined))):ls))
		return (True,([reaz],[]))
		where
		chiudibene u r l = do
			modifica $ \(Responsabili us ls ) -> Responsabili (elimina u us) (elimina l ls)
			logga $ "responsabile eliminato " ++ show u
			return ([],[EventoInterno $ EventoEliminazioneResponsabile u r])
		chiudimale u r l = do
			modifica $ \(Responsabili us ls ) -> Responsabili us  (elimina l ls)
			logga $ "richiesta eliminazione responsabile " ++ show u ++ " fallita"
			return nessunEffetto
------------------------------------------------------------------------------------------------
-- reparto costruzione ed interrogazione
------------------------------------------------------------------------------------------------
 
-- | estrae la lista di costrResponsabili
costrResponsabili :: (Monad m , ParteDi Responsabili s) => Supporto m s b [Responsabile]	
costrResponsabili = do
	(rs,_) <- asks responsabili
	when (null rs) $ throwError "nessun responsabile presente" 
	return rs

-- | estrae la lista di costrUtenti
costrUtenti :: (Monad m, ParteDi Anagrafe s) => Supporto m s b [Utente]	
costrUtenti = do
	rs <- asks utenti
	when (null rs) $ throwError "nessun utente presente" 
	return rs

-- | costruzione degli eventi esterni per la gestione costrUtenti e costrResponsabili
costrEventiAnagrafe :: (Monad m, ParteDi Anagrafe s, ParteDi Responsabili s) => CostrAction m c EsternoAnagrafico s
costrEventiAnagrafe s kp kn =	[("inserimento nuovo utente", eventoNuovoUtente)]
	where
	run = runSupporto s kn kp
        eventoNuovoUtente =  run $ do
		us <- costrUtenti 
                n <- libero "il nome del nuovo utente"
		when (n `elem` us) $ throwError "utente già presente"	
                return $ NuovoUtente n
costrEventiResponsabili :: (Monad m, ParteDi Responsabili s, ParteDi Anagrafe s) => CostrAction m c EsternoAnagrafico s
costrEventiResponsabili s kp kn =
	[("elezione di un nuovo responsabile", eventoElezioneResponsabile)
	,("richiesta di eliminazione di un responsabile",eventoEliminazioneResponsabile)
	] 
	where
	run = runSupporto s kn kp
        eventoElezioneResponsabile = run $ do
		us <- costrUtenti 
		rs <- costrResponsabili 
		let ds = us \\ map fst rs
		when (null ds) $ throwError "nessun utente non responsabile disponibile"
                n <- scelte (map (id &&& id) ds) "selezione eleggibile" 
                m <- upload $ "chiavi per l'utente" ++ n
                return $ ElezioneResponsabile m

        eventoEliminazioneResponsabile = run $ do
		rs <- costrResponsabili 
                n <- scelte (map (fst &&& id) rs) "selezione responsabile da eliminare"
                return $ EliminazioneResponsabile (fst n)

-- | costruzione delle interrogazione sull'anagrafe e sui costrResponsabili
costrQueryAnagrafe :: (Monad m, ParteDi Anagrafe s, ParteDi Responsabili s) => CostrAction m c Response s
costrQueryAnagrafe s kp kn = 	[("la chiave pubblica di un responsabile",queryChiave)
				,("elenco nomi utenti",queryElencoUtenti)
				,("elenco nomi responsabili",queryElencoResponsabili)
				] 
	where
	run = runSupporto s kn kp
	queryChiave = run $ do
		rs <- costrResponsabili 
		(u,v) <- scelte (map (fst &&& id) rs) "selezione responsabile" 
		return $ Response [("responsabile",ResponseOne u),("chiave pubblica",ResponseOne v)]
	queryElencoUtenti = run $ do
		us <- costrUtenti 
		return $ Response [("elenco nomi utenti", ResponseMany $ map ResponseOne us)]
	queryElencoResponsabili = run $ do
		rs <- costrResponsabili 
		return $ Response [("elenco nomi responsabili", ResponseMany $ map (ResponseOne. fst) rs )]
		
----------------------------------------------------------------------------------------------------------------------
--  sezione assensi, putroppo non ha un modulo a parte a causa del ciclo di dipendenze con l'anagrafe

-- | gli eventi che interessano una raccolta di assensi
data EsternoAssenso = Assenso Indice | Dissenso Indice | EventoFallimentoAssenso Indice deriving (Read, Show)

-- | lo stato necessario per la gestione di un tipo di assensi
data Assensi = Assensi [Utente] [Utente] deriving (Show,Read)


data Check = Positivo | Negativo | Indecidibile deriving (Show,Read)

-- controlla che la maggioranza sia raggiunta
maggioranza :: (Responsabili `ParteDi` s) => ([Utente],[Utente]) -> MTInserzione s c Utente Check
maggioranza (ps,ns) = do 	
	Responsabili us _ <- osserva
	let soglia =  (length us + 1) `div` 2
	return $ if length ns >= soglia then Negativo else 
			if length ps >= soglia then Positivo else
				Indecidibile


-- | funzione di programmazione per una nuova raccolta di assensi
programmazioneAssenso :: (
	Servizio Assensi `ParteDi` s
	, Responsabili `ParteDi` s
	, Anagrafe `ParteDi` s
	, Parser c EsternoAssenso
	)
	=> String	-- ^ nome della raccolta 
	-> Utente 	-- ^ l'utente che la richiede
	-> (([Utente],[Utente]) -> MTInserzione s c Utente Check) -- ^ condizione di rolling 
	-> (Indice -> MTInserzione s c Utente (Effetti s c Utente)) -- ^ la chiusura per il successo della raccolta
	-> (Indice -> MTInserzione s c Utente (Effetti s c Utente)) -- ^ la chiusura per il fallimento della raccolta
	-> MTInserzione s c Utente (Int, Reazione s c Utente)	-- ^ la chiave per emettere assensi relativi e la reazione da schedulare

programmazioneAssenso se ur c k kn = do
	l <- nuovoStatoServizio (Assensi [] []) se -- ricevi la chiave per la nuova raccolta
	let 	eliminaRichiesta u j = do
			fallimento (ur /= u) "eliminazione di una richiesta di assenso effettuata da altro"
			eliminaStatoServizio j (undefined :: Assensi)  
			logga $ "chiusa la raccolta di assensi numero " ++ show l
			return (False,nessunEffetto) -- non rischedula il reattore

		reattoreAssenso (Right (first validante -> (w,Assenso j))) = w $ \r -> do
			when (j /= l) mzero
			Assensi ps ns <- osservaStatoServizio j 
			fallimento (r `elem` (ps ++ ns)) "il responsabile ha gia' posto un giudizio sulla richiesta" 
			let ps' = r:ps -- la nuova lista di assensi per j 
			t <- c (ps',ns) -- controlla che si debba continuare a ricevere gli assensi
			case t of
				Positivo  -> do
					eliminaStatoServizio j (undefined :: Assensi) 
					(,) False <$> k j -- esegui la procedura finale come coronamento del 
						-- consenso e non rischedula il reattore 
				Indecidibile -> do		
					logga $ "ricevuto assenso da " ++ show r ++ " sulla raccolta numero " ++ show j
					modificaStatoServizio j $ \_ -> return (Assensi ps' ns) -- registra gli assensi
					return (True,nessunEffetto) -- continua a ricevere

		reattoreAssenso (Right (first validante -> (w,Dissenso j))) = w $ \r -> do
			when (j /= l) mzero
			Assensi ps ns <- osservaStatoServizio j 
			fallimento (r `elem` (ps ++ ns)) "il responsabile ha gia' posto un giudizio sulla richiesta" 
			let ns' = r:ns -- la nuova lista di assensi per j 
			t <- c (ps,ns') -- controlla che si debba continuare a ricevere gli assensi
			case t of 	
				Negativo -> do
					eliminaRichiesta r j
					(,) False <$> kn j
				Indecidibile -> do		
					logga $ "ricevuto dissenso da " ++ show r ++ " sulla raccolta numero " ++ show j
					modificaStatoServizio j $ \_ -> return (Assensi ps ns') -- registra gli assensi
					return (True,nessunEffetto) -- continua a ricevere

		reattoreAssenso (Right (first validante -> (w,EventoFallimentoAssenso j))) = w $ \r -> do
			when (j /= l) mzero
			eliminaRichiesta r j
			(,) False <$> kn j
		reattoreAssenso (Left (eliminazioneResponsabile -> Just (u,r))) = conFallimento $ do
			when (ur /= u) mzero
			eliminaRichiesta u l
			(,) False <$> kn l
		reattoreAssenso (Left _) = return Nothing
	logga $ "aperta la raccolta di assensi numero " ++ show l
 	return (l,Reazione (Nothing, reattoreAssenso)) -- restituisce il riferimento a questa richiesta perché venga nominato negli eventi di assenso

--------------------------- costruzioni per il modulo assensi -----------------------------

-- | estrae gli assensi dallo stato in lettura
assensi :: (Monad m, ParteDi (Servizio Assensi) s) => Supporto m s b [(String, Int)]
assensi = do
	xs :: [(Int,(String,Assensi))] <- asks elencoSottoStati 
	when (null xs) $ throwError "nessuna raccolta di assensi attiva"
	return  $ map (fst . snd &&& fst) xs

assensiFiltrati u = do
	xs :: [(Int,(String,Assensi))] <- filter (not . elem u . (\(Assensi ps ns) -> ps ++ ns) . snd . snd)
		<$> asks elencoSottoStati 
	when (null xs) . throwError $ "nessuna raccolta di assensi aperta per l'utente " ++ u
	return  $ map (fst . snd &&& fst)  $ xs

-- | costrutore degli eventi di assenso
costrEventiAssenso :: (Monad m, Servizio Assensi `ParteDi` s) => Utente -> CostrAction m c EsternoAssenso s
costrEventiAssenso u s kp kn = 	[("fallimento di una raccolta di assensi",eventoFallimentoAssenso)
				,("attribuzione di un assenso",eventoAssenso u s kp kn )
				] 
	where
	eventoAssenso u s kp kn = runSupporto s kn kp $ do
		ys <- assensiFiltrati u
		n <- scelte ys "seleziona la questione"
		ad <- scelte [("assenso",Assenso),("dissenso",Dissenso)] "esprimi un parere" 
		return $ ad n

        eventoFallimentoAssenso = runSupporto s kn kp $ do
		ys <- assensi
                n <- scelte ys "selezione richiesta per fallire" 
              	return $ EventoFallimentoAssenso n

-- | costruzione delle interrogazioni sul modulo di assensi
costrQueryAssenso :: (Monad m , Servizio Assensi `ParteDi` s) => CostrAction m c Response s
costrQueryAssenso s kp kn = [("elenco richieste di assenso aperte", querySottoStati)] 
	where
	querySottoStati = runSupporto s kn kp $ do
		ys <- assensi
		return $ Response [("elenco richieste di assenso aperte", if null ys then
			ResponseOne "nessuna richiesta aperta" else Response (map (show . snd &&& ResponseOne . fst) ys))]
priorityAssenso = R k where
	k (Assenso _) = -15
	k (Dissenso _) = -14
	k (EventoFallimentoAssenso _) = -16
---------------------------------------------------


