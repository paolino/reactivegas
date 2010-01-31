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
import Lib.Costruzione  (Costruzione)
import Lib.Response (Response (..))
import Lib.Prioriti (R (..))

import Core.Inserimento (logga, conFallimento, MTInserzione, osserva, modifica, fallimento)
import Core.Programmazione (Inserzione, EventoInterno (..), soloEsterna, nessunEffetto, Reazione (..),Effetti)
import Core.Types (Message)
import Core.Costruzione (Supporto,libero,dafile,scelte,runSupporto,CostrAction)
import Core.Parsing (ParserConRead, Parser)

import Eventi.Servizio 

deriving instance Read PublicKey
deriving instance Eq PublicKey

-- | chiave pubblica di un responsabile
type Chiave = PublicKey
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

-- | aggiunge la parte anagrafica dello stato allo stato iniziale
bootAnagrafe :: [Responsabile] ->  a -> TyAnagrafe a
bootAnagrafe unos x = Anagrafe (map fst unos) .< Responsabili unos [] .< servizio0 .< x

-- | un utente con chiave pubblica
type Responsabile = (Utente,Chiave)

-- | la lista dei responsabili eletti e in elezione
data Responsabili = Responsabili {eletti::[Responsabile], inodore ::[(Indice,Responsabile)]} deriving (Show,Read)

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

-- controlla che la maggioranza sia raggiunta
maggioranza :: (Responsabili `ParteDi` s) => [a] -> MTInserzione s c Utente Bool
maggioranza ns = do 	
	Responsabili us _ <- osserva
	return $ length ns >= (length us + 1) `div` 2

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
		(l,reaz) <- programmazioneAssenso ("elezione dell'utente " ++ show u) r maggioranza (chiudi u)
		modifica $ \(Responsabili us ls) -> Responsabili us ((l,u):ls)
		return (True,([reaz],[])) 
		where
		chiudi u l = do
			modifica $ \(Responsabili us ls) -> Responsabili (u:us) (filter ((/=) l . fst) ls)
			logga $ "nuovo responsabile " ++ show u
			return nessunEffetto
	reattoreAnagrafe' (first validante -> (w,EliminazioneResponsabile u)) = w $ \r -> do
		Anagrafe us <- osserva
		esistenzaUtente u
		esistenzaResponsabile u
		Responsabili us ls <- osserva 
		fallimento (u `elem` map (fst . snd) ls) "responsabile gia' in eliminazione" 
		(l,reaz) <- programmazioneAssenso ("eliminazione del responsabile "++ show u) r maggioranza (chiudi u r)
		modifica $ \(Responsabili us ls) -> (Responsabili us ((l,(u,us ? (u,undefined))):ls))
		return (True,([reaz],[]))
		where
		chiudi u r l = do
			modifica $ \(Responsabili us ls ) -> Responsabili (elimina u us) (elimina l ls)
			logga $ "responsabile eliminato " ++ show u
			return ([],[EventoInterno $ EventoEliminazioneResponsabile u r])
------------------------------------------------------------------------------------------------
-- reparto costruzione ed interrogazione
------------------------------------------------------------------------------------------------
 
-- | estrae la lista di responsabili
responsabili :: (ParteDi Responsabili s) => Supporto s b [Responsabile]	
responsabili = do
	Responsabili rs _ <- asks see 
	when (null rs) $ throwError "nessun responsabile presente" 
	return rs

-- | estrae la lista di utenti
utenti :: (ParteDi Anagrafe s) => Supporto s b [Utente]	
utenti = do
	Anagrafe rs <- asks see
	when (null rs) $ throwError "nessun utente presente" 
	return rs

-- | costruzione degli eventi esterni per la gestione utenti e responsabili
costrEventiAnagrafe :: (ParteDi Anagrafe s, ParteDi Responsabili s) => CostrAction c EsternoAnagrafico s
costrEventiAnagrafe s kp kn =	[("inserimento nuovo utente", eventoNuovoUtente)
				,("elezione di un nuovo responsabile", eventoElezioneResponsabile)
				,("richiesta di eliminazione di un responsabile",eventoEliminazioneResponsabile)
				] 
	where
	run = runSupporto s kn kp
        eventoNuovoUtente =  run $ do
		us <- utenti 
                n <- libero "il nome del nuovo utente"
		when (n `elem` us) $ throwError "utente già presente"	
                return $ NuovoUtente n
        eventoElezioneResponsabile = run $ do
		us <- utenti 
		rs <- responsabili 
		let ds = us \\ map fst rs
		when (null ds) $ throwError "nessun utente non responsabile disponibile"
                n <- scelte (map (id &&& id) ds) "selezione eleggibile" 
                m <- dafile "il modulo della chiave pubblica"
                return $ ElezioneResponsabile (n,m)
        eventoEliminazioneResponsabile = run $ do
		rs <- responsabili 
                n <- scelte (map (fst &&& id) rs) "selezione responsabile da eliminare"
                return $ EliminazioneResponsabile (fst n)

-- | costruzione delle interrogazione sull'anagrafe e sui responsabili
costrQueryAnagrafe :: (ParteDi Anagrafe s, ParteDi Responsabili s) => CostrAction c Response s
costrQueryAnagrafe s kp kn = 	[("la chiave pubblica di un responsabile",queryChiave)
				,("elenco nomi utenti",queryElencoUtenti)
				,("elenco nomi responsabili",queryElencoResponsabili)
				] 
	where
	run = runSupporto s kn kp
	queryChiave = run $ do
		rs <- responsabili 
		(u,v) <- scelte (map (fst &&& id) rs) "selezione responsabile" 
		return $ Response [("responsabile",ResponseOne u),("chiave pubblica",ResponseOne v)]
	queryElencoUtenti = run $ do
		us <- utenti 
		return $ Response [("elenco nomi utenti", ResponseMany us)]
	queryElencoResponsabili = run $ do
		rs <- responsabili 
		return $ Response [("elenco nomi responsabili", ResponseMany rs )]
		
----------------------------------------------------------------------------------------------------------------------
--  sezione assensi, putroppo non ha un modulo a parte a causa del ciclo di dipendenze con l'anagrafe

-- | gli eventi che interessano una raccolta di assensi
data EsternoAssenso = Assenso Indice | EventoFallimentoAssenso Indice deriving (Read, Show)

-- | lo stato necessario per la gestione di un tipo di assensi
data Assensi = Assensi [Utente] deriving (Show,Read)

-- | funzione di programmazione per una nuova raccolta di assensi
programmazioneAssenso :: (
	Servizio Assensi `ParteDi` s
	, Responsabili `ParteDi` s
	, Anagrafe `ParteDi` s
	, Parser c EsternoAssenso
	)
	=> String	-- ^ nome della raccolta 
	-> Utente 	-- ^ l'utente che la richiede
	-> ([Utente] -> MTInserzione s c Utente Bool) -- ^ condizione di rolling 
	-> (Indice -> MTInserzione s c Utente (Effetti s c Utente)) -- ^ la chiusura per il successo della raccolta
	-> MTInserzione s c Utente (Int, Reazione s c Utente)	-- ^ la chiave per emettere assensi relativi e la reazione da schedulare

programmazioneAssenso se ur c k = do
	l <- nuovoStatoServizio (Assensi []) se -- ricevi la chiave per la nuova raccolta
	let 	eliminaRichiesta u j = do
			Assensi zs  <- osservaStatoServizio j 
			fallimento (ur /= u) "eliminazione di una richiesta di assenso effettuata da altro"
			eliminaStatoServizio j (undefined :: Assensi)  
			logga $ "chiusa la raccolta di assensi numero " ++ show l
			return (False,nessunEffetto) -- non rischedula il reattore

		reattoreAssenso (Right (first validante -> (w,Assenso j))) = w $ \r -> do
			when (j /= l) mzero
			Assensi zs <- osservaStatoServizio j 
			fallimento (r `elem` zs) "il responsabile ha gia' posto l'assenso sulla richiesta" 
			let zs' = r:zs -- la nuova lista di assensi per j 
			t <- c zs' -- controlla che si debba continuare a ricevere gli assensi
			if t then do
				eliminaStatoServizio j (undefined :: Assensi) 
				(,) False <$> k j -- esegui la procedura finale come coronamento del 
						-- consenso e non rischedula il reattore 
				else do		
					logga $ "ricevuto assenso da " ++ show r ++ " sulla raccolta numero " ++ show j
					modificaStatoServizio j $ \_ -> return (Assensi zs') -- registra gli assensi
					return (True,nessunEffetto) -- continua a ricevere

		reattoreAssenso (Right (first validante -> (w,EventoFallimentoAssenso j))) = w $ \r -> do
			when (j /= l) mzero
			eliminaRichiesta r j
		reattoreAssenso (Left (eliminazioneResponsabile -> Just (u,r))) = conFallimento $ do
			when (ur /= u) mzero
			eliminaRichiesta u l
		reattoreAssenso (Left _) = return Nothing
	logga $ "aperta la raccolta di assensi numero " ++ show l
 	return (l,Reazione (Nothing, reattoreAssenso)) -- restituisce il riferimento a questa richiesta perché venga nominato negli eventi di assenso

--------------------------- costruzioni per il modulo assensi -----------------------------

-- | estrae gli assensi dallo stato in lettura
assensi :: ParteDi (Servizio Assensi) s => Supporto s b [(String, Int)]
assensi = do
	xs :: [(Int,(String,Assensi))] <- asks elencoSottoStati 
	when (null xs) $ throwError "nessuna raccolta di assensi attiva"
	return  $ map (fst . snd &&& fst) xs

-- | costrutore degli eventi di assenso
costrEventiAssenso :: (Servizio Assensi `ParteDi` s) => CostrAction c EsternoAssenso s
costrEventiAssenso s kp kn = 	[("fallimento di una raccolta di assensi",eventoFallimentoAssenso)
				,("attribuzione di un assenso",eventoAssenso)
				] 
	where
	run = runSupporto s kn kp
        eventoFallimentoAssenso = run $ do
		ys <- assensi
                n <- scelte ys "selezione richiesta per fallire" 
              	return $ EventoFallimentoAssenso n
        eventoAssenso = run $ do
		ys <- assensi
                n <- scelte ys "selezione richiesta per assenso" 
                return $ Assenso n

-- | costruzione delle interrogazioni sul modulo di assensi
costrQueryAssenso :: (Servizio Assensi `ParteDi` s) => CostrAction c Response s
costrQueryAssenso s kp kn = [("elenco richieste di assenso aperte", querySottoStati)] 
	where
	querySottoStati = runSupporto s kn kp $ do
		ys <- assensi
		return $ Response [("elenco richieste di assenso aperte", if null ys then
			ResponseOne "nessuna richiesta aperta" else ResponseMany ys)]
priorityAssenso = R k where
	k (Assenso _) = -15
	k (EventoFallimentoAssenso _) = 15
---------------------------------------------------


