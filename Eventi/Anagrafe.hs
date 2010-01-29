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
import Codec.Crypto.RSA
import Core.Inserimento (logga, conFallimento, MTInserzione, osserva, modifica, fallimento)
import Core.Programmazione (Inserzione, EventoInterno (..), soloEsterna, nessunEffetto, Reazione (..))
import Lib.Aspetti ((.<),ParteDi,see)
import Lib.Assocs (assente,(?),updateM,elimina)
import Lib.Costruzione -- (libero,scelta,Costruzione,Svolgimento)
import Lib.Response
import Lib.Prioriti (R (..))
import Core.Types (Message)
import Core.Parsing
import Control.Monad.Cont

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
maggioranza ns = do 	
	Responsabili us _ <- osserva
	return $ length ns >= (length us + 1) `div` 2

-- | la reazione agli eventi anagrafici
reazioneAnagrafe :: (
	ParteDi (Servizio Assensi) s,
	ParteDi Anagrafe s,
	ParteDi Responsabili s,
	Parser c Assenso,
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
------------------------------------------------------------------------------------------------
responsabili k (see -> Responsabili rs _) = when (null rs) (k "nessun responsabile presente") >> return rs
utenti k (see -> Anagrafe us)  = when (null us) (k "nessun utente presente") >> return us

makeEventiAnagrafe s kp kn = [eventoNuovoUtente, eventoElezioneResponsabile,eventoEliminazioneResponsabile] 
	where
        eventoNuovoUtente = (,) "inserimento nuovo utente"  $ do
		us <- utenti kn s
                n <- libero "il nome del nuovo utente"
		when (n `elem` us) . kn $  "utente già presente"	
                kp  $ NuovoUtente n
        eventoElezioneResponsabile = (,) "elezione di un nuovo responsabile" $ do
		us <- utenti kn s
		rs <- responsabili kn s
		let disponibili = us \\ map fst rs
		when (null disponibili) . kn $  "nessun utente non responsabile disponibile"
                n <- scelte (map (id &&& id) $ disponibili) "selezione eleggibile" 
                m <- dafile "il modulo della chiave pubblica"
                kp  $ ElezioneResponsabile (n,m)
        eventoEliminazioneResponsabile = (,) "richiesta di eliminazione di un responsabile" $ do
		rs <- responsabili kn s
                n <- scelte (map (fst &&& id) $ rs) "selezione responsabile da eliminare"
                kp $ EliminazioneResponsabile (fst n)

queryAnagrafe s kp kn = [queryChiave,queryElencoUtenti,queryElencoResponsabili] where
	queryChiave = (,) "la chiave pubblica di un responsabile"  $ do
		rs <- responsabili kn s
		(u,v) <- scelte (map (fst &&& id) $ rs) "selezione responsabile" 
		kp $ Response [("responsabile",ResponseOne u),("chiave pubblica",ResponseOne v)]

	queryElencoUtenti = (,) "elenco nomi utenti" $ do
		us <- utenti kn s 
		kp . Response $ [("elenco nomi utenti", ResponseMany us)]
	queryElencoResponsabili = (,) "elenco nomi responsabili" $ do
		rs <- responsabili kn s 
		kp . Response $ [("elenco nomi responsabili", ResponseMany rs )]
		
----------------------------------------------------------------------------------------------------------------------
--  sezione assensi, putroppo non ha un modulo a parte a causa del ciclo di dipendenze con l'anagrafe

data Assenso = Assenso Indice | EventoFallimentoAssenso Indice deriving (Read, Show)

data Assensi = Assensi [Utente] deriving (Show,Read)
type StatoAssensi = Servizio Assensi

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

askAssensi :: ParteDi (Servizio Assensi) s => s -> [(String, Int)]
askAssensi s = let
	xs :: [(Int,(String,Assensi))] = elencoSottoStati $ s
	in map (fst . snd &&& fst) xs

makeEventiAssenso s kp kn = [eventoFallimentoAssenso , eventoAssenso] where
	ys = askAssensi s
        eventoFallimentoAssenso = (,) "fallimento di una raccolta di assensi" $ do
		when (null ys) $ kn "nessuna raccolta di assensi attiva"
                n <- scelte ys "selezione richiesta per fallire" 
              	kp $ EventoFallimentoAssenso n
        eventoAssenso = (,) "attribuzione di un assenso" $ do
		when (null ys) $ kn "nessuna raccolta di assensi attiva"
                n <- scelte ys "selezione richiesta per assenso" 
                kp $ Assenso n
queryAssenso
  :: (ParteDi (Servizio Assensi) s) => s
     -> (Response -> Cont (Passo b1) b)
     -> ([Char] -> Cont (Passo b1) ())
     -> [([Char], Cont (Passo b1) b)]

queryAssenso s kp kn = [querySottoStati] where
	ys = askAssensi s
	querySottoStati = (,) "elenco richieste di assenso aperte" $ do
		kp $ Response [("elenco richieste di assenso aperte", if null ys then
			ResponseOne "nessuna richiesta aperta" else ResponseMany ys)]
---------------------------------------------------


