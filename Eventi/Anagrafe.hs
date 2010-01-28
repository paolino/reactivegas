{-# LANGUAGE NoMonomorphismRestriction, StandaloneDeriving, ViewPatterns, FlexibleContexts, ScopedTypeVariables, ExistentialQuantification #-}
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
import Control.Arrow ((***), (&&&))
import Control.Monad (when, mzero)
import Control.Monad.Reader (asks, MonadReader)
import Codec.Crypto.RSA
import Core.Inserimento (logga, conFallimento, MTInserzione, osserva, modifica, fallimento)
import Core.Programmazione (Inserzione, EventoInterno (..), soloEsterna, nessunEffetto, Reazione (..))
import Lib.Aspetti ((.<),ParteDi,see)
import Lib.Assocs (assente,(?),updateM,elimina)
import Lib.Costruzione -- (libero,scelta,Costruzione,Svolgimento)
import Lib.Prioriti (R (..))
import Core.Types (Message)


import Eventi.Servizio

deriving instance Read PublicKey
deriving instance Eq PublicKey

type Chiave= PublicKey
type Indice = Int
type Utente = String

eventoValidato  (r,a) = (\f -> conFallimento (esistenzaResponsabile r >> f r), a)

data Anagrafe = Anagrafe [Utente] deriving (Show,Read)
statoInizialeAnagrafe unos x = Anagrafe (map fst unos) .< Responsabili unos [] .< x

type Responsabile = (Utente,Chiave)

data Responsabili = Responsabili {eletti::[Responsabile], inodore ::[(Indice,Responsabile)]} deriving (Show,Read)
priorityAnagrafe = R k where
	k (NuovoUtente _) = -20
	k (EliminazioneResponsabile _) = -19
	k (ElezioneResponsabile _) = -18

priorityAnagrafeI = R k where
	k (EventoEliminazioneResponsabile _ _) = 20


data Evento = NuovoUtente String | ElezioneResponsabile Responsabile | EliminazioneResponsabile String deriving (Read,Show)
data EventiInterni = EventoEliminazioneResponsabile Utente Utente deriving (Show, Read)

eliminazioneResponsabile (EventoEliminazioneResponsabile w r) = Just (w,r)
eliminazioneResponsabile _ = Nothing

esistenzaUtente u  = osserva >>= \(Anagrafe us) -> fallimento (u `notElem` us) $ "utente "++ show u ++ " sconosciuto" 
esistenzaResponsabile u = esistenzaUtente u >> osserva >>= \(Responsabili us _) -> fallimento (u `assente` us) $ "responsabile " ++ show u ++ " non eletto"

-- controlla che la maggioranza sia raggiunta, riceve i responsabili che hanno dato l'assenso
test ns = do 	
	Responsabili us _ <- osserva
	return (length ns > length us `div` 2)

reazioneAnagrafe = soloEsterna reattoreAnagrafe' where
	reattoreAnagrafe' (eventoValidato -> (w,NuovoUtente u)) = w $ \r -> do
		Anagrafe us <- osserva
		fallimento (u `elem` us) "utente gia' presente" 
		logga $ "accettato nuovo utente " ++ show u
		modifica . const $ Anagrafe (u:us)
		return (True, nessunEffetto)
	reattoreAnagrafe' (eventoValidato -> (w,ElezioneResponsabile u)) = w $ \(r :: Utente) -> do
		Anagrafe us <- osserva
		esistenzaUtente (fst u)
		Responsabili us ls <- osserva 
		fallimento (u `elem` us) "utente gia' eletto" 
		fallimento (u `elem` map snd ls) "utente gia' in elezione" 
		(l,reaz) <- programmazioneAssenso ("elezione dell'utente "++ show u) r test (chiudi u)
		modifica $ \(Responsabili us ls) -> (Responsabili us ((l,u):ls))
		return (True,([reaz],[])) 
		where
		chiudi u l = do
			modifica $ \(Responsabili us ls ) -> Responsabili (u:us) (filter ((/=) l . fst) ls)
			logga $ "nuovo responsabile " ++ show u
			return nessunEffetto
	reattoreAnagrafe' (eventoValidato -> (w,EliminazioneResponsabile u)) = w $ \r -> do
		Anagrafe us <- osserva
		esistenzaUtente u
		esistenzaResponsabile u
		Responsabili us ls <- osserva 
		fallimento (u `elem` map (fst . snd) ls) "responsabile gia' in eliminazione" 
		(l,reaz) <- programmazioneAssenso ("eliminazione del responsabile "++ show u) r test (chiudi u r)
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
                m <- libero "il modulo della chiave pubblica"
                kp  $ ElezioneResponsabile (n,m)
        eventoEliminazioneResponsabile = (,) "richiesta di eliminazione di un responsabile" $ do
		rs <- responsabili kn s
                n <- scelte (map (fst &&& id) $ rs) "selezione responsabile da eliminare"
                kp $ EliminazioneResponsabile (fst n)

{-
queryAnagrafe = [queryChiave,queryElencoUtenti,queryElencoResponsabili] where
	queryChiave k = (,) "la chiave pubblica di un responsabile"  $ do
		rs <- responsabili k
		(u,v) <- parametro (Scelta "selezione responsabile" . map (fst &&& id) $ rs)		
		return $ Response [("responsabile",ResponseOne u),("chiave pubblica",ResponseOne v)]

	queryElencoUtenti k = (,) "elenco nomi utenti" $ do
		us <- utenti k 
		return . Response $ [("elenco nomi utenti", ResponseMany us)]
	queryElencoResponsabili k = (,) "elenco nomi responsabili" $ do
		rs <- responsabili k 
		return . Response $ [("elenco nomi responsabili", ResponseMany rs )]
		
-}
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
			return (False,nessunEffetto) -- non rischedula il reattore

		reattoreAssenso (Right (r,Assenso j)) = conFallimento $ do
			esistenzaResponsabile r
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

		reattoreAssenso (Right (r,EventoFallimentoAssenso j)) = conFallimento $ do
			esistenzaResponsabile r
			when (j /= l) mzero
			eliminaRichiesta r j
		reattoreAssenso (Left (eliminazioneResponsabile -> Just (u,r))) = conFallimento $ do
			when (ur /= u) mzero
			eliminaRichiesta u l
		reattoreAssenso (Left _) = return Nothing
	logga $ "aperta la raccolta di assensi numero " ++ show l
	return (l,Reazione (Nothing, reattoreAssenso)) -- restituisce il riferimento a questa richiesta perché venga nominato negli eventi di assenso
{-
askAssensi :: (ParteDi (Servizio Assensi) s, MonadReader s m) => m [(String, Int)]
askAssensi 	= do 	(xs :: [(Int,(String,Assensi))]) <- asks elencoSottoStati 
			return $ map (fst . snd &&& fst) xs
makeEventiAssenso
	:: (ParteDi (Servizio Assensi) s, MonadReader s m) =>
	[(String -> Svolgimento b m ()) -> (String , Svolgimento b m String)]

makeEventiAssenso = [eventoFallimentoAssenso , eventoAssenso] where
        eventoFallimentoAssenso k = (,) "fallimento di una raccolta di assensi" $ do
		ys <- askAssensi 
		when (null ys) $ k "nessuna raccolta di assensi attiva"
                n <- parametro . Scelta "selezione richiesta per fallire" $ ys
              	return $ show (EventoFallimentoAssenso n)
        eventoAssenso k = (,) "attribuzione di un assenso" $ do
		ys <- askAssensi 
		when (null ys) $ k "nessuna raccolta di assensi attiva"
                n <- parametro . Scelta "selezione richiesta per assenso" $ ys
                return $ show (Assenso n)
queryAssenso
  :: (ParteDi (Servizio Assensi) s, MonadReader s m) =>
	[(String -> Svolgimento b m ()) -> (String , Svolgimento b m Response)]

queryAssenso = [querySottoStati] where
	querySottoStati _ = (,) "elenco richieste di assenso aperte" $ do
		ys <- askAssensi 
		return $ Response [("elenco richieste di assenso aperte", if null ys then
			ResponseOne "nessuna richiesta aperta" else ResponseMany ys)]
-}
---------------------------------------------------


