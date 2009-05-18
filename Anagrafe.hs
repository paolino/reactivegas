{-# LANGUAGE NoMonomorphismRestriction, ViewPatterns, ScopedTypeVariables, ExistentialQuantification #-}
module Anagrafe (eliminazioneResponsabile, esistenzaUtente , Anagrafe, Responsabili, statoInizialeAnagrafe, esistenzaResponsabile ,
	reazioneAnagrafe, programmazioneAssenso , Chiave, Assensi, eventoValidato, Utente, makeEventiAssenso, makeEventiAnagrafe,
	responsabili , utenti, priorityAnagrafeI, priorityAnagrafe, Evento (..)) where

import Control.Monad.RWS
import Control.Applicative
import Data.List
import Data.Maybe
import Control.Arrow
import Aspetti ((.<),see)
import Core
import Servizio
import Costruzione
import Prioriti
import Lib0
import Lib1

type Utente = String

eventoValidato  (r,a) = (\f -> conFallimento (esistenzaResponsabile r >> f r), a)

data Anagrafe = Anagrafe [Utente] deriving (Show,Read)
utenti = (\(Anagrafe us) -> us) . see

statoInizialeAnagrafe uno x = Anagrafe [fst uno] .< Responsabili [uno] [] .< x

type Responsabile = (Utente,Chiave)

data Responsabili = Responsabili {eletti::[Responsabile], inodore ::[(Indice,Responsabile)]} deriving (Show,Read)
responsabili = eletti . see

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
makeEventiAnagrafe k = [
	(,) "evento di nuovo utente" eventoNuovoUtente,
	(,) "evento di elezione responsabile" eventoElezioneResponsabile,
	(,) "evento di eliminazione responsabile" eventoEliminazioneResponsabile
	] where
        eventoNuovoUtente = do
                s <- ask
                n <- parametro (Libero "il nuovo nome")
                return $ show (NuovoUtente n)

        eventoElezioneResponsabile = do
                s <- ask 
		let disponibili = utenti s \\ map fst (responsabili s)
		when (null disponibili) $ k "nessun utente disponibile"
                n <- parametro . Scelta "selezione eleggibile" $ (map (show &&& id) $ disponibili)
                m <- parametro (Libero "il modulo della chiave pubblica")
                return $ show (ElezioneResponsabile (n,m))

        eventoEliminazioneResponsabile = do
                s <- ask
		when (null $ responsabili s) $ k "nessun utente disponibile"
                n <- parametro . Scelta "selezione responsabile" . map (show &&& id) $ responsabili s
                return $ show (EliminazioneResponsabile (fst n))



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

makeEventiAssenso k = [
	(,) "evento di fallimento raccolta assenso" eventoFallimentoAssenso, 
	(,) "evento di assenso" eventoAssenso
	] where
        eventoFallimentoAssenso = do
                s <- ask
		when (null $ elencoSottoStati (undefined :: Assensi) s) $ k "nessuna raccolta di assensi attiva"
                n <- parametro . Scelta "selezione richiesta per fallire" $ (map (snd &&& fst) $ elencoSottoStati (undefined :: Assensi) s)
                return $ show (EventoFallimentoAssenso n)
        eventoAssenso = do
                s <- ask
		when (null $ elencoSottoStati (undefined :: Assensi) s) $ k "nessuna raccolta di assensi attiva"
                n <- parametro . Scelta "selezione richiesta per assenso"  $ (map (snd &&& fst) $ elencoSottoStati (undefined :: Assensi) s)
                return $ show (Assenso n)

---------------------------------------------------


