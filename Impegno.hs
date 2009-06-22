{-# LANGUAGE FlexibleContexts, ViewPatterns, NoMonomorphismRestriction #-}

-- modulo di implementazione della funzionalitá dell'impegno economico. La funzionalitá é generalizzata sull'obbiettivo dell'impegno, infatti
-- esponiamo l'interfaccia programmazioneImpegno. La gestione dello stato é fornita dal modulo di Servizio come in tutti i moduli higher order.
module Impegno (
	Impegni,  -- necessario per l'istanziazione dell'aspetto del tipo dello stato
	statoInizialeImpegni, -- evitiamo di esportare il costruttore dell'aspetto
	programmazioneImpegno, -- l'interfaccia per programmare il modulo
	fallimentoImpegno,
	makeEventiImpegno,
	priorityImpegnoI,
	priorityImpegno,
	unImpegno
	)  where

import Control.Monad.Maybe (MaybeT)
import Control.Monad (mzero, when)
import Control.Applicative ((<$>))
import Control.Monad.Reader (ask)
import Control.Arrow ((&&&))

import Aspetti ((.<), ParteDi)
import Lib1
import Lib0 
import Core (Reazione (Reazione), Inserzione, EventoInterno (..), Parser, nessunEffetto, Effetti)
import Costruzione 
import Prioriti

---- moduli necessari
import Servizio (Servizio, statoInizialeServizio, nuovoStatoServizio, modificaStatoServizio, osservaStatoServizio, eliminaStatoServizio,elencoSottoStati, seeStatoServizio)
import Anagrafe (Utente, Responsabili, Anagrafe, eliminazioneResponsabile, eventoValidato,utenti, esistenzaResponsabile)
import Accredito (preleva, accredita, Conti)

-- gli eventi , solo esterni, che il reattoreImpegno intercetta
data Eventi 
	= Impegno Utente Float Indice  -- indica un impegno di denaro da parte dell'utente per la causa chiave
	| FineImpegno Indice  -- indica la chiusura positiva della causa, viene decurtato la somma impegnati dagli utenti dal saldo del responsabile
	| FallimentoImpegno Indice  -- indica la chiusura negativa della causa, gli impegni in denaro vengono restituiti
	deriving (Show,Read)
priorityImpegno = R k where
	k (Impegno _ _ _) = -10
	k (FineImpegno _) = 15
	k (FallimentoImpegno _) = 15

priorityImpegnoI = R k where
	k (EventoFallimentoImpegno _) = 20

data Interni = EventoFallimentoImpegno (Utente, Float) deriving (Show,Read)
fallimentoImpegno (EventoFallimentoImpegno t) = Just t
fallimentoImpegno _ = Nothing

-- | lo stato per ogni causa
data Impegni = Impegni [(Utente,Float)] deriving (Show,Read)

-- | aggiunta dell'aspetto Servizio Impegni 
statoInizialeImpegni  :: a -> (Servizio Impegni, a)
statoInizialeImpegni = statoInizialeServizio 

unImpegno s n = (\(Impegni us) -> us) <$> snd <$> seeStatoServizio  (undefined :: Impegni) s n

-- | il tipo della funzione da passare alla hof restituita da programmazioneImpegno 
type ConclusioneReattoreImpegno s c = Maybe ([(Utente, Float)]) -> MTInserzione s c Utente (Effetti s c Utente)

-- | la programmazione di una ca(,) False <$> usa impegni richiede il nome del responsabile che la apre e restituisce la chiave del nuovo stato impegni
-- con una una azione monadica in grado di creare una nuova Reazione se fornita della giusta procedura. La giusta procedura definisce cosa 
-- eseguire alla fine della raccolta impegni. In caso di successo l'azione riceve la lista di impegni raccolta , in caso di fallimento Nothing.
-- Comunque essa deve fornire una lista di nuovi reattori e una lista di eventi interni.
programmazioneImpegno :: (
	Parser c Eventi,  -- okkio se serve un parser va implementato
	Anagrafe 		`ParteDi` s,  -- eventoValidato lo richiede
	Conti 			`ParteDi` s,  -- preleva e accredita lo richiedono
	Responsabili 		`ParteDi` s,  -- eliminazioneResponsabile lo richiede
	Servizio Impegni 	`ParteDi` s)  -- il nostro aspetto
	=> String
	-> Utente  -- ^ l'utente responsabile dell'impegno
	-> MTInserzione s c Utente (Int , ConclusioneReattoreImpegno s c -> Reazione s c Utente) -- ^ la chiave e una hof in grado di produrre una Reazione

programmazioneImpegno q ur  = do
	l <- nuovoStatoServizio (Impegni []) q
	let 	
		reattoreImpegno _ (Right (r,Impegno u v j)) = conFallimento $ do
			esistenzaResponsabile r
			when (l /= j) mzero
			preleva u v 
			modificaStatoServizio j $ \(Impegni  is) -> return (Impegni $ update u (+ v) 0 is)
			logga  $ "impegnati " ++ show v ++ " euro da " ++ show u ++ " per la causa " ++ show j
			return (True,nessunEffetto)
		reattoreImpegno k (Right (r,FineImpegno j)) = conFallimento $  do
			esistenzaResponsabile r
			when (l /= j) mzero
			Impegni us <- osservaStatoServizio j
			fallimento (ur /= r) "solo chi ha aperto una impegnativa puó chiuderla"
			eliminaStatoServizio j (undefined :: Impegni)
			(,) False <$> k (Just us) 
		reattoreImpegno k (Right (r,FallimentoImpegno j)) = conFallimento $ do
			esistenzaResponsabile r
			when (l /= j) mzero
			Impegni us <- osservaStatoServizio j
			fallimento (ur /= r) "solo chi ha aperto una impegnativa puó chiuderla"
			mapM_ (\(u,v) -> accredita u v) us
			eliminaStatoServizio j (undefined :: Impegni)
			(ks,is) <- k Nothing 
			return (False,(ks,EventoInterno (EventoFallimentoImpegno (ur,sum (map snd us))): is))
		reattoreImpegno k (Left (eliminazioneResponsabile -> Just (u,_))) = conFallimento $ do
			when (ur /= u) mzero
			Impegni  us <- osservaStatoServizio l
			mapM_ (\(u,v) -> accredita u v) us
			eliminaStatoServizio l (undefined :: Impegni)
			(,) False <$> k Nothing 
		reattoreImpegno _ (Left _) = return Nothing
			
	return (l, \t -> Reazione (Nothing,reattoreImpegno t) )

makeEventiImpegno = [ eventoFineImpegno, eventoFallimentoImpegno ,  eventoImpegno] where
        eventoFineImpegno k = (,) "fine di una raccolta impegni" $ \s -> do
		let e = elencoSottoStati (undefined :: Impegni) s
		when (null e) $ k "nessuna raccolta di impegni attiva"
                n <- parametro . Scelta "selezione raccolta impegni da chiudere" $ (map (snd &&& fst) e)
                return $ show (FineImpegno n)
        eventoFallimentoImpegno k = (,) "fallimento di una raccolta impegni" $ \s -> do
		let e = elencoSottoStati (undefined :: Impegni) s
		when (null e) $ k "nessuna raccolta di impegni attiva"
                n <- parametro . Scelta "selezione raccolta da far fallire" $ (map (snd &&& fst) e)
                return $ show (FallimentoImpegno n)
        eventoImpegno k = (,) "impegno di una somma" $ \s -> do
		let e = elencoSottoStati (undefined :: Impegni) s
		when (null e) $ k "nessuna raccolta di impegni attiva"
                n <- parametro . Scelta "selezione raccolta impegni"  $ (map (snd &&& fst) e)
		let us = utenti s
		when (null us) $ k "nessun utente disponibile"
		u <- parametro . Scelta "selezione utente impegnante" $ (map (id &&& id) us)
		z <- parametro $ Libero "somma impegnata"
                return $ show (Impegno u z n)

