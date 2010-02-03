{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, ViewPatterns, NoMonomorphismRestriction #-}

-- | modulo di implementazione della funzionalitá dell'impegno economico. La funzionalitá é generalizzata sull'obiettivo dell'impegno, infatti esponiamo l'interfaccia programmazioneImpegno. La gestione dello stato é fornita dal modulo di Servizio come in tutti i moduli higher order.
module Eventi.Impegno {-(
	Impegni,  -- necessario per l'istanziazione dell'aspetto del tipo dello stato
	statoInizialeImpegni, -- evitiamo di esportare il costruttore dell'aspetto
	programmazioneImpegno, -- l'interfaccia per programmare il modulo
	fallimentoImpegno,
	makeEventiImpegno,
	priorityImpegnoI,
	priorityImpegno,
	queryImpegni
--	unImpegno
	)-}  where

import Control.Monad.Maybe (MaybeT)
import Control.Monad (mzero, when)
import Control.Monad.Error (throwError)
import Control.Applicative ((<$>))
import Control.Monad.Reader (asks, MonadReader)
import Control.Arrow ((&&&), first)

import Lib.Aspetti ((.<), ParteDi)
import Lib.Costruzione (Costruzione)
import Lib.Prioriti (R(..))
import Lib.Assocs (update)
import Lib.Response (Response (..))

import Core.Costruzione (libero, scelte, runSupporto, CostrAction)
import Core.Parsing (Parser)
import Core.Programmazione (Effetti, Reazione (..) , EventoInterno (..),  nessunEffetto)
import Core.Inserimento (MTInserzione, conFallimento, fallimento, osserva, modifica, logga)

import Eventi.Servizio (Servizio, servizio0, nuovoStatoServizio, modificaStatoServizio, osservaStatoServizio, eliminaStatoServizio,elencoSottoStati)
import Eventi.Anagrafe (Utente, Responsabili, Anagrafe, eliminazioneResponsabile, validante ,utenti, esistenzaResponsabile)
import Eventi.Accredito (preleva, accredita, Conti)

type Indice = Int
-- | gli eventi  esterni per questo modulo
data EsternoImpegno 
	= Impegno Utente Float Indice	-- ^ indica un impegno di denaro da parte dell'utente per la causa chiave
	| FineImpegno Indice  		-- ^ indica la chiusura positiva della causa
	| FallimentoImpegno Indice	-- ^ indica la chiusura negativa della causa
	deriving (Show,Read)

priorityImpegno = R k where
	k (Impegno _ _ _) = -10
	k (FineImpegno _) = 15
	k (FallimentoImpegno _) = 15

priorityImpegnoI = R k where
	k (EventoFallimentoImpegno _) = 20

-- | evento interno che segnala il fallimento di una causa
data Interni = EventoFallimentoImpegno (Utente, Float) deriving (Show,Read)

-- | intercettore per gli eventi interni
fallimentoImpegno (EventoFallimentoImpegno t) = Just t
fallimentoImpegno _ = Nothing

-- | lo stato per ogni causa
data Impegni = Impegni [(Utente,Float)] deriving (Show,Read)

-- | tipo dello stato aggiunto degli impegni
type TyImpegni a = (Servizio Impegni , a)

-- | aggiunta dell'aspetto Servizio Impegni per il boot
bootImpegni :: a -> TyImpegni a
bootImpegni x = (servizio0,x)

-- unImpegno s n = (\(Impegni us) -> us) <$> snd <$> seeStatoServizio  (undefined :: Impegni) s n

-- | il tipo della funzione da passare alla hof restituita da programmazioneImpegno 
-- type ConclusioneReattoreImpegno s c = Maybe ([(Utente, Float)]) -> MTInserzione s c Utente (Effetti s c Utente)

-- | la programmazione di un impegni richiede il nome del responsabile che la apre e restituisce la chiave del nuovo stato impegni con una una azione monadica in grado di creare una nuova Reazione se fornita della giusta procedura. La giusta procedura definisce cosa eseguire alla fine della raccolta impegni. In caso di successo l'azione riceve la lista di impegni raccolta , in caso di fallimento Nothing. Comunque essa deve fornire una lista di nuovi reattori e una lista di eventi interni.

type ChiudiProgrammazioneImpegno s c = (Maybe ([(Utente, Float)]) -> MTInserzione s c Utente (Effetti s c Utente))
programmazioneImpegno :: (
	Parser c EsternoImpegno,  -- okkio se serve un parser va implementato
	Anagrafe 		`ParteDi` s,  -- eventoValidato lo richiede
	Conti 			`ParteDi` s,  -- preleva e accredita lo richiedono
	Responsabili 		`ParteDi` s,  -- eliminazioneResponsabile lo richiede
	Servizio Impegni 	`ParteDi` s)  -- il nostro aspetto
	=> String
	-> Utente  -- ^ l'utente responsabile dell'impegno
	-> MTInserzione s c Utente (Int ,ChiudiProgrammazioneImpegno s c -> Reazione s c Utente) -- ^ la chiave e una Reazione

programmazioneImpegno q ur = do
	l <- nuovoStatoServizio (Impegni []) q
	let 	
		reattoreImpegno k (Right (first validante ->  (w,Impegno u v j))) = w $ \r -> do
			when (l /= j) mzero
			preleva u v 
			modificaStatoServizio j $ \(Impegni  is) -> return (Impegni $ update u (+ v) 0 is)
			logga  $ "impegnati " ++ show v ++ " euro da " ++ show u ++ " per la causa " ++ q
			return (True,nessunEffetto)
		reattoreImpegno k (Right (first validante -> (w,FineImpegno j))) = w $ \r -> do
			when (l /= j) mzero
			Impegni us <- osservaStatoServizio j
			fallimento (ur /= r) "solo chi ha aperto una impegnativa puó chiuderla"
			eliminaStatoServizio j (undefined :: Impegni)
			(,) False <$> k (Just us) 
		reattoreImpegno k (Right (first validante -> (w,FallimentoImpegno j))) = w $ \r -> do
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
			
	return (l,\k -> Reazione (Nothing,reattoreImpegno k) )
-----------------------------------------------------------------------------------
----------------------------------------------------------------------------------
-- askImpegni :: (ParteDi (Servizio Impegni) r, MonadReader r m) =>
--      (String -> m ()) -> m [(String, Int)]
impegni 	= do 	(xs :: [(Int,(String,Impegni))]) <- asks elencoSottoStati
			when (null xs) $ throwError "nessuna raccolta di impegni attiva"
			return $ map (fst . snd &&& fst) xs
costrEventiImpegno :: (
	ParteDi (Servizio Impegni) s, 
	ParteDi Anagrafe s
	) =>
	CostrAction c EsternoImpegno s

costrEventiImpegno s kp kn = 	[("fine di una raccolta impegni", eventoFineImpegno)
				,("fallimento di una raccolta impegni", eventoFallimentoImpegno) 
				,("impegno di una somma", eventoImpegno)
				] 
	where
	run = runSupporto s kn kp
        eventoFineImpegno = run $ do
		is <- impegni 
                n <- scelte is "selezione raccolta impegni da chiudere" 
                return $ FineImpegno n
        eventoFallimentoImpegno = run $ do
		is <- impegni 
                n <- scelte is  "selezione raccolta da far fallire" 
                return $ FallimentoImpegno n
        eventoImpegno  = run $ do
		is <- impegni 
                n <- scelte is  "selezione raccolta impegni"  
		us <- asks utenti 
		u <- scelte (map (id &&& id) us) "selezione utente impegnante"
		z <- libero "somma impegnata"
                return $ Impegno u z n

costrQueryImpegni :: (ParteDi (Servizio Impegni) s) => CostrAction c Response s
costrQueryImpegni s kp kn = 	[("elenco impegni",r)
				,("raccolte impegni",q)
				] 
	where
	run = runSupporto s kn kp
	r =  run $ do
		is <- impegni  
		return $ Response [("elenco impegni",ResponseMany (map fst is))]
	q = run $ do
		is <- impegni
		n <- scelte is "selezione raccolta impegni" 
		isx <- asks elencoSottoStati
		case lookup n isx of 
			Nothing -> throwError "incoerenza interna" 
			Just (t,Impegni is) -> return $ 
				Response [("obiettivo raccolta",ResponseOne t),("somme impegnate", ResponseAL is)]

