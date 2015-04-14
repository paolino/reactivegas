{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, ViewPatterns, NoMonomorphismRestriction #-}

-- | modulo di implementazione della funzionalitá dell'ordine economico. La funzionalitá é generalizzata sull'obiettivo dell'ordine, infatti esponiamo l'interfaccia programmazioneOrdine. La gestione dello stato é fornita dal modulo di Servizio come in tutti i moduli higher order.
module Eventi.Ordine {-(
	Ordini,  -- necessario per l'istanziazione dell'aspetto del tipo dello stato
	statoInizialeOrdini, -- evitiamo di esportare il costruttore dell'aspetto
	programmazioneOrdine, -- l'interfaccia per programmare il modulo
	fallimentoOrdine,
	makeEventiOrdine,
	priorityOrdineI,
	priorityOrdine,
	queryOrdini
--	unOrdine
	)-}  where
import Data.List (delete)
import Data.Maybe (fromJust)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad (mzero, when)
import Control.Monad.Error (throwError)
import Control.Applicative ((<$>))
import Control.Monad.Reader (asks, MonadReader)
import Control.Arrow ((&&&), (***), first)
import Data.Time (LocalTime)

import Lib.Aspetti ((.<), ParteDi, see)
import Lib.Prioriti (R(..))
import Lib.Assocs (update)
import Lib.Response (Response (..))
import Lib.QInteger (QInteger)

import Core.Types (Utente)
import Core.Costruzione (libero, scelte, runSupporto, CostrAction)
import Core.Parsing (Parser)
import Core.Programmazione (Effetti, Reazione (..) , EventoInterno (..),  nessunEffetto)
import Core.Inserimento (MTInserzione, conFallimento, fallimento, osserva, modifica, loggamus)

import Eventi.Servizio (Servizio, servizio0, nuovoStatoServizio, modificaStatoServizio, osservaStatoServizio, eliminaStatoServizio,elencoSottoStati)
import Eventi.Anagrafe (EsternoAssenso, Assensi, programmazionePermesso, Responsabili, Anagrafe, eliminazioneResponsabile, validante ,utenti, esistenzaResponsabile, SUtente (..))
import Eventi.Accredito (salda, accredita, Conti, Saldi)
import Lib.Voci.Core
import Lib.Voci.Instances

import Debug.Trace

import Lib.ShowRead
import Lib.Euro

type Indice = QInteger
-- | gli eventi  esterni per questo modulo
data EsternoOrdine 
	= Ordine Utente Euro Indice	-- ^ indica un ordine di denaro da parte dell'utente per la causa chiave
	| FineOrdine Indice  		-- ^ indica la chiusura positiva della causa
	| FallimentoOrdine Indice	-- ^ indica la chiusura negativa della causa
	| CorrezioneOrdine Utente Euro Indice

instance Show EsternoOrdine where
	show (Ordine u f i) = "ordine da " ++ quote u ++ " di " ++ show f ++ " in riferimento a " ++ show i
	show (FineOrdine i) = "chiusura della raccolta ordini riferita a " ++ show i
	show (FallimentoOrdine i) = "fallimento della raccolta ordini riferita a " ++ show i
	show (CorrezioneOrdine u f i) = "correzione ordine da " ++ quote u ++ " di " ++ show f ++ 
		" in riferimento a " ++ show i

instance Read EsternoOrdine where
	readPrec = let
		imp = do
			string "ordine da "
			u <- phrase 
			string " di " 
			f <- reads'
			string " in riferimento a "
			i <- reads'
			return $ Ordine u f i
		fin = do 
			string "chiusura della raccolta ordini riferita a "
			i <- reads'
			return $ FineOrdine i
		fal = do 
			string "fallimento della raccolta ordini riferita a "
			i <- reads'
			return $ FallimentoOrdine i
		cor = do 
			string "correzione ordine da "
			u <- phrase 
			string " di " 
			f <- reads'
			string " in riferimento a "
			i <- reads'
			return $ CorrezioneOrdine u f i
		in lift $ imp <++ fin <++ fal <++ cor

priorityOrdine = R k where
	k (Ordine _ _ _) = -27
	k (FineOrdine _) = 15
	k (FallimentoOrdine _) = 16
	k (CorrezioneOrdine _ _ _) = -20

priorityOrdineI = R k where
	k (EventoFallimentoOrdine _) = 20

-- | evento interno che segnala il fallimento di una causa
data Interni = ForzaFallimento | EventoFallimentoOrdine (Utente, Euro) deriving (Show,Read)

-- | intercettore per gli eventi interni
fallimentoOrdine (EventoFallimentoOrdine t) = Just t
fallimentoOrdine _ = Nothing

-- | lo stato per ogni causa
data Ordini = Ordini {
	permesso :: Bool, 
	referente :: Utente, 
	accettati :: [(Utente,[Ordine])], 
	inattesa :: [(Utente,[Ordine])]
	} deriving (Show,Read,Eq)

-- | tipo dello stato aggiunto degli ordini
type TyOrdini a = (Servizio Ordini , a)

-- | aggiunta dell'aspetto Servizio Ordini per il boot
bootOrdini :: a -> TyOrdini a
bootOrdini x = (servizio0,x)

raccolte :: (Servizio Ordini `ParteDi` s) => s -> [String]
raccolte s = let xs :: [(Indice,(String,Ordini))] = elencoSottoStati s  in map (fst . snd) xs

-- unOrdine s n = (\(Ordini us) -> us) <$> snd <$> seeStatoServizio  (undefined :: Ordini) s n

-- | il tipo della funzione da passare alla hof restituita da programmazioneOrdine 
-- type ConclusioneReattoreOrdine s c = Maybe ([(Utente, Euro)]) -> MTInserzione s c Utente (Effetti s c Utente)

-- | la programmazione di un ordini richiede il nome del responsabile che la apre e restituisce la chiave del nuovo stato ordini con una una azione monadica in grado di creare una nuova Reazione se fornita della giusta procedura. La giusta procedura definisce cosa eseguire alla fine della raccolta ordini. In caso di successo l'azione riceve la lista di ordini raccolta , in caso di fallimento Nothing. Comunque essa deve fornire una lista di nuovi reattori e una lista di eventi interni.
programmazioneOrdine' :: (
	Parser c EsternoOrdine,  -- okkio se serve un parser va implementato
	Parser c EsternoAssenso,
	Anagrafe 		`ParteDi` s,  -- eventoValidato lo richiede
	Conti 			`ParteDi` s,  -- preleva e accredita lo richiedono
	Saldi 			`ParteDi` s,  -- preleva e accredita lo richiedono
	Responsabili 		`ParteDi` s,  -- eliminazioneResponsabile lo richiede
	Servizio Assensi 		`ParteDi` s,
	Integer `ParteDi` s,
	Servizio Ordini 	`ParteDi` s)  -- il nostro aspetto
	=> String	-- ^ motivazione della raccolta
	-> Utente 	-- ^ l'utente responsabile dell'ordine
	-> (Maybe ([(Utente, Euro)]) -> MTInserzione s c Utente (Effetti s c Utente))
	-> MTInserzione s c Utente 
		( Indice
		, MTInserzione s c Utente (Effetti s c Utente)
		, MTInserzione s c Utente () -> Reazione s c Utente
		, MTInserzione s c Utente ()
		)

programmazioneOrdine' q' ur k  = do
	l <- nuovoStatoServizio (Ordini False ur [] []) (ur,q')
	let q = " per l'acquisto di " ++ q'
	let 	effettoF j = do 
			Ordini _ ur as is <- osservaStatoServizio j
			mapM_ (\(u,v) -> accredita u (mkDEuro v) $ "restituzione per fallimento " ++ q) (as ++ is) -- restituzione del denaro di tutti gli ordini
			eliminaStatoServizio j (undefined :: Ordini)
			(ks,es) <- k Nothing 
			return (ks,EventoInterno (EventoFallimentoOrdine (ur,sum (map snd (as ++ is)))): es)
		reattoreOrdine _ (Right (first validante -> (w,i@(CorrezioneOrdine u v j)))) = w $ \r -> do
				when (l /= j) mzero
				fallimento (ur /= r)  $ "solo " ++ ur ++ " può correggere gli ordini per " ++ q
				Ordini _ _ as is <- osservaStatoServizio j 
				fallimento (not $ u `elem` map fst as) "nessun ordine tra gli accettati per l'utente"
				let vp = fromJust (lookup u as) -- denaro impegnato
				accredita u (mkDEuro $ vp - v) $ "correzione ordine " ++ q 
				modificaStatoServizio j $ \(Ordini ch ur as is) -> return $
					Ordini ch ur ((u,v):(filter ((/=) u . fst) as)) is
				loggamus  $ "correzione d'ordine per  " ++ show (mkDEuro $ v - vp) ++ " da " ++ u  ++ q
				return (True, nessunEffetto)  
		reattoreOrdine _ (Right (first validante ->  (w,i@(Ordine u v j)))) = w $ \r -> let
			positivo _ = do
				loggamus $ "accettato l'ordine di " ++ show v ++ " da " ++ u ++ q 
				modificaStatoServizio j $ \(Ordini ch ur as is) -> return 
					(Ordini ch ur ((u,v):as) (delete (u,v) is))
				return nessunEffetto
			negativo _ = do 
				accredita u (mkDEuro v) $ "restituzione per rifiuto richiesta " ++ q
				modificaStatoServizio j $ \(Ordini ch ur as is) -> return 
					(Ordini ch ur as (delete (u,v) is))
				loggamus $ "rifiutato l'ordine di " ++ show v ++ " da " ++ u  ++ q
				return nessunEffetto
			in do 
				when (l /= j) mzero
				Ordini _ _ as is <- osservaStatoServizio j 
				fallimento (u `elem` map fst (as ++ is)) "ordine già richiesto o accettato per l'utente"
				accredita u (mkDEuro $ negate v) $ "richiesta di ordine " ++ q 
				modificaStatoServizio j $ \(Ordini ch ur as is) -> return (Ordini ch ur as $ (u,v):
					filter ((/=) u . fst) is)
				loggamus  $ "richiesta di ordine di  " ++ show v ++ " da " ++ u  ++ q
				(_,reaz) <- programmazionePermesso 
					("ordine di " ++ show v ++ " da " ++ u ++ q)
					r ur positivo negativo
				return (True, ([reaz],[]))   
		reattoreOrdine _ (Right (first validante -> (w,FineOrdine j))) = w $ \r -> do
			when (l /= j) mzero
			Ordini y ur as is <- osservaStatoServizio j
			fallimento (ur /= r) $ "solo " ++ ur ++ " può chiudere la raccolta di ordini " ++ q
			fallimento (not y) $ "la chiusura non è stata concessa  " ++ q
			fallimento (not . null $ is) "richieste di ordine ancora in attesa di conferma"
			salda r (mkDEuro . negate $ (sum $ map snd as)) $ "spesa  " ++ q
			loggamus $ "raccolta di ordini " ++ q ++ " chiusa positivamente"
			eliminaStatoServizio j (undefined :: Ordini)
			(,) False <$> k (Just as) 
		reattoreOrdine esf (Right (first validante -> (w,FallimentoOrdine j))) = w $ \r -> do
			when (l /= j) mzero
			fallimento (ur /= r) $ "solo " ++ ur ++ " può chiudere la raccolta di ordini " ++ q
			Ordini y ur as is <- osservaStatoServizio j
			when (not y) esf 
			(,) False <$> effettoF j
		reattoreOrdine esf (Left (eliminazioneResponsabile -> Just (u,_))) = conFallimento $ do
			when (ur /= u) mzero
			Ordini y ur as is <- osservaStatoServizio l
			when (not y) esf 
			(,) False <$> effettoF l
		reattoreOrdine  _ (Left _) = return Nothing
	loggamus $ "raccolta di ordini " ++ q ++ " aperta"	
	return $ 
		( l
		, effettoF l
		, \esf -> Reazione (Nothing,reattoreOrdine esf)
		, modificaStatoServizio l $ \(Ordini _ ur as is) -> return (Ordini True ur as is)
		)




-----------------------------------------------------------------------------------
----------------------------------------------------------------------------------
-- askOrdini :: (ParteDi (Servizio Ordini) r, MonadReader r m) =>
--      (String -> m ()) -> m [(String, Int)]
reportOrdini :: (ParteDi (Servizio Ordini) s) => s -> [(String,Bool,Utente,[(Utente,Euro)],[(Utente,Euro)])]
reportOrdini x = let (xs :: [(Indice,(String,Ordini))])=  elencoSottoStati  $ x
	in map (\(_,(s,Ordini b u is as)) -> (s,b,u,is,as)) xs 

ordini 	= do 	(xs :: [(Indice,(String,Ordini))]) <- asks elencoSottoStati
			when (null xs) $ throwError "nessuna raccolta di ordini aperta"
			return $ map (fst . snd &&& fst) xs
ordiniFiltrati k e t = do
	SUtente mu <- asks see
	case mu of
		Just u -> do
			xs :: [(Indice,(String,Ordini))] <- filter (k u . snd . snd)
				<$> asks elencoSottoStati 
			let ys = filter (t . snd . snd) xs
			when (null ys) . throwError $ e u
			return  $ map (fst . snd &&& fst)  $ ys
		Nothing -> throwError $ "manca la selezione del responsabile autore"

costrEventiOrdine :: (
	Monad m,
	ParteDi (Servizio Ordini) s, 
	ParteDi Anagrafe s,
	ParteDi SUtente s
	) =>
	CostrAction m c EsternoOrdine s

costrEventiOrdine s kp kn = 	[("richiesta di ordine di denaro per un utente", eventoOrdine)
				,("correzione di ordine di denaro per un utente",eventoCorrezioneOrdine)
				,("fine di una raccolta ordini", eventoFineOrdine)
				,("fallimento di una raccolta ordini", eventoFallimentoOrdine) 
				] 
	where
	run = runSupporto s kn kp
        eventoFineOrdine = run $ do
		is <- ordiniFiltrati (\u -> (==) u . referente) ("nessuna raccolta ordini chiudibile dal responsabile " ++) permesso
                n <- scelte  is  $ ResponseOne "raccolta ordini da chiudere positivamente" 
                return $ FineOrdine n
        eventoFallimentoOrdine = run $ do
		is <- ordiniFiltrati (\u -> (==) u . referente) ("nessuna raccolta ordini aperta dal responsabile " ++) (const True)
                n <- scelte  is  $ ResponseOne  "raccolta ordini da chiudere negativamente" 
                return $ FallimentoOrdine n
	eventoCorrezioneOrdine = run $ do
		is <- ordini 
                n <- scelte  is  $ ResponseOne  "raccolta ordini per la quale correggere un ordine"  
		(xs :: [(Indice,(String,Ordini))]) <- asks elencoSottoStati 
		let zs = fromJust (lookup n xs)
		u <- scelte  (map (fst &&& fst) $ accettati $ snd $ zs)  $ ResponseOne  "utente coinvolto nella correzione"
		z <- libero  $ ResponseOne "nuova somma impegnata"
		return $ CorrezioneOrdine u z n
		
        eventoOrdine  = run $ do
		is <- ordini 
                n <- scelte  is  $ ResponseOne  "raccolta ordini alla quale partecipare"  
		us <- asks utenti 
		u <- scelte  (map (id &&& id) us)  $ ResponseOne  "utente impegnante"
		z <- libero  $ ResponseOne "somma impegnata"
                return $ Ordine u z n

costrQueryOrdini :: (Monad m, ParteDi (Servizio Ordini) s) => CostrAction m c Response s
costrQueryOrdini s kp kn = 	[("raccolte di ordini aperte",q)] 
	where
	run = runSupporto s kn kp
	r =  run $ do
		is <- ordini  
		return $ Response [("raccolte di ordini aperte",ResponseMany (map fst is))]
	q = run $ do
		is <- ordini
		n <- scelte  is  $ ResponseOne  "esamina la raccolta di ordini" 
		isx <- asks elencoSottoStati
		case lookup n isx of 
			Nothing -> throwError "incoerenza interna" 
			Just (t,Ordini ch ur as is) -> return $ 
				Response [("obiettivo della raccolta di ordini",ResponseOne t),
					("responsabile della raccolta di ordini", ResponseOne ur),
					("permesso a chiudere", ResponseOne $ if ch then "concesso" else "non ancora concesso"),
					("somme impegnate accettate ", ResponseMany . map (ResponseOne *** id) $ as),
					("somme impegnate in attesa di conferma ", ResponseMany . map (ResponseOne *** id) $ is),
					("riferimento", ResponseOne $ show n)]

