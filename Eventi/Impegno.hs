{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, ViewPatterns, NoMonomorphismRestriction, DeriveDataTypeable #-}

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
import Data.List (delete, union)
import Data.Maybe (fromJust)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad (mzero, when, msum)
import Control.Monad.Error (throwError)
import Control.Applicative ((<$>))
import Control.Monad.Reader (asks, MonadReader)
import Control.Arrow ((&&&), (***), first)
import Data.Time (LocalTime)
import Data.Typeable

import Lib.Aspetti ((.<), ParteDi, see)
import Lib.Prioriti (R(..))
import Lib.Assocs (update)
import Lib.Response (Response (..))
import Lib.QInteger (QInteger)
import Lib.NaturalLanguage

import Core.Types (Utente, Responsabile,Indice)
import Core.Costruzione (toSupporto, libero, scelte, runSupporto, CostrAction)
import Core.Programmazione (Effetti, Reazione (..) , EventoInterno (..),  nessunEffetto, Accentratore , Deviatore (..))
import Core.Inserimento (MTInserzione, conFallimento, fallimento, osserva, modifica, loggamus)
import Core.Parsing (Parser, ParserConRead (ParserConRead))
import Core.Dichiarazioni -- (Dichiarazione(Singola), Singola)

import Eventi.Servizio (Servizio, servizio0, nuovoStatoServizio, modificaStatoServizio, osservaStatoServizio, eliminaStatoServizio,elencoSottoStati)
import Eventi.Anagrafe (EsternoAssenso, Assensi, programmazionePermesso, Responsabili, Anagrafe, eliminazioneResponsabile, validante ,utenti, esistenzaResponsabile, SUtente (..))
import Eventi.Accredito (salda, accredita, Conti, Saldi)
import Eventi.Voci hiding (patcher,nullaer)
import Voci.UI.Ordini (nuovoOrdine)
import Voci.Names
import Debug.Trace

import Lib.ShowRead
import Lib.Euro

-- | gli eventi  esterni per questo modulo
data EsternoImpegno 
	= Impegno Utente Euro Indice	-- ^ indica un impegno di denaro da parte dell'utente per la causa chiave
	| FineImpegno Indice  		-- ^ indica la chiusura positiva della causa
	| FallimentoImpegno Indice	-- ^ indica la chiusura negativa della causa
	| CorrezioneImpegno Utente Euro Indice
	
data EsternoImpegnoVincolato =
	ImpegnoVincolato Utente [Ordine] Indice
	| CorrezioneImpegnoVincolato Utente [Ordine] [Ordine] Indice deriving (Show,Read, Typeable)

patcher (ImpegnoVincolato u os i) (ImpegnoVincolato u' os' i') 
	| u == u' && i == i' = Just $ ImpegnoVincolato u (os `union` os') i
patcher (CorrezioneImpegnoVincolato u ns vs i) (CorrezioneImpegnoVincolato u' ns' vs' i') 
	| u == u' && i == i' = Just $ (uncurry (CorrezioneImpegnoVincolato u)  $ mix ns ns' vs vs') i
patcher _ _ = Nothing

nullaer (ImpegnoVincolato _  [] _) = True
nullaer (CorrezioneImpegnoVincolato _ [] [] _) = True
nullaer _ = False

instance Patch EsternoImpegnoVincolato where
	patch xs x = case msum . map (\(y,ys) -> (,) ys `fmap` patcher x y) $ holing xs of
		Nothing -> x : xs 
		Just (ys,y)  -> y : ys
	nulla = nullaer

instance Show EsternoImpegno where
	show (Impegno u f i) = "impegno da " ++ quote u ++ " di " ++ show f ++ " in riferimento a " ++ show i
	show (FineImpegno i) = "chiusura della raccolta impegni riferita a " ++ show i
	show (FallimentoImpegno i) = "fallimento della raccolta impegni riferita a " ++ show i
	show (CorrezioneImpegno u f i) = "correzione impegno da " ++ quote u ++ " di " ++ show f ++ 
		" in riferimento a " ++ show i

instance Read EsternoImpegno where
	readPrec = let
		imp = do
			string "impegno da "
			u <- phrase 
			string " di " 
			f <- reads'
			string " in riferimento a "
			i <- reads'
			return $ Impegno u f i	
		fin = do 
			string "chiusura della raccolta impegni riferita a "
			i <- reads'
			return $ FineImpegno i
		fal = do 
			string "fallimento della raccolta impegni riferita a "
			i <- reads'
			return $ FallimentoImpegno i
		cor = do 
			string "correzione impegno da "
			u <- phrase 
			string " di " 
			f <- reads'
			string " in riferimento a "
			i <- reads'
			return $ CorrezioneImpegno u f i

		in lift $ imp <++ fin <++ fal <++ cor
priorityImpegno = R k where
	k (Impegno _ _ _) = -27
	k (FineImpegno _) = 15
	k (FallimentoImpegno _) = 16
	k (CorrezioneImpegno _ _ _) = -20

priorityImpegnoVincolato = R k where	
	k (ImpegnoVincolato _ _ _) = -27
	k (CorrezioneImpegnoVincolato _ _ _ _) = -20


-- | evento interno che segnala il fallimento di una causa
data Interni = EliminazioneResponsabileA Utente deriving (Show,Read)


-- accentratore dell'evento interno di eliminazione responsabile negli eventi interni del modulo
acc = [Deviatore (\(i::Interni) -> Just i),Deviatore f] where
	f x = case eliminazioneResponsabile x of
		Nothing -> Nothing
		Just (w,r) -> Just $ EliminazioneResponsabileA w 


-- | lo stato per ogni causa
data Impegni = Impegni {permesso :: Bool, referente::Utente, accettati :: [(Utente,Euro)], inattesa :: [(Utente,Euro)]} deriving (Show,Read,Eq)

-- | tipo dello stato aggiunto degli impegni
type TyImpegni a = (Servizio Impegni , a)

-- | aggiunta dell'aspetto Servizio Impegni per il boot
bootImpegni :: a -> TyImpegni a
bootImpegni x = (servizio0,x)

raccolte :: (Servizio Impegni `ParteDi` s) => s -> [String]
raccolte s = let xs :: [(Indice,(String,Impegni))] = elencoSottoStati s  in map (fst . snd) xs
raccoltei :: (Servizio Impegni `ParteDi` s) => s -> [(String,Indice)]
raccoltei s = let xs :: [(Indice,(String,Impegni))] = elencoSottoStati s  in map (fst . snd &&& fst) xs
nomeRaccolta :: ParteDi (Servizio Impegni) s => s -> Indice -> Maybe String
nomeRaccolta s i = let xs :: [(Indice,(String,Impegni))] = elencoSottoStati s in fst <$> lookup i xs 
-- unImpegno s n = (\(Impegni us) -> us) <$> snd <$> seeStatoServizio  (undefined :: Impegni) s n

-- | il tipo della funzione da passare alla hof restituita da programmazioneImpegno 
-- type ConclusioneReattoreImpegno s c = Maybe ([(Utente, Euro)]) -> MTInserzione s c Utente (Effetti s c Utente)

-- | la programmazione di un impegni richiede il nome del responsabile che la apre e restituisce la chiave del nuovo stato impegni con una una azione monadica in grado di creare una nuova Reazione se fornita della giusta procedura. La giusta procedura definisce cosa eseguire alla fine della raccolta impegni. In caso di successo l'azione riceve la lista di impegni raccolta , in caso di fallimento Nothing. Comunque essa deve fornire una lista di nuovi reattori e una lista di eventi interni.
programmazioneImpegno' :: (Parser c EsternoImpegnoVincolato,
	Parser c EsternoImpegno,  -- okkio se serve un parser va implementato
	Parser c EsternoAssenso,
	Anagrafe 		`ParteDi` s,  -- eventoValidato lo richiede
	Conti 			`ParteDi` s,  -- preleva e accredita lo richiedono
	Saldi 			`ParteDi` s,  -- preleva e accredita lo richiedono
	Responsabili 		`ParteDi` s,  -- eliminazioneResponsabile lo richiede
	Servizio Assensi 		`ParteDi` s,
	Integer `ParteDi` s,
	Servizio Impegni 	`ParteDi` s)  -- il nostro aspetto
	=> String	-- ^ motivazione della raccolta
	-> Utente 	-- ^ l'utente responsabile dell'impegno
	-> (Maybe ([(Utente, Euro)]) -> MTInserzione s c Utente (Effetti s c Utente))
	-> MTInserzione s c Utente 
		( Indice
		, MTInserzione s c Utente (Effetti s c Utente)
		, MTInserzione s c Utente () ->  [Reazione s c Utente]
		, MTInserzione s c Utente ()
		, [EventoInterno]
		)

programmazioneImpegno' q' ur k  = do
	l <- nuovoStatoServizio (Impegni False ur [] []) (ur,q')
	let q = " per l'acquisto di " ++ q'
	let 	effettoF j = do 
			Impegni _ ur as is <- osservaStatoServizio j
			mapM_ (\(u,v) -> accredita u (mkDEuro v) $ "restituzione per fallimento " ++ q) (as ++ is) -- restituzione del denaro di tutti gli impegni
			eliminaStatoServizio j (undefined :: Impegni)
			(ks,es) <- k Nothing 
			return (ks,[])
		reattoreImpegno _ (Right (first validante -> (w,i@(CorrezioneImpegno u v j)))) = w $ \r -> do
				when (l /= j) mzero
				fallimento (ur /= r)  $ "solo " ++ ur ++ " può correggere gli impegni per " ++ q
				Impegni _ _ as is <- osservaStatoServizio j 
				fallimento (not $ u `elem` map fst as) "nessun impegno tra gli accettati per l'utente"
				let vp = fromJust (lookup u as) -- denaro impegnato
				z <- accredita u (mkDEuro $ vp - v) $ "correzione impegno " ++ q 
				fallimento (z < 0) "il credito non copre l'operazione" 
				modificaStatoServizio j $ \(Impegni ch ur as is) -> return $
					Impegni ch ur ((u,v):(filter ((/=) u . fst) as)) is
				loggamus  $ "correzione d'impegno per  " ++ show (mkDEuro $ v - vp) ++ " da " ++ u  ++ q
				return (True, nessunEffetto)  

		reattoreImpegno _ (Right (first validante ->  (w,i@(Impegno u v j)))) = w $ \r -> let
			positivo _ = do
				loggamus $ "accettato l'impegno di " ++ show v ++ " da " ++ u ++ q 
				modificaStatoServizio j $ \(Impegni ch ur as is) -> return 
					(Impegni ch ur ((u,v):as) (delete (u,v) is))
				return nessunEffetto
			negativo _ = do 
				accredita u (mkDEuro v) $ "restituzione per rifiuto richiesta " ++ q
				modificaStatoServizio j $ \(Impegni ch ur as is) -> return 
					(Impegni ch ur as (delete (u,v) is))
				loggamus $ "rifiutato l'impegno di " ++ show v ++ " da " ++ u  ++ q
				return nessunEffetto
			in do 
				when (l /= j) mzero
				Impegni _ _ as is <- osservaStatoServizio j 
				-- fallimento (u `elem` map fst (as ++ is)) "impegno già richiesto o accettato per l'utente"
				z <- accredita u (mkDEuro $ negate v) $ "richiesta di impegno " ++ q 
				fallimento (z < 0) "il credito non copre l'operazione" 
				
				modificaStatoServizio j $ \(Impegni ch ur as is) -> return (Impegni ch ur as $ (u,v):
					filter ((/=) u . fst) is)
				loggamus  $ "richiesta di impegno di  " ++ show v ++ " da " ++ u  ++ q
				(_,reaz) <- programmazionePermesso 
					("impegno di " ++ show v ++ " da " ++ u ++ q)
					r ur positivo negativo
				return (True, ([reaz],[]))  
		reattoreImpegno _ (Right (first validante -> (w,FineImpegno j))) = w $ \r -> do
			when (l /= j) mzero
			Impegni y ur as is <- osservaStatoServizio j
			fallimento (ur /= r) $ "solo " ++ ur ++ " può chiudere la raccolta di impegni " ++ q
			fallimento (not y) $ "la chiusura non è stata concessa  " ++ q
			fallimento (not . null $ is) "richieste di impegno ancora in attesa di conferma"
			salda r (mkDEuro . negate $ (sum $ map snd as)) $ "spesa  " ++ q
			loggamus $ "raccolta di impegni " ++ q ++ " chiusa positivamente"
			eliminaStatoServizio j (undefined :: Impegni)
			(rzs,ievs) <- k (Just as)
			return (False,(rzs,EventoInterno (EventoChiusuraImpegni l):ievs)) 
		reattoreImpegno esf (Right (first validante -> (w,FallimentoImpegno j))) = w $ \r -> do
			when (l /= j) mzero
			fallimento (ur /= r) $ "solo " ++ ur ++ " può chiudere la raccolta di impegni " ++ q
			Impegni y ur as is <- osservaStatoServizio j
			fallimento (not $ null is) $ "manca l'assenso su alcune richieste"
			when (not y) esf 
			(,) False <$> effettoF j
		reattoreImpegno esf (Left (EliminazioneResponsabileA u)) = conFallimento $ do
			when (ur /= u) mzero
			Impegni y ur as is <- osservaStatoServizio l
			when (not y) esf 
			(,) False <$> effettoF l
		reattoreImpegno  _ (Left _) = return Nothing


 		reattoreImpegnoVincolato _ (Right (first validante ->  (w,i@(ImpegnoVincolato u os j)))) = w $ \r -> let
			v = sum . map denaro $ os
			positivo _ = do
				loggamus $ "accettato l'impegno di " ++ show v ++ " da " ++ u ++ q 
				modificaStatoServizio j $ \(Impegni ch ur as is) -> return 
					(Impegni ch ur ((u,v):as) (delete (u,v) is))
				return nessunEffetto

			negativo _ = do 
				accredita u (mkDEuro v) $ "restituzione per rifiuto richiesta " ++ q
				modificaStatoServizio j $ \(Impegni ch ur as is) -> return 
					(Impegni ch ur as (delete (u,v) is))
				loggamus $ "rifiutato l'impegno di " ++ show v ++ " da " ++ u  ++ q

				return ([],[EventoInterno (InternoVoci (CorreggiOrdine j u [] os))]) 
			in do 
				when (l /= j) mzero
				Impegni _ _ as is <- osservaStatoServizio j 
				fallimento (u `elem` map fst (as ++ is)) "impegno già richiesto o accettato per l'utente"
				accredita u (mkDEuro $ negate v) $ "richiesta di impegno " ++ q 
				modificaStatoServizio j $ \(Impegni ch ur as is) -> return (Impegni ch ur as $ (u,v):
					filter ((/=) u . fst) is)
				loggamus  $ "richiesta di impegno vincolato di  " ++ show v ++ " da " ++ u  ++ q
				(_,reaz) <- programmazionePermesso 
					("impegno di " ++ show v ++ " da " ++ u ++ q)
					r ur positivo negativo
				return (True, ([reaz],[EventoInterno (InternoVoci (CorreggiOrdine j u os []))]))   
		reattoreImpegnoVincolato _ (Right (first validante -> (w,i@(CorrezioneImpegnoVincolato u nos vos j)))) = w $ \r -> do
				let v = sum (map denaro nos) - sum (map denaro vos)
				when (l /= j) mzero
				fallimento (ur /= r)  $ "solo " ++ ur ++ " può correggere gli impegni per " ++ q
				Impegni _ _ as is <- osservaStatoServizio j 
				fallimento (not $ u `elem` map fst as) "nessun impegno tra gli accettati per l'utente"
				let vp = fromJust (lookup u as) -- denaro impegnato
				accredita u (mkDEuro $ vp - v) $ "correzione impegno " ++ q 
				modificaStatoServizio j $ \(Impegni ch ur as is) -> return $
					Impegni ch ur ((u,v):(filter ((/=) u . fst) as)) is
				loggamus  $ "correzione d'impegno per  " ++ show (mkDEuro $ v - vp) ++ " da " ++ u  ++ q
				return (True, ([],[EventoInterno (InternoVoci (CorreggiOrdine j u nos vos))])   )
		reattoreImpegnoVincolato esf (Left (EliminazioneResponsabileA u)) = conFallimento $ do
			when (ur /= u) mzero
			Impegni y ur as is <- osservaStatoServizio l
			when (not y) esf 
			(,) False <$> effettoF l
		reattoreImpegnoVincolato  _ (Left _) = return Nothing

	loggamus $ "raccolta di impegni " ++ q ++ " aperta"	
	return $ 
		( l
		, effettoF l
		, \esf -> [Reazione (Just acc,reattoreImpegno esf),Reazione (Just acc,reattoreImpegnoVincolato esf)]
		, modificaStatoServizio l $ \(Impegni _ ur as is) -> return (Impegni True ur as is)
		,[EventoInterno $ EventoAperturaImpegni ur l q']
		)




-----------------------------------------------------------------------------------
----------------------------------------------------------------------------------
-- askImpegni :: (ParteDi (Servizio Impegni) r, MonadReader r m) =>
--      (String -> m ()) -> m [(String, Int)]
reportImpegni :: (ParteDi (Servizio Impegni) s) => s -> [(String,Bool,Utente,[(Utente,Euro)],[(Utente,Euro)])]
reportImpegni x = let (xs :: [(Indice,(String,Impegni))])=  elencoSottoStati  $ x
	in map (\(_,(s,Impegni b u is as)) -> (s,b,u,is,as)) xs 

impegni 	= do 	(xs :: [(Indice,(String,Impegni))]) <- asks elencoSottoStati
			when (null xs) $ throwError "nessuna raccolta di impegni aperta"
			return $ map (fst . snd &&& fst) xs

impegniFiltrati k e t = do
	SUtente mu <- asks see
	case mu of
		Just u -> do
			xs :: [(Indice,(String,Impegni))] <- filter (k u . snd . snd)
				<$> asks elencoSottoStati 
			let ys = filter (t . snd . snd) xs
			when (null ys) . throwError $ e u
			return  $ map (fst . snd &&& fst)  $ ys
		Nothing -> throwError $ "manca la selezione del responsabile autore"

costrEventiImpegno :: (
	Monad m,
	Parser p EsternoImpegno,
	ParteDi (Servizio Impegni) s, 
	ParteDi Anagrafe s,
	ParteDi SUtente s
	) =>
	CostrAction m c (Dichiarazione p Singola) s

costrEventiImpegno s kp kn = 	[("richiesta di impegno di denaro per un utente", eventoImpegno)
				,("correzione di impegno di denaro per un utente",eventoCorrezioneImpegno)
				,("fine di una raccolta impegni", eventoFineImpegno)
				,("fallimento di una raccolta impegni", eventoFallimentoImpegno) 
				] 
	where
	run = runSupporto s kn kp
        eventoFineImpegno = run $ do
		is <- impegniFiltrati (\u -> (==) u . referente) ("nessuna raccolta impegni chiudibile dal responsabile " ++) permesso
                n <- scelte  is  $ ResponseOne "raccolta impegni da chiudere positivamente" 
                return . Singola  $ FineImpegno n
        eventoFallimentoImpegno = run $ do
		is <- impegniFiltrati (\u -> (==) u . referente) ("nessuna raccolta impegni aperta dal responsabile " ++) (const True)
                n <- scelte  is  $ ResponseOne  "raccolta impegni da chiudere negativamente" 
                return . Singola  $ FallimentoImpegno n
	eventoCorrezioneImpegno = run $ do
		is <- impegni 
                n <- scelte  is  $ ResponseOne  "raccolta impegni per la quale correggere un impegno"  
		(xs :: [(Indice,(String,Impegni))]) <- asks elencoSottoStati 
		let zs = fromJust (lookup n xs)
		u <- scelte  (map (fst &&& fst) $ accettati $ snd $ zs)  $ ResponseOne  "utente coinvolto nella correzione"
		z <- libero  $ ResponseOne "nuova somma impegnata"
		return . Singola  $ CorrezioneImpegno u z n
		
        eventoImpegno  = run $ do
		is <- impegni 
                n <- scelte  is  $ ResponseOne  "raccolta impegni alla quale partecipare"  
		us <- asks utenti 
		u <- scelte  (map (id &&& id) us)  $ ResponseOne  "utente impegnante"
		z <- libero  $ ResponseOne "somma impegnata"
                return . Singola  $ Impegno u z n
costrEventiImpegnoVincolato s kp kn = 	[("richiesta di nuovo ordine per un utente", eventoImpegnoVincolato)
				,("correzione di ordine per un utente",eventoCorrezioneImpegnoVincolato)
				] 
	where
	run = runSupporto s kn kp

        eventoImpegnoVincolato  = run $ do
		is <- impegni 
                n <- scelte  is  $ ResponseOne  "acquisto al quale partecipare"  
		us <- asks utenti 
		u <- scelte  (map (id &&& id) us)  $ ResponseOne  "utente ordinante"
		StatoVoci _ as _ <- asks see
		case lookup n as of
			Nothing -> throwError "l'acquisto non ha voci ordinabili"
			Just (_,_,vs) -> do 
				Voce cs fs o <- scelte (map (loggaVoce &&& id) vs) $ ResponseOne "voce da ordinare"
				o <- toSupporto $ nuovoOrdine o
		                return . Composta $ [ImpegnoVincolato u [Ordine cs fs o] n]
	eventoCorrezioneImpegnoVincolato = undefined



costrQueryImpegni :: (Monad m, ParteDi (Servizio Impegni) s) => CostrAction m c Response s
costrQueryImpegni s kp kn = 	[("raccolte di impegni aperte",q)] 
	where
	run = runSupporto s kn kp
	r =  run $ do
		is <- impegni  
		return $ Response [("raccolte di impegni aperte",ResponseMany (map fst is))]
	q = run $ do
		is <- impegni
		n <- scelte  is  $ ResponseOne  "esamina la raccolta di impegni" 
		isx <- asks elencoSottoStati
		case lookup n isx of 
			Nothing -> throwError "incoerenza interna" 
			Just (t,Impegni ch ur as is) -> return $ 
				Response [("obiettivo della raccolta di impegni",ResponseOne t),
					("responsabile della raccolta di impegni", ResponseOne ur),
					("permesso a chiudere", ResponseOne $ if ch then "concesso" else "non ancora concesso"),
					("somme impegnate accettate ", ResponseMany . map (ResponseOne *** id) $ as),
					("somme impegnate in attesa di conferma ", ResponseMany . map (ResponseOne *** id) $ is),
					("riferimento", ResponseOne $ show n)]

