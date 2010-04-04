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
import Control.Arrow ((***),first,  second,  (&&&))
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
type Indice = Integer
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
	k (NuovoUtente _) = -40
	k (EliminazioneResponsabile _) = -38
	k (ElezioneResponsabile _) = -39
	
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
	ParteDi Integer s, 
	Parser c EsternoAnagrafico) =>
	Reazione s c Utente

reazioneAnagrafe = soloEsterna reattoreAnagrafe' where
	reattoreAnagrafe' (first validante -> (w,NuovoUtente u)) = w $ \r -> do
		Anagrafe us <- osserva
		fallimento (u `elem` us) "utente già presente nel gruppo" 
		modifica . const $ Anagrafe (u:us)
		logga $ "accettato il nuovo utente " ++ u
		return (True, nessunEffetto)
	reattoreAnagrafe' (first validante -> (w,ElezioneResponsabile u)) = w $ \r -> do
		Anagrafe us <- osserva
		esistenzaUtente (fst u)
		Responsabili us ls <- osserva 
		fallimento (u `elem` us) "utente già eletto" 
		fallimento (u `elem` map snd ls) "utente già in elezione" 
		(l,reaz) <- programmazioneAssenso ("elezione dell'utente " ++ fst u) r maggioranza (chiudibene u) (chiudimale u)
		modifica $ \(Responsabili us ls) -> Responsabili us ((l,u):ls)
		return (True,([reaz],[])) 
		where
		chiudibene u l = do
			modifica $ \(Responsabili us ls) -> Responsabili (u:us) (filter ((/=) l . fst) ls)
			logga $ "eletto il nuovo responsabile " ++ fst u
			return nessunEffetto
		chiudimale u l = do
			modifica $ \(Responsabili us ls) -> Responsabili us (filter ((/=) l . fst) ls)
			logga $ "elezione dell'utente " ++ fst u ++ " fallita"
			return nessunEffetto

	reattoreAnagrafe' (first validante -> (w,EliminazioneResponsabile u)) = w $ \r -> do
		Anagrafe us <- osserva
		esistenzaUtente u
		esistenzaResponsabile u
		Responsabili us ls <- osserva 
		fallimento (u `elem` map (fst . snd) ls) "revoca del responsabile già richiesta" 
		(l,reaz) <- programmazioneAssenso ("revoca del responsabile " ++ u) r maggioranza (chiudibene u r) (chiudimale u r)
		modifica $ \(Responsabili us ls) -> (Responsabili us ((l,(u,us ? (u,undefined))):ls))
		return (True,([reaz],[]))
		where
		chiudibene u r l = do
			modifica $ \(Responsabili us ls ) -> Responsabili (elimina u us) (elimina l ls)
			logga $ "revocato il responsabile  " ++ u
			return ([],[EventoInterno $ EventoEliminazioneResponsabile u r])
		chiudimale u r l = do
			modifica $ \(Responsabili us ls ) -> Responsabili us  (elimina l ls)
			logga $ "rinuncia alla revoca del responsabile " ++ u
			return nessunEffetto
------------------------------------------------------------------------------------------------
-- reparto costruzione ed interrogazione
------------------------------------------------------------------------------------------------
 
-- | estrae la lista di costrResponsabili
costrResponsabili :: (Monad m , ParteDi Responsabili s) => Supporto m s b [Responsabile]	
costrResponsabili = do
	(rs,_) <- asks responsabili
	when (null rs) $ throwError "nessun responsabile presente nel gruppo" 
	return rs

-- | estrae la lista di costrUtenti
costrUtenti :: (Monad m, ParteDi Anagrafe s) => Supporto m s b [Utente]	
costrUtenti = do
	rs <- asks utenti
	when (null rs) $ throwError "nessun utente presente nel gruppo" 
	return rs

-- | costruzione degli eventi esterni per la gestione costrUtenti e costrResponsabili
costrEventiAnagrafe :: (Monad m, ParteDi Anagrafe s, ParteDi Responsabili s) => CostrAction m c EsternoAnagrafico s
costrEventiAnagrafe s kp kn =	[("inserimento di un nuovo utente", eventoNuovoUtente)]
	where
	run = runSupporto s kn kp
        eventoNuovoUtente =  run $ do
		us <- costrUtenti 
                n <- libero "il nomignolo del nuovo utente"
		when (n `elem` us) $ throwError "nome già utilizzato"	
                return $ NuovoUtente n
costrEventiResponsabili :: (Monad m, ParteDi Responsabili s, ParteDi Anagrafe s) => CostrAction m c EsternoAnagrafico s
costrEventiResponsabili s kp kn =
	[("richiesta di elezione di un nuovo responsabile", eventoElezioneResponsabile)
	,("richiesta di revoca di un responsabile",eventoEliminazioneResponsabile)
	] 
	where
	run = runSupporto s kn kp
        eventoElezioneResponsabile = run $ do
		us <- costrUtenti 
		rs <- costrResponsabili 
		let ds = us \\ map fst rs
		when (null ds) $ throwError "nessun utente non responsabile disponibile"
                n <- scelte (map (id &&& id) ds) "selezione dell'utente da eleggere a responsabile" 
                m <- upload $ "inserimento delle chiavi del responsabile " ++ n ++ " per la sua elezione"
                return $ ElezioneResponsabile m

        eventoEliminazioneResponsabile = run $ do
		rs <- costrResponsabili 
                n <- scelte (map (fst &&& id) rs) "selezione del responsabile da revocare"
                return $ EliminazioneResponsabile (fst n)

-- | costruzione delle interrogazione sull'anagrafe e sui costrResponsabili
costrQueryAnagrafe :: (Monad m, ParteDi Anagrafe s, ParteDi Responsabili s) => CostrAction m c Response s
costrQueryAnagrafe s kp kn = 	[("nomi utenti",queryElencoUtenti)
				,("nomi responsabili",queryElencoResponsabili)
				] 
	where
	run = runSupporto s kn kp
	queryElencoUtenti = run $ do
		us <- costrUtenti 
		return $ Response [("nomi utenti", ResponseMany $ map ResponseOne us)]
	queryElencoResponsabili = run $ do
		rs <- costrResponsabili 
		return $ Response [("nomi responsabili", ResponseMany $ map (ResponseOne. fst) rs )]
		
----------------------------------------------------------------------------------------------------------------------
--  sezione assensi, putroppo non ha un modulo a parte a causa del ciclo di dipendenze con l'anagrafe

-- | gli eventi che interessano una raccolta di assensi
data EsternoAssenso = Assenso Indice | Dissenso Indice | EventoFallimentoAssenso Indice deriving (Read, Show)

-- | lo stato necessario per la gestione di un tipo di assensi
data Assensi = Assensi Utente [Utente] [Utente] | Permesso Utente Utente deriving (Show,Read)

richiedente (Assensi r _ _) = r
richiedente (Permesso r _) = r 
data Check = Positivo | Negativo | Indecidibile deriving (Show,Read)

-- controlla che la maggioranza sia raggiunta
maggioranza :: (Responsabili `ParteDi` s) => ([Utente],[Utente]) -> MTInserzione s c Utente Check
maggioranza (ps,ns) = do 	
	Responsabili us _ <- osserva
	let soglia =  (length us + 1) `div` 2
	return $ if length ns >= soglia then Negativo else 
			if length ps >= soglia then Positivo else
				Indecidibile

programmazionePermesso se ur ut k kn = do
	l <- nuovoStatoServizio (Permesso ur ut) (ur ++ ", " ++ se)
	let 	eliminaRichiesta u j = do
			eliminaStatoServizio j (undefined :: Assensi)  
			logga $ "rinuncia alla questione " ++ se
			return (False,nessunEffetto) -- non rischedula il reattore
		reattoreAssenso (Right (first validante -> (w,Assenso j))) = w $ \r -> do
			when (j /= l) mzero
			Permesso _ ut' <- osservaStatoServizio j 
			fallimento (ut /= ut') "il responsabile non è tenuto a dare il permesso sulla questione" 
			eliminaStatoServizio j (undefined :: Assensi)
			logga $ "chiusura postiva della questione " ++ se 
			(,) False <$> k j -- esegui la procedura finale come coronamento del 
						-- consenso e non rischedula il reattore 
		reattoreAssenso (Right (first validante -> (w,Dissenso j))) = w $ \r -> do
			when (j /= l) mzero
			Permesso _ ut' <- osservaStatoServizio j 
			fallimento (ut /= ut') "il responsabile non è tenuto a dare il permesso sulla questione" 
			eliminaRichiesta r j
			logga $ "chiusura negativa della questione " ++ se
			(,) False <$> kn j
		reattoreAssenso (Right (first validante -> (w,EventoFallimentoAssenso j))) = w $ \r -> do
			when (j /= l) mzero
			fallimento (ur /= r)  "questione aperta da un altro responsabile"
			eliminaRichiesta r j
		reattoreAssenso (Left (eliminazioneResponsabile -> Just (u,r))) = conFallimento $ do
			when (ur /= u) mzero
			logga $ "eliminazione della richiesta " ++ se
			eliminaRichiesta u l

	logga $ "posta la questione per l'obiettivo " ++ se 
 	return (l,Reazione (Nothing, reattoreAssenso)) -- restituisce il riferimento a questa richiesta perché venga nominato negli eventi di assenso
-- | funzione di programmazione per una nuova raccolta di assensi
programmazioneAssenso :: (
	Servizio Assensi `ParteDi` s
	, Responsabili `ParteDi` s
	, Anagrafe `ParteDi` s
	, Parser c EsternoAssenso
	, ParteDi Integer s
	)
	=> String	-- ^ nome della raccolta 
	-> Utente 	-- ^ l'utente che la richiede
	-> (([Utente],[Utente]) -> MTInserzione s c Utente Check) -- ^ condizione di rolling 
	-> (Indice -> MTInserzione s c Utente (Effetti s c Utente)) -- ^ la chiusura per il successo della raccolta
	-> (Indice -> MTInserzione s c Utente (Effetti s c Utente)) -- ^ la chiusura per il fallimento della raccolta
	-> MTInserzione s c Utente (Indice, Reazione s c Utente)	-- ^ la chiave per emettere assensi relativi e la reazione da schedulare

programmazioneAssenso se ur c k kn = do
	l <- nuovoStatoServizio (Assensi ur [] []) (ur ++ ", " ++ se) -- ricevi la chiave per la nuova raccolta
	let 	eliminaRichiesta u j = do
			eliminaStatoServizio j (undefined :: Assensi)  
			logga $ "rinuncia alla questione " ++ se
			return (False,nessunEffetto) -- non rischedula il reattore

		reattoreAssenso (Right (first validante -> (w,Assenso j))) = w $ \r -> do
			when (j /= l) mzero
			Assensi ur ps ns <- osservaStatoServizio j 
			fallimento (r `elem` (ps ++ ns)) "il responsabile si è già espresso sulla questione" 
			let ps' = r:ps -- la nuova lista di assensi per j 
			t <- c (ps',ns) -- controlla che si debba continuare a ricevere gli assensi
			case t of
				Positivo  -> do
					eliminaStatoServizio j (undefined :: Assensi)
					logga $ "chiusura postiva della questione " ++ se 
					(,) False <$> k j -- esegui la procedura finale come coronamento del 
						-- consenso e non rischedula il reattore 
				Indecidibile -> do		
					logga $ "ricevuto l'assenso da " ++ r 
						++ " sulla questione " ++ se
					modificaStatoServizio j $ \_ -> return (Assensi ur ps' ns) -- registra gli assensi
					return (True,nessunEffetto) -- continua a ricevere

		reattoreAssenso (Right (first validante -> (w,Dissenso j))) = w $ \r -> do
			when (j /= l) mzero
			Assensi ur ps ns <- osservaStatoServizio j 
			fallimento (r `elem` (ps ++ ns)) "il responsabile si è già espresso sulla questione" 
			let ns' = r:ns -- la nuova lista di assensi per j 
			t <- c (ps,ns') -- controlla che si debba continuare a ricevere gli assensi
			case t of 	
				Negativo -> do
					eliminaRichiesta r j
					logga $ "chiusura negativa della questione " ++ se
					(,) False <$> kn j
				Indecidibile -> do		
					logga $ "ricevuto il dissenso da " ++ r 
						++ " sulla questione " ++ se
					modificaStatoServizio j $ \_ -> return (Assensi ur ps ns') -- registra gli assensi
					return (True,nessunEffetto) -- continua a ricevere

		reattoreAssenso (Right (first validante -> (w,EventoFallimentoAssenso j))) = w $ \r -> do
			when (j /= l) mzero
			fallimento (ur /= r) "questione aperta da un altro responsabile"
			eliminaRichiesta r j
			(,) False <$> kn j
		reattoreAssenso (Left (eliminazioneResponsabile -> Just (u,r))) = conFallimento $ do
			when (ur /= u) mzero
			logga $ "eliminazione della richiesta " ++ se
			eliminaRichiesta u l
			(,) False <$> kn l
		reattoreAssenso (Left _) = return Nothing
	logga $ "posta la questione per l'obiettivo " ++ se 
 	return (l,Reazione (Nothing, reattoreAssenso)) -- restituisce il riferimento a questa richiesta perché venga nominato negli eventi di assenso

--------------------------- costruzioni per il modulo assensi -----------------------------

-- | estrae gli assensi dallo stato in lettura
assensi :: (Monad m, ParteDi (Servizio Assensi) s) => Supporto m s b [(String, Utente)]
assensi = do
	xs :: [(Indice,(String,Assensi))] <- asks elencoSottoStati 
	when (null xs) $ throwError "nessuna questione aperta"
	return  $ map (second richiedente . snd) xs

filtra u (Assensi ur ps ns) = not . elem u $ ps ++ ns
filtra u (Permesso ur u') = u' == u 
assensiFiltrati k e = do
	SUtente mu <- asks see
	case mu of
		Just u -> do
			xs :: [(Indice,(String,Assensi))] <- filter (k u . snd . snd)
				<$> asks elencoSottoStati 
			when (null xs) . throwError $ e u
			return  $ map (fst . snd &&& (id *** fst))  $ xs
		Nothing -> throwError $ "manca la selezione del responsabile autore"


data SUtente = SUtente (Maybe Utente)
-- | costrutore degli eventi di assenso
costrEventiAssenso :: (Monad m, Servizio Assensi `ParteDi` s, SUtente `ParteDi` s) => CostrAction m c EsternoAssenso s
costrEventiAssenso s kp kn = 	[("parere su una questione",eventoAssenso s kp kn )
				,("chiusura prematura di una questione",eventoFallimentoAssenso)
				] 
	where
	eventoAssenso s kp kn = runSupporto s kn kp $ do
		ys <- assensiFiltrati filtra ("nessuna questione ancora aperta per il responsabile " ++) 
		(n,s) <- scelte ys "questione sulla quale esprimersi" 
		ad <- scelte [("assenso",Assenso),("dissenso",Dissenso)] ("parere sulla questione " ++ s) 
		return $ ad n

        eventoFallimentoAssenso = runSupporto s kn kp $ do
		ys <- assensiFiltrati (\u -> (==) u . richiedente) ("nessuna questione aperta dal responsabile " ++)
                (n,_) <- scelte ys "seleziona la questione da chiudere"   
              	return $ EventoFallimentoAssenso n

-- | costruzione delle interrogazioni sul modulo di assensi
costrQueryAssenso :: (Monad m , Servizio Assensi `ParteDi` s) => CostrAction m c Response s
costrQueryAssenso s kp kn = [("questioni aperte", querySottoStati)] 
	where
	querySottoStati = runSupporto s kn kp $ do
		ys <- assensi
		return $ Response [("questioni aperte", if null ys then
			ResponseOne "nessuna questione aperta" else ResponseAL $ 
				map (\(s,u) -> (u,ResponseOne s)) ys)]
priorityAssenso = R k where
	k (Assenso _) = -25
	k (Dissenso _) = -24
	k (EventoFallimentoAssenso _) = -16
---------------------------------------------------


