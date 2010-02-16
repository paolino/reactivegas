
{-# LANGUAGE FlexibleContexts, ExistentialQuantification, ScopedTypeVariables, GeneralizedNewtypeDeriving, NoMonomorphismRestriction #-}
module Core.UI where

import Data.Maybe (fromJust)
import Data.List (delete)

import Control.Arrow
import Control.Applicative
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import System.Console.Haskeline (MonadException)
import Debug.Trace


import Lib.Passo (Costruzione,menu, rotonda ,rmenu, Passo) 
import qualified Lib.Passo as P

import Lib.TreeLogs (eccoILogs)
import Lib.Firmabile (cryptobox)
import Lib.Prioriti (R)
import Lib.Response (Response (..))


import Core.Types (Esterno, Evento)
import Core.Controllo (caricaEventi, SNodo (..))
import Core.Contesto (flatten)
import Core.Programmazione (Reazione)
import Core.Parsing (ParserConRead)
import Core.Patch (login, Firmante (..))
import Core.Costruzione (runSupporto, Supporto)
import Core.Persistenza (Persistenza (..))


import Eventi.Amministrazione
import Eventi.Anagrafe
import Eventi.Accredito
import Eventi.Impegno
import Eventi.Ordine


-- | il tipo dello stato accessibile
type TS = TyAnagrafe (TyAccredito (TyImpegni (TyOrdini ())))

-- |tipo dello stato con la serializzazione dei reattori
type QS = (TS,[SNodo TS Utente])

-- | lista di prioritizzatori, definiscono un riordinamento tra gli eventidi una patch
priorita :: [Lib.Prioriti.R]
priorita = [priorityAnagrafe, priorityAnagrafeI, priorityAccredito
		, priorityImpegnoI, priorityImpegno, priorityOrdine, priorityAssenso] 

-- | lista di reattori. I reattori di base per gli eventi
reattori :: [Reazione TS ParserConRead Utente]
reattori = [reazioneAnagrafe, reazioneAccredito, reazioneOrdine] 

-- | creazione di un novo stato di tipo QS
nuovoStato :: [Responsabile] -> QS
nuovoStato rs = (bootAnagrafe rs  . bootAccredito . bootImpegni . bootOrdini $ (), replicate (length rs) $ SNodo True [])

-- | effettua un inserimento di eventi esterni nello stato, restituendo il nuovo. Stampa i logs
caricamento :: [Esterno Utente] -> QS -> (QS,String)
caricamento es = second (eccoILogs . map (first flatten)) . caricaEventi priorita reattori es 

-- | la monade dove gira il programma. Mantiene in lettura lo stato del gruppo insieme alle operazioni di IO. Nello stato la lista degli eventi aspiranti un posto nella patch
type Env = ReaderT (Persistenza QS) IO
type Interfaccia = Costruzione Env () 


-- applicazione :: Wake QS -> Costruzione IO () (Env (Passo Env ()), [String], Env a -> StateT [String] IO a)
applicazione p = forever $ do
	ms <- asks readStato >>= liftIO
	case ms of 
		Nothing -> rotonda $ \k -> do  
			P.menu "il gruppo non esiste ancora" 
				[("creazione nuove chiavi di responsable", bootChiavi)
				,("creazione nuovo stato di gruppo", bootGruppo [] >> k ())
				]
		Just s -> do 
			mk <- accesso 
			case mk of 
				Nothing -> menu "accesso anonimo" [("interrogazioni", interrogazioni)
						,("creazione nuove chiavi di responsable", bootChiavi)
						]
				Just q@((u,_),_) -> rotonda $ \k -> do
					rmenu (\x -> salvaPatch q >> k x) ("accesso secondo " ++ u) [
						("eventi" , rmenu return "produzione eventi" $ 
							[("votazioni",votazioni q)
							,("economia",economia q)
							,("anagrafe",anagrafica q)
							,("correzione",eliminazioneEvento q)
							]),
						("interrogazione", caricando q interrogazioni),
						("amministrazione",amministrazione q k )
						]


-- | comunica che c'è un errore logico nella richiesta
bocciato :: String -> Interfaccia ()
bocciato x =  P.errore . Response $ [("Incoerenza", ResponseOne x)] 

-- | semplifica il running dei Supporto
conStato :: (String -> Interfaccia a) -> (b -> Interfaccia a) -> Supporto Env TS () b -> Interfaccia a
conStato x y z = do	(s,_) <- letturaStato
			runSupporto s x y z

type Accesso = (Responsabile, Supporto Env TS () (Firmante TS))
-- | prova a fare accedere un responsabile. ritorna Nothing segnalando accesso anonimo
accesso :: Interfaccia  (Maybe Accesso)
accesso = rotonda $ \k -> conStato bocciato k login 

bootGruppo :: [Responsabile] -> Interfaccia ()
bootGruppo xs = do
	P.output . ResponseMany $ map fst xs
	menu "inserimento responsabili iniziali" $ 
		[("caricamento altra chiave responsabile", P.upload "chiave" >>= \x -> bootGruppo (x:xs))
		,("fine caricamento chiavi", asks writeStato >>= liftIO . ($ nuovoStato xs))
		]

bootChiavi :: Interfaccia ()
bootChiavi = do
	u <- P.libero "scegli il tuo nome di utente"
	p <- P.libero "immetti una password, una frase , lunga almeno 12 caratteri"
	P.download (u ++ ".chiavi") (u,cryptobox p)

letturaStato :: Interfaccia QS
letturaStato = asks readStato >>= fmap fromJust . liftIO 

interrogazioni :: Interfaccia ()
interrogazioni = rotonda $ \k -> do
	(s,_) <- letturaStato
	rmenu k "interrogazione stato del gruppo" . concatMap ($ bocciato) $ [
		costrQueryAccredito s P.output,
		costrQueryAnagrafe s P.output,
		costrQueryOrdine s P.output,
		costrQueryAssenso s P.output,
		costrQueryImpegni s P.output
		]

letturaEventi :: Utente -> Interfaccia [Evento]
letturaEventi u = asks readEventi >>= liftIO . ($ u) 

svuotaEventi u = asks writeEventi >>= \e -> liftIO (e u [])

salvaPatch  :: Accesso -> Interfaccia ()
salvaPatch q@((u,_),sf) = do
	(s,_) <- letturaStato
	evs <- letturaEventi u
	let salvataggio = do
		p <- asks  writeUPatch 
		runSupporto s bocciato (\(Firmante f) -> liftIO (p u $ f s evs)) sf
	when (not $ null evs) . menu "trattamento eventi in sospeso"  $
		[("firma",salvataggio),("mantieni",return ()),("elimina", svuotaEventi u), ("scarica eventi", P.download "eventi.txt" evs)]

caricando :: Accesso -> Interfaccia a -> Interfaccia a
caricando q@((u,_),_) k = do 
	evs <- letturaEventi u  
	s <- letturaStato
	if null evs then k  else  do 
		let c = Just . fst . caricamento (map ((,) u) evs) . fromJust  --buggy, dove sono i logs ?
		local (\f -> f {readStato = c <$> readStato f} ) k 

eliminazioneEvento :: Accesso -> Interfaccia ()
eliminazioneEvento ((u,_),_) = do
	es <- letturaEventi u
	if null es then bocciato "non ci sono eventi da eliminare"
		else do 
			x <- P.scelte (zip es es) "seleziona evento da eliminare"
			correzioneEventi u $ delete x es


-- correzioneEventi  :: Show a => a -> Interfaccia ()
correzioneEventi u evs  = asks writeEventi >>= \f -> liftIO $ f u evs

addEvento u x = letturaEventi u >>= \evs -> correzioneEventi u (show x:evs)

anagrafica :: Accesso -> Interfaccia ()
anagrafica q@((u,_),_) = rotonda $ \k -> caricando q $ do
	(s,_) <- letturaStato
	rmenu k "anagrafe" . concatMap ($ bocciato) $ [
		costrEventiResponsabili s (addEvento u),
		costrEventiAnagrafe s (addEvento u)
		]


economia  :: Accesso -> Interfaccia () 
economia q@((u,_),_) = rotonda $ \k -> caricando q $ do
	(s,_) <- letturaStato
	rmenu k "economia" . concatMap ($ bocciato) $ [
		costrEventiAccredito s (addEvento u),
		costrEventiImpegno s (addEvento u) ,
		costrEventiOrdine s (addEvento u)
		]

votazioni :: Accesso -> Interfaccia ()
votazioni q@((u,_),_) = rotonda $ \k -> caricando q $ do
	n <- conStato (const $ return 0) (return . length) $ assensiFiltrati u
	(s,_) <- letturaStato
	rmenu k ("votazioni (" ++ show n ++ " votazioni in attesa)") . concatMap ($ bocciato) $ [
		costrEventiAssenso u s (addEvento u)
		]

-- amministrazione :: (Responsabile,Firmante) -> Interfaccia ()
amministrazione q@(_,sf) k = do
	let	sincronizza = do
			p <- asks writeGPatch 
			rs <- asks readUPatches >>= liftIO
			(s,_) <- letturaStato
			case rs of 
				[] -> bocciato $ "nessun aggiornamento individale per lo stato attuale"
				xs -> runSupporto s bocciato (\(Firmante f) -> liftIO (p $ f s xs)) sf
	menu "amministrazione" $ [
			("firma un aggiornamento di gruppo (pericoloso)", sincronizza)
			]


