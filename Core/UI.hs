
{-# LANGUAGE FlexibleContexts, ExistentialQuantification, ScopedTypeVariables, GeneralizedNewtypeDeriving, NoMonomorphismRestriction #-}
module Core.UI where

import Data.Maybe (fromJust)

import Control.Arrow
import Control.Applicative
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import System.Console.Haskeline (MonadException)
import Debug.Trace


import Lib.Response
import Core.Types
import Core.Controllo
import Core.Contesto (flatten)
import Core.Programmazione
import Core.Parsing (ParserConRead)
import Core.Patch
import Core.Costruzione
import Core.Persistenza

import Lib.Passo (Costruzione,menu, rotonda ,rmenu) 
import qualified Lib.Passo as P
import Lib.TreeLogs
import Lib.Firmabile
import Lib.Prioriti (R)

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
type Env = ReaderT (Aggiornamento QS) (StateT [String] IO)
type Interfaccia = Costruzione Env () 

-- | comunica che c'è un errore logico nella richiesta
bocciato :: String -> Interfaccia ()
bocciato x =  P.output (ResponseOne $ "Incoerenza!: " ++ x) 

-- | semplifica il running dei Supporto
conStato :: (String -> Interfaccia a) -> (b -> Interfaccia a) -> Supporto Env TS () b -> Interfaccia a
conStato x y z = do	(s,_) <- asks readStato 
			runSupporto s x y z
-- | prova a fare accedere un responsabile. ritorna Nothing segnalando accesso anonimo
accesso :: Interfaccia  (Maybe (Responsabile, Firmante))
accesso = rotonda $ \k -> conStato bocciato k login 

bootChiavi :: Interfaccia ()
bootChiavi = do
	q <- asks writeChiavi
	u <- P.libero "scegli il tuo nome di utente"
	p <- P.libero "immetti una password, una frase , lunga almeno 12 caratteri"
	liftIO $ q (u,cryptobox p)

boot :: Interfaccia () 
boot = menu "amministrazione" $ [
			("creazione chiavi per un nuovo responsabile" ,bootChiavi ),
			("creazione nuovo stato di gruppo", asks writeStato >>= liftIO . ($nuovoStato))
			]


chiavi :: (String,Interfaccia ())
chiavi = (,) "creazione chiavi per un nuovo responsabile" $ do
	sc <- asks writeChiavi 
	conStato bocciato (liftIO . sc) nuoveChiavi

interrogazioni :: Interfaccia ()
interrogazioni = rotonda $ \k -> do
	(s,_) <- asks readStato 
	rmenu k "interrogazione stato del gruppo" . concatMap ($ bocciato) $ [
		costrQueryAccredito s P.output,
		costrQueryAnagrafe s P.output,
		costrQueryOrdine s P.output,
		costrQueryAssenso s P.output,
		costrQueryImpegni s P.output
		]
svuotaEventi = asks writeEventi >>= \e -> liftIO (e [])
salvaPatch  :: (Responsabile,Firmante) -> Interfaccia ()
salvaPatch q@((u,_), Firmante f) = caricando q $ do
	evs <- get 
	let salvataggio = do
		p <- asks  writeUPatch 
		liftIO $ p u (f evs)
	when (not $ null evs) . menu "trattamento eventi in sospeso"  $
		[("firma",salvataggio >> svuotaEventi),("mantieni",return ()),("elimina", svuotaEventi)]

caricando :: (Responsabile,Firmante) -> Interfaccia a -> Interfaccia a
caricando q@((u,_),Firmante f) k = do 
	evs <- get
	s <- asks readStato
	if null evs then k else  do 
			pe <- asks writeEventi 
			liftIO $ pe evs
			let (s',_) = caricamento (map ((,) u) evs)  s --buggy, dove sono i logs ?
			local (\f -> f {readStato = s'}) k


nuovoevento :: Show a => a -> Interfaccia ()
nuovoevento x = modify (++ [show x]) 

gestione evs = costrGestionePatch evs () $ modify . const

anagrafica :: (Responsabile,Firmante) -> Interfaccia ()
anagrafica q = rotonda $ \k -> caricando q $ do
	evs <- get
	(s,_) <- asks readStato
	rmenu k "anagrafe" . concatMap ($ bocciato) $ [
		costrEventiResponsabili s nuovoevento,
		costrEventiAnagrafe s nuovoevento,
		gestione evs
		]


economia  ::(Responsabile,Firmante) -> Interfaccia () 
economia q@((u,_),_) = rotonda $ \k -> caricando q $ do
	evs <- get
	(s,_) <- asks readStato
	rmenu k "economia" . concatMap ($ bocciato) $ [
		costrEventiAccredito s nuovoevento,
		costrEventiImpegno s nuovoevento ,
		costrEventiOrdine s nuovoevento ,
		gestione evs
		]

votazioni ::(Responsabile,Firmante) -> Interfaccia ()
votazioni q@((u,_),_) = rotonda $ \k -> caricando q $ do
	n <- conStato (const $ return 0) (return . length) $ assensiFiltrati u
	(s,_) <- asks readStato
	evs <- get
	rmenu k ("votazioni (" ++ show n ++ " votazioni in attesa)") . concatMap ($ bocciato) $ [
		costrEventiAssenso u s nuovoevento,
		gestione evs
		]

-- amministrazione :: (Responsabile,Firmante) -> Interfaccia ()
amministrazione q@(_,Firmante f) k = do
	let	sincronizza = do
			p <- asks writeGPatch 
			case p of 
				Nothing -> bocciato $ "nessun aggiornamento individale per lo stato attuale"
				Just publ -> liftIO (publ f) >>  k ()
	menu "amministrazione" $ [
			second (caricando q) chiavi,
			("firma un aggiornamento di gruppo (pericoloso)", sincronizza)
			]

flow :: Interfaccia ()
flow = accesso >>= \t -> case t of 
		Just q@((u,_),_) -> rotonda $ \k -> do
			rmenu (\x -> salvaPatch q >> k x) ("radice (" ++ u ++ ")") [
				("votazioni",votazioni q),
				("economia",economia q),
				("anagrafe",anagrafica q),
				("interrogazione", caricando q interrogazioni),
				("amministrazione",amministrazione q k )
				]
		Nothing ->  menu "radice (anonimo)" $ [("interrogazione", interrogazioni),chiavi]



