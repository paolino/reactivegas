{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, GeneralizedNewtypeDeriving, NoMonomorphismRestriction #-}

import System.Random
import Codec.Crypto.RSA
import Control.Arrow
import Control.Applicative
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import System.Exit
import Data.IORef
import System.FilePath

import System.Console.Haskeline (runInputT, defaultSettings)


import Data.Maybe
import Lib.Response
import Core.Types
import Core.Controllo
import Core.Contesto (flatten)
import Core.Inserimento
import Core.Programmazione
import Core.Parsing
import Core.Patch
import Core.Costruzione
import Core.Aggiornamento

import Lib.Console
import Lib.Passo (Costruzione,menu,svolgi) 
import qualified Lib.Passo as P
import Lib.TreeLogs
import Lib.Firmabile

import Eventi.Amministrazione
import Eventi.Anagrafe
import Eventi.Accredito
import Eventi.Impegno
import Eventi.Ordine
import Eventi.Sincronizzatore

----------------- hacking for a boot state ---------------
nuovoStato :: [Responsabile] -> Responsabile -> (TS,[SNodo TS Utente])
nuovoStato rs s = (bootAnagrafe rs  . bootAccredito . bootImpegni . bootOrdini . bootSincronizzatore s $ (),replicate (length rs) $ SNodo True [])
------------------------------------------

-- | il tipo dello stato generale
type TS = TyAnagrafe (TyAccredito (TyImpegni (TyOrdini (TySincronizzatore ()))))

-- | lista di prioritizzatori
ps = [priorityAnagrafe, priorityAnagrafeI, priorityAccredito, priorityImpegnoI, priorityImpegno, priorityOrdine, priorityAssenso] 

-- | lista di reattori
rs = [reazioneAnagrafe, reazioneAccredito, reazioneOrdine, reazioneSincronizzatore] :: [Reazione TS ParserConRead Utente ]

caricamento :: MonadIO m => [Esterno Utente] -> (TS,[SNodo TS Utente]) -> m (TS,[SNodo TS Utente])
caricamento es s = do  
	let (q,logs) = caricaEventi ps rs es s
	liftIO $ do 	putStrLn "\n********** Caricamento **************"
			putStrLn $ eccoILogs $ map (first flatten) logs
			putStrLn "*************************************\n"
	return q


type Eventi = [String]
type Logs = String

data Patching
	= forall a. Show a => Evento a 
	| Bocciato String 
	| Sfronda Eventi
	| FinePatch

type Env m = ReaderT (Aggiornamento (TS,[SNodo TS Utente])) (StateT (Maybe Utente,[String]) m)
type Interface m = Costruzione (Env m) () 

caricamentoT :: MonadIO m => Costruzione (Env m) () (TS,[SNodo TS Utente])
caricamentoT = do 
	(_,evs) <- get
	if (not $ null evs) then do 
		t <- forzaUtente
		if t then do 
			(Just u,evs) <- get 	
			s <- asks stato 
			caricamento (map ((,) u) evs)  s
			else asks stato
		else asks stato

bocciato :: MonadIO m => Interface m a -> String -> Interface m a
bocciato f x =  liftIO (putStrLn ("*** Incoerenza: " ++ x) >> getLine) >> f

forzaUtente :: MonadIO m => Interface m Bool
forzaUtente = do 
	(u,_) <- get
	(s',_) <- asks stato
	case u of 
		Nothing -> runSupporto s' (bocciato (return False)) (\u -> (modify . first . const . Just $ u) >> return True) $ do
				(us :: [Utente]) <- map fst <$> costrResponsabili
				scelte (zip us us) "il nome dell'autore degli eventi"
		Just _ -> return True


patching  :: MonadIO m => Interface m () 
patching = do
	let	nuovoevento x = modify (second $ (++ [show x]) )  >> patching	
		sfronda xs = modify (second $ const xs) >> patching
	(s,_) <- caricamentoT
	(_,evs) <- get
	menu "produzione eventi" . (("fine produzione eventi",return ()):) . concatMap ($bocciato patching) $ [
		costrEventiAnagrafe s nuovoevento ,
		costrEventiAssenso s nuovoevento ,
		costrEventiAccredito s nuovoevento ,
		costrEventiImpegno s nuovoevento ,
		costrEventiOrdine s nuovoevento ,
		costrEventiSincronizzatore s nuovoevento ,
		costrGestionePatch evs () sfronda 
		]

interrogazioni :: MonadIO m => Interface m ()
interrogazioni = do
	(s,_) <- caricamentoT
	(_,evs) <- get
	let 	response x = liftIO (print x >> getLine) >> interrogazioni -- buggy, i Response devoo essere gestiti in Passo !
	menu "interrogazione stato del gruppo" . (("fine interrogazione",return ()):) . concatMap ($ bocciato interrogazioni) $ [
		costrQueryAccredito s response,
		costrQueryAnagrafe s response,
		costrQueryOrdine s response,
		costrQueryAssenso s response,
		costrQueryImpegni s response
		]

salvataggio = do
	(Just u,evs) <- get
	(s,_) <- asks stato
	mpa <- runSupporto s (bocciato $ salvaPatch >> return Nothing) (return . Just) $ mkPatch u evs
	case mpa of 
		Nothing -> return ()
		Just pa -> do 
			p <- asks  publishUPatch  
			liftIO $ p u pa

salvaPatch = do
	(_,evs) <- get
	when (not $ null evs) $ do 
		t <- forzaUtente
		when t $ do 
			when (not $ null evs) $ menu "vuoi firmare l'aggiornamento che hai prodotto?" $ [("si",salvataggio),("no",return ())]
 	
sincronizza = do
	p <- asks publishGPatch 
	(s,_) <- asks stato
	case p of 
		Nothing -> bocciato (return ()) $ "nessun aggiornamento per lo stato "
		Just publ -> do
			f <- runSupporto s  (bocciato $ return Nothing) (return . Just)  $ mkGroup 
			case f of 
				Nothing -> return ()
				Just f -> liftIO $ publ f
chiavi = do
	(s,_) <- caricamentoT
	sc <- asks publishChiavi 
	runSupporto s (bocciato $ return ()) (liftIO . sc) nuoveChiavi

amministrazione = menu "amministrazione" $ [
			("creazione chiavi per un nuovo responsabile" ,chiavi),
			("creazione aggiornamento di gruppo",sincronizza)
			]

bootChiavi = do
	q <- asks publishChiavi
	u <- P.libero "scegli il tuo nome di utente"
	p <- P.libero "immetti una password, una frase , lunga almeno 12 caratteri"
	liftIO $ q (u,cryptobox p)

bootStato = do
	cs <- asks responsabiliBoot
	c <- P.scelte (map (fst &&& id) cs) "scegli il sincronizzatore"
	p <- asks  publishStato
	liftIO $ p (nuovoStato cs c)
	
	
boot = menu "amministrazione" $ [
			("creazione chiavi per un nuovo responsabile" ,bootChiavi ),
			("creazione nuovo stato di gruppo", bootStato)
			]

mainmenu :: MonadIO m => Interface m ()
mainmenu = do
	menu "reactivegas (2010)" $ [
		("fine programma",salvaPatch),
		("interrogazione dello stato",interrogazioni >> mainmenu),
		("produzione eventi",patching >> mainmenu),
		("amministrazione",amministrazione >> mainmenu)
		]

-- loader :: MonadIO m => (TS,[SNodo TS Utente]) -> Group -> ErrorT String m (TS,[SNodo TS Utente])
loader (ts@(s,_)) g = do 
	es <- runErrorT $ runReaderT (fromGroup g) s
	either error (flip caricamento ts) es

main :: IO ()
main = do
	mq <- aggiornamento Nothing loader
	case mq of
		b@(Boot _ _ _) -> evalStateT (runReaderT (interazione () boot) b) (Nothing,[])
		fl -> evalStateT (runReaderT (interazione () mainmenu) fl) (Nothing,[])

{-
ts0 = s0 [("paolino",cryptobox "gaien")] ("sincronizzatore",cryptobox "desinc")
-}
