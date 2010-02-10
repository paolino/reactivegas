{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, GeneralizedNewtypeDeriving, NoMonomorphismRestriction #-}

import Data.Maybe (fromJust)

import Control.Arrow
import Control.Applicative
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import System.Console.Haskeline (MonadException)


import Lib.Response
import Core.Types
import Core.Controllo
import Core.Contesto (flatten)
import Core.Programmazione
import Core.Parsing (ParserConRead)
import Core.Patch
import Core.Costruzione
import Core.Aggiornamento

import Lib.Console
import Lib.Passo (Costruzione,menu) 
import qualified Lib.Passo as P
import Lib.TreeLogs
import Lib.Firmabile
import Lib.Prioriti (R)

import Eventi.Amministrazione
import Eventi.Anagrafe
import Eventi.Accredito
import Eventi.Impegno
import Eventi.Ordine


-- | il tipo dello stato generale
type TS = TyAnagrafe (TyAccredito (TyImpegni (TyOrdini ())))

-- |tipo dello stato con la serializzazione dei reattori
type QS = (TS,[SNodo TS Utente])

----------------- hacking for a boot state ---------------
nuovoStato :: [Responsabile] -> QS
nuovoStato rs = (bootAnagrafe rs  . bootAccredito . bootImpegni . bootOrdini $ (), replicate (length rs) $ SNodo True [])
------------------------------------------


-- | lista di prioritizzatori
priorita :: [Lib.Prioriti.R]
priorita = [priorityAnagrafe, priorityAnagrafeI, priorityAccredito, priorityImpegnoI, priorityImpegno, priorityOrdine, priorityAssenso] 

-- | lista di reattori
reattori :: [Reazione TS ParserConRead Utente]
reattori = [reazioneAnagrafe, reazioneAccredito, reazioneOrdine] :: [Reazione TS ParserConRead Utente]

caricamento :: MonadIO m => [Esterno Utente] -> QS -> m QS
caricamento es s = do  
	let (q,logs) = caricaEventi priorita reattori es s
	liftIO $ do 	putStrLn "\n********** Caricamento **************"
			putStrLn $ eccoILogs $ map (first flatten) logs
			putStrLn "*************************************\n"
	return q

type Env m = ReaderT (Aggiornamento QS) (StateT (Maybe Utente,[String]) m)

type Interface m = Costruzione (Env m) () 

bocciato :: MonadIO m => Interface m a -> String -> Interface m a
bocciato f x =  P.output (ResponseOne $ "Incoerenza!: " ++ x) >> f

conUtente :: MonadIO m => a -> (Utente -> Interface m a) -> Interface m a
conUtente q f = do 
	(u,_) <- get
	t <- case u of 
		Nothing -> do
				s <- fst <$> asks getStato
				runSupporto s 
					(bocciato (return False)) 
					(\w -> (modify . first . const . Just $ w) >> return True) $ do
						(us :: [Utente]) <- map fst <$> costrResponsabili
						scelte (zip us us) "il nome dell'autore degli eventi"
		Just _ -> return True
	if t then gets fst >>= f . fromJust
		else return q

caricamentoT :: MonadIO m => Costruzione (Env m) () QS
caricamentoT = do 
	(_,evs) <- get
	s <- asks getStato
	if null evs then return s else  
		conUtente s $ \u -> do 
			pe <- asks publishEventi 
			liftIO $ pe evs
			caricamento (map ((,) u) evs)  s

nuovoevento x = modify (second $ (++ [show x]) )  
sfronda xs = modify (second $ const xs) 

patching  :: MonadIO m => Interface m () 
patching = do
	(s,_) <- caricamentoT
	(_,evs) <- get
	let nv x = nuovoevento x >> patching
	menu "funzione di sportello" . (("indietro",return ()):) 
		. concatMap ($ bocciato patching) $ [
		costrEventiAnagrafe s nv ,
		costrEventiImpegno s nv ,
		costrEventiOrdine s nv ,
		costrGestionePatch evs () (\x -> sfronda x >> patching)
		]

democrazia :: MonadIO m => Interface m ()
democrazia = conUtente () $ \u -> do
	(s,_) <- caricamentoT
	(_,evs) <- get
	n <- runSupporto s (\_ -> return 0) (return . length) (assensiFiltrati u)
	menu ("espressione democratica (" ++ show n ++ " votazioni in attesa)") . (("indietro",return ()):) 
		. concatMap ($ bocciato democrazia) $ [
		costrEventiResponsabili s (\x -> nuovoevento x >> democrazia),
		costrEventiAssenso u s (\x -> nuovoevento x >> democrazia),
		costrGestionePatch evs () (\x -> sfronda x >> democrazia)
		]

	
interrogazioni :: MonadIO m => Interface m ()
interrogazioni = do
	let 	response x = P.output x >> interrogazioni
	(s,_) <- caricamentoT
	menu "interrogazione stato del gruppo" . (("indietro",return ()):) 
		. concatMap ($ bocciato interrogazioni) $ [
		costrQueryAccredito s response,
		costrQueryAnagrafe s response,
		costrQueryOrdine s response,
		costrQueryAssenso s response,
		costrQueryImpegni s response
		]

amministrazione :: MonadIO m => Interface m ()
amministrazione = do
	let	sincronizza = conUtente () $ \u -> do
			p <- asks publishGPatch 
			(s,_) <- asks getStato
			case p of 
				Nothing -> bocciato (return ()) $ "nessun aggiornamento per lo stato "
				Just publ -> do
					f <- runSupporto s  (bocciato $ return Nothing) (return . Just)  $ mkGroup u
					case f of 
						Nothing -> return ()
						Just f -> liftIO $ publ f
		chiavi = do
			(s,_) <- caricamentoT
			sc <- asks publishChiavi 
			runSupporto s (bocciato $ return ()) (liftIO . sc) nuoveChiavi
	menu "amministrazione" $ [
			("creazione chiavi per un nuovo responsabile" ,chiavi),
			("creazione aggiornamento di gruppo",sincronizza)
			]

salvaPatch :: MonadIO m => Interface m ()
salvaPatch = do
	evs <- gets snd
	let	salvataggio = conUtente () $ \u -> do
			(s,_) <- asks getStato
			mpa <- runSupporto s (bocciato $ salvaPatch >> return Nothing) (return . Just) $ mkPatch u evs
			case mpa of 
				Nothing -> return ()
				Just pa -> do 
					p <- asks  publishUPatch  
					liftIO $ p u pa
	when (not $ null evs) . menu "vuoi firmare l'aggiornamento prodotto?"  $
		[("si",salvataggio),("no",return ())]

mainmenu :: MonadIO m => Interface m ()
mainmenu = do
	menu "reactivegas (2010)" $ [
		("fine programma",salvaPatch),
		("interrogazioni",interrogazioni >> mainmenu),
		("espressione democratica",democrazia >> mainmenu),
		("funzione di sportello",patching >> mainmenu),
		("amministrazione",amministrazione >> mainmenu)
		]

bootChiavi :: MonadIO m => Interface m ()
bootChiavi = do
	q <- asks publishChiavi
	u <- P.libero "scegli il tuo nome di utente"
	p <- P.libero "immetti una password, una frase , lunga almeno 12 caratteri"
	liftIO $ q (u,cryptobox p)

boot :: MonadIO m => Interface m ()
boot = menu "amministrazione" $ [
			("creazione chiavi per un nuovo responsabile" ,bootChiavi ),
			("creazione nuovo stato di gruppo", asks publishStato >>= liftIO . ($nuovoStato))
			]

-- | supporto 
loader ::  QS -> Group -> IO (Utente,QS)
loader (qs@(s,_)) g = do 
		e <-  runErrorT $ do
			(u,es) <- runReaderT (fromGroup g) s
			qs' <- liftIO $ caricamento es qs
			return (u,qs')
		either error return e

run :: (MonadIO m, MonadException m) => Interface m () -> Aggiornamento QS -> [Evento] -> m ()
run m x evs = evalStateT (runReaderT (interazione () m) x) (Nothing, evs)

main :: IO ()
main = do
	mq <- aggiornamento Nothing loader
	case mq of
		b@(Boot _ _) -> run boot b []
		fl -> run mainmenu fl (getEventi fl)

