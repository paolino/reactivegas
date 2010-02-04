{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, NoMonomorphismRestriction #-}

import System.Random
import Codec.Crypto.RSA
import Control.Arrow
import Control.Applicative
import Lib.Console
import Lib.Passo (Costruzione,menu) 
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Reader
import System.Exit
import Data.IORef

import System.Console.Haskeline (runInputT, defaultSettings)


import Data.Maybe
import Lib.Response
import Core.Controllo
import Core.Contesto
import Core.Inserimento
import Core.Programmazione
import Core.Parsing
import Core.Patch
import Core.File
import Core.Costruzione
import Lib.TreeLogs
import Lib.Firmabile

import Eventi.Amministrazione
import Eventi.Anagrafe
import Eventi.Accredito
import Eventi.Impegno
import Eventi.Ordine
import Eventi.Sincronizzatore

----------------- hacking for a boot state ---------------
s0 rs s = bootAnagrafe rs  . bootAccredito . bootImpegni . bootOrdini . bootSincronizzatore s $ ()
------------------------------------------

-- | il tipo dello stato generale
type TS = TyAnagrafe (TyAccredito (TyImpegni (TyOrdini (TySincronizzatore ()))))

-- | lista di prioritizzatori
ps = [priorityAnagrafe, priorityAnagrafeI, priorityAccredito, priorityImpegnoI, priorityImpegno, priorityOrdine, priorityAssenso] 

-- | lista di reattori
rs = [reazioneAnagrafe, reazioneAccredito, reazioneOrdine, reazioneSincronizzatore] :: [Reazione TS ParserConRead Utente ]

type Eventi = [String]
type Logs = String

data Patching
	= forall a. Show a => Evento a 
	| Bocciato String 
	| Sfronda Eventi
	| FinePatch

type Env m = ReaderT (TS,[SNodo TS Utente]) (StateT (Maybe Utente,[String]) m)
type Interface m a = Costruzione (Env m) () a

bocciato :: MonadIO m => Interface m a -> String -> Interface m a
bocciato f x =  liftIO (putStrLn ("*** Incoerenza: " ++ x) >> getLine) >> f

forzaUtente :: MonadIO m => Interface m Bool
forzaUtente = do 
	(u,_) <- get
	q@(s',_) <- ask
	case u of 
		Nothing -> runSupporto s' (bocciato (return False)) (\u -> (modify . first . const . Just $ u) >> return True) $ do
				(us :: [Utente]) <- map fst <$> costrResponsabili
				scelte (zip us us) "il nome dell'autore degli eventi"
		Just _ -> return True


patching  :: MonadIO m => Interface m () 
patching = do
	t <- forzaUtente
	when True $ do 	
		(Just u,evs) <- get 	
		((s,_),logs) <- caricaEventi ps rs (map ((,) u) evs) <$> ask
		liftIO $ do 	putStrLn "\n********** Caricamento **************"
				putStrLn $ eccoILogs $ map (first flatten) logs
				putStrLn "*************************************\n"

		let	nuovoevento x = modify (second $ (++) [show x])  >> patching	
			sfronda xs = modify (second $ const xs) >> patching

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
	q@(s,_) <- ask
	let 	response x = liftIO (print x >> getLine) >> interrogazioni
	menu "interrogazione stato del gruppo" . (("fine interrogazione",return ()):) . concatMap ($ bocciato interrogazioni) $ [
		costrQueryAccredito s response,
		costrQueryAnagrafe s response,
		costrQueryOrdine s response,
		costrQueryAssenso s response,
		costrQueryImpegni s response
		]
salvataggio = do
	(u,evs) <- get
	case u of 
		Nothing -> return ()
		Just u -> do 
			(s,_) <- ask
			pa <- runSupporto s (bocciato (return Nothing)) (return . Just) $ mkPatch u evs
			case pa of
				Nothing -> return ()
				Just pa -> liftIO $ writeFile ("aggiornamento." ++ u) (show pa) 

salvaPatch = do
	(u,evs) <- get
	when (not $ null evs) $ menu "vuoi firmare l'aggiornamento che hai prodotto?" $ [("si",salvataggio),("no",return ())]
 	
mainmenu :: MonadIO m => Interface m ()
mainmenu = do
	menu "reactivegas (2010)" $ [
		("fine programma",salvaPatch),
		("interrogazione dello stato",interrogazioni >> mainmenu),
		("produzione eventi",patching >> mainmenu)
		]
ts0 = s0 [("paolino",cryptobox "gaien")] ("sincronizzatore",cryptobox "desinc")
prova = runStateT (runReaderT (interazione () mainmenu) (ts0,repeat $ SNodo True [] )) (Nothing,[])
{-
main =	do
	let 	paolino = ("paolino",cryptobox "gaien")
		sinc = ("sincronizzatore",cryptobox "desinc")
	let 	machine ns = do
			evs <- readEvents
			((s1,_),logs) <- caricaEventi ps rs (map ((,) "paolino") evs) (s0 [paolino] sinc ,ns) 
			l <- runInputT defaultSettings 
				. runPasso . (:[Costruito Fine]) 
				. flip runCont Costruito . callCC $ \k -> do
					let csl = cs k s1
					join $ scelte csl "Cliente reactivegas (2010)"
					return $ Bocciato "errore interno"
			case l of 
				Fine -> return ()
				Report x -> print x >> getLine >> machine  ns
				Bocciato x -> putStrLn ("*** Incoerenza: " ++ x) >> getLine >> machine ns
				Evento x -> writeEvents (evs ++ [show x]) >> machine  ns
	machine $ repeat (SNodo True []) 
-}
