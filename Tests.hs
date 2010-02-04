{-# LANGUAGE ExistentialQuantification, NoMonomorphismRestriction #-}

import System.Random
import Codec.Crypto.RSA
import Control.Arrow
import Lib.Console
import Lib.Passo
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Reader
import System.Exit
import Data.IORef

import System.Console.Haskeline (runInputT, defaultSettings)


import Lib.Response
import Core.Controllo
import Core.Contesto
import Core.Inserimento
import Core.Programmazione
import Core.Parsing
import Core.File
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

type Env m = ReaderT TS (StateT [String] m)
type Menu m = Costruzione (Env m) () ()

bocciato f x =  liftIO (putStrLn ("*** Incoerenza: " ++ x) >> getLine) >> f

patching  :: MonadIO m => Menu m 
patching = do
	evs <- get
	s <- ask
	let 	nuovoevento x = modify (++ [show x])  >> patching	
		sfronda xs = put xs >> patching

	join . flip scelte "produzione eventi" . (("fine produzione eventi",return ()):) . concatMap ($bocciato patching) $ [
		costrEventiAnagrafe s nuovoevento ,
		costrEventiAssenso s nuovoevento ,
		costrEventiAccredito s nuovoevento ,
		costrEventiImpegno s nuovoevento ,
		costrEventiOrdine s nuovoevento ,
		costrEventiSincronizzatore s nuovoevento ,
		costrGestionePatch evs () sfronda 
		]

interrogazioni :: MonadIO m => Menu m 
interrogazioni = do
	s <- ask
	let 	response x = liftIO (print x >> getLine) >> interrogazioni
	join . flip scelte "interrogazione stato del gruppo" . (("fine interrogazione",return ()):) . concatMap ($ bocciato interrogazioni) $ [
		costrQueryAccredito s response,
		costrQueryAnagrafe s response,
		costrQueryOrdine s response,
		costrQueryAssenso s response,
		costrQueryImpegni s response
		]
	
mainmenu :: MonadIO m => Menu m 
mainmenu = do
	join . flip scelte "reactivegas (2010)" $ [
		("fine programma",return ()),
		("interrogazione dello stato",interrogazioni >> mainmenu),
		("produzione eventi",patching >> mainmenu)
		]
ts0 = s0 [("paolino",cryptobox "gaien")] ("sincronizzatore",cryptobox "desinc")
prova = runStateT (runReaderT (interazione () mainmenu) ts0 ) []
{-
main =	do
	let 	paolino = ("paolino",cryptobox "gaien")
		sinc = ("sincronizzatore",cryptobox "desinc")
	let 	machine ns = do
			evs <- readEvents
			((s1,_),logs) <- caricaEventi ps rs (map ((,) "paolino") evs) (s0 [paolino] sinc ,ns) 
			putStrLn "\n********** Caricamento **************"
			putStrLn $ eccoILogs $ map (first flatten) logs
			putStrLn "*************************************\n"
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
