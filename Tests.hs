{-# LANGUAGE ExistentialQuantification, NoMonomorphismRestriction #-}

import System.Random
import Codec.Crypto.RSA
import Control.Arrow
import Lib.Console
import Lib.Costruzione
import Control.Monad.Cont
import Control.Monad.State
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

-- | costruzione aggiornamento
cs :: TS -> [String] -> (BoxPatch -> Costruzione a ()) -> [(String, Costruzione a ())]
cs s evs k = concat $ [
	costrEventiAnagrafe s (k . Evento) (k . Bocciato),
	costrEventiAssenso s (k . Evento) (k . Bocciato),
	costrEventiAccredito s (k . Evento) (k . Bocciato),
	costrEventiImpegno s (k . Evento) (k . Bocciato),
	costrEventiOrdine s (k . Evento) (k . Bocciato),
	costrEventiSincronizzatore s (k . Evento) (k . Bocciato),
	costrGestionePatch evs () (k . Elimina) (k . Bocciato)
	]

treat r (Evento x) = modify (++ [show x])  >> r
treat r (Bocciato x) = liftIO (putStrLn ("*** Incoerenza: " ++ x) >> getLine) >> r
treat r (Elimina xs) = put xs >> r
treat _ FinePatch = return ()

costruzionePatch ts = do
	(u,_) <- ask
	let ((ts1,_),logs) <- caricaEventi ps rs (map ((,) "paolino") evs) (s0 [paolino] sinc ,ns)
	evs <- get
	e <- lift $ runMenu "produzione eventi" FinePatch (cs ts evs)
	treat (costruzionePatch ts) e

runMenu x def q = runInputT defaultSettings . runPasso . (:[Costruito def]) . flip runCont Costruito . callCC $ \k -> do
		join . scelte (q k) $ x
		error "fine inaspettata"

data BoxPatch
	= forall a. Show a => Evento a 
	| Bocciato String 
	| Elimina [String]
	| FinePatch

ts0 = s0 [("paolino",cryptobox "gaien")] ("sincronizzatore",cryptobox "desinc")
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
