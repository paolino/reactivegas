{-# LANGUAGE ExistentialQuantification, NoMonomorphismRestriction #-}

import System.Random
import Codec.Crypto.RSA
import Control.Arrow
import Lib.Console
import Lib.Costruzione
import Control.Monad.Cont
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

import Eventi.Anagrafe
import Eventi.Accredito
import Eventi.Impegno
import Eventi.Ordine

----------------- hacking for a boot state ---------------
(pu,pr,_) = generateKeyPair (mkStdGen 0) 512
s0 = bootAnagrafe [("paolino",pu)]  . bootAccredito . bootImpegni . bootOrdini $ ()
------------------------------------------

-- | il tipo dello stato generale
type TS = TyAnagrafe (TyAccredito (TyImpegni (TyOrdini ())))

-- | lista di prioritizzatori
ps = [priorityAnagrafe, priorityAnagrafeI, priorityAccredito, priorityImpegnoI, priorityImpegno, priorityOrdine, priorityAssenso] 

-- | lista di reattori
rs = [reazioneAnagrafe, reazioneAccredito, reazioneOrdine] :: [Reazione TS ParserConRead Utente ]

-- | crea i costruttori
cs :: (Box -> Costruzione a ()) -> TS -> [(String, Costruzione a ())]
cs k s = concat $ [
	costrEventiAnagrafe s (k . Evento) (k . Bocciato),
	costrEventiAssenso s (k . Evento) (k . Bocciato),
	costrEventiAccredito s (k . Evento) (k . Bocciato),
	costrEventiImpegno s (k . Evento) (k . Bocciato),
	costrEventiOrdine s (k . Evento) (k . Bocciato),
	costrQueryAnagrafe s (k . Report) (k . Bocciato),
	costrQueryAssenso s (k . Report) (k. Bocciato),
	costrQueryAccredito s (k . Report) (k . Bocciato),
	costrQueryImpegni s (k . Report) (k . Bocciato),
	costrQueryOrdine s (k . Report) (k . Bocciato)
	]

data Box 
	= forall a. Show a => Evento a 
	| Bocciato String 
	| Report Response 
	| Elimina Int	
	| Fine

main =	machine $ repeat (SNodo True []) 

machine ns = do
		evs <- readEvents
		((s1,_),logs) <- caricaEventi ps rs (map ((,) "paolino") evs) (s0,ns) 
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
