module Core.Applicazione where

import Control.Monad.Error (runErrorT)
import Control.Monad.Writer (Writer, tell)
import Control.Monad.Cont
import Control.Arrow
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent.STM
import Control.Concurrent
import Debug.Trace

import Lib.Passo
-- import Lib.Console
-- import Lib.HTTP
import Lib.TreeLogs
import Lib.Prioriti
import Lib.Aspetti (seeset)

import Core.Patch (fromGroup, Group)
import Core.Persistenza
-- import Core.UI

import Eventi.Anagrafe (Utente)

import Core.Types (Esterno, Evento)
import Core.Controllo (caricaEventi, SNodo (..))
import Core.Contesto (flatten)
import Core.Programmazione (Reazione)
import Core.Parsing (ParserConRead)


import Eventi.Anagrafe
import Eventi.Accredito
import Eventi.Impegno
import Eventi.Logger
import Eventi.Acquisto



-- | il tipo dello stato accessibile
type TS = TyAnagrafe (TyAccredito (TyImpegni (TyAcquisti Integer)))

-- |tipo dello stato con la serializzazione dei reattori
type QS = (TS,[SNodo TS Utente])

-- | lista di prioritizzatori, definiscono un riordinamento tra gli eventidi una patch
priorita :: [Lib.Prioriti.R]
priorita = [ priorityAnagrafe, priorityAnagrafeI, priorityAccredito
		, priorityImpegnoI, priorityImpegno, priorityAcquisto, priorityAssenso] 

-- | lista di reattori. I reattori di base per gli eventi
reattori :: [Reazione TS ParserConRead Utente]
reattori = [reazioneLogger, reazioneAnagrafe, reazioneAccredito, reazioneAcquisto] 


-- | effettua un inserimento di eventi esterni nello stato, restituendo il nuovo. Stampa i logs
caricamento :: Int -> [Esterno Utente] -> QS -> (QS,String)
caricamento l es = second (eccoILogs . map (first flatten)) . caricaEventi priorita reattori l es 

-- | creazione di un novo stato di tipo QS
nuovoStato :: [Responsabile] -> QS
nuovoStato rs = (bootAnagrafe rs  . bootAccredito . bootImpegni . bootAcquisti  $ 0, replicate (length reattori) $ SNodo True [])

sortEventi :: [Evento] -> [Evento]
sortEventi = sortP 100 priorita id

levelsEventi :: [Evento] -> [(Evento,Int)]
levelsEventi = levelsP  priorita id

loader ::  QS -> Group -> Writer [String] (Either String QS)
loader (qs@(s,_)) g = runErrorT $ do
			(_,es) <- runReaderT (fromGroup g) s
			let (qs',ef) = caricamento 100 es qs 
			tell [ef]
			return $ first (seeset ((+) 1 :: Integer -> Integer)) qs'

