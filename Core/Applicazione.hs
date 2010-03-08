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


-- | effettua un inserimento di eventi esterni nello stato, restituendo il nuovo. Stampa i logs
caricamento :: [Esterno Utente] -> QS -> (QS,String)
caricamento es = trace "caricamento" $ second (eccoILogs . map (first flatten)) . caricaEventi priorita reattori es 

-- | creazione di un novo stato di tipo QS
nuovoStato :: [Responsabile] -> QS
nuovoStato rs = (bootAnagrafe rs  . bootAccredito . bootImpegni . bootOrdini $ (), replicate (length reattori) $ SNodo True [])

sortEventi :: [Evento] -> [Evento]
sortEventi = sortP priorita id

loader ::  QS -> Group -> Writer [String] (Either String QS)
loader (qs@(s,_)) g = runErrorT $ do
			(_,es) <- runReaderT (fromGroup g) s
			let (qs',ef) = caricamento es qs
			tell [ef]
			return qs'

