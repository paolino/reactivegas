module  Applicazioni.Reactivegas where

import Control.Monad.Error (runErrorT)
import Control.Monad.Writer (Writer, tell)
import Control.Monad.Reader (runReaderT)
import Control.Arrow (second, first)

import Lib.TreeLogs (eccoILogs)
import Lib.Prioriti (R, sortP, levelsP)
import Lib.Aspetti (seeset, see)
import Lib.Response (Response(ResponseMany,ResponseOne))

import Core.Patch (fromGroup, Group)

import Core.Types (Esterno, Evento, Utente, Responsabile)
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


-- | creazione di un novo stato di tipo QS
nuovoStato :: [Responsabile] -> QS
nuovoStato rs = (bootAnagrafe rs  . bootAccredito . bootImpegni . bootAcquisti  $ 0, replicate (length reattori) $ SNodo True [])

maxLevel = 100
sortEventi :: [Evento] -> [Evento]
sortEventi = sortP maxLevel priorita id

levelsEventi :: [Evento] -> [(Evento,Int)]
levelsEventi = levelsP  priorita id

caricamento' l es = second (eccoILogs . map (first flatten)) . caricaEventi priorita reattori l es

loader ::  QS -> Group -> Writer [String] (Either String QS)
loader (qs@(s,_)) g = runErrorT $ do
			(_,es) <- runReaderT (fromGroup (fst . responsabili) g) s
			let (qs',ef) = caricamento' maxLevel es qs 
			tell [ef]
			return $ first (seeset ((+) 1 :: Integer -> Integer)) qs'

-- | effettua un inserimento di eventi esterni nello stato, restituendo il nuovo. Stampa i logs
caricamento :: Int -> QS -> [Esterno Utente] -> (QS,Response)
caricamento l s es = let
	(s',qs) = caricamento' l es $ s
	qs' = ResponseMany (map ResponseOne $ lines qs)
	in (s',qs')

