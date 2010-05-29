module  Applicazioni.Reactivegas where

import Control.Applicative ((<$>))
import Control.Monad.Error (runErrorT, lift)
import Control.Monad.Reader (runReader)
import Control.Arrow (second, first)

import Lib.TreeLogs (eccoILogs)
import Lib.Prioriti (R, sortP, levelsP)
import Lib.Aspetti (seeset, see)
import Lib.Response (Response(ResponseMany,ResponseOne))

import Core.Patch (fromGroup, Group,Patch)

import Core.Types (Esterno, Evento, Utente, Responsabile)
import Core.Controllo (caricaEventi, SNodo (..), )
import Core.Inserimento (UString (..))
import Core.Contesto (flatten)
import Core.Programmazione (Reazione, estrai, Message, lascia)
import Core.Parsing (ParserConRead)
import Core.Contesto (Contestualizzato)


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

filtroMovimenti :: Effetti -> ([Movimento],Effetti)
filtroMovimenti = estrai 

type Effetti = [Contestualizzato Utente Message]

caricamento' :: Int -> [Esterno Utente] -> QS -> (QS,Effetti)
caricamento' = caricaEventi priorita reattori 


-- | aggiornamento di gruppo
loader :: QS -> [Esterno Utente] -> Either String (QS,Effetti)
loader (qs@(s,_)) es = flip runReader s . runErrorT $ do
			-- caricamento e aggiorna l'indice di stato
			return . first (first $ seeset ((+) 1 :: Integer -> Integer)) $ caricamento' maxLevel es qs

-- | effettua un inserimento di eventi esterni nello stato, restituendo il nuovo. Stampa i logs
bianco :: Int -> QS -> [Esterno Utente] -> (QS,Response)
bianco l s es = let
	(s',qs) = second (eccoILogs . map (first flatten) . lascia (UString "")) . caricamento' l es $ s
	qs' = ResponseMany (map ResponseOne $ lines qs)
	in (s',qs')

