{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, TupleSections, OverlappingInstances, NoMonomorphismRestriction, FlexibleContexts #-}
module  Applicazioni.Reactivegas where

import Data.Function (on)
import Data.List (nubBy)

import Control.Applicative ((<$>))
import Control.Monad.Error (runErrorT, lift)
import Control.Monad.Reader (runReader)
import Control.Arrow (second, first, (***))

import Lib.TreeLogs (eccoILogs)
import Lib.Prioriti (R, sortP, levelsP)
import Lib.Aspetti (seeset, see)
import Lib.Response (Response(ResponseMany,ResponseOne))

import Core.Patch (fromGroup, Group,Patch)

import Core.Types (Esterno, Evento, Utente, Responsabile)
import Core.Controllo -- (caricaEventi, SNodo (..),amendSNodo, nodoVuoto )
import Core.Nodo
import Core.Inserimento (UString (..))
import Core.Contesto (flatten, esterno)
import Core.Programmazione (Fallimento (..), Reazione, estrai, Message, lascia)
import Core.Parsing (ParserConRead)
import Core.Contesto (Contestualizzato)
import Lib.States

import Eventi.Anagrafe 
import Eventi.Accredito
import Eventi.Impegno
import Eventi.Logger
import Data.Time 
import Eventi.Acquisto
import Eventi.Voci
import Core.Dichiarazioni

esing = [
	Singola (error "using a witness" :: EsternoAcquisto), 
	Singola (error "using a witness" :: EsternoAccredito),
	Singola (error "using a witness" :: EsternoImpegno),
	Singola (error "using a witness" :: EsternoAnagrafico),
	Singola (error "using a witness" :: EsternoAssenso)
	]

-- ecompo = [Composta ([]::[EventoVoci]), Composta ([]::[EsternoImpegnoVincolato])]

mkDichiarazioni = parseDichiarazioni esing []
-- | il tipo dello stato accessibile
type TS' = TyAnagrafe (TyAccredito (TyImpegni (TyAcquisti Integer)))
type TS = TS'
-- |tipo dello stato con la serializzazione dei reattori
-- type QS = (TS,[SNodo TS  Utente])

newtype QS = QS {unQS :: (TS,[Nodo TS ParserConRead Utente])} 


instance Show QS where
	show (QS (ts,ns)) = show (ts,map serializza ns)

instance Read QS where
	readsPrec t x = map (first ( QS . second y)) $ readsPrec t x where
		y nss = case sequence . map (uncurry deserializza) . zip nss $ reattori of
			Nothing -> error $ "deserializzazione fallita" 
			Just ns ->  ns
-- | lista di prioritizzatori, definiscono un riordinamento tra gli eventidi una patch
priorita :: [Lib.Prioriti.R]
priorita = [ priorityAnagrafe, priorityAnagrafeI, priorityAccredito
		,  priorityImpegno, priorityAcquisto, priorityAssenso, {-priorityImpegnoVincolato,-} priorityEventoVoci] 

-- | lista di reattori. I reattori di base per gli eventi
reattori :: [Reazione TS ParserConRead Utente]
reattori = [reazioneLogger, reazioneAnagrafe, reazioneAccredito, reazioneAcquisto] 


-- | creazione di un novo stato di tipo QS
nuovoStato :: [Responsabile] -> QS
nuovoStato rs = QS $ (bootAnagrafe rs  . bootAccredito . bootImpegni . bootAcquisti  $ 0, map (\r -> Nodo (Just r) []) reattori)

maxLevel = 100

sortEventi :: [Evento] -> [Evento]
sortEventi = sortP maxLevel priorita id

levelsEventi :: [Evento] -> [(Evento,Int)]
levelsEventi = levelsP  priorita id

filtroMovimenti :: Effetti -> ([Movimento],Effetti)
filtroMovimenti = estrai 

type Effetti = [Contestualizzato Utente Message]

caricamento'' :: Int -> [Esterno Utente] -> QS -> (QS,Effetti)
caricamento'' l es (QS q) = let (q',ef) = (fst q == fst q) `seq` caricaEventi' priorita  l es q
	in  (QS q',ef)

-- | aggiornamento di gruppo
loader :: QS -> [Esterno Utente] -> Either String (QS,Effetti)
loader (qs@(QS (s,_))) es = flip runReader s . runErrorT  $
			return . first (\(QS q) -> QS . first (seeset ((+) 1 :: Integer -> Integer)) $ q) $ caricamento'' maxLevel es qs

-- | effettua un inserimento di eventi esterni nello stato, restituendo il nuovo. Stampa i logs
bianco :: Int -> QS -> [Esterno Utente] -> (QS,Response)
bianco l s es = let
	(s',qs) = second (eccoILogs . map (first flatten) . (\r -> lascia (UString "") r 
		++ nubBy ((==) `on` (esterno . fst)) (lascia (Fallimento (UString "")) r))) . caricamento'' l es $ s
	qs' = ResponseMany (map ResponseOne $ lines qs)
	in (s',qs')

