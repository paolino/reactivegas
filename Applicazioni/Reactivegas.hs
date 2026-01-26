{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Applicazioni.Reactivegas where

import Data.Function (on)
import Data.List (nubBy)

import Control.Applicative ((<$>))
import Control.Arrow (first, second, (***))
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReader)
import Control.Monad.Trans (lift)

import Lib.Aspetti (see, seeset)
import Lib.Prioriti (R, levelsP, sortP)
import Lib.Response (Response (ResponseMany, ResponseOne))
import Lib.TreeLogs (eccoILogs)

import Core.Patch (Group, Patch, fromGroup)

-- (caricaEventi, SNodo (..),amendSNodo, nodoVuoto )

import Core.Contesto (Contestualizzato, esterno, flatten)
import Core.Controllo
import Core.Inserimento (UString (..))
import Core.Nodo
import Core.Parsing (ParserConRead)
import Core.Programmazione (Fallimento (..), Message, Reazione, estrai, lascia)
import Core.Types (Esterno, Evento, Responsabile, Utente)
import Lib.States

import Core.Dichiarazioni
import Data.Time
import Eventi.Accredito
import Eventi.Acquisto
import Eventi.Anagrafe
import Eventi.Impegno
import Eventi.Logger
import Eventi.Voci

esing =
    [ Singola (error "using a witness" :: EsternoAcquisto)
    , Singola (error "using a witness" :: EsternoAccredito)
    , Singola (error "using a witness" :: EsternoImpegno)
    , Singola (error "using a witness" :: EsternoAnagrafico)
    , Singola (error "using a witness" :: EsternoAssenso)
    ]

-- ecompo = [Composta ([]::[EventoVoci]), Composta ([]::[EsternoImpegnoVincolato])]

mkDichiarazioni = parseDichiarazioni esing []

-- | il tipo dello stato accessibile
type TS' = TyAnagrafe (TyAccredito (TyImpegni (TyAcquisti Integer)))

type TS = TS'

{- | tipo dello stato con la serializzazione dei reattori
 type QS = (TS,[SNodo TS  Utente])
-}
newtype QS = QS {unQS :: (TS, [Nodo TS ParserConRead Utente])}

instance Show QS where
    show (QS (ts, ns)) = show (ts, map serializza ns)

instance Read QS where
    readsPrec t x = map (first (QS . second y)) $ readsPrec t x
      where
        y nss = case mapM (uncurry deserializza) (zip nss reattori) of
            Nothing -> error "deserializzazione fallita"
            Just ns -> ns

-- | lista di prioritizzatori, definiscono un riordinamento tra gli eventidi una patch
priorita :: [Lib.Prioriti.R]
priorita =
    [ priorityAnagrafe
    , priorityAnagrafeI
    , priorityAccredito
    , priorityImpegno
    , priorityAcquisto
    , priorityAssenso {-priorityImpegnoVincolato,-}
    , priorityEventoVoci
    ]

-- | lista di reattori. I reattori di base per gli eventi
reattori :: [Reazione TS ParserConRead Utente]
reattori = [reazioneLogger, reazioneAnagrafe, reazioneAccredito, reazioneAcquisto]

-- | Create a new state of type QS
nuovoStato :: [Responsabile] -> QS
nuovoStato rs =
    QS
        ( bootAnagrafe rs . bootAccredito . bootImpegni . bootAcquisti $ 0
        , map (\r -> Nodo (Just r) []) reattori
        )

maxLevel = 100

sortEventi :: [Evento] -> [Evento]
sortEventi = sortP maxLevel priorita id

levelsEventi :: [Evento] -> [(Evento, Int)]
levelsEventi = levelsP priorita id

filtroMovimenti :: Effetti -> ([Movimento], Effetti)
filtroMovimenti = estrai

type Effetti = [Contestualizzato Utente Message]

caricamento'' :: Int -> [Esterno Utente] -> QS -> (QS, Effetti)
caricamento'' l es (QS q) =
    let (q', ef) = (fst q == fst q) `seq` caricaEventi' priorita l es q
     in (QS q', ef)

-- | Group update loader
loader :: QS -> [Esterno Utente] -> Either String (QS, Effetti)
loader qs@(QS (s, _)) es =
    flip runReader s . runExceptT $
        return . first (\(QS q) -> QS . first (seeset ((+) 1 :: Integer -> Integer)) $ q) $
            caricamento'' maxLevel es qs

-- | effettua un inserimento di eventi esterni nello stato, restituendo il nuovo. Stampa i logs
bianco :: Int -> QS -> [Esterno Utente] -> (QS, Response)
bianco l s es =
    let
        (s', qs) =
            second
                ( eccoILogs
                    . map (first flatten)
                    . ( \r ->
                            lascia (UString "") r
                                ++ nubBy ((==) `on` (esterno . fst)) (lascia (Fallimento (UString "")) r)
                      )
                )
                . caricamento'' l es
                $ s
        qs' = ResponseMany (map ResponseOne $ lines qs)
     in
        (s', qs')
