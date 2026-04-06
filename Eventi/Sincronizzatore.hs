{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Eventi.Sincronizzatore where

import Control.Applicative ((<$>))
import Control.Arrow (first, (&&&))
import Control.Monad (mzero, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Trans.Maybe (MaybeT)

import Lib.Aspetti (ParteDi, see, (.<))
import Lib.Assocs (update)
import Lib.Costruzione (Costruzione)
import Lib.Prioriti (R (..))
import Lib.Response (Response (..))

import Core.Costruzione (CostrAction, libero, runSupporto, scelte)
import Core.Inserimento (MTInserzione, conFallimento, fallimento, logga, modifica, osserva)
import Core.Parsing (Parser)
import Core.Programmazione (Effetti, EventoInterno (..), Reazione (..), nessunEffetto, soloEsterna)

import Eventi.Anagrafe (Responsabile, Utente)
import Lib.Firmabile

-- | gli eventi  esterni per questo modulo
newtype EsternoSincronizzatore
    = -- | imposta un nuovo responsabile di sincronizzazione
      NuovoSincronizzatore Responsabile
    deriving (Show, Read)

prioritySincronizzatore = R k
  where
    k (NuovoSincronizzatore _) = 20

sincronizzatore = (\(Sincronizzatore r) -> r) . see

data Sincronizzatore = Sincronizzatore Responsabile (Bool, Utente) deriving (Read, Show)

-- | tipo dello stato aggiunto del sincronizzatore
type TySincronizzatore a = (Sincronizzatore, a)

-- | aggiunta dell'aspetto Sincronizzatore, serve un responsabile al momento del boot
bootSincronizzatore :: Responsabile -> a -> TySincronizzatore a
bootSincronizzatore y x = (Sincronizzatore y, x)

reazioneSincronizzatore ::
    ( Sincronizzatore `ParteDi` s
    , Parser c EsternoSincronizzatore
    ) =>
    Reazione s c Utente
reazioneSincronizzatore = soloEsterna reattore
  where
    reattore (u, NuovoSincronizzatore r) = conFallimento $ do
        Sincronizzatore _ t <- osserva
        when t $ fallimento "richiesta di modifica già attivata"
        (l, k) <- programmazioneAssenso "modifica della chiave di sincronizzazione" maggioranza chiudibene chiudimale

        fallimento (u /= "sincronizzatore") "solo il sincronizzatore può cambiare se stesso"
        modifica (\(Sincronizzatore _) -> Sincronizzatore r)
        logga "il sincronizzatore è stato cambiato"
        return (True, nessunEffetto)

costrEventiSincronizzatore :: (Monad m) => CostrAction m c EsternoSincronizzatore s
costrEventiSincronizzatore s kp kn = [("sostituzione sincronizzatore", sost)]
  where
    sost = runSupporto s kn kp $ do
        p <- libero "password per il nuovo sincronizzatore"
        return (NuovoSincronizzatore ("sincronizzatore", cryptobox p))
