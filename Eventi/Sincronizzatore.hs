
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, ViewPatterns, NoMonomorphismRestriction #-}
module Eventi.Sincronizzatore where




import Control.Monad.Maybe (MaybeT)
import Control.Monad (mzero, when)
import Control.Monad.Error (throwError)
import Control.Applicative ((<$>))
import Control.Monad.Reader (asks, MonadReader)
import Control.Arrow ((&&&), first)

import Lib.Aspetti (see, (.<), ParteDi)
import Lib.Costruzione (Costruzione)
import Lib.Prioriti (R(..))
import Lib.Assocs (update)
import Lib.Response (Response (..))

import Core.Costruzione (libero, scelte, runSupporto, CostrAction)
import Core.Parsing (Parser)
import Core.Programmazione (Effetti, Reazione (..) , EventoInterno (..), soloEsterna, nessunEffetto)
import Core.Inserimento (MTInserzione, conFallimento, fallimento, osserva, modifica, logga)

import Eventi.Anagrafe (Responsabile, Utente)
import Lib.Firmabile

-- | gli eventi  esterni per questo modulo
data EsternoSincronizzatore 
	= NuovoSincronizzatore Responsabile -- ^ imposta un nuovo responsabile di sincronizzazione
	deriving (Show,Read)

prioritySincronizzatore = R k where
	k (NuovoSincronizzatore _) = 20

sincronizzatore = (\(Sincronizzatore r) -> r) . see

data Sincronizzatore = Sincronizzatore Responsabile deriving (Read, Show)

-- | tipo dello stato aggiunto del sincronizzatore
type TySincronizzatore a = (Sincronizzatore , a)

-- | aggiunta dell'aspetto Sincronizzatore, serve un responsabile al momento del boot
bootSincronizzatore :: Responsabile -> a -> TySincronizzatore a
bootSincronizzatore y x = (Sincronizzatore y, x)


reazioneSincronizzatore :: 
	(Sincronizzatore `ParteDi` s
	, Parser c EsternoSincronizzatore
	) => Reazione s c Utente
reazioneSincronizzatore = soloEsterna reattore where
	reattore (u,NuovoSincronizzatore r) = conFallimento $ do
		fallimento (u /= "sincronizzatore") "solo il sincronizzatore può cambiare se stesso"
		modifica (\(Sincronizzatore _) -> Sincronizzatore r)
		logga "il sincronizzatore è stato cambiato"
		return (True,nessunEffetto)	

costrEventiSincronizzatore :: CostrAction c EsternoSincronizzatore s
costrEventiSincronizzatore s kp kn = 	[("sostituzione sincronizzatore", sost)]
	where
	sost = runSupporto s kn kp $ do
		p <- libero "password per il nuovo sincronizzatore"
		return (NuovoSincronizzatore $ ("sincronizzatore",cryptobox p)) 	
