{-# LANGUAGE NoMonomorphismRestriction, TypeOperators, MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
module Lib1 where

import Control.Arrow
import Control.Monad.Maybe
import Control.Monad.RWS
import Data.Maybe
import qualified Aspetti
import Core
import Codec.Crypto.RSA


fallimento t s = when t $ logga s >> mzero
osserva = lift Aspetti.osserva
modifica = lift . Aspetti.modifica
logga s = lift (logInserimento s)
conFallimento = runMaybeT
ignoraEventoInterno (Left ()) = undefined
type MTInserzione s c d = MaybeT (Inserzione s c d)

