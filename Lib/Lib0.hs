{-# LANGUAGE NoMonomorphismRestriction, TypeOperators, MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Lib.Lib0 where

import Control.Arrow
import Control.Monad.Maybe
import Control.Monad.RWS
import Data.Maybe
import Codec.Crypto.RSA


deriving instance Read PublicKey
deriving instance Ord PublicKey
deriving instance Eq PublicKey

deriving instance Read PrivateKey
deriving instance Ord PrivateKey
deriving instance Eq PrivateKey


------------------------------------------
type Indice = Int
type Chiave = PublicKey



