{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module Voci.Dynamics where

import Data.Typeable

import Voci.Beni
import Voci.Data

deriving instance Typeable Confezionato
deriving instance Typeable Sfuso
deriving instance Typeable Voce
deriving instance Typeable BWord

-- deriving instance Typeable
-- deriving instance Typeable
