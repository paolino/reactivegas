{-# LANGUAGE StandaloneDeriving #-}
module Bugs where

import Codec.Crypto.RSA

deriving instance Read PublicKey
deriving instance Ord PublicKey
deriving instance Eq PublicKey

deriving instance Read PrivateKey
deriving instance Ord PrivateKey
deriving instance Eq PrivateKey


