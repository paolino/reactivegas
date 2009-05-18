{-# LANGUAGE NoMonomorphismRestriction, TypeOperators, MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
module Lib0 where

import Control.Arrow
import Control.Monad.Maybe
import Control.Monad.RWS
import Data.Maybe
import qualified Aspetti
import Codec.Crypto.RSA



deriving instance Read PublicKey
deriving instance Ord PublicKey
deriving instance Eq PublicKey

deriving instance Read PrivateKey
deriving instance Ord PrivateKey
deriving instance Eq PrivateKey

-----------------------------------------

update k dv v kvs = case lookup k kvs of
	Nothing -> (k,dv v):kvs
	Just v -> (k,dv v):filter ((/=) k . fst) kvs
assente k kvs = case lookup k kvs of
	Nothing -> True
	Just _ -> False
elimina k kvs = filter ((/=) k . fst) kvs

secondM f (x,y) = f y >>= return . (,) x
updateM k dv v kvs = case lookup k kvs of
	Nothing -> dv v >>= \v' -> return $ (k,v'):kvs
	Just v -> dv v >>= \v' -> return $ (k,v'):filter ((/=) k . fst) kvs
xs ? (k,t) = maybe t id $ lookup k xs 
------------------------------------------
type Indice = Int
type Chiave = PublicKey
