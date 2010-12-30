
{-# LANGUAGE Rank2Types, ScopedTypeVariables, StandaloneDeriving, TupleSections , FlexibleContexts, NoMonomorphismRestriction #-}

module Lib.Tokens where

import Control.Monad.Random (getRandom)
import Control.Monad (replicateM)
import Control.Monad.Trans (liftIO, MonadIO)
import Text.ParserCombinators.ReadPrec
import Text.Read
import System.Random (Random (..))
import Control.Arrow ((***),first)
import Data.Typeable (Typeable)

import Lib.Passo
import Lib.Response 
import Lib.Modify (PeekPoke (peek), modifyT)

-- testing


newtype Token = Token Int deriving Eq

instance Show Token where
	show (Token n) = show n

instance Read Token where
	readPrec = Token `fmap` readS_to_Prec (const reads)

instance Random Token where
	random = (Token . (`mod` 1000000000) . abs *** id) . random
	randomR = undefined



