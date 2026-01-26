{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Lib.Tokens where

import Control.Arrow (first, (***))
import Control.Monad (replicateM)
import Control.Monad.Random (getRandom)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Typeable (Typeable)
import System.Random (Random (..))
import Text.ParserCombinators.ReadPrec
import Text.Read

import Lib.Modify (PeekPoke (peek), modifyT)
import Lib.Passo
import Lib.Response

-- testing

newtype Token = Token String deriving (Eq)

instance Show Token where
    show (Token n) = show n

instance Read Token where
    readPrec = Token `fmap` readS_to_Prec (const reads)

instance Random Token where
    random = (Token . show . (`mod` (1000000000 :: Int)) . abs *** id) . random
    randomR = undefined
