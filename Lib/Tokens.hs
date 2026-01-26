{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Lib.Tokens
Description : Random token generation
Copyright   : (c) Paolo Veronelli, 2025
License     : BSD-3-Clause

Provides random token generation for session management
and authentication purposes.
-}
module Lib.Tokens
    ( Token (..)
    ) where

import Control.Arrow (first)
import System.Random (Random (..))
import Text.ParserCombinators.ReadPrec (readS_to_Prec)
import Text.Read (Read (..), readPrec)

-- | A random token represented as a string
newtype Token = Token String
    deriving (Eq)

instance Show Token where
    show (Token n) = show n

instance Read Token where
    readPrec = Token <$> readS_to_Prec (const reads)

instance Random Token where
    random g =
        first (Token . show . (`mod` (1000000000 :: Int)) . abs) $
            random g
    randomR = undefined
