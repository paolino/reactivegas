{-# LANGUAGE ExistentialQuantification #-}

{- |
Module      : Lib.Response
Description : Polymorphic response types for display
Copyright   : (c) Paolo Veronelli, 2025
License     : BSD-3-Clause

Provides existential response types that can hold various
data structures for formatted display.
-}
module Lib.Response
    ( Response (..)
    ) where

import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, cast, typeOf, typeRep)
import Text.PrettyPrint (Doc, nest, render, text, vcat, ($+$))

-- | Polymorphic response type for formatted output
data Response
    = -- | Single typed value
      forall a. (Typeable a, Show a) => ResponseOne a
    | -- | List of values
      forall a. (Show a) => ResponseMany [a]
    | -- | Association list with string keys
      forall a. (Show a) => ResponseAL [(String, a)]
    | -- | Nested response structure
      Response [(String, Response)]

instance Show Response where
    show x = render $ renderResponse x

-- | Render a response as a pretty-printed document
renderResponse :: Response -> Doc
renderResponse (ResponseOne x) =
    text $ case typeOf x == typeRep (Proxy :: Proxy String) of
        True -> fromJust $ cast x
        False -> show x
renderResponse (ResponseMany xs) =
    vcat $ map (text . show) xs
renderResponse (ResponseAL xs) =
    vcat $ map renderPair xs
  where
    renderPair (key, value) =
        text (key ++ ":") $+$ nest 3 (text $ show value)
renderResponse (Response rs) =
    vcat $ map renderNested rs
  where
    renderNested (label, response) =
        text (label ++ ":") $+$ nest 3 (renderResponse response)
