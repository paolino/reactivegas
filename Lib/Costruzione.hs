{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving, FlexibleContexts, ScopedTypeVariables, NoMonomorphismRestriction,ViewPatterns #-}

module Lib.Costruzione where

import Lib.Aspetti
import Control.Monad.Cont
import Control.Monad.Reader
import Text.PrettyPrint

import Control.Applicative
import Data.List

type Continuazione m b a = a -> m (Passo m b) -- una continuazione monadica

data Monad m => Passo m b 
	= forall a. Scelta String [(String,a)] (Passo m b)  (Continuazione m b a)
	| forall a. Read a => Libero String (Passo m b)  (Continuazione m b a)
	| Costruito b

type Svolgimento b m a = ContT (Passo m b) (ReaderT (Passo m b) m) a

reset :: Monad m => (Passo m b -> Continuazione m b a -> Passo m b) -> Svolgimento b m a
reset f = ContT $ \k -> local (l k) ask where
	l k u = f u $ \x -> runReaderT (k x) (l k u)

libero :: (Monad m, Read a) => String -> Svolgimento b m a 
libero prompt = reset $ Libero prompt

scelte :: Monad m =>  [(String,a)] -> String -> Svolgimento b m a 
scelte xs prompt = reset $ Scelta prompt xs

type Costruzione m b = Svolgimento b m b

svolgi :: Monad m => Passo m b -> Costruzione m b -> m (Passo m b)
svolgi p c = flip runReaderT p $ runContT c (return . Costruito)

----------------------  un driver per utilizzare una Costruzione ----------------------------------------						
data Response 
	= forall a. Show a => ResponseOne a
	| forall a . Show a => ResponseMany [a]
	| forall a . Show a =>  ResponseAL [(String,a)]
	| Response [(String,Response)]

renderResponse (ResponseOne x) = text (show x)
renderResponse (ResponseMany xs) = vcat $ map (text . show) xs
renderResponse (ResponseAL xs) = vcat $ map (\(x,y) -> hang (text (x ++ ":")) 3 (text . show $ y)) xs
renderResponse (Response rs) = vcat $ map (\(s,r) -> hang (text (s ++ ":")) 3 (renderResponse r)) rs


