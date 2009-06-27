{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving, FlexibleContexts, ScopedTypeVariables, NoMonomorphismRestriction,ViewPatterns #-}

module Costruzione where

import Aspetti
import Control.Monad.Cont
import Control.Monad.Reader
import Text.PrettyPrint

import System.Directory
import Control.Applicative
import Data.List

data SceltaOLibero a = Scelta String [(String,a)] | Libero String

type Continuazione m b a = a -> m (Costruzione m b) -- una continuazione monadica
data Monad m => Costruzione m b 
	= forall a . (Show a,Read a) => 
		Costruzione (SceltaOLibero a) (Continuazione m b a)
newtype Monad m => Svolgimento b m a = Svolgimento (ContT (Costruzione m b) m a)  deriving 
	(Functor, Monad, MonadCont)

instance MonadTrans (Svolgimento b) where
	lift k = Svolgimento (lift $  k) 
instance Show (Svolgimento b m a)
instance Read (Svolgimento b m a)

parametro :: (Monad m, Show a,Read a) => SceltaOLibero a -> Svolgimento b m a 
parametro scelte = Svolgimento (ContT $ \k -> return (Costruzione scelte k))

-- svolgi :: Monad m => Svolgimento b m b  -> m (Costruzione m b)
svolgi (Svolgimento c) = runContT c undefined 

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


