{-# LANGUAGE NoMonomorphismRestriction, ViewPatterns, ScopedTypeVariables, MultiParamTypeClasses, FlexibleContexts #-}
-- | modulo inutile per mostrare l'utilizzo dei Deviatori

module Eventi.Logger where
import Control.Monad.Writer
import Control.Applicative
import Codec.Binary.UTF8.String

import Core.Programmazione
import Core.Inserimento (CoreEvents, eventoRifiutato, mus)

import Lib.Prioriti

data Logger  = Logger String  deriving (Show)

instance Read Logger where
	readsPrec _ x = [(Logger x, "")]

d3 :: CoreEvents -> Maybe Logger
d3 (eventoRifiutato -> Just x) = Just (Logger $  encodeString "la dichiarazione non ha modificato la conoscenza")
d3 _ = Nothing

reazioneLogger = Reazione (Just [Deviatore d3], \x ->  
	either (\(Logger x) -> logInserimento . mus $ x) (\(_,()) ->  return ()) x >> return (Just (True,nessunEffetto))
	) 
