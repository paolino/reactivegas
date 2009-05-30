{-# LANGUAGE NoMonomorphismRestriction, ViewPatterns, ScopedTypeVariables, MultiParamTypeClasses #-}
-- | modulo inutile per mostrare l'utilizzo dei Deviatori

module Logger where
import Control.Monad.Writer
import Control.Applicative
import Codec.Binary.UTF8.String

import Core
import Anagrafe (eliminazioneResponsabile)
import Impegno (fallimentoImpegno)

data Logger  = Logger String  deriving (Show)

instance Read Logger where
	readsPrec _ x = [(Logger x, "")]

d1 (eliminazioneResponsabile -> Just (u,w)) = Just (Logger "ahi, comportato male eh")
d1 _ = Nothing
d2 (fallimentoImpegno -> Just (u,v)) = Just (Logger $ u ++ " ha fatto un danno :)")
d2 _ = Nothing
d3 (eventoRifiutato -> Just x) = Just (Logger $  encodeString "l'evento non ha modificato la realtá")
reazioneLogger = Reazione (Just [Deviatore d1, Deviatore d2, Deviatore d3], \x ->  either (\(Logger x) -> logInserimento x) (\(_,()) ->  return ()) x 
									>> return (Just (True,nessunEffetto))) 
