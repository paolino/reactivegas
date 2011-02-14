{-# LANGUAGE TupleSections #-}
module Voci.UI.Ordini where

import Data.Maybe (catMaybes)
import Control.Monad (join)
import Control.Arrow ((&&&))

import Voci.Boxes
import Voci.Quantita
import Voci.Names
import Lib.Passo
import Lib.Response
import Lib.Units
import Lib.NaturalLanguage


nuovoOrdine :: Monad m => BoxVoce -> Costruzione m b BoxOrdine
nuovoOrdine x = do
	let 	complete f = do
			u <- scelte  (map (render . singolare &&& id) [minBound .. maxBound]) $ ResponseOne  $ 
				"unità di misura per il valore dell'ordine"
			x <- libero $ ResponseOne "valore dell'ordine"
			return $ f $ (toRational x :? u)
			
	let 	md = (" in denaro ",) `fmap` complete `fmap` mkInDenaro x 
		mc = (" in confezioni ",) `fmap` complete `fmap` mkInConfezioni x
		mp = (" in peso ",) `fmap` complete `fmap` mkInPeso x
		mv = (" in volume ",) `fmap` complete `fmap` mkInVolume x
		mu = (" in unità di bene ",) `fmap` complete `fmap` mkInPezzi x
	join . scelte (catMaybes [md,mc,mp,mv,mu]) $ ResponseOne "dimensione per il valore dell'ordine"
	



