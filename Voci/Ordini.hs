{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, FunctionalDependencies, GADTs, FlexibleContexts, UndecidableInstances, FlexibleInstances, TypeSynonymInstances, ViewPatterns #-}
module Voci.Ordini where

import Data.Typeable
import Voci.Data
import Voci.Compare
import Lib.Units
import Voci.Boxes
import Voci.Beni
import Voci.Quantita
import Debug.Trace


matchOV ::(Eq (BVoce  b c d)) => BOrdine b c d -> BVoce b c d -> Bool

InPezzi _ y  `matchOV` z = y == z
InPeso _ y  `matchOV` z = y == z
InVolume _ y  `matchOV` z = y == z
InDenaro _ y  `matchOV` z = y == z
InConfezioni _ y  `matchOV` z = y == z

bicast :: forall a b c d. (Typeable a, Typeable b, Typeable d, Typeable c) =>  a -> b -> (c -> d -> Bool) -> Bool
bicast x y f = case cast y of
	Nothing -> False
	Just y' -> case cast x of 
		Just x' -> x' `f` y'
		Nothing -> False

match :: BoxOrdine -> BoxVoce -> Bool
(BoxOrdine o) `match` (BoxVoce c) = any id [
	bicast o c (matchOV :: BOrdine Pesi Pesi Sfuso -> BVoce Pesi Pesi Sfuso -> Bool),
	bicast o c (matchOV :: BOrdine Volumi Volumi Confezionato -> BVoce Volumi Volumi Confezionato -> Bool),
	bicast o c (matchOV :: BOrdine Unità Unità Sfuso -> BVoce Unità Unità Sfuso -> Bool),
	bicast o c (matchOV :: BOrdine Pesi Pesi Confezionato -> BVoce Pesi Pesi Confezionato -> Bool),
	bicast o c (matchOV :: BOrdine Unità Unità Confezionato -> BVoce Unità Unità Confezionato -> Bool),
	bicast o c (matchOV :: BOrdine Unità Pesi Sfuso -> BVoce Unità Pesi Sfuso -> Bool),
	bicast o c (matchOV :: BOrdine Unità Pesi Confezionato -> BVoce Unità Pesi Confezionato -> Bool)
	]

class Valuta a b where
	valuta :: a -> Maybe (Quantità b)

instance Valuta (BOrdine Pesi Pesi Sfuso) Denaro where
	valuta (InPeso v (AlPeso _ q)) = Just $ v `qon` q where
	valuta (InDenaro v _) = Just v
instance Valuta (BOrdine Unità Unità Sfuso) Denaro where
	valuta (InPezzi v (AlPezzo _ q ))  = Just $ v `qon` q where
	valuta (InDenaro v (AlPezzo _ q))  = Just $ n `qon` q where
		n = floorQ $ v `qon` q :: Quantità Unità -- numero pezzi
instance Valuta (BOrdine Unità Pesi Sfuso) Denaro where
	valuta (InPeso v (AlPezzoStimato _ (_,p2) q))  = Just $ ((n `qon` p2 :: Quantità Pesi) `qon` q) where
		n = floorQ $ v `qon` p2  :: Quantità Unità-- numero di pezzi
	valuta (InDenaro v (AlPezzoStimato _ (_,p2 )  q)) = Just $  ((n `qon` p2 :: Quantità Pesi) `qon` q) where
		n = floorQ $ (v `qon` q :: Quantità Pesi) `qon` p2 :: Quantità Unità 
instance Valuta (BOrdine Pesi Pesi Confezionato) Denaro where
	valuta (InConfezioni v (AllaConfezione _ _ q)) = Just $ v `qon` q
	valuta (InConfezioni v (AlPesoConfezionato c _ q)) = Just $ (v `qon` n :: Quantità Unità) `qon` 
			(p0 `qon` q :: Quantità (Denaro,Unità)) where
		(n,p0) = confezioniEPeso c 
	valuta (InPeso v (AllaConfezione c _ q)) = Just $ m `qon` q where
		(n,p0) = confezioniEPeso c 
		z = n `qon` p0 :: Quantità Pesi
		m = floorQ $ v `qon` z :: Quantità Unità 
	valuta (InPeso v (AlPesoConfezionato c _ q)) =  Just $ (m `qon` z :: Quantità Pesi) `qon` q where 
		(n,p0) = confezioniEPeso c 
		z = n `qon` p0 :: Quantità Pesi -- peso confezione minima
		m = floorQ $ v `qon` z :: Quantità Unità -- numero confezioni
	valuta (InDenaro v (AllaConfezione c _ q)) = Just $ m `qon` q where
		m = floorQ $ v `qon` q :: Quantità Unità 
	valuta (InDenaro v (AlPesoConfezionato c _ q)) = Just $ m `qon` cc  where 
		(n,p0) = confezioniEPeso c 
		z = n `qon` p0 :: Quantità Pesi -- peso confezione minima
		cc = z `qon` q :: Quantità Denaro -- costo alla condezione
		m = floorQ $ v `qon` cc :: Quantità Unità -- numero confezioni 
instance Valuta (BOrdine Volumi Volumi Sfuso) Denaro where
	valuta (InVolume v (AlVolume _ q)) = Just $ v `qon` q where
	valuta (InDenaro v _) = Just v

instance Valuta (BOrdine Volumi Volumi Confezionato) Denaro where
	valuta (InConfezioni v (AllaConfezione _ _ q)) = Just $ v `qon` q
	valuta (InConfezioni v (AlVolumeConfezionato c _ q)) = Just $ (v `qon` n :: Quantità Unità) `qon` 
			(p0 `qon` q :: Quantità (Denaro,Unità)) where
		(n,p0) = confezioniEVolume c 
	valuta (InVolume v (AllaConfezione c _ q)) = Just $ m `qon` q where
		(n,p0) = confezioniEVolume c 
		z = n `qon` p0 :: Quantità Volumi
		m = floorQ $ v `qon` z :: Quantità Unità 
	valuta (InVolume v (AlVolumeConfezionato c _ q)) =  Just $ (m `qon` z :: Quantità Volumi) `qon` q where 
		(n,p0) = confezioniEVolume c 
		z = n `qon` p0 :: Quantità Volumi -- peso confezione minima
		m = floorQ $ v `qon` z :: Quantità Unità -- numero confezioni
	valuta (InDenaro v (AllaConfezione c _ q)) = Just $ m `qon` q where
		m = floorQ $ v `qon` q :: Quantità Unità 
	valuta (InDenaro v (AlVolumeConfezionato c _ q)) = Just $ m `qon` cc  where 
		(n,p0) = confezioniEVolume c 
		z = n `qon` p0 :: Quantità Volumi -- peso confezione minima
		cc = z `qon` q :: Quantità Denaro -- costo alla condezione
		m = floorQ $ v `qon` cc :: Quantità Unità -- numero confezioni 
instance Valuta (BOrdine Unità Unità Confezionato) Denaro where
	valuta (InConfezioni v (AllaConfezione _ _ q)) = Just $ v `qon` q
	valuta (InDenaro v (AllaConfezione c _ q)) = Just $ m `qon` q where
		m = floorQ $ v `qon` q :: Quantità Unità 
instance Valuta (BOrdine Unità Pesi Confezionato) Denaro where
	valuta (InConfezioni v (AllaConfezioneStimata c _ (_,p2) q)) = Just $ p `qon` q where
		p = v `qon` p2 :: Quantità Pesi
	valuta (InPeso v (AllaConfezioneStimata c _ (_,p2) q)) = Just $ p `qon` q where
		m = floorQ $ v `qon` p2 :: Quantità Unità -- numero confezioni
		p = m `qon` p2 :: Quantità Pesi
	valuta (InDenaro v (AllaConfezioneStimata c _ (_,p2) q)) = Just $ p `qon` q where
		cc = p2 `qon` q :: Quantità (Denaro,Unità)
		m = floorQ $ v `qon` cc :: Quantità Unità
		p = m `qon` p2 :: Quantità Pesi

--------------------------------------------  Pesi ------------------------------------------------------
instance Valuta (BOrdine Pesi Pesi Sfuso) Pesi where
	valuta (InPeso v (AlPeso _ q)) = Just $ v  where
	valuta (InDenaro v (AlPeso _ q)) = Just $ v `qon` q
instance Valuta (BOrdine Unità Pesi Sfuso) Pesi where
	valuta (InPeso v (AlPezzoStimato _ (_,p2) q))  = Just $ n `qon` p2  where
		n = floorQ $ v `qon` p2  :: Quantità Unità-- numero di pezzi
	valuta (InDenaro v (AlPezzoStimato _ (_,p2 )  q)) = Just $  n `qon` p2 where
		n = floorQ $ (v `qon` q :: Quantità Pesi) `qon` p2 :: Quantità Unità 
instance Valuta (BOrdine Unità Pesi Confezionato) Pesi where
	valuta (InConfezioni v (AllaConfezioneStimata c _ (_,p2) q)) = Just $ p where
		p = v `qon` p2 :: Quantità Pesi
	valuta (InPeso v (AllaConfezioneStimata c _ (_,p2) q)) = Just $ p where
		m = floorQ $ v `qon` p2 :: Quantità Unità -- numero confezioni
		p = m `qon` p2 :: Quantità Pesi
	valuta (InDenaro v (AllaConfezioneStimata c _ (_,p2) q)) = Just $ p  where
		cc = p2 `qon` q :: Quantità (Denaro,Unità)
		m = floorQ $ v `qon` cc :: Quantità Unità
		p = m `qon` p2 :: Quantità Pesi
		n = floorQ $ (v `qon` q :: Quantità Pesi) `qon` p2 :: Quantità Unità 
instance Valuta (BOrdine Pesi Pesi Confezionato) Pesi where
	valuta (InConfezioni v (AllaConfezione c _ q)) = Just $ v `qon` z  where
		z = n `qon` p0 :: Quantità Pesi -- peso confezione minima
		(n,p0) = confezioniEPeso c 
	valuta (InConfezioni v (AlPesoConfezionato c _ q)) = Just $ v `qon` z where 
		(n,p0) = confezioniEPeso c 
		z = n `qon` p0 :: Quantità Pesi -- peso confezione minima
	valuta (InPeso v (AllaConfezione c _ q)) = Just $ m `qon` z where
		(n,p0) = confezioniEPeso c 
		z = n `qon` p0 :: Quantità Pesi
		m = floorQ $ v `qon` z :: Quantità Unità 
	valuta (InPeso v (AlPesoConfezionato c _ q)) =  Just $ m `qon` z  where 
		(n,p0) = confezioniEPeso c 
		z = n `qon` p0 :: Quantità Pesi -- peso confezione minima
		m = floorQ $ v `qon` z :: Quantità Unità -- numero confezioni
	valuta (InDenaro v (AllaConfezione c _ q)) = Just $ m `qon` z where
		(n,p0) = confezioniEPeso c 
		z = n `qon` p0 :: Quantità Pesi -- peso confezione minima
		m = floorQ $ v `qon` q :: Quantità Unità 
	valuta (InDenaro v (AlPesoConfezionato c _ q)) = Just $ m `qon` z  where 
		(n,p0) = confezioniEPeso c 
		z = n `qon` p0 :: Quantità Pesi -- peso confezione minima
		cc = z `qon` q :: Quantità Denaro -- costo alla confezione
		m = floorQ $ v `qon` cc :: Quantità Unità -- numero confezioni 
------------------------------ Volumi ---------------------------------------------------

instance Valuta (BOrdine Volumi Volumi Sfuso) Volumi where
	valuta (InVolume v (AlVolume _ q)) = Just $ v where
	valuta (InDenaro v (AlVolume _ q)) = Just $ v `qon` q

instance Valuta (BOrdine Volumi Volumi Confezionato) Volumi where
	valuta (InConfezioni v (AllaConfezione c _ q)) = Just $ v `qon` z  where
		z = n `qon` p0 :: Quantità Volumi -- peso confezione minima
		(n,p0) = confezioniEVolume c 
	valuta (InConfezioni v (AlVolumeConfezionato c _ q)) = Just $ v `qon` z where 
		(n,p0) = confezioniEVolume c 
		z = n `qon` p0 :: Quantità Volumi -- peso confezione minima
	valuta (InVolume v (AllaConfezione c _ q)) = Just $ m `qon` z where
		(n,p0) = confezioniEVolume c 
		z = n `qon` p0 :: Quantità Volumi
		m = floorQ $ v `qon` z :: Quantità Unità 
	valuta (InVolume v (AlVolumeConfezionato c _ q)) =  Just $ m `qon` z  where 
		(n,p0) = confezioniEVolume c 
		z = n `qon` p0 :: Quantità Volumi -- peso confezione minima
		m = floorQ $ v `qon` z :: Quantità Unità -- numero confezioni
	valuta (InDenaro v (AllaConfezione c _ q)) = Just $ m `qon` z where
		(n,p0) = confezioniEVolume c 
		z = n `qon` p0 :: Quantità Volumi -- peso confezione minima
		m = floorQ $ v `qon` q :: Quantità Unità 
	valuta (InDenaro v (AlVolumeConfezionato c _ q)) = Just $ m `qon` z  where 
		(n,p0) = confezioniEVolume c 
		z = n `qon` p0 :: Quantità Volumi -- peso confezione minima
		cc = z `qon` q :: Quantità Denaro -- costo alla confezione
		m = floorQ $ v `qon` cc :: Quantità Unità -- numero confezioni 


