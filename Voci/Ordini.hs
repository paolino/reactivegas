{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, FunctionalDependencies, GADTs, FlexibleContexts, FlexibleInstances, TypeSynonymInstances, ViewPatterns, OverlappingInstances #-}
module Voci.Ordini where

import Data.Typeable
import Voci.Data
import Voci.Compare
import Lib.Units
import Voci.Beni
import Voci.Quantita
import Debug.Trace


matchOV ::(Eq (BVoce  b c d)) => BOrdine b c d -> BVoce b c d -> Bool

InPezzi _ y  `matchOV` z = y == z
InPeso _ y  `matchOV` z = y == z
InVolume _ y  `matchOV` z = y == z
InDenaro _ y  `matchOV` z = y == z
InConfezioni _ y  `matchOV` z = y == z


class Valuta b c d e where
	valuta :: BOrdine b c d  -> Maybe (Quantità e)



--------- Richiesta Denaro ----------------------------------------------------

----------------------------- Sfuso --------------------------------------
instance Valuta Pesi Pesi Sfuso Denaro where
	valuta (InPeso v (AlPeso _ q)) = Just $ v *|* q where
	valuta (InDenaro v _) = Just v
instance Valuta Unità Unità Sfuso Denaro where
	valuta (InPezzi v (AlPezzo _ q ))  = Just $ v *|* q where
	valuta (InDenaro v (AlPezzo _ q))  = Just $ n *|* q where
		n = floorQ $ v *|* q :: Quantità Unità -- numero pezzi
instance Valuta Unità Pesi Sfuso Denaro where
	valuta (InPeso v (AlPezzoStimato _ (_,p2) q))  = Just $ ((n *|* p2 :: Quantità Pesi) *|* q) where
		n = floorQ $ v *|* p2  :: Quantità Unità-- numero di pezzi
	valuta (InDenaro v (AlPezzoStimato _ (_,p2 )  q)) = Just $  ((n *|* p2 :: Quantità Pesi) *|* q) where
		n = floorQ $ (v *|* q :: Quantità Pesi) *|* p2 :: Quantità Unità 
instance Valuta Volumi Volumi Sfuso Denaro where
	valuta (InVolume v (AlVolume _ q)) = Just $ v *|* q where
	valuta (InDenaro v _) = Just v

----------------------------- Confezionato ---------------------------------
instance Valuta Pesi Pesi Confezionato Denaro where
	valuta (InConfezioni v (AllaConfezione _ _ q)) = Just $ v *|* q
	valuta (InConfezioni v (AlPesoConfezionato c _ q)) = Just $ (v *|* n :: Quantità Unità) *|* 
			(p0 *|* q :: Quantità (Denaro,Unità)) where
		(n,p0) = confezioniEPeso c 
	valuta (InPeso v (AllaConfezione c _ q)) = Just $ m *|* q where
		(n,p0) = confezioniEPeso c 
		z = n *|* p0 :: Quantità Pesi
		m = floorQ $ v *|* z :: Quantità Unità 
	valuta (InPeso v (AlPesoConfezionato c _ q)) =  Just $ (m *|* z :: Quantità Pesi) *|* q where 
		(n,p0) = confezioniEPeso c 
		z = n *|* p0 :: Quantità Pesi -- peso confezione minima
		m = floorQ $ v *|* z :: Quantità Unità -- numero confezioni
	valuta (InDenaro v (AllaConfezione c _ q)) = Just $ m *|* q where
		m = floorQ $ v *|* q :: Quantità Unità 
	valuta (InDenaro v (AlPesoConfezionato c _ q)) = Just $ m *|* cc  where 
		(n,p0) = confezioniEPeso c 
		z = n *|* p0 :: Quantità Pesi -- peso confezione minima
		cc = z *|* q :: Quantità Denaro -- costo alla condezione
		m = floorQ $ v *|* cc :: Quantità Unità -- numero confezioni 

instance Valuta Volumi Volumi Confezionato Denaro where
	valuta (InConfezioni v (AllaConfezione _ _ q)) = Just $ v *|* q
	valuta (InConfezioni v (AlVolumeConfezionato c _ q)) = Just $ (v *|* n :: Quantità Unità) *|* 
			(p0 *|* q :: Quantità (Denaro,Unità)) where
		(n,p0) = confezioniEVolume c 
	valuta (InVolume v (AllaConfezione c _ q)) = Just $ m *|* q where
		(n,p0) = confezioniEVolume c 
		z = n *|* p0 :: Quantità Volumi
		m = floorQ $ v *|* z :: Quantità Unità 
	valuta (InVolume v (AlVolumeConfezionato c _ q)) =  Just $ (m *|* z :: Quantità Volumi) *|* q where 
		(n,p0) = confezioniEVolume c 
		z = n *|* p0 :: Quantità Volumi -- peso confezione minima
		m = floorQ $ v *|* z :: Quantità Unità -- numero confezioni
	valuta (InDenaro v (AllaConfezione c _ q)) = Just $ m *|* q where
		m = floorQ $ v *|* q :: Quantità Unità 
	valuta (InDenaro v (AlVolumeConfezionato c _ q)) = Just $ m *|* cc  where 
		(n,p0) = confezioniEVolume c 
		z = n *|* p0 :: Quantità Volumi -- peso confezione minima
		cc = z *|* q :: Quantità Denaro -- costo alla condezione
		m = floorQ $ v *|* cc :: Quantità Unità -- numero confezioni
 
instance Valuta Unità Unità Confezionato Denaro where
	valuta (InConfezioni v (AllaConfezione _ _ q)) = Just $ v *|* q
	valuta (InDenaro v (AllaConfezione c _ q)) = Just $ m *|* q where
		m = floorQ $ v *|* q :: Quantità Unità 

instance Valuta Unità Pesi Confezionato Denaro where
	valuta (InConfezioni v (AllaConfezioneStimata c _ (_,p2) q)) = Just $ p *|* q where
		p = v *|* p2 :: Quantità Pesi
	valuta (InPeso v (AllaConfezioneStimata c _ (_,p2) q)) = Just $ p *|* q where
		m = floorQ $ v *|* p2 :: Quantità Unità -- numero confezioni
		p = m *|* p2 :: Quantità Pesi
	valuta (InDenaro v (AllaConfezioneStimata c _ (_,p2) q)) = Just $ p *|* q where
		cc = p2 *|* q :: Quantità (Denaro,Unità)
		m = floorQ $ v *|* cc :: Quantità Unità
		p = m *|* p2 :: Quantità Pesi

------------Richiesta  Pesi ------------------------------------------------------

------------------------------ Sfuso ------------------------------------
instance Valuta Unità Pesi Sfuso Pesi where
	valuta (InPeso v (AlPezzoStimato _ (_,p2) q))  = Just $ n *|* p2  where
		n = floorQ $ v *|* p2  :: Quantità Unità-- numero di pezzi
	valuta (InDenaro v (AlPezzoStimato _ (_,p2 )  q)) = Just $  n *|* p2 where
		n = floorQ $ (v *|* q :: Quantità Pesi) *|* p2 :: Quantità Unità 

instance Valuta Pesi Pesi Sfuso Pesi where
	valuta (InPeso v (AlPeso _ q)) = Just $ v  where
	valuta (InDenaro v (AlPeso _ q)) = Just $ v *|* q
instance Valuta b c Sfuso Pesi where
	valuta _ = Nothing

------------------------------- Confezionato ---------------------------------
instance Valuta Unità Pesi Confezionato Pesi where
	valuta (InConfezioni v (AllaConfezioneStimata c _ (_,p2) q)) = Just $ p where
		p = v *|* p2 :: Quantità Pesi
	valuta (InPeso v (AllaConfezioneStimata c _ (_,p2) q)) = Just $ p where
		m = floorQ $ v *|* p2 :: Quantità Unità -- numero confezioni
		p = m *|* p2 :: Quantità Pesi
	valuta (InDenaro v (AllaConfezioneStimata c _ (_,p2) q)) = Just $ p  where
		cc = p2 *|* q :: Quantità (Denaro,Unità)
		m = floorQ $ v *|* cc :: Quantità Unità
		p = m *|* p2 :: Quantità Pesi
		n = floorQ $ (v *|* q :: Quantità Pesi) *|* p2 :: Quantità Unità 


instance Valuta Pesi Pesi Confezionato Pesi where
	valuta (InConfezioni v (AllaConfezione c _ q)) = Just $ v *|* z  where
		z = n *|* p0 :: Quantità Pesi -- peso confezione minima
		(n,p0) = confezioniEPeso c 
	valuta (InConfezioni v (AlPesoConfezionato c _ q)) = Just $ v *|* z where 
		(n,p0) = confezioniEPeso c 
		z = n *|* p0 :: Quantità Pesi -- peso confezione minima
	valuta (InPeso v (AllaConfezione c _ q)) = Just $ m *|* z where
		(n,p0) = confezioniEPeso c 
		z = n *|* p0 :: Quantità Pesi
		m = floorQ $ v *|* z :: Quantità Unità 
	valuta (InPeso v (AlPesoConfezionato c _ q)) =  Just $ m *|* z  where 
		(n,p0) = confezioniEPeso c 
		z = n *|* p0 :: Quantità Pesi -- peso confezione minima
		m = floorQ $ v *|* z :: Quantità Unità -- numero confezioni
	valuta (InDenaro v (AllaConfezione c _ q)) = Just $ m *|* z where
		(n,p0) = confezioniEPeso c 
		z = n *|* p0 :: Quantità Pesi -- peso confezione minima
		m = floorQ $ v *|* q :: Quantità Unità 
	valuta (InDenaro v (AlPesoConfezionato c _ q)) = Just $ m *|* z  where 
		(n,p0) = confezioniEPeso c 
		z = n *|* p0 :: Quantità Pesi -- peso confezione minima
		cc = z *|* q :: Quantità Denaro -- costo alla confezione
		m = floorQ $ v *|* cc :: Quantità Unità -- numero confezioni 
instance Valuta b c Confezionato Pesi where
	valuta _ = Nothing


------------------------------ Richieste Volumi ---------------------------------------------------

----------------------------------------Sfuso ---------------------------
instance Valuta Volumi Volumi Sfuso Volumi where
	valuta (InVolume v (AlVolume _ q)) = Just $ v where
	valuta (InDenaro v (AlVolume _ q)) = Just $ v *|* q
instance Valuta b c Sfuso Volumi where
	valuta _ = Nothing

---------------------------------------- Confezionato
instance Valuta Volumi Volumi Confezionato Volumi where
	valuta (InConfezioni v (AllaConfezione c _ q)) = Just $ v *|* z  where
		z = n *|* p0 :: Quantità Volumi -- peso confezione minima
		(n,p0) = confezioniEVolume c 
	valuta (InConfezioni v (AlVolumeConfezionato c _ q)) = Just $ v *|* z where 
		(n,p0) = confezioniEVolume c 
		z = n *|* p0 :: Quantità Volumi -- peso confezione minima
	valuta (InVolume v (AllaConfezione c _ q)) = Just $ m *|* z where
		(n,p0) = confezioniEVolume c 
		z = n *|* p0 :: Quantità Volumi
		m = floorQ $ v *|* z :: Quantità Unità 
	valuta (InVolume v (AlVolumeConfezionato c _ q)) =  Just $ m *|* z  where 
		(n,p0) = confezioniEVolume c 
		z = n *|* p0 :: Quantità Volumi -- peso confezione minima
		m = floorQ $ v *|* z :: Quantità Unità -- numero confezioni
	valuta (InDenaro v (AllaConfezione c _ q)) = Just $ m *|* z where
		(n,p0) = confezioniEVolume c 
		z = n *|* p0 :: Quantità Volumi -- peso confezione minima
		m = floorQ $ v *|* q :: Quantità Unità 
	valuta (InDenaro v (AlVolumeConfezionato c _ q)) = Just $ m *|* z  where 
		(n,p0) = confezioniEVolume c 
		z = n *|* p0 :: Quantità Volumi -- peso confezione minima
		cc = z *|* q :: Quantità Denaro -- costo alla confezione
		m = floorQ $ v *|* cc :: Quantità Unità -- numero confezioni 
instance Valuta b c Confezionato Volumi where
	valuta _ = Nothing

---------------------------- Richieste Unità -----------------------

----------------------------------- Sfuso --------------------------
instance Valuta Unità Unità Sfuso Unità where
	valuta (InPezzi v (AlPezzo _ q)) = Just v
	valuta (InDenaro v (AlPezzo _ q)) = Just . floorQ $ v *|* q
instance Valuta Unità Pesi Sfuso Unità where
	valuta (InPezzi v (AlPezzoStimato _ (_,p) q)) = Just v
	valuta (InPeso v (AlPezzoStimato _ (_,p) q)) = Just . floorQ $ v *|* p
	valuta (InDenaro v (AlPezzoStimato _ (_,p) q)) = Just . floorQ $ r *|* p where
		r = v *|* q :: Quantità Pesi -- peso acquistabile

instance Valuta b c Sfuso Unità where
	valuta _ = Nothing

instance Valuta Unità Unità Confezionato Unità where
	valuta (InConfezioni v (AllaConfezione c _ q)) = Just v
	valuta (InDenaro v (AllaConfezione c _ q)) = Just . floorQ $ v *|* q
instance Valuta Unità Pesi Confezionato Unità where
	valuta (InConfezioni v (AllaConfezioneStimata c _ (_,p) q)) = Just v
	valuta (InPeso v (AllaConfezioneStimata c _ (_,p) q)) = Just $ floorQ $ v *|* p
	valuta (InDenaro v (AllaConfezioneStimata c _ (_,p) q)) = Just . floorQ $ 
		(v *|* q :: Quantità Pesi) *|* p

instance Valuta Pesi Pesi Confezionato Unità where
	valuta (InConfezioni v (AllaConfezione c _ q)) = Just v
	valuta (InDenaro v (AllaConfezione c _ q)) = Just . floorQ $ v *|* q
	valuta (InPeso v (AllaConfezione c _ q)) = Just $ floorQ $ v *|* r where
		(n,p0) = confezioniEPeso c
		r = n *|* p0 :: Quantità Pesi
		
	
instance Valuta Volumi Volumi Confezionato Unità where
	valuta (InConfezioni v (AllaConfezione c _ q)) = Just v
	valuta (InDenaro v (AllaConfezione c _ q)) = Just . floorQ $ v *|* q
	valuta (InVolume v (AllaConfezione c _ q)) = Just $ floorQ $ v *|* r where
		(n,p0) = confezioniEVolume c
		r = n *|* p0 :: Quantità Volumi
instance Valuta b c Confezionato Unità where
	valuta _ = Nothing

