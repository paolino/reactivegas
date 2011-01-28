{-# LANGUAGE FlexibleInstances, GADTs , StandaloneDeriving, ScopedTypeVariables, FlexibleContexts, UndecidableInstances#-}
module Voci.Compare where


import Data.Typeable
import Data.List (lookup)
import Numeric (showFFloat)
import Lib.Units
import Voci.Quantita
import Voci.Data
import Voci.Beni


instance Eq a => Eq (Bene a Pesi) where
	Pesato x == Pesato y = x == y
instance Eq a => Eq (Bene a Volumi) where
	Volumato x == Volumato y = x == y
instance Eq a => Eq (Bene a Unità) where
	Contato x == Contato y = x == y

instance Eq (Contenitore Volumi) where	
	Brick x == Brick y = x == y
	Flacone x == Flacone y = x == y
	Damigiana x == Damigiana y = x == y
	Bottiglia x == Bottiglia y = x == y

instance Eq (Contenitore Pesi) where	
	Pacchetto x == Pacchetto y = x == y
	Sacco x == Sacco y = x == y
	Sacchetto x == Sacchetto y = x == y
	Cassetta x == Cassetta y = x == y

instance Eq (Contenitore Unità) where	
	Plateau x == Plateau y = x == y
	Scatola x == Scatola y = x == y
	Scatolone x == Scatolone y = x == y

deriving instance (Eq (Contenitore b), Eq b) => Eq (Confezionamento b)

instance Eq a => Eq (Voce a Pesi Pesi Confezionato) where
	AllaConfezione c1 b1 q1 == AllaConfezione c2 b2 q2 = c1 == c2 && b1 == b2 && q1 == q2 
	AlPesoConfezionato c1 b1 q1 == AlPesoConfezionato c2 b2 q2 = c1 == c2 && b1 == b2 && q1 == q2 

instance Eq a => Eq (Voce a Pesi Pesi Sfuso) where
	AlPeso b1 q1 == AlPeso b2 q2 = b1 == b2 && q1 == q2 

instance Eq a => Eq (Voce a Volumi Volumi Confezionato) where
	AllaConfezione c1 b1 q1 == AllaConfezione c2 b2 q2 = c1 == c2 && b1 == b2 && q1 == q2 
	AlVolumeConfezionato c1 b1 q1 == AlVolumeConfezionato c2 b2 q2 = c1 == c2 && b1 == b2 && q1 == q2 

instance Eq a => Eq (Voce a Unità Unità Confezionato) where
	AllaConfezione c1 b1 q1 == AllaConfezione c2 b2 q2 = c1 == c2 && b1 == b2 && q1 == q2 

instance Eq a => Eq (Voce a Unità Unità Sfuso) where
	AlPezzo b1 q1 == AlPezzo b2 q2 = b1 == b2 && q1 == q2 

instance Eq a => Eq (Voce a Unità Pesi Confezionato) where
	AllaConfezioneStimata c1 b1 dq1 q1 == AllaConfezioneStimata c2 b2 dq2 q2 =  c1 == c2 && b1 == b2 && q1 == q2 &&
		dq1 == dq2

instance Eq a => Eq (Voce a Unità Pesi Sfuso) where
	AlPezzoStimato b1 dq1 q1 == AlPezzoStimato b2 dq2 q2 =  b1 == b2 && q1 == q2 &&
		dq1 == dq2

instance Eq a => Eq (Ordine a Volumi Volumi Confezionato) where
	InDenaro q1 v1 == InDenaro q2 v2 = q1 == q2 && v1 == v2
	InConfezioni q1 v1 == InConfezioni q2 v2 = q1 == q2 && v1 == v2
	InVolume q1 v1 == InVolume q2 v2 = q1 == q2 && v1 == v2

instance Eq a => Eq (Ordine a Pesi Pesi Confezionato) where
	InDenaro q1 v1 == InDenaro q2 v2 = q1 == q2 && v1 == v2
	InConfezioni q1 v1 == InConfezioni q2 v2 = q1 == q2 && v1 == v2
	InPeso q1 v1 == InPeso q2 v2 = q1 == q2 && v1 == v2

instance Eq a => Eq (Ordine a Unità Unità Confezionato) where
	InDenaro q1 v1 == InDenaro q2 v2 = q1 == q2 && v1 == v2
	InConfezioni q1 v1 == InConfezioni q2 v2 = q1 == q2 && v1 == v2

instance Eq a => Eq (Ordine a Unità Unità Sfuso) where
	InDenaro q1 v1 == InDenaro q2 v2 = q1 == q2 && v1 == v2
	InPezzi q1 v1 == InPezzi q2 v2 = q1 == q2 && v1 == v2

instance Eq a => Eq (Ordine a Pesi Pesi Sfuso) where
	InDenaro q1 v1 == InDenaro q2 v2 = q1 == q2 && v1 == v2
	InPeso q1 v1 == InPeso q2 v2 = q1 == q2 && v1 == v2

instance Eq a => Eq (Ordine a Unità Pesi Confezionato) where
	InDenaro q1 v1 == InDenaro q2 v2 = q1 == q2 && v1 == v2
	InConfezioni q1 v1 == InConfezioni q2 v2 = q1 == q2 && v1 == v2
	InPeso q1 v1 == InPeso q2 v2 = q1 == q2 && v1 == v2

instance Eq a => Eq (Ordine a Unità Pesi Sfuso) where
	InDenaro q1 v1 == InDenaro q2 v2 = q1 == q2 && v1 == v2
	InPezzi q1 v1 == InPezzi q2 v2 = q1 == q2 && v1 == v2
	InPeso q1 v1 == InPeso q2 v2 = q1 == q2 && v1 == v2
instance Eq (BWord Pesi) where
	PWord x == PWord y = x == y
instance Eq (BWord Volumi) where
	VWord x == VWord y = x == y
instance Eq (BWord Unità) where
	UWord x == UWord y = x == y

