
{-# LANGUAGE GADTs, ExistentialQuantification, FlexibleContexts #-}
module Voci.Core where

import Lib.Units -- (Pesi,Volumi,Unità, Denaro,UnitClass (..))
import Lib.NaturalLanguage
import Lib.QInteger
import Eventi.Servizio

--------------------------------- simple quantities library -------------------------------
data Quantità b = Rational :? b deriving (Show,Read)

instance (UnitClass a, UnitClass b) => UnitClass (a,b) where
	base (x,y) = let 
		(kx,x') = base x
		(ky,y') = base y
		in (kx/ky,(x',y'))
instance UnitClass b => Eq (Quantità b) where
	(x :? y) == (x1 :? y1) =  (x * fst (base y)) == (x1 * fst (base y1))

instance UnitClass b => Ord (Quantità b) where
	(x :? y) `compare` (x1 :? y1) =  (x * fst (base y)) `compare` (x1 * fst (base y1))
--------------------------------------------------------------------------------------------

data Bene a b where
	Pesato :: a -> Bene a Pesi
	Volumato :: a -> Bene a Volumi
	Contato :: a -> Bene a Unità

data Contenitore b where 
	Brick :: Contenitore Volumi
	Flacone :: Contenitore Volumi 
	Damigiana :: Contenitore Volumi
	Pacchetto :: Contenitore Pesi
	Sacco :: Contenitore Pesi
	Sacchetto :: Contenitore Pesi
	Cassetta :: Contenitore Pesi
		
data Scatolame = Scatola | Plateau | Scatolone | Pallet deriving (Eq,Enum,Bounded, Read,Show)

data Confezione b where
	Base :: Confezione b 
	Solido :: Quantità Pesi -> Contenitore Pesi -> Confezione Pesi
	Liquido :: Quantità Volumi -> Contenitore Volumi -> Confezione Volumi
	Inscatolato :: Scatolame -> Int -> Confezione b -> Confezione b

explode :: Confezione b -> [Confezione b]
explode Base = [Base]
explode s@(Solido _ _) = [s]
explode l@(Liquido _ _) = [l]
explode i@(Inscatolato _ _ c) = i: explode c


data Scaffale a b = Scaffale (Confezione b)  (Bene a b) 


----------------------------------------------------------------------
data Prezzato a b c where
	AllaConfezione 
		:: Scaffale a b -> Quantità Denaro -> Prezzato a b b 
	AlPeso 
		:: Scaffale a Pesi -> Quantità (Denaro,Pesi) -> Prezzato a Pesi Pesi
	AlVolume 
		:: Scaffale a Volumi -> Quantità (Denaro,Volumi) -> Prezzato a Volumi Volumi
	AlPesoStimato
		:: Scaffale a Unità  -> (Quantità Pesi, Quantità Pesi) 
			-> Quantità (Denaro,Pesi) -> Prezzato a Unità Pesi 
	
------------------------------------ datatype reale dei nomi dei beni -------------------------
data BWord a where
	PWord :: Molteplicita Word -> BWord Pesi
	VWord :: Molteplicita Word -> BWord Volumi
	UWord :: (Word,Word) -> BWord Unità
fromPesato :: BBene Pesi -> Molteplicita Word
fromPesato (Pesato (PWord x)) = x	 
fromVolumato :: BBene Volumi -> Molteplicita Word
fromVolumato (Volumato (VWord x)) = x	 

type BBene b = Bene (BWord b) b
type BScaffale b = Scaffale (BWord b) b

data Commercio = forall a b c . (Show a , Show (Scaffale a c) , Name (Prezzato a b c) , UnitClass b, UnitClass c) => 
	Commercio (Prezzato a b c)

data Voce =  Voce {
	categorie :: [String],
	filiera :: [String],
	commercio :: Commercio	
	} 



