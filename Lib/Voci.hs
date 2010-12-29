{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances, FlexibleContexts, Rank2Types, ScopedTypeVariables, GADTs, ExistentialQuantification, DeriveDataTypeable, StandaloneDeriving #-}
module Lib.Voci where

import Control.Arrow
import Lib.Units
import Lib.Passo
import Lib.Console
import Lib.NaturalLanguage
import Lib.Response
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


data Scaffale a b = Scaffale (Confezione b)  (Bene a b) 
deriving instance Show a => Show (Scaffale a Unità)
deriving instance Show a => Show (Scaffale a Pesi)
deriving instance Show a => Show (Scaffale a Volumi)
deriving instance Read a => Read (Scaffale a Unità)
deriving instance Read a => Read (Scaffale a Pesi)
deriving instance Read a => Read (Scaffale a Volumi)

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
	
----------------------------------------------------------------------------------
instance (Show (Scaffale a c), Show a) => Show (Prezzato a b c) where
	show (AllaConfezione s q) = show ("AC",s,q)
	show (AlPeso s q) = show ("AP",s,q)
	show (AlVolume s q) = show ("AV",s,q)
	show (AlPesoStimato s qs q) = show ("ASS",s,qs,q)

instance Read a => Read (Prezzato a Pesi Pesi) where
	readsPrec _ x = case reads x of
		[(("AP",s,q),r)] -> [(AlPeso s q,r)]
		_ -> case reads x of
			[(("AC",s,q),r)] -> [(AllaConfezione s q,r)]
			_ -> []

instance Read a => Read (Prezzato a Volumi Volumi) where
	readsPrec _ x = case reads x of
		[(("AV",s,q),r)] -> [(AlVolume s q,r)]
		_ -> case reads x of
			[(("AC",s,q),r)] -> [(AllaConfezione s q,r)]
			_ -> []

instance Read a => Read (Prezzato a Unità Pesi) where
	readsPrec _ x = case reads x of
		[(("ASS",s,qs,q),r)] -> [(AlPesoStimato s qs q,r)]
		_ -> []

instance Show a => Show (Bene a b) where
	show (Pesato x) = show ("P",x)
	show (Volumato x) = show ("V",x)
	show (Contato x) = show ("C",x)

instance Read a => Read (Bene a Pesi) where
	readsPrec _ x = case reads x of
		[(("P",x),r)] -> [(Pesato x,r)]
		_ -> []
instance Read a => Read (Bene a Volumi) where
	readsPrec _ x = case reads x of
		[(("V",x),r)]-> [(Volumato x,r)]
		_ -> []
instance Read a => Read (Bene a Unità) where
	readsPrec _ x = case reads x of
		[(("C",x),r)] -> [(Contato x,r)]
		_ -> []


instance Show (Contenitore b) where
	show Brick = show "Brick"
	show Flacone = show "Flacone"
	show Damigiana = show "Damigiana"
	show Pacchetto = show "Pacchetto"
	show Sacco = show "Sacco"
	show Sacchetto = show "Sacchetto"
	show Cassetta = show "Cassetta"

instance Read (Contenitore Volumi) where
	readsPrec _ x = case reads x of
		[(x,r)] -> case x of 
			"Brick" -> [(Brick,r)]
			"Flacone" -> [(Flacone,r)]
			"Damigiana" -> [(Damigiana,r)]
		_ -> []
instance Read (Contenitore Pesi) where
	readsPrec _ x = case reads x of
		[(x,r)] -> case x of 
			"Pacchetto" -> [(Pacchetto,r)]
			"Sacco" -> [(Sacco,r)]
			"Sacchetto" -> [(Sacchetto,r)]
			"Cassetta" -> [(Cassetta,r)]
		_ -> []

instance Show (Confezione b) where
	show Base = show "B"
	show (Solido q cp) = show ("S",q,cp)
	show (Liquido q cl) = show ("L",q,cl)
	show (Inscatolato s i c) = show ("I",s,i,c)

instance Read (Confezione Pesi) where
	readsPrec _ x = case reads x of
		[("B",r)] -> [(Base,r)]
		_ -> case reads x of
			[(("S",q,cp),r)] -> [(Solido q cp,r)]
			_ -> case reads x of
				[(("I",s,i,c),r)] -> [(Inscatolato s i c,r)]
				_ -> []
instance Read (Confezione Volumi) where
	readsPrec _ x = case reads x of
		[("B",r)] -> [(Base,r)]
		_ -> case reads x of
			[(("L",q,cp),r)] -> [(Liquido q cp,r)]
			_ -> case reads x of
				[(("I",s,i,c),r)] -> [(Inscatolato s i c,r)]
				_ -> []
instance Read (Confezione Unità) where
	readsPrec _ x = case reads x of
		[("B",r)] -> [(Base,r)]
		_ -> case reads x of
			[(("I",s,i,c),r)] -> [(Inscatolato s i c,r)]
			_ -> []

explode :: Confezione b -> [Confezione b]
explode Base = [Base]
explode s@(Solido _ _) = [s]
explode l@(Liquido _ _) = [l]
explode i@(Inscatolato _ _ c) = i: explode c


instance Eq (Contenitore b) where
	Brick == Brick = True
	Flacone == Flacone = True
	Damigiana == Damigiana = True
	Pacchetto == Pacchetto = True
	Sacco == Sacco = True
	Sacchetto == Sacchetto = True
	Cassetta == Cassetta = True

instance Eq b => Eq (Confezione b) where
	Base == Base = True
	Solido q1 c1 == Solido q2 c2 = q2 == q1 && c2 == c1
	Liquido q1 c1 == Liquido q2 c2 = q2 == q1 && c2 == c1
	Inscatolato s1 n1 c1 == Inscatolato s2 n2 c2 =
		s1 == s2 && n1 == n2 && c1 == c2
	_ == _ = False

data Voce = forall a b c . (Name (Prezzato a b c) , UnitClass b, UnitClass c) =>  Voce (Prezzato a b c)


--------------- Esempi ------------------------------------------------------------------------

pollo =  AlPesoStimato
		(Scaffale Base $ Contato ("pollo","polli"))
		(700 :? Grammo, 1300 :? Grammo)
		(9.5 :? (Euro,Chilogrammo))

caffe =  AllaConfezione 
		(Scaffale (Solido (250 :? Grammo) Pacchetto)  $ Pesato (Singolare "caffè"))
		(2.3 :? Euro)
		
uova =  AllaConfezione 
		(Scaffale (Inscatolato Scatola 6 Base) $ Contato ("uovo", "uova"))
		(2 :? Euro)
olio =  AlVolume 
		(Scaffale Base $ Volumato (Singolare "olio"))
		(8.5 :? (Euro,Litro))

caki =  AlPesoStimato
		(Scaffale (Inscatolato Plateau 15 Base) $ Contato ("caco","cachi"))
		(3.5 :? Chilogrammo, 4.5 :? Chilogrammo)
		(3.25 :? (Euro,Chilogrammo))


