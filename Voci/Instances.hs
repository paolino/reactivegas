
{-# LANGUAGE TypeSynonymInstances, Rank2Types, ScopedTypeVariables, StandaloneDeriving, 
	FlexibleInstances, FlexibleContexts, GADTs, OverlappingInstances #-}
module Voci.Instances where

import Lib.NaturalLanguage -- (Name (..))
import Numeric (showFFloat)
import Lib.Units

import Voci.Core
import Voci.Language


------- serialization via Show and Read ---------------------------------------

instance (Show (Scaffale a c), Show a) => Show (Prezzato a b c) where
	show (AllaConfezione s q) = show ("AC",s,q)
	show (AlPeso s q) = show ("AP",s,q)
	show (AlVolume s q) = show ("AV",s,q)
	show (AlPesoStimato s qs q) = show ("ASS",s,qs,q)

deriving instance Show a => Show (Scaffale a Unità)
deriving instance Show a => Show (Scaffale a Pesi)
deriving instance Show a => Show (Scaffale a Volumi)
deriving instance Read a => Read (Scaffale a Unità)
deriving instance Read a => Read (Scaffale a Pesi)
deriving instance Read a => Read (Scaffale a Volumi)



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

instance Read a => Read (Prezzato a Unità Unità) where
	readsPrec _ x = case reads x of
		[(("AC",s,q),r)] -> [(AllaConfezione s q,r)]
		_ -> []

instance Read a => Read (Prezzato a Unità Pesi) where
	readsPrec _ x = case reads x of
		[(("ASS",s,qs,q),r)] -> [(AlPesoStimato s qs q,r)]
		_ -> []


------------------ con 'a' specificato ---------------------------------

instance Show (BWord a) where
	show (PWord x) = show ("PW",x)
	show (VWord x) = show ("VW",x)
	show (UWord x) = show ("UW",x)

instance Read (BWord Pesi) where
	readsPrec _ x = case reads x of
		[(("PW",x),r)] -> [(PWord x,r)]
		_ -> []
instance Read (BWord Volumi) where
	readsPrec _ x = case reads x of
		[(("VW",x),r)] -> [(VWord x,r)]
		_ -> []
instance Read (BWord Unità) where
	readsPrec _ x = case reads x of
		[(("UW",x),r)] -> [(UWord x,r)]
		_ -> []

instance Read Commercio where
	readsPrec _ x = case reads x of 
		[(z :: Prezzato (BWord Pesi) Pesi Pesi,r)] -> [(Commercio z,r)]
		_ -> case reads x of 
			[(z :: Prezzato (BWord Volumi) Volumi Volumi,r)] -> [(Commercio z,r)]
			_ -> case reads x of  
				[(z :: Prezzato (BWord Unità) Unità Unità,r)] -> [(Commercio z,r)]
				_ -> case reads x of  
					[(z :: Prezzato (BWord Unità) Unità Pesi,r)] -> [(Commercio z,r)]
					_ -> []

instance Show Commercio where
	show (Commercio z) = show z

deriving instance Show Voce
deriving instance Read Voce

------------------------- istanze di Name per le descrizioni --------------------------------

instance Name b => Name (Quantità b) where
	singolare (x :? y) = fmap ((showFFloat (Just 2) (fromRational x) "" ++ " ") ++) $ 
		if x == 1 then singolare y else plurale y
	plurale z =  singolare z


instance (UnitClass a, UnitClass b, Name a, Name b ) => Name (a,b) where
	singolare (x,y) = (++ (render $ " " :+: ADeterminativo &.& singolare2 y)) `fmap` singolare x 
	plurale (x,y) = (++ (render $ " " :+: ADeterminativo &.& singolare2 y)) `fmap`  plurale x 

instance Name (BBene Pesi) where
	singolare (Pesato (PWord x)) = unmulti  x 
	plurale (Pesato (PWord x)) = unmulti  x 

instance  Name (BBene Volumi) where
	singolare (Volumato (VWord x)) = unmulti  x 
	plurale (Volumato (VWord x)) = unmulti  x 

instance  Name (BBene Unità) where
	singolare (Contato (UWord (x,_))) =  x 
	plurale (Contato (UWord (_,x))) =  x 

instance Name (Contenitore b) where
	singolare Pacchetto = Maschile "pacchetto"
	singolare Sacco = Maschile "sacco"
	singolare Sacchetto = Maschile "sacchetto"
	singolare Cassetta = Femminile "cassetta"
	singolare Brick = Maschile "brick"
	singolare Flacone = Maschile "flacone"
	singolare Damigiana = Femminile "damigiana"
	plurale Pacchetto = Maschile "pacchetti"
	plurale Sacco = Maschile "sacchi"
	plurale Sacchetto = Maschile "sacchetti"
	plurale Cassetta = Femminile "cassette"
	plurale Brick = Maschile "brick"
	plurale Flacone = Maschile "flaconi"
	plurale Damigiana = Femminile "damigiane"

instance Name Scatolame where
	singolare Scatola 	= Femminile "scatola" 
	singolare Plateau 	= Maschile "plateau"
	singolare Scatolone 	= Maschile "scatolone"
	singolare Pallet 	= Maschile "pallet"
	plurale Scatola		= Femminile "scatole" 
	plurale Plateau         = Maschile "plateau"
	plurale Scatolone       = Maschile "scatoloni"
	plurale Pallet          = Maschile "pallets"

nameInscatolato :: Name (BScaffale b) =>  BScaffale b -> (Morfato Costante (Morfato Costante Scatolame))
nameInscatolato (Scaffale (Inscatolato s n c) b) = Costante x :++ Costante " di" :++ s
	where 
		x = render $ " " :+: show n :+: " " :+: (checkUnità n singolare2 plurale2 $ Scaffale c b) 
		checkUnità 1 f _ = f
		checkUnità _ _ g = g

nameContenitore s q b = Costante c :++ Costante " da" :++  s where
	c = render $ " " :+: singolare q :+: " di " :+: plurale b

instance Name (BScaffale Unità) where
	singolare (Scaffale Base b) = singolare b
	singolare i = singolare . nameInscatolato $ i	
	plurale (Scaffale Base b) = plurale b
	plurale i = plurale . nameInscatolato $ i

multiSfuso b = unmulti (Sfuso &.& singolare2 b)

instance Name (BScaffale Pesi) where
	singolare (Scaffale Base b) = multiSfuso b
	singolare (Scaffale (Solido q s) b) = singolare $ nameContenitore s q b 
	singolare i = singolare . nameInscatolato $ i	
	plurale (Scaffale Base b) = multiSfuso b
	plurale (Scaffale (Solido q s) b) = plurale $ nameContenitore s q b
	plurale i = plurale . nameInscatolato $ i

instance Name (BScaffale Volumi) where
	singolare (Scaffale Base b) = multiSfuso b
	singolare (Scaffale (Liquido q s) b) = singolare $ nameContenitore s q b
	singolare i = singolare . nameInscatolato $ i	
	plurale (Scaffale Base b) = multiSfuso b
	plurale (Scaffale (Liquido q s) b) = plurale $ nameContenitore s q b
	plurale i = plurale . nameInscatolato $ i


class GContenitore a where
	contenitore :: (forall g . Name g => g -> b) -> a -> b	

contenitore' :: (forall g . Name g => g -> c) -> BScaffale b -> c
contenitore' _ (Scaffale Base _) = error "bene sfuso"
contenitore' f (Scaffale (Inscatolato c _ _) _) = f c
contenitore' f (Scaffale _ _) = error "uso improprio di contenitore'"

instance GContenitore (BScaffale Pesi) where
	contenitore f (Scaffale (Solido _ c) _) = f c
	contenitore f s = contenitore' f s

instance GContenitore (BScaffale Volumi) where
	contenitore f (Scaffale (Liquido _ c) _) = f c
	contenitore f s = contenitore' f s

instance GContenitore (BScaffale Unità) where
	contenitore f s = contenitore' f s

nameConfezione z@(Scaffale c b) q = 
	Costante (render $ " al prezzo di " :+: singolare q :+: " " :+: ADeterminativo &.& Singolare (contenitore singolare z)) :++  z

nameAllaMisura q z = Costante (render $ " al prezzo di " :+: singolare q) :++  z

instance Name (Prezzato (BWord Pesi) Pesi Pesi) where	
	singolare (AllaConfezione z@(Scaffale _ b) q) = singolare $ nameConfezione z q
	singolare (AlPeso z q) = singolare $ nameAllaMisura q z 
	plurale (AllaConfezione z@(Scaffale _ b) q) = plurale $ nameConfezione z q
	plurale (AlPeso z q) = plurale $ nameAllaMisura q z 

instance Name (Prezzato (BWord Volumi) Volumi Volumi) where
	singolare (AllaConfezione z@(Scaffale _ b) q) = singolare $ nameConfezione z q
	singolare (AlVolume z q) = singolare $ nameAllaMisura q z 
	plurale (AllaConfezione z@(Scaffale _ b) q) = plurale $ nameConfezione z q
	plurale (AlVolume z q) = plurale $ nameAllaMisura q z

instance Name (Prezzato (BWord Unità) Unità Unità) where
	singolare (AllaConfezione z@(Scaffale _ b) q) = singolare $ nameConfezione z q
	plurale (AllaConfezione z@(Scaffale _ b) q) = plurale $ nameConfezione z q

nameStimato z (q1,q2) p = Costante c :++ z where
	c = render $ " al prezzo di " :+: singolare p :+: " con peso stimato di " 
		:+: Indeterminativo &.& singolare2 z :+: " da " :+: singolare q1 :+: " a " :+: singolare q2

instance Name (Prezzato (BWord Unità) Unità Pesi) where
	singolare (AlPesoStimato z (q1,q2) p) = singolare $ nameStimato z (q1,q2) p
	plurale (AlPesoStimato z (q1,q2) p) = plurale$ nameStimato z (q1,q2) p
		



