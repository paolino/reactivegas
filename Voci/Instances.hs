
{-# LANGUAGE TypeSynonymInstances, Rank2Types, ScopedTypeVariables, UndecidableInstances, StandaloneDeriving, 
	FlexibleInstances, FlexibleContexts, GADTs, OverlappingInstances #-}
{-
instance (Show (Contenitore b), Show a) => Show (Voce a b c d) where
	show (AllaConfezione s q) = show ("AC",s,q)
	show (AlPezzo s q) = show ("AUS",s,q)
	show (AlPesoConfezionato s q) = show ("AP",s,q)
	show (AlPesoSfuso s q) = show ("APS",s,q)
	show (AlVolume s q) = show ("AV",s,q)
	show (AlPesoStimato s qs q) = show ("ASS",s,qs,q)

instance Show (Confezione b) where
	show Numerico = show "B"
	show (Solido q cp) = show ("S",q,cp)
	show (Liquido q cl) = show ("L",q,cl)
	show (Inscatolato s i c) = show ("I",s,i,c)

instance Read (Confezione Pesi) where
	readsPrec _ x = case reads x of
			[(("S",q,cp),r)] -> [(Solido q cp,r)]
			_ -> case reads x of
				[(("I",s,i,c),r)] -> [(Inscatolato s i c,r)]
				_ -> []
instance Read (Confezione Volumi) where
	readsPrec _ x = case reads x of
			[(("L",q,cp),r)] -> [(Liquido q cp,r)]
			_ -> case reads x of
				[(("I",s,i,c),r)] -> [(Inscatolato s i c,r)]
				_ -> []
instance Read (Confezione Unità) where
	readsPrec _ x = case reads x of
		[("B",r)] -> [(Numerico,r)]
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
	Numerico == Numerico = True
	Solido q1 c1 == Solido q2 c2 = q2 == q1 && c2 == c1
	Liquido q1 c1 == Liquido q2 c2 = q2 == q1 && c2 == c1
	Inscatolato s1 n1 c1 == Inscatolato s2 n2 c2 =
		s1 == s2 && n1 == n2 && c1 == c2
	_ == _ = False

instance Eq a =>  Eq (Bene a b ) where
	Pesato x == Pesato y = x == y
	Volumato x == Volumato y = x == y
	Contato x == Contato y = x == y

deriving instance Eq a => Eq (Confezionato a Pesi)
deriving instance Eq a => Eq (Confezionato a Volumi)
deriving instance Eq a => Eq (Confezionato a Unità)
deriving instance Eq a => Eq (Sfuso a Pesi)
deriving instance Eq a => Eq (Sfuso a Volumi)
deriving instance Eq a => Eq (Sfuso a Unità)

instance Read a => Read (Prezzato a Pesi Pesi (Confezionato a Pesi)) where
	readsPrec _ x = case reads x of
		[(("AP",s,q),r)] -> [(AlPesoConfezionato s q,r)]
		_ -> case reads x of
			[(("AC",s,q),r)] -> [(AllaConfezione s q,r)]
			_ -> []
instance Read a => Read (Prezzato a Pesi Pesi (Sfuso a Pesi)) where
	readsPrec _ x = case reads x of
		[(("APS",s,q),r)] -> [(AlPesoSfuso s q,r)]
		_ -> []


instance Read a => Read (Prezzato a Volumi Volumi (Confezionato a Volumi)) where
	readsPrec _ x = case reads x of
		[(("AV",s,q),r)] -> [(AlVolume s q,r)]
		_ -> case reads x of
			[(("AC",s,q),r)] -> [(AllaConfezione s q,r)]
			_ -> []

instance Read a => Read (Prezzato a Unità Unità (Confezionato a Unità)) where
	readsPrec _ x = case reads x of
		[(("AC",s,q),r)] -> [(AllaConfezione s q,r)]
		_ -> []
instance Read a => Read (Prezzato a Unità Unità (Sfuso a Unità)) where
	readsPrec _ x = case reads x of
		[(("AUS",s,q),r)] -> [(AlPezzoSfuso s q,r)]
		_ -> []


instance Read a => Read (Prezzato a Unità Pesi (Confezionato a Unità)) where
	readsPrec _ x = case reads x of
		[(("ASS",s,qs,q),r)] -> [(AlPesoStimato s qs q,r)]
		_ -> []
instance Read a => Read (Prezzato a Unità Pesi (Sfuso a Unità)) where
	readsPrec _ x = case reads x of
		[(("APSS",s,qs,q),r)] -> [(AlPesoStimatoSfuso s qs q,r)]
		_ -> []

instance Eq a => Eq (Prezzato a Pesi Pesi (Confezionato a Pesi)) where
	AlPesoConfezionato s q == AlPesoConfezionato s' q' = s' == s && q' == q
	AllaConfezione s q == AllaConfezione s' q' = s' == s && q' == q

instance Eq a => Eq (Prezzato a Pesi Pesi (Sfuso a Pesi)) where
	AlPesoSfuso s q == AlPesoSfuso s' q' = s' == s && q' == q

instance Eq a => Eq (Prezzato a Volumi Volumi (Confezionato a Volumi)) where
	AlVolume s q == AlVolume s' q' = s' == s && q' == q
	AllaConfezione s q == AllaConfezione s' q' = s' == s && q' == q

instance Eq a => Eq (Prezzato a Unità Unità (Confezionato a Unità)) where
	AllaConfezione s q == AllaConfezione s' q' = s' == s && q' == q

instance Eq a => Eq (Prezzato a Unità Unità (Sfuso a Unità)) where
	AlPezzoSfuso s q == AlPezzoSfuso s' q' = s' == s && q' == q

instance Eq a => Eq (Prezzato a Unità Pesi (Confezionato a Unità)) where
	AlPesoStimato  s qs q == AlPesoStimato s' qs' q' = s' == s && q' == q && qs' == qs

instance Eq a => Eq (Prezzato a Unità Pesi (Sfuso a Unità)) where
	AlPesoStimatoSfuso  s qs q == AlPesoStimatoSfuso s' qs' q' = s' == s && q' == q && qs' == qs



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

instance Eq (BWord Pesi) where
	PWord x == PWord y = x == y
	
instance Eq (BWord Volumi) where
	VWord x == VWord y = x == y

instance Eq (BWord Unità) where
	UWord x == UWord y = x == y

{-	
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
-}

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

nameInscatolato :: Name (BConfezionato b) =>  BConfezionato b -> (Morfato Costante (Morfato Costante Scatolame))
nameInscatolato (Confezionato (Inscatolato s n c) b) = Costante x :++ Costante " di" :++ s
	where 
		x = render $ " " :+: show n :+: " " :+: (checkUnità n singolare2 plurale2 $ Confezionato c b) 
		checkUnità 1 f _ = f
		checkUnità _ _ g = g

nameContenitore s q b = Costante c :++ Costante " da" :++  s where
	c = render $ " " :+: singolare q :+: " di " :+: plurale b

instance Name (BConfezionato Unità) where
	singolare (Confezionato Numerico b) = singolare b
	singolare i = singolare . nameInscatolato $ i	
	plurale (Confezionato Numerico b) = plurale b
	plurale i = plurale . nameInscatolato $ i

multiSfuso b = unmulti (WSfuso &.& singolare2 b)

instance Name (BConfezionato Pesi) where
	singolare (Confezionato (Solido q s) b) = singolare $ nameContenitore s q b 
	singolare i = singolare . nameInscatolato $ i	
	plurale (Confezionato (Solido q s) b) = plurale $ nameContenitore s q b
	plurale i = plurale . nameInscatolato $ i

instance Name (BConfezionato Volumi) where
	singolare (Confezionato (Liquido q s) b) = singolare $ nameContenitore s q b
	singolare i = singolare . nameInscatolato $ i	
	plurale (Confezionato (Liquido q s) b) = plurale $ nameContenitore s q b
	plurale i = plurale . nameInscatolato $ i

instance Name (BBene b) => Name (BSfuso b) where
	singolare (Sfuso b) = multiSfuso b
	plurale (Sfuso b) = multiSfuso b

class GContenitore a where
	contenitore :: (forall g . Name g => g -> b) -> a -> b	

contenitore' :: Name (BBene b) => (forall g . Name g => g -> c) -> BConfezionato b -> c
contenitore' f (Confezionato Numerico b) = errore "doveva essere sfuso"
contenitore' f (Confezionato (Inscatolato c _ _) _) = f c
contenitore' f (Confezionato _ _) = error "uso improprio di contenitore'"

instance GContenitore (BConfezionato Pesi) where
	contenitore f (Confezionato (Solido _ c) _) = f c
	contenitore f s = contenitore' f s

instance GContenitore (BConfezionato Volumi) where
	contenitore f (Confezionato (Liquido _ c) _) = f c
	contenitore f s = contenitore' f s

instance GContenitore (BConfezionato Unità) where
	contenitore f s = contenitore' f s

nameConfezione z@(Confezionato c b) q = 
	Costante (render $ " al prezzo di " :+: singolare q :+: " " :+: ADeterminativo &.& Singolare (contenitore singolare z)) :++  z

nameAllaMisura q z = Costante (render $ " al prezzo di " :+: singolare q) :++  z

instance Name (Prezzato (BWord Pesi) Pesi Pesi (BConfezionato Pesi)) where	
	singolare (AllaConfezione z@(Confezionato _ b) q) = singolare $ nameConfezione z q
	singolare (AlPesoConfezionato z q) = singolare $ nameAllaMisura q z 
	plurale (AllaConfezione z@(Confezionato _ b) q) = plurale $ nameConfezione z q
	plurale (AlPesoConfezionato z q) = plurale $ nameAllaMisura q z 

instance Name (Prezzato (BWord Volumi) Volumi Volumi (BConfezionato Volumi)) where
	singolare (AllaConfezione z@(Confezionato _ b) q) = singolare $ nameConfezione z q
	singolare (AlVolume z q) = singolare $ nameAllaMisura q z 
	plurale (AllaConfezione z@(Confezionato _ b) q) = plurale $ nameConfezione z q
	plurale (AlVolume z q) = plurale $ nameAllaMisura q z

instance Name (Prezzato (BWord Unità) Unità Unità (BConfezionato Unità)) where
	singolare (AllaConfezione z@(Confezionato _ b) q) = singolare $ nameConfezione z q
	plurale (AllaConfezione z@(Confezionato _ b) q) = plurale $ nameConfezione z q

nameStimato z (q1,q2) p = Costante c :++ z where
	c = render $ " al prezzo di " :+: singolare p :+: " con peso stimato di " 
		:+: Indeterminativo &.& singolare2 z :+: " da " :+: singolare q1 :+: " a " :+: singolare q2

instance Name (Prezzato (BWord Unità) Unità Pesi (BConfezionato Unità)) where
	singolare (AlPesoStimato z (q1,q2) p) = singolare $ nameStimato z (q1,q2) p
	plurale (AlPesoStimato z (q1,q2) p) = plurale$ nameStimato z (q1,q2) p
-}
{-		
instance Name Commercio where
	singolare (Commercio x) = singolare x
	plurale (Commercio x) = plurale x

instance Eq Commercio where
	Commercio x == Commercio y = case cast x of 
		Just x' -> x' == y
		_ -> False

deriving instance Eq Voce
-}
