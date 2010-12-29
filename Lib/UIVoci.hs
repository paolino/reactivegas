{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, Rank2Types, GADTs, OverlappingInstances #-}

import Data.Maybe
import Control.Arrow
import Control.Monad (join)
import Lib.Units
import Lib.Passo
import Lib.Console
import Lib.NaturalLanguage
import Lib.Response
import Numeric

import Lib.Voci

------------------------------------ datatype reale dei nomi dei beni -------------------------
data BWord a where
	PWord :: Molteplicita Word -> BWord Pesi
	VWord :: Molteplicita Word -> BWord Volumi
	UWord :: (Word,Word) -> BWord Unità
	 
type BBene b = Bene (BWord b) b
type BScaffale b = Scaffale (BWord b) b

---------------------------------- parte specifica del linguaggio per le descrizioni ------------

data Sfuso = Sfuso
instance Polimorfo Sfuso where
	singolareA Sfuso (Maschile x) = Maschile $ x ++ " sfuso"
	singolareA Sfuso (Femminile x) = Femminile $ x ++ " sfusa"
	pluraleA Sfuso (Maschile x) = Maschile $ x ++ " sfusi"
	pluraleA Sfuso (Femminile x) = Femminile $ x ++ " sfuse"

data Misurato = Misurato 
instance Polimorfo Misurato where
	singolareA Misurato (Maschile x) = Maschile $ x ++ " misurato"
	singolareA Misurato (Femminile x) = Femminile $ x ++ " misurata"
	pluraleA Misurato (Maschile x) = Maschile $ x ++ " misurati"
	pluraleA Misurato (Femminile x) = Femminile $ x ++ " misurate"

data Contenuto = Contenuto 
instance Polimorfo Contenuto where
	singolareA Contenuto (Maschile x) = Maschile $ x ++ " contenuto"
	singolareA Contenuto (Femminile x) = Femminile $ x ++ " contenuta"
	pluraleA Contenuto (Maschile x) = Maschile $ x ++ " contenuti"
	pluraleA Contenuto (Femminile x) = Femminile $ x ++ " contenute"

data Contenente = Contenente 
instance Polimorfo Contenente where
	singolareA Contenente (Maschile x) = Maschile $ x ++ " contenente"
	singolareA Contenente (Femminile x) = Femminile $ x ++ " contenente"
	pluraleA Contenente (Maschile x) = Maschile $ x ++ " contenenti"
	pluraleA Contenente (Femminile x) = Femminile $ x ++ " contenenti"


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
		
------------------------------------------- interfaccia utente ------------------------------------

--------- disambigua ----------------
forma x = scelte x "forma corretta"

disambigua (x,y) = forma [
		("un " ++ x ++ ", " ++ onstz y "degli " "dei " ,(Maschile x,Maschile y)),
		("un " ++ x ++ ", " ++ "delle " ++ y ,(Maschile x,Femminile y)),
		("un" ++ (if vocale (head x) then "'" else "a ") ++ x ++ ", " ++ onstz y "degli " "dei ", 
			(Femminile x,Maschile y)),
		("un" ++ (if vocale (head x) then "'" else "a ") ++ x ++ ", " ++ "delle " ++ y, 
			(Femminile x,Femminile y))
		] 

disambiguaSP y x = forma [
		(unsex $ singolare (y :++ maschile x),Singolare $ Maschile x),
		(unsex $ singolare (y :++ femminile x), Singolare $ Femminile x),
		(unsex $ plurale (y :++ maschile x), Plurale $ Maschile x),
		(unsex $ plurale (y :++ femminile x), Plurale $ Femminile x)
		] 

--------------------------------------

-- decide la misura che descrive il bene
uiBene 	:: Monad m 
	=> (BBene Pesi -> Costruzione m b a) 
	-> (BBene Volumi -> Costruzione m b a) 
	-> (BBene Unità -> Costruzione m b a) 
	-> Costruzione m b a
uiBene kp kv kc = do 
	x <- scelte  [("peso",0),("volume",1),("unità",2)] "descrizione del bene in"
	case x of 
		0 -> do 
			(l :: String) <- libero "nome comune del bene sfuso"
			l' <- disambiguaSP Determinativo l
			kp $ Pesato (PWord l')
		1 -> do
			(l :: String) <- libero "nome comune del bene sfuso"
			l' <- disambiguaSP Determinativo l
			kv $ Volumato (VWord l')
		2 -> do 
			(s :: String) <- libero "nome singolare del bene"
			p <- libero "nome plurale del bene"
			(s',p') <- disambigua (s,p)
			kc $ Contato (UWord (s',p'))


-- decide il contenitore per il bene sfuso
uiContenitore :: (Monad m ,Name (BBene c), Name c, Enum c, Bounded c)
	=> [Contenitore c]
	-> (Quantità c -> Contenitore c -> Confezione c)	
	-> (BScaffale c -> Costruzione m b (BScaffale c))
	-> (BScaffale c -> Costruzione m b a)
	-> BBene c
	-> Costruzione m b a
uiContenitore cs f ks kq  xp = do
	let b = render $ Determinativo &.& Sfuso &.& singolare2 xp
	y <- scelte (("nessuno",Nothing):map (render . singolare &&& Just ) cs) $ "contenitore per " ++ b
	case y of 
		Nothing -> kq (Scaffale Base xp)
		Just y' -> do 
			let 	b' = render $ b :+: " " :+: InDeterminativo &.& plurale2 y'
			z <- scelte (map (render . singolare &&& id) [minBound .. maxBound]) $ 
				"unità di misura per " ++ b' 
			(q :: Float) <- libero . render $ "quantità di " :+: singolare xp :+: " " 
				:+: InDeterminativo &.& singolare2 y' 
				:+: " espressa in " :+: plurale z 
			ks  (Scaffale (f (toRational q :? z)  y') xp) >>= kq

uiPesato  = uiContenitore [Pacchetto, Sacco, Sacchetto, Cassetta] Solido 
uiVolumato  = uiContenitore [Brick,Flacone,Damigiana] Liquido 

-- i beni unitari passano all'inscatolamento
uiContato :: Monad m
	=> (BScaffale Unità -> Costruzione m b (BScaffale Unità))
	-> (BScaffale Unità -> Costruzione m b a)
	-> BBene Unità
	-> Costruzione m b a
uiContato ks kq xp = ks  (Scaffale Base xp) >>= kq


-- decide l'inscatolamento
uiScatola :: (Name (BScaffale c), Monad m)   
	=> BScaffale c
	-> Costruzione m b (BScaffale c)
uiScatola  g@(Scaffale c b) = do
	x <- scelte (("nessuna",Nothing): map (render . singolare &&& Just) [minBound .. maxBound]) $ 
		"eventuale confezione contenente " ++ render (plurale g)
	case x of 
		Nothing -> return $ Scaffale c b
		Just s -> do 
			n <- libero . render $ "numero di " :+: Contenuto &.& plurale2 (Scaffale c b) :+: " in "
				:+: Indeterminativo &.& singolare2 s
			uiScatola $ Scaffale (Inscatolato s n c) b

-- prezzatura alla confezione
uiAllaConfezione :: (Monad m, Name (BScaffale c), UnitClass c, Name (Prezzato (BWord c) c c)) 
	=> BScaffale c
	-> Costruzione m b Voce
uiAllaConfezione z@(Scaffale c b) = do
	(y :: Float) <- libero . render $ "prezzo in euro di " :+: Indeterminativo &.& singolare2 (Scaffale c b)
	return $ Voce $  AllaConfezione z (toRational y :? Euro) 

-- prezzatura in unità di misura
uiAllaMisura :: (Monad m, Name (BScaffale c), UnitClass c, 
	Name (BBene c), Name c, Enum c, Bounded c, Name (Prezzato (BWord c) c c)) 
	=> BScaffale c
	-> (BScaffale c -> Quantità (Denaro,c) -> Prezzato (BWord c) c c)
	-> Costruzione m b Voce

uiAllaMisura z@(Scaffale c b) f = do 
	x <- scelte (map (render . singolare &&& id) [minBound .. maxBound]) $ 
		"unità di misura relativa al prezzo" 
	(y :: Float) <- libero . render $ "prezzo in euro di " 
		:+: Indeterminativo &.& singolare2 x
		:+: " di " :+: singolare b
	return $ Voce $  f z (toRational y :? (Euro,x))

-- prezzatura pesati
uiPrezzaPesato :: Monad m
	=> BScaffale Pesi
	-> Costruzione m b Voce
uiPrezzaPesato z@(Scaffale Base _) = uiAllaMisura z AlPeso
uiPrezzaPesato z@(Scaffale _ x) = join $ scelte [
	("di " ++ render (Indeterminativo &.& singolare2 z), uiAllaConfezione z),
	(render (DiDeterminativo &.& singolare2 (Scaffale Base x)), uiAllaMisura z AlPeso)] 
	"prezzo"

-- prezzatura volumati
uiPrezzaVolumato :: Monad m
	=> BScaffale Volumi
	-> Costruzione m b Voce
uiPrezzaVolumato z@(Scaffale Base _) = uiAllaMisura z AlVolume
uiPrezzaVolumato z @(Scaffale _ x) = join $ scelte [
		(render $ (ADeterminativo &.& singolare2 z), uiAllaConfezione z),
		("al volume " ++ render (DiDeterminativo &.& singolare2 (Scaffale Base x)), uiAllaMisura z AlVolume)] 
	"prezzo del bene relativo" 

-- prezzatura contati
uiPrezzaContato :: Monad m
	=> BScaffale Unità
	-> Costruzione m b Voce
uiPrezzaContato z =  join $ scelte [
		(render $ (ADeterminativo &.& singolare2 z), uiAllaConfezione z),
		("al peso stimato di " ++ render (Indeterminativo &.& singolare2 z), uiAlPesoStimato z)] 
	"prezzo del bene relativo"


-- prezzatura al peso stimato di una confezione o dell'unità di bene

uiAlPesoStimato :: Monad m => BScaffale Unità -> Costruzione m b Voce
uiAlPesoStimato z@(Scaffale c b)  = do
	x <- scelte (map (render . singolare &&& id) [minBound .. maxBound]) $ 
		"unità di misura relativa al prezzo" 
	(y :: Float) <- libero . render $ "prezzo in euro di " 
		:+: Indeterminativo &.& singolare2 x
		:+: " di " :+: plurale b
	(pm :: Float) <- libero . render $ "peso minimo di " :+: Indeterminativo &.& singolare2 (Scaffale c b) :+:
		" in " :+: plurale x

	(pd :: Float) <- libero . render $ "peso massimo di " :+: Indeterminativo &.& singolare2 (Scaffale c b) :+:
		" in " :+: plurale x
	return $ Voce $  AlPesoStimato z (toRational pm :? x, toRational pd :? x) 
		(toRational y :? (Euro,x))

-- creazione di una voce di bene
ui = uiBene 	(uiPesato uiScatola uiPrezzaPesato) 
		(uiVolumato uiScatola uiPrezzaVolumato) 
		(uiContato uiScatola uiPrezzaContato)

main = svolgi ui >>= interazione >>= \(Voce x) -> putStrLn (render $ plurale x)

