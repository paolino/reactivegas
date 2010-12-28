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
---------------------------------------------------------------------------------------------
instance Name b => Name (Quantità b) where
	singolare (x :? y) = fmap ((showFFloat (Just 2) (fromRational x) "" ++ " ") ++) $ if x == 1 then singolare y else plurale y
	plurale z =  singolare z

instance (UnitClass a, UnitClass b, Name a, Name b ) => Name (a,b) where
	singolare (x,y) = (++ (render $ " " :+: ADeterminativo &.& singolare2 y)) `fmap` singolare x 
	plurale (x,y) = (++ (render $ " " :+: ADeterminativo &.& singolare2 y)) `fmap`  plurale x 

instance Name (Bene Word Pesi) where
	singolare (Pesato x) = unmulti  x 
	plurale (Pesato x) = unmulti  x 

instance  Name (Bene Word Volumi) where
	singolare (Volumato x) = unmulti  x 
	plurale (Volumato x) = unmulti  x 

instance  Name (Bene Word Unità) where
	singolare (Contato (x,_)) =  x 
	plurale (Contato (_,x)) =  x 

checkUnità 1 f _ = f
checkUnità _ _ g = g
nameInscatolato :: Name (Scaffale Word b) =>  Scaffale Word b -> (Morfato Costante (Morfato Contenente Scatolame))
nameInscatolato (Scaffale (Inscatolato s n c) b) = (Costante b'' :++ Contenente :++ s)
	where b'' = render $ " " :+: show n :+: " " :+: (checkUnità n singolare2 plurale2 $ Scaffale c b) 

instance Name (Scaffale Word Unità) where
	singolare (Scaffale Base b) = singolare b
	singolare i = singolare . nameInscatolato $ i	
	plurale (Scaffale Base b) = plurale b
	plurale i = plurale . nameInscatolato $ i

instance Name (Scaffale Word Pesi) where
	singolare (Scaffale Base b) = unmulti (Sfuso &.& singolare2 b)
	singolare (Scaffale (Solido q s) b) = singolare (Costante c :++ Contenente :++  s) where
		c = render $ " " :+: singolare q :+: " di " :+: plurale b
	singolare i = singolare . nameInscatolato $ i	
	plurale (Scaffale Base b) = unmulti (Sfuso &.& singolare2 b)
	plurale (Scaffale (Solido q s) b) = plurale (Costante c :++ Contenente :++  s) where
		c = render $ " " :+: singolare q :+: " di " :+: plurale b
	plurale i = plurale . nameInscatolato $ i

instance Name (Scaffale Word Volumi) where
	singolare (Scaffale Base b) = unmulti (Sfuso &.& singolare2 b)
	singolare (Scaffale (Liquido q s) b) = singolare (Costante c :++ Contenente :++  s) where
		c = render $ " " :+: singolare q :+: " di " :+: plurale b
	singolare i = singolare . nameInscatolato $ i	
	plurale (Scaffale Base b) = unmulti (Sfuso &.& singolare2 b)
	plurale (Scaffale (Liquido q s) b) = plurale (Costante c :++ Contenente :++  s) where
		c = render $ " " :+: singolare q :+: " di " :+: plurale b
	plurale i = plurale . nameInscatolato $ i

instance Name (Prezzato Word Pesi Pesi) where	
	singolare (AllaConfezione z@(Scaffale _ b) c q) = singolare 
		(Costante (render $ " al prezzo di " :+: singolare q :+: " per ogni " :+: singolare (Scaffale c b)) 
			:++  z)	
	singolare (AlPeso z q) = singolare (Costante (render $ " al prezzo di " :+: singolare q) :++  z)	
	plurale (AllaConfezione z@(Scaffale _ b) c q) = plurale 
		(Costante (render $ " al prezzo di " :+: singolare q :+: " per ogni " :+: singolare (Scaffale c b)) 
			:++  z)
	plurale (AlPeso z q) = plurale (Costante (render $ " al prezzo di " :+: singolare q) :++  z)	


instance Name (Prezzato Word Volumi Volumi) where
	singolare (AllaConfezione z@(Scaffale _ b) c q) = singolare 
		(Costante (render $ " al prezzo di " :+: singolare q :+: " per ogni " :+: singolare (Scaffale c b)) 
			:++  z)	
	singolare (AlVolume z q) = singolare (Costante (render $ " al prezzo di " :+: singolare q) :++  z)	
	plurale (AllaConfezione z@(Scaffale _ b) c q) = plurale 
		(Costante (render $ " al prezzo di " :+: singolare q :+: " per ogni " :+: singolare (Scaffale c b)) 
			:++  z)
	plurale (AlVolume z q) = plurale (Costante (render $ " al prezzo di " :+: singolare q) :++  z)	

instance Name (Prezzato Word Unità Unità) where
	singolare (AllaConfezione z@(Scaffale _ b) c q) = singolare 
		(Costante (render $ " al prezzo di " :+: singolare q :+: " per ogni " :+: singolare (Scaffale c b)) 
			:++  z)	
	plurale (AllaConfezione z@(Scaffale _ b) c q) = plurale 
		(Costante (render $ " al prezzo di " :+: singolare q :+: " per ogni " :+: singolare (Scaffale c b)) 
			:++  z)

instance Name (Prezzato Word Unità Pesi) where
	singolare (AlPesoStimato z c (q1,q2) p) = singolare 
		(Costante (render $ " al prezzo di " :+: singolare p :+: " con peso stimato di " 
			:+: Indeterminativo &.& singolare2 z :+: " da " :+: singolare q1 :+: " a " :+: singolare q2) 
			:++  z)
	plurale (AlPesoStimato z c (q1,q2) p) = plurale
		(Costante (render $ " al prezzo di " :+: singolare p :+: " con peso stimato di " 
			:+: Indeterminativo &.& singolare2 z :+: " da " :+: singolare q1 :+: " a " :+: singolare q2) 
			:++  z)
		

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

data A = A 
instance Polimorfo A where
	singolareA A x = let (y:ys) = unsex x in if vocale y then respect (("ad " ++),("ad " ++)) x else 
		respect (("a " ++),("a " ++)) x
	pluraleA A x = singolareA A x


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

forme x = scelte x "forma corretta"

uiBene 
	:: Monad m 
	=> (Bene Word Pesi -> Costruzione m b a) 
	-> (Bene Word Volumi -> Costruzione m b a) 
	-> (Bene Word Unità -> Costruzione m b a) 
	-> Costruzione m b a
uiBene kp kv kc = do 
	x <- scelte  [("peso",0),("volume",1),("unità",2)] "il bene si misura in"
	case x of 
		0 -> do 
			(l :: String) <- libero "nome comune del bene sfuso"
			l' <- forme (maschileFemminileSP Sfuso l)
			kp $ Pesato l'
		1 -> do
			(l :: String) <- libero "nome comune del bene sfuso"
			l' <- forme (maschileFemminileSP Sfuso l)
			kv $ Volumato l'
		2 -> do 
			(s :: String) <- libero "nome singolare del bene"
			s' <- forme (maschileFemminile (Singolare s))
			p <- libero "nome plurale del bene"
			p' <- forme (maschileFemminile (Plurale p))
			let z = Contato (s',p')	
			kc z


uiContenitore :: (Monad m ,Name (Bene Word c), Name c, Enum c, Bounded c)
	=> [Contenitore c]
	-> (Quantità c -> Contenitore c -> Confezione c)	
	-> (Scaffale Word c -> Costruzione m b (Scaffale Word c))
	-> (Scaffale Word c -> Costruzione m b a)
	-> Bene Word c
	-> Costruzione m b a
uiContenitore cs f ks kq  xp = do
	let b = render $ Determinativo &.& singolare2 xp
	y <- scelte (("nessuno",Nothing):map (render . singolare &&& Just ) cs) $ "tipo di contenitore per " ++ b
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

uiContato :: Monad m
	=> (Scaffale Word Unità -> Costruzione m b (Scaffale Word Unità))
	-> (Scaffale Word Unità -> Costruzione m b a)
	-> Bene Word Unità
	-> Costruzione m b a
uiContato ks kq xp = ks  (Scaffale Base xp) >>= kq


uiScatola :: forall m b c.  (Name (Scaffale Word c), Monad m)   
	=> Scaffale Word c
	-> Costruzione m b (Scaffale Word c)
uiScatola  g@(Scaffale c b) = do
	x <- scelte (("nessuno",Nothing): map (render . singolare &&& Just) [minBound .. maxBound]) $ 
		"ulteriore confezionamento intorno " ++ render (A &.& Indeterminativo &.& singolare2 g)
	case x of 
		Nothing -> return $ Scaffale c b
		Just s -> do 
			n <- libero . render $ "numero di " :+: Contenuto &.& plurale2 (Scaffale c b) :+: " in "
				:+: Indeterminativo &.& singolare2 s
			uiScatola $ Scaffale (Inscatolato s n c) b
 
uiPrezzaPesato :: Monad m
	=> Scaffale Word Pesi
	-> Costruzione m b (Voce Word)
uiPrezzaPesato z@(Scaffale Base _) = uiAllaMisura z AlPeso
uiPrezzaPesato z = join $  scelte [("alla confezione", uiAllaConfezione z),
	("al peso", uiAllaMisura z AlPeso)] "prezzo del bene relativo"
uiPrezzaVolumato :: Monad m
	=> Scaffale Word Volumi
	-> Costruzione m b (Voce Word)
uiPrezzaVolumato z@(Scaffale Base _) = uiAllaMisura z AlVolume
uiPrezzaVolumato z = join $  scelte [("alla confezione", uiAllaConfezione z),
	("al volume", uiAllaMisura z AlVolume)] "prezzo del bene relativo" 
uiPrezzaContato :: Monad m
	=> Scaffale Word Unità
	-> Costruzione m b (Voce Word)
uiPrezzaContato z@(Scaffale Base _) =  join $  scelte [(render $ (ADeterminativo &.& singolare2 z), uiAllaConfezione z),
	("al peso stimato di " ++ render (Indeterminativo &.& singolare2 z), uiAlPesoStimato z)] "prezzo del bene relativo"
uiPrezzaContato z = join $  scelte [("alla confezione", uiAllaConfezione z),
	("al peso stimato di un confezionamento", uiAlPesoStimato z)] "prezzo del bene relativo" 

uiAllaMisura :: (Monad m, Name (Scaffale Word c), UnitClass c, 
	Name (Bene Word c), Name c, Enum c, Bounded c, Name (Prezzato Word c c)) 
	=> Scaffale Word c
	-> (Scaffale Word c -> Quantità (Denaro,c) -> Prezzato Word c c)
	-> Costruzione m b (Voce Word)

uiAllaMisura z@(Scaffale c b) f = do 
	x <- scelte (map (render . singolare &&& id) [minBound .. maxBound]) $ 
		"unità di misura relativa al prezzo" 
	(y :: Float) <- libero . render $ "prezzo in euro di " 
		:+: Indeterminativo &.& singolare2 x
		:+: " di " :+: singolare b
	return $ fromJust $ mkVoce $ f z (toRational y :? (Euro,x))

uiAlPesoStimato z@(Scaffale c b)  = do
	x <- scelte (map (render . singolare &&& id) [minBound .. maxBound]) $ 
		"unità di misura relativa al prezzo" 
	(y :: Float) <- libero . render $ "prezzo in euro di " 
		:+: Indeterminativo &.& singolare2 x
		:+: " di " :+: plurale b
	let cs = explode c
	c' <- case cs of
		[c] -> return c	
		cs -> scelte  (map ((\c -> render $ Indeterminativo &.& singolare2 (Scaffale c b)) &&& id) cs) 
			"confezione da stimare in peso"
	(pm :: Float) <- libero . render $ "peso minimo di " :+: Indeterminativo &.& singolare2 (Scaffale c' b) :+:
		" in " :+: plurale x

	(pd :: Float) <- libero . render $ "peso massimo di " :+: Indeterminativo &.& singolare2 (Scaffale c' b) :+:
		" in " :+: plurale x
	return $ fromJust $ mkVoce $ AlPesoStimato z c' (toRational pm :? x, toRational pd :? x) 
		(toRational y :? (Euro,x))


uiAllaConfezione :: (Monad m, Name (Scaffale Word c), UnitClass c, Name (Prezzato Word c c)) 
	=> Scaffale Word c
	-> Costruzione m b (Voce Word)
uiAllaConfezione z@(Scaffale c b) = do
	let cs = explode c
	c' <- case cs of
		[c] -> return c	
		cs -> scelte  (map (render . singolare . flip Scaffale b  &&& id) cs) 
			"confezione da prezzare"
	(y :: Float) <- libero . render $ "prezzo in euro di " :+: Indeterminativo &.& singolare2 (Scaffale c' b)
	return $ fromJust $ mkVoce $ AllaConfezione z c' (toRational y :? Euro) 


ui = uiBene 	(uiPesato uiScatola uiPrezzaPesato) 
		(uiVolumato uiScatola uiPrezzaVolumato) 
		(uiContato uiScatola uiPrezzaContato)

main = svolgi ui >>= interazione >>= \(Voce x) -> putStrLn (render $ singolare x)

