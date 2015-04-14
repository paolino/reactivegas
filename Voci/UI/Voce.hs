{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables, FlexibleContexts, GADTs, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}
module Voci.UI.Voci where

import Prelude hiding (Word)
import Data.Typeable
import Data.Dynamic
import Data.Maybe
import Control.Arrow
import Control.Monad (join, liftM2)
import Lib.Units
import Lib.Passo
-- import Lib.Console
import Lib.NaturalLanguage
import Lib.Response
import Numeric

import Voci.Data
import Voci.Beni
import Voci.Names
import Voci.Language
import Voci.Quantita
import Voci.Boxes
import Voci.Compare

------------------------------------------- interfaccia utente ------------------------------------
ftr :: Float -> Rational
ftr = toRational
--------- disambigua ----------------
forma x = scelte  x  $ ResponseOne "forma corretta"

disambigua z (x,y) = forma $ map p $ liftM2 (,) [Maschile,Femminile] [Maschile,Femminile] 
		where p (f,g) = (render (z &.& Singolare (f x)) ++ 
			", " ++ render (z &.& Plurale (g y)),(f x,g y))

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
	let	peso = do 
			(l :: String) <- libero  $ ResponseOne "nome comune del bene sfuso"
			l' <- disambiguaSP Determinativo l
			kp $ Pesato (PWord l')
		volume = do
			(l :: String) <- libero  $ ResponseOne "nome comune del bene sfuso"
			l' <- disambiguaSP Determinativo l
			kv $ Volumato (VWord l')
		unità = do 
			(s :: String) <- libero  $ ResponseOne "nome singolare del bene"
			p <- libero  $ ResponseOne "nome plurale del bene"
			(s',p') <- disambigua Indeterminativo (s,p)
			kc $ Contato (UWord (s',p'))
	join $ flip (scelte ) (ResponseOne  "descrizione del bene in") [
		("peso",peso),
		("volume",volume),
		("unità",unità)]


-- decide il contenitore per il bene sfuso
uiContenitore :: (Monad m ,Name (BBene c), Name c, Enum c, Bounded c)
	=> (BBene c -> Molteplicita Word)
	-> [(String, Quantità c -> Contenitore c)]
	-> Maybe (BBene c -> Costruzione m b a) -- cosa fare con il bene sfuso
	-> (Confezionamento c -> BBene c -> Costruzione m b a) -- cosa fare col bene nel contenitore 
	-> BBene c
	-> Costruzione m b a
uiContenitore mo cs ksfuso kcontenuto xp = do
	y <- scelte  ((if isJust ksfuso  then [("<nessuno>",Nothing)] else []) ++ map (second Just) cs) $ ResponseOne  
		$ render $ "contenitore per " :+:  Determinativo &.& mo xp
	case y of 
		Nothing -> fromJust ksfuso xp 
		Just f -> do 	
			z <- scelte  (map (render . singolare &&& id) [minBound .. maxBound]) $ ResponseOne  $ 
				render $ "unità di misura per il bene nel contenitore"
			(q :: Float) <- libero  . ResponseOne $ render $ "capienza del contenitore espressa in " 
				:+: plurale z 
			kcontenuto (Primo . f $ ftr q :? z) xp 
uiContenitorePesato
  :: Monad m =>
     (BBene Pesi -> Costruzione m b a)
     -> (Confezionamento Pesi -> BBene Pesi -> Costruzione m b a)
     -> BBene Pesi
     -> Costruzione m b a
uiContenitorePesato  = uiContenitore fromPesato 
	[("pacchetto",Pacchetto), ("sacco",Sacco), ("sacchetto", Sacchetto) , ("cassetta",Cassetta)] . Just 
uiContenitoreVolumato
  :: Monad m 
	=>  (BBene Volumi -> Costruzione m b a)
	-> (Confezionamento Volumi -> BBene Volumi -> Costruzione m b a)
	-> BBene Volumi
	-> Costruzione m b a

uiContenitoreVolumato  = uiContenitore 
	fromVolumato 
	[("brick",Brick),("bottiglia",Bottiglia),("flacone", Flacone),("damigiana", Damigiana)] . Just

uiContenitoreUnitario
  :: Monad m =>
     (BBene Unità -> Costruzione m b a)
     -> (Confezionamento Unità -> BBene Unità -> Costruzione m b a)
     -> BBene Unità
     -> Costruzione m b a

scatolame = [("scatola",Scatola),("scatolone",Scatolone),("plateau",Plateau),("sacchetto",Sacchetto)]
uiContenitoreUnitario = uiContenitore singolare2 scatolame . Just

-- decide l'inscatolamento
uiScatola ::   (Monad m, Name (Contenitore c))
	=> (Confezionamento c -> BBene c -> Costruzione m b a) 
	-> Confezionamento c 
	-> BBene c 
	-> Costruzione m b a

uiScatola kcontenitore c xp = do
	y <- scelte  (("<nessuno>",Nothing):map (second Just) scatolame) $ ResponseOne  
		$ render $ "confezionamento per " :+:  Determinativo &.& singolare2 c 
	case y of 
		Nothing -> kcontenitore c xp
		Just f -> do 	
			(q :: Float) <- libero  . ResponseOne $ render $ "numero di " 
				:+: plurale c :+: " nella confezione"
			uiScatola kcontenitore (Inscatolato (f $ ftr q :? Unità) c) xp

uiAlPezzo :: Monad m 
	=> BBene Unità
	-> Costruzione m b (BVoce Unità Unità Sfuso)
uiAlPezzo xp = do 
	c <- libero  . ResponseOne . render $ "costo per " :+: Indeterminativo &.& singolare2 xp
	return $ AlPezzo xp (ftr c :? (Euro,Unità))

uiAlPezzoOPezzoStimatoD xp = join . scelte 
		[(render $ ADeterminativo &.& singolare2 xp,BoxVoce `fmap` uiAlPezzo xp),
		(render $ "al peso stimato di " :+: Indeterminativo &.& singolare2 xp,BoxVoce `fmap` uiAlPezzoStimato xp)
		] $ ResponseOne "prezzo espresso"


uiAlPezzoStimato :: Monad m 
	=> BBene Unità
	-> Costruzione m b (BVoce Unità Pesi Sfuso)
uiAlPezzoStimato xp = do
	u <- scelte  (map (render . singolare &&& id) [minBound .. maxBound]) . ResponseOne . render $ 
		"unità di misura per la stima in peso di " :+: Indeterminativo &.& singolare2 xp
	p0 <- libero  . ResponseOne . render $ "peso minimo di " :+: Indeterminativo &.& singolare2 xp
		:+: " in " :+: plurale u
	p1 <- libero  . ResponseOne . render $ "peso massimo di " :+: Indeterminativo &.& singolare2 xp 
		:+: " in " :+: plurale u
	c <- libero  . ResponseOne . render $ "prezzo in euro per " :+: Indeterminativo &.& singolare2 u 
			:+: " di " :+: plurale2 xp
	return $ AlPezzoStimato xp (ftr p0 :? (u,Unità),ftr p1 :? (u,Unità)) (ftr c :? (Euro,u))

uiAllaConfezioneStimata c xp =  do
	u <- scelte  (map (render . singolare &&& id) [minBound .. maxBound]) . ResponseOne . render $ 
		"unità di misura per la stima in peso di " :+: Indeterminativo &.& singolare2 c :+: " di " 
		:+: plurale xp
	p0 <- libero  . ResponseOne . render $ "peso minimo di " :+: Indeterminativo &.& singolare2 c :+: " di " 
		:+: plurale xp 
		:+: " in " :+: plurale u
	p1 <- libero  . ResponseOne . render $ "peso massimo di " :+: Indeterminativo &.& singolare2 c :+: " di " 
		:+: plurale xp 
		:+: " in " :+: plurale u
	p <- libero  . ResponseOne . render $ "prezzo in euro per " :+: Indeterminativo &.& singolare2 u 
			:+: " di " :+: plurale2 xp
	return $ AllaConfezioneStimata c xp (ftr p0 :? (u,Unità),ftr p1 :? (u,Unità)) (ftr p :? (Euro,u))

uiAllaConfezioneOConfezioneStimataD c xp = join . scelte 
		[(render $ ADeterminativo &.& singolare2 c :+: " di " :+: plurale xp ,
			BoxVoce `fmap` uiAllaConfezione plurale c xp),
		(render $ "al peso stimato di " :+: Indeterminativo &.& singolare2 c :+: " di " :+: plurale xp,
			BoxVoce `fmap` uiAllaConfezioneStimata c xp)
		] $ ResponseOne "prezzo espresso"

uiAlPeso :: Monad m
	=> BBene Pesi
	-> Costruzione m b (BVoce Pesi Pesi Sfuso)
uiAlPeso xp = do
	u <- scelte  (map (render . singolare &&& id) [minBound .. maxBound]) . ResponseOne . render $ 
		"unità di misura relativa al prezzo " :+: DiDeterminativo &.& WSfuso &.& fromPesato xp
	c <- libero  . ResponseOne . render $ "prezzo in euro per " :+: Indeterminativo &.& singolare2 u 
			:+: " di " :+: WSfuso &.& fromPesato xp
	return $ AlPeso xp (ftr c :? (Euro,u))
uiAlPesoD = fmap BoxVoce . uiAlPeso


uiAlVolume :: Monad m
	=> BBene Volumi
	-> Costruzione m b (BVoce Volumi Volumi Sfuso)
uiAlVolume xp = do
	u <- scelte  (map (render . singolare &&& id) [minBound .. maxBound]) . ResponseOne . render $ 
		"unità di misura relativa al prezzo " :+: DiDeterminativo &.& WSfuso &.& fromVolumato xp
	c <- libero  . ResponseOne . render $ "prezzo in euro per " :+: Indeterminativo &.& singolare2 u 
			:+: " di " :+: WSfuso &.& fromVolumato xp
	return $ AlVolume xp (ftr c :? (Euro,u))
uiAlVolumeD = fmap BoxVoce . uiAlVolume

uiAlPesoConfezionato c xp = do
	u <- scelte  (map (render . singolare &&& id) [minBound .. maxBound]) . ResponseOne . render $ 
		"unità di misura relativa al prezzo " :+: DiDeterminativo &.& WSfuso &.& fromPesato xp
	p <- libero  . ResponseOne . render $ "prezzo in euro per " :+: Indeterminativo &.& singolare2 u 
			:+: " di " :+: fromPesato xp
	return $ AlPesoConfezionato c xp (ftr p :? (Euro,u))
uiAlVolumeConfezionato c xp = do
	u <- scelte  (map (render . singolare &&& id) [minBound .. maxBound]) . ResponseOne . render $ 
		"unità di misura relativa al prezzo " :+: DiDeterminativo &.& WSfuso &.& fromVolumato xp
	p <- libero  . ResponseOne . render $ "prezzo in euro per " :+: Indeterminativo &.& singolare2 u 
			:+: " di " :+: fromVolumato xp
	return $ AlVolumeConfezionato c xp (ftr p :? (Euro,u))

uiAlVolumeConfezionatoOAllaConfezioneD c xp = join . scelte 
		[(render $ ADeterminativo &.& singolare2 c :+: " di " :+: fromVolumato xp ,
			BoxVoce `fmap` uiAllaConfezione fromVolumato c xp),
		(render $ "al volume " :+: DiDeterminativo &.& fromVolumato xp,BoxVoce `fmap` uiAlVolumeConfezionato c xp)
		] $ ResponseOne "prezzo espresso"

uiAlPesoConfezionatoOAllaConfezioneD c xp = join . scelte 
		[(render $ ADeterminativo &.& singolare2 c :+: " di " :+: fromPesato xp ,
			BoxVoce `fmap` uiAllaConfezione fromPesato c xp),
		(render $ "al peso " :+: DiDeterminativo &.& fromPesato xp,BoxVoce `fmap` uiAlPesoConfezionato c xp)
		] $ ResponseOne "prezzo espresso"


uiAllaConfezione mo c xp = do
	p <- libero  . ResponseOne . render $ "prezzo in euro per " :+: Indeterminativo &.& singolare2 c 
		:+: " di " :+: mo xp
	return $ AllaConfezione c xp (ftr p :? (Euro,Unità))
uiAllaConfezioneD mo c = fmap BoxVoce . uiAllaConfezione mo c

ui = uiBene 
	(uiContenitorePesato  uiAlPesoD 	(uiScatola uiAlPesoConfezionatoOAllaConfezioneD)) 
	(uiContenitoreVolumato uiAlVolumeD 		(uiScatola uiAlVolumeConfezionatoOAllaConfezioneD)) 
	(uiContenitoreUnitario uiAlPezzoOPezzoStimatoD 	(uiScatola (uiAllaConfezioneOConfezioneStimataD)))

class  ModificaPrezzo m b a where
	modificaPrezzo :: a -> Costruzione m b a

instance Monad m =>ModificaPrezzo m b (BVoce Pesi Pesi Sfuso) where
	modificaPrezzo (AlPeso x q) = uiAlPeso x
instance Monad m => ModificaPrezzo m b (BVoce Unità Unità Sfuso) where
	modificaPrezzo (AlPezzo x q) = uiAlPezzo x
instance Monad m => ModificaPrezzo m b (BVoce  Volumi Volumi Sfuso) where
	modificaPrezzo (AlVolume x q) = uiAlVolume x
