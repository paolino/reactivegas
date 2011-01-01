{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables, FlexibleContexts, GADTs #-}
module Voci.UI.Commercio where

import Data.Maybe
import Control.Arrow
import Control.Monad (join, liftM2)
import Lib.Units
import Lib.Passo
import Lib.Console
import Lib.NaturalLanguage
import Lib.Response
import Numeric

import Voci.Core
import Voci.Instances
import Voci.Language

------------------------------------------- interfaccia utente ------------------------------------

--------- disambigua ----------------
forma x = scelte x  $ ResponseOne "forma corretta"

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
	x <- scelte  [("peso",0),("volume",1),("unità",2)] $ ResponseOne  "descrizione del bene in"
	case x of 
		0 -> do 
			(l :: String) <- libero $ ResponseOne "nome comune del bene sfuso"
			l' <- disambiguaSP Determinativo l
			kp $ Pesato (PWord l')
		1 -> do
			(l :: String) <- libero $ ResponseOne "nome comune del bene sfuso"
			l' <- disambiguaSP Determinativo l
			kv $ Volumato (VWord l')
		2 -> do 
			(s :: String) <- libero $ ResponseOne "nome singolare del bene"
			p <- libero $ ResponseOne "nome plurale del bene"
			(s',p') <- disambigua Rosso (s,p)
			kc $ Contato (UWord (s',p'))


-- decide il contenitore per il bene sfuso
uiContenitore :: (Monad m ,Name (BBene c), Name c, Enum c, Bounded c)
	=> [Contenitore c]
	-> (BBene c -> Molteplicita Word)
	-> (Quantità c -> Contenitore c -> Confezione c)	
	-> (BScaffale c -> Costruzione m b (BScaffale c))
	-> (BScaffale c -> Costruzione m b a)
	-> BBene c
	-> Costruzione m b a
uiContenitore cs mo f ks kq  xp = do
	let b = render $ Determinativo &.& Sfuso &.& mo xp
	y <- scelte (("nessuno",Nothing):map (render . singolare &&& Just ) cs) $ ResponseOne  $ "contenitore per " ++ b
	case y of 
		Nothing -> kq (Scaffale Base xp)
		Just y' -> do 
			let 	b' = render $ b :+: " " :+: InDeterminativo &.& singolare2 y'
			z <- scelte (map (render . singolare &&& id) [minBound .. maxBound]) $ ResponseOne  $ 
				"unità di misura per " ++ b' 
			(q :: Float) <- libero . ResponseOne $ render $ "quantità di " :+: singolare xp :+: " " 
				:+: InDeterminativo &.& singolare2 y' 
				:+: " espressa in " :+: plurale z 
			ks  (Scaffale (f (toRational q :? z)  y') xp) >>= kq

uiPesato  = uiContenitore [Pacchetto, Sacco, Sacchetto, Cassetta] fromPesato Solido 
uiVolumato  = uiContenitore [Brick,Flacone,Damigiana] fromVolumato Liquido 

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
	x <- scelte (("nessuna",Nothing): map (render . singolare &&& Just) [minBound .. maxBound]) $ ResponseOne  $ 
		"eventuale confezione contenente " ++ render (plurale g)
	case x of 
		Nothing -> return $ Scaffale c b
		Just s -> do 
			n <- libero . ResponseOne $ render $ "numero di " :+: Contenuto &.& plurale2 (Scaffale c b) :+: " in "
				:+: Indeterminativo &.& singolare2 s
			uiScatola $ Scaffale (Inscatolato s n c) b

-- prezzatura alla confezione
uiAllaConfezione :: (Monad m, Name (BScaffale c), Show (BScaffale c), UnitClass c, Name (Prezzato (BWord c) c c)) 
	=> BScaffale c
	-> Costruzione m b Commercio
uiAllaConfezione z@(Scaffale c b) = do
	(y :: Float) <- libero . ResponseOne $ render $ "prezzo in euro di " :+: Indeterminativo &.& singolare2 (Scaffale c b)
	return $ Commercio $ AllaConfezione z (toRational y :? Euro) 

-- prezzatura in unità di misura
uiAllaMisura :: (Monad m, Name (BScaffale c),Show (BScaffale c), UnitClass c, 
	Name (BBene c), Name c, Enum c, Bounded c, Name (Prezzato (BWord c) c c)) 
	=> BScaffale c
	-> (BScaffale c -> Quantità (Denaro,c) -> Prezzato (BWord c) c c)
	-> (BBene c -> Molteplicita Word)
	-> Costruzione m b Commercio

uiAllaMisura z@(Scaffale c b) f mo = do 
	x <- scelte (map (render . singolare &&& id) [minBound .. maxBound]) $ ResponseOne  $ 
		"unità di misura relativa al prezzo" 
	(y :: Float) <- libero . ResponseOne $ render $ "prezzo in euro di " 
		:+: Indeterminativo &.& singolare2 x
		:+: " di " :+: mo b
	return $ Commercio $ f z (toRational y :? (Euro,x))

-- prezzatura pesati
uiPrezzaPesato :: Monad m
	=> BScaffale Pesi
	-> Costruzione m b Commercio
uiPrezzaPesato z@(Scaffale Base _) = uiAllaMisura z AlPeso (\(Pesato (PWord x)) -> x)
uiPrezzaPesato z@(Scaffale _ x) = join $ scelte [
	("di " ++ render (Indeterminativo &.& singolare2 z), uiAllaConfezione z),
	(render (DiDeterminativo &.& Sfuso &.& fromPesato x), uiAllaMisura z AlPeso fromPesato
	)] 
	 $ ResponseOne "prezzo"

-- prezzatura volumati
uiPrezzaVolumato :: Monad m
	=> BScaffale Volumi
	-> Costruzione m b Commercio
uiPrezzaVolumato z@(Scaffale Base _) = uiAllaMisura z AlVolume fromVolumato 
uiPrezzaVolumato z @(Scaffale _ x) = join $ scelte [
		(render $ (ADeterminativo &.& singolare2 z), uiAllaConfezione z),
		("al volume " ++ render (DiDeterminativo &.& Sfuso &.& fromVolumato x), 
		uiAllaMisura z AlVolume fromVolumato)] 
	 $ ResponseOne "prezzo del bene relativo" 

-- prezzatura contati
uiPrezzaContato :: Monad m
	=> BScaffale Unità
	-> Costruzione m b Commercio
uiPrezzaContato z =  join $ scelte [
		(render $ (ADeterminativo &.& singolare2 z), uiAllaConfezione z),
		("al peso stimato di " ++ render (Indeterminativo &.& singolare2 z), uiAlPesoStimato z)] 
	 $ ResponseOne "prezzo del bene relativo"


-- prezzatura al peso stimato di una confezione o dell'unità di bene

uiAlPesoStimato :: Monad m => BScaffale Unità -> Costruzione m b Commercio
uiAlPesoStimato z@(Scaffale c b)  = do
	x <- scelte (map (render . singolare &&& id) [minBound .. maxBound]) $ ResponseOne  $ 
		"unità di misura relativa al prezzo" 
	(y :: Float) <- libero . ResponseOne $ render $ "prezzo in euro di " 
		:+: Indeterminativo &.& singolare2 x
		:+: " di " :+: plurale b
	(pm :: Float) <- libero . ResponseOne $ render $ "peso minimo di " :+: Indeterminativo &.& singolare2 (Scaffale c b) :+:
		" in " :+: plurale x

	(pd :: Float) <- libero . ResponseOne $ render $ "peso massimo di " :+: Indeterminativo &.& singolare2 (Scaffale c b) :+:
		" in " :+: plurale x
	return $ Commercio $  AlPesoStimato z (toRational pm :? x, toRational pd :? x) 
		(toRational y :? (Euro,x))

-- creazione di una voce di bene
ui = uiBene 	(uiPesato uiScatola uiPrezzaPesato) 
		(uiVolumato uiScatola uiPrezzaVolumato) 
		(uiContato uiScatola uiPrezzaContato)


