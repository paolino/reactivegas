
{-# LANGUAGE TypeSynonymInstances, Rank2Types, ScopedTypeVariables, UndecidableInstances, StandaloneDeriving, 
	FlexibleInstances, FlexibleContexts, GADTs, OverlappingInstances #-}
module Voci.Names where

import Voci.Beni
import Voci.Data
import Lib.Units
import Lib.NaturalLanguage
import Voci.Language
import Voci.Quantita
import Numeric (showFFloat)

instance Name b => Name (Quantità (b,Unità)) where
	singolare (x :? (y,_)) = fmap ((showFFloat (Just 2) (fromRational x) "" ++ " ") ++) $ 
		if x == 1 then singolare y else plurale y 
	plurale z =  singolare z

instance Name (Quantità Unità) where
	singolare (x :? y) = Maschile ((showFFloat (Just 0) (fromRational x) "" ++ " ")) 
	plurale z =  singolare z

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

instance Name (Contenitore Pesi) where
	singolare (Pacchetto x) = Maschile $ "pacchetto da " ++ (render . singolare) x
	singolare (Sacco x) = Maschile $ "sacco da " ++ (render . singolare) x
	singolare (Sacchetto x) = Maschile $ "sacchetto da " ++ (render . singolare) x
	singolare (Cassetta x) = Femminile $ "cassetta da " ++ (render . singolare) x
	plurale (Pacchetto x) = Maschile $ "pacchetti da " ++ (render . singolare) x
	plurale (Sacco x) = Maschile $ "sacchi da " ++ (render . singolare) x
	plurale (Sacchetto x) = Maschile $ "sacchetti da " ++ (render . singolare) x
	plurale (Cassetta x) = Femminile $ "cassette da " ++ (render . singolare) x

instance Name (Contenitore Volumi) where
	singolare (Brick x) = Maschile $ "brick da " ++ (render . singolare) x
	singolare (Flacone x) = Maschile $ "flacone da " ++ (render . singolare) x
	singolare (Damigiana x) = Femminile $ "damigiana da " ++ (render . singolare) x
	singolare (Bottiglia x) = Femminile $ "bottiglia da " ++ (render . singolare) x
	plurale (Brick x) = Maschile $ "brick da " ++ (render . singolare) x
	plurale (Flacone x) = Maschile $ "flaconi da " ++ (render . singolare) x
	plurale (Damigiana x) = Femminile $ "damigiane da " ++ (render . singolare) x
	plurale (Bottiglia x) = Femminile $ "bottiglie da " ++ (render . singolare) x

instance Name (Contenitore Unità) where
	singolare (Scatola x) = Femminile $ "scatola da " ++ (render . singolare) x
	singolare (Scatolone x) = Maschile $ "scatolone da " ++ (render . singolare) x
	singolare (Plateau x) = Maschile $ "plateau da " ++ (render . singolare) x
	plurale (Scatola x) = Femminile $ "scatole da " ++ (render . singolare) x
	plurale (Scatolone x) = Maschile $ "scatoloni da " ++ (render . singolare) x
	plurale (Plateau x) = Maschile $ "plateau da " ++ (render . singolare) x

instance Name (Contenitore b) => Name (Confezionamento b) where
	singolare (Primo c) = singolare c
	singolare (Inscatolato c n) =  (++ render (checkUnità (count c) singolare plurale $ n)) `fmap` singolare c
	plurale (Primo c) = plurale c
	plurale (Inscatolato c n) =  (++ render (checkUnità (count c) singolare plurale $ n)) `fmap` plurale c

checkUnità (1 :? Unità) f _ = f 
checkUnità _ _ g = g

instance Name (BVoce Pesi Pesi Confezionato) where
	singolare (AllaConfezione c b q) = Maschile . render $ singolare c :+: " di " :+: singolare b
 		:+: " a " :+: singolare q :+: " alla confezione"
	singolare (AlPesoConfezionato c b q) = Maschile . render $ singolare c :+: " di " :+: singolare b 
		:+: " a " :+:  singolare q 
	plurale (AllaConfezione c b q) = Maschile . render $ plurale c :+: " di " :+: singolare b
 		:+: " a " :+: singolare q :+: " alla confezione"
	plurale (AlPesoConfezionato c b q) = Maschile . render $ plurale c :+: " di " :+: singolare b 
		:+: " a " :+:  singolare q

instance Name (BVoce Volumi Volumi Confezionato) where	
	singolare (AllaConfezione c b q) = Maschile . render $ singolare c :+: " di " :+: singolare b
 		:+: " a " :+: singolare q :+: " alla confezione"
	singolare (AlVolumeConfezionato c b q) = Maschile . render $ singolare c :+: " di " :+: singolare b 
		:+: " a " :+:  singolare q
	plurale (AllaConfezione c b q) = Maschile . render $ plurale c :+: " di " :+: singolare b
 		:+: " a " :+: singolare q :+: " alla confezione"
	plurale (AlVolumeConfezionato c b q) = Maschile . render $ plurale c :+: " di " :+: singolare b 
		:+: " a " :+:  singolare q

instance Name (BVoce Unità Unità Confezionato) where	
	singolare (AllaConfezione c b q) = Maschile . render $ singolare c :+: plurale b
 		:+: " a " :+: singolare q :+: " alla confezione"
	plurale (AllaConfezione c b q) = Maschile . render $ plurale c :+: plurale b
 		:+: " a " :+: singolare q 

instance Name (BVoce Unità Unità Sfuso) where
	singolare (AlPezzo b q) = Maschile . render $ singolare b :+: " a " :+:  singolare q 
		:+: " ogni " :+: singolare b
	plurale (AlPezzo b q) = Maschile . render $ plurale b :+: " a " :+:  singolare q 
		:+: " ogni " :+: singolare b

instance Name (BVoce Pesi Pesi Sfuso) where
	singolare (AlPeso b q) = Maschile . render $ WSfuso &.& fromPesato b :+: " a " :+:  singolare q 
	plurale = singolare

instance Name (BVoce Volumi Volumi Sfuso) where
	singolare (AlVolume b q) = Maschile . render $ WSfuso &.& fromVolumato b :+: " a " :+:  singolare q 
	plurale = singolare

instance Name (BVoce Unità Pesi Sfuso) where
	singolare (AlPezzoStimato b (q0,q1) q) = Maschile . render $ singolare b :+: 
		", con peso unitario variabile tra " 
		:+: singolare q0 :+: " e " :+: singolare q1 :+: ", a " :+: singolare q
	plurale (AlPezzoStimato b (q0,q1) q) = Maschile . render $ plurale b :+: 
		", con peso unitario variabile tra " 
		:+: singolare q0 :+: " e " :+: singolare q1 :+: ", a " :+: singolare q

instance Name (BVoce Unità Pesi Confezionato) where
	singolare (AllaConfezioneStimata c b (q0,q1) q) = Maschile . render $ singolare c :+: plurale b 
		:+: ", con peso della confezione variabile tra " 
		:+: singolare q0 :+: " e " :+: singolare q1 :+: ", a " :+: singolare q
	plurale (AllaConfezioneStimata c b (q0,q1) q) = Maschile . render $ plurale c :+: plurale b 
		:+: ", con peso della confezione variabile tra " 
		:+: singolare q0 :+: " e " :+: singolare q1 :+: ", a " :+: singolare q

	
instance  Name (BOrdine Volumi Volumi Confezionato) where
	singolare (InDenaro q v) = Maschile . render $ (singolare q :+: " di " :+: plurale v)
	singolare (InConfezioni q v) = Maschile . render $ (singolare q :+: checkUnità q singolare plurale v)
	singolare (InVolume q v) = Maschile . render $ (singolare q :+: " di " :+: plurale v)
	plurale = singolare
instance  Name (BOrdine Pesi Pesi Confezionato) where
	singolare (InDenaro q v) = Maschile . render $ (singolare q :+: " di " :+: plurale v)
	singolare (InConfezioni q v) = Maschile . render $ (singolare q :+: checkUnità q singolare plurale v)
	singolare (InPeso q v) = Maschile . render $ (singolare q :+: " di " :+: plurale v)
	plurale = singolare

instance  Name (BOrdine Unità Unità Confezionato) where
	singolare (InDenaro q v) = Maschile . render $ (singolare q :+: " di " :+: plurale v)
	singolare (InConfezioni q v) = Maschile . render $ (singolare q :+: checkUnità q singolare plurale v)
	plurale = singolare

instance  Name (BOrdine Unità Unità Sfuso) where
	singolare (InDenaro q v) = Maschile . render $ (singolare q :+: " di " :+: plurale v)
	singolare (InPezzi q v) = Maschile . render $ (singolare q :+: checkUnità q singolare plurale v)
	plurale = singolare

instance  Name (BOrdine Pesi Pesi Sfuso) where
	singolare (InDenaro q v) = Maschile . render $ (singolare q :+: " di " :+: plurale v)
	singolare (InPeso q v) = Maschile . render $ (singolare q :+: " di " :+: plurale v)
	plurale = singolare
instance  Name (BOrdine Volumi Volumi Sfuso) where
	singolare (InDenaro q v) = Maschile . render $ (singolare q :+: " di " :+: plurale v)
	singolare (InVolume q v) = Maschile . render $ (singolare q :+: " di " :+: plurale v)
	plurale = singolare

instance  Name (BOrdine Unità Pesi Confezionato) where
	singolare (InDenaro q v) = Maschile . render $ (singolare q :+: " di " :+: plurale v)
	singolare (InConfezioni q v) = Maschile . render $ (singolare q :+: checkUnità q singolare plurale v)
	singolare (InPeso q v) = Maschile . render $ (singolare q :+: " di " :+: plurale v)
	plurale = singolare

instance  Name (BOrdine Unità Pesi Sfuso) where
	singolare (InDenaro q v) = Maschile . render $ (singolare q :+: " di " :+: plurale v)
	singolare (InPeso q v) = Maschile . render $ (singolare q :+: " di " :+: plurale v)
	singolare (InPezzi q v) = Maschile . render $ (singolare q :+: checkUnità q singolare plurale v)
	plurale = singolare


