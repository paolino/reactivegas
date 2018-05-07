{-# LANGUAGE GADTs, FlexibleContexts, ScopedTypeVariables, StandaloneDeriving, DeriveDataTypeable,
	 MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}

module Voci.Boxes where

import Control.Arrow
import Control.Monad (msum, mplus)
import Lib.Units
import Lib.NaturalLanguage
import Voci.Data
import Voci.Beni
import Voci.Dynamics
import Voci.Compare
import Voci.Serializzazione
import Voci.Names
import Data.Typeable
import Voci.Ordini
import Voci.Quantita


data BoxVoce where
	BoxVoce :: (
		Read (BVoce b c d),
		Show (BVoce b c d),
		Eq (BVoce b c d), 
		Name (BVoce b c d) , Typeable b, Typeable c, Typeable d,
		Typeable (BVoce b c d), 
		UnitClass b, 
		UnitClass c
		) => BVoce b c d -> BoxVoce

instance Show BoxVoce where
	show (BoxVoce x) = show x
instance Eq BoxVoce where
	BoxVoce x == BoxVoce y = maybe False (==y) $ cast x

instance Name BoxVoce where
	singolare (BoxVoce x) = singolare x
	plurale (BoxVoce x) = plurale x

instance Read BoxVoce where
	readsPrec _ x = case reads x of 
		[(z :: BVoce Pesi Pesi Sfuso ,r)] -> [(BoxVoce z,r)]
		_ -> case reads x of 
			[(z :: BVoce Pesi Pesi Confezionato,r)] -> [(BoxVoce z,r)]
			_ -> case reads x of 
				[(z :: BVoce Volumi Volumi Confezionato,r)] -> [(BoxVoce z,r)]
				_ -> case reads x of  
					[(z :: BVoce Unità Unità Sfuso,r)] -> [(BoxVoce z,r)]
					_ -> case reads x of  
						[(z :: BVoce Unità Unità Confezionato,r)] -> [(BoxVoce z,r)]
						_ -> case reads x of  
							[(z :: BVoce Unità Pesi Sfuso,r)] -> [(BoxVoce z,r)]
							_ -> case reads x of
								[(z :: BVoce Unità Pesi Confezionato,r)] -> [(BoxVoce z,r)]
								_ -> []
data BoxOrdine where
	BoxOrdine :: (
		Read (BOrdine b c d),
		Show (BOrdine b c d),
		Eq (BOrdine b c d), 
		Name (BOrdine b c d) , 
		Typeable (BOrdine b c d),
		Valuta b c d Pesi, 
		Valuta b c d Volumi, 
		Valuta b c d Unità, 
		Valuta b c d Denaro, 
		VoceOf (BWord b) b c d,
		UnitClass b, 
		UnitClass c) => BOrdine b c d -> BoxOrdine

deriving instance Typeable Ordine

instance Show BoxOrdine where
	show (BoxOrdine x) = show x

instance Read BoxOrdine where
	readsPrec _ x = case reads x of 
		[(z :: BOrdine Pesi Pesi Sfuso ,r)] -> [(BoxOrdine z,r)]
		_ -> case reads x of 
			[(z :: BOrdine Pesi Pesi Confezionato,r)] -> [(BoxOrdine z,r)]
			_ -> case reads x of 
				[(z :: BOrdine Volumi Volumi Confezionato,r)] -> [(BoxOrdine z,r)]
				_ -> case reads x of  
					[(z :: BOrdine Unità Unità Sfuso,r)] -> [(BoxOrdine z,r)]
					_ -> case reads x of  
						[(z :: BOrdine Unità Unità Confezionato,r)] -> [(BoxOrdine z,r)]
						_ -> case reads x of  
							[(z :: BOrdine Unità Pesi Sfuso,r)] -> [(BoxOrdine z,r)]
							_ -> case reads x of
								[(z :: BOrdine Unità Pesi Confezionato,r)] -> [(BoxOrdine z,r)]
								_ -> []

instance Eq BoxOrdine where
	BoxOrdine x == BoxOrdine y = maybe False (==y) $ cast x

instance Name BoxOrdine where
	singolare (BoxOrdine x) = singolare x
	plurale (BoxOrdine x) = plurale x

bicast :: forall a b c d. (Typeable a, Typeable b, Typeable d, Typeable c) =>  a -> b -> (c -> d -> Bool) -> Bool
bicast x y f = case cast y of
	Nothing -> False
	Just y' -> case cast x of 
		Just x' -> x' `f` y'
		Nothing -> False

match :: BoxOrdine -> BoxVoce -> Bool
(BoxOrdine o) `match` (BoxVoce c) = any id [
	bicast o c (matchOV :: BOrdine Pesi Pesi Sfuso -> BVoce Pesi Pesi Sfuso -> Bool),
	bicast o c (matchOV :: BOrdine Volumi Volumi Confezionato -> BVoce Volumi Volumi Confezionato -> Bool),
	bicast o c (matchOV :: BOrdine Unità Unità Sfuso -> BVoce Unità Unità Sfuso -> Bool),
	bicast o c (matchOV :: BOrdine Pesi Pesi Confezionato -> BVoce Pesi Pesi Confezionato -> Bool),
	bicast o c (matchOV :: BOrdine Unità Unità Confezionato -> BVoce Unità Unità Confezionato -> Bool),
	bicast o c (matchOV :: BOrdine Unità Pesi Sfuso -> BVoce Unità Pesi Sfuso -> Bool),
	bicast o c (matchOV :: BOrdine Unità Pesi Confezionato -> BVoce Unità Pesi Confezionato -> Bool)
	]

mkIn f v = flip f  `fmap` cast v 
mkInConfezioni (BoxVoce v) = 
	(fmap BoxOrdine `fmap` (mkIn InConfezioni v :: Maybe (Quantità Unità -> BOrdine Volumi Volumi Confezionato)))
	`mplus`
	(fmap BoxOrdine `fmap` (mkIn InConfezioni v :: Maybe (Quantità Unità -> BOrdine Pesi Pesi Confezionato)))
	`mplus`
	(fmap BoxOrdine `fmap` (mkIn InConfezioni v :: Maybe (Quantità Unità -> BOrdine Unità Unità Confezionato)))
	
mkInPezzi (BoxVoce v) = 
	(fmap BoxOrdine `fmap` (mkIn InPezzi v :: Maybe (Quantità Unità -> BOrdine Unità Unità Sfuso)))

mkInPeso (BoxVoce v) = 
	(fmap BoxOrdine `fmap` (mkIn InPeso v :: Maybe (Quantità Pesi -> BOrdine Pesi Pesi Confezionato)))
	`mplus`
	(fmap BoxOrdine `fmap` (mkIn InPeso v :: Maybe (Quantità Pesi -> BOrdine Pesi Pesi Sfuso)))
	`mplus`
	(fmap BoxOrdine `fmap` (mkIn InPeso v :: Maybe (Quantità Pesi -> BOrdine Unità Pesi Sfuso)))
	`mplus`
	(fmap BoxOrdine `fmap` (mkIn InPeso v :: Maybe (Quantità Pesi -> BOrdine Unità Pesi Confezionato)))

mkInVolume (BoxVoce v) = 
	(fmap BoxOrdine `fmap` (mkIn InVolume v :: Maybe (Quantità Volumi -> BOrdine Volumi Volumi Confezionato)))
	`mplus`
	(fmap BoxOrdine `fmap` (mkIn InVolume v :: Maybe (Quantità Volumi -> BOrdine Volumi Volumi Sfuso)))

mkInDenaro (BoxVoce v) = 
	(fmap BoxOrdine `fmap` (mkIn InDenaro v :: Maybe (Quantità Denaro -> BOrdine Volumi Volumi Confezionato)))
	`mplus`
	(fmap BoxOrdine `fmap` (mkIn InDenaro v :: Maybe (Quantità Denaro -> BOrdine Pesi Pesi Confezionato)))
	`mplus`
	(fmap BoxOrdine `fmap` (mkIn InDenaro v :: Maybe (Quantità Denaro -> BOrdine Unità Unità Confezionato)))
	`mplus`
	(fmap BoxOrdine `fmap` (mkIn InDenaro v :: Maybe (Quantità Denaro -> BOrdine Volumi Volumi Sfuso)))
	`mplus`
	(fmap BoxOrdine `fmap` (mkIn InDenaro v :: Maybe (Quantità Denaro -> BOrdine Pesi Pesi Sfuso)))
	`mplus`
	(fmap BoxOrdine `fmap` (mkIn InDenaro v :: Maybe (Quantità Denaro -> BOrdine Unità Unità Sfuso)))
	`mplus`
	(fmap BoxOrdine `fmap` (mkIn InDenaro v :: Maybe (Quantità Denaro -> BOrdine Unità Pesi Confezionato)))
	`mplus`
	(fmap BoxOrdine `fmap` (mkIn InDenaro v :: Maybe (Quantità Denaro -> BOrdine Unità Pesi Sfuso)))
	


