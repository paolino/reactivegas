{-# LANGUAGE GADTs, FlexibleContexts, ScopedTypeVariables, StandaloneDeriving, DeriveDataTypeable #-}

module Voci.Boxes where

import Control.Arrow
import Lib.Units
import Lib.NaturalLanguage
import Voci.Data
import Voci.Beni
import Voci.Dynamics
import Voci.Compare
import Voci.Serializzazione
import Voci.Names
import Data.Typeable


data BoxVoce where
	BoxVoce :: (
		Read (BVoce b c d),
		Show (BVoce b c d),
		Eq (BVoce b c d), 
		Name (BVoce b c d) , 
		Typeable (BVoce b c d), 
		UnitClass b, 
		UnitClass c) => BVoce b c d -> BoxVoce

instance Show BoxVoce where
	show (BoxVoce x) = show x

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
		UnitClass b, 
		UnitClass c) => BOrdine b c d -> BoxOrdine

deriving instance Typeable4 Ordine

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
