{-# LANGUAGE GADTs, FlexibleContexts, ScopedTypeVariables #-}

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


data Commercio where
	Commercio :: (
		Read (BVoce b c d),
		Show (BVoce b c d),
		Eq (BVoce b c d), 
		Name (BVoce b c d) , 
		Typeable (BVoce b c d), 
		UnitClass b, 
		UnitClass c) => BVoce b c d -> Commercio

instance Show Commercio where
	show (Commercio x) = show x

instance Read Commercio where
	readsPrec _ x = case reads x of 
		[(z :: BVoce Pesi Pesi Sfuso ,r)] -> [(Commercio z,r)]
		_ -> case reads x of 
			[(z :: BVoce Pesi Pesi Confezionato,r)] -> [(Commercio z,r)]
			_ -> case reads x of 
				[(z :: BVoce Volumi Volumi Confezionato,r)] -> [(Commercio z,r)]
				_ -> case reads x of  
					[(z :: BVoce Unità Unità Sfuso,r)] -> [(Commercio z,r)]
					_ -> case reads x of  
						[(z :: BVoce Unità Unità Confezionato,r)] -> [(Commercio z,r)]
						_ -> case reads x of  
							[(z :: BVoce Unità Pesi Sfuso,r)] -> [(Commercio z,r)]
							_ -> case reads x of
								[(z :: BVoce Unità Pesi Confezionato,r)] -> [(Commercio z,r)]
								_ -> []

instance Eq Commercio where
	Commercio x == Commercio y = maybe False (==y) $ cast x

instance Name Commercio where
	singolare (Commercio x) = singolare x
	plurale (Commercio x) = plurale x
