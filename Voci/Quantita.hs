module Voci.Quantita where

import Lib.Units  (UnitClass (..))

data Quantità b = Rational :? b deriving (Show,Read)

instance (UnitClass a, UnitClass b) => UnitClass (a,b) where
	base (x,y) = let 
		(kx,x') = base x
		(ky,y') = base y
		in (kx/ky,(x',y'))
instance UnitClass b => Eq (Quantità b) where
	(x :? y) == (x1 :? y1) =  (x * fst (base y)) == (x1 * fst (base y1))

instance UnitClass b => Ord (Quantità b) where
	(x :? y) `compare` (x1 :? y1) =  (x * fst (base y)) `compare` (x1 * fst (base y1))

