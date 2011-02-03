{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, OverlappingInstances #-}
module Voci.Quantita where

import Lib.Units  -- (UnitClass (..))

data Quantità b = Rational :? b deriving (Show,Read)

instance UnitClass () where
	base () = (1,())
instance (UnitClass a, UnitClass b) => UnitClass (a,b) where
	base (x,y) = let 
		(kx,x') = base x
		(ky,y') = base y
		in (kx/ky,(x',y'))
instance UnitClass b => Eq (Quantità b) where
	(x :? y) == (x1 :? y1) =  (x * fst (base y)) == (x1 * fst (base y1))

instance UnitClass b => Ord (Quantità b) where
	(x :? y) `compare` (x1 :? y1) =  (x * fst (base y)) `compare` (x1 * fst (base y1))

class Qon a b c where
	qon :: Quantità a -> Quantità b -> Quantità c

floorQ (x :? y) = ((toRational . floor $ x) :? y)
instance UnitClass a => Qon a a Unità where
	qon (x:?dx) (y:?dy) =
		let 	(bx,_) = base dx
			(by,_) = base dy
		in  (bx/by*x/y):? Unità

instance UnitClass a => Qon a (b,a) b where
	qon (x:?dx) (y:?(dyn,dyd)) =
		let 	(bx,_) = base dx
			(by,_) = base dyd
		in  (x*y*bx/by):?dyn

instance UnitClass a => Qon (a,c) (b,a) (b,c) where
	qon (x:?(dxn,dxd)) (y:?(dyn,dyd)) =
		let 	(bx,_) = base dxn
			(by,_) = base dyd
		in  (x*y*bx/by):?(dyn,dxd)

instance UnitClass a => Qon a (a,b) b where
	qon (x:?dx) (y:?(dyn,dyd)) =
		let 	(bx,_) = base dx
			(by,_) = base dyn
		in  (x/y*bx/by):?dyd

instance UnitClass a => Qon Unità a a where
	qon (x:?dx) (y:?dy) =
		let 	(bx,_) = base dx
		in  (x*y*bx):?dy

instance Qon Unità Unità Unità where
	qon (x:?dx) (y:?dy) =
		let 	(bx,_) = base dx
		in  (x*y*bx):?dy
