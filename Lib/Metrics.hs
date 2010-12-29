{-# LANGUAGE ExistentialQuantification, NoMonomorphismRestriction, DeriveDataTypeable, TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies, DeriveFunctor #-}
module Lib.Dimensions where

import Data.Ratio
import Data.List
import Data.Ord
import Control.Arrow
import Data.Typeable
import Data.Maybe
import Control.Monad
import Text.Read
import Text.ParserCombinators.ReadPrec
import Text.ParserCombinators.ReadP

import Lib.NaturalLanguage



type Base a = (Rational, a)

class UnitClass a where
	base :: a -> Base a

------------------------------------------------------------------
data Pesi = Grammo | Chilogrammo   deriving (Read,Show,Typeable,Enum)

instance UnitClass Pesi where
	base Grammo = (1, Grammo)
	base Chilogrammo = (1000, Grammo)

instance Nome Pesi where
	singolare Grammo = Maschile "grammo"
	singolare Chilogrammo = Maschile "chilogrammo"
	plurale Grammo = Maschile "grammi"
	plurale Chilogrammo = Maschile "chilogrammi"

data Volumi = Litro |  Millilitro deriving (Read,Show,Typeable,Enum)


data Denaro = Euro | Centesimo  deriving (Read,Show,Typeable,Enum)

instance UnitClass Denaro where
	base Euro = (1,Euro)
	base Centesimo = (1%100,Euro)

instance Nome Denaro where
	singolare Euro = Maschile "euro"
	singolare Centesimo = Maschile "centesimo"
	plurale Euro = Maschile "euro"
	plurale Centesimo = Maschile "centesimi"




data Unita = Unita  deriving (Read,Show,Typeable)
instance Nome Unita where
	singolare Unita = Femminile "unità"
	plurale Unita = Femminile "unità"

instance UnitClass Unita where
	base Unita = (1,Unita)

--------------------------------------------------------------------------

data Dimension = forall a . (Nome a, Read a, Show a, Typeable a, UnitClass a) => Dimension (Int,a)
instance Show Dimension where
	show (Dimension x) = show x

instance Nome Dimension where
	
	singolare (Dimension (d,x)) = prettyDimension d `fmap` singolare x
	plurale (Dimension (d,x)) = prettyDimension d `fmap` plurale x

prettyDimension i 	
		| i == 0 =  const  ""
		| abs i == 1 = id 
		| abs i == 2 = (++ "quadrato")
		| abs i == 3 = (++ "cubo") 
		| abs i == 4 = (++ "alla quarta")
		| otherwise = (\x -> x ++ "alla " ++ show i ++ "ma potenza")

esp (Dimension (i,_)) = i
mkDimension i x = Dimension (i,x)

dbase (Dimension (i,x)) = (^^i) *** (mkDimension i) $ base x

chdim f (Dimension (i,x)) = Dimension (f i,x)
trydim (Dimension (_,x)) (Dimension (i,y)) = (mkDimension i . (`asTypeOf` x)) `fmap` cast y
operate f (Dimension (i,x)) (Dimension (j,y)) = do 
	t <- (`asTypeOf` x) `fmap` cast y
	let 	(n,_) = base x
		(m,_) = base y
	l <- f j i 
	return $ (m / n, Dimension (l,x))

convert = operate $ \j i -> if i == j then Just i else Nothing

readDimension :: [Unit]  -> ReadS Dimension
readDimension [] _  = []
readDimension (Unit x:xs) s = case reads s of
	[] -> readDimension xs s
	ys -> map (first $ Dimension . second (`asTypeOf` x)) $ ys


data Unit = forall a.  (Nome a, Read a, Show a, Typeable a, UnitClass a) => Unit a

instance Read Dimension where
	readsPrec _ =  readDimension $ [Unit Euro, Unit Grammo, Unit Unita, Unit Scatola,Unit Pacchetto]


--------------------------------------------------------------------------------------
type Dimensions = [Dimension] 

instance UnitClass Dimensions where
	base ds = second (filter ((/=0) . esp)) $ collapse ds
		where
		collapse :: [Dimension] -> (Rational,[Dimension])
		collapse [] = (1,[])
		collapse (x:xs) = let
			
			w (q,x) y = ((*q) *** id) `fmap` operate (\i j -> return (i + j)) x y
			k y (x,zs) = case w x y of 
				Nothing -> (x,y:zs)
				Just x' -> (x',zs)
			((z',x'),xs'') = foldr k ((1,x),[]) xs
			in ((*z') *** (x':)) $ collapse xs''

instance Nome Dimensions where
	singolare [] = Maschile ""
	singolare xs = let 
		(rs,qs) = partition ((>= 0) . esp) $ xs 
		q = concatMap (unsex . prefix (" al "," alla ") . singolare)
		in Maschile $ 
			(intercalate " per " . map (unsex . singolare) $ rs) ++ (q qs)
	plurale [] = Maschile ""
	plurale xs =  let 
		(rs,qs) = partition ((>= 0) . esp) $ xs 
		q = concatMap (unsex . prefix (" al "," alla ") . singolare)
		in Maschile $ 
			(intercalate " per " . zipWith  (\f -> unsex . f) (plurale : repeat singolare) $ rs) 
			++ q qs

denominatore = map (chdim negate) .  filter ((==) (-1) . esp) . dimensions
numeratore = filter ((==) 1 . esp) . dimensions
 
infixl 4 ===

(===) :: Dimensions -> Dimensions -> Bool
ds === ds' = let
	(_,dz) = base ds
	(_,dz') = base ds'
	in length dz == length dz' && isJust (msum $ map (\z -> foldr mplus Nothing $ 
		map (\z' ->  trydim z z' >> if esp z /= esp z' then Nothing else Just undefined ) dz) dz')

---------------------------------------------------------------------------------------------------
class Dimensioned a where
	dimensions :: a -> Dimensions

infixl 3 =~=

(=~=) :: (Dimensioned a , Dimensioned b) => a -> b -> Bool
x =~= y = dimensions x === dimensions y


  	-- Defined in GHC.Real

		
