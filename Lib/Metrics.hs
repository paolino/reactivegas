{-# LANGUAGE ExistentialQuantification, NoMonomorphismRestriction, DeriveDataTypeable, TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies, DeriveFunctor #-}
module Lib.Metrics where

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





type Base a = (Rational, a)

class UnitClass a where
	base :: a -> Base a

data Sex  a = Femminile a | Maschile a deriving Functor
instance Show a => Show (Sex a) where
	show (Maschile x) = show x
	show (Femminile x) = show x
bisex (Maschile x) = x
bisex (Femminile x) = x

respect (m,f) (Maschile x) = Maschile (m x)
respect (m,f) (Femminile x) = Femminile (f x)

class Pretty a where
	singolare :: a -> Sex String
	plurale :: a -> Sex String
	

------------------------------------------------------------------
data Pesi = Grammo | Chilogrammo   deriving (Read,Show,Typeable,Enum)

instance UnitClass Pesi where
	base Grammo = (1, Grammo)
	base Chilogrammo = (1000, Grammo)

instance Pretty Pesi where
	singolare Grammo = Maschile "grammo"
	singolare Chilogrammo = Maschile "chilogrammo"
	plurale Grammo = Maschile "grammi"
	plurale Chilogrammo = Maschile "chilogrammi"

data Denaro = Euro | Centesimo  deriving (Read,Show,Typeable,Enum)

instance UnitClass Denaro where
	base Euro = (1,Euro)
	base Centesimo = (1%100,Euro)

instance Pretty Denaro where
	singolare Euro = Maschile "euro"
	singolare Centesimo = Maschile "centesimo"
	plurale Euro = Maschile "euro"
	plurale Centesimo = Maschile "centesimi"

data Pezzi a = Singolo a | Dozzina a  deriving (Read,Show,Typeable)

instance Pretty a => Pretty (Pezzi a) where
	singolare (Singolo x) = singolare x
	singolare (Dozzina x) = ("dozzina di " ++) `fmap` plurale x
	plurale (Singolo x) = plurale x
	plurale (Dozzina x) = ("dozzine di " ++) `fmap`  plurale x 
	
instance UnitClass (Pezzi a) where
	base (Singolo x) = (1,Singolo x)
	base (Dozzina x) = (12,Singolo x)

data Pezzo = Pezzo deriving (Read,Show,Typeable)
instance Pretty Pezzo where
	singolare Pezzo = Maschile "pezzo"
	plurale Pezzo = Maschile "pezzi"
 
data Scatola = Scatola deriving (Read,Show,Typeable)
instance Pretty Scatola where
	singolare Scatola = Femminile "scatola"
	plurale Scatola = Femminile "scatole"

data Pacchetto = Pacchetto deriving (Read,Show,Typeable)
instance Pretty Pacchetto where
	singolare Pacchetto = Maschile "pacchetto"
	plurale Pacchetto = Maschile "pacchetti"

unitPezzi = [Unit (Singolo Pezzo), Unit (Singolo Scatola), Unit (Singolo Pacchetto)]
--------------------------------------------------------------------------

data Dimension = forall a . (Pretty a, Read a, Show a, Typeable a, UnitClass a) => Dimension (Int,a)
instance Show Dimension where
	show (Dimension x) = show x

instance Pretty Dimension where
	
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


data Unit = forall a.  (Pretty a, Read a, Show a, Typeable a, UnitClass a) => Unit a

instance Read Dimension where
	readsPrec _ =  readDimension $ [Unit Euro, Unit Grammo] ++ unitPezzi


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

instance Pretty Dimensions where
	singolare [] = Maschile ""
	singolare xs = let 
		(rs,qs) = partition ((>= 0) . esp) $ xs 
		q (Maschile x) = " al " ++ x
		q (Femminile x) = " alla " ++ x
		in Maschile $ (intercalate " per " . map (bisex . singolare) $ rs) ++ (concatMap (q . singolare) $ qs)
	plurale [] = Maschile ""
	plurale xs =  let 
		(rs,qs) = partition ((>= 0) . esp) $ xs 
		in Maschile $ (intercalate " per " . zipWith  (\f -> bisex . f) (plurale : repeat singolare) $ rs) 
			++ (concatMap (bisex . respect ((" al " ++),(" alla " ++)) . singolare) $ qs)

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

data Dimensioned = Dimensioned {
	value :: Rational,
	dimensions :: Dimensions 
	} deriving (Read,Show)

prettyDimensioned (Dimensioned v ds) = kr v ++ " " ++  bisex (if v == 1 then singolare ds else plurale ds)
		where	kr v 	| denominator v == 1 = show $ numerator v 
				| otherwise = show $ (fromRational v :: Float) 

infixl 3 =~=

(=~=) :: Dimensioned -> Dimensioned -> Bool
x =~= y = dimensions x === dimensions y

instance Eq Dimensioned where
	(Dimensioned x dx) == (Dimensioned y dy) = 
		let 	(x',dx') = base dx
			(y',dy') = base dy
		in dx === dy && x' * x == y' * y

fmapDim f (Dimensioned x ds) = Dimensioned (f x) ds

instance Num Dimensioned  where
	(Dimensioned x ds) * (Dimensioned y ds') = let 
		(ex,nds) = base $ (ds ++ ds')
		in Dimensioned (x * y * ex) nds
	(Dimensioned x ds) + (Dimensioned y ds') 
		| not (ds === ds') = error "summing apples and pears"
		| otherwise = let
			(n1,d1) = base ds
			(n2,_) = base ds'
			in Dimensioned (x * n1  + y * n2) d1
	negate = fmapDim negate
	abs = fmapDim abs
	signum = fmapDim signum
	fromInteger x = Dimensioned (fromInteger x) []

instance Fractional Dimensioned where
	(Dimensioned x ds) / (Dimensioned y ds') = let 
		(ex,nds) = base $ (ds ++ map (\(Dimension (i,x)) -> Dimension ((-i),x)) ds')
		in Dimensioned (x * ex / y) nds
	fromRational x = Dimensioned (fromRational x) []
instance Real Dimensioned where
	toRational = value
instance Ord Dimensioned where
	(Dimensioned x _) `compare` (Dimensioned y _) = x `compare` y

instance RealFrac Dimensioned where
  properFraction (Dimensioned x ds) = let 
	(y,x') = properFraction x in (y,Dimensioned x' ds)
  truncate = truncate . value
  round = round . value
  ceiling = ceiling . value
  floor = floor . value
  	-- Defined in GHC.Real

		
