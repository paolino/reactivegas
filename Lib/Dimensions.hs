{-# LANGUAGE ExistentialQuantification, NoMonomorphismRestriction, DeriveDataTypeable, TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies, DeriveFunctor #-}

data Dimension = forall a . (Name a, Read a, Show a, Typeable a, UnitClass a) => Dimension (Int,a)
instance Show Dimension where
	show (Dimension x) = show x

hack (Dimension (i,x)) = show (typeOf x)
instance Name Dimension where
	
	singolare (Dimension (d,x)) = prettyDimension d `fmap` singolare x
	plurale (Dimension (d,x)) = prettyDimension d `fmap` plurale x

instance Ord Dimension where
	compare = compare `on` hack

instance Eq Dimension where
	(==) = (==) `on` hack

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
operate f g dx@(Dimension (i,x)) dy@(Dimension (j,y)) = do 
	t <- (`asTypeOf` x) `fmap` cast y
	let 	(n,_) = dbase dx
		(m,_) = dbase dy
	l <- j `f` i 
	return $ (m `g` n, Dimension (l,x))

convert = operate (\j i -> if i == j then Just i else Nothing) (/)

x ^. n = Dimension (n,x)

readDimension :: [Unit]  -> ReadS Dimension
readDimension [] _  = []
readDimension (Unit x:xs) s = case reads s of
	[] -> readDimension xs s
	ys -> map (first $ Dimension . second (`asTypeOf` x)) $ ys


data Unit = forall a.  (Name a, Read a, Show a, Typeable a, UnitClass a) => Unit a

instance Read Dimension where
	readsPrec _ =  readDimension $ [Unit Euro, Unit Grammo, Unit Unita]


--------------------------------------------------------------------------------------
type Dimensions = [Dimension] 

catchNothing s Nothing = error s
catchNothing _ (Just x) = x

collapse (x:xs) = foldl (\(l,x) y -> catchNothing "collapse failed" $ do 
	(l',x') <- operate (\i j -> return (i + j)) (*) x y
	return (l' * l,x')
	) (1,x) xs
instance UnitClass Dimensions where
	base ds = (product *** filter ((/=0) . esp)) $ unzip . map collapse . group . sort $ ds

instance Name Dimensions where
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
	dz = map (snd . dbase) $ snd $ base ds
	dz'= map (snd . dbase) $ snd $ base ds'
	in sort dz == sort dz'

align xs ys = let 
		(bx,xs') = base xs
		(by,ys') = base ys
		in (first (foldr (*) (bx/by)) . unzip) `fmap` sequence (zipWith convert xs' ys')
	
---------------------------------------------------------------------------------------------------
class Dimensioned a where
	dimensions :: a -> Dimensions

infixl 3 =~=

(=~=) :: (Dimensioned a , Dimensioned b) => a -> b -> Bool
x =~= y = dimensions x === dimensions y


  	-- Defined in GHC.Real

		
