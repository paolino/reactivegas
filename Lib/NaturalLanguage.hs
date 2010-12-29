{-# LANGUAGE DeriveFunctor, FlexibleInstances, TypeSynonymInstances, NoMonomorphismRestriction#-}
module Lib.NaturalLanguage where

import Data.Typeable
import Data.Monoid


data Sexed  a = Femminile {unsex :: a} | Maschile {unsex :: a} deriving (Read,Show,Functor)


prettySexed (Maschile x) = maybe (show x) id $ cast x 
prettySexed (Femminile x) = maybe (show x) id $ cast x 

respect (f,_) (Maschile x) = Maschile (f x)
respect (_,f) (Femminile x) = Femminile (f x)

prefix (x,y) = respect ((x ++),(y ++))
vocale = (`elem` "aeiou")

type Word = Sexed String

class Name a where
	singolare,plurale :: a -> Word 

instance Name (Word,Word) where
	singolare = fst
	plurale = snd	


class Polimorfo a where
	singolareA, pluraleA :: a -> Word -> Word

data Morfato a b = a :++ b 
infixr 7 :++

instance (Polimorfo a, Name b) => Name (Morfato a b) where
	singolare (x :++ y) = singolareA x $ singolare y
	plurale (x :++ y) = pluraleA x $ plurale y

x &.& (Singolare y) = Singolare $ singolareA x y
x &.& (Plurale y) = Plurale $ pluraleA x y
infixr 5 &.&

maschile x = (Maschile x, Maschile x)
femminile x = (Femminile x, Femminile x)

data Molteplicita a = Singolare {unmulti :: a} | Plurale {unmulti ::  a} deriving (Show,Read)

plurale2 = Plurale . plurale
singolare2 = Singolare . singolare


stz (x:y:xs) = x == 's' && not (vocale y)  || x == 'z'
onstz x y z = if stz x then (y ++ x) else (z ++ x)
data Indeterminativo = Indeterminativo deriving (Show,Read)
instance Polimorfo Indeterminativo where
	singolareA Indeterminativo (Maschile x) = Maschile ("un " ++ x)
	singolareA Indeterminativo (Femminile x) = Femminile $ "un" ++ (if vocale (head x) then "'" else "a ") ++ x
	pluraleA Indeterminativo (Maschile x) = Maschile ("alcuni " ++ x)
	pluraleA Indeterminativo (Femminile x) = Femminile ("alcune " ++ x)

data Determinativo = Determinativo deriving (Show,Read)
instance Polimorfo Determinativo where
	singolareA Determinativo (Maschile x) = Maschile (onstz x "lo " "il ")
	singolareA Determinativo (Femminile x) = Femminile $ "l" ++ (if vocale (head x) then "'" else "a ") ++ x
	pluraleA Determinativo (Maschile x) = Maschile (onstz x "gli " "i " )
	pluraleA Determinativo (Femminile x) = Femminile ("le " ++ x)

data InDeterminativo = InDeterminativo deriving (Show,Read)
instance Polimorfo InDeterminativo where
	singolareA InDeterminativo (Maschile x) = Maschile (onstz x "nello " "nel ")
	singolareA InDeterminativo (Femminile x) = Femminile $ "nell" ++ (if vocale (head x) then "'" else "a ") ++ x
	pluraleA InDeterminativo (Maschile x) = Maschile (onstz x "negli " "nei ")
	pluraleA InDeterminativo (Femminile x) = Femminile ("nelle " ++ x)

data Costante = Costante String deriving (Show,Read)
instance Polimorfo Costante where
	singolareA (Costante s) = respect ((++ s),(++ s))
	pluraleA (Costante s) = respect ((++ s),(++ s))

data ADeterminativo = ADeterminativo deriving (Show,Read)
instance Polimorfo ADeterminativo where
	singolareA ADeterminativo (Maschile x) = Maschile (onstz x "allo " "al ")
	singolareA ADeterminativo (Femminile x) = Femminile $ "all" ++ (if vocale (head x) then "'" else "a ") ++ x
	pluraleA ADeterminativo (Maschile x) = Maschile (onstz x "agli " "ai " )
	pluraleA ADeterminativo (Femminile x) = Femminile ("alle " ++ x)

data DiDeterminativo = DiDeterminativo deriving (Show,Read)
instance Polimorfo DiDeterminativo where
	singolareA DiDeterminativo (Maschile x) = Maschile (onstz x "dello " "del ")
	singolareA DiDeterminativo (Femminile x) = Femminile $ "dell" ++ (if vocale (head x) then "'" else "a ") ++ x
	pluraleA DiDeterminativo (Maschile x) = Maschile (onstz x "degli " "dei " )
	pluraleA DiDeterminativo (Femminile x) = Femminile ("delle " ++ x)

data A = A 
instance Polimorfo A where
	singolareA A x = let (y:ys) = unsex x in if vocale y then respect (("ad " ++),("ad " ++)) x else 
		respect (("a " ++),("a " ++)) x
	pluraleA A x = singolareA A x

class Render a where
	render :: a -> String

instance Render String where
	render = id
instance Render a => Render (Molteplicita a) where
	render = render . unmulti 
instance Render a => Render (Sexed a) where
	render = render . unsex 

data Composer a b = a :+: b

infixr 1 :+:
instance (Render a, Render b) => Render (Composer a b) where
	render (x :+: y) = render x ++ render y

