{-# LANGUAGE NoMonomorphismRestriction, Rank2Types, ScopedTypeVariables, GADTs, ExistentialQuantification, DeriveDataTypeable #-}

module Lib.Units where

import Data.Ratio
import Data.Typeable
import Lib.NaturalLanguage

class Eq a => UnitClass a where
	base :: a -> (Rational, a)

------------------------------------------------------------------
data Pesi = Grammo | Chilogrammo   deriving (Read,Show,Typeable,Enum,Eq, Bounded)

instance UnitClass Pesi where
	base Grammo = (1, Grammo)
	base Chilogrammo = (1000, Grammo)

instance Name Pesi where
	singolare Grammo = Maschile "grammo"
	singolare Chilogrammo = Maschile "chilogrammo"
	plurale Grammo = Maschile "grammi"
	plurale Chilogrammo = Maschile "chilogrammi"

data Volumi = Litro |  Millilitro  | Centilitro deriving (Read,Show,Typeable,Enum,Eq, Bounded)
instance UnitClass Volumi where
	base Litro = (1,Litro)
	base Millilitro = (1%1000,Litro)
	base Centilitro = (1%100,Litro)

instance Name Volumi where
	singolare Litro = Maschile "litro"
	singolare Millilitro = Maschile "millilitro"
	singolare Centilitro = Maschile "centilitro"
	plurale Litro = Maschile "litri"
	plurale Millilitro = Maschile "millilitro"
	plurale Centilitro = Maschile "centilitri"

data Denaro = Euro | Centesimo  deriving (Read,Show,Typeable,Enum,Eq)

instance UnitClass Denaro where
	base Euro = (1,Euro)
	base Centesimo = (1%100,Euro)

instance Name Denaro where
	singolare Euro = Maschile "euro"
	singolare Centesimo = Maschile "centesimo"
	plurale Euro = Maschile "euro"
	plurale Centesimo = Maschile "centesimi"


data Unità = Unità  deriving (Read,Show,Typeable,Eq, Enum)
instance Name Unità where
	singolare Unità = Femminile "unità"
	plurale Unità = Femminile "unità"

instance UnitClass Unità where
	base Unità = (1,Unità)

--------------------------------------------------------------------------

