module Fields where

import Data.List
import Text.ParserCombinators.ReadP as P
import Data.Char
import Control.Applicative




newtype User = User String deriving (Eq,Ord)
instance Read User where
	readsPrec _ = P.readP_to_S  (P.skipSpaces >> User <$> P.munch1 (isAlphaNum))
instance Show User where
	show (User u) = u
newtype Bene = Bene String deriving (Eq,Ord)
instance Show Bene where
	show (Bene o) = o
instance Read Bene where
	readsPrec _ = P.readP_to_S  (P.skipSpaces >> Bene <$> P.munch1 (isAlphaNum))

data Mese = Gennaio | Febbraio | Marzo | Aprile | Maggio | Giugno | Luglio | Agosto | Settembre | Ottobre | Novembre | Dicembre 
	deriving (Show,Read,Eq)
newtype Tempo = Tempo (Int, Mese, Int) deriving Eq
instance Show Tempo where
	show (Tempo (g,m,a)) = show g ++ " " ++ show m ++ " " ++ show a
instance Read Tempo where
	readsPrec _ = P.readP_to_S $ P.skipSpaces >> Tempo <$> (do  	g <- read <$> P.munch1 (isNumber) 
									P.skipSpaces 
									m <- read <$> P.munch1 (isAlpha) 
									P.skipSpaces 
									a <- read <$>  P.munch1 (isNumber) 
									return (g,m,a))

type Valore = Double
