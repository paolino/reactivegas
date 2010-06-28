{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module Lib.Euro where

-- import Text.ParserCombinators.ReadPrec
import Control.Arrow
import Control.Applicative ((<$>))
import Text.ParserCombinators.ReadP
import Text.Read (readPrec, lift)
import Text.Printf
import Data.Ratio
import Data.List


newtype Euro = Euro Rational deriving (Eq,Num,Ord)

instance Show Euro where
	show (Euro x) = printf "%d euro" y' ++ if yc' > 0 then printf " e %d centesimi" yc' else "" where
		z = truncate $ fromRational (x * 100) :: Int
		(y,yc) = z `divMod` 100 
		(y',yc') = if y >= 0 then (y,yc) else 
			if yc > 0 then (y + 1, 100 - yc) else (y,yc)

afterSpaces x =  skipSpaces >> string x
instance Read Euro where
	readPrec = let 
		com =  do
			n <- readS_to_P reads
			optional $ afterSpaces "euro"
			do	choice [string ",",afterSpaces "e"]
				c <-  readS_to_P reads
				optional $ afterSpaces "centesimi"
				return . Euro $ (n * 100  + if n < 0 then negate c else c) % 100
			  <++ (return . Euro $ n % 1)
		bug = do 
			r <- readS_to_P reads
			return . Euro $ approxRational r 0.01
		in lift $ com <++ bug

newtype DEuro = DEuro (Euro -> Euro)

mkDEuro :: Euro -> DEuro
mkDEuro x = DEuro (+ x)

($^) :: DEuro -> Euro -> Euro
DEuro x $^ y =  x y

opposite :: DEuro -> DEuro 
opposite f = mkDEuro . negate $ f $^ 0
instance Show DEuro where
	show (DEuro f) = show (f $ fromInteger 0)

instance Read DEuro where
	readPrec = mkDEuro <$> readPrec
		
