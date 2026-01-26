{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      : Lib.Euro
Description : Euro currency type with parsing and display
Copyright   : (c) Paolo Veronelli, 2025
License     : BSD-3-Clause

Provides a Euro currency type that handles euros and cents,
with natural language parsing support (Italian).
-}
module Lib.Euro
    ( Euro (..)
    , DEuro (..)
    , mkDEuro
    , ($^)
    , opposite
    ) where

import Data.Ratio (Rational, approxRational, (%))
import Data.Typeable (Typeable)
import Text.ParserCombinators.ReadP
    ( ReadP
    , choice
    , optional
    , readS_to_P
    , skipSpaces
    , string
    , (<++)
    )
import Text.Printf (printf)
import Text.Read (lift, readPrec)

-- | Currency type representing Euro amounts
newtype Euro = Euro Rational
    deriving (Eq, Num, Ord, Typeable)

instance Show Euro where
    show (Euro x) =
        printf "%d euro" euros'
            ++ if cents' > 0
                then printf " e %d centesimi" cents'
                else ""
      where
        totalCents = truncate $ fromRational (x * 100) :: Int
        (euros, cents) = totalCents `divMod` 100
        (euros', cents') =
            if euros >= 0
                then (euros, cents)
                else
                    if cents > 0
                        then (euros + 1, 100 - cents)
                        else (euros, cents)

-- | Parse "euro" after optional spaces
afterSpaces :: String -> ReadP ()
afterSpaces s = skipSpaces >> string s >> return ()

instance Read Euro where
    readPrec =
        let
            -- Parse: "N euro e M centesimi" or "N,M" format
            parseEuroCents = do
                n <- readS_to_P reads
                optional $ afterSpaces "euro"
                do
                    choice [string ",", afterSpaces "e" >> return ","]
                    c <- readS_to_P reads
                    optional $ afterSpaces "centesimi"
                    let amount =
                            (n * 100 + if n < 0 then negate c else c) % 100
                    return $ Euro amount
                    <++ return (Euro $ n % 1)
            -- Fallback: parse as decimal
            parseDecimal = do
                r <- readS_to_P reads
                return . Euro $ approxRational r 0.01
        in
            lift $ parseEuroCents <++ parseDecimal

-- | Difference in Euro (represented as a function)
newtype DEuro = DEuro (Euro -> Euro)
    deriving (Typeable)

-- | Create a Euro difference from an amount
mkDEuro :: Euro -> DEuro
mkDEuro x = DEuro (+ x)

-- | Apply a Euro difference to a base amount
($^) :: DEuro -> Euro -> Euro
DEuro f $^ y = f y

-- | Negate a Euro difference
opposite :: DEuro -> DEuro
opposite f = mkDEuro . negate $ f $^ 0

instance Show DEuro where
    show (DEuro f) = show (f 0)

instance Read DEuro where
    readPrec = mkDEuro <$> readPrec
