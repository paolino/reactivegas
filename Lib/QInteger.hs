{-# LANGUAGE DeriveDataTypeable #-}

{- |
Module      : Lib.QInteger
Description : Chunked integer representation for display
Copyright   : (c) Paolo Veronelli, 2025
License     : BSD-3-Clause

Provides a chunked integer type that displays large integers
in groups of digits for improved readability.
-}
module Lib.QInteger
    ( makeQInteger
    , fromQInteger
    , QInteger
    ) where

import Data.List (unfoldr)
import Data.Typeable (Typeable)
import Text.Printf (printf)

-- | Number of digits per chunk
chunkSize :: Int
chunkSize = 4

-- | Integer represented as chunks of digits for display
newtype QInteger = QInteger [Int]
    deriving (Eq, Ord, Typeable)

instance Show QInteger where
    show (QInteger xs) =
        concatMap (printf ("%0" ++ show chunkSize ++ "d ")) xs

-- | Parse chunked integer format
readChunks :: String -> ([Int], String)
readChunks [] = ([], "")
readChunks xs =
    let (prefix, rest) = splitAt (chunkSize + 1) $ dropWhile (== ' ') xs
    in  if length prefix < (chunkSize + 1)
            then ([], xs)
            else case reads prefix of
                [] -> ([], xs)
                [(i, " ")] ->
                    let (js, remaining) = readChunks rest
                    in  (i : js, remaining)
                _ -> ([], xs)

instance Read QInteger where
    readsPrec _ x = case readChunks x of
        ([], _) -> []
        (js, remaining) -> [(QInteger js, remaining)]

-- | Extract one chunk from an integer
takeChunk :: Integer -> (Int, Integer)
takeChunk x =
    let (quotient, remainder) = x `divMod` (10 ^ chunkSize)
    in  (fromIntegral remainder, quotient)

-- | Convert an 'Integer' to chunked representation
makeQInteger :: Integer -> QInteger
makeQInteger = QInteger . unfoldr step
  where
    step 0 = Nothing
    step x = Just $ takeChunk x

-- | Convert chunked representation back to 'Integer'
fromQInteger :: QInteger -> Integer
fromQInteger (QInteger xs) =
    foldr (\x acc -> acc * 10 ^ chunkSize + fromIntegral x) 0 xs
