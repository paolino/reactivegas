{-# LANGUAGE ImportQualifiedPost #-}

module Hex (
    hexDecode,
    hexEncode,
) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word8)

hexEncode :: ByteString -> ByteString
hexEncode = BS.concat . map word8Hex . BS.unpack
  where
    word8Hex w = BS.pack [digit (w `shiftR` 4), digit (w .&. 0x0f)]
    digit n = if n < 10 then n + 48 else n + 87

hexDecode :: ByteString -> Either String ByteString
hexDecode bs
    | odd (BS.length bs) = Left "odd number of hex digits"
    | otherwise = BS.pack <$> go (BS.unpack bs)
  where
    go :: [Word8] -> Either String [Word8]
    go [] = Right []
    go [_] = Left "odd number of hex digits"
    go (hi : lo : rest) = do
        h <- digit hi
        l <- digit lo
        (h * 16 + l :) <$> go rest
    digit :: Word8 -> Either String Word8
    digit c
        | c >= 48 && c <= 57 = Right (c - 48)
        | c >= 97 && c <= 102 = Right (c - 87)
        | c >= 65 && c <= 70 = Right (c - 55)
        | otherwise = Left ("invalid hex digit: " ++ show c)
