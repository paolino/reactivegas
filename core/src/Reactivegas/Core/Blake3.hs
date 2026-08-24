{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}

{- |
Module      : Reactivegas.Core.Blake3
Description : Pure BLAKE3 hashing (RFC draft-irtf-cfrg-blake3)
Copyright   : (c) 2026 Paolo Veronelli
License     : BSD3
Maintainer  : Paolo Veronelli <paolo.veronelli@gmail.com>
Stability   : experimental

Minimal pure-Haskell BLAKE3 covering plain, keyed and derive-key
modes, validated against the official test vectors in
@vectors/blake3.json@. Vendored to keep @reactivegas-core@ free of
native-library dependencies.
-}
module Reactivegas.Core.Blake3 (
    hash256,
    extendedHash,
    keyedHash256,
    deriveKey256,
) where

import Data.Array.Unboxed (UArray, listArray, (!), (//))
import Data.Bits (
    rotateR,
    shiftL,
    shiftR,
    xor,
    (.&.),
    (.|.),
 )
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List (foldl')
import Data.Word (Word32, Word64)

iv :: [Word32]
iv =
    [ 0x6A09E667
    , 0xBB67AE85
    , 0x3C6EF372
    , 0xA54FF53A
    , 0x510E527F
    , 0x9B05688C
    , 0x1F83D9AB
    , 0x5BE0CD19
    ]

msgPermutation :: [Int]
msgPermutation = [2, 6, 3, 10, 7, 0, 4, 13, 1, 11, 12, 5, 9, 14, 15, 8]

flagChunkStart, flagChunkEnd, flagParent, flagRoot :: Word32
flagChunkStart = 1
flagChunkEnd = 2
flagParent = 4
flagRoot = 8

flagKeyedHash, flagDeriveKeyContext, flagDeriveKeyMaterial :: Word32
flagKeyedHash = 16
flagDeriveKeyContext = 32
flagDeriveKeyMaterial = 64

blockSize :: Int
blockSize = 64

chunkSize :: Int
chunkSize = 1024

gMix ::
    Word32 ->
    Word32 ->
    Word32 ->
    Word32 ->
    Word32 ->
    Word32 ->
    (Word32, Word32, Word32, Word32)
gMix a b c d mx my = (a4, b4, c4, d4)
  where
    a1 = a + b + mx
    d1 = rotateR (d `xor` a1) 16
    c1 = c + d1
    b1 = rotateR (b `xor` c1) 12
    a4 = a1 + b1 + my
    d4 = rotateR (d1 `xor` a4) 8
    c4 = c1 + d4
    b4 = rotateR (b1 `xor` c4) 7

mixAt ::
    UArray Int Word32 ->
    Int ->
    Int ->
    Int ->
    Int ->
    Word32 ->
    Word32 ->
    UArray Int Word32
mixAt s ia ib ic id_ mx my =
    let (a', b', c', d') = gMix (s ! ia) (s ! ib) (s ! ic) (s ! id_) mx my
     in s // [(ia, a'), (ib, b'), (ic, c'), (id_, d')]

blakeRound :: UArray Int Word32 -> [Word32] -> UArray Int Word32
blakeRound s m = foldl' step s columns
  where
    step st (ia, ib, ic, id_, mx, my) = mixAt st ia ib ic id_ mx my
    firstOf (w : _) = w
    firstOf [] = error "Blake3.blakeRound: empty message block"
    columns =
        [ (0, 4, 8, 12, firstOf m, m !! 1)
        , (1, 5, 9, 13, m !! 2, m !! 3)
        , (2, 6, 10, 14, m !! 4, m !! 5)
        , (3, 7, 11, 15, m !! 6, m !! 7)
        , (0, 5, 10, 15, m !! 8, m !! 9)
        , (1, 6, 11, 12, m !! 10, m !! 11)
        , (2, 7, 8, 13, m !! 12, m !! 13)
        , (3, 4, 9, 14, m !! 14, m !! 15)
        ]

permute :: [Word32] -> [Word32]
permute m = map (m !!) msgPermutation

compress ::
    [Word32] -> [Word32] -> Word64 -> Word32 -> Word32 -> [Word32]
compress cv blockWords counter blockLenW flags = post final
  where
    state0 :: UArray Int Word32
    state0 =
        listArray
            (0, 15)
            ( cv
                ++ take 4 iv
                ++ [ fromIntegral (counter .&. 0xFFFFFFFF)
                   , fromIntegral (counter `shiftR` 32)
                   , blockLenW
                   , flags
                   ]
            )
    rounds :: UArray Int Word32 -> [Word32] -> Int -> UArray Int Word32
    rounds st _ 7 = st
    rounds st m r = rounds (blakeRound st m) (permute m) (r + 1)
    final :: UArray Int Word32
    final = rounds state0 blockWords 0
    post :: UArray Int Word32 -> [Word32]
    post f =
        [f ! i `xor` f ! (i + 8) | i <- [0 .. 7]]
            ++ [f ! (i + 8) `xor` cv !! i | i <- [0 .. 7]]

-- | Pad a partial block to a full block of zero bytes.
paddedBlock :: ByteString -> ByteString
paddedBlock bs =
    let n = BS.length bs
     in if n >= blockSize
            then BS.take blockSize bs
            else bs <> BS.replicate (blockSize - n) 0

{- | Little-endian words from a bytestring whose length is a
multiple of four.
-}
leWords :: ByteString -> [Word32]
leWords bs =
    [ fromIntegral (BS.index bs i)
        .|. (fromIntegral (BS.index bs (i + 1)) `shiftL` 8)
        .|. (fromIntegral (BS.index bs (i + 2)) `shiftL` 16)
        .|. (fromIntegral (BS.index bs (i + 3)) `shiftL` 24)
    | i <- [0, 4 .. BS.length bs - 4]
    ]

-- | Low 32 bytes of the first eight words, little-endian.
leBytes :: [Word32] -> ByteString
leBytes = BS.pack . concatMap wordBytes
  where
    wordBytes w =
        [ fromIntegral (w .&. 0xFF)
        , fromIntegral ((w `shiftR` 8) .&. 0xFF)
        , fromIntegral ((w `shiftR` 16) .&. 0xFF)
        , fromIntegral ((w `shiftR` 24) .&. 0xFF)
        ]

data Output = Output
    { oIcv :: [Word32]
    , oBlockWords :: [Word32]
    , oCounter :: !Word64
    , oBlockLen :: !Word32
    , oFlags :: !Word32
    }

outputChaining :: Output -> [Word32]
outputChaining o =
    take 8 (compress (oIcv o) (oBlockWords o) (oCounter o) (oBlockLen o) (oFlags o))

-- | Chaining value of a parent node over two child chaining values.
parentChaining :: [Word32] -> Word32 -> [Word32] -> [Word32] -> [Word32]
parentChaining key baseFlags leftCv rightCv =
    take
        8
        ( compress
            key
            (leftCv ++ rightCv)
            0
            (fromIntegral blockSize)
            (flagParent .|. baseFlags)
        )

rootBytes :: Int -> Output -> ByteString
rootBytes n o =
    BS.take n (BS.concat (map outputBlock [0 .. outBlocks - 1]))
  where
    outBlocks = max 1 ((n + blockSize - 1) `div` blockSize)
    outputBlock i =
        leBytes
            ( compress
                (oIcv o)
                (oBlockWords o)
                (fromIntegral i)
                (oBlockLen o)
                (oFlags o .|. flagRoot)
            )

data ChunkState = ChunkState
    { csCv :: [Word32]
    , csCounter :: !Word64
    , csBlock :: !ByteString
    , csBlocksCompressed :: !Int
    , csFlags :: !Word32
    }

newChunkState :: [Word32] -> Word64 -> Word32 -> ChunkState
newChunkState key counter flags =
    ChunkState
        { csCv = key
        , csCounter = counter
        , csBlock = BS.empty
        , csBlocksCompressed = 0
        , csFlags = flags
        }

chunkLenSoFar :: ChunkState -> Int
chunkLenSoFar cs = csBlocksCompressed cs * blockSize + BS.length (csBlock cs)

chunkStartFlag :: ChunkState -> Word32
chunkStartFlag cs = if csBlocksCompressed cs == 0 then flagChunkStart else 0

csUpdate :: ChunkState -> ByteString -> ChunkState
csUpdate cs input
    | BS.null input = cs
    | BS.length (csBlock cs) == blockSize =
        let cv' =
                take
                    8
                    ( compress
                        (csCv cs)
                        (leWords (paddedBlock (csBlock cs)))
                        (csCounter cs)
                        (fromIntegral blockSize)
                        (csFlags cs .|. chunkStartFlag cs)
                    )
         in csUpdate cs{csCv = cv', csBlocksCompressed = csBlocksCompressed cs + 1, csBlock = BS.empty} input
    | otherwise =
        let want = blockSize - BS.length (csBlock cs)
            (taken, rest) = BS.splitAt want input
         in csUpdate cs{csBlock = csBlock cs <> taken} rest

chunkOutput :: ChunkState -> Output
chunkOutput cs =
    Output
        { oIcv = csCv cs
        , oBlockWords = leWords (paddedBlock (csBlock cs))
        , oCounter = csCounter cs
        , oBlockLen = fromIntegral (BS.length (csBlock cs))
        , oFlags = csFlags cs .|. chunkStartFlag cs .|. flagChunkEnd
        }

data Hasher = Hasher
    { hChunk :: !ChunkState
    , hKey :: [Word32]
    , hStack :: [[Word32]]
    , hFlags :: !Word32
    }

hasherNew :: [Word32] -> Word32 -> Hasher
hasherNew key flags =
    Hasher{hChunk = newChunkState key 0 flags, hKey = key, hStack = [], hFlags = flags}

addChunkChainingValue :: Hasher -> [Word32] -> Word64 -> Hasher
addChunkChainingValue h newCv totalChunks = go (hStack h) newCv totalChunks
  where
    go stack acc tc
        | even tc = case stack of
            (top : rest) ->
                go rest (parentChaining (hKey h) (hFlags h) top acc) (tc `shiftR` 1)
            [] -> error "Blake3.addChunkChainingValue: empty CV stack"
        | otherwise = h{hStack = acc : stack}

hasherUpdate :: Hasher -> ByteString -> Hasher
hasherUpdate h input
    | BS.null input = h
    | otherwise = go h input
  where
    go acc bs
        | BS.null bs = acc
        | chunkLenSoFar (hChunk acc) == chunkSize =
            let done = chunkOutput (hChunk acc)
                counter = csCounter (hChunk acc) + 1
                acc' =
                    addChunkChainingValue
                        acc
                        (outputChaining done)
                        counter
                acc'' = acc'{hChunk = newChunkState (hKey h) counter (hFlags h)}
             in go acc'' bs
        | otherwise =
            let cap = chunkSize - chunkLenSoFar (hChunk acc)
                (taken, rest) = BS.splitAt cap bs
                acc' = acc{hChunk = csUpdate (hChunk acc) taken}
             in go acc' rest

hasherFinalize :: Int -> Hasher -> ByteString
hasherFinalize n h = rootBytes n (foldl' absorb (chunkOutput (hChunk h)) (hStack h))
  where
    -- The stack is newest-first; each older chaining value becomes the
    -- left child of the accumulated subtree, per the reference
    -- implementation.
    absorb prev cv =
        Output
            { oIcv = hKey h
            , oBlockWords = cv ++ outputChaining prev
            , oCounter = 0
            , oBlockLen = fromIntegral blockSize
            , oFlags = flagParent .|. hFlags h
            }

-- | BLAKE3-256 of the input.
hash256 :: ByteString -> ByteString
hash256 msg = extendedHash 32 msg

-- | BLAKE3 of arbitrary output length (plain mode).
extendedHash :: Int -> ByteString -> ByteString
extendedHash n msg = hasherFinalize n (hasherUpdate (hasherNew iv 0) msg)

-- | Keyed BLAKE3-256. The key must be 32 bytes.
keyedHash256 :: ByteString -> ByteString -> ByteString
keyedHash256 key msg
    | BS.length key /= 32 = error "Blake3.keyedHash256: key must be 32 bytes"
    | otherwise =
        hasherFinalize
            32
            ( hasherUpdate
                (hasherNew (leWords key) flagKeyedHash)
                msg
            )

-- | Key derivation mode BLAKE3-256. The context is a UTF-8 string.
deriveKey256 :: ByteString -> ByteString -> ByteString
deriveKey256 context material =
    let contextHash =
            hasherFinalize
                32
                (hasherUpdate (hasherNew iv flagDeriveKeyContext) context)
        contextKey = leWords contextHash
     in hasherFinalize
            32
            ( hasherUpdate
                (hasherNew contextKey flagDeriveKeyMaterial)
                material
            )
