{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Deterministic generator for @vectors/envelope.json@, the
cross-language golden corpus shared with the PureScript frontend.
Every input is a compile-time constant; reruns must produce
byte-identical JSON.
-}
module Main (main) where

import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text.Encoding qualified as TE
import Data.Word (Word64)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath (takeDirectory, (</>))

import Reactivegas.Core.Blake3 (hash256)
import Reactivegas.Core.Ed25519
import Reactivegas.Core.Envelope

newtype Hex = Hex ByteString

instance Aeson.ToJSON Hex where
    toJSON (Hex bs) = Aeson.String (TE.decodeUtf8 (hexEncode bs))

data CaseJson = CaseJson
    { cjEnv :: Envelope
    , cjSeed :: ByteString
    , cjPk :: PublicKey
    , cjHeader :: Header
    }

instance Aeson.ToJSON CaseJson where
    toJSON (CaseJson env seed pk h) =
        Aeson.object
            [ "seed" Aeson..= Hex seed
            , "group" Aeson..= Hex (unGroupId (headerGroup h))
            , "author" Aeson..= Hex (unMemberId (headerAuthor h))
            , "public_key" Aeson..= Hex (publicKeyBytes pk)
            , "lamport" Aeson..= headerLamport h
            , "parents" Aeson..= map (Hex . unEventId) (headerParents h)
            , "ts" Aeson..= headerTs h
            , "kind" Aeson..= show (headerKind h)
            , "body" Aeson..= Hex (envBody env)
            , "id" Aeson..= Hex (unEventId (envId env))
            , "encoded" Aeson..= Hex (encodeEnvelope env)
            , "sig" Aeson..= Hex (envSig env)
            ]

hexEncode :: ByteString -> ByteString
hexEncode = BS.concat . map word8Hex . BS.unpack
  where
    word8Hex w = BS.pack [digit (w `div` 16), digit (w `mod` 16)]
    digit n = if n < 10 then n + 48 else n + 87

hexDecodeStrict :: ByteString -> ByteString
hexDecodeStrict raw = case decodeHex raw of
    Right bs -> bs
    Left err -> error ("generator vector hex: " ++ err)
  where
    decodeHex b
        | odd (BS.length b) = Left "odd number of hex digits"
        | otherwise = BS.pack <$> traverse pair (pairs b)
    pairs b =
        [ (BS.index b i, BS.index b (i + 1))
        | i <- [0, 2 .. BS.length b - 2]
        ]
    pair (hi, lo) = do
        h <- digit hi
        l <- digit lo
        pure (h * 16 + l)
    digit c
        | c >= 48 && c <= 57 = Right (c - 48)
        | c >= 97 && c <= 102 = Right (c - 87)
        | otherwise = Left ("invalid hex digit: " ++ show c)

-- | Member identity is BLAKE3-256 of the member's public key.
memberIdOf :: PublicKey -> MemberId
memberIdOf pk = MemberId (hash256 (publicKeyBytes pk))

golden ::
    ByteString ->
    Word64 ->
    [EventId] ->
    Word64 ->
    EventKind ->
    ByteString ->
    CaseJson
golden seed lamport parents ts kind body =
    CaseJson
        { cjEnv = env
        , cjSeed = seed
        , cjPk = pk
        , cjHeader = header
        }
  where
    sk = either error id (newSecretKey seed)
    pk = derivePublicKey sk
    header =
        Header
            { headerGroup = GroupId group
            , headerAuthor = memberIdOf pk
            , headerLamport = lamport
            , headerParents = parents
            , headerTs = ts
            , headerKind = kind
            }
    env = either error id (sealEnvelope sk header body)

group :: ByteString
group = BS.replicate 32 0x01

seedA, seedB :: ByteString
seedA =
    hexDecodeStrict
        "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"
seedB =
    hexDecodeStrict
        "4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb"

patternBytes :: Int -> ByteString
patternBytes n = BS.pack [fromIntegral (i `mod` 251) | i <- [0 .. n - 1]]

main :: IO ()
main = do
    root <- locateRoot
    let g1 = golden seedA 7 [] 1724500000123 Impegno ""
        g2 =
            golden
                seedA
                8
                [envId (cjEnv g1)]
                1724500000124
                Acquisto
                (patternBytes 300)
        g3 =
            golden
                seedB
                1
                [envId (cjEnv g1), envId (cjEnv g2)]
                1724500000125
                Assenso
                "assenso"
        g4 = golden seedB 2 [] 1724500000126 Voci (patternBytes 1024)
        doc =
            Aeson.object
                [ "version" Aeson..= (1 :: Int)
                , "cases" Aeson..= map Aeson.toJSON [g1, g2, g3, g4]
                ]
    BSL.writeFile (root </> "vectors" </> "envelope.json") (Aeson.encode doc)
    putStrLn "wrote vectors/envelope.json"

locateRoot :: IO FilePath
locateRoot = getCurrentDirectory >>= go
  where
    go dir = do
        found <- doesFileExist (dir </> "vectors" </> "blake3.json")
        if found
            then pure dir
            else case takeDirectory dir of
                parent
                    | parent == dir ->
                        fail "repository root not found (vectors/blake3.json missing)"
                parent -> go parent
