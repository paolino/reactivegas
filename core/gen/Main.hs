{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Deterministic generator for @vectors/envelope.json@ (the
cross-language envelope corpus) and @vectors/reducer.json@ (golden
reducer fixtures). Every input is a compile-time constant; reruns
must produce byte-identical JSON.
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
import Reactivegas.Core.Payload
import Reactivegas.Core.Projection
import Reactivegas.Core.Reduce

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
    BSL.writeFile (root </> "vectors" </> "reducer.json") (Aeson.encode reducerDoc)
    putStrLn "wrote vectors/envelope.json and vectors/reducer.json"

-- Reducer fixtures -----------------------------------------------------

seedC :: ByteString
seedC = BS.replicate 32 0x33

skOf :: ByteString -> SecretKey
skOf seed = either error id (newSecretKey seed)

authorOf :: ByteString -> MemberId
authorOf seed = memberIdOf (derivePublicKey (skOf seed))

cidN :: Int -> CampaignId
cidN n = CampaignId (BS.pack [0xC0, fromIntegral n])

kidN :: Int -> CommitmentId
kidN n = CommitmentId (BS.pack [0xB0, fromIntegral n])

midN :: Int -> MovementId
midN n = MovementId (BS.pack [0xE0, fromIntegral n])

pay :: ByteString -> Payload -> Envelope
pay seed pl =
    either error id (sealEnvelope sk header (encodePayload pl))
  where
    sk = skOf seed
    header =
        Header
            { headerGroup = GroupId group
            , headerAuthor = authorOf seed
            , headerLamport = 0
            , headerParents = []
            , headerTs = 1724500001000
            , headerKind = payloadKind pl
            }

data ReducerStep = ReducerStep Envelope (Maybe Reject) Projection

instance Aeson.ToJSON ReducerStep where
    toJSON (ReducerStep env rej proj) =
        Aeson.object
            [ "envelope" Aeson..= Hex (encodeEnvelope env)
            , "reject" Aeson..= fmap show rej
            , "projection" Aeson..= proj
            ]

data ReducerCase = ReducerCase String [ReducerStep]

instance Aeson.ToJSON ReducerCase where
    toJSON (ReducerCase name steps) =
        Aeson.object ["name" Aeson..= name, "steps" Aeson..= map Aeson.toJSON steps]

{- | Reduce the fixed scenario, recording every step's envelope,
outcome and post-state. The generator is authoritative: it runs the
real reducer, so regenerating after an intentional semantic change
refreshes the goldens in lockstep.
-}
reduceCase :: String -> [Envelope] -> ReducerCase
reduceCase name envs =
    ReducerCase name (reverse (go emptyProjection envs []))
  where
    go _ [] acc = acc
    go p (e : es) acc = case step p e of
        Left r -> go p es (ReducerStep e (Just r) p : acc)
        Right p' -> go p' es (ReducerStep e Nothing p' : acc)

lifecycleEnvs :: [Envelope]
lifecycleEnvs =
    [ pay seedA (MemberAdmitted (authorOf seedA))
    , pay seedA (MemberAdmitted (authorOf seedB))
    , pay seedA (RoleAssigned (authorOf seedB) RoleTreasurer)
    , pay seedA (CampaignOpened (cidN 1))
    , pay seedA (CampaignCatalogSet (cidN 1) "catalog-root-v1")
    , pay seedB (CommitmentProposed (cidN 1) (kidN 1) (EuroCent 50000))
    , pay seedA (CommitmentAccepted (cidN 1) (kidN 1))
    , pay seedA (CampaignClosedForOrders (cidN 1))
    , pay seedA (OrderAllocated (cidN 1) (authorOf seedB) (EuroCent 50000))
    , pay seedA (CampaignFinalized (cidN 1))
    , pay seedB (CreditIssued (midN 1) (authorOf seedB) (EuroCent 50000))
    , pay seedB (DebitIssued (midN 2) (authorOf seedB) (EuroCent 50000))
    ]

{- | Negative steps interleaved with the valid prefix that keeps the
projection reachable; every negative step must be rejected and leaves
the projection untouched.
-}
rejectionEnvs :: [Envelope]
rejectionEnvs =
    [ pay seedA (MemberAdmitted (authorOf seedA))
    , pay seedA (MemberAdmitted (authorOf seedB))
    , -- admission by a non-referente
      pay seedB (MemberAdmitted (authorOf seedC))
    , pay seedA (CampaignOpened (cidN 1))
    , -- commitment while still collecting the catalog
      pay seedB (CommitmentProposed (cidN 1) (kidN 8) (EuroCent 100))
    , pay seedA (CampaignCatalogSet (cidN 1) "catalog-root-v1")
    , -- unknown signer proposes
      pay seedC (CommitmentProposed (cidN 1) (kidN 7) (EuroCent 100))
    , -- allocation before the order phase closes
      pay seedA (OrderAllocated (cidN 1) (authorOf seedB) (EuroCent 100))
    , -- treasurer role missing for balance movements
      pay seedB (CreditIssued (midN 1) (authorOf seedB) (EuroCent 100))
    , -- closure by a non-referente
      pay seedB (CampaignClosedForOrders (cidN 1))
    ]

reducerCases :: [ReducerCase]
reducerCases =
    [ reduceCase "order-lifecycle" lifecycleEnvs
    , reduceCase "rejections" rejectionEnvs
    ]

reducerDoc :: Aeson.Value
reducerDoc =
    Aeson.object
        [ "version" Aeson..= (1 :: Int)
        , "cases" Aeson..= map Aeson.toJSON reducerCases
        ]

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
