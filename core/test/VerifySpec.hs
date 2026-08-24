{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module VerifySpec (
    spec,
) where

import Data.Bits (shiftL, xor)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Word (Word64, Word8)
import Test.Hspec (
    Spec,
    describe,
    expectationFailure,
    it,
    shouldBe,
 )
import Test.QuickCheck (
    Arbitrary (..),
    Gen,
    Property,
    choose,
    forAll,
    property,
    vectorOf,
 )

import Hex (hexDecode)
import Reactivegas.Core.Blake3 (hash256)
import Reactivegas.Core.Ed25519
import Reactivegas.Core.Envelope
import Reactivegas.Core.Verify

-- | Unwrapping helper for constant vector data.
hex :: ByteString -> ByteString
hex raw = either error id (hexDecode raw)

rfcSeedA, rfcSeedB :: ByteString
rfcSeedA =
    hex "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"
rfcSeedB =
    hex "4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb"

skA, skB :: SecretKey
skA = secretOf rfcSeedA
skB = secretOf rfcSeedB

pkA, pkB :: PublicKey
pkA = derivePublicKey skA
pkB = derivePublicKey skB

secretOf :: ByteString -> SecretKey
secretOf = either error id . newSecretKey

-- | Member identity is BLAKE3-256 of the member's public key.
memberIdOf :: PublicKey -> MemberId
memberIdOf pk = MemberId (hash256 (publicKeyBytes pk))

groupOf :: GroupId
groupOf = GroupId (BS.replicate 32 0x01)

knownKeys :: MemberId -> Maybe PublicKey
knownKeys m
    | m == memberIdOf pkA = Just pkA
    | m == memberIdOf pkB = Just pkB
    | otherwise = Nothing

sealBy :: SecretKey -> Word64 -> [EventId] -> ByteString -> Envelope
sealBy sk lamport parents body =
    either error id $
        sealEnvelope
            sk
            Header
                { headerGroup = groupOf
                , headerAuthor = memberIdOf (derivePublicKey sk)
                , headerLamport = lamport
                , headerParents = parents
                , headerTs = 1000 + lamport
                , headerKind = Impegno
                }
            body

stepOrDie :: Verifier -> Envelope -> Verifier
stepOrDie v e = either (error . show) id (verifyStep v e)

-- | Assert acceptance without requiring Show/Eq on the verifier.
accepts :: Verifier -> Envelope -> IO ()
accepts v e = case verifyStep v e of
    Right _ -> pure ()
    Left err -> expectationFailure ("expected acceptance, got " ++ show err)

rejectsWith :: VerifyError -> Verifier -> Envelope -> IO ()
rejectsWith want v e = case verifyStep v e of
    Left err -> err `shouldBe` want
    Right _ -> expectationFailure ("expected " ++ show want ++ ", got acceptance")

spec :: Spec
spec = describe "Reactivegas.Core.Verify" $ do
    it "accepts a freshly sealed envelope" $
        accepts (verifierWith knownKeys) (sealBy skA 1 [] "body")

    it "rejects a tampered body as an id mismatch" $
        let env = sealBy skA 1 [] "body"
            bad = env{envBody = "bodY"}
         in rejectsWith ErrIdMismatch (verifierWith knownKeys) bad

    it "rejects a corrupted signature" $
        let env = sealBy skA 1 [] "body"
            bad = env{envSig = flipFirstByte (envSig env)}
         in rejectsWith ErrBadSignature (verifierWith knownKeys) bad

    it "rejects an unresolvable author key" $
        rejectsWith
            ErrUnknownAuthor
            (verifierWith (const Nothing))
            (sealBy skA 1 [] "body")

    it "rejects replaying an already accepted envelope" $
        let env = sealBy skA 1 [] "body"
            v1 = stepOrDie (verifierWith knownKeys) env
         in rejectsWith ErrReplay v1 env

    it "enforces per-author lamport growth along parent chains" $
        let parent = sealBy skA 5 [] "parent"
            v1 = stepOrDie (verifierWith knownKeys) parent
            childFlat = sealBy skA 5 [envId parent] "child"
            childBack = sealBy skA 4 [envId parent] "child"
            childNext = sealBy skA 6 [envId parent] "child"
         in do
                rejectsWith ErrLamportRegression v1 childFlat
                rejectsWith ErrLamportRegression v1 childBack
                accepts v1 childNext

    it "accepts children from other authors regardless of their lamport" $
        let parent = sealBy skA 900 [] "parent"
            v1 = stepOrDie (verifierWith knownKeys) parent
            childOther = sealBy skB 1 [envId parent] "child"
         in accepts v1 childOther

    it "rejects oversized bodies reaching the pipeline" $
        let env = sealBy skA 1 [] "body"
            big = env{envBody = BS.replicate (16 * 1024 + 1) 0x07}
         in rejectsWith ErrBodyTooLarge (verifierWith knownKeys) big

    it "mutation fuzz: single-bit flips never yield an accepted envelope" $
        property bitFlipFuzz

bitFlipFuzz :: Property
bitFlipFuzz = forAll (arbitrary :: Gen SealedEnvelope) $ \(SealedEnvelope env) ->
    let raw = encodeEnvelope env
     in forAll (choose (0, BS.length raw * 8 - 1)) $ \bitIx ->
            let byteIx = bitIx `div` 8
                mask = (1 :: Int) `shiftL` (bitIx `mod` 8)
                mutate bs =
                    BS.concat
                        [ BS.take byteIx bs
                        , BS.singleton (BS.index bs byteIx `xorW` fromIntegral mask)
                        , BS.drop (byteIx + 1) bs
                        ]
             in case decodeEnvelope (mutate raw) of
                    Left _ -> property True
                    Right mutated ->
                        property (isLeft (verifyStep (verifierWith knownKeys) mutated))

xorW :: Word8 -> Word8 -> Word8
xorW = xor

flipFirstByte :: ByteString -> ByteString
flipFirstByte bs = BS.cons (BS.head bs `xor` 1) (BS.tail bs)

-- Local generator host (avoids an orphan Arbitrary instance).
data SealedEnvelope = SealedEnvelope Envelope
    deriving (Show)

instance Arbitrary SealedEnvelope where
    arbitrary = do
        seed <- genBytes32
        parents <- genParents
        body <- genBody
        SealedEnvelope <$> either error pure (sealBy' seed parents body)
      where
        sealBy' seed parents body = do
            let sk = secretOf seed
            sealEnvelope
                sk
                Header
                    { headerGroup = groupOf
                    , headerAuthor = memberIdOf (derivePublicKey sk)
                    , headerLamport = 42
                    , headerParents = parents
                    , headerTs = 12345
                    , headerKind = Assenso
                    }
                body

genBytes32 :: Gen ByteString
genBytes32 = BS.pack <$> vectorOf 32 arbitrary

genParents :: Gen [EventId]
genParents = do
    n <- choose (0, 2)
    vectorOf n (EventId <$> genBytes32)

genBody :: Gen ByteString
genBody = do
    n <- choose (0, 128)
    BS.pack <$> vectorOf n arbitrary
