{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module EnvelopeSpec (
    spec,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (
    Arbitrary (..),
    Gen,
    Property,
    chooseInt,
    elements,
    forAll,
    vectorOf,
    (.&&.),
    (===),
 )

import Hex (hexDecode)
import Reactivegas.Core.Blake3 (hash256)
import Reactivegas.Core.Ed25519
import Reactivegas.Core.Envelope

-- | Unwrapping helper for constant vector data.
hex :: ByteString -> ByteString
hex raw = either error id (hexDecode raw)

rfcSeed :: ByteString
rfcSeed =
    hex "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"

secretOf :: ByteString -> SecretKey
secretOf = either error id . newSecretKey

sampleHeader :: Header
sampleHeader =
    Header
        { headerGroup = GroupId (BS.replicate 32 0x01)
        , headerAuthor = MemberId (BS.replicate 32 0x02)
        , headerLamport = 7
        , headerParents = []
        , headerTs = 1724500000123
        , headerKind = Impegno
        }

sampleBody :: ByteString
sampleBody = "canonical-body"

genBytes32 :: Gen ByteString
genBytes32 = BS.pack <$> vectorOf 32 arbitrary

genBody :: Gen ByteString
genBody = do
    n <- chooseInt (0, 128)
    BS.pack <$> vectorOf n arbitrary

genParents :: Gen [EventId]
genParents = do
    n <- chooseInt (0, 2)
    vectorOf n (EventId <$> genBytes32)

-- | Newtype to host the generator without an orphan instance.
newtype SealedEnvelope = SealedEnvelope Envelope
    deriving (Show)

instance Arbitrary SealedEnvelope where
    arbitrary = do
        seed <- genBytes32
        parents <- genParents
        kind <-
            elements [Anagrafe, Impegno, Acquisto, Accredito, Assenso, Voci]
        body <- genBody
        let header =
                sampleHeader
                    { headerLamport = 42
                    , headerParents = parents
                    , headerKind = kind
                    }
        SealedEnvelope <$> either error pure (sealEnvelope (secretOf seed) header body)

spec :: Spec
spec = describe "Reactivegas.Core.Envelope" $ do
    it "seals an envelope with BLAKE3 id and Ed25519 signature" $ do
        let sk = secretOf rfcSeed
            env =
                either error id (sealEnvelope sk sampleHeader sampleBody)
            expectedId = hash256 (headerBytes sampleHeader <> sampleBody)
            pk = either error id (parsePublicKey (publicKeyBytes (derivePublicKey sk)))
            sig = either error id (parseSignature (envSig env))
        unEventId (envId env) `shouldBe` expectedId
        verifySignature pk (signableBytes env) sig `shouldBe` True

    it "roundtrips arbitrary envelopes through the canonical codec" $
        propertyRoundtrip

    it "rejects trailing bytes after the envelope" $
        forAll (arbitrary :: Gen SealedEnvelope) $ \(SealedEnvelope env) ->
            case decodeEnvelope (encodeEnvelope env <> "\x00") of
                Left _ -> True
                Right _ -> False

    it "rejects oversized bodies" $ do
        let sk = secretOf rfcSeed
            bigBody = BS.replicate (16 * 1024 + 1) 0x42
        isLeft (sealEnvelope sk sampleHeader bigBody) `shouldBe` True

    it "rejects more than two or duplicate parents" $ do
        let sk = secretOf rfcSeed
            eid = EventId (BS.replicate 32 0x03)
            three = replicate 3 eid
            dup = replicate 2 eid
        isLeft (sealEnvelope sk sampleHeader{headerParents = three} sampleBody)
            `shouldBe` True
        isLeft (sealEnvelope sk sampleHeader{headerParents = dup} sampleBody)
            `shouldBe` True

propertyRoundtrip :: Property
propertyRoundtrip = forAll (arbitrary :: Gen SealedEnvelope) $ \(SealedEnvelope env) ->
    (decodeEnvelope (encodeEnvelope env) === Right env)
        .&&. (encodeEnvelope env === encodeEnvelope env)
