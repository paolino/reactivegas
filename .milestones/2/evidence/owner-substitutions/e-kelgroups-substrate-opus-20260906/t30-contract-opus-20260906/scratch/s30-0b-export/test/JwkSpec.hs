{- |
Module      : JwkSpec
Description : Tests for JWK private key export/import
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0
-}
module JwkSpec (spec) where

import Crypto.PubKey.Ed25519 qualified as Ed
import Data.Aeson
    ( Value (..)
    , eitherDecode
    )
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as LBS
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)
import KelGroups.Jwk
import KelGroups.Store
    ( closeKEL
    , openKEL
    , openKELWithIdentity
    , serverKeyPair
    )
import KelGroups.Trivial
    ( trivialFold
    , trivialInitial
    )
import Keri.Crypto.Ed25519
    ( KeyPair (..)
    , generateKeyPair
    , publicKeyBytes
    , secretKeyBytes
    , secretKeyFromBytes
    , sign
    , verify
    )
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , counterexample
    , property
    , vectorOf
    , (.&&.)
    , (===)
    )

-- --------------------------------------------------------
-- RFC 8037 appendix vectors
-- --------------------------------------------------------

rfcD, rfcX, rfcSignatureB64 :: Text
rfcD =
    "nWGxne_9WmC6hEr0kuwsxERJxWl7MmkZcDusAxyuf2A"
rfcX =
    "11qYAYKxCrfVS_7TyWQHOg7hcvPapiMlrwIaaPcHURo"
rfcSignatureB64 =
    "hgyY0il_MGCjP0JzlnLWG1PPOt7-09PGcvMg3AIbQR6d\
    \WbhijcNR4ki4iylGjg5BhVsPt9g7sVvpAr_MuM0KAg"

-- JWS signing input from RFC 8037 appendix A.4.
rfcSigningInput :: ByteString
rfcSigningInput =
    BC.pack
        "eyJhbGciOiJFZERTQSJ9.\
        \RXhhbXBsZSBvZiBFZDI1NTE5IHNpZ25pbmc"

rfcJwkJson :: Text
rfcJwkJson = jsonFor rfcX rfcD

-- --------------------------------------------------------
-- Helpers
-- --------------------------------------------------------

newtype Seed32 = Seed32 ByteString
    deriving stock (Show)

instance Arbitrary Seed32 where
    arbitrary =
        Seed32 . BS.pack
            <$> vectorOf 32 (arbitrary :: Gen Word8)

mkPairFromSeed :: ByteString -> Either String KeyPair
mkPairFromSeed seed = do
    sk <- secretKeyFromBytes seed
    pure
        KeyPair
            { secretKey = sk
            , publicKey = Ed.toPublic sk
            }

jsonFor :: Text -> Text -> Text
jsonFor = jsonWith "OKP" "Ed25519"

jsonWith :: Text -> Text -> Text -> Text -> Text
jsonWith kty crv x d =
    "{\"kty\":\""
        <> kty
        <> "\",\"crv\":\""
        <> crv
        <> "\",\"x\":\""
        <> x
        <> "\",\"d\":\""
        <> d
        <> "\"}"

decodeJsonText :: Text -> Either String KeyPair
decodeJsonText =
    decodeJwkJson . LBS.fromStrict . TE.encodeUtf8

-- Asserts rejection and that no key material leaks
-- into the error.
expectRejected :: Text -> Text -> Text -> Expectation
expectRejected label x d =
    case decodeJsonText (jsonFor x d) of
        Right _ ->
            expectationFailure
                ("expected rejection: " <> T.unpack label)
        Left err -> do
            mapM_
                ( \frag ->
                    err
                        `shouldNotSatisfy` T.isInfixOf frag
                            . T.pack
                )
                [rfcX, rfcD]
            err `shouldNotBe` ""

expectMessage :: Text -> Either String KeyPair -> Text -> Expectation
expectMessage label result fragment =
    case result of
        Right _ ->
            expectationFailure
                ("expected rejection: " <> T.unpack label)
        Left err ->
            err `shouldContain` T.unpack fragment

bytesOf :: Text -> ByteString
bytesOf t = either error id (decodeB64url t)

-- --------------------------------------------------------
-- Spec
-- --------------------------------------------------------

spec :: Spec
spec = describe "KelGroups.Jwk" $ do
    describe "RFC 8037 appendix vectors" $ do
        it "imports the A.1 Ed25519 private key" $ do
            kp <- either fail pure (decodeJsonText rfcJwkJson)
            publicKeyBytes (publicKey kp)
                `shouldBe` bytesOf rfcX
            secretKeyBytes (secretKey kp)
                `shouldBe` bytesOf rfcD

        it "reproduces the A.4 signature" $ do
            kp <- either fail pure (decodeJsonText rfcJwkJson)
            sign kp rfcSigningInput
                `shouldBe` bytesOf rfcSignatureB64

    describe "round trip" $ do
        it "preserves generated key pairs" $
            property roundTripProp
        it "signatures verify across export/import" $
            property signatureProp

    describe "JSON shape" $ do
        it "emits exactly kty/crv/x/d" $ do
            kp <- generateKeyPair
            let json = encodeJwkJson (keyPairToJwk kp)
            case eitherDecode json :: Either String Value of
                Left err -> expectationFailure err
                Right (Object o) -> do
                    sort (K.toText <$> KM.keys o)
                        `shouldBe` sort ["crv", "d", "kty", "x"]
                    KM.lookup (K.fromText "kty") o
                        `shouldBe` Just (String "OKP")
                    KM.lookup (K.fromText "crv") o
                        `shouldBe` Just (String "Ed25519")
                Right _ ->
                    expectationFailure "expected JSON object"

        it "never shows key material via Show" $ do
            kp <- generateKeyPair
            let jwk = keyPairToJwk kp
            show jwk `shouldBe` "Jwk<redacted>"
            show jwk `shouldNotContain` T.unpack (jwkX jwk)

    describe "malformed input rejection" $ do
        it "rejects wrong kty" $
            expectMessage
                "wrong kty"
                (decodeJsonText (jsonWith "EC" "Ed25519" rfcX rfcD))
                "unsupported kty"

        it "rejects unsupported curve" $
            expectMessage
                "unsupported curve"
                (decodeJsonText (jsonWith "OKP" "P-256" rfcX rfcD))
                "unsupported crv"

        it "rejects bad base64url characters" $
            expectRejected
                "bad base64url"
                (T.replace "_" "+" rfcX)
                rfcD

        it "rejects padded base64url" $
            expectRejected
                "padded base64url"
                (rfcX <> "==")
                rfcD

        it "rejects truncated d" $
            expectRejected
                "truncated d"
                rfcX
                (T.dropEnd 1 rfcD)

        it "rejects short x" $
            expectRejected
                "short x"
                (T.dropEnd 1 rfcX)
                rfcD

        it "rejects empty members" $
            expectRejected "empty members" "" ""

        it "rejects non-canonical padding bits" $
            expectRejected
                "non-canonical"
                rfcX
                (T.dropEnd 1 rfcD <> "B")

        it "rejects mismatched public/private key" $ do
            kp1 <- generateKeyPair
            kp2 <- generateKeyPair
            let mixed =
                    (keyPairToJwk kp1){jwkD = jwkD (keyPairToJwk kp2)}
            case jwkToKeyPair mixed of
                Left err ->
                    err
                        `shouldBe` "public key does not match private key"
                Right _ ->
                    expectationFailure "expected rejection"

    describe "store integration" $ do
        it "imports into a fresh store and reloads" $
            withSystemTempDirectory "jwk-store" $ \dir -> do
                let db = dir ++ "/kel.db"
                kp <- either fail pure (decodeJsonText rfcJwkJson)
                store <-
                    openKELWithIdentity
                        trivialFold
                        trivialInitial
                        db
                        kp
                closeKEL store
                reloaded <- openKEL trivialFold trivialInitial db
                publicKeyBytes
                    (publicKey (serverKeyPair reloaded))
                    `shouldBe` bytesOf rfcX
                closeKEL reloaded

        it "refuses to replace an existing identity" $
            withSystemTempDirectory "jwk-clash" $ \dir -> do
                let db = dir ++ "/kel.db"
                kp <- generateKeyPair
                other <- generateKeyPair
                store1 <-
                    openKELWithIdentity
                        trivialFold
                        trivialInitial
                        db
                        kp
                closeKEL store1
                openKELWithIdentity
                    trivialFold
                    trivialInitial
                    db
                    other
                    `shouldThrow` anyIOException

-- --------------------------------------------------------
-- Properties
-- --------------------------------------------------------

roundTripProp :: Seed32 -> Property
roundTripProp (Seed32 seed) =
    case mkPairFromSeed seed of
        Left err -> counterexample err False
        Right kp ->
            case jwkToKeyPair (keyPairToJwk kp) of
                Left err -> counterexample err False
                Right kp' ->
                    publicKeyBytes (publicKey kp')
                        === publicKeyBytes (publicKey kp)
                        .&&. secretKeyBytes (secretKey kp')
                            === secretKeyBytes (secretKey kp)

signatureProp :: Seed32 -> Property
signatureProp (Seed32 seed) =
    case mkPairFromSeed seed of
        Left err -> counterexample err False
        Right kp ->
            case jwkToKeyPair (keyPairToJwk kp) of
                Left err -> counterexample err False
                Right kp' ->
                    verify (publicKey kp') rfcSigningInput (sign kp rfcSigningInput)
                        === True
