{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module EnvelopeVectorSpec (
    spec,
) where

import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Word (Word64)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath (takeDirectory, (</>))
import Test.Hspec (
    Spec,
    describe,
    expectationFailure,
    it,
    shouldBe,
 )

import Hex (hexDecode)
import Reactivegas.Core.Blake3 (hash256)
import Reactivegas.Core.Ed25519
import Reactivegas.Core.Envelope
import Reactivegas.Core.Verify

data VectorCase = VectorCase
    { vcSeed :: ByteString
    , vcGroup :: ByteString
    , vcAuthor :: ByteString
    , vcPublicKey :: ByteString
    , vcLamport :: Word64
    , vcParents :: [ByteString]
    , vcTs :: Word64
    , vcKind :: EventKind
    , vcBody :: ByteString
    , vcId :: ByteString
    , vcEncoded :: ByteString
    , vcSig :: ByteString
    }

instance Aeson.FromJSON VectorCase where
    parseJSON = Aeson.withObject "VectorCase" $ \o ->
        VectorCase
            <$> hexF o "seed"
            <*> hexF o "group"
            <*> hexF o "author"
            <*> hexF o "public_key"
            <*> o .: "lamport"
            <*> parentsF o
            <*> o .: "ts"
            <*> kindF o
            <*> hexF o "body"
            <*> hexF o "id"
            <*> hexF o "encoded"
            <*> hexF o "sig"
      where
        hexF obj key = do
            txt <- obj .: key
            either fail pure (hexDecode (TE.encodeUtf8 txt))
        parentsF obj = do
            txts <- obj .: "parents"
            mapM
                (either fail pure . hexDecode . TE.encodeUtf8)
                (txts :: [Text])
        kindF obj = do
            name <- obj .: "kind"
            case lookupKind (name :: String) of
                Just k -> pure k
                Nothing -> fail ("unknown kind: " ++ name)

lookupKind :: String -> Maybe EventKind
lookupKind name =
    lookup name [(show k, k) | k <- [minBound .. maxBound]]

loadCases :: IO [VectorCase]
loadCases = do
    path <- locateVectorFile
    raw <- BSL.readFile path
    doc <- either fail pure (Aeson.eitherDecode raw :: Either String VectorFile)
    pure (vfCases doc)

data VectorFile = VectorFile {vfCases :: [VectorCase]}

instance Aeson.FromJSON VectorFile where
    parseJSON = Aeson.withObject "VectorFile" $ \o ->
        VectorFile <$> o Aeson..: "cases"

locateVectorFile :: IO FilePath
locateVectorFile = getCurrentDirectory >>= go
  where
    go dir = do
        let candidate = dir </> "vectors" </> "envelope.json"
        found <- doesFileExist candidate
        if found
            then pure candidate
            else case takeDirectory dir of
                parent
                    | parent == dir ->
                        fail "vectors/envelope.json not found above the test directory"
                parent -> go parent

spec :: Spec
spec =
    describe "golden envelope vectors" $
        it "reproduces every golden envelope byte-for-byte and verifies" $ do
            cases <- loadCases
            for_ cases $ \c -> do
                let sk = either error id (newSecretKey (vcSeed c))
                    pk = derivePublicKey sk
                publicKeyBytes pk `shouldBe` vcPublicKey c
                unMemberId (memberIdOf pk) `shouldBe` vcAuthor c
                let header =
                        Header
                            { headerGroup = GroupId (vcGroup c)
                            , headerAuthor = memberIdOf pk
                            , headerLamport = vcLamport c
                            , headerParents = map EventId (vcParents c)
                            , headerTs = vcTs c
                            , headerKind = vcKind c
                            }
                    sealed = either error id (sealEnvelope sk header (vcBody c))
                encodeEnvelope sealed `shouldBe` vcEncoded c
                unEventId (envId sealed) `shouldBe` vcId c
                envSig sealed `shouldBe` vcSig c
                decodeEnvelope (vcEncoded c) `shouldBe` Right sealed
                case verifyStep (verifierFor pk) sealed of
                    Right _ -> pure ()
                    Left err ->
                        expectationFailure ("verification failed: " ++ show err)

verifierFor :: PublicKey -> Verifier
verifierFor pk =
    verifierWith (\m -> if m == memberIdOf pk then Just pk else Nothing)

-- | Member identity is BLAKE3-256 of the member's public key.
memberIdOf :: PublicKey -> MemberId
memberIdOf pk = MemberId (hash256 (publicKeyBytes pk))
