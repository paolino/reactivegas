{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Blake3Spec (
    spec,
) where

import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (for_)
import Data.Text.Encoding qualified as TE
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath (takeDirectory, (</>))
import Test.Hspec (Spec, describe, it, shouldBe)

import Hex (hexDecode)
import Reactivegas.Core.Blake3 (
    deriveKey256,
    extendedHash,
    hash256,
    keyedHash256,
 )

data VectorCase = VectorCase
    { vcInputLen :: Int
    , vcHash :: ByteString
    , vcKeyed :: ByteString
    , vcDeriveKey :: ByteString
    }

instance Aeson.FromJSON VectorCase where
    parseJSON = Aeson.withObject "VectorCase" $ \o ->
        VectorCase
            <$> o .: "input_len"
            <*> hexField o "hash"
            <*> hexField o "keyed_hash"
            <*> hexField o "derive_key"
      where
        hexField obj key = do
            txt <- obj .: key
            either fail pure (hexDecode (TE.encodeUtf8 txt))

data VectorFile = VectorFile
    { vfKey :: ByteString
    , vfContext :: ByteString
    , vfCases :: [VectorCase]
    }

instance Aeson.FromJSON VectorFile where
    parseJSON = Aeson.withObject "VectorFile" $ \o ->
        VectorFile
            <$> (TE.encodeUtf8 <$> o .: "key")
            <*> (TE.encodeUtf8 <$> o .: "context_string")
            <*> o .: "cases"

-- | Official vector inputs are the byte sequence @i \`mod\` 251@.
vectorInput :: Int -> ByteString
vectorInput n = BS.pack (map (\i -> fromIntegral (i `mod` 251)) [0 .. n - 1])

loadVectors :: IO VectorFile
loadVectors = do
    path <- locateVectorFile
    raw <- BSL.readFile path
    either fail pure (Aeson.eitherDecode raw)

{- | The test working directory depends on how cabal runs the suite,
so look for the shared corpus walking up from here.
-}
locateVectorFile :: IO FilePath
locateVectorFile = getCurrentDirectory >>= go
  where
    go dir = do
        let candidate = dir </> "vectors" </> "blake3.json"
        found <- doesFileExist candidate
        if found
            then pure candidate
            else case takeDirectory dir of
                parent
                    | parent == dir ->
                        fail "vectors/blake3.json not found above the test directory"
                parent -> go parent

spec :: Spec
spec = describe "Reactivegas.Core.Blake3" $ do
    it "matches every official vector case in all modes" $ do
        vectors <- loadVectors
        for_ (vfCases vectors) $ \c -> do
            let input = vectorInput (vcInputLen c)
                check what got want
                    | got == want = pure ()
                    | otherwise =
                        fail
                            ( "vector mismatch (input_len "
                                ++ show (vcInputLen c)
                                ++ "): "
                                ++ what
                            )
            -- The corpus ships 131-byte extended digests for
            -- every mode; we assert on their 32-byte prefixes.
            check "hash" (hash256 input) (BS.take 32 (vcHash c))
            check
                "extended"
                (extendedHash 131 input)
                (vcHash c)
            check
                "keyed"
                (keyedHash256 (vfKey vectors) input)
                (BS.take 32 (vcKeyed c))
            check
                "derive_key"
                (deriveKey256 (vfContext vectors) input)
                (BS.take 32 (vcDeriveKey c))

    it "produces prefix-consistent extended outputs" $ do
        vectors <- loadVectors
        let biggest = maximum (map vcInputLen (vfCases vectors))
        for_ [8, 17, 33, 64, 65] $ \short ->
            BS.take short (extendedHash 256 (vectorInput biggest))
                `shouldBe` extendedHash short (vectorInput biggest)
