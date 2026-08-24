{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Ed25519Spec (
    spec,
) where

import Data.Bits (xor)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Test.Hspec (Spec, describe, it, shouldBe)

import Hex (hexDecode)
import Reactivegas.Core.Ed25519

-- | Unwrapping helper for constant vector data.
hex :: ByteString -> ByteString
hex raw = either error id (hexDecode raw)

pubFromBytes :: ByteString -> PublicKey
pubFromBytes = either error id . parsePublicKey

mkSig :: ByteString -> Signature
mkSig = either error id . parseSignature

secretOf :: ByteString -> SecretKey
secretOf = either error id . newSecretKey

flipFirstByte :: ByteString -> ByteString
flipFirstByte bs = BS.cons (BS.head bs `xor` 1) (BS.tail bs)

{- | RFC 8032 section 7.1 vectors: (seed, public key, message,
signature).
-}
rfcVectors :: [(ByteString, ByteString, ByteString, ByteString)]
rfcVectors =
    [
        ( hex "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"
        , hex "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"
        , ""
        , hex "e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e06522490155"
            <> hex "5fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b"
        )
    ,
        ( hex "4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb"
        , hex "3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c"
        , BS.singleton 0x72
        , hex "92a009a9f0d4cab8720e820b5f642540a2b27b5416503f8fb3762223ebdb69da"
            <> hex "085ac1e43e15996e458f3613d0f11d8c387b2eaeb4302aeeb00d291612bb0c00"
        )
    ]

spec :: Spec
spec = describe "Reactivegas.Core.Ed25519" $ do
    it "derives the RFC 8032 public key from every test seed" $
        for_ rfcVectors $ \(seed, pub, _, _) ->
            (publicKeyBytes . derivePublicKey <$> newSecretKey seed)
                `shouldBe` Right pub

    it "reproduces the RFC 8032 signature for every test case" $
        for_ rfcVectors $ \(seed, _, msg, sig) ->
            let sk = secretOf seed
                pk = derivePublicKey sk
             in signMessage sk pk msg `shouldBe` mkSig sig

    it "verifies genuine signatures and rejects tampering" $
        for_ rfcVectors $ \(_, pub, msg, sig) ->
            let pk = pubFromBytes pub
                nonEmptyMsg = if BS.null msg then BS.singleton 0x00 else msg
                genuine = verifySignature pk msg (mkSig sig)
                tamperedMsg = verifySignature pk (flipFirstByte nonEmptyMsg) (mkSig sig)
                tamperedSig = verifySignature pk nonEmptyMsg (mkSig (flipFirstByte sig))
             in do
                    genuine `shouldBe` True
                    tamperedMsg `shouldBe` False
                    tamperedSig `shouldBe` False
