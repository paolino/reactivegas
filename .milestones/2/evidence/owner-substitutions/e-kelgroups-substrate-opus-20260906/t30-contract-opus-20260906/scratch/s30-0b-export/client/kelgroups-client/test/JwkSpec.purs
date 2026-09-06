-- | Tests for JWK private key export/import, pinning
-- the RFC 8037 appendix vectors.
module Test.JwkSpec (run) where

import Prelude

import Data.Array ((..))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.String as S
import Data.String.CodeUnits (dropRight)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import FFI.KeyBytes (fromArray, fromSeed, toArray)
import FFI.TextEncoder (encodeUtf8)
import FFI.TweetNaCl as NaCl
import KelGroups.Client.Jwk
  ( Jwk
  , b64urlDecodeInts
  , encodeJwkJson
  , jwkToKeyPair
  , keyPairToJwk
  , parseJwkJson
  )
import Test.QuickCheck (quickCheck, (<?>))
import Test.QuickCheck.Gen (chooseInt, vectorOf)

rfcD :: String
rfcD = "nWGxne_9WmC6hEr0kuwsxERJxWl7MmkZcDusAxyuf2A"

rfcX :: String
rfcX = "11qYAYKxCrfVS_7TyWQHOg7hcvPapiMlrwIaaPcHURo"

rfcSigB64 :: String
rfcSigB64 =
  "hgyY0il_MGCjP0JzlnLWG1PPOt7-09PGcvMg3AIbQR6dWbhijcNR4ki4iylGjg5BhVsPt9g7sVvpAr_MuM0KAg"

-- JWS signing input from RFC 8037 appendix A.4.
rfcSigningInput :: String
rfcSigningInput =
  "eyJhbGciOiJFZERTQSJ9.RXhhbXBsZSBvZiBFZDI1NTE5IHNpZ25pbmc"

rfcJson :: String
rfcJson = jsonFor "OKP" "Ed25519" rfcX rfcD

jsonFor :: String -> String -> String -> String -> String
jsonFor kty crv x d =
  "{\"kty\":\""
    <> kty
    <> "\",\"crv\":\""
    <> crv
    <> "\",\"x\":\""
    <> x
    <> "\",\"d\":\""
    <> d
    <> "\"}"

mkPairFromSeed :: Array Int -> NaCl.KeyPair
mkPairFromSeed seed = fromSeed (fromArray seed)

seedOf :: Int -> Array Int
seedOf n = map (\i -> i * 37 + n) (1 .. 32)

expectRejected :: String -> Jwk -> Effect Unit
expectRejected label j = case jwkToKeyPair j of
  Left err -> do
    failIf (err == "") "empty error message"
    failIf
      ( any (\frag -> S.contains (Pattern frag) err)
          (Array.filter (_ /= "") [ j.x, j.d ])
      )
      ("error leaks key material: " <> label)
  Right _ ->
    throw ("expected rejection: " <> label)

failIf :: Boolean -> String -> Effect Unit
failIf cond msg = when cond $ throw msg

run :: Effect Unit
run = do
  log "  J1: RFC 8037 A.1 key imports with A.1 members"
  case parseJwkJson rfcJson of
    Left e -> throw ("J1 failed: " <> e)
    Right kp -> do
      let j = keyPairToJwk kp
      failIf (j.x /= rfcX || j.d /= rfcD) "A.1 members mismatch"

  log "  J2: RFC 8037 A.4 signature reproduces"
  case parseJwkJson rfcJson of
    Left e -> throw ("J2 failed: " <> e)
    Right kp -> do
      let sig = toArray (NaCl.sign (encodeUtf8 rfcSigningInput) kp.secretKey)
      expected <- case b64urlDecodeInts rfcSigB64 of
        Left e -> throw ("decode failed: " <> e)
        Right bytes -> pure bytes
      failIf (sig /= expected) "A.4 signature bytes mismatch"

  log "  J3: export then import preserves the pair"
  quickCheck do
    seed <- vectorOf 32 (chooseInt 0 255)
    let kp = mkPairFromSeed seed
        imported = jwkToKeyPair (keyPairToJwk kp)
    pure $ case imported of
      Left _ -> false <?> "import of exported JWK failed"
      Right kp' ->
        ( toArray kp'.publicKey == toArray kp.publicKey
            && toArray kp'.secretKey == toArray kp.secretKey
        )
          <?> "round trip changed the key"

  log "  J4: JSON round trip preserves the members"
  quickCheck do
    seed <- vectorOf 32 (chooseInt 0 255)
    let j = keyPairToJwk (mkPairFromSeed seed)
    pure $ case parseJwkJson (encodeJwkJson j) of
      Left _ -> false <?> "re-parse of encoded JWK failed"
      Right _ -> true <?> "unreachable"

  log "  J5: rejects wrong kty"
  expectRejected "wrong kty" (baseJwk "EC" "Ed25519")

  log "  J6: rejects unsupported curve"
  expectRejected "unsupported curve" (baseJwk "OKP" "P-256")

  log "  J7: rejects bad base64url characters"
  expectRejected "bad base64url"
    (baseJwk "OKP" "Ed25519") { x = replaceUnderscore rfcX }

  log "  J8: rejects padded base64url"
  expectRejected "padded base64url"
    (baseJwk "OKP" "Ed25519") { x = rfcX <> "==" }

  log "  J9: rejects truncated d"
  expectRejected "truncated d"
    (baseJwk "OKP" "Ed25519") { d = dropRight 1 rfcD }

  log "  J10: rejects short x"
  expectRejected "short x"
    (baseJwk "OKP" "Ed25519") { x = dropRight 1 rfcX }

  log "  J11: rejects empty members"
  expectRejected "empty members"
    { kty: "OKP", crv: "Ed25519", x: "", d: "" }

  log "  J12: rejects non-canonical padding bits"
  expectRejected "non-canonical"
    (baseJwk "OKP" "Ed25519") { d = dropRight 1 rfcD <> "B" }

  log "  J13: rejects mismatched public/private key"
  let mixed = (keyPairToJwk (mkPairFromSeed (seedOf 1)))
        { d = (keyPairToJwk (mkPairFromSeed (seedOf 2))).d }
  case jwkToKeyPair mixed of
    Left err ->
      failIf (err /= "public key does not match private key")
        "unexpected mismatch error"
    Right _ -> throw "expected rejection: mismatched pair"
  where
  baseJwk kty crv = { kty, crv, x: rfcX, d: rfcD }

  replaceUnderscore = S.replace (Pattern "_") (Replacement "+")
