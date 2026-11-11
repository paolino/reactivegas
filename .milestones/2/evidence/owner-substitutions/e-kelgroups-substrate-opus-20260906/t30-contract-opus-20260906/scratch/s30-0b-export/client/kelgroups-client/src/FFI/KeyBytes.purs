module FFI.KeyBytes
  ( toArray
  , fromArray
  , fromSeed
  ) where

import Data.ArrayBuffer.Types (Uint8Array)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import FFI.TweetNaCl (KeyPair)

foreign import toArrayImpl :: Uint8Array -> Array Int

foreign import fromArrayImpl :: Array Int -> Uint8Array

foreign import fromSeedImpl :: Uint8Array -> Effect KeyPair

toArray :: Uint8Array -> Array Int
toArray = toArrayImpl

fromArray :: Array Int -> Uint8Array
fromArray = fromArrayImpl

-- tweetnacl's keyPair.fromSeed is a deterministic,
-- side-effect-free computation; the Effect wrapper is
-- an artifact of the JS API.
fromSeed :: Uint8Array -> KeyPair
fromSeed seed = unsafePerformEffect (fromSeedImpl seed)
