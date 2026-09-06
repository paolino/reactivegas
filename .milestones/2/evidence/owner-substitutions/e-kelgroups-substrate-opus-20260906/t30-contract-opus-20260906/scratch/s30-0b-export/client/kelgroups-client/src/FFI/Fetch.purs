module FFI.Fetch
  ( FetchOptions
  , FetchResponse
  , fetch
  ) where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Exception (Error)

type FetchOptions =
  { method :: String
  , body :: String
  }

type FetchResponse =
  { status :: Int
  , body :: String
  }

foreign import fetchImpl
  :: String
  -> FetchOptions
  -> (Error -> Effect Unit)
  -> (FetchResponse -> Effect Unit)
  -> Effect (Effect Unit)

fetch :: String -> FetchOptions -> Aff FetchResponse
fetch url opts = makeAff \cb -> do
  _ <- fetchImpl url opts
    (cb <<< Left)
    (cb <<< Right)
  pure nonCanceler
