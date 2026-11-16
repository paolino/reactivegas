-- | REST client for kelgroups server.
module KelGroups.Client.Api
  ( postEvent
  , getEvents
  , getInfo
  ) where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode
  ( decodeJson
  , printJsonDecodeError
  , (.:)
  )
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, throwError)
import Effect.Exception (error)
import FFI.Fetch as Fetch
import KelGroups.Client.Codec (decodeInfoResponse)

-- | Post a submission JSON to the server. Returns sequence number.
postEvent :: String -> Json -> Aff Int
postEvent baseUrl body = do
  res <- Fetch.fetch (baseUrl <> "/events")
    { method: "POST"
    , body: stringify body
    }
  when (res.status /= 200) do
    throwError $ error $
      "POST /events failed: " <> show res.status
        <> " "
        <> res.body
  case parseResult res.body of
    Left err -> throwError $ error err
    Right sn -> pure sn
  where
  parseResult s = do
    json <- lmap show (jsonParser s)
    lmap printJsonDecodeError do
      obj <- decodeJson json
      obj .: "sequenceNumber"

-- | Get event at position after+1. Returns Nothing at end.
getEvents
  :: String
  -> String
  -> Int
  -> Aff (Maybe { signer :: String, event :: Json })
getEvents baseUrl key after = do
  res <- Fetch.fetch
    ( baseUrl <> "/events?after=" <> show after
        <> "&key="
        <> key
    )
    { method: "GET", body: "" }
  case res.status of
    200 -> case parseEvent res.body of
      Left err -> throwError $ error err
      Right r -> pure (Just r)
    404 -> pure Nothing
    _ -> throwError $ error $
      "GET /events failed: " <> show res.status
  where
  parseEvent s = do
    json <- lmap show (jsonParser s)
    lmap printJsonDecodeError do
      obj <- decodeJson json
      signer <- obj .: "signer"
      evt <- obj .: "event"
      pure { signer, event: evt }

-- | Get public info (admin emails, pending introduction).
getInfo
  :: String
  -> String
  -> Aff
       { publicAdminEmails :: Array String
       , pendingIntroduction :: Boolean
       }
getInfo baseUrl key = do
  res <- Fetch.fetch
    (baseUrl <> "/info?key=" <> key)
    { method: "GET", body: "" }
  when (res.status /= 200) do
    throwError $ error $
      "GET /info failed: " <> show res.status
  case parseInfo res.body of
    Left err -> throwError $ error err
    Right r -> pure r
  where
  parseInfo s = do
    json <- lmap show (jsonParser s)
    lmap printJsonDecodeError (decodeInfoResponse json)
