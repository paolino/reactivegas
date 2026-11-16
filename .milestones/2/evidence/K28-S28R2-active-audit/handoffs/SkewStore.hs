{-# LANGUAGE GHC2021, OverloadedStrings #-}
module SkewStore (skewAppend) where
import KelGroups.Store
import KelGroups.Fold
import KelGroups.Event (IntegratedEvent)
import Control.Concurrent.MVar (withMVar)
import Control.Concurrent.STM (atomically, writeTVar)
import Control.Exception (evaluate)
import Data.Aeson (ToJSON, encode)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.ByteString.Lazy qualified as LBS
import Database.SQLite.Simple (execute)
skewAppend
    :: (ToJSON e, ToJSON bp)
    => KELStore s
    -> Integration s e bp err
    -> Text
    -> IntegratedEvent bp e
    -> IO (Either (IntegratedError err) (IntegratedResult s))
skewAppend store integration signer event = do
    gs <- readState store
    n <- kelLength store
    case applyIntegratedEvent integration gs signer event of
        Left err -> pure (Left err)
        Right result -> withMVar (storeAppendLock store) $ \() -> do
                let payloadJson = encode event
                    payloadText = TE.decodeUtf8 (LBS.toStrict payloadJson)
                    noEnvelope = T.empty
                _ <- evaluate payloadText
                execute
                    (storeConn store)
                    "INSERT INTO events \
                    \(signer, event_bytes, signature, \
                    \group_event, prefix, seq_no, digest) \
                    \VALUES (?, ?, ?, ?, ?, ?, ?)"
                    ( signer
                    , payloadText
                    , noEnvelope
                    , payloadJson
                    , noEnvelope
                    , n + 1
                    , noEnvelope
                    )
                atomically $ do
                    writeTVar (stateVar store) (irState result)
                    writeTVar (lengthVar store) (n + 1)
                pure (Right result)
