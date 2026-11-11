appendIntegratedEvent
    :: (ToJSON e, ToJSON bp)
    => KELStore s
    -> Integration s e bp err
    -> Text
    -> IntegratedEvent bp e
    -> IO (Either (IntegratedError err) (IntegratedResult s))
appendIntegratedEvent store integration signer event =
    withMVar (storeAppendLock store) $ \() -> do
        gs <- readState store
        case applyIntegratedEvent integration gs signer event of
            Left err -> pure (Left err)
            Right result -> do
                let payloadJson = encode event
                    payloadText = TE.decodeUtf8 (LBS.toStrict payloadJson)
                    noEnvelope = T.empty
                _ <- evaluate payloadText
                n <- kelLength store
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
