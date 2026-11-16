{- |
Module      : ServerSpec
Description : HTTP-level tests for the kelgroups server
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Tests the WAI application through real HTTP using
warp's testWithApplication and http-client.
-}
module ServerSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, race)
import Control.Concurrent.STM
    ( atomically
    , newBroadcastTChanIO
    , newTChanIO
    , readTChan
    , writeTChan
    )
import Data.Aeson
    ( FromJSON (..)
    , decode
    , encode
    , withObject
    , (.:)
    )
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import KelGroups.Event
    ( BaseEvent (..)
    , GroupEvent (..)
    , Proposal (..)
    )
import KelGroups.Server (ServerEnv (..), mkApp, mkKeriEvent)
import KelGroups.Server.JSON
    ( AppendResult (..)
    , Submission (..)
    )
import KelGroups.Store
    ( ChainTip (..)
    , chainTip
    , closeKEL
    , openKEL
    )
import KelGroups.Trivial
    ( trivialConfig
    , trivialFold
    , trivialInitial
    )
import KelGroups.Types (Admin (..), Role (..))
import Keri.Cesr.DerivationCode (DerivationCode (..))
import Keri.Cesr.Encode qualified as Cesr
import Keri.Cesr.Primitive (Primitive (..))
import Keri.Crypto.Ed25519 qualified as Ed25519
import Keri.Event
    ( Event
    , eventDigest
    , eventPrefix
    , eventSequenceNumber
    )
import Keri.Event.Serialize (serializeEvent)
import Network.HTTP.Client qualified as HC
import Network.HTTP.Types
    ( status200
    , status401
    , status403
    , status404
    , status422
    )
import Network.Wai.Handler.Warp qualified as Warp
import System.Directory (removeFile)
import System.IO.Temp (emptySystemTempFile)
import Test.Hspec
    ( Spec
    , around
    , describe
    , it
    , shouldBe
    )

-- --------------------------------------------------------
-- Helpers
-- --------------------------------------------------------

-- | Passphrase used in all tests.
testPass :: Text
testPass = "test-bootstrap-pass"

-- | A test identity for ServerSpec.
data STestId = STestId
    { stKeyPair :: Ed25519.KeyPair
    , stKey :: Text
    }

-- | Generate a fresh test identity.
newSTestId :: IO STestId
newSTestId = do
    kp <- Ed25519.generateKeyPair
    let cesrKey =
            Cesr.encode
                Primitive
                    { code = Ed25519PubKey
                    , raw =
                        Ed25519.publicKeyBytes
                            (Ed25519.publicKey kp)
                    }
    pure STestId{stKeyPair = kp, stKey = cesrKey}

{- | Sign a KERI event with a test identity.
Returns the CESR-encoded Ed25519 signature.
-}
signKeri :: STestId -> Event -> Text
signKeri tid keriEvt =
    let msg = serializeEvent keriEvt
        sigBytes = Ed25519.sign (stKeyPair tid) msg
    in  Cesr.encode
            Primitive
                { code = Ed25519Sig
                , raw = sigBytes
                }

-- | Test context: port + tip tracking.
data TestCtx = TestCtx
    { tcPort :: Warp.Port
    , tcTip :: IORef (Maybe ChainTip)
    }

-- | Set up a test server on a random port.
withTestApp :: (TestCtx -> IO a) -> IO a
withTestApp action = do
    dbPath <- emptySystemTempFile "kelgroups-test-.db"
    store <- openKEL trivialFold trivialInitial dbPath
    ch <- newBroadcastTChanIO
    let env =
            ServerEnv
                { envStore = store
                , envConfig = trivialConfig
                , envAppFold = trivialFold
                , envPassphrase = testPass
                , envBroadcast = ch
                }
    tip0 <- chainTip store
    tipRef <- newIORef tip0
    result <-
        Warp.testWithApplication
            (pure $ mkApp env Nothing)
            ( \port ->
                action TestCtx{tcPort = port, tcTip = tipRef}
            )
    closeKEL store
    removeFile dbPath
    pure result

-- | Make a GET request to the test server.
httpGet
    :: HC.Manager
    -> TestCtx
    -> String
    -> IO (HC.Response LBS.ByteString)
httpGet mgr ctx path = do
    req <-
        HC.parseRequest $
            "http://127.0.0.1:"
                <> show (tcPort ctx)
                <> path
    HC.httpLbs req mgr

-- | Make a POST request with JSON body.
httpPost
    :: HC.Manager
    -> TestCtx
    -> String
    -> LBS.ByteString
    -> IO (HC.Response LBS.ByteString)
httpPost mgr ctx path body = do
    initReq <-
        HC.parseRequest $
            "http://127.0.0.1:"
                <> show (tcPort ctx)
                <> path
    let req =
            initReq
                { HC.method = "POST"
                , HC.requestBody = HC.RequestBodyLBS body
                , HC.requestHeaders =
                    [("Content-Type", "application/json")]
                }
    HC.httpLbs req mgr

-- | Decode or fail the test.
decodeOrFail :: (FromJSON a) => LBS.ByteString -> IO a
decodeOrFail bs = case decode bs of
    Just x -> pure x
    Nothing -> error "JSON decode failed"

{- | Build a signed bootstrap submission. The signer
introduces themselves as admin.
-}
mkBootstrapSub
    :: TestCtx -> IO (Submission (), STestId)
mkBootstrapSub ctx = do
    admin1 <- newSTestId
    let evt :: GroupEvent ()
        evt =
            Base $
                Propose $
                    IntroduceMember
                        (stKey admin1)
                        (stKey admin1 <> "@test.example")
                        ( Set.singleton
                            (AdminRole PublicAdmin)
                        )
    tip <- readIORef (tcTip ctx)
    let keriEvt =
            mkKeriEvent tip (stKey admin1) evt
        sig = signKeri admin1 keriEvt
    pure
        ( Submission
            { subPassphrase = Just testPass
            , subSigner = stKey admin1
            , subSignature = sig
            , subPriorDigest = fmap tipDigest tip
            , subEvent = evt
            }
        , admin1
        )

{- | Post a bootstrap submission and update the
client-side chain tip.
-}
postBootstrap
    :: HC.Manager
    -> TestCtx
    -> IO (Submission (), STestId)
postBootstrap mgr ctx = do
    (sub, admin1) <- mkBootstrapSub ctx
    resp <-
        httpPost mgr ctx "/events" (encode sub)
    HC.responseStatus resp `shouldBe` status200
    updateTip ctx (stKey admin1) (subEvent sub)
    pure (sub, admin1)

{- | Build and post a signed submission for a group
event in normal mode.
-}
postNormalSub
    :: HC.Manager
    -> TestCtx
    -> STestId
    -> GroupEvent ()
    -> IO (HC.Response LBS.ByteString)
postNormalSub mgr ctx tid evt = do
    tip <- readIORef (tcTip ctx)
    let keriEvt = mkKeriEvent tip (stKey tid) evt
        sig = signKeri tid keriEvt
        sub =
            Submission
                { subPassphrase = Nothing
                , subSigner = stKey tid
                , subSignature = sig
                , subPriorDigest = fmap tipDigest tip
                , subEvent = evt
                }
    httpPost mgr ctx "/events" (encode sub)

-- | Update client-side chain tip after a successful append.
updateTip :: TestCtx -> Text -> GroupEvent () -> IO ()
updateTip ctx signer evt = do
    tip <- readIORef (tcTip ctx)
    let keriEvt = mkKeriEvent tip signer evt
        newTip =
            ChainTip
                { tipPrefix = eventPrefix keriEvt
                , tipSeqNo =
                    eventSequenceNumber keriEvt
                , tipDigest = eventDigest keriEvt
                }
    writeIORef (tcTip ctx) (Just newTip)

-- | Helper to extract a text field from JSON response.
data ConditionResp = ConditionResp
    { condAuthMode :: Text
    }

instance FromJSON ConditionResp where
    parseJSON = withObject "ConditionResp" $ \o ->
        ConditionResp <$> o .: "authMode"

-- | Helper to extract signer from event response.
data EventResp = EventResp
    { evtSigner :: Text
    }

instance FromJSON EventResp where
    parseJSON = withObject "EventResp" $ \o ->
        EventResp <$> o .: "signer"

-- --------------------------------------------------------
-- Specs
-- --------------------------------------------------------

spec :: Spec
spec = describe "KelGroups.Server (HTTP)" $ do
    around withTestApp $ do
        describe "GET /condition" $ do
            it "empty KEL: non-member gets 403" $
                \ctx -> do
                    mgr <-
                        HC.newManager
                            HC.defaultManagerSettings
                    resp <-
                        httpGet
                            mgr
                            ctx
                            "/condition?key=anyone"
                    HC.responseStatus resp
                        `shouldBe` status403

            it "empty KEL: missing key gets 401" $
                \ctx -> do
                    mgr <-
                        HC.newManager
                            HC.defaultManagerSettings
                    resp <-
                        httpGet mgr ctx "/condition"
                    HC.responseStatus resp
                        `shouldBe` status401

        describe "POST /events" $ do
            it
                "bootstrap with correct passphrase succeeds"
                $ \ctx -> do
                    mgr <-
                        HC.newManager
                            HC.defaultManagerSettings
                    (sub, _admin1) <-
                        mkBootstrapSub ctx
                    resp <-
                        httpPost
                            mgr
                            ctx
                            "/events"
                            (encode sub)
                    HC.responseStatus resp
                        `shouldBe` status200
                    ar <-
                        decodeOrFail (HC.responseBody resp)
                    -- Event 2 (server inception is 1)
                    sequenceNumber ar `shouldBe` 2

            it "wrong passphrase returns 401" $
                \ctx -> do
                    mgr <-
                        HC.newManager
                            HC.defaultManagerSettings
                    (sub, _) <- mkBootstrapSub ctx
                    let sub' =
                            sub
                                { subPassphrase =
                                    Just "wrong"
                                }
                    resp <-
                        httpPost
                            mgr
                            ctx
                            "/events"
                            (encode sub')
                    HC.responseStatus resp
                        `shouldBe` status401

            it
                "missing passphrase in bootstrap returns 401"
                $ \ctx -> do
                    mgr <-
                        HC.newManager
                            HC.defaultManagerSettings
                    (sub, _) <- mkBootstrapSub ctx
                    let sub' =
                            sub
                                { subPassphrase = Nothing
                                }
                    resp <-
                        httpPost
                            mgr
                            ctx
                            "/events"
                            (encode sub')
                    HC.responseStatus resp
                        `shouldBe` status401

        describe "GET /events" $ do
            it "after submit returns event" $
                \ctx -> do
                    mgr <-
                        HC.newManager
                            HC.defaultManagerSettings
                    (_, admin1) <-
                        postBootstrap mgr ctx
                    resp <-
                        httpGet
                            mgr
                            ctx
                            ( "/events?after=-1&key="
                                <> T.unpack (stKey admin1)
                            )
                    HC.responseStatus resp
                        `shouldBe` status200

            it "non-member gets 403" $
                \ctx -> do
                    mgr <-
                        HC.newManager
                            HC.defaultManagerSettings
                    _ <- postBootstrap mgr ctx
                    resp <-
                        httpGet
                            mgr
                            ctx
                            "/events?after=-1&key=nobody"
                    HC.responseStatus resp
                        `shouldBe` status403

        describe "POST + GET roundtrip" $ do
            it "submitted event matches retrieved" $
                \ctx -> do
                    mgr <-
                        HC.newManager
                            HC.defaultManagerSettings
                    (sub, admin1) <-
                        postBootstrap mgr ctx
                    -- after=1 skips server inception (id=1)
                    resp <-
                        httpGet
                            mgr
                            ctx
                            ( "/events?after=1&key="
                                <> T.unpack (stKey admin1)
                            )
                    HC.responseStatus resp
                        `shouldBe` status200
                    er <-
                        decodeOrFail (HC.responseBody resp)
                    evtSigner er
                        `shouldBe` subSigner sub

        describe "GET /condition reflects changes" $ do
            it "after bootstrap, mode is normal" $
                \ctx -> do
                    mgr <-
                        HC.newManager
                            HC.defaultManagerSettings
                    (_, admin1) <-
                        postBootstrap mgr ctx
                    resp <-
                        httpGet
                            mgr
                            ctx
                            ( "/condition?key="
                                <> T.unpack (stKey admin1)
                            )
                    HC.responseStatus resp
                        `shouldBe` status200
                    cr <-
                        decodeOrFail (HC.responseBody resp)
                    condAuthMode cr `shouldBe` "normal"

        describe "validation errors" $ do
            it "invalid event in normal mode returns 422" $
                \ctx -> do
                    mgr <-
                        HC.newManager
                            HC.defaultManagerSettings
                    _ <- postBootstrap mgr ctx
                    -- Now try invalid: non-member proposing
                    nobody <- newSTestId
                    k2 <- newSTestId
                    let badEvt :: GroupEvent ()
                        badEvt =
                            Base $
                                Propose $
                                    IntroduceMember
                                        (stKey k2)
                                        ( stKey k2
                                            <> "@test.example"
                                        )
                                        ( Set.singleton
                                            ( AdminRole
                                                PublicAdmin
                                            )
                                        )
                    resp <-
                        postNormalSub
                            mgr
                            ctx
                            nobody
                            badEvt
                    HC.responseStatus resp
                        `shouldBe` status422

        describe "unknown route" $ do
            it "returns 404" $
                \ctx -> do
                    mgr <-
                        HC.newManager
                            HC.defaultManagerSettings
                    resp <-
                        httpGet mgr ctx "/nonexistent"
                    HC.responseStatus resp
                        `shouldBe` status404

        describe "GET /info" $ do
            it "returns public admin emails" $
                \ctx -> do
                    mgr <-
                        HC.newManager
                            HC.defaultManagerSettings
                    _ <- postBootstrap mgr ctx
                    resp <-
                        httpGet
                            mgr
                            ctx
                            "/info?key=nobody"
                    HC.responseStatus resp
                        `shouldBe` status200

        describe "SSE /stream" $ do
            it "receives notification after POST" $
                \ctx -> do
                    mgr <-
                        HC.newManager
                            HC.defaultManagerSettings
                    (_, admin1) <-
                        postBootstrap mgr ctx
                    resultChan <- newTChanIO
                    listener <- async $ do
                        initReq <-
                            HC.parseRequest $
                                "http://127.0.0.1:"
                                    <> show (tcPort ctx)
                                    <> "/stream?key="
                                    <> T.unpack
                                        (stKey admin1)
                        HC.withResponse initReq mgr $
                            \resp -> do
                                chunk <-
                                    HC.responseBody resp
                                atomically $
                                    writeTChan
                                        resultChan
                                        chunk
                    threadDelay 50000
                    -- POST another event as admin1
                    u1 <- newSTestId
                    let sub2Evt :: GroupEvent ()
                        sub2Evt =
                            Base $
                                Propose $
                                    IntroduceMember
                                        (stKey u1)
                                        ( stKey u1
                                            <> "@test.example"
                                        )
                                        Set.empty
                    resp <-
                        postNormalSub
                            mgr
                            ctx
                            admin1
                            sub2Evt
                    HC.responseStatus resp
                        `shouldBe` status200
                    result <-
                        race
                            (threadDelay 2000000)
                            ( atomically $
                                readTChan resultChan
                            )
                    cancel listener
                    case result of
                        Right bs ->
                            BS.isInfixOf
                                "\"sn\":"
                                bs
                                `shouldBe` True
                        Left () ->
                            error "SSE timeout"
