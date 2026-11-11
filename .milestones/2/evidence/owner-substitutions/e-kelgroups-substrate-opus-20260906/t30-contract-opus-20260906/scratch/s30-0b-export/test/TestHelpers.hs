{-# OPTIONS_GHC -Wno-x-partial #-}

{- |
Module      : TestHelpers
Description : Shared test infrastructure for E2E and multi-client specs
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Reusable test environment, HTTP helpers, submission builders,
and response decoders for integration tests. All submissions
carry real Ed25519 signatures over serialized KERI events.
-}
module TestHelpers
    ( -- * Test environment
      TestEnv (..)
    , withTestEnv

      -- * HTTP helpers
    , httpGet
    , httpPost
    , postEvent
    , getCondition
    , getInfo
    , decodeOrFail

      -- * Identities (keypair + CESR key)
    , TestId (..)
    , newTestId

      -- * Submission builders (unsigned)
    , TestSub (..)
    , testPass
    , bootstrap
    , proposeAdmin
    , proposeMember
    , proposeRemove
    , proposeChangeRoles
    , approve

      -- * Signing helpers
    , signSubmission

      -- * Response decoders
    , ConditionResp (..)
    , MemberResp (..)
    , EventResp (..)
    , InfoResp (..)
    ) where

import Control.Concurrent.STM (newBroadcastTChanIO)
import Data.Aeson
    ( FromJSON (..)
    , Value (..)
    , decode
    , encode
    , withObject
    , (.:)
    )
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
    ( IORef
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.Set qualified as Set
import Data.Text (Text)
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
    , KELStore (..)
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
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp qualified as Warp
import System.Directory (removeFile)
import System.IO.Temp (emptySystemTempFile)
import Test.Hspec (shouldBe)

-- --------------------------------------------------------
-- Test identities
-- --------------------------------------------------------

-- | A test identity: keypair + CESR-encoded public key.
data TestId = TestId
    { tidKeyPair :: Ed25519.KeyPair
    , tidKey :: Text
    -- ^ CESR-encoded Ed25519 public key
    }

-- | Generate a fresh test identity.
newTestId :: IO TestId
newTestId = do
    kp <- Ed25519.generateKeyPair
    let cesrKey =
            Cesr.encode
                Primitive
                    { code = Ed25519PubKey
                    , raw =
                        Ed25519.publicKeyBytes
                            (Ed25519.publicKey kp)
                    }
    pure TestId{tidKeyPair = kp, tidKey = cesrKey}

{- | Sign the canonical serialization of a KERI event.
Returns the CESR-encoded Ed25519 signature.
-}
signKeriBytes :: TestId -> Event -> Text
signKeriBytes tid keriEvt =
    let msg = serializeEvent keriEvt
        sigBytes = Ed25519.sign (tidKeyPair tid) msg
    in  Cesr.encode
            Primitive
                { code = Ed25519Sig
                , raw = sigBytes
                }

-- --------------------------------------------------------
-- Test environment
-- --------------------------------------------------------

-- | Passphrase used in all tests.
testPass :: Text
testPass = "e2e-bootstrap-pass"

-- | Test environment with a running server and HTTP manager.
data TestEnv = TestEnv
    { tePort :: Warp.Port
    , teMgr :: HC.Manager
    , teTip :: IORef (Maybe ChainTip)
    -- ^ Client-side chain tip for KERI event construction
    , teServerKey :: Text
    -- ^ CESR-encoded server public key
    }

-- | Spin up a fresh server on a random port for one test.
withTestEnv :: (TestEnv -> IO a) -> IO a
withTestEnv action = do
    dbPath <- emptySystemTempFile "kelgroups-e2e-.db"
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
        sKey = serverCesrKey store
    mgr <- HC.newManager HC.defaultManagerSettings
    tip <- chainTip store
    tipRef <- newIORef tip
    result <-
        Warp.testWithApplication
            (pure $ mkApp env Nothing)
            ( \port ->
                action
                    TestEnv
                        { tePort = port
                        , teMgr = mgr
                        , teTip = tipRef
                        , teServerKey = sKey
                        }
            )
    closeKEL store
    removeFile dbPath
    pure result

-- --------------------------------------------------------
-- HTTP helpers
-- --------------------------------------------------------

-- | GET request to the test server.
httpGet
    :: TestEnv -> String -> IO (HC.Response LBS.ByteString)
httpGet te path = do
    req <-
        HC.parseRequest $
            "http://127.0.0.1:" <> show (tePort te) <> path
    HC.httpLbs req (teMgr te)

-- | POST request with JSON body.
httpPost
    :: TestEnv
    -> String
    -> LBS.ByteString
    -> IO (HC.Response LBS.ByteString)
httpPost te path body = do
    initReq <-
        HC.parseRequest $
            "http://127.0.0.1:" <> show (tePort te) <> path
    let req =
            initReq
                { HC.method = "POST"
                , HC.requestBody = HC.RequestBodyLBS body
                , HC.requestHeaders =
                    [("Content-Type", "application/json")]
                }
    HC.httpLbs req (teMgr te)

{- | POST a test submission and expect 200. Constructs
the KERI event, signs it, sends, and updates the
client-side chain tip.
-}
postEvent :: TestEnv -> TestSub -> IO Int
postEvent te ts = do
    sub <- signSubmission te ts
    resp <- httpPost te "/events" (encode sub)
    HC.responseStatus resp `shouldBe` status200
    ar <- decodeOrFail (HC.responseBody resp)
    -- Update client-side chain tip
    tip <- readIORef (teTip te)
    let keriEvt =
            mkKeriEvent
                tip
                (tidKey $ tsSigner ts)
                (tsEvent ts)
        newTip =
            ChainTip
                { tipPrefix = eventPrefix keriEvt
                , tipSeqNo =
                    eventSequenceNumber keriEvt
                , tipDigest = eventDigest keriEvt
                }
    writeIORef (teTip te) (Just newTip)
    pure (sequenceNumber ar)

-- | GET /condition?key=K and decode.
getCondition :: TestEnv -> String -> IO ConditionResp
getCondition te key = do
    resp <-
        httpGet te ("/condition?key=" <> key)
    HC.responseStatus resp `shouldBe` status200
    decodeOrFail (HC.responseBody resp)

-- | GET /info?key=K and decode.
getInfo :: TestEnv -> String -> IO InfoResp
getInfo te key = do
    resp <- httpGet te ("/info?key=" <> key)
    HC.responseStatus resp `shouldBe` status200
    decodeOrFail (HC.responseBody resp)

-- | Decode JSON or fail the test.
decodeOrFail :: (FromJSON a) => LBS.ByteString -> IO a
decodeOrFail bs = case decode bs of
    Just x -> pure x
    Nothing ->
        error $
            "JSON decode failed: "
                <> show (LBS.take 200 bs)

-- --------------------------------------------------------
-- Unsigned test submissions
-- --------------------------------------------------------

{- | An unsigned test submission. Signed by 'postEvent'
or 'signSubmission' using the current chain tip.
-}
data TestSub = TestSub
    { tsSigner :: TestId
    , tsPassphrase :: Maybe Text
    , tsEvent :: GroupEvent ()
    }

{- | Sign a test submission using the current chain tip.
Constructs the KERI event and signs it.
-}
signSubmission :: TestEnv -> TestSub -> IO (Submission ())
signSubmission te ts = do
    tip <- readIORef (teTip te)
    let keriEvt =
            mkKeriEvent
                tip
                (tidKey $ tsSigner ts)
                (tsEvent ts)
        sig = signKeriBytes (tsSigner ts) keriEvt
    pure
        Submission
            { subPassphrase = tsPassphrase ts
            , subSigner = tidKey (tsSigner ts)
            , subSignature = sig
            , subPriorDigest = fmap tipDigest tip
            , subEvent = tsEvent ts
            }

-- --------------------------------------------------------
-- Submission builders
-- --------------------------------------------------------

-- | Bootstrap the first admin.
bootstrap :: TestId -> TestSub
bootstrap tid =
    let evt =
            Base $
                Propose $
                    IntroduceMember
                        (tidKey tid)
                        (tidKey tid <> "@test.example")
                        ( Set.singleton
                            (AdminRole PublicAdmin)
                        )
    in  TestSub tid (Just testPass) evt

-- | Propose a new member with PublicAdmin role.
proposeAdmin :: TestId -> TestId -> TestSub
proposeAdmin signer newMember =
    let evt =
            Base $
                Propose $
                    IntroduceMember
                        (tidKey newMember)
                        (tidKey newMember <> "@test.example")
                        ( Set.singleton
                            (AdminRole PublicAdmin)
                        )
    in  TestSub signer Nothing evt

-- | Propose a new member with no roles.
proposeMember :: TestId -> TestId -> TestSub
proposeMember signer newMember =
    let evt =
            Base $
                Propose $
                    IntroduceMember
                        (tidKey newMember)
                        (tidKey newMember <> "@test.example")
                        Set.empty
    in  TestSub signer Nothing evt

-- | Propose removing a member.
proposeRemove :: TestId -> TestId -> TestSub
proposeRemove signer target =
    let evt =
            Base $
                Propose $
                    RemoveMember (tidKey target)
    in  TestSub signer Nothing evt

-- | Propose changing a member's roles.
proposeChangeRoles
    :: TestId -> TestId -> Set.Set Role -> TestSub
proposeChangeRoles signer target roles =
    let evt =
            Base $
                Propose $
                    ChangeRoles (tidKey target) roles
    in  TestSub signer Nothing evt

-- | Approve a pending proposal.
approve :: TestId -> Text -> TestSub
approve signer proposalId =
    let evt = Base $ Approve proposalId
    in  TestSub signer Nothing evt

-- --------------------------------------------------------
-- Response decoders
-- --------------------------------------------------------

-- | Decoded /condition response.
data ConditionResp = ConditionResp
    { crAuthMode :: Text
    , crMembers :: [MemberResp]
    , crPending :: [(Text, Value)]
    }

instance FromJSON ConditionResp where
    parseJSON = withObject "ConditionResp" $ \o -> do
        mode <- o .: "authMode"
        st <- o .: "state"
        ms <- st .: "members"
        pps <- st .: "pendingProposals"
        pure
            ConditionResp
                { crAuthMode = mode
                , crMembers = ms
                , crPending = pps
                }

-- | Decoded member within a /condition response.
data MemberResp = MemberResp
    { mrKey :: Text
    , mrRoles :: [Value]
    }
    deriving stock (Show)

instance FromJSON MemberResp where
    parseJSON = withObject "MemberResp" $ \o ->
        MemberResp <$> o .: "key" <*> o .: "roles"

-- | Decoded /events response.
data EventResp = EventResp
    { erSigner :: Text
    , _erEvent :: Value
    }

instance FromJSON EventResp where
    parseJSON = withObject "EventResp" $ \o ->
        EventResp <$> o .: "signer" <*> o .: "event"

-- | Decoded /info response.
data InfoResp = InfoResp
    { irEmails :: [Text]
    , irPending :: Bool
    , irServerKey :: Text
    , irGroupId :: Text
    }

instance FromJSON InfoResp where
    parseJSON = withObject "InfoResp" $ \o ->
        InfoResp
            <$> o .: "publicAdminEmails"
            <*> o .: "pendingIntroduction"
            <*> o .: "serverKey"
            <*> o .: "groupId"
