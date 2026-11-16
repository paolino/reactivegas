{- |
Module      : S28AppApiSpec
Description : S28-1 integrated app-api properties with six-group witnesses
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Six frozen groups proving the integrated boundary. QuickCheck uses
standalone generators only. Agreement traces include non-member and
domain-invalid events.
-}
module S28AppApiSpec (spec) where

import Control.Concurrent
    ( forkIO
    , killThread
    , newEmptyMVar
    , putMVar
    , takeMVar
    , threadDelay
    , tryPutMVar
    , tryReadMVar
    )
import Control.Exception (SomeException, bracket, mask, try)
import Data.Aeson (ToJSON (..), decode, decodeStrict, encode)
import Data.ByteString qualified as BS
import Data.Either (isRight)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import KelGroups.Event
    ( BaseChange (..)
    , BaseMutation (..)
    , DirectCommand (..)
    , IntegratedEvent
    )
import KelGroups.Event qualified as Evt
import KelGroups.Fold
    ( IntegratedError (..)
    , IntegratedResult (..)
    , Integration (..)
    , applyIntegratedEvent
    , commitBaseChange
    , enactMutation
    , foldIntegrated
    , foldIntegratedFrom
    , tryEnactBase
    )
import KelGroups.Server.JSON ()
import KelGroups.State
    ( GroupState (..)
    , PendingBase (..)
    , emptyState
    , groupView
    , lookupPendingBase
    )
import KelGroups.Store
    ( KELStore
    , StoredEvent (..)
    , appendIntegratedEvent
    , closeKEL
    , kelLength
    , openIntegratedKEL
    , readEventsFrom
    , readState
    )
import KelGroups.Types
    ( Admin (..)
    , Member (..)
    , Role (..)
    , isAdminInView
    , isMemberInView
    , lookupMemberInView
    )
import KelGroups.Validate
    ( ValidationError (..)
    , validateBaseApproval
    , validateBaseMutation
    , validateDirectAdmission
    )
import S28DemoApp
    ( DemoError (..)
    , DemoEvent (..)
    , DemoProposal (..)
    , DemoState (..)
    , demoBaseHook
    , demoDigest
    , demoInitialState
    , demoIntegration
    , demoProposalMutation
    , demoReserved
    , foundingDemo
    , protectedKey
    )
import System.Directory (removeFile)
import System.IO.Temp (emptySystemTempFile)
import System.Timeout (timeout)
import Test.Hspec
    ( Spec
    , describe
    , expectationFailure
    , it
    , shouldBe
    , shouldSatisfy
    )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, chooseInt, elements, forAll, listOf)

adminMember :: Text -> Member
adminMember key =
    Member
        { memberKey = key
        , memberEmail = key <> "@test.example"
        , memberRoles = Set.singleton (AdminRole PublicAdmin)
        }

plainMember :: Text -> Member
plainMember key =
    Member
        { memberKey = key
        , memberEmail = key <> "@test.example"
        , memberRoles = Set.empty
        }

gsWithAdmin :: Text -> GroupState DemoState
gsWithAdmin adminKey =
    demoInitialState
        { members = Map.singleton adminKey (adminMember adminKey)
        }

withTempIntegrated :: (FilePath -> IO a) -> IO a
withTempIntegrated action = do
    path <- emptySystemTempFile "s28-test-.db"
    result <- action path
    removeFile path
    pure result

genDemoProposal :: Gen DemoProposal
genDemoProposal = do
    key <- elements ["admin-key-1", "member-key-2", "outsider-key-9"]
    elements [DemoRemove key, DemoChangeRoles key Set.empty]

genTrace :: Gen [(Text, IntegratedEvent DemoProposal DemoEvent)]
genTrace = listOf genSigned
  where
    genSigned = do
        signer <- elements ["admin-key-1", "outsider-key-9"]
        evt <- genMixed
        pure (signer, evt)
    genMixed = do
        n <- chooseInt (-5, 10)
        elements
            [ Evt.IEApp (DemoAdd n)
            , Evt.IEApp (DemoAdd (-3))
            , Evt.IEApp DemoReset
            , Evt.IEApp DemoNoop
            , Evt.IEDirect (AdmitMember "fresh-key" "f@x" Set.empty)
            ]

applyStep
    :: GroupState DemoState
    -> (Text, IntegratedEvent DemoProposal DemoEvent)
    -> GroupState DemoState
applyStep gs (signer, evt) =
    case applyIntegratedEvent demoIntegration gs signer evt of
        Right result -> irState result
        Left _ -> gs

-- Deliberately faulting application codec: a refused caller must never
-- need it. Accepted use throws observably before any durable write.
data FaultingCodec = FaultingCodec
    deriving stock (Eq, Show)

instance ToJSON FaultingCodec where
    toJSON _ = error "S28-R2 faulting codec (refusal must short-circuit)"

faultingIntegration
    :: Integration DemoState FaultingCodec DemoProposal DemoError
faultingIntegration =
    Integration
        { intReserved = demoReserved
        , intDigest = demoDigest
        , intProposalMutation = demoProposalMutation
        , intAppFold = \_ _ _ st _ -> Right st
        , intBaseHook = demoBaseHook
        }

tryFaultingAppend
    :: KELStore DemoState
    -> Text
    -> IO
        ( Either
            SomeException
            ( Either
                (IntegratedError DemoError)
                (IntegratedResult DemoState)
            )
        )
tryFaultingAppend store signer =
    try
        ( appendIntegratedEvent
            store
            faultingIntegration
            signer
            (Evt.IEApp FaultingCodec)
        )

spec :: Spec
spec = do
    describe "S28-1 distinct types + signer + GroupView" $ do
        it "member add authorizes through the sole view" $ do
            let gs = gsWithAdmin "admin-key-1"
            let view = groupView gs
            lookupMemberInView "admin-key-1" view `shouldSatisfy` (/= Nothing)
            isMemberInView "admin-key-1" view `shouldBe` True
            isAdminInView "admin-key-1" view `shouldBe` True
            case applyIntegratedEvent
                demoIntegration
                gs
                "admin-key-1"
                (Evt.IEApp (DemoAdd 3)) of
                Right result -> demoCounter (appFold (irState result)) `shouldBe` 3
                Left err -> expectationFailure ("expected DemoAdd ok: " <> show err)
        it "nonmember app refused before any fold" $ do
            let gs = gsWithAdmin "admin-key-1"
            case applyIntegratedEvent
                demoIntegration
                gs
                "outsider-key-9"
                (Evt.IEApp (DemoAdd 1)) of
                Left (IEValidation (NotAMember _)) -> pure ()
                other -> expectationFailure ("expected NotAMember: " <> show other)
        prop "nonadmin reset never advances the counter" $ do
            forAll (elements ["member-key-2", "outsider-key-9"]) $ \outsider ->
                let gs =
                        (gsWithAdmin "admin-key-1")
                            { members =
                                Map.insert
                                    "member-key-2"
                                    (plainMember "member-key-2")
                                    (members (gsWithAdmin "admin-key-1"))
                            }
                in  case applyIntegratedEvent demoIntegration gs outsider (Evt.IEApp DemoReset) of
                        Left (IEValidation (NotAMember _)) -> True
                        Left (IEApp (DemoNotAdmin _)) -> True
                        _ -> False
    describe "S28-1 rejecting step before append" $ do
        it "accepted events persist with readable rows" $ do
            store <- openIntegratedKEL demoIntegration foundingDemo ":memory:"
            n0 <- kelLength store
            result <-
                appendIntegratedEvent
                    store
                    demoIntegration
                    "admin-key-1"
                    (Evt.IEApp (DemoAdd 2))
            case result of
                Right _ -> pure ()
                Left err -> expectationFailure ("expected append ok: " <> show err)
            n1 <- kelLength store
            (n1 == n0 + 1) `shouldBe` True
            live <- readState store
            demoCounter (appFold live) `shouldBe` 2
            rows <- readEventsFrom store 1
            length rows `shouldBe` 1
            closeKEL store
        it "two appends conserve state, rows, length and replay" $ do
            store <- openIntegratedKEL demoIntegration foundingDemo ":memory:"
            first <-
                appendIntegratedEvent
                    store
                    demoIntegration
                    "admin-key-1"
                    (Evt.IEApp (DemoAdd 1))
            second <-
                appendIntegratedEvent
                    store
                    demoIntegration
                    "admin-key-1"
                    (Evt.IEApp (DemoAdd 2))
            case (first, second) of
                (Right _, Right _) -> pure ()
                other ->
                    expectationFailure
                        ("expected both appends ok: " <> show other)
            live <- readState store
            demoCounter (appFold live) `shouldBe` 3
            n <- kelLength store
            n `shouldBe` 2
            rows <- readEventsFrom store 1
            length rows `shouldBe` 2
            let decoded =
                    mapMaybe
                        (\se -> (seSigner se,) <$> decodeStrict (seEventBytes se))
                        rows
            length decoded `shouldBe` 2
            foldIntegratedFrom demoIntegration foundingDemo decoded
                `shouldBe` live
            closeKEL store
        -- Assurance scope (NOTE-001/NOTE-002): concurrent execution with
        -- a co-occurrence receipt (A commits observed while B runs; the
        -- length-delta does NOT prove a shared vulnerability window) and
        -- exact conservation; no defect claimed in bdc9895.
        -- Timeout/poll failures are SETUP, never semantic kills.
        -- Cleanup release coverage holds on every exit path (bracket:
        -- stop, kill, close); execution-observed on positive +
        -- semantic-negative; setup-failure, closeKEL-throw,
        -- kill-live-worker and thrown-exception rows argued with limits
        -- (see resubmission limit list).
        -- Order boundary: no sqlite-simple in test deps, so seq_no is not
        -- read directly; id-order == commit order is pinned by exact
        -- replay == live over distinguishable log entries.
        it "concurrent appends conserve every committed transition" $ do
            bracket
                ( do
                    store <- openIntegratedKEL demoIntegration foundingDemo ":memory:"
                    stopFlag <- newEmptyMVar
                    doneA <- newEmptyMVar
                    doneB <- newEmptyMVar
                    workerRef <- newIORef []
                    let loopA n = do
                            stopped <- tryReadMVar stopFlag
                            case stopped of
                                Just _ -> putMVar doneA (Right n)
                                Nothing -> do
                                    r <-
                                        appendIntegratedEvent
                                            store
                                            demoIntegration
                                            "admin-key-1"
                                            (Evt.IEApp (DemoAdd 1))
                                    case r of
                                        Right _ -> loopA (n + 1)
                                        Left err -> putMVar doneA (Left (show err))
                    _tidA <- mask $ \restore -> do
                        tid <- forkIO (restore (loopA 0))
                        writeIORef workerRef [tid]
                        pure tid
                    pure (store, stopFlag, doneA, doneB, workerRef)
                )
                ( \(store, stopFlag, _doneA, _doneB, workerRef) -> do
                    _ <- tryPutMVar stopFlag ()
                    tids <- readIORef workerRef
                    mapM_ killThread tids
                    closeKEL store
                )
                ( \(store, stopFlag, doneA, doneB, workerRef) -> do
                    let awaitActive k
                            | k <= (0 :: Int) =
                                expectationFailure "SETUP: worker A never committed"
                            | otherwise = do
                                n0 <- kelLength store
                                if n0 >= 5
                                    then pure ()
                                    else threadDelay 10000 >> awaitActive (k - 1)
                        loopB k n
                            | k <= (0 :: Int) = putMVar doneB (Right n)
                            | otherwise = do
                                r <-
                                    appendIntegratedEvent
                                        store
                                        demoIntegration
                                        "admin-key-1"
                                        (Evt.IEApp (DemoAdd 2))
                                case r of
                                    Right _ -> loopB (k - 1) (n + 1)
                                    Left err -> putMVar doneB (Left (show err))
                    awaitActive 3000
                    commitsBeforeB <- kelLength store
                    _tidB <- mask $ \restore -> do
                        tid <- forkIO (restore (loopB 200 0))
                        modifyIORef' workerRef (tid :)
                        pure tid
                    outcomeB <- timeout 300000000 (takeMVar doneB)
                    bCount <- case outcomeB of
                        Just (Right n) -> pure n
                        Just (Left err) ->
                            expectationFailure ("worker B refused: " <> err) >> pure 0
                        Nothing ->
                            expectationFailure "SETUP: worker B join timed out" >> pure 0
                    commitsAfterB <- kelLength store
                    let concurrentCommits = commitsAfterB - commitsBeforeB - bCount
                    if concurrentCommits >= 1
                        then pure ()
                        else
                            expectationFailure
                                ( "SETUP: co-occurrence receipt empty (delta="
                                    <> show (commitsAfterB - commitsBeforeB)
                                    <> ")"
                                )
                    putMVar stopFlag ()
                    outcomeA <- timeout 300000000 (takeMVar doneA)
                    aCount <- case outcomeA of
                        Just (Right n) -> pure n
                        Just (Left err) ->
                            expectationFailure ("worker A refused: " <> err) >> pure 0
                        Nothing ->
                            expectationFailure "SETUP: worker A join timed out" >> pure 0
                    bCount `shouldBe` 200
                    live <- readState store
                    demoCounter (appFold live) `shouldBe` (aCount + 2 * bCount)
                    members live `shouldBe` members foundingDemo
                    pendingBase live `shouldBe` Map.empty
                    pendingProposals live `shouldBe` Map.empty
                    sort (demoLog (appFold live))
                        `shouldBe` sort (replicate aCount "add 1" ++ replicate 200 "add 2")
                    n <- kelLength store
                    n `shouldBe` (aCount + bCount)
                    rows <- readEventsFrom store 1
                    length rows `shouldBe` (aCount + bCount)
                    let decoded =
                            mapMaybe
                                (\se -> (seSigner se,) <$> decodeStrict (seEventBytes se))
                                rows
                    length decoded `shouldBe` (aCount + bCount)
                    let adds =
                            mapMaybe
                                ( \row -> case row of
                                    (_, Evt.IEApp (DemoAdd d)) -> Just d
                                    _ -> Nothing
                                )
                                decoded
                    length (filter (== 1) adds) `shouldBe` aCount
                    length (filter (== 2) adds) `shouldBe` bCount
                    foldIntegratedFrom demoIntegration foundingDemo decoded
                        `shouldBe` live
                )
        it "faulting codec from member throws observably" $ do
            store <- openIntegratedKEL demoIntegration foundingDemo ":memory:"
            memberResult <- tryFaultingAppend store "admin-key-1"
            case memberResult of
                Left _ -> pure ()
                Right other ->
                    expectationFailure
                        ("expected codec exception: " <> show other)
            live <- readState store
            demoCounter (appFold live) `shouldBe` 0
            n <- kelLength store
            n `shouldBe` 0
            rows <- readEventsFrom store 1
            length rows `shouldBe` 0
            closeKEL store
        it "faulting codec from nonmember keeps exact refusal" $ do
            let pureDecision =
                    applyIntegratedEvent
                        faultingIntegration
                        foundingDemo
                        "outsider"
                        (Evt.IEApp FaultingCodec)
            pureDecision
                `shouldBe` Left (IEValidation (NotAMember "outsider"))
            store <- openIntegratedKEL demoIntegration foundingDemo ":memory:"
            refused <-
                appendIntegratedEvent
                    store
                    faultingIntegration
                    "outsider"
                    (Evt.IEApp FaultingCodec)
            refused `shouldBe` Left (IEValidation (NotAMember "outsider"))
            live <- readState store
            demoCounter (appFold live) `shouldBe` 0
            n <- kelLength store
            n `shouldBe` 0
            rows <- readEventsFrom store 1
            length rows `shouldBe` 0
            closeKEL store
        it "faulting codec leaves zero state, counts and rows" $ do
            store <- openIntegratedKEL demoIntegration foundingDemo ":memory:"
            memberResult <- tryFaultingAppend store "admin-key-1"
            case memberResult of
                Left _ -> pure ()
                Right other ->
                    expectationFailure
                        ("expected codec exception: " <> show other)
            refused <-
                appendIntegratedEvent
                    store
                    faultingIntegration
                    "outsider"
                    (Evt.IEApp FaultingCodec)
            refused `shouldBe` Left (IEValidation (NotAMember "outsider"))
            live <- readState store
            demoCounter (appFold live) `shouldBe` 0
            n <- kelLength store
            n `shouldBe` 0
            rows <- readEventsFrom store 1
            length rows `shouldBe` 0
            let decoded =
                    mapMaybe
                        (\se -> (seSigner se,) <$> decodeStrict (seEventBytes se))
                        rows
            length decoded `shouldBe` 0
            foldIntegratedFrom demoIntegration foundingDemo decoded
                `shouldBe` live
            closeKEL store
        it "domain-invalid add never appends a byte" $ do
            withTempIntegrated $ \path -> do
                store <- openIntegratedKEL demoIntegration foundingDemo path
                gs0 <- readState store
                n0 <- kelLength store
                bytes0 <- BS.readFile path
                result <-
                    appendIntegratedEvent
                        store
                        demoIntegration
                        "admin-key-1"
                        (Evt.IEApp (DemoAdd (-1)))
                case result of
                    Left (IEApp (DemoNegative _)) -> pure ()
                    other -> expectationFailure ("expected DemoNegative: " <> show other)
                gs1 <- readState store
                gs1 `shouldBe` gs0
                n1 <- kelLength store
                n1 `shouldBe` n0
                bytes1 <- BS.readFile path
                bytes1 `shouldBe` bytes0
                closeKEL store
        it "nonmember append persists nothing byte-identical" $ do
            withTempIntegrated $ \path -> do
                store <- openIntegratedKEL demoIntegration foundingDemo path
                gs0 <- readState store
                n0 <- kelLength store
                bytes0 <- BS.readFile path
                result <-
                    appendIntegratedEvent
                        store
                        demoIntegration
                        "outsider-key-9"
                        (Evt.IEApp (DemoAdd 1))
                case result of
                    Left (IEValidation (NotAMember _)) -> pure ()
                    other -> expectationFailure ("expected NotAMember: " <> show other)
                gs1 <- readState store
                gs1 `shouldBe` gs0
                n1 <- kelLength store
                n1 `shouldBe` n0
                bytes1 <- BS.readFile path
                bytes1 `shouldBe` bytes0
                closeKEL store
    describe "S28-1 atomic hook" $ do
        it "succeeding hook commits with admitted evidence" $ do
            let gs = gsWithAdmin "admin-key-1"
            let pre = gs
            let post =
                    gs
                        { members =
                            Map.insert "member-key-2" (plainMember "member-key-2") (members gs)
                        }
            case commitBaseChange
                demoIntegration
                pre
                post
                (MemberAdmitted "member-key-2") of
                Right result -> irChange result `shouldBe` Just (MemberAdmitted "member-key-2")
                Left err -> expectationFailure ("expected hook ok: " <> show err)
        it "hook refusal rejects the whole transition" $ do
            let base = gsWithAdmin "admin-key-1"
            let gs =
                    base
                        { members =
                            Map.insert protectedKey (plainMember protectedKey) (members base)
                        }
            let pre = gs
            let post = gs{members = Map.delete protectedKey (members gs)}
            case commitBaseChange demoIntegration pre post (MemberRemoved protectedKey) of
                Left (IEApp (DemoHookRefused _)) -> pure ()
                other -> expectationFailure ("expected DemoHookRefused: " <> show other)
        it "failing hook restores prestate plus prelog" $ do
            withTempIntegrated $ \path -> do
                let founding =
                        foundingDemo
                            { members =
                                Map.insert
                                    protectedKey
                                    (plainMember protectedKey)
                                    (members foundingDemo)
                            }
                store <- openIntegratedKEL demoIntegration founding path
                baseline <-
                    appendIntegratedEvent
                        store
                        demoIntegration
                        "admin-key-1"
                        (Evt.IEApp (DemoAdd 5))
                case baseline of
                    Right _ -> pure ()
                    Left err -> expectationFailure ("baseline append ok: " <> show err)
                gs0 <- readState store
                n0 <- kelLength store
                bytes0 <- BS.readFile path
                result <-
                    appendIntegratedEvent
                        store
                        demoIntegration
                        "admin-key-1"
                        (Evt.IEPropose (DemoRemove protectedKey))
                case result of
                    Left (IEApp (DemoHookRefused _)) -> pure ()
                    other -> expectationFailure ("expected hook refusal: " <> show other)
                gs1 <- readState store
                gs1 `shouldBe` gs0
                n1 <- kelLength store
                n1 `shouldBe` n0
                bytes1 <- BS.readFile path
                bytes1 `shouldBe` bytes0
                closeKEL store
                store2 <- openIntegratedKEL demoIntegration founding path
                gs2 <- readState store2
                gs2 `shouldBe` gs0
                closeKEL store2
    describe "S28-1 direct-only admission" $ do
        it "direct admit by admin inserts the member" $ do
            let gs = gsWithAdmin "admin-key-1"
            case validateDirectAdmission
                demoReserved
                gs
                "admin-key-1"
                "member-key-2"
                "m@x"
                Set.empty of
                Right () -> pure ()
                Left err -> expectationFailure ("expected admit valid: " <> show err)
            case applyIntegratedEvent
                demoIntegration
                gs
                "admin-key-1"
                (Evt.IEDirect (AdmitMember "member-key-2" "m@x" Set.empty)) of
                Right result ->
                    isMemberInView "member-key-2" (groupView (irState result))
                        `shouldBe` True
                Left err -> expectationFailure ("expected admit ok: " <> show err)
        it "reserved key refused apart from members" $ do
            let gs = gsWithAdmin "admin-key-1"
            case validateDirectAdmission
                demoReserved
                gs
                "admin-key-1"
                demoReserved
                "r@x"
                Set.empty of
                Left (ReservedKey _) -> pure ()
                other -> expectationFailure ("expected ReservedKey: " <> show other)
        prop "voted mutations never insert members" $ do
            forAll genDemoProposal $ \proposal' ->
                let mutation = demoProposalMutation proposal'
                    gs = gsWithAdmin "admin-key-1"
                    post = enactMutation gs mutation
                    preKeys = Map.keysSet (members gs)
                    postKeys = Map.keysSet (members post)
                in  postKeys `Set.isSubsetOf` preKeys
        it "voted change-roles never inserts absent members" $ do
            let gs = gsWithAdmin "admin-key-1"
            let post =
                    enactMutation
                        gs
                        (ChangeRolesVoted "absent-witness-9" Set.empty)
            Map.keysSet (members post)
                `shouldBe` Map.keysSet (members gs)
            lookupMemberInView "absent-witness-9" (groupView post)
                `shouldBe` Nothing
        it "voted change-roles keeps present member keys" $ do
            let gs = gsWithAdmin "admin-key-1"
            let post =
                    enactMutation
                        gs
                        (ChangeRolesVoted "admin-key-1" Set.empty)
            Map.keysSet (members post)
                `shouldBe` Map.keysSet (members gs)
            isMemberInView "admin-key-1" (groupView post)
                `shouldBe` True
        it "voted remove never inserts absent members" $ do
            let gs = gsWithAdmin "admin-key-1"
            let post =
                    enactMutation gs (RemoveMemberVoted "absent-witness-9")
            Map.keysSet (members post)
                `shouldBe` Map.keysSet (members gs)
            lookupMemberInView "absent-witness-9" (groupView post)
                `shouldBe` Nothing
        it "nonempty pendingBase roundtrips through JSON" $ do
            let pending =
                    PendingBase
                        (RemoveMemberVoted "member-key-2")
                        "admin-key-1"
                        (Set.singleton "admin-key-1")
            let gs = foundingDemo{pendingBase = Map.singleton "pid-1" pending}
            decode (encode gs) `shouldBe` Just gs
        it "old rows decode with empty pendingBase" $ do
            let oldRow =
                    "{\"members\": [], \"pendingProposals\": [], \"appFold\": {\"demoCounter\": 0, \"demoLog\": []}}"
            case decodeStrict oldRow :: Maybe (GroupState DemoState) of
                Just gs -> pendingBase gs `shouldBe` Map.empty
                Nothing -> expectationFailure "old row should decode"
        it "malformed pendingBase fails decode" $ do
            let badRow =
                    "{\"members\": [], \"pendingProposals\": [], \"pendingBase\": 42, \"appFold\": {\"demoCounter\": 0, \"demoLog\": []}}"
            (decodeStrict badRow :: Maybe (GroupState DemoState))
                `shouldBe` Nothing
    describe "S28-1 validate/fold agreement" $ do
        prop "prefix folds match steps over mixed traces" $ do
            forAll genTrace $ \trace ->
                let prefixes = [take k trace | k <- [0 .. length trace]]
                    folded =
                        [ foldIntegrated demoIntegration (DemoState 0 []) prefix
                        | prefix <- prefixes
                        ]
                    stepped = scanl applyStep (emptyState (DemoState 0 [])) trace
                in  folded == stepped
        prop "founding folds match steps over mixed traces" $ do
            forAll genTrace $ \trace ->
                let prefixes = [take k trace | k <- [0 .. length trace]]
                    folded =
                        [ foldIntegratedFrom demoIntegration (gsWithAdmin "admin-key-1") prefix
                        | prefix <- prefixes
                        ]
                    stepped = scanl applyStep (gsWithAdmin "admin-key-1") trace
                in  folded == stepped
        it "accepted traces apply cleanly end to end" $ do
            let gs = gsWithAdmin "admin-key-1"
            case tryEnactBase demoIntegration gs "no-such-proposal" of
                Right result -> do
                    irChange result `shouldBe` Nothing
                    irState result `shouldBe` gs
                Left err -> expectationFailure ("expected no-op enact: " <> show err)
            let trace =
                    [ ("admin-key-1", Evt.IEApp (DemoAdd 1))
                    , ("admin-key-1", Evt.IEApp DemoNoop)
                    ]
            let outcomes =
                    [ applyIntegratedEvent demoIntegration state signer evt
                    | (state, (signer, evt)) <- zip (scanl applyStep gs trace) trace
                    ]
            all isRight outcomes `shouldBe` True
            foldIntegratedFrom demoIntegration gs trace
                `shouldBe` last (scanl applyStep gs trace)
    describe "S28-1 no client-decided authority" $ do
        it "verdicts flow only through the boundary" $ do
            let gs = gsWithAdmin "admin-key-1"
            case applyIntegratedEvent
                demoIntegration
                gs
                "admin-key-1"
                (Evt.IEApp (DemoAdd 4)) of
                Right result -> demoCounter (appFold (irState result)) `shouldBe` 4
                Left err -> expectationFailure ("expected verdict: " <> show err)
        it "replayed log reproduces live state exactly" $ do
            store <- openIntegratedKEL demoIntegration foundingDemo ":memory:"
            first <-
                appendIntegratedEvent
                    store
                    demoIntegration
                    "admin-key-1"
                    (Evt.IEApp (DemoAdd 3))
            case first of
                Right _ -> pure ()
                Left err -> expectationFailure ("first append ok: " <> show err)
            second <-
                appendIntegratedEvent
                    store
                    demoIntegration
                    "admin-key-1"
                    (Evt.IEApp DemoNoop)
            case second of
                Right _ -> pure ()
                Left err -> expectationFailure ("second append ok: " <> show err)
            live <- readState store
            rows <- readEventsFrom store 1
            length rows `shouldBe` 2
            let decoded =
                    mapMaybe
                        (\se -> (seSigner se,) <$> decodeStrict (seEventBytes se))
                        rows
            foldIntegratedFrom demoIntegration foundingDemo decoded
                `shouldBe` live
            closeKEL store
        it "unknown approval refused over pendingBase" $ do
            let gs = gsWithAdmin "admin-key-1"
            lookupPendingBase "missing-proposal" gs `shouldBe` Nothing
            case validateBaseApproval gs "admin-key-1" "missing-proposal" of
                Left (ProposalNotFound _) -> pure ()
                other -> expectationFailure ("expected ProposalNotFound: " <> show other)
        it "voted validation covers both arms exactly" $ do
            let gs = gsWithAdmin "admin-key-1"
            case validateBaseMutation
                gs
                "admin-key-1"
                (RemoveMemberVoted "member-key-2") of
                Left (MemberNotFound _) -> pure ()
                other -> expectationFailure ("expected MemberNotFound: " <> show other)
            case validateBaseMutation
                gs
                "admin-key-1"
                (ChangeRolesVoted "admin-key-1" Set.empty) of
                Right () -> pure ()
                Left err -> expectationFailure ("expected ChangeRoles ok: " <> show err)
        it "pending entries survive close and reopen" $ do
            withTempIntegrated $ \path -> do
                let founding =
                        demoInitialState
                            { members =
                                Map.fromList
                                    [ ("admin-key-1", adminMember "admin-key-1")
                                    , ("admin-key-2", adminMember "admin-key-2")
                                    , ("admin-key-3", adminMember "admin-key-3")
                                    , ("member-key-2", plainMember "member-key-2")
                                    ]
                            }
                store <- openIntegratedKEL demoIntegration founding path
                result <-
                    appendIntegratedEvent
                        store
                        demoIntegration
                        "admin-key-1"
                        (Evt.IEPropose (DemoRemove "member-key-2"))
                case result of
                    Right pending -> irChange pending `shouldBe` Nothing
                    Left err -> expectationFailure ("expected pending ok: " <> show err)
                live <- readState store
                lookupPendingBase (demoDigest (DemoRemove "member-key-2")) live
                    `shouldSatisfy` (/= Nothing)
                closeKEL store
                store2 <- openIntegratedKEL demoIntegration founding path
                live2 <- readState store2
                live2 `shouldBe` live
                closeKEL store2
