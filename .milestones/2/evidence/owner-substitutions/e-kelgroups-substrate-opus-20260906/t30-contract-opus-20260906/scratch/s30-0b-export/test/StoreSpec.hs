{- |
Module      : StoreSpec
Description : SQLite KEL store mechanics properties
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Monadic QuickCheck properties testing the SQLite-backed
KEL store through real disk I/O. Covers roundtrip, fold
consistency, readEventsFrom, and kelLength.
-}
module StoreSpec (spec) where

import Control.Monad (forM_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Set qualified as Set
import Data.Text (Text, pack)
import KelGroups.Event
    ( BaseEvent (..)
    , GroupEvent (..)
    , Proposal (..)
    )
import KelGroups.Server (mkKeriEvent)
import KelGroups.State (emptyState)
import KelGroups.Store
    ( ChainTip (..)
    , StoredEvent (..)
    , appendEvent
    , chainTip
    , closeKEL
    , kelLength
    , openKEL
    , readEventsFrom
    , readState
    )
import KelGroups.Trivial
    ( trivialFold
    , trivialInitial
    )
import KelGroups.Types (Admin (..), Role (..))
import Keri.Event
    ( eventDigest
    , eventPrefix
    , eventSequenceNumber
    )
import System.Directory (removeFile)
import System.IO.Temp (emptySystemTempFile)
import Test.Hspec (Spec, around, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
    ( Gen
    , chooseInt
    , listOf1
    , oneof
    )
import Test.QuickCheck.Monadic
    ( assert
    , monadicIO
    , pick
    , run
    )

-- --------------------------------------------------------
-- Helpers
-- --------------------------------------------------------

-- | Run an action with a temporary SQLite KEL.
withTempKEL :: (FilePath -> IO a) -> IO a
withTempKEL action = do
    path <- emptySystemTempFile "kelgroups-test-.db"
    result <- action path
    removeFile path
    pure result

arbitraryKey :: Gen Text
arbitraryKey =
    pack . ("key" <>) . show <$> chooseInt (0, 19)

{- | Generate a base event stream that makes sense:
bootstrap proposals first, then normal operations.
-}
arbitraryBaseEvents :: Gen [(Text, GroupEvent ())]
arbitraryBaseEvents = do
    bootstrapKey <- arbitraryKey
    let bootstrapEvt =
            ( "bootstrap"
            , Base $
                Propose $
                    IntroduceMember
                        bootstrapKey
                        (bootstrapKey <> "@test.example")
                        ( Set.singleton
                            (AdminRole PublicAdmin)
                        )
            )
    rest <- listOf1 $ do
        key <- arbitraryKey
        evt <-
            oneof
                [ pure $
                    Base $
                        Propose $
                            IntroduceMember
                                key
                                (key <> "@test.example")
                                ( Set.singleton
                                    (AdminRole PublicAdmin)
                                )
                , pure $
                    Base $
                        Propose $
                            RemoveMember key
                ]
        pure (bootstrapKey, evt)
    pure $ bootstrapEvt : rest

-- --------------------------------------------------------
-- Specs
-- --------------------------------------------------------

spec :: Spec
spec = describe "KelGroups.Store (SQLite)" $ do
    describe "empty KEL" $ do
        it "opening empty DB gives emptyState" $
            withTempKEL $ \path -> do
                store <-
                    openKEL trivialFold trivialInitial path
                gs <- readState store
                len <- kelLength store
                closeKEL store
                gs `shouldBe` emptyState trivialInitial
                -- Server inception is auto-created
                len `shouldBe` 1

    describe "roundtrip" $ do
        prop "signers match after append+read" $
            monadicIO $ do
                events <- pick arbitraryBaseEvents
                signers <- run $
                    withTempKEL $ \path -> do
                        store <-
                            openKEL
                                trivialFold
                                trivialInitial
                                path
                        tip0 <- chainTip store
                        tipRef <- newIORef tip0
                        forM_ events $
                            \(signer, groupEvt) -> do
                                tip <-
                                    readIORef tipRef
                                let keriEvt =
                                        mkKeriEvent
                                            tip
                                            signer
                                            groupEvt
                                appendEvent
                                    store
                                    trivialFold
                                    signer
                                    keriEvt
                                    "test-sig"
                                    groupEvt
                                writeIORef tipRef $
                                    Just
                                        ChainTip
                                            { tipPrefix =
                                                eventPrefix
                                                    keriEvt
                                            , tipSeqNo =
                                                eventSequenceNumber
                                                    keriEvt
                                            , tipDigest =
                                                eventDigest
                                                    keriEvt
                                            }
                        -- Skip server inception (id=1)
                        replayed <-
                            readEventsFrom store 2
                        closeKEL store
                        pure
                            ( map fst events
                            , map seSigner replayed
                            )
                assert $
                    fst signers == snd signers

    describe "fold consistency" $ do
        prop
            "incremental TVar matches reopened replay"
            $ monadicIO
            $ do
                events <- pick arbitraryBaseEvents
                (stateIncremental, stateReplayed) <- run $
                    withTempKEL $ \path -> do
                        store <-
                            openKEL
                                trivialFold
                                trivialInitial
                                path
                        tip0 <- chainTip store
                        tipRef <- newIORef tip0
                        forM_ events $
                            \(signer, groupEvt) -> do
                                tip <-
                                    readIORef tipRef
                                let keriEvt =
                                        mkKeriEvent
                                            tip
                                            signer
                                            groupEvt
                                appendEvent
                                    store
                                    trivialFold
                                    signer
                                    keriEvt
                                    "test-sig"
                                    groupEvt
                                writeIORef tipRef $
                                    Just
                                        ChainTip
                                            { tipPrefix =
                                                eventPrefix
                                                    keriEvt
                                            , tipSeqNo =
                                                eventSequenceNumber
                                                    keriEvt
                                            , tipDigest =
                                                eventDigest
                                                    keriEvt
                                            }
                        gs1 <- readState store
                        closeKEL store
                        store2 <-
                            openKEL
                                trivialFold
                                trivialInitial
                                path
                        gs2 <- readState store2
                        closeKEL store2
                        pure (gs1, gs2)
                assert $
                    stateIncremental == stateReplayed

    describe "readEventsFrom" $ around withTempKEL $ do
        it "returns tail from index N" $ \path -> do
            store <-
                openKEL trivialFold trivialInitial path
            let events =
                    [
                        ( "signer1"
                        , Base $
                            Propose $
                                IntroduceMember
                                    "k1"
                                    "k1@test.example"
                                    ( Set.singleton
                                        (AdminRole PublicAdmin)
                                    )
                        )
                    ,
                        ( "k1"
                        , Base $
                            Propose $
                                IntroduceMember
                                    "k2"
                                    "k2@test.example"
                                    ( Set.singleton
                                        (AdminRole PublicAdmin)
                                    )
                        )
                    ,
                        ( "k1"
                        , Base $
                            Propose $
                                RemoveMember "k2"
                        )
                    ]
            tip0 <- chainTip store
            tipRef <- newIORef tip0
            forM_ events $ \(signer, groupEvt) -> do
                tip <- readIORef tipRef
                let keriEvt =
                        mkKeriEvent tip signer groupEvt
                appendEvent
                    store
                    trivialFold
                    signer
                    keriEvt
                    "test-sig"
                    groupEvt
                writeIORef tipRef $
                    Just
                        ChainTip
                            { tipPrefix =
                                eventPrefix keriEvt
                            , tipSeqNo =
                                eventSequenceNumber
                                    keriEvt
                            , tipDigest =
                                eventDigest keriEvt
                            }
            -- Client events at ids 2,3,4; skip first
            tail' <- readEventsFrom store 3
            closeKEL store
            map seSigner tail'
                `shouldBe` map fst (drop 1 events)

        it "returns empty for index beyond length" $
            \path -> do
                store <-
                    openKEL
                        trivialFold
                        trivialInitial
                        path
                tip0 <- chainTip store
                let signer = "s"
                    groupEvt :: GroupEvent ()
                    groupEvt =
                        Base $
                            Propose $
                                IntroduceMember
                                    "k"
                                    "k@test.example"
                                    ( Set.singleton
                                        (AdminRole PublicAdmin)
                                    )
                    keriEvt =
                        mkKeriEvent tip0 signer groupEvt
                appendEvent
                    store
                    trivialFold
                    signer
                    keriEvt
                    "test-sig"
                    groupEvt
                tail' <- readEventsFrom store 99
                closeKEL store
                tail' `shouldBe` []

    describe "kelLength" $ do
        prop "length matches number of appends + 1" $
            monadicIO $ do
                events <- pick arbitraryBaseEvents
                len <- run $
                    withTempKEL $ \path -> do
                        store <-
                            openKEL
                                trivialFold
                                trivialInitial
                                path
                        tip0 <- chainTip store
                        tipRef <- newIORef tip0
                        forM_ events $
                            \(signer, groupEvt) -> do
                                tip <-
                                    readIORef tipRef
                                let keriEvt =
                                        mkKeriEvent
                                            tip
                                            signer
                                            groupEvt
                                appendEvent
                                    store
                                    trivialFold
                                    signer
                                    keriEvt
                                    "test-sig"
                                    groupEvt
                                writeIORef tipRef $
                                    Just
                                        ChainTip
                                            { tipPrefix =
                                                eventPrefix
                                                    keriEvt
                                            , tipSeqNo =
                                                eventSequenceNumber
                                                    keriEvt
                                            , tipDigest =
                                                eventDigest
                                                    keriEvt
                                            }
                        l <- kelLength store
                        closeKEL store
                        pure l
                -- +1 for the server inception
                assert $ len == length events + 1
