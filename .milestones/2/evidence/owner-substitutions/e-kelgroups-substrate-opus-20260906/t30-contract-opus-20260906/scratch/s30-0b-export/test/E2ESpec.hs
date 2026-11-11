{-# OPTIONS_GHC -Wno-x-partial #-}

{- |
Module      : E2ESpec
Description : End-to-end scenarios through the HTTP API
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Multi-step workflows exercising the full group lifecycle
via HTTP calls: bootstrap, proposals, approvals, role
changes, member removal, KEL replay, and SSE.
-}
module E2ESpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, race)
import Control.Concurrent.STM
    ( atomically
    , newTChanIO
    , readTChan
    , writeTChan
    )
import Data.Aeson (encode)
import Data.ByteString qualified as BS
import Data.Set qualified as Set
import Data.Text qualified as T
import KelGroups.Server.JSON (Submission (..))
import KelGroups.Types (Admin (..), Role (..))
import Network.HTTP.Client qualified as HC
import Network.HTTP.Types
    ( status200
    , status401
    , status422
    )
import Test.Hspec
    ( Spec
    , around
    , describe
    , it
    , shouldBe
    , shouldSatisfy
    )
import TestHelpers

-- --------------------------------------------------------
-- Specs
-- --------------------------------------------------------

spec :: Spec
spec = describe "E2E scenarios" $ around withTestEnv $ do
    describe "Group bootstrap and single-admin lifecycle" $ do
        it "bootstrap → add member → verify state" $
            \te -> do
                admin1 <- newTestId
                user1 <- newTestId

                -- Bootstrap first admin (event 2,
                -- server inception is event 1)
                sn1 <- postEvent te (bootstrap admin1)
                sn1 `shouldBe` 2

                -- Now in normal mode with 1 member
                c1 <-
                    getCondition
                        te
                        (T.unpack $ tidKey admin1)
                crAuthMode c1 `shouldBe` "normal"
                length (crMembers c1) `shouldBe` 1
                mrKey (head $ crMembers c1)
                    `shouldBe` tidKey admin1

                -- Single admin proposes a non-admin member
                -- majority(1) = 1, auto-enacts
                sn2 <-
                    postEvent
                        te
                        (proposeMember admin1 user1)
                sn2 `shouldBe` 3

                -- Verify 2 members now
                c2 <-
                    getCondition
                        te
                        (T.unpack $ tidKey admin1)
                length (crMembers c2) `shouldBe` 2

    describe "Multi-admin approval flow" $ do
        it
            "3 admins, proposal requires 2 approvals"
            $ \te -> do
                a1 <- newTestId
                a2 <- newTestId
                a3 <- newTestId
                user1 <- newTestId

                -- Bootstrap 3 admins
                _ <- postEvent te (bootstrap a1)
                _ <- postEvent te (proposeAdmin a1 a2)
                _ <- postEvent te (proposeAdmin a1 a3)

                c <-
                    getCondition
                        te
                        (T.unpack $ tidKey a1)
                length (crMembers c) `shouldBe` 3

                -- Now majority = ceil(3/2) = 2
                _ <-
                    postEvent
                        te
                        (proposeMember a1 user1)

                c1 <-
                    getCondition
                        te
                        (T.unpack $ tidKey a1)
                length (crMembers c1) `shouldBe` 3
                length (crPending c1) `shouldBe` 1

                -- a2 approves — reaches majority, enacted
                let pid = fst (head $ crPending c1)
                _ <- postEvent te (approve a2 pid)

                c2 <-
                    getCondition
                        te
                        (T.unpack $ tidKey a1)
                length (crMembers c2) `shouldBe` 4
                crPending c2 `shouldSatisfy` null

        it "duplicate approval is rejected" $ \te -> do
            a1 <- newTestId
            a2 <- newTestId
            a3 <- newTestId
            user1 <- newTestId

            _ <- postEvent te (bootstrap a1)
            _ <- postEvent te (proposeAdmin a1 a2)
            _ <- postEvent te (proposeAdmin a1 a3)

            _ <-
                postEvent
                    te
                    (proposeMember a1 user1)
            c <-
                getCondition
                    te
                    (T.unpack $ tidKey a1)
            let pid = fst (head $ crPending c)

            -- a1 tries to approve own proposal
            sub <- signSubmission te (approve a1 pid)
            resp <-
                httpPost
                    te
                    "/events"
                    (encode sub)
            HC.responseStatus resp `shouldBe` status422

    describe "Member removal" $ do
        it "admin removes a member via proposal" $
            \te -> do
                a1 <- newTestId
                user1 <- newTestId

                _ <- postEvent te (bootstrap a1)
                _ <-
                    postEvent
                        te
                        (proposeMember a1 user1)

                c0 <-
                    getCondition
                        te
                        (T.unpack $ tidKey a1)
                length (crMembers c0) `shouldBe` 2

                _ <-
                    postEvent
                        te
                        (proposeRemove a1 user1)

                c1 <-
                    getCondition
                        te
                        (T.unpack $ tidKey a1)
                length (crMembers c1) `shouldBe` 1
                mrKey (head $ crMembers c1)
                    `shouldBe` tidKey a1

        it "removing last admin returns to bootstrap" $
            \te -> do
                a1 <- newTestId

                _ <- postEvent te (bootstrap a1)
                c0 <-
                    getCondition
                        te
                        (T.unpack $ tidKey a1)
                crAuthMode c0 `shouldBe` "normal"

                _ <- postEvent te (proposeRemove a1 a1)

                resp <- httpGet te "/info?key=anyone"
                HC.responseStatus resp `shouldBe` status200

    describe "Role changes" $ do
        it "promote member to admin" $ \te -> do
            a1 <- newTestId
            user1 <- newTestId

            _ <- postEvent te (bootstrap a1)
            _ <-
                postEvent
                    te
                    (proposeMember a1 user1)

            _ <-
                postEvent
                    te
                    ( proposeChangeRoles
                        a1
                        user1
                        ( Set.singleton
                            (AdminRole PublicAdmin)
                        )
                    )

            c <-
                getCondition
                    te
                    (T.unpack $ tidKey a1)
            length (crMembers c) `shouldBe` 2

        it "demote admin to regular member" $ \te -> do
            a1 <- newTestId
            a2 <- newTestId

            _ <- postEvent te (bootstrap a1)
            _ <- postEvent te (proposeAdmin a1 a2)

            _ <-
                postEvent
                    te
                    ( proposeChangeRoles
                        a1
                        a2
                        Set.empty
                    )

            c <-
                getCondition
                    te
                    (T.unpack $ tidKey a1)
            length (crMembers c) `shouldBe` 2

    describe "KEL replay via GET /events" $ do
        it "reads back all events in order" $ \te -> do
            a1 <- newTestId
            u1 <- newTestId
            u2 <- newTestId

            _ <- postEvent te (bootstrap a1)
            _ <-
                postEvent te (proposeMember a1 u1)
            _ <-
                postEvent te (proposeMember a1 u2)

            let keyParam =
                    T.unpack (tidKey a1)
            -- Event 1: server inception
            e0 <-
                httpGet
                    te
                    ( "/events?after=-1&key="
                        <> keyParam
                    )
            HC.responseStatus e0 `shouldBe` status200
            er0 <- decodeOrFail (HC.responseBody e0)
            erSigner er0 `shouldBe` teServerKey te

            -- Event 2: bootstrap
            e1 <-
                httpGet
                    te
                    ( "/events?after=1&key="
                        <> keyParam
                    )
            HC.responseStatus e1 `shouldBe` status200

            -- Event 3: propose u1
            e2 <-
                httpGet
                    te
                    ( "/events?after=2&key="
                        <> keyParam
                    )
            HC.responseStatus e2 `shouldBe` status200

    describe "Authorization edge cases" $ do
        it "non-member cannot propose" $ \te -> do
            a1 <- newTestId
            nobody <- newTestId
            u1 <- newTestId

            _ <- postEvent te (bootstrap a1)

            sub <-
                signSubmission
                    te
                    (proposeMember nobody u1)
            resp <-
                httpPost
                    te
                    "/events"
                    (encode sub)
            HC.responseStatus resp `shouldBe` status422

        it "non-admin member cannot propose" $ \te -> do
            a1 <- newTestId
            user1 <- newTestId
            user2 <- newTestId

            _ <- postEvent te (bootstrap a1)
            _ <-
                postEvent
                    te
                    (proposeMember a1 user1)

            sub <-
                signSubmission
                    te
                    (proposeMember user1 user2)
            resp <-
                httpPost
                    te
                    "/events"
                    (encode sub)
            HC.responseStatus resp `shouldBe` status422

        it "bootstrap rejects missing passphrase" $
            \te -> do
                a1 <- newTestId
                sub <- signSubmission te (bootstrap a1)
                let sub' = sub{subPassphrase = Nothing}
                resp <-
                    httpPost te "/events" (encode sub')
                HC.responseStatus resp
                    `shouldBe` status401

        it "duplicate member introduction is rejected" $
            \te -> do
                a1 <- newTestId
                _ <- postEvent te (bootstrap a1)
                sub <-
                    signSubmission
                        te
                        (proposeAdmin a1 a1)
                resp <-
                    httpPost
                        te
                        "/events"
                        (encode sub)
                HC.responseStatus resp
                    `shouldBe` status422

    describe "Re-bootstrap after total admin removal" $ do
        it "new admin can bootstrap after wipe" $ \te -> do
            a1 <- newTestId
            a2 <- newTestId

            _ <- postEvent te (bootstrap a1)
            _ <- postEvent te (proposeRemove a1 a1)

            resp0 <- httpGet te "/info?key=anyone"
            HC.responseStatus resp0 `shouldBe` status200

            _ <- postEvent te (bootstrap a2)

            c1 <-
                getCondition
                    te
                    (T.unpack $ tidKey a2)
            crAuthMode c1 `shouldBe` "normal"
            length (crMembers c1) `shouldBe` 1
            mrKey (head $ crMembers c1)
                `shouldBe` tidKey a2

    describe "SSE receives all events in sequence" $ do
        it "2 POSTs yield 2 SSE notifications" $
            \te -> do
                a1 <- newTestId
                u1 <- newTestId
                u2 <- newTestId

                _ <- postEvent te (bootstrap a1)

                resultChan <- newTChanIO
                listener <- async $ do
                    initReq <-
                        HC.parseRequest $
                            "http://127.0.0.1:"
                                <> show (tePort te)
                                <> "/stream?key="
                                <> T.unpack (tidKey a1)
                    HC.withResponse initReq (teMgr te) $
                        \resp -> do
                            let readChunks n
                                    | n <= 0 = pure ()
                                    | otherwise = do
                                        chunk <-
                                            HC.responseBody
                                                resp
                                        atomically $
                                            writeTChan
                                                resultChan
                                                chunk
                                        readChunks (n - 1)
                            readChunks (2 :: Int)

                threadDelay 50000

                _ <-
                    postEvent
                        te
                        (proposeMember a1 u1)
                _ <-
                    postEvent
                        te
                        (proposeMember a1 u2)

                result <-
                    race
                        (threadDelay 2000000)
                        ( do
                            c1 <-
                                atomically $
                                    readTChan resultChan
                            c2 <-
                                atomically $
                                    readTChan resultChan
                            pure [c1, c2]
                        )
                cancel listener
                case result of
                    Right chunks -> do
                        length chunks `shouldBe` 2
                        all
                            (BS.isInfixOf "\"sn\":")
                            chunks
                            `shouldBe` True
                    Left () ->
                        error "SSE timeout"
