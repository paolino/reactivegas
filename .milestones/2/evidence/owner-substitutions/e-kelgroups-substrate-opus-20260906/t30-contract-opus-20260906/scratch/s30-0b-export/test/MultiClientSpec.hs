{-# OPTIONS_GHC -Wno-x-partial #-}

{- |
Module      : MultiClientSpec
Description : Multi-client E2E scenarios
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Explicit multi-client scenarios where separate identities
(Alice, Bob, Charlie, Dave) each make HTTP calls with
their own @?key=@ parameter and observe different views
of the group.
-}
module MultiClientSpec (spec) where

import Data.Text qualified as T
import Network.HTTP.Client qualified as HC
import Network.HTTP.Types (status200, status403)
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
-- Scenario 1: Admin bootstraps, outsider joins
-- --------------------------------------------------------

scenario1 :: Spec
scenario1 =
    describe
        "Two clients: admin bootstraps, outsider joins"
        $ around withTestEnv
        $ do
            it
                "Bob sees empty group, Alice bootstraps, \
                \Bob joins"
                $ \te -> do
                    alice <- newTestId
                    bob <- newTestId

                    -- 1. Bob sees no admins and is not pending
                    info0 <-
                        getInfo
                            te
                            (T.unpack $ tidKey bob)
                    irEmails info0 `shouldBe` []
                    irPending info0 `shouldBe` False

                    -- 2. Alice bootstraps as PublicAdmin
                    -- (event 2, server inception is 1)
                    sn1 <-
                        postEvent te (bootstrap alice)
                    sn1 `shouldBe` 2

                    -- 3. Bob sees Alice's email in admin list
                    info1 <-
                        getInfo
                            te
                            (T.unpack $ tidKey bob)
                    length (irEmails info1) `shouldBe` 1
                    irPending info1 `shouldBe` False

                    -- 4. Bob cannot access /condition
                    resp403 <-
                        httpGet
                            te
                            ( "/condition?key="
                                <> T.unpack (tidKey bob)
                            )
                    HC.responseStatus resp403
                        `shouldBe` status403

                    -- 5. Alice proposes introducing Bob
                    sn2 <-
                        postEvent
                            te
                            (proposeMember alice bob)
                    sn2 `shouldBe` 3

                    -- 6. Bob checks /info — already enacted
                    info2 <-
                        getInfo
                            te
                            (T.unpack $ tidKey bob)
                    irPending info2 `shouldBe` False

                    -- 7. Bob can now access /condition
                    cBob <-
                        getCondition
                            te
                            (T.unpack $ tidKey bob)
                    crAuthMode cBob `shouldBe` "normal"
                    length (crMembers cBob) `shouldBe` 2

                    -- 8. Bob can replay the KEL
                    -- First event is server inception
                    e0 <-
                        httpGet
                            te
                            ( "/events?after=-1&key="
                                <> T.unpack (tidKey bob)
                            )
                    HC.responseStatus e0
                        `shouldBe` status200
                    er0 <-
                        decodeOrFail (HC.responseBody e0)
                    erSigner er0
                        `shouldBe` teServerKey te

-- --------------------------------------------------------
-- Scenario 2: Multi-admin majority approval
-- --------------------------------------------------------

scenario2 :: Spec
scenario2 =
    describe
        "Three clients: multi-admin majority approval"
        $ around withTestEnv
        $ do
            it "3 admins require 2 approvals for Dave" $
                \te -> do
                    alice <- newTestId
                    bob <- newTestId
                    charlie <- newTestId
                    dave <- newTestId

                    _ <-
                        postEvent te (bootstrap alice)
                    _ <-
                        postEvent
                            te
                            (proposeAdmin alice bob)
                    _ <-
                        postEvent
                            te
                            ( proposeAdmin
                                alice
                                charlie
                            )

                    infoDave <-
                        getInfo
                            te
                            (T.unpack $ tidKey dave)
                    length (irEmails infoDave)
                        `shouldBe` 3

                    _ <-
                        postEvent
                            te
                            (proposeMember alice dave)

                    cAlice <-
                        getCondition
                            te
                            (T.unpack $ tidKey alice)
                    length (crPending cAlice)
                        `shouldBe` 1
                    length (crMembers cAlice)
                        `shouldBe` 3

                    cBob <-
                        getCondition
                            te
                            (T.unpack $ tidKey bob)
                    length (crPending cBob) `shouldBe` 1
                    length (crMembers cBob) `shouldBe` 3

                    let pid =
                            fst (head $ crPending cAlice)
                    _ <-
                        postEvent te (approve bob pid)

                    cCharlie <-
                        getCondition
                            te
                            ( T.unpack $
                                tidKey charlie
                            )
                    length (crMembers cCharlie)
                        `shouldBe` 4
                    crPending cCharlie
                        `shouldSatisfy` null

                    cDave <-
                        getCondition
                            te
                            (T.unpack $ tidKey dave)
                    crAuthMode cDave `shouldBe` "normal"
                    length (crMembers cDave) `shouldBe` 4

-- --------------------------------------------------------
-- Scenario 3: Full lifecycle
-- --------------------------------------------------------

scenario3 :: Spec
scenario3 =
    describe
        "Three clients: full lifecycle"
        $ around withTestEnv
        $ do
            it "introduce, remove, wipe, re-bootstrap" $
                \te -> do
                    alice <- newTestId
                    bob <- newTestId
                    charlie <- newTestId

                    _ <-
                        postEvent te (bootstrap alice)
                    _ <-
                        postEvent
                            te
                            (proposeAdmin alice bob)
                    _ <-
                        postEvent
                            te
                            ( proposeMember
                                alice
                                charlie
                            )

                    c0 <-
                        getCondition
                            te
                            (T.unpack $ tidKey alice)
                    length (crMembers c0) `shouldBe` 3

                    _ <-
                        postEvent
                            te
                            (proposeRemove bob charlie)

                    resp403 <-
                        httpGet
                            te
                            ( "/condition?key="
                                <> T.unpack
                                    (tidKey charlie)
                            )
                    HC.responseStatus resp403
                        `shouldBe` status403

                    infoCharlie <-
                        getInfo
                            te
                            ( T.unpack $
                                tidKey charlie
                            )
                    length (irEmails infoCharlie)
                        `shouldBe` 2

                    _ <-
                        postEvent
                            te
                            (proposeRemove alice bob)
                    _ <-
                        postEvent
                            te
                            (proposeRemove alice alice)

                    resp403b <-
                        httpGet
                            te
                            ( "/condition?key="
                                <> T.unpack (tidKey bob)
                            )
                    HC.responseStatus resp403b
                        `shouldBe` status403

                    infoAnyone <-
                        getInfo te "anyone"
                    irEmails infoAnyone `shouldBe` []

                    _ <- postEvent te (bootstrap bob)

                    cBob <-
                        getCondition
                            te
                            (T.unpack $ tidKey bob)
                    crAuthMode cBob `shouldBe` "normal"
                    length (crMembers cBob) `shouldBe` 1
                    mrKey (head $ crMembers cBob)
                        `shouldBe` tidKey bob

                    resp403c <-
                        httpGet
                            te
                            ( "/condition?key="
                                <> T.unpack
                                    (tidKey alice)
                            )
                    HC.responseStatus resp403c
                        `shouldBe` status403

-- --------------------------------------------------------
-- Top-level spec
-- --------------------------------------------------------

spec :: Spec
spec = describe "Multi-client scenarios" $ do
    scenario1
    scenario2
    scenario3
