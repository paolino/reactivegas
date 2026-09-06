{-# LANGUAGE OverloadedStrings #-}

{- | Direct behavior of the selected custody arms: guards, balance effects,
frame preservation, lossless identity and the canonical-view queries.
Shared fixtures and the behavioral oracle live here so every spec and
every fault-injected mutant is judged by the same predicate.
-}
module MoneyCustodySpec (
    spec,
    TestFrame (..),
    Outcome (..),
    frame,
    adminView,
    realQueries,
    memberFaultQueries,
    transferSignerFaultQueries,
    donateSignerFaultQueries,
    oracle,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import KelGroups.Types (
    Admin (..),
    GroupView (..),
    Member (..),
    Role (..),
 )
import Reactivegas.Economic.Core (
    CustodyEvent (..),
    Key,
    Queries (..),
    State (..),
 )
import Reactivegas.Economic.KelGroups (queriesFromView, stepInView)
import Test.Hspec

{- | A caller frame with nonempty collections, votes and opaque payload:
every applied transition must preserve it structurally unchanged.
-}
data TestFrame = TestFrame
    { tfCollections :: [Text]
    , tfVotes :: [Int]
    , tfOpaque :: Maybe (Text, [Int])
    }
    deriving (Eq, Show)

frame :: TestFrame
frame = TestFrame ["col-7"] [1, 2] (Just ("opaque-payload", [3]))

{- | The corpus view shape: keys 1 and 2 are admins, 3 is a plain member,
9 is absent. Keys are opaque text, never normalized.
-}
adminView :: GroupView
adminView =
    GroupView
        { gvMembers =
            Map.fromList
                [ ("1", Member "1" "1@trace" adminRoles)
                , ("2", Member "2" "2@trace" adminRoles)
                , ("3", Member "3" "3@trace" Set.empty)
                ]
        }
  where
    adminRoles = Set.singleton (AdminRole PublicAdmin)

-- | Production queries derived from the one canonical view.
realQueries :: Queries
realQueries = queriesFromView adminView

-- | Fault-injected boundary: only the member query is mutated.
memberFaultQueries :: Queries
memberFaultQueries =
    realQueries
        { memberQuery = \key -> key == "9" || memberQuery realQueries key
        }

-- | Fault-injected boundary: only the transfer signer's admin answer changes.
transferSignerFaultQueries :: Queries
transferSignerFaultQueries =
    realQueries
        { adminQuery = \key -> key /= "2" && adminQuery realQueries key
        }

-- | Fault-injected boundary: only the donation signer's admin answer changes.
donateSignerFaultQueries :: Queries
donateSignerFaultQueries =
    realQueries
        { adminQuery = \key -> key /= "1" && adminQuery realQueries key
        }

{- | Complete expected result at the comparison boundary: the one refusal,
or the complete applied state including conti, casse and the frame.
-}
data Outcome
    = Refused
    | Applied [(Key, Integer)] [(Key, Integer)] TestFrame
    deriving (Eq, Show)

{- | The shared behavioral oracle. The same call that accepts a real
transition must reject a fault-injected mutant of it.
-}
oracle :: Outcome -> Maybe (State TestFrame) -> Bool
oracle outcome actual = case (outcome, actual) of
    (Refused, Nothing) -> True
    (Applied c cs f, Just s) ->
        conti s == c && casse s == cs && untouched s == f
    _ -> False

apply :: State TestFrame -> Key -> CustodyEvent -> Maybe (State TestFrame)
apply = stepInView adminView

spec :: Spec
spec = do
    depositSpec
    withdrawSpec
    transferSpec
    donateSpec
    identitySpec
    querySpec

depositSpec :: Spec
depositSpec =
    describe "deposit" $ do
        it "credits a member conti from the signer casse" $
            apply (State [] [] frame) "2" (Deposit "1" 40)
                `shouldBe` Just (State [("1", 40)] [("2", 40)] frame)
        it "accepts a zero amount and appends zero entries" $
            apply (State [] [] frame) "1" (Deposit "3" 0)
                `shouldBe` Just (State [("3", 0)] [("1", 0)] frame)
        it "refuses a nonmember user" $
            apply (State [] [] frame) "1" (Deposit "9" 5) `shouldBe` Nothing
        it "refuses a nonadmin signer" $
            apply (State [] [] frame) "3" (Deposit "1" 5) `shouldBe` Nothing
        it "refuses a self deposit" $
            apply (State [] [] frame) "1" (Deposit "1" 5) `shouldBe` Nothing
        it "refuses a negative amount" $
            apply (State [] [] frame) "2" (Deposit "1" (-5)) `shouldBe` Nothing
        it "bumps only the first matching duplicate entry" $
            apply (State [("1", 10), ("1", 5)] [] frame) "2" (Deposit "1" 1)
                `shouldBe` Just (State [("1", 11), ("1", 5)] [("2", 1)] frame)
        it "preserves the frame on success" $
            fmap untouched (apply (State [] [] frame) "2" (Deposit "1" 40))
                `shouldBe` Just frame

withdrawSpec :: Spec
withdrawSpec =
    describe "withdraw" $ do
        it "debits a member conti into the signer casse" $
            apply (State [("1", 50)] [] frame) "2" (Withdraw "1" 30)
                `shouldBe` Just (State [("1", 20)] [("2", -30)] frame)
        it "refuses when the balance is insufficient" $
            apply (State [("1", 10)] [] frame) "2" (Withdraw "1" 30)
                `shouldBe` Nothing
        it "accepts a zero amount under the pinned balance guard" $
            apply (State [("1", 0)] [] frame) "2" (Withdraw "1" 0)
                `shouldBe` Just (State [("1", 0)] [("2", 0)] frame)
        it "reverses a negative amount under the actual Lean guard" $
            apply (State [("1", 40)] [] frame) "2" (Withdraw "1" (-5))
                `shouldBe` Just (State [("1", 45)] [("2", 5)] frame)
        it "refuses a nonadmin signer" $
            apply (State [("1", 50)] [] frame) "3" (Withdraw "1" 5)
                `shouldBe` Nothing
        it "refuses a nonmember user" $
            apply (State [("9", 50)] [] frame) "2" (Withdraw "9" 5)
                `shouldBe` Nothing
        it "refuses a self withdrawal" $
            apply (State [("1", 50)] [] frame) "1" (Withdraw "1" 5)
                `shouldBe` Nothing
        it "refuses while the comune account is stalled" $
            apply (State [("comune", -1), ("1", 40)] [] frame) "2" (Withdraw "1" 10)
                `shouldBe` Nothing

transferSpec :: Spec
transferSpec =
    describe "transferCassa" $ do
        it "moves between the cash boxes of two admins" $
            apply (State [] [("1", 100), ("2", 50)] frame) "2" (TransferCassa "1" 30)
                `shouldBe` Just (State [] [("1", 70), ("2", 80)] frame)
        it "appends the receiving cash box when absent" $
            apply (State [] [("1", 10)] frame) "2" (TransferCassa "1" 4)
                `shouldBe` Just (State [] [("1", 6), ("2", 4)] frame)
        it "allows the source cash box to go negative" $
            apply (State [] [("1", 5)] frame) "2" (TransferCassa "1" 10)
                `shouldBe` Just (State [] [("1", -5), ("2", 10)] frame)
        it "refuses a zero amount" $
            apply (State [] [("1", 10)] frame) "2" (TransferCassa "1" 0)
                `shouldBe` Nothing
        it "refuses a negative amount" $
            apply (State [] [("1", 10)] frame) "2" (TransferCassa "1" (-1))
                `shouldBe` Nothing
        it "refuses a nonadmin source" $
            apply (State [] [("3", 10), ("2", 5)] frame) "2" (TransferCassa "3" 4)
                `shouldBe` Nothing
        it "refuses a nonadmin signer" $
            apply (State [] [("1", 10)] frame) "3" (TransferCassa "1" 4)
                `shouldBe` Nothing
        it "refuses a self transfer" $
            apply (State [] [("1", 10)] frame) "1" (TransferCassa "1" 4)
                `shouldBe` Nothing
        it "preserves conti and the frame" $
            fmap
                (\s -> (conti s, untouched s))
                (apply (State [("1", 3)] [("1", 10)] frame) "2" (TransferCassa "1" 4))
                `shouldBe` Just ([("1", 3)], frame)

donateSpec :: Spec
donateSpec =
    describe "donate" $ do
        it "credits the signer casse and the comune account" $
            apply (State [] [] frame) "1" (Donate 10)
                `shouldBe` Just (State [("comune", 10)] [("1", 10)] frame)
        it "cures a stalled comune account" $
            apply (State [("comune", -5)] [] frame) "1" (Donate 10)
                `shouldBe` Just (State [("comune", 5)] [("1", 10)] frame)
        it "refuses a zero amount" $
            apply (State [] [] frame) "1" (Donate 0) `shouldBe` Nothing
        it "refuses a negative amount" $
            apply (State [] [] frame) "1" (Donate (-10)) `shouldBe` Nothing
        it "refuses a nonadmin signer" $
            apply (State [] [] frame) "3" (Donate 10) `shouldBe` Nothing
        it "appends comune rather than normalizing existing entries" $
            fmap
                conti
                (apply (State [("comune", 2)] [] frame) "1" (Donate 3))
                `shouldBe` Just [("comune", 5)]

identitySpec :: Spec
identitySpec =
    describe "lossless identity" $ do
        it "keeps arbitrary unicode keys distinct without normalization" $ do
            let greek = "Κέλυφος-π"
                greekSpaced = "Κέλυφος-π "
                greekMember = Member greek "greek@trace" Set.empty
                spacedMember = Member greekSpaced "spaced@trace" Set.empty
                uniExtras =
                    Map.fromList
                        [ (greek, greekMember)
                        , (greekSpaced, spacedMember)
                        ]
                uniView = GroupView (Map.union uniExtras (gvMembers adminView))
                uniApply = stepInView uniView
            uniApply (State [] [] frame) "2" (Deposit greek 7)
                `shouldBe` Just (State [(greek, 7)] [("2", 7)] frame)
            uniApply (State [(greek, 7)] [] frame) "2" (Deposit greekSpaced 1)
                `shouldBe` Just (State [(greek, 7), (greekSpaced, 1)] [("2", 1)] frame)
        it "carries large integer amounts exactly" $
            apply (State [] [] frame) "2" (Deposit "1" (10 ^ (40 :: Int)))
                `shouldBe` Just (State [("1", 10 ^ (40 :: Int))] [("2", 10 ^ (40 :: Int))] frame)
        it "refuses a unicode nonmember rather than inferring membership" $
            apply (State [] [] frame) "2" (Deposit "Κέλυφος-π" 7) `shouldBe` Nothing

querySpec :: Spec
querySpec =
    describe "canonical view queries" $ do
        it "memberQuery reads the canonical relation" $ do
            memberQuery realQueries "3" `shouldBe` True
            memberQuery realQueries "9" `shouldBe` False
        it "adminQuery reads roles from the same view" $ do
            adminQuery realQueries "1" `shouldBe` True
            adminQuery realQueries "3" `shouldBe` False
            adminQuery realQueries "9" `shouldBe` False
        it "both queries derive from one GroupView" $
            (memberQuery realQueries "1", adminQuery realQueries "3")
                `shouldBe` (True, False)
