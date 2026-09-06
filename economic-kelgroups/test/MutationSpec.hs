{-# LANGUAGE OverloadedStrings #-}

{- | Compiled guard-mutant controls for all four arms. Each control
injects one query fault at the declared Queries boundary, drives the
real production step through the mutated boundary with every other
guard and effect retained, and requires the shared behavioral oracle
that accepts the real transition to reject the mutant. A mutant that
is unreachable or indistinguishable fails the suite here.
-}
module MutationSpec (spec) where

import Data.Maybe (isJust)
import Reactivegas.Economic.Core (
    CustodyEvent (..),
    State (..),
    step,
 )
import Test.Hspec

import MoneyCustodySpec (
    Outcome (..),
    adminFaultQueries,
    frame,
    memberFaultQueries,
    oracle,
    realQueries,
 )

spec :: Spec
spec =
    describe "query-guard fault controls" $ do
        depositControl
        withdrawControl
        transferControl
        donateControl

-- deposit: real refuses the nonmember user; mutating the member query
-- makes the same transition apply, which the shared oracle rejects.
depositControl :: Spec
depositControl =
    it "deposit: member-query fault" $ do
        let start = State [] [] frame
            witness = Deposit "9" 5
            baseline = step realQueries start "2" witness
            mutant = step memberFaultQueries start "2" witness
        baseline `shouldBe` Nothing
        oracle Refused baseline `shouldBe` True
        mutant `shouldSatisfy` isJust
        oracle Refused mutant `shouldBe` False
        putStrLn
            ( "CONTROL arm=deposit guard=memberQuery(user)"
                <> " mutation=memberQuery:=(const True)"
                <> " witness=deposit 5 to nonmember 9 by admin 2"
                <> " baseline=Nothing mutant=applied-just"
                <> " oracle-rejection=yes"
            )

-- withdraw: real refuses the nonmember user; the fault lets the
-- withdrawal debit a real balance, which the shared oracle rejects.
withdrawControl :: Spec
withdrawControl =
    it "withdraw: member-query fault" $ do
        let start = State [("9", 50)] [] frame
            witness = Withdraw "9" 5
            baseline = step realQueries start "2" witness
            mutant = step memberFaultQueries start "2" witness
        baseline `shouldBe` Nothing
        oracle Refused baseline `shouldBe` True
        mutant
            `shouldBe` Just (State [("9", 45)] [("2", -5)] frame)
        oracle Refused mutant `shouldBe` False
        putStrLn
            ( "CONTROL arm=withdraw guard=memberQuery(user)"
                <> " mutation=memberQuery:=(const True)"
                <> " witness=withdraw 5 from funded nonmember 9 by admin 2"
                <> " baseline=Nothing mutant=Just(State [(9,45)] [(2,-5)] frame)"
                <> " oracle-rejection=yes"
            )

-- transferCassa: real moves between two admin cash boxes; mutating the
-- admin query refuses the same transition, which the shared oracle rejects.
transferControl :: Spec
transferControl =
    it "transferCassa: admin-query fault" $ do
        let start = State [] [("1", 100), ("2", 50)] frame
            witness = TransferCassa "1" 30
            baseline = step realQueries start "2" witness
            mutant = step adminFaultQueries start "2" witness
        baseline `shouldBe` Just (State [] [("1", 70), ("2", 80)] frame)
        oracle (Applied [] [("1", 70), ("2", 80)] frame) baseline `shouldBe` True
        mutant `shouldBe` Nothing
        oracle (Applied [] [("1", 70), ("2", 80)] frame) mutant `shouldBe` False
        putStrLn
            ( "CONTROL arm=transferCassa guard=adminQuery(signer)"
                <> " mutation=adminQuery:=(const False)"
                <> " witness=transfer 30 from cash 1 to admin signer 2"
                <> " baseline=Just(State [] [(1,70),(2,80)] frame)"
                <> " mutant=Nothing oracle-rejection=yes"
            )

-- donate: real credits the signer cash box and cures the negative comune
-- account; mutating the admin query refuses, rejected by the same oracle.
donateControl :: Spec
donateControl =
    it "donate: admin-query fault" $ do
        let start = State [("comune", -5)] [] frame
            witness = Donate 10
            baseline = step realQueries start "1" witness
            mutant = step adminFaultQueries start "1" witness
        baseline `shouldBe` Just (State [("comune", 5)] [("1", 10)] frame)
        oracle (Applied [("comune", 5)] [("1", 10)] frame) baseline `shouldBe` True
        mutant `shouldBe` Nothing
        oracle (Applied [("comune", 5)] [("1", 10)] frame) mutant `shouldBe` False
        putStrLn
            ( "CONTROL arm=donate guard=adminQuery(signer)"
                <> " mutation=adminQuery:=(const False)"
                <> " witness=donate 10 by admin 1 over comune -5"
                <> " baseline=Just(State [(comune,5)] [(1,10)] frame)"
                <> " mutant=Nothing oracle-rejection=yes"
            )
