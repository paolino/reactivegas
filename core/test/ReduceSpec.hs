{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Tests for the semantic reducer: payload codec roundtrips, replay
determinism, conservation invariants over generated valid logs,
authorization and phase negative tests, plus the legacy @Eventi/*@
order lifecycle transcribed end to end.
-}
module ReduceSpec (
    spec,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Either (isLeft, lefts)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import Test.Hspec (
    Spec,
    describe,
    it,
    shouldBe,
    shouldSatisfy,
 )
import Test.QuickCheck (
    Gen,
    Property,
    chooseInt,
    conjoin,
    counterexample,
    elements,
    forAll,
    (===),
 )

import Reactivegas.Core.Blake3 (hash256)
import Reactivegas.Core.Ed25519
import Reactivegas.Core.Envelope
import Reactivegas.Core.Payload
import Reactivegas.Core.Projection
import Reactivegas.Core.Reduce

-- Fixed identity pool -------------------------------------------------

seedA, seedB, seedC, seedD :: ByteString
seedA = BS.replicate 32 0x11
seedB = BS.replicate 32 0x22
seedC = BS.replicate 32 0x33
seedD = BS.replicate 32 0x44

skOf :: ByteString -> SecretKey
skOf seed = either (error . ("bad seed: " ++)) id (newSecretKey seed)

authorOf :: ByteString -> MemberId
authorOf seed = MemberId (hash256 (publicKeyBytes (derivePublicKey (skOf seed))))

cid :: Int -> CampaignId
cid n = CampaignId (BS.pack [0xC0, fromIntegral n])

kid :: Int -> CommitmentId
kid n = CommitmentId (BS.pack [0xB0, fromIntegral n])

mid :: Int -> MovementId
mid n = MovementId (BS.pack [0xE0, fromIntegral n])

pidIx :: Int -> ProposalId
pidIx n = ProposalId (BS.pack [0xD0, fromIntegral n])

prid :: Int -> ProductId
prid n = ProductId (BS.pack [0xF0, fromIntegral n])

mkEnv :: ByteString -> Word64 -> EventKind -> Payload -> Envelope
mkEnv seed lamport kind pl =
    either (error . ("seal: " ++)) id (sealEnvelope sk header (encodePayload pl))
  where
    sk = skOf seed
    header =
        Header
            { headerGroup = GroupId (BS.replicate 32 0x07)
            , headerAuthor = authorOf seed
            , headerLamport = lamport
            , headerParents = []
            , headerTs = 1724500000000 + lamport
            , headerKind = kind
            }

-- Envelope whose kind is derived from the payload itself.
pay :: ByteString -> Payload -> Envelope
pay seed pl = mkEnv seed 1 (payloadKind pl) pl

negCent :: EuroCent -> EuroCent
negCent (EuroCent c) = EuroCent (negate c)

centI :: Int -> EuroCent
centI = EuroCent . fromIntegral

-- Stepping helpers ----------------------------------------------------

stepAll :: Projection -> [Envelope] -> ([Either Reject Projection], Projection)
stepAll = go []
  where
    go acc p [] = (reverse acc, p)
    go acc p (e : es) = case step p e of
        Left r -> go (Left r : acc) p es
        Right p' -> go (Right p' : acc) p' es

-- Projections after every accepted prefix (stops at first reject).
acceptedPrefixes :: Projection -> [Envelope] -> [Projection]
acceptedPrefixes p0 = go p0
  where
    go _ [] = []
    go p (e : es) = case step p e of
        Left _ -> []
        Right p' -> p' : go p' es

reduceOk :: [Envelope] -> Projection
reduceOk envs = case stepAll emptyProjection envs of
    (results, final) ->
        case lefts results of
            [] -> final
            rejects -> error ("valid script rejected: " ++ show rejects)

-- Shared membership prefix ---------------------------------------------

baseEnvs :: [Envelope]
baseEnvs =
    [ pay seedA (MemberAdmitted (authorOf seedA))
    , pay seedA (MemberAdmitted (authorOf seedB))
    , pay seedA (MemberAdmitted (authorOf seedC))
    , pay seedA (RoleAssigned (authorOf seedB) RoleTreasurer)
    , pay seedA (RoleAssigned (authorOf seedC) RoleCatalogEditor)
    ]

baseProj :: Projection
baseProj = reduceOk baseEnvs

-- Legacy order lifecycle ----------------------------------------------

legacyLifecycle :: [Envelope]
legacyLifecycle =
    baseEnvs
        ++ [ pay seedC (CatalogUpserted (prid 1) "riso" (centI 250))
           , pay seedA (CampaignOpened (cid 1))
           , pay seedA (CampaignCatalogSet (cid 1) "catalog-root-v1")
           , pay seedB (CommitmentProposed (cid 1) (kid 1) (centI 500))
           , pay seedC (CommitmentProposed (cid 1) (kid 2) (centI 300))
           , pay seedA (CommitmentAccepted (cid 1) (kid 1))
           , pay seedA (CommitmentAccepted (cid 1) (kid 2))
           , pay seedB (CommitmentAmended (cid 1) (kid 1) (centI 700))
           , pay seedA (CampaignClosedForOrders (cid 1))
           , pay seedA (OrderAllocated (cid 1) (authorOf seedB) (centI 700))
           , pay seedA (OrderAllocated (cid 1) (authorOf seedC) (centI 300))
           , pay seedA (CampaignFinalized (cid 1))
           , pay seedB (CreditIssued (mid 1) (authorOf seedB) (centI 700))
           , pay seedB (CreditIssued (mid 2) (authorOf seedC) (centI 300))
           , pay seedB (DebitIssued (mid 3) (authorOf seedB) (centI 700))
           , pay seedB (DebitIssued (mid 4) (authorOf seedC) (centI 300))
           ]

-- Valid script generator ------------------------------------------------

stampLamports :: [Envelope] -> [Envelope]
stampLamports =
    zipWith (\i e -> e{envHeader = (envHeader e){headerLamport = i}}) [1 ..]

genScript :: Gen [Envelope]
genScript = do
    govE <- genGovernanceStory
    catE <- genCatalogStory
    campE <- genCampaignStory
    ledgE <- genLedgerStory
    pure (stampLamports (concat [baseEnvs, govE, catE, campE, ledgE]))

genGovernanceStory :: Gen [Envelope]
genGovernanceStory =
    chooseInt (0, 1) >>= \include ->
        if include == 0
            then pure []
            else do
                yes <- elements [VoteYes, VoteNo]
                no <- elements [VoteYes, VoteNo]
                pure
                    [ pay seedA (ProposalOpened (pidIx 1))
                    , pay seedB (BallotCast (pidIx 1) yes)
                    , pay seedC (BallotCast (pidIx 1) no)
                    , pay seedA (QuorumCertified (pidIx 1))
                    ]

genCatalogStory :: Gen [Envelope]
genCatalogStory = do
    n <- chooseInt (0, 3)
    items <-
        mapM
            ( \i -> do
                name <- elements ["riso", "farro", "mele", "vino"]
                price <- centI <$> chooseInt (50, 9999)
                pure (pay seedC (CatalogUpserted (prid i) name price))
            )
            [1 .. n]
    removals <- chooseInt (0, n)
    pure (items ++ map (removeItem seedC) [1 .. removals])
  where
    removeItem editor i = pay editor (CatalogItemRemoved (prid i))

type Pledge = (CommitmentId, ByteString, Maybe EuroCent, EuroCent)

pledgeAmount :: Pledge -> EuroCent
pledgeAmount (_, _, amended, original) = fromMaybe original amended

genCampaignStory :: Gen [Envelope]
genCampaignStory = do
    include <- chooseInt (0, 2)
    if include == 0
        then pure []
        else do
            abortPath <- chooseInt (0, 1)
            kCount <- chooseInt (1, 2)
            pledges <-
                mapM
                    ( \i -> do
                        pledger <- elements [seedB, seedC]
                        original <- centI <$> chooseInt (100, 1000)
                        amended <-
                            chooseInt (0, 1) >>= \doAmend ->
                                if doAmend == 0
                                    then pure Nothing
                                    else Just . centI <$> chooseInt (100, 1000)
                        pure (kid i, pledger, amended, original)
                    )
                    [1 .. kCount]
            let proposeE (k, pl, _, c) = pay pl (CommitmentProposed (cid 9) k c)
                amendE (k, pl, Just newC, _) = [pay pl (CommitmentAmended (cid 9) k newC)]
                amendE _ = []
                acceptE (k, _, _, _) = [pay seedA (CommitmentAccepted (cid 9) k)]
                total = EuroCent (sum (map (unEuroCent . pledgeAmount) pledges))
                opening =
                    [ pay seedA (CampaignOpened (cid 9))
                    , pay seedA (CampaignCatalogSet (cid 9) "root")
                    ]
                closing
                    | abortPath == 1 = [pay seedA (CampaignAborted (cid 9))]
                    | otherwise =
                        [ pay seedA (CampaignClosedForOrders (cid 9))
                        , pay seedA (OrderAllocated (cid 9) (authorOf seedB) total)
                        , pay seedA (CampaignFinalized (cid 9))
                        ]
            pure $
                stampLamports $
                    concat
                        [opening, map proposeE pledges, concatMap amendE pledges, concatMap acceptE pledges, closing]

genLedgerStory :: Gen [Envelope]
genLedgerStory = do
    n <- chooseInt (0, 6)
    concat <$> mapM movement [1 .. n]
  where
    movement i = do
        target <- elements [authorOf seedB, authorOf seedC]
        amt <- centI <$> chooseInt (1, 10000)
        kindIx <- chooseInt (0, 3)
        signed <-
            if kindIx == 2 then elements [amt, negCent amt] else pure amt
        pure $ case kindIx of
            0 -> [pay seedB (CreditIssued (mid i) target amt)]
            1 -> [pay seedB (DebitIssued (mid i) target amt)]
            2 -> [pay seedB (SettlementAgreed (mid i) target signed)]
            _ -> [pay seedB (TreasuryTransferred (mid i) amt)]

-- Properties ------------------------------------------------------------

prop_validScripts :: Property
prop_validScripts =
    forAll genScript $ \envs ->
        let running = emptyProjection : acceptedPrefixes emptyProjection envs
            unaccepted = drop (length running - 1) envs
         in if null unaccepted
                then
                    conjoin
                        [ counterexample
                            ("ledgerTotal nonzero: " ++ show (map ledgerTotal running))
                            (all ((== 0) . ledgerTotal) running)
                        , foldLog emptyProjection envs === last running
                        ]
                else counterexample ("unexpected reject after " ++ show (length running - 1) ++ " accepted steps") False

samplePayloads :: [Payload]
samplePayloads =
    [ MemberAdmitted (authorOf seedB)
    , MemberSuspended (authorOf seedB)
    , RoleAssigned (authorOf seedB) RoleTreasurer
    , RoleRevoked (authorOf seedB) RoleTreasurer
    , ProposalOpened (pidIx 1)
    , BallotCast (pidIx 1) VoteYes
    , QuorumCertified (pidIx 1)
    , CampaignOpened (cid 1)
    , CampaignCatalogSet (cid 1) "root-bytes"
    , CampaignClosedForOrders (cid 1)
    , CampaignFinalized (cid 1)
    , CampaignAborted (cid 1)
    , CommitmentProposed (cid 1) (kid 1) (centI 500)
    , CommitmentAccepted (cid 1) (kid 1)
    , CommitmentAmended (cid 1) (kid 1) (centI 700)
    , CommitmentCanceled (cid 1) (kid 1)
    , OrderAllocated (cid 1) (authorOf seedB) (centI 700)
    , CreditIssued (mid 1) (authorOf seedB) (centI 700)
    , DebitIssued (mid 2) (authorOf seedC) (centI 300)
    , SettlementAgreed (mid 3) (authorOf seedB) (negCent (centI 120))
    , TreasuryTransferred (mid 4) (centI 999)
    , CatalogUpserted (prid 1) "riso" (centI 250)
    , CatalogItemRemoved (prid 1)
    ]

spec :: Spec
spec = do
    describe "payload codec" $ do
        it "roundtrips every payload constructor" $
            mapM_ (\p -> decodePayload (encodePayload p) `shouldBe` Right p) samplePayloads

        it "rejects trailing bytes after the payload" $
            mapM_
                (\p -> shouldSatisfy (decodePayload (encodePayload p <> "\x00")) isLeft)
                samplePayloads

        it "rejects unknown payload tags" $
            shouldSatisfy (decodePayload (BS.pack [0x82, 0xFF, 0x00])) isLeft

        it "rejects bodies whose category disagrees with the header kind" $
            step
                baseProj
                (mkEnv seedA 1 Acquisto (ProposalOpened (pidIx 7)))
                `shouldBe` Left (BadPayload "payload category does not match header kind")

    describe "legacy order lifecycle" $ do
        it "runs open -> pledge -> accept -> close -> allocate -> finalize -> debit" $ do
            let beforeClose = reduceOk (take 13 legacyLifecycle)
            acceptedTotalFor beforeClose (cid 1) `shouldBe` centI 1000
            let p = reduceOk legacyLifecycle
            Map.lookup (cid 1) (projCampaigns p)
                `shouldBe` Just
                    ( CampaignState
                        { campaignPhase = Finalized
                        , campaignCatalogRoot = Just "catalog-root-v1"
                        , campaignAllocations =
                            Map.fromList
                                [ (authorOf seedB, centI 700)
                                , (authorOf seedC, centI 300)
                                ]
                        }
                    )
            fmap commitmentStatus (Map.lookup (kid 1) (projCommitments p)) `shouldBe` Just AcceptedCommitment
            fmap commitmentCents (Map.lookup (kid 1) (projCommitments p)) `shouldBe` Just (centI 700)
            fmap commitmentCents (Map.lookup (kid 2) (projCommitments p)) `shouldBe` Just (centI 300)
            fmap accountBalance (Map.lookup (authorOf seedB) (projAccounts p)) `shouldBe` Just (EuroCent 0)
            fmap accountBalance (Map.lookup (authorOf seedC) (projAccounts p)) `shouldBe` Just (EuroCent 0)
            projTreasury p `shouldBe` EuroCent 0
            ledgerTotal p `shouldBe` 0

        it "rejects finalization while allocations miss the accepted total" $ do
            let short =
                    take 14 legacyLifecycle
                        ++ [ pay seedA (OrderAllocated (cid 1) (authorOf seedB) (centI 400))
                           , pay seedA (CampaignFinalized (cid 1))
                           ]
            case stepAll emptyProjection short of
                (results, _) ->
                    last results `shouldBe` Left (AllocationMismatch (centI 400) (centI 1000))

    describe "authorization" $ do
        it "rejects events signed by unknown members" $
            step
                baseProj
                (pay seedD (CommitmentProposed (cid 1) (kid 1) (centI 100)))
                `shouldBe` Left (UnknownSigner (authorOf seedD))

        it "requires bootstrap self-admission on an empty projection" $
            step emptyProjection (pay seedA (MemberAdmitted (authorOf seedB)))
                `shouldBe` Left SelfAdmissionRequired

        it "forbids admission by non-referenti" $
            step
                baseProj
                (pay seedB (MemberAdmitted (authorOf seedD)))
                `shouldBe` Left NotReferente

        it "forbids campaign closure by non-referenti" $ do
            let p = reduceOk (baseEnvs ++ [pay seedA (CampaignOpened (cid 1)), pay seedA (CampaignCatalogSet (cid 1) "root")])
            step p (pay seedB (CampaignClosedForOrders (cid 1))) `shouldBe` Left NotReferente

        it "forbids credit issuance by non-treasurers" $
            step
                baseProj
                (pay seedC (CreditIssued (mid 1) (authorOf seedB) (centI 100)))
                `shouldBe` Left NotTreasurer

        it "rejects role assignment of the referente role" $
            step baseProj (pay seedA (RoleAssigned (authorOf seedB) RoleReferente))
                `shouldBe` Left (UnassignableRole RoleReferente)

        it "protects referenti from suspension" $
            step baseProj (pay seedA (MemberSuspended (authorOf seedA)))
                `shouldBe` Left (ProtectedMember (authorOf seedA))

        it "blocks suspended members from voting" $ do
            let p =
                    reduceOk
                        ( baseEnvs
                            ++ [ pay seedA (MemberSuspended (authorOf seedC))
                               , pay seedA (ProposalOpened (pidIx 1))
                               ]
                        )
            step p (pay seedC (BallotCast (pidIx 1) VoteYes))
                `shouldBe` Left (SignerSuspended (authorOf seedC))

    describe "phase guards" $ do
        it "rejects commitments outside OpenForOrders" $ do
            let p = reduceOk (baseEnvs ++ [pay seedA (CampaignOpened (cid 1))])
            step p (pay seedB (CommitmentProposed (cid 1) (kid 1) (centI 100)))
                `shouldBe` Left (WrongPhase (cid 1) CollectingCatalog)

        it "rejects allocations before the order phase closes" $ do
            let p = reduceOk (baseEnvs ++ [pay seedA (CampaignOpened (cid 1)), pay seedA (CampaignCatalogSet (cid 1) "root")])
            step p (pay seedA (OrderAllocated (cid 1) (authorOf seedB) (centI 100)))
                `shouldBe` Left (WrongPhase (cid 1) OpenForOrders)

        it "rejects finalization from OpenForOrders" $ do
            let p = reduceOk (baseEnvs ++ [pay seedA (CampaignOpened (cid 1)), pay seedA (CampaignCatalogSet (cid 1) "root")])
            step p (pay seedA (CampaignFinalized (cid 1)))
                `shouldBe` Left (WrongPhase (cid 1) OpenForOrders)

        it "rejects amendments by non-pleders" $ do
            let p =
                    reduceOk
                        ( baseEnvs
                            ++ [ pay seedA (CampaignOpened (cid 1))
                               , pay seedA (CampaignCatalogSet (cid 1) "root")
                               , pay seedC (CommitmentProposed (cid 1) (kid 1) (centI 100))
                               ]
                        )
            step p (pay seedB (CommitmentAmended (cid 1) (kid 1) (centI 200)))
                `shouldBe` Left NotPledger

        it "rejects acceptance through a mismatched campaign id" $ do
            let p =
                    reduceOk
                        ( baseEnvs
                            ++ [ pay seedA (CampaignOpened (cid 1))
                               , pay seedA (CampaignCatalogSet (cid 1) "root")
                               , pay seedC (CommitmentProposed (cid 1) (kid 1) (centI 100))
                               ]
                        )
            step p (pay seedA (CommitmentAccepted (cid 2) (kid 1)))
                `shouldBe` Left (UnknownCommitment (kid 1))

    describe "accounting" $ do
        it "rejects reused movement ids" $ do
            let p =
                    reduceOk
                        ( baseEnvs
                            ++ [pay seedB (CreditIssued (mid 1) (authorOf seedB) (centI 100))]
                        )
            step p (pay seedB (CreditIssued (mid 1) (authorOf seedC) (centI 100)))
                `shouldBe` Left (DuplicateMovement (mid 1))

        it "rejects zero settlements and non-positive amounts" $ do
            step baseProj (pay seedB (SettlementAgreed (mid 1) (authorOf seedB) (EuroCent 0)))
                `shouldBe` Left ZeroSettlement
            step baseProj (pay seedB (CreditIssued (mid 1) (authorOf seedB) (EuroCent 0)))
                `shouldBe` Left NonPositiveAmount
            step baseProj (pay seedB (DebitIssued (mid 1) (authorOf seedB) (negCent (centI 5))))
                `shouldBe` Left NonPositiveAmount

    describe "governance" $ do
        it "rejects certification below quorum" $ do
            let p = reduceOk (baseEnvs ++ [pay seedA (ProposalOpened (pidIx 1))])
            step p (pay seedA (QuorumCertified (pidIx 1))) `shouldBe` Left (QuorumNotMet 1)

        it "rejects double ballots and double certification" $ do
            let p =
                    reduceOk
                        ( baseEnvs
                            ++ [ pay seedA (ProposalOpened (pidIx 1))
                               , pay seedB (BallotCast (pidIx 1) VoteYes)
                               ]
                        )
            step p (pay seedB (BallotCast (pidIx 1) VoteNo)) `shouldBe` Left (DuplicateBallot (authorOf seedB))
            let certified =
                    reduceOk
                        ( baseEnvs
                            ++ [ pay seedA (ProposalOpened (pidIx 1))
                               , pay seedB (BallotCast (pidIx 1) VoteYes)
                               , pay seedA (QuorumCertified (pidIx 1))
                               ]
                        )
            step certified (pay seedA (QuorumCertified (pidIx 1)))
                `shouldBe` Left (AlreadyCertified (pidIx 1))

    describe "catalog" $ do
        it "rejects removal of unknown products" $
            step baseProj (pay seedC (CatalogItemRemoved (prid 9)))
                `shouldBe` Left (UnknownProduct (prid 9))

        it "rejects catalog writes by non-editors" $
            step baseProj (pay seedB (CatalogUpserted (prid 1) "riso" (centI 250)))
                `shouldBe` Left NotCatalogEditor

    describe "properties" $ do
        it "generated valid scripts are fully accepted and conserve the ledger" $
            prop_validScripts
