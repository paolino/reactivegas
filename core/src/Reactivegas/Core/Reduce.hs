{-# LANGUAGE ImportQualifiedPost #-}

{- |
Module      : Reactivegas.Core.Reduce
Description : Pure semantic reducer (plan §6.3 step 3)
Copyright   : (c) 2026 Paolo Veronelli
License     : BSD3
Maintainer  : Paolo Veronelli <paolo.veronelli@gmail.com>
Stability   : experimental

The reference domain rules both the coordinator and every client
replay. 'step' assumes structural and signature checks already
happened ('Reactivegas.Core.Verify'); it enforces only semantics:

* authorization — referenti drive campaigns and membership,
  treasurers move money, catalog editors curate products;
* phase guards — commitments live in @OpenForOrders@, allocations in
  @ClosedForOrders@;
* conservation — every movement nets to zero against treasury or
  reserve, and a campaign finalizes only when its allocations equal
  its accepted commitments.

Bootstrap rule: the very first event on an empty projection must be a
self-admission; the founding member holds 'RoleReferente'.
-}
module Reactivegas.Core.Reduce (
    Reject (..),
    step,
    foldLog,
) where

import Data.ByteString (ByteString)
import Data.Foldable (foldl')
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set

import Reactivegas.Core.Envelope (
    Envelope (..),
    EventKind (..),
    Header (..),
    MemberId (..),
 )
import Reactivegas.Core.Payload
import Reactivegas.Core.Projection

data Reject
    = BadPayload String
    | UnknownSigner MemberId
    | SignerSuspended MemberId
    | SelfAdmissionRequired
    | NotReferente
    | NotTreasurer
    | NotCatalogEditor
    | DuplicateMember MemberId
    | UnknownTarget MemberId
    | ProtectedMember MemberId
    | UnassignableRole Role
    | DuplicateCampaign CampaignId
    | UnknownCampaign CampaignId
    | WrongPhase CampaignId Phase
    | DuplicateCommitment CommitmentId
    | UnknownCommitment CommitmentId
    | NotPledger
    | NonPositiveAmount
    | ZeroSettlement
    | DuplicateMovement MovementId
    | UnknownProposal ProposalId
    | DuplicateProposal ProposalId
    | AlreadyCertified ProposalId
    | DuplicateBallot MemberId
    | QuorumNotMet Int
    | AllocationMismatch EuroCent EuroCent
    | UnknownProduct ProductId
    deriving (Eq, Show)

-- | Reduce one envelope against the current projection.
step :: Projection -> Envelope -> Either Reject Projection
step proj env = do
    payload <- decodeBody (headerKind h) (envBody env)
    authorize proj author payload
    applyEvent proj author payload
  where
    h = envHeader env
    author = headerAuthor h

decodeBody :: EventKind -> ByteString -> Either Reject Payload
decodeBody kind raw = do
    p <- either (Left . BadPayload) Right (decodePayload raw)
    if payloadKind p == kind
        then Right p
        else Left (BadPayload "payload category does not match header kind")

-- Authorization gate shared by every payload.
authorize :: Projection -> MemberId -> Payload -> Either Reject ()
authorize proj author p
    | Map.null (projMembers proj) =
        case p of
            MemberAdmitted t | t == author -> Right ()
            _ -> Left SelfAdmissionRequired
    | otherwise = case Map.lookup author (projMembers proj) of
        Nothing -> Left (UnknownSigner author)
        Just m
            | memberStatus m == SuspendedMember -> Left (SignerSuspended author)
            | otherwise -> Right ()

applyEvent :: Projection -> MemberId -> Payload -> Either Reject Projection
applyEvent proj author p = case p of
    -- Identity ------------------------------------------------------
    MemberAdmitted t -> admitMember proj author t
    MemberSuspended t -> suspendMember proj author t
    RoleAssigned t r -> assignRole proj author t r Set.insert
    RoleRevoked t r -> assignRole proj author t r Set.delete
    -- Governance ----------------------------------------------------
    ProposalOpened i ->
        if Map.member i votes
            then Left (DuplicateProposal i)
            else
                Right proj{projGovernance = gov{governanceVotes = Map.insert i mempty votes}}
      where
        votes = governanceVotes (projGovernance proj)
    BallotCast i c -> castBallot proj author i c
    QuorumCertified i -> certifyProposal proj author i
    -- Campaigns -----------------------------------------------------
    CampaignOpened cid ->
        if Map.member cid (projCampaigns proj)
            then Left (DuplicateCampaign cid)
            else
                Right
                    proj
                        { projCampaigns =
                            Map.insert cid emptyCampaignState (projCampaigns proj)
                        }
    CampaignCatalogSet cid root -> do
        c <- requirePhase proj cid CollectingCatalog
        Right $
            bumpCampaign proj cid $
                c
                    { campaignPhase = OpenForOrders
                    , campaignCatalogRoot = Just root
                    }
    CampaignClosedForOrders cid -> do
        c <- requirePhase proj cid OpenForOrders
        Right (bumpCampaign proj cid c{campaignPhase = ClosedForOrders})
    CampaignFinalized cid -> do
        c <- requirePhase proj cid ClosedForOrders
        let allocated =
                EuroCent (sum (map unEuroCent (Map.elems (campaignAllocations c))))
            accepted = acceptedTotalFor proj cid
        if allocated /= accepted
            then Left (AllocationMismatch allocated accepted)
            else Right (bumpCampaign proj cid c{campaignPhase = Finalized})
    CampaignAborted cid -> do
        c <- requireCampaign proj cid
        if campaignPhase c `elem` [Finalized, Aborted]
            then Left (WrongPhase cid (campaignPhase c))
            else Right (bumpCampaign proj cid c{campaignPhase = Aborted})
    -- Commitments ---------------------------------------------------
    CommitmentProposed cid kid cents -> do
        _ <- requirePhase proj cid OpenForOrders
        guardPositive cents
        if Map.member kid (projCommitments proj)
            then Left (DuplicateCommitment kid)
            else
                Right
                    proj
                        { projCommitments =
                            Map.insert
                                kid
                                CommitmentState
                                    { commitmentCampaign = cid
                                    , commitmentPledger = author
                                    , commitmentCents = cents
                                    , commitmentStatus = ProposedCommitment
                                    }
                                (projCommitments proj)
                        }
    CommitmentAccepted cid kid -> do
        requireReferente proj author
        _ <- requirePhase proj cid OpenForOrders
        k <- requireCommitment proj cid kid
        if commitmentStatus k == AcceptedCommitment
            then Right proj
            else
                Right (bumpCommitment proj kid k{commitmentStatus = AcceptedCommitment})
    CommitmentAmended cid kid cents -> do
        k <- requireCommitment proj cid kid
        if commitmentPledger k /= author
            then Left NotPledger
            else do
                _ <- requirePhase proj cid OpenForOrders
                guardPositive cents
                Right (bumpCommitment proj kid k{commitmentCents = cents})
    CommitmentCanceled cid kid -> do
        k <- requireCommitment proj cid kid
        let byPledger =
                commitmentPledger k == author && commitmentStatus k == ProposedCommitment
        if byPledger
            then Right ()
            else requireReferente proj author
        _ <- requirePhase proj cid OpenForOrders
        Right proj{projCommitments = Map.delete kid (projCommitments proj)}
    OrderAllocated cid target cents -> do
        requireReferente proj author
        c <- requirePhase proj cid ClosedForOrders
        _ <- requireActiveMember proj target
        guardPositive cents
        let prior = fromMaybe (EuroCent 0) (Map.lookup target (campaignAllocations c))
        Right $
            bumpCampaign proj cid $
                c
                    { campaignAllocations =
                        Map.insert target (addEuro prior cents) (campaignAllocations c)
                    }
    -- Balances ------------------------------------------------------
    CreditIssued mid target cents ->
        movement proj author mid target (Just cents) $ \p ->
            moveBalance p target cents
    DebitIssued mid target cents ->
        movement proj author mid target (Just cents) $ \pr ->
            moveBalance pr target (negateEuro cents)
    SettlementAgreed mid target cents ->
        movement proj author mid target Nothing $ \pr ->
            if unEuroCent cents == 0
                then Left ZeroSettlement
                else Right (moveBalance pr target cents)
    TreasuryTransferred mid cents ->
        movement proj author mid target' (Just cents) $ \pr ->
            Right
                pr
                    { projTreasury = subEuro (projTreasury pr) cents
                    , projReserve = addEuro (projReserve pr) cents
                    }
      where
        target' = author
    -- Catalog -------------------------------------------------------
    CatalogUpserted pid name price -> do
        requireCatalogEditor proj author
        guardPositive price
        Right proj{projCatalog = CatalogState (Map.insert pid (CatalogItem name price) items)}
      where
        items = catalogItems (projCatalog proj)
    CatalogItemRemoved pid -> do
        requireCatalogEditor proj author
        if Map.notMember pid (catalogItems (projCatalog proj))
            then Left (UnknownProduct pid)
            else
                Right
                    proj{projCatalog = CatalogState (Map.delete pid (catalogItems (projCatalog proj)))}

-- Shared pipeline for balance-affecting payloads.
movement
    :: Projection
    -> MemberId
    -> MovementId
    -> MemberId
    -- ^ Target member whose existence and activity are required;
    -- pass the acting treasurer for treasury-only transfers.
    -> Maybe EuroCent
    -- ^ Positive-only amounts carry 'Just'.
    -> (Projection -> Either Reject Projection)
    -> Either Reject Projection
movement proj author mid target amountLimit apply = do
    requireTreasurer proj author
    _ <- requireActiveMember proj target
    maybe (Right ()) guardPositive amountLimit
    if Set.member mid (projMovements proj)
        then Left (DuplicateMovement mid)
        else apply proj{projMovements = Set.insert mid (projMovements proj)}

admitMember :: Projection -> MemberId -> MemberId -> Either Reject Projection
admitMember proj author t = do
    requireReferente proj author
    case Map.lookup t (projMembers proj) of
        Just m
            | memberStatus m == ActiveMember -> Left (DuplicateMember t)
            -- Re-admission reinstates a suspended member, roles intact.
            | otherwise -> Right (adjustMember proj t (\st -> st{memberStatus = ActiveMember}))
        Nothing -> Right (insertMember proj t emptyMemberState)

suspendMember :: Projection -> MemberId -> MemberId -> Either Reject Projection
suspendMember proj author t = do
    requireReferente proj author
    m <- requireMember proj t
    if Set.member RoleReferente (memberRoles m)
        then Left (ProtectedMember t)
        else Right (adjustMember proj t (\st -> st{memberStatus = SuspendedMember}))

assignRole
    :: Projection
    -> MemberId
    -> MemberId
    -> Role
    -> (Role -> Set.Set Role -> Set.Set Role)
    -> Either Reject Projection
assignRole proj author target role update = do
    requireReferente proj author
    guardAssignableRole role
    _ <- requireActiveMember proj target
    Right (adjustMember proj target bump)
  where
    bump st = st{memberRoles = update role (memberRoles st)}

castBallot
    :: Projection -> MemberId -> ProposalId -> Choice -> Either Reject Projection
castBallot proj voter pid choice = do
    _ <- openVotes proj pid
    let perProposal = governanceVotes (projGovernance proj)
        votes = fromMaybe mempty (Map.lookup pid perProposal)
    if Map.member voter votes
        then Left (DuplicateBallot voter)
        else
            Right
                proj
                    { projGovernance =
                        (projGovernance proj)
                            { governanceVotes = Map.insert pid (Map.insert voter choice votes) perProposal
                            }
                    }

certifyProposal :: Projection -> MemberId -> ProposalId -> Either Reject Projection
certifyProposal proj closer pid = do
    requireReferente proj closer
    votes <- openVotes proj pid
    if Map.size votes >= projQuorum proj
        then
            Right
                proj
                    { projGovernance =
                        (projGovernance proj)
                            { governanceCertified =
                                Set.insert pid (governanceCertified (projGovernance proj))
                            }
                    }
        else Left (QuorumNotMet (projQuorum proj))

openVotes :: Projection -> ProposalId -> Either Reject (Map.Map MemberId Choice)
openVotes proj pid
    | Map.notMember pid allVotes = Left (UnknownProposal pid)
    | Set.member pid certifiedSet = Left (AlreadyCertified pid)
    | otherwise = Right (fromMaybe mempty (Map.lookup pid allVotes))
  where
    allVotes = governanceVotes (projGovernance proj)
    certifiedSet = governanceCertified (projGovernance proj)

-- Helpers -------------------------------------------------------------

requireReferente :: Projection -> MemberId -> Either Reject ()
requireReferente proj who =
    if hasRole proj who RoleReferente then Right () else Left NotReferente

requireTreasurer :: Projection -> MemberId -> Either Reject ()
requireTreasurer proj who =
    if hasRole proj who RoleTreasurer then Right () else Left NotTreasurer

requireCatalogEditor :: Projection -> MemberId -> Either Reject ()
requireCatalogEditor proj who =
    if hasRole proj who RoleCatalogEditor then Right () else Left NotCatalogEditor

hasRole :: Projection -> MemberId -> Role -> Bool
hasRole proj who r =
    maybe False (Set.member r . memberRoles) (Map.lookup who (projMembers proj))

guardAssignableRole :: Role -> Either Reject ()
guardAssignableRole r =
    if r == RoleReferente
        then Left (UnassignableRole r)
        else Right ()

guardPositive :: EuroCent -> Either Reject ()
guardPositive c = if unEuroCent c > 0 then Right () else Left NonPositiveAmount

requireMember :: Projection -> MemberId -> Either Reject MemberState
requireMember proj t =
    maybe (Left (UnknownTarget t)) Right (Map.lookup t (projMembers proj))

-- Suspended members are invisible to domain actions.
requireActiveMember :: Projection -> MemberId -> Either Reject MemberState
requireActiveMember proj t = do
    m <- requireMember proj t
    if memberStatus m == SuspendedMember
        then Left (UnknownTarget t)
        else Right m

insertMember :: Projection -> MemberId -> MemberState -> Projection
insertMember proj t st =
    proj
        { projMembers = Map.insert t st (projMembers proj)
        , projAccounts = Map.insert t (AccountState (EuroCent 0)) (projAccounts proj)
        }

adjustMember :: Projection -> MemberId -> (MemberState -> MemberState) -> Projection
adjustMember proj t f = proj{projMembers = Map.adjust f t (projMembers proj)}

requireCampaign :: Projection -> CampaignId -> Either Reject CampaignState
requireCampaign proj cid =
    maybe (Left (UnknownCampaign cid)) Right (Map.lookup cid (projCampaigns proj))

requirePhase :: Projection -> CampaignId -> Phase -> Either Reject CampaignState
requirePhase proj cid want = do
    c <- requireCampaign proj cid
    if campaignPhase c == want
        then Right c
        else Left (WrongPhase cid (campaignPhase c))

bumpCampaign :: Projection -> CampaignId -> CampaignState -> Projection
bumpCampaign proj cid c = proj{projCampaigns = Map.insert cid c (projCampaigns proj)}

requireCommitment
    :: Projection -> CampaignId -> CommitmentId -> Either Reject CommitmentState
requireCommitment proj cid kid = do
    k <-
        maybe (Left (UnknownCommitment kid)) Right (Map.lookup kid (projCommitments proj))
    if commitmentCampaign k == cid
        then Right k
        else Left (UnknownCommitment kid)

bumpCommitment :: Projection -> CommitmentId -> CommitmentState -> Projection
bumpCommitment proj kid k = proj{projCommitments = Map.insert kid k (projCommitments proj)}

addEuro :: EuroCent -> EuroCent -> EuroCent
addEuro (EuroCent a) (EuroCent b) = EuroCent (a + b)

negateEuro :: EuroCent -> EuroCent
negateEuro (EuroCent a) = EuroCent (negate a)

subEuro :: EuroCent -> EuroCent -> EuroCent
subEuro a b = addEuro a (negateEuro b)

{- | Apply a signed delta to one member's account with the exact
opposite delta on treasury: money is moved, never created.
-}
moveBalance :: Projection -> MemberId -> EuroCent -> Projection
moveBalance proj target d =
    proj
        { projAccounts =
            Map.adjust (\(AccountState b) -> AccountState (addEuro b d)) target accounts
        , projTreasury = subEuro (projTreasury proj) d
        }
  where
    accounts = projAccounts proj

-- Fold a causally valid log into the final projection.
foldLog :: Projection -> [Envelope] -> Projection
foldLog start = foldl' advance start
  where
    advance p e = either (const p) id (step p e)
