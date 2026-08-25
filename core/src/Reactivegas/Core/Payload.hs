{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Reactivegas.Core.Payload
Description : Typed domain event payloads with canonical CBOR codec
Copyright   : (c) 2026 Paolo Veronelli
License     : BSD3
Maintainer  : Paolo Veronelli <paolo.veronelli@gmail.com>
Stability   : experimental

Typed bodies carried inside 'Reactivegas.Core.Envelope.Envelope's.
The envelope header keeps the six coarse legacy kinds
('Reactivegas.Core.Envelope.EventKind'); the precise payload variant
is a CBOR tag at the head of the body, so the wire contract frozen by
@vectors/envelope.json@ is unchanged. The reducer rejects any body
whose category disagrees with its header kind.

Encoding is the same deterministic CBOR subset as the envelope:
fixed-width arrays, no maps, no indefinite lengths, amounts as scaled
integers ('EuroCent'), never floats. Decoding accepts only inputs
that are byte-identical to their canonical re-encoding.
-}
module Reactivegas.Core.Payload (
    -- * Identifiers and scalars
    EuroCent (..),
    CampaignId (..),
    CommitmentId (..),
    MovementId (..),
    ProposalId (..),
    ProductId (..),
    Role (..),
    Choice (..),

    -- * Payloads
    Payload (..),
    payloadTag,
    payloadKind,

    -- * Canonical codec
    encodePayload,
    decodePayload,
) where

import Codec.CBOR.Decoding (
    Decoder,
    decodeBytes,
    decodeInt64,
    decodeListLen,
    decodeString,
    decodeWord,
 )
import Codec.CBOR.Encoding (
    Encoding,
    encodeBytes,
    encodeInt64,
    encodeListLen,
    encodeString,
    encodeWord,
 )
import Codec.CBOR.Read qualified as Read
import Codec.CBOR.Write qualified as Write
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Int (Int64)
import Data.Text (Text)
import Data.Word (Word8)

import Reactivegas.Core.Envelope (EventKind (..), MemberId (..))

-- | Amount in euro cents; the only monetary unit on the wire.
newtype EuroCent = EuroCent {unEuroCent :: Int64}
    deriving (Eq, Ord, Show)

newtype CampaignId = CampaignId {unCampaignId :: ByteString}
    deriving (Eq, Ord, Show)

newtype CommitmentId = CommitmentId {unCommitmentId :: ByteString}
    deriving (Eq, Ord, Show)

newtype MovementId = MovementId {unMovementId :: ByteString}
    deriving (Eq, Ord, Show)

newtype ProposalId = ProposalId {unProposalId :: ByteString}
    deriving (Eq, Ord, Show)

newtype ProductId = ProductId {unProductId :: ByteString}
    deriving (Eq, Ord, Show)

-- | Group roles; 'RoleReferente' is only granted at admission bootstrap.
data Role
    = RoleReferente
    | RoleTreasurer
    | RoleCatalogEditor
    deriving (Eq, Ord, Show, Enum, Bounded)

data Choice = VoteYes | VoteNo
    deriving (Eq, Ord, Show, Enum, Bounded)

{- | Every domain fact, grouped exactly like the plan's event table.
The 'MemberId' fields name the /target/ of the event; the acting
member is always the envelope author.
-}
data Payload
    = MemberAdmitted MemberId
    | MemberSuspended MemberId
    | RoleAssigned MemberId Role
    | RoleRevoked MemberId Role
    | ProposalOpened ProposalId
    | BallotCast ProposalId Choice
    | QuorumCertified ProposalId
    | CampaignOpened CampaignId
    | CampaignCatalogSet CampaignId ByteString
    | CampaignClosedForOrders CampaignId
    | CampaignFinalized CampaignId
    | CampaignAborted CampaignId
    | CommitmentProposed CampaignId CommitmentId EuroCent
    | CommitmentAccepted CampaignId CommitmentId
    | CommitmentAmended CampaignId CommitmentId EuroCent
    | CommitmentCanceled CampaignId CommitmentId
    | OrderAllocated CampaignId MemberId EuroCent
    | CreditIssued MovementId MemberId EuroCent
    | DebitIssued MovementId MemberId EuroCent
    | SettlementAgreed MovementId MemberId EuroCent
    | TreasuryTransferred MovementId EuroCent
    | CatalogUpserted ProductId Text EuroCent
    | CatalogItemRemoved ProductId
    deriving (Eq, Show)

payloadTag :: Payload -> Word8
payloadTag p = case p of
    MemberAdmitted{} -> 0
    MemberSuspended{} -> 1
    RoleAssigned{} -> 2
    RoleRevoked{} -> 3
    ProposalOpened{} -> 4
    BallotCast{} -> 5
    QuorumCertified{} -> 6
    CampaignOpened{} -> 7
    CampaignCatalogSet{} -> 8
    CampaignClosedForOrders{} -> 9
    CampaignFinalized{} -> 10
    CampaignAborted{} -> 11
    CommitmentProposed{} -> 12
    CommitmentAccepted{} -> 13
    CommitmentAmended{} -> 14
    CommitmentCanceled{} -> 15
    OrderAllocated{} -> 16
    CreditIssued{} -> 17
    DebitIssued{} -> 18
    SettlementAgreed{} -> 19
    TreasuryTransferred{} -> 20
    CatalogUpserted{} -> 21
    CatalogItemRemoved{} -> 22

-- | Coarse envelope kind each payload belongs to.
payloadKind :: Payload -> EventKind
payloadKind p = case p of
    MemberAdmitted{} -> Anagrafe
    MemberSuspended{} -> Anagrafe
    RoleAssigned{} -> Anagrafe
    RoleRevoked{} -> Anagrafe
    ProposalOpened{} -> Assenso
    BallotCast{} -> Assenso
    QuorumCertified{} -> Assenso
    CampaignOpened{} -> Acquisto
    CampaignCatalogSet{} -> Acquisto
    CampaignClosedForOrders{} -> Acquisto
    CampaignFinalized{} -> Acquisto
    CampaignAborted{} -> Acquisto
    CommitmentProposed{} -> Impegno
    CommitmentAccepted{} -> Impegno
    CommitmentAmended{} -> Impegno
    CommitmentCanceled{} -> Impegno
    OrderAllocated{} -> Impegno
    CreditIssued{} -> Accredito
    DebitIssued{} -> Accredito
    SettlementAgreed{} -> Accredito
    TreasuryTransferred{} -> Accredito
    CatalogUpserted{} -> Voci
    CatalogItemRemoved{} -> Voci

encodePayload :: Payload -> ByteString
encodePayload = Write.toStrictByteString . encodeFields

encodeFields :: Payload -> Encoding
encodeFields p = case p of
    MemberAdmitted t ->
        len2 0 <> bytes (unMemberId t)
    MemberSuspended t ->
        len2 1 <> bytes (unMemberId t)
    RoleAssigned t r ->
        len3 2 <> bytes (unMemberId t) <> roleEnc r
    RoleRevoked t r ->
        len3 3 <> bytes (unMemberId t) <> roleEnc r
    ProposalOpened i ->
        len2 4 <> bytes (unProposalId i)
    BallotCast i c ->
        len3 5 <> bytes (unProposalId i) <> choiceEnc c
    QuorumCertified i ->
        len2 6 <> bytes (unProposalId i)
    CampaignOpened i ->
        len2 7 <> bytes (unCampaignId i)
    CampaignCatalogSet i root ->
        len3 8 <> bytes (unCampaignId i) <> bytes root
    CampaignClosedForOrders i ->
        len2 9 <> bytes (unCampaignId i)
    CampaignFinalized i ->
        len2 10 <> bytes (unCampaignId i)
    CampaignAborted i ->
        len2 11 <> bytes (unCampaignId i)
    CommitmentProposed ci ki c ->
        len4 12 <> bytes (unCampaignId ci) <> bytes (unCommitmentId ki) <> centsEnc c
    CommitmentAccepted ci ki ->
        len3 13 <> bytes (unCampaignId ci) <> bytes (unCommitmentId ki)
    CommitmentAmended ci ki c ->
        len4 14 <> bytes (unCampaignId ci) <> bytes (unCommitmentId ki) <> centsEnc c
    CommitmentCanceled ci ki ->
        len3 15 <> bytes (unCampaignId ci) <> bytes (unCommitmentId ki)
    OrderAllocated ci t c ->
        len4 16 <> bytes (unCampaignId ci) <> bytes (unMemberId t) <> centsEnc c
    CreditIssued m t c ->
        len4 17 <> bytes (unMovementId m) <> bytes (unMemberId t) <> centsEnc c
    DebitIssued m t c ->
        len4 18 <> bytes (unMovementId m) <> bytes (unMemberId t) <> centsEnc c
    SettlementAgreed m t c ->
        len4 19 <> bytes (unMovementId m) <> bytes (unMemberId t) <> centsEnc c
    TreasuryTransferred m c ->
        len3 20 <> bytes (unMovementId m) <> centsEnc c
    CatalogUpserted pid name price ->
        len4 21 <> bytes (unProductId pid) <> encodeString name <> centsEnc price
    CatalogItemRemoved pid ->
        len2 22 <> bytes (unProductId pid)
  where
    len2 w = encodeListLen 2 <> encodeWord w
    len3 w = encodeListLen 3 <> encodeWord w
    len4 w = encodeListLen 4 <> encodeWord w
    bytes = encodeBytes
    roleEnc = encodeWord . fromIntegral . fromEnum
    choiceEnc = encodeWord . fromIntegral . fromEnum
    centsEnc = encodeInt64 . unEuroCent

decodePayload :: ByteString -> Either String Payload
decodePayload raw = do
    (trailing, p) <-
        either
            (Left . show)
            Right
            (Read.deserialiseFromBytes decodeFieldsW (BSL.fromStrict raw))
    if not (BSL.null trailing)
        then Left "trailing bytes after payload"
        else
            if encodePayload p /= raw
                then Left "non-canonical encoding"
                else Right p

decodeFieldsW :: Decoder s Payload
decodeFieldsW =
    decodeListLen >>= \n ->
        decodeWord >>= \w -> case (n, w) of
            (2, 0) -> MemberAdmitted <$> memberIdDec
            (2, 1) -> MemberSuspended <$> memberIdDec
            (3, 2) -> targetDec $ \t -> roleDec (pure . RoleAssigned t)
            (3, 3) -> targetDec $ \t -> roleDec (pure . RoleRevoked t)
            (2, 4) -> ProposalOpened <$> proposalIdDec
            (3, 5) -> proposalIdDec >>= \i -> choiceDec (pure . BallotCast i)
            (2, 6) -> QuorumCertified <$> proposalIdDec
            (2, 7) -> CampaignOpened <$> campaignIdDec
            (3, 8) ->
                CampaignCatalogSet
                    <$> campaignIdDec
                    <*> decodeBytes
            (2, 9) -> CampaignClosedForOrders <$> campaignIdDec
            (2, 10) -> CampaignFinalized <$> campaignIdDec
            (2, 11) -> CampaignAborted <$> campaignIdDec
            (4, 12) ->
                CommitmentProposed
                    <$> campaignIdDec
                    <*> commitmentIdDec
                    <*> centsDec
            (3, 13) ->
                CommitmentAccepted
                    <$> campaignIdDec
                    <*> commitmentIdDec
            (4, 14) ->
                CommitmentAmended
                    <$> campaignIdDec
                    <*> commitmentIdDec
                    <*> centsDec
            (3, 15) ->
                CommitmentCanceled
                    <$> campaignIdDec
                    <*> commitmentIdDec
            (4, 16) ->
                OrderAllocated
                    <$> campaignIdDec
                    <*> memberIdDec
                    <*> centsDec
            (4, 17) ->
                CreditIssued <$> movementIdDec <*> memberIdDec <*> centsDec
            (4, 18) ->
                DebitIssued <$> movementIdDec <*> memberIdDec <*> centsDec
            (4, 19) ->
                SettlementAgreed <$> movementIdDec <*> memberIdDec <*> centsDec
            (3, 20) -> TreasuryTransferred <$> movementIdDec <*> centsDec
            (4, 21) ->
                CatalogUpserted
                    <$> productIdDec
                    <*> decodeString
                    <*> centsDec
            (2, 22) -> CatalogItemRemoved <$> productIdDec
            _ ->
                fail
                    ("unknown payload shape: length " ++ show n ++ " tag " ++ show w)
  where
    targetDec k = memberIdDec >>= k
    roleDec k =
        decodeWord >>= \w ->
            if w <= fromIntegral (fromEnum (maxBound :: Role))
                then k (toEnum (fromIntegral w))
                else fail ("role out of range: " ++ show w)
    choiceDec k =
        decodeWord >>= \w ->
            if w <= fromIntegral (fromEnum (maxBound :: Choice))
                then k (toEnum (fromIntegral w))
                else fail ("choice out of range: " ++ show w)
    memberIdDec = MemberId <$> decodeBytes
    campaignIdDec = CampaignId <$> decodeBytes
    commitmentIdDec = CommitmentId <$> decodeBytes
    movementIdDec = MovementId <$> decodeBytes
    proposalIdDec = ProposalId <$> decodeBytes
    productIdDec = ProductId <$> decodeBytes
    centsDec = EuroCent <$> decodeInt64
