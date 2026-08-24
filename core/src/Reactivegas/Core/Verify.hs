{-# LANGUAGE ImportQualifiedPost #-}

{- |
Module      : Reactivegas.Core.Verify
Description : Envelope verification pipeline (plan §6.3)
Copyright   : (c) 2026 Paolo Veronelli
License     : BSD3
Maintainer  : Paolo Veronelli <paolo.veronelli@gmail.com>
Stability   : experimental

Stateless structural, hash and signature checks combined with a
per-verifier accepted-envelope index used for replay protection and
per-author lamport-chain enforcement. Unknown parents are tolerated:
holding them in a waiting pool is the coordinator's concern.
-}
module Reactivegas.Core.Verify (
    VerifyError (..),
    Verifier (..),
    verifierWith,
    verifyStep,
) where

import Data.ByteString qualified as BS
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word64)

import Reactivegas.Core.Blake3 (hash256)
import Reactivegas.Core.Ed25519
import Reactivegas.Core.Envelope

data VerifyError
    = ErrBodyTooLarge
    | ErrTooManyParents
    | ErrDuplicateParents
    | ErrIdMismatch
    | ErrUnknownAuthor
    | ErrBadSignature
    | ErrReplay
    | ErrLamportRegression
    deriving (Eq, Ord, Show)

{- | Key resolution plus the log of already-accepted envelopes keyed
by id, recording each author and lamport counter.
-}
data Verifier = Verifier
    { vKeyOf :: MemberId -> Maybe PublicKey
    , vLog :: Map EventId (MemberId, Word64)
    }

verifierWith :: (MemberId -> Maybe PublicKey) -> Verifier
verifierWith keyOf = Verifier keyOf Map.empty

{- | Run every check in pipeline order; on success the envelope is
recorded in the returned verifier's log.
-}
verifyStep :: Verifier -> Envelope -> Either VerifyError Verifier
verifyStep v e = do
    checkStructure e
    checkId e
    checkNotReplay v e
    checkSignature v e
    checkLamport v e
    pure v{vLog = Map.insert (envId e) entry (vLog v)}
  where
    h = envHeader e
    entry = (headerAuthor h, headerLamport h)

checkStructure :: Envelope -> Either VerifyError ()
checkStructure e
    | BS.length (envBody e) > maxBodySize = Left ErrBodyTooLarge
    | length parents > maxParents = Left ErrTooManyParents
    | List.length (List.nub parents) /= length parents = Left ErrDuplicateParents
    | otherwise = Right ()
  where
    parents = headerParents (envHeader e)

checkId :: Envelope -> Either VerifyError ()
checkId e =
    if envId e == expected
        then Right ()
        else Left ErrIdMismatch
  where
    expected = EventId (hash256 (headerBytes (envHeader e) <> envBody e))

checkNotReplay :: Verifier -> Envelope -> Either VerifyError ()
checkNotReplay v e =
    if Map.member (envId e) (vLog v)
        then Left ErrReplay
        else Right ()

checkSignature :: Verifier -> Envelope -> Either VerifyError ()
checkSignature v e = do
    pk <- maybe (Left ErrUnknownAuthor) Right (vKeyOf v author)
    case parseSignature (envSig e) of
        Left _ -> Left ErrBadSignature
        Right sig ->
            if verifySignature pk (signableBytes e) sig
                then Right ()
                else Left ErrBadSignature
  where
    author = headerAuthor (envHeader e)

-- A parent constrains the lamport counter only when it is known and
-- was authored by the same member: per-author logs are total orders.
checkLamport :: Verifier -> Envelope -> Either VerifyError ()
checkLamport v e =
    if any regresses sameAuthorParents
        then Left ErrLamportRegression
        else Right ()
  where
    h = envHeader e
    author = headerAuthor h
    sameAuthorParents =
        [ lamport
        | p <- headerParents h
        , Just (a, lamport) <- [Map.lookup p (vLog v)]
        , a == author
        ]
    regresses parentLamport = headerLamport h <= parentLamport
