{-# LANGUAGE ImportQualifiedPost #-}

{- |
Module      : Reactivegas.Core.Envelope
Description : Canonical signed event envelopes (plan §6.2)
Copyright   : (c) 2026 Paolo Veronelli
License     : BSD3
Maintainer  : Paolo Veronelli <paolo.veronelli@gmail.com>
Stability   : experimental

Envelopes are the self-verifying unit of the reactivegas log. The
codec emits a deterministic CBOR subset: fixed-width arrays, no maps,
no indefinite lengths, so identical envelopes always produce
identical bytes across languages. Decoding rejects trailing bytes
and any input whose re-encoding differs byte-for-byte.
-}
module Reactivegas.Core.Envelope (
    GroupId (..),
    MemberId (..),
    EventId (..),
    EventKind (..),
    Header (..),
    Envelope (..),
    maxBodySize,
    maxParents,
    validHeader,
    headerBytes,
    signableBytes,
    sealEnvelope,
    encodeEnvelope,
    decodeEnvelope,
) where

import Codec.CBOR.Decoding (
    Decoder,
    decodeBytes,
    decodeListLen,
    decodeWord,
    decodeWord64,
 )
import Codec.CBOR.Encoding (
    Encoding,
    encodeBytes,
    encodeListLen,
    encodeWord,
    encodeWord64,
 )
import Codec.CBOR.Read qualified as Read
import Codec.CBOR.Write qualified as Write
import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.List qualified as List
import Data.Word (Word64)

import Reactivegas.Core.Blake3 (hash256)
import Reactivegas.Core.Ed25519

newtype GroupId = GroupId {unGroupId :: ByteString}
    deriving (Eq, Show)

-- | Self-sovereign member identity: BLAKE3-256 of its public key.
newtype MemberId = MemberId {unMemberId :: ByteString}
    deriving (Eq, Show)

newtype EventId = EventId {unEventId :: ByteString}
    deriving (Eq, Ord, Show)

data EventKind
    = Anagrafe
    | Impegno
    | Acquisto
    | Accredito
    | Assenso
    | Voci
    deriving (Eq, Ord, Show, Enum, Bounded)

kindToWord :: EventKind -> Word
kindToWord = toEnum . fromEnum

wordToKind :: Word -> Maybe EventKind
wordToKind w
    | w <= fromIntegral (fromEnum (maxBound :: EventKind)) =
        Just (toEnum (fromIntegral w))
    | otherwise = Nothing

data Header = Header
    { headerGroup :: GroupId
    , headerAuthor :: MemberId
    , headerLamport :: Word64
    , headerParents :: [EventId]
    , headerTs :: Word64
    , headerKind :: EventKind
    }
    deriving (Eq, Show)

data Envelope = Envelope
    { envId :: EventId
    , envHeader :: Header
    , envBody :: ByteString
    , envSig :: ByteString
    }
    deriving (Eq, Show)

maxBodySize :: Int
maxBodySize = 16 * 1024

maxParents :: Int
maxParents = 2

-- | Structural bounds: at most two distinct parent tips.
validHeader :: Header -> Bool
validHeader h =
    length (headerParents h) <= maxParents
        && List.length (List.nub (headerParents h)) == length (headerParents h)

{- | Canonical CBOR of the header fields; the envelope carries id and
signature outside this encoding.
-}
headerBytes :: Header -> ByteString
headerBytes = Write.toStrictByteString . encodeFields

encodeFields :: Header -> Encoding
encodeFields h =
    encodeListLen 6
        <> encodeBytes (unGroupId (headerGroup h))
        <> encodeBytes (unMemberId (headerAuthor h))
        <> encodeWord64 (headerLamport h)
        <> encodeListLen (fromIntegral (length (headerParents h)))
        <> mconcat (map (encodeBytes . unEventId) (headerParents h))
        <> encodeWord64 (headerTs h)
        <> encodeWord (kindToWord (headerKind h))

decodeFields :: Decoder s Header
decodeFields = do
    n <- decodeListLen
    if n /= 6
        then fail ("expected 6 header fields, found " ++ show n)
        else do
            group <- GroupId <$> decodeSizedBytes 32
            author <- MemberId <$> decodeSizedBytes 32
            lamport <- decodeWord64
            pn <- decodeListLen
            parents <-
                if pn > maxParents
                    then fail "too many parents"
                    else replicateM pn (EventId <$> decodeSizedBytes 32)
            ts <- decodeWord64
            kw <- decodeWord
            kind <-
                maybe (fail ("unknown event kind: " ++ show kw)) pure (wordToKind kw)
            pure Header{headerGroup = group, headerAuthor = author, headerLamport = lamport, headerParents = parents, headerTs = ts, headerKind = kind}

decodeSizedBytes :: Int -> Decoder s ByteString
decodeSizedBytes n = do
    bs <- decodeBytes
    if BS.length bs /= n
        then fail ("expected " ++ show n ++ " bytes, found " ++ show (BS.length bs))
        else pure bs

{- | Bytes covered by the Ed25519 signature:
id || group || header fields || body.
-}
signableBytes :: Envelope -> ByteString
signableBytes e =
    unEventId (envId e) <> headerBytes (envHeader e) <> envBody e

-- | Compute the id and append the author signature.
sealEnvelope :: SecretKey -> Header -> ByteString -> Either String Envelope
sealEnvelope sk h body
    | not (validHeader h) = Left "invalid header structure"
    | BS.length body > maxBodySize = Left "body exceeds 16 KiB"
    | otherwise =
        let unsigned = headerBytes h <> body
            eid = EventId (hash256 unsigned)
         in Right
                Envelope
                    { envId = eid
                    , envHeader = h
                    , envBody = body
                    , envSig =
                        signatureBytes
                            (signMessage sk (derivePublicKey sk) (unEventId eid <> unsigned))
                    }

encodeEnvelope :: Envelope -> ByteString
encodeEnvelope e =
    Write.toStrictByteString $
        encodeListLen 4
            <> encodeBytes (unEventId (envId e))
            <> encodeFields (envHeader e)
            <> encodeBytes (envBody e)
            <> encodeBytes (envSig e)

{- | Strict decoding: no trailing bytes, fixed field sizes, structural
bounds, and the input must equal its own canonical re-encoding.
-}
decodeEnvelope :: ByteString -> Either String Envelope
decodeEnvelope raw = do
    (trailing, e) <-
        either
            (Left . show)
            Right
            (Read.deserialiseFromBytes decodeEnvelopeW (BSL.fromStrict raw))
    if not (BSL.null trailing)
        then Left "trailing bytes after envelope"
        else
            if BS.length (envBody e) > maxBodySize
                then Left "body exceeds 16 KiB"
                else
                    if not (validHeader (envHeader e))
                        then Left "invalid header structure"
                        else
                            if encodeEnvelope e /= raw
                                then Left "non-canonical encoding"
                                else Right e

decodeEnvelopeW :: Decoder s Envelope
decodeEnvelopeW = do
    n <- decodeListLen
    if n /= 4
        then fail ("expected 4 envelope fields, found " ++ show n)
        else do
            eid <- EventId <$> decodeSizedBytes 32
            h <- decodeFields
            body <- decodeBytes
            sig <- decodeSizedBytes 64
            pure (Envelope eid h body sig)
