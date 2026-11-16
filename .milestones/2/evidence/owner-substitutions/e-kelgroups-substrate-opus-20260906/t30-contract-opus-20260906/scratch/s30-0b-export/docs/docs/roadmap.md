# Roadmap

This document maps kelgroups concepts to [KERI](https://keri.one) concepts,
tracks the gaps between the current implementation and full KERI compliance,
and outlines the path forward.

The reference KERI implementation is
[keri-hs](https://github.com/paolino/keri-hs), already available in the
project's nix flake.

## 1. KERI Primer

Five core concepts that kelgroups builds on, each with the keri-hs
type that implements it.

| Concept | What it is | keri-hs module | Type / function |
|---------|-----------|----------------|-----------------|
| **Self-certifying identifier** | The identifier IS the hash of the inception event — no external registry needed | `Keri.Event`, `Keri.Crypto.Digest` | `eventDigest`, `eventPrefix`, `computeSaid` |
| **Signed events** | Every KEL entry carries Ed25519 signatures; the key state determines which keys are valid | `Keri.Kel`, `Keri.KeyState.Verify` | `SignedEvent { event, signatures }`, `verifySignatures` |
| **Digest chain** | Each event includes the hash of its predecessor, forming a tamper-evident chain | `Keri.Event` | `priorDigest` field on `RotationData` / `InteractionData` |
| **SAID** | Self-Addressing Identifier — the event's own digest field is computed over a serialization that contains a placeholder, then replaced with the real hash | `Keri.Crypto.Digest`, `Keri.Crypto.SAID` | `computeSaid`, `saidPlaceholder`, `verifySaid`, `replaceDigest` |
| **Key rotation** | Current signing keys can be rotated by revealing pre-committed next keys; the commitment is a hash of the future key | `Keri.Event.Rotation`, `Keri.KeyState.PreRotation` | `RotationConfig`, `mkRotation`, `commitKey` |

### Supporting concepts

- **CESR encoding** — all cryptographic material (keys, digests, signatures)
  is encoded as self-framing Base64url text with a derivation-code prefix.
  Types: `Ed25519PubKey` (`"D"`), `Blake2bDigest` (`"F"`),
  `Ed25519Sig` (`"0B"`). Module: `Keri.Cesr.*`.

- **Interaction anchors** — interaction events carry arbitrary JSON anchors
  (`ixAnchors :: [Value]`) without changing key state. Used to bind
  external data (proposals, approvals) to the KEL.

- **Signing thresholds** — `stateSigningThreshold :: Int` in `KeyState`
  defines the minimum number of valid signatures required to authorize an
  event. Set at inception, updatable via rotation.

## 2. Concept Mapping

Side-by-side mapping from kelgroups to KERI.

| kelgroups concept | Implementation | KERI equivalent | keri-hs reference |
|---|---|---|---|
| **Group bootstrap** | Passphrase-gated inception event via `mkInception` with first admin's key | **Inception event** with initial keys and signing threshold | `mkInception :: InceptionConfig -> Event` |
| **Member key** | CESR-encoded Ed25519 public key, validated on introduction via `Cesr.decode` | **CESR-encoded Ed25519 public key** | `Cesr.decode :: Text -> Either String Primitive` |
| **Event signer** | Ed25519 signature verified against serialized KERI event bytes | **Indexed signature** verified against key state | `Ed25519.verify`, `serializeEvent` |
| **Proposal** | `Base (Propose proposal)` anchored in KERI interaction event | **Interaction anchor** carrying proposal JSON | `InteractionConfig { ixAnchors = [proposalJson] }` |
| **Approval** | `Base (Approve pid)` anchored in KERI interaction event | **Interaction anchor** carrying approval reference | same, with anchor referencing proposal SAID |
| **Proposal digest** | `computeSaid` over canonical JSON of proposal content | **SAID** of the proposal | `computeSaid` over `serializeEvent` bytes |
| **Event storage** | SQLite with KERI canonical JSON + denormalized chain metadata (prefix, seq_no, digest) | **KEL** with digest chain | `mkInception`, `mkInteraction`, `serializeEvent` |
| **Event serialization** | Canonical JSON via keri-hs `serializeEvent` | **Canonical JSON** with deterministic field order | `serializeEvent :: Event -> ByteString` |
| **Digest chain** | Every event carries `priorDigest`; stale-tip detection rejects outdated submissions | **Hash chain** via `priorDigest` on interaction events | `priorDigest` field on `InteractionData` |
| **Admin majority** | `majority gs = (adminCount gs + 1) \`div\` 2` | **Signing threshold** (`kt` field in events) | `stateSigningThreshold` in `KeyState` |
| **Key state** | Not tracked — members map is flat | `KeyState` with current keys, next commitments, sequence number | `applyEvent :: KeyState -> Event -> Either String KeyState` |

## 3. Current Status

### Completed

| Feature | What it delivers |
|---|---|
| **Ed25519 signatures** | Server verifies every submission against serialized KERI event bytes |
| **CESR key validation** | Member keys must be valid CESR-encoded Ed25519 public keys |
| **SAID proposal digests** | `proposalDigest` uses `computeSaid` over canonical JSON |
| **Hash-chained events** | KERI inception + interaction events with `priorDigest` linking |
| **Canonical JSON** | Events serialized via keri-hs `serializeEvent`, CBOR removed |
| **Stale-tip detection** | 409 rejection when `priorDigest` doesn't match current chain tip |
| **Client-side verification** | Clients receive full KERI events + signatures, can verify the chain independently |
| **Server identity** | Server generates Ed25519 keypair on first start, persisted in `server_identity` table |
| **L1 inception** | Event 0 = server inception via `mkInception`; group identifier = inception SAID |
| **Server key in /info** | `GET /info` returns `serverKey` (CESR public key) and `groupId` (inception prefix) |
| **SAID verification** | `verifySaid` rejects events whose digest doesn't match the recomputed SAID — prevents post-creation field tampering. Implemented in keri-hs (`Keri.Crypto.SAID`), keri-purs, and formalized in keri-lean (`KERI.SAID`). Not yet wired into kelgroups Store. |

### Open Gaps

#### Gap 6: No key state machine (MEDIUM)

`GroupState` has a flat `members :: Map Text Member`. There is no concept
of current vs. next keys, no per-member sequence numbers, no pre-rotation
commitments.

KERI requires `KeyState` tracking `stateKeys`, `stateNextKeys`,
`stateSequenceNumber`, `stateLastDigest`, evolving via `applyEvent`.

#### Gap 7: No KEL-managed identities (HIGH)

Server and admin keys are bare Ed25519 public keys. Signature verification
is a direct `Ed25519.verify` call against the raw key. There is no KERI
identifier (prefix + KEL) for any participant.

In proper KERI, each entity has its own KEL. Signature verification
resolves through `replay → keyState → verifySignatures`. This is the
foundation for key rotation, pre-rotation, and recovery — without it,
a compromised key cannot be rotated out.

keri-hs already provides the full machinery: `Keri.Kel.Append`,
`Keri.Kel.Replay`, `Keri.KeyState`, `Keri.KeyState.Verify`. The gap
is integration, not implementation.

#### Gap 8: No witness receipts (LOW — future)

Current scope: single trusted server. KERI's witness infrastructure
provides independent confirmation of L1 events, out-of-order delivery,
and availability guarantees. Without witnesses, clients must trust the
server's L1 chain on its word alone.

#### Gap 9: No duplicity detection (LOW — future)

No mechanism to detect a compromised server publishing conflicting L1
chains. Requires witnesses (Gap 8) to be meaningful — duplicity is
detected when witnesses disagree on the KEL contents.

#### Gap 10: No out-of-order event escrow (LOW — future)

L2 approvals that arrive before their predecessors are rejected. An
escrow mechanism would park them and apply them once predecessors arrive.

## 4. Architecture: L1/L2 Separation

### Problem

KERI supports multi-sig with signing thresholds (`kt`), but that
mechanism authorizes a single event with multiple signatures collected
at once. kelgroups governance is *asynchronous voting* — admins submit
separate approval events over time, and a decision is enacted when
majority is reached.

Putting the entire voting process on a single chain means L1 is
cluttered with intermediate votes. Every client must replay the full
propose/approve sequence to derive the current state. Enacted decisions
are not directly visible — they are implicit in the fold.

### Design: L1 for outcomes, L2 for voting

**L1 (main KEL)** — the group's primary hash-chained event log.
The first event is the server's inception event (the server is not an
admin — it has no voting power). Subsequent events are outcomes:
enacted decisions and expired proposals. Each enacted event carries the
proposal SAID and the collected admin approval signatures as proof.
The fold over L1 is simple — it applies enacted decisions sequentially.

**L2 (per-proposal KELs)** — one ephemeral KEL per proposal, using the
same code and structure as L1 (signed events, digest chain, SAID). The
L2 inception event anchors the proposal content and timeout metadata.
The only interaction events allowed on an L2 are approvals — each
approval anchors the proposal SAID, signed by the approving admin.
The server rejects any other event type on L2.

Voting order is irrelevant — it doesn't matter whether Alice approved
before or after Bob. Only the set of approvals matters. The L2 KEL
structure is reused because the code already exists, not because
ordering is meaningful.

**Server identifier** — the server has its own KERI identifier (its own
keypair, with inception as L1 event 0). It is not an admin and cannot
approve proposals. It acts as an aggregator: when an L2 reaches
threshold, the server creates a single interaction event on L1
containing the proposal SAID and the collected approval signatures
(extracted from L2 events). The server signs this L1 event, but its
signature is attestation, not a trust assumption — the embedded admin
signatures are the real proof, independently verifiable by any client.

### Replay prevention

Proposals include a client-generated nonce as part of their content.
The proposal SAID is computed over the full content including the nonce,
so identical proposals submitted at different times produce different
SAIDs. Approval signatures are over the proposal SAID, binding them
to a specific proposal instance. The L1 enactment carries the proposal
SAID (which covers the nonce), so any client can verify that approvals
were not replayed from a different proposal — without needing the L2.

On L1, the server generates the inception nonce (establishing its own
identity). On L2, the proposing admin generates the nonce (the proposal
is the admin's act, not the server's).

The server rejects proposals whose SAID matches any existing or past
L2 (a simple set of seen SAIDs). This prevents both accidental
resubmission and intentional replay.

### Lifecycle of a proposal

1. **Propose** — an admin submits a proposal to the server, including
   a client-generated nonce in the proposal content. The server creates
   an L2 KEL: inception event anchoring the proposal content (with
   nonce) and timeout. The proposal's identity = SAID of this inception
   event, which depends on the admin's nonce.
2. **Vote** — admins submit approval events to the L2. Each approval
   is a signed interaction event anchoring the proposal SAID. The
   server verifies the signature against the current key state and
   rejects non-admin signers or duplicate approvals.
3. **Enact** — when the server sees enough approvals on L2 (admin
   majority), it writes an enacted interaction event on L1. This event
   anchors: the proposal SAID and the approval signatures (each as an
   `(admin-key, signature)` pair). This is a compact proof — the full
   L2 chain is not copied to L1.
4. **Expire** — if the timeout elapses before threshold is met, the
   server writes an expired event on L1 referencing the proposal SAID.
5. **Garbage collect** — after resolution (enacted or expired), the L2
   KEL can be discarded. The L1 event is the self-contained permanent
   record.

### Timeout enforcement

Each L2 is created with a timeout (set at proposal creation). The
server enforces it. This is verifiable: if the L2 signatures show
threshold was met before the timeout, and the server wrote "expired"
instead, any client with the L2 data can prove the server lied.

### Invariants

Formalized in Lean 4: predicate definitions in
[`lean/KelGroups/KEL.lean`](https://github.com/paolino/kelgroups/blob/main/lean/KelGroups/KEL.lean),
proofs in
[`lean/KelGroups/KELInvariants.lean`](https://github.com/paolino/kelgroups/blob/main/lean/KelGroups/KELInvariants.lean).

1. **L1 is append-only and hash-chained** (`hashChainValid`). Every
   non-inception event has `priorDigest.isSome` and its sequence
   number equals its predecessor's plus one. The inception event has
   `sequenceNumber = 0` and `priorDigest = none`. The predicate is
   generic — it applies to both L1 and L2 chains.

2. **L1 event 0 is the server's inception** (`l1StartsWithInception`).
   The oldest L1 event has `sequenceNumber = 0`,
   `priorDigest = none`, and payload `inception k` for some key `k`.
   This key is the server's public key (`serverKey`). The server is
   not an admin and has no voting power.

3. **Only the server writes to L1** (`l1ServerOnly`). Every event in
   L1 has `signer = serverKey l1`. If the server key cannot be
   extracted (no valid inception), the predicate is `False`.

4. **Every L1 enacted event is self-contained**
   (`l1EnactedSelfContained`). For enacted events: the proposal SAID
   is non-zero (`said ≠ 0`) and at least one approval proof is
   present (`proofs.length > 0`). Inception and expired events
   satisfy the predicate trivially. Any client can verify the
   enactment from L1 alone, without fetching the L2.

5. **Approval signatures are over the proposal SAID**
   (`l2ApprovalsMatchSAID`). Every approval event in an L2 references
   the same SAID as the L2's proposal. Inception events satisfy the
   predicate trivially. This binds each approval to a specific
   proposal instance.

6. **Proposal SAIDs are unique** (`proposalSAIDsUnique`). The list of
   proposal SAIDs across all L2s has no duplicates (`List.Nodup`).
   Each proposal includes a client-generated nonce; the SAID is
   computed over the full content including the nonce. The server
   rejects proposals with a previously-seen SAID. Proved: adding a
   fresh SAID to a unique list preserves uniqueness.

7. **The proposing admin controls the nonce** (`l2InceptionByAdmin`).
   The L2 inception event is signed by a key that is not the server
   key and is in the current admin list. The server cannot forge the
   nonce without invalidating the admin's signature.

8. **L2 only accepts approvals** (`l2OnlyApprovals`). Inception events
   have `sequenceNumber = 0`; all subsequent events have
   `sequenceNumber > 0` and are approvals. Only inception at
   position 0, only approvals after.

9. **No duplicate approvals** (`l2NoDuplicateApprovals`). The signer
   keys extracted from approval events in an L2 have no duplicates
   (`List.Nodup`). Each admin approves at most once per proposal.

10. **Threshold = admin majority** (`thresholdMet`). Enactment
    requires `approvalCount ≥ majority adminCnt` where
    `majority n = (n + 1) / 2`. Proved: 3 admins / 2 approvals meets
    threshold; 3 admins / 1 approval does not; 1 admin / 1 approval
    meets threshold; bootstrap (0/0) satisfies trivially.

11. **L2 has a timeout** (`l2HasTimeout`). The L2 inception event
    carries a timeout field that is greater than zero. On expiry
    without threshold, the server writes an expired event on L1.
    Verifiable: if L2 data shows threshold was met before timeout, a
    lying server is detectable.

12. **L2 is ephemeral** (`l1EnactmentComplete`). The L1 enacted event
    carries enough approval proofs to meet threshold independently
    (`thresholdMet proofs.length adminCnt`). After resolution, the L2
    can be garbage collected — the L1 event is the self-contained
    permanent record.

### State machine transitions

The invariants above are static predicates. To prove the system
*maintains* them, each operation is modeled as a transition function
with preservation theorems. Definitions in
[`lean/KelGroups/KEL.lean`](https://github.com/paolino/kelgroups/blob/main/lean/KelGroups/KEL.lean),
proofs in
[`lean/KelGroups/KELInvariants.lean`](https://github.com/paolino/kelgroups/blob/main/lean/KelGroups/KELInvariants.lean).

**L2 transitions (per-proposal voting chain):**

- **`mkL2`** — creates an L2 with a single inception event. The
  proposing admin signs. Proved to satisfy: `hashChainValid`,
  `l2NoDuplicateApprovals`, `l2OnlyApprovals`, `l2HasTimeout`
  (given `timeout > 0`), `l2InceptionByAdmin` (given admin ∉ server,
  admin ∈ admins), `l2ApprovalsMatchSAID` (vacuously — no approvals
  yet).

- **`appendApproval`** — appends an approval event referencing the
  proposal SAID. The approving admin signs. Proved:
  `appendApproval_preserves_approvals_match` (if the existing L2
  matches the SAID, the extended L2 still does),
  `appendApproval_fresh_preserves_no_duplicates` (if the signer is
  fresh, no-duplicate-approvals is preserved).

**L1 transitions (main outcome chain):**

- **`mkL1`** — creates an L1 with the server inception event. The
  server signs with its own key. Proved to satisfy: `hashChainValid`,
  `l1StartsWithInception`, `l1ServerOnly`, and all events are
  `l1EnactedSelfContained`.

- **`appendEnacted`** — appends an enacted event carrying the proposal
  SAID and collected approval proofs. Proved:
  `appendEnacted_preserves_self_contained` (given `proposalSAID ≠ 0`
  and `proofs.length > 0`, self-containment holds for all events
  including the new one).

- **`appendExpired`** — appends an expired event carrying the proposal
  SAID. Proved: `appendExpired_preserves_self_contained` (expired
  events satisfy self-containment trivially).

**Combined validity structures:**

- `L1Valid` bundles invariants 1–4: hash chain, inception, server-only,
  self-contained.
- `L2Valid` bundles invariants 5, 7–9, 11: approvals match SAID,
  inception by admin, only approvals, no duplicates, has timeout.

Each transition preserves the fields of its validity structure.
This maps directly to QuickCheck state machine testing: each
preservation theorem becomes a property that generates valid states,
applies the transition, and asserts the invariant holds after.

### Trust model

The server is untrusted. Clients perform all cryptographic operations:
key generation, event signing, SAID computation. The server's role is
strictly verification and aggregation — it checks signatures against
the current key state, monitors L2 KELs for threshold/timeout, and
packages results into L1. It never holds or generates private keys.

The server's own KERI identifier allows it to sign L1 events, but this
signature is not a trust assumption. It is attestation that the server
verified the L2 threshold. The approval signatures embedded in the L1
enactment event are the actual proof — any client can re-verify them
against the admin keys in the current key state.

### What this replaces

The previous options (A: group-as-identifier, B: member KELs + group
log, C: hybrid) are superseded by this design. Option C's starting
point (group KEL with challenge-response auth) is still the foundation
for L1, but the voting mechanism moves to L2 KELs instead of being
interleaved on L1.

Per-member KELs (option B) are planned as Step 10 — each admin gets
their own KERI identifier with KEL-managed key state and pre-rotation.

## 5. Next Steps

### Step 6: Server identifier + L1 inception ✓

Closed **Gap 6** partially. The server is now a proper KERI entity.

- ✓ Server generates Ed25519 keypair on first `openKEL`, persisted in
  `server_identity` SQLite table (singleton row)
- ✓ L1 event 0 = server inception via `mkInception` (server key,
  threshold=1, no anchors)
- ✓ Group identifier = SAID of L1 inception (available as `tipPrefix`
  after `openKEL`)
- ✓ `KELStore` extended with `serverKeyPair` and `serverCesrKey`
- ✓ `GET /info` returns `serverKey` and `groupId`
- ✓ Bootstrap admin intro is now an interaction event (event 1+), not
  inception
- ✓ All 87 tests updated and passing

### Step 7: L2 voting KELs

Implements the L1/L2 separation described in section 4. This is the
largest remaining step — it restructures how proposals and approvals
flow through the system.

- Each proposal creates an L2 KEL (inception = proposal + nonce + timeout)
- Approvals are L2 interaction events (anchor = proposal SAID)
- Server monitors L2 for threshold (admin majority) or timeout
- On threshold: extract approval proofs, write enacted event on L1
- On timeout: write expired event on L1, discard L2
- L2 event type restriction (only approvals after inception)
- SAID uniqueness check (reject duplicate proposal SAIDs)
- Client verifies L1 enacted events from embedded approval signatures
- **Lean:** `L2Valid` predicates become testable, `thresholdMet` exercised
- **Tests:** full voting lifecycle (propose → approve → enact), timeout
  expiry, duplicate rejection, approval signature verification from L1,
  nonce replay prevention

### Step 8: HTTP session authentication

Closes [issue #8](https://github.com/paolino/kelgroups/issues/8).
Currently every request carries a signature. Session auth reduces this
to a one-time challenge-response handshake.

- Server sends random nonce, client signs with Ed25519 key
- Server verifies signature against current group key state
- Session cookie or token for subsequent requests
- WAI middleware layer — transparent to endpoint handlers
- **Tests:** session lifecycle, expired sessions, key rotation
  invalidates sessions

### Step 9: Wire SAID verification into Store

Prerequisite: SAID verification is implemented in keri-hs (done).
Integration step — wire `verifySaid` into `kelgroups`' event ingestion.

- Call `verifySaid` in `Store.appendEvent` before persisting
- Reject events whose SAID doesn't match the recomputed digest
- Mirror in keri-purs client for client-side verification
- **Tests:** tampered events rejected by Store, round-trip with
  valid events unaffected

### Step 10: KEL-managed identities

Closes **Gap 7**. The foundational step for proper KERI crypto — each
participant gets a KERI identifier instead of a bare key.

- Server has its own KEL (inception at startup, maintained in memory +
  SQLite). Signature verification for L1 goes through `replay →
  keyState → verifySignatures`.
- Each admin has their own KEL (provided at introduction, stored per
  member). Signature verification for L2 approvals resolves through
  the admin's key state, not the raw key.
- `GroupState.members` keys become KERI prefixes (SAID of inception)
  instead of raw CESR public keys. Current signing keys are resolved
  via KEL replay.
- The keri-hs machinery already exists: `Keri.Kel.Append`,
  `Keri.Kel.Replay`, `Keri.KeyState`, `Keri.KeyState.Verify`. The
  work is integration and storage.
- **Lean:** formalize that L1/L2 signer resolution goes through key
  state, not raw keys
- **Tests:** member introduction with KEL, signature verification via
  key state, reject events signed by revoked keys

### Step 11: Key rotation for participants

Depends on Step 10. Once identities are KEL-managed, participants can
rotate keys using KERI's pre-rotation mechanism.

- Admins submit rotation events to their own KEL before signing with
  new keys. The server verifies the rotation (pre-rotation commitment
  check) and updates the stored KEL.
- Server can rotate its own key (rotation event on its KEL, all
  subsequent L1 events signed with the new key).
- Compromise recovery: if an admin key is compromised, the admin
  rotates to the pre-committed next key. The compromised key is no
  longer valid for approvals.
- Session invalidation (Step 8): key rotation invalidates existing
  sessions for that identity.
- keri-hs already has: `mkRotation`, `commitKey`, `applyEvent` with
  pre-rotation verification.
- **Tests:** rotation round-trip, signature with old key rejected
  after rotation, pre-rotation commitment verified, compromised key
  recovery scenario

### Step 12: Witness receipts

Closes **Gap 8**. Independent confirmation of L1 events.

- Configurable set of witness keys (set at server inception, updatable
  via rotation with `WitnessConfig`).
- Witnesses receipt L1 events — each receipt is a KERI receipt event
  (`Rct`) signed by the witness key.
- Server collects receipts before considering L1 events confirmed.
- Clients can verify receipts independently — they don't need to trust
  the server's claim that an event was receipted.
- keri-hs has the `Receipt` event type and receipt handling in
  `KeyState` (`receipt_neutral` theorem in keri-lean).
- **Tests:** receipt collection, receipt verification, witness
  threshold enforcement

### Step 13: Duplicity detection

Closes **Gap 9**. Depends on Step 12 (witnesses).

- Detect conflicting KELs when witnesses disagree on event content
  at the same sequence number.
- Flag and quarantine conflicting events.
- Alert mechanism for clients to detect server misbehavior.
- **Lean:** formalize duplicity as two valid but conflicting chains
  sharing a prefix

## 6. Out of Scope

These KERI features are not needed for kelgroups:

- **Weighted thresholds** — kelgroups uses simple admin majority, not
  fractional weighted signing thresholds
- **Delegated identifiers** — no hierarchical identifier delegation;
  admin identities are independent
- **OOBI protocol** — out-of-band introduction for discovering KELs;
  kelgroups has a centralized server that stores all participant KELs
- **Indirect mode networking** — witness-mediated message delivery;
  kelgroups uses direct HTTP
