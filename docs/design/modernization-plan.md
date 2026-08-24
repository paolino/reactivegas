# Reactivegas Modernization Plan

**Status:** Draft · **Date:** 2026-08-24 · **Branch:** `feat/modernize-architecture`

This document specifies the target architecture and the migration path for reactivegas:
from a server-centric, text-serialized, RSA-password-keyed Haskell application to a
**local-first, cryptographically sovereign event-sourced system** with a PureScript/Halogen
frontend and a lightweight Haskell coordinator backend.

---

## Table of Contents

1. [Executive Summary](#1-executive-summary)
2. [Current State Analysis (Legacy)](#2-current-state-analysis-legacy)
3. [Domain Analysis: Legacy → Event-Sourced Mapping](#3-domain-analysis-legacy--event-sourced-mapping)
4. [Architectural Vision](#4-architectural-vision)
5. [High-Level System Architecture & Communication Flow](#5-high-level-system-architecture--communication-flow)
6. [Cryptographic Identity & Event Structure](#6-cryptographic-identity--event-structure)
7. [PureScript Frontend Architecture](#7-purescript-frontend-architecture)
8. [Coordinator Backend Architecture (Haskell)](#8-coordinator-backend-architecture-haskell)
9. [Implementation Roadmap](#9-implementation-roadmap)
10. [Test & Verification Gates](#10-test--verification-gates)
11. [Risks & Mitigations](#11-risks--mitigations)
12. [Glossary](#12-glossary)

---

## 1. Executive Summary

Reactivegas manages the economic life of solidarity purchasing groups (GAS): membership,
collective money commitments (*impegni*), orders (*ordini*), purchase sessions (*acquisti*),
balance movements (*accrediti*) and the product catalog (*voci*).

Today all of this runs through a central Haskell server that owns the authoritative state,
signs "group patches" with a responsabile's RSA key derived from a password, and serves an
xhtml UI. Users have no ownership of their own history.

The modernization inverts this trust model:

| Concern | Legacy | Target |
|---|---|---|
| Authoritative state | Server-owned reactor state | Each member's own signed event log, on their device |
| Identity | Username + password-wrapped RSA-1024 key held by server | Ed25519 keypair generated and stored client-side (WebCrypto / IndexedDB) |
| Event format | Italian `Show`/`Read` strings | Canonical binary/JSON envelopes, hash-chained, individually signed |
| Sync | Single `Sincronizzatore` node exchanging patches | Coordinator relays and caches; gossip-compatible protocol |
| UI | Server-rendered xhtml | PureScript + Halogen SPA, offline-first |
| Backend | Monolithic SCGI/xhtml server | Lightweight verification + projection cache + WebSocket relay |

The backend becomes a **coordinator**: it verifies signatures, relays events between
members and *referenti*, and maintains materialized projections (order summaries, balances,
distribution matrices) as an optimization — never as authority.

---

## 2. Current State Analysis (Legacy)

### 2.1 Module inventory

```
Core/            Event-sourcing kernel: Nodo, Programmazione, Inserzione, Patch, Controllo,
                 Aggiornamento, Differenze, Parsing, Dichiarazioni, Programmazione
Eventi/          Domain reactors: Anagrafe, Ordine, Impegno, Acquisto, Accredito,
                 Voci, Sincronizzatore, Servizio, Logger
Applicazioni/    Application assembly: Reactivegas (QS node tree), Persistenza, Sessione,
                 Amministratore, Report, Aggiornamento, Database/{GPatch,Movimenti,Acquisti}
Lib/             Utilities incl. Firmabile (RSA/AES signing), Euro, QInteger, Aspetti (lenses),
                 Prioriti (event priority), STM, SCGI/HTTP servers, TreeLogs
Server/          Boot, Service, Opzioni, Layout (SCGI + xhtml rendering)
Voci/            Structured quantity language for catalog items and orders
                 (Quantità × Unit, Valuta, Boxes, Beni, Compare, UI)
Console/         Haskeline console client
```

### 2.2 Key legacy mechanics

- **Reactor kernel** (`Core/Nodo.hs`, `Core/Programmazione.hs`): a tree of nodes, each owning
  a slice of state (`Aspetti.ParteDi` lens-style substate). External events (`Esterno d`)
  and internal events (`Interno`) are dispatched through `Inserzione` monadic reactions;
  reactions emit log messages and further internal events.
- **Sub-state services** (`Eventi/Servizio.hs`): generic keyed sub-states — each economic
  activity ("causa") lives under a `QInteger` index.
- **Priorities** (`Lib/Prioriti.hs`): every external/internal event carries a priority
  (`R Int`); concurrent patches are merged by deterministic priority ordering — an early
  form of causal conflict resolution.
- **Signing** (`Lib/Firmabile.hs`, `Core/Patch.hs`):
  - RSA-1024 keypair generated from a password-derived SHA-256 seed;
  - private key AES-CBC encrypted with the password, stored **server-side** in the
    `Responsabile = (Utente, (Chiave, Segreto))` record;
  - group updates travel as `Group = (Chiave, Firma, [Patch])` — one signature over a batch
    of patches; individual user events are unsigned.
- **Serialization**: human-readable Italian `show`/`read` strings, e.g.
  `"ordine da \"paolo\" di €12,50 in riferimento a 3"` — fragile parsing, locale-bound,
  no canonical hashing.
- **Persistence**: filesystem value files (`Lib/Valuedfiles.hs`, `Applicazioni/FileSystem.hs`),
  HDBC/sqlite3 for report tables, STM for in-memory coordination.

### 2.3 Legacy pain points motivating the rewrite

1. **No user sovereignty** — keys and full history live on the server; users cannot audit or
   carry their own data.
2. **Weak cryptography** — RSA-1024, password-derived keys, AES-CBC, unauthenticated
   ciphertext, `unsafePerformIO` randomness.
3. **Non-canonical serialization** — `Read`-based parsing is injective-but-unstable; no
   content-addressable event identity, hence no sound gossip.
4. **Server-rendered UI** — no offline capability, session-per-request model
   (`SessioneAcquisto`, `SessioneOrdinante`) instead of durable client state.
5. **Single sync node** — `Sincronizzatore` is a hard single point of failure and trust.

---

## 3. Domain Analysis: Legacy → Event-Sourced Mapping

All domain concepts survive; they are re-expressed as **signed facts owned by actors**
instead of mutations of server state. Naming keeps Italian domain terms (ubiquitous language)
with English type names where clearer.

### 3.1 Concept map

| Legacy module / concept | Legacy representation | Modern event-sourced concept |
|---|---|---|
| `Anagrafe` (`Anagrafe [Utente]`) | server-side user list | `MemberAdmitted { memberId }` — admission events signed by group quorum; membership is a *projection* over these |
| `Responsabili`, `ElezioneResponsabile`, `EsternoAssenso` (`Assenso/Dissenso`, `Permesso`) | election votes handled by server | `ElectionOpened/Closed`, `VoteCast { voterSig }` — votes are individually signed member events; *referente* role is granted by a `RoleGranted` quorum certificate |
| `Ordine Utente Euro Indice` (`Eventi/Ordine.hs`) | order of money toward a "causa" | `MoneyPledged { pledgeId, orderId, amount }` signed by the pledging member; `OrderClosed/OrderFailed` signed by referente |
| `CorrezioneOrdine` | correction patch | `PledgeAmended` — supersede-by-reference (new event referencing prior `eventId`), original retained for auditability |
| `Impegno Utente Euro Indice` (`Impegni{accettati,inattesa}`) | commitment awaiting referente acceptance | `CommitmentProposed` (member-signed) → `CommitmentAccepted/Rejected` (referente-signed); the `inattesa`/`accettati` split becomes a projection |
| `FineImpegno` / `FallimentoImpegno` | closure of collection | `CollectionPhaseClosed { outcome }` referente event |
| `Acquisto`, `AperturaAcquisto`, `FineAcquisto [(Utente,Euro)]` | purchase session lifecycle | `PurchaseSessionOpened/Closed { sessionId, totalsRoot }`; per-member totals are derived from member-signed `DeliveryReceipt` events |
| `Accredito/Addebito/Saldo` (`Conti`, `Saldi`) | balance movements by treasurer | `CreditIssued/DebitIssued/SettlementAgreed { movementId }` signed by treasurer (or dual-signed by member for settlement) |
| `Voci` (`Voce`, `StatoVoci`, `CorreggiVoci`) | shared product catalog | Catalog entries are CRDT-friendly documents: `CatalogUpserted/CatalogRetired` signed by catalog editors; convergence via LWW-element-set keyed on `productId@lamport` |
| `EventoVoci.CorreggiOrdine` (structured `Ordine` rows, `Quantità Denaro/Euro`) | item-level orders | `ItemOrdered { orderId, productId, qty : Quantity, unitPrice }` — quantity/unit algebra ported from `Voci/Quantita.hs` to PureScript |
| `Sincronizzatore` | privileged sync identity | **Eliminated as trust root.** Replaced by any number of coordinators; coordinators hold no signing power over member events |
| `Servizio` keyed sub-states (`QInteger` indices) | numeric "causa" ids | ULID-based aggregate ids (`OrderId`, `SessionId`) — globally unique without coordination, sort-friendly for logs |
| `Prioriti` (`R Int`) | merge ordering of patches | Lamport timestamps + `(lamport, signerId)` total order; domain priorities become validation rules (e.g., closure beats late pledges) rather than merge magic |
| `Group = (Chiave, Firma, [Patch])` | batched responsabile-signed updates | Per-event signatures; batching survives only as transport framing (batch of independently verifiable envelopes) |

### 3.2 Target aggregates

```purescript
-- Shared domain package (PureScript; mirrored in Haskell for server-side validation)

type MemberId  = PublicKeyHex      -- self-sovereign identity
type OrderId   = ULID              -- replaces legacy `Indice`
type SessionId = ULID

data DomainEvent
  = MemberAdmitted     { memberId :: MemberId, admittedBy :: Array SignatureRef }
  | RoleGranted        { memberId :: MemberId, role :: Role, quorumCert :: QuorumCertificate }
  | VoteCast           { electionId :: ElectionId, voter :: MemberId, choice :: Choice }
  | CollectionOpened   { orderId :: OrderId, referente :: MemberId, goal :: Amount }
  | CommitmentProposed { orderId :: OrderId, amount :: Amount }
  | CommitmentAccepted { orderId :: OrderId, member :: MemberId }
  | PledgeAmended      { supersedes :: EventId, newAmount :: Amount }
  | ItemOrdered        { orderId :: OrderId, productId :: ProductId, qty :: Quantity, unitPrice :: Amount }
  | OrderClosed        { orderId :: OrderId, outcome :: Outcome }
  | PurchaseOpened     { sessionId :: SessionId }
  | DeliveryReceipt    { sessionId :: SessionId, lines :: Array ReceiptLine }
  | CreditIssued       { movementId :: MovementId, member :: MemberId, amount :: Amount, memo :: Memo }
  | DebitIssued        { movementId :: MovementId, member :: MemberId, amount :: Amount, memo :: Memo }
  | SettlementAgreed   { movementId :: MovementId, member :: MemberId }   -- dual-signed
  | CatalogUpserted    { product :: ProductDocument }
  | CatalogRetired     { productId :: ProductId }
```

### 3.3 Authorization rules (carried over from legacy semantics)

- Only a *referente* may close/fail a collection (`FineOrdine`/`FallimentoOrdine`,
  `priorityOrdineI` positive priorities in legacy terms).
- Balance-affecting events (`Accredito`, `Addebito`, `Saldo`) require treasurer role;
  `SettlementAgreed` requires both treasurer and member signatures (replacing the legacy
  password-mediated confirmation).
- Membership admission historically required responsabile action; it now requires a
  configurable quorum of existing members (default: 1 referente) expressed as a
  multi-signature certificate embedded in the event.

---

## 4. Architectural Vision

### 4.1 Principles

1. **Sovereign event ownership.** Every member generates an Ed25519 keypair in their browser.
   Every event they author is signed at creation time and appended to *their* canonical log,
   stored locally (IndexedDB). The server can be wiped and rebuilt from members' logs plus
   coordinator caches.
2. **Backend as coordinator & cache.** The Haskell backend verifies, relays, fans out via
   WebSocket, and materializes projections into SQLite for fast multi-user queries. It holds
   no private keys and cannot forge history. Losing it degrades latency, not correctness.
3. **Deterministic reduction.** The same rules reduce events identically in browser
   (TypeScript/PureScript) and server (Haskell). Validation logic is specified once as a
   table-driven spec and property-tested for cross-implementation equivalence.
4. **Offline-first.** All reads come from the local projection; writes append locally and sync
   later. Conflict handling follows causal-order rules with explicit escalation only when
   authorization (not just concurrency) conflicts.
5. **Minimal dependency budget.** Frontend npm surface limited to build tooling (Spago 2 +
   `purs` + esbuild); zero runtime npm packages — VDOM, crypto glue and storage wrappers live
   in the PureScript source tree or FFI files we own.

### 4.2 Trust topology

```
┌────────────┐   signed events    ┌──────────────────┐   verified relay    ┌────────────┐
│  Member A  │ ─────────────────▶ │   Coordinator(s) │ ──────────────────▶ │  Referente │
│ (browser,  │ ◀───────────────── │  (Haskell, state-│ ◀────────────────── │  (member   │
│  own log)  │   projections/ws   │   less wrt trust)│   broadcasts        │   w/ role) │
└────────────┘                    └──────────────────┘                     └────────────┘
      │                                   ▲    │
      │  gossip (future, peer-to-peer)    │    │ SQLite materialized views
      ▼                                   │    ▼
┌────────────┐                            │  Query API (HTTP JSON)
│  Member B  │ ───────────────────────────┘
└────────────┘
```

Coordinators are interchangeable replicas; members may configure several and switch freely.
A future phase allows direct member↔member gossip using the identical envelope format.

---

## 5. High-Level System Architecture & Communication Flow

### 5.1 Components

1. **Client runtime** (PureScript, in-browser):
   - *Keystore*: non-extractable Ed25519 `CryptoKey` (WebCrypto), wrapped backup key in IndexedDB.
   - *Local event store*: append-only object store, one entry per envelope, indexed by
     `(streamId, lamport, hash)`.
   - *Reducer*: pure fold `State → Event → State` producing the local projection.
   - *Sync agent*: WebSocket client with persistent cursor per coordinator; HTTP fallback for
     catch-up backfill.
2. **Coordinator** (Haskell, `coordinator` executable replacing `server`):
   - *Ingest pipeline*: schema decode → structural validation → signature check →
     causal/authorization check → accept/reject receipt.
   - *Relay*: per-group broadcast hub (WebSocket), per-member fanout cursors.
   - *Projection engine*: folds accepted events into SQLite tables (orders, commitments,
     balances, distribution matrix); rebuildable at any time by replay.
   - *Query API*: read-only JSON endpoints over projections.
3. **Shared spec artifacts**: canonical encoding vectors, reducer test vectors (JSON fixtures)
   executed by both implementations in CI.

### 5.2 Protocol

**Transport**

- `wss://<coordinator>/groups/<groupId>/sync` — bidirectional frame stream (length-prefixed
  CBOR frames over WebSocket binary messages).
- `GET /groups/<id>/events?after=<cursor>&limit=N` — idempotent catch-up (cursor = last
  accepted `(lamport, hash)`).
- `POST /groups/<id>/events` — batch submission (used for first contact, offline replay,
  and environments where WS egress is blocked).

**Frame types**

```cbor
Frame = Submit   { envelopes : [Envelope] }
      | Accept   { hashes : [Hash], cursor : Cursor }
      | Reject   { hash : Hash, reason : RejectReason }
      | Announce { envelopes : [Envelope], cursor : Cursor }   -- server push
      | Ping / Pong { cursorHint : Cursor }
```

**Causal ordering**

- Each event carries `lamport` (per-author monotonic counter advanced on observed events)
  and `parents` (hashes of ≤2 prior heads — a small DAG tip set).
- Total order within the coordinator: topological sort with tiebreak `(lamport, signerId, hash)`.
- This directly generalizes the legacy `Prioriti` scheme: domain priorities become
  *validation constraints* (e.g., `OrderClosed` rejects subsequent `CommitmentProposed`
  for the same `orderId`) instead of implicit merge ordering.

**Conflict policy**

| Situation | Resolution |
|---|---|
| Two amendments to same pledge | Both valid; reducer applies highest `(lamport, signerId)`; earlier retained in audit trail |
| Concurrent close vs late commitment | Close wins deterministically; late commitment rejected with `PhaseClosed` |
| Duplicate submission (same hash) | Idempotent accept |
| Forked tips across coordinators | Union merge via parent DAG; reducers are confluent (property-tested) |

**Gossip readiness.** Envelopes are self-verifying and carry parents, so any subset of members
can exchange them directly (Phase 6). The coordinator protocol is deliberately a strict subset
of what a gossip overlay needs: nothing in `Envelope` references coordinator state.

---

## 6. Cryptographic Identity & Event Structure

### 6.1 Identity

- **Algorithm:** Ed25519 (RFC 8032). Browser: WebCrypto `Ed25519` (non-extractable private
  key, `sign` usage only). Coordinator: `ed25519` Haskell package (libsodium bindings).
- **Identity = public key.** `MemberId = base32(lowercase, no-padding) of BLAKE3-256(pubkey)`,
  rendered like `rg1a8f3k…` (bech32-style checksum, similar discipline to SS58/bech32
  addresses). No usernames in the identity layer; display names are profile events.
- **Backup:** at enrollment the client exports an encrypted mnemonic-free backup: raw seed
  XOR-derived keystore (Argon2id passphrase → ChaCha20-Poly1305 blob) downloadable by the
  member. The legacy password-wrapped-key UX is preserved, but the wrap happens client-side
  and the server never sees key material.
- **Rotation:** `KeyRotated { oldKeyId, newKeyId }` signed by the old key and countersigned
  by a referente; verifiers accept chains up to depth 2.

### 6.2 Canonical envelope

```text
Envelope (CBOR-DAG-CBOR subset, deterministic encodings enforced):
  v          : 1                       -- envelope version
  id         : bytes32                 -- BLAKE3-256 over canonical payload bytes
  group      : GroupId                 -- BLAKE3 of group genesis record
  author     : MemberId                -- = hash(pubkey)
  lamport    : uint64
  parents    : [bytes32] (0..2)        -- DAG tips known to the author
  ts         : uint64 (ms since epoch, advisory only)
  kind       : enum(DomainEvent variants)
  body       : canonical CBOR of the event body
  sig        : bytes64                 -- Ed25519 over id || group || header fields || body
```

Canonicalization rules (must match byte-for-byte in PS and HS):

- CBOR core deterministic encoding: map keys sorted by encoded bytes, shortest float/int
  forms, no indefinite lengths.
- Strings NFC-normalized UTF-8; amounts as scaled integers (`EuroCent int64`), never floats.
- `id = BLAKE3-256(canonical(header-without-id-and-sig || body))`.

### 6.3 Validation pipeline (coordinator, mirrors client pre-validation)

1. **Structural:** version supported, required fields present, sizes bounded (body ≤ 16 KiB,
   parents ≤ 2), CBOR strictly canonical (reject non-canonical re-encodings).
2. **Signature:** `author` resolves to pubkey (directly, or via rotation chain); Ed25519 verify.
3. **Semantic:** reducer step against the coordinator's current projection must succeed —
   authorization (role checks, quorum certificates), phase checks (collection open),
   balance invariants (no negative-balance violation unless group policy allows).
4. **Causal:** parents exist or are concurrently pending (hold in a small waiting pool with
   timeout → reject `OrphanTimeout`).

Rejection receipts carry machine-readable reasons so clients can surface precise errors and,
where applicable, auto-repair (e.g., bump lamport, re-parent, resubmit).

### 6.4 Hash chain & auditability

Per-author logs form a hash chain via `parents` + `lamport`. Any third party given an export
of a member's log can independently verify completeness (chain integrity), authenticity
(signatures), and replay the reducer to reproduce every projection — including balances owed
to that member. This is the concrete payoff of sovereignty: **the member's log is evidence**.

---

## 7. PureScript Frontend Architecture

### 7.1 Toolchain & budget

- **Spago 2** workspace (`spago.dhall`/`spago.yaml`), `purs` via Nix
  (**purescript-overlay** pinning exact `purs`/`spago` versions), esbuild for bundling.
- **npm devDependencies only**: `esbuild`, `spago`, `purs-tidy` (formatting). No runtime npm
  libraries. Budget: < 25 MB `node_modules`, install time < 60 s cold, enforced in CI.
- Core libraries: `halogen`, `halogen-subscriptions`, `aff`, `parallel`, `effect`,
  `web-html`, `web-storage`, `foreign-object`, `argonaut`/own CBOR encoder (small FFI),
  `formatters`, `routing`.
- Own thin FFIs (owned, in-tree): `CBOR.purs`+`.js` (canonical encode), `Blake3.purs`+`.js`
  (via WASM build checked into repo), `IndexedDb.purs`, `Crypto.purs` (WebCrypto Ed25519).

### 7.2 Layering

```
src/
  Domain/           Types.hs, Events.hs, Amounts.hs, Quantity.hs   (pure, no Effect)
  Reduce/           Reducer.hs, Rules.hs, Validate.hs             (pure fold + validation)
  Crypto/           Keys.hs, Sign.hs, Hash.hs                      (Effect, WebCrypto/WASM)
  Store/            LogStore.hs (IndexedDB append-only), Backup.hs
  Sync/             Socket.hs, Backfill.hs, Coordinator.hs        (Aff driver)
  App/
    Component/      Shell, Router, Member views, Referente views, Admin views
    Model/          Store.hs (Halogen store), Navigation.hs
  Main.purs
```

Everything below `App/` is effectful composition; everything above is pure and shared-shaped
with the server spec.

### 7.3 State reduction

```purescript
reduce :: Projection → Envelope → V Projection     -- Either RejectMsg
foldLog :: Projection → Array Envelope → Projection
```

- The Halogen app never mutates business state directly. Components raise actions; a single
  `store` component subscribes to (a) local append results and (b) sync-agent announcements,
  folds them through `reduce`, publishes the new `Projection` on a
  `halogen-subscriptions` topic, and components subscribe to slices they need.
- Optimistic intent: user actions create a *pending* envelope immediately reduced into a
  forked projection view (`pending` overlay); rejection from the coordinator flips it to a
  visible error card with retry/edit affordances.

### 7.4 Local event log (IndexedDB)

- DB `reactivegas-v1`, stores: `envelopes` (keyPath `id`, indexes: `[lamport,id]`,
  `kind`, `group`), `meta` (keystore metadata, coordinator cursors), `kv` (projection
  snapshot checkpoints every N events for fast boot).
- Append path: sign (WebCrypto) → write envelope → notify reducer → enqueue to sync agent.
  Writes are transactional; crash between write and network send is safe (resend on boot by
  scanning for unsynced — envelopes absent from coordinator acks).
- Storage estimate guard + export/import (single `.rglog` file = CBOR stream of envelopes)
  for device migration and offline audit.

### 7.5 Offline-first behavior

- Boot: load checkpoint snapshot → tail events after checkpoint → render immediately;
  sync agent connects in background and reconciles.
- All screens operate from local projection; coordinator absence shows a subtle banner, not
  a blocked UI. Actions that require fresh remote knowledge (closing a collection you don't
  own) degrade to queued intents with explanatory states.

### 7.6 Halogen component tree

```
Shell
 ├─ TopBar (identity badge, connectivity, group switcher)
 ├─ Router (hash-based)
 │   ├─ Dashboard        — balances, my active impegni/ordini, next deadlines
 │   ├─ Orders           — list/detail; member: propose commitment, amend;
 │   │                     referente: accept/reject, open/close/fail collection
 │   ├─ Catalog          — voci browsing/search; editors: upsert/retire products
 │   ├─ Purchases        — sessions, delivery receipts, distribution matrix
 │   ├─ Balances         — movements history, settlement proposals
 │   └─ Governance       — elections (assenso/dissenso), roles, members
 └─ Toasts / ErrorCards (rejections, sync gaps)
```

Referente views gate on `myRoles` from the projection; unauthorized actions aren't hidden
mysteriously — they render disabled with the rule that blocks them ("collection already
closed").

---

## 8. Coordinator Backend Architecture (Haskell)

### 8.1 Shape

A single `coordinator` executable (cabal target in the existing package; later its own
package) built from:

```
src-coordinator/
  Coordinator/Main.hs          -- optparse-applicative config, graceful shutdown
  Coordinator/API.hs           -- Warp + WAI: REST + WebSocket endpoints
  Coordinator/Hub.hs           -- STM broadcast hubs per group, per-member cursors
  Coordinator/Ingest.hs        -- verification pipeline (pure core + IO edges)
  Coordinator/Projector.hs     -- event → SQLite projection fold (transactional batches)
  Coordinator/Store.hs         -- sqlite (direct-sqlite/simple-sqlite): events + projections
  Coordinator/Genesis.hs       -- group genesis records, rotation registry
```

Deliberately **no framework stack**: `wai`, `warp`, `websockets`, `stm`,
`sqlite-simple`, `cborg`, `cryptonite`/`crypto-token`, `blake3`, `ed25519`, `aeson` (admin
only), `optparse-applicative`. The xhtml/SCGI/HDBC/AES/RSA legacy dependencies are dropped
from the new target.

### 8.2 Verification pipeline

```
WS/POST bytes
  → decode (cborg, strict canonical check)
  → structurally validate            (pure)
  → resolve author key (+rotation)   (cache: in-memory LRU, backed by SQLite)
  → Ed25519 verify                   (batch-verify queue)
  → semantic validate                (same rules table as client; pure)
  → causal slotting                  (topo insertion into pending DAG)
  → commit: append event row + update projection in ONE SQLite transaction
  → publish to hub (STM broadcast)   → per-subscriber filters
  → emit Accept/Reject frame
```

Throughput target: ≥ 500 events/s verified+projected on a small VPS — far beyond GAS scale
(a group produces maybe hundreds of events/week); the design constraint is correctness and
crash-consistency, not throughput.

### 8.3 Storage layout (SQLite)

```sql
events(        -- immutable, content addressed
  id BLOB PRIMARY KEY,          -- blake3 hash
  group_id BLOB NOT NULL,
  author BLOB NOT NULL,
  lamport INTEGER NOT NULL,
  seq INTEGER UNIQUE NOT NULL,  -- coordinator total order
  body BLOB NOT NULL,           -- canonical envelope bytes
  status TEXT NOT NULL          -- 'accepted' | 'rejected'
);
projections(   -- disposable, rebuildable
  group_id BLOB, view TEXT,     -- 'balances' | 'orders' | 'matrix' | ...
  entity_key BLOB, doc JSON, updated_seq INTEGER,
  PRIMARY KEY (group_id, view, entity_key)
);
cursors( member_key BLOB, group_id BLOB, last_seq INTEGER, PRIMARY KEY(member_key, group_id) );
```

Projections are a **cache**: `rg-admin reproject` wipes and replays `events`. Startup performs
a cheap integrity probe (count + head hash) before serving queries.

Materialized views maintained incrementally per event kind:

- `balances`: fold `CreditIssued/DebitIssued/SettlementAgreed`.
- `orders`: open/closed collections, pledges in `accettati`/`inattesa` equivalents, item totals.
- `matrix`: per-session distribution (member × product quantities) for the referente screen.

### 8.4 Relay & subscriptions

- One STM `TChan` per group hub; subscribers register with a filter predicate
  (all events / own events / events touching my orders) mapped to a compact subscription DSL
  sent in the WS handshake.
- Slow-consumer policy: bounded queues; overflow switches the subscriber to
  "cursor + backfill" mode (client fetches missed range via HTTP), never drops silently.
- Multi-coordinator federation (optional, Phase 6): coordinators mirror each other with the
  same Submit/Accept protocol — they are peers, no hierarchy.

### 8.5 Operations

- Single static binary via Nix flake output (`packages.coordinator`), config file + flags
  (listen addr, db path, max body size, retention).
- Metrics: Prometheus text endpoint (`prometheus` wai middleware) — ingest rate, reject
  reasons histogram, ws connections, projection lag.
- Backups: `sqlite3 .backup` cron + documented export endpoint producing the same `.rglog`
  CBOR stream clients use.

---

## 9. Implementation Roadmap

Phases are sized for reviewability (each ends in a merged, tagged milestone). Legacy server
keeps running untouched until Phase 5 cut-over; the console (`Console/console.hs`) remains
the reference client during Phases 1–3.

### Phase 0 — Foundations & decisions (1–2 weeks)

- Freeze the spec artifacts in `docs/design/`: this document + canonical encoding test
  vectors + reducer fixture format.
- New cabal package layout: `reactivegas-core` (shared types/validation, no IO),
  `reactivegas-coordinator`, keep legacy `reactivegas` library/executable intact.
- CI: add Nix-based job building new packages + running vector tests (extend existing
  `.github/workflows/ci.yaml`).

### Phase 1 — Crypto & event core (Haskell) (2–3 weeks)

- Ed25519 sign/verify, BLAKE3 hashing, canonical CBOR encode/decode (strict mode) in
  `reactivegas-core`; golden vectors committed.
- `Envelope` type + parser/serializer + property tests (roundtrip, malleability rejection:
  any byte change ⇒ different id or failed verify).
- Rotation-chain resolution logic.
- **Gate G1:** cross-checked vectors pass in CI; fuzzing (quickcheck corpus of mutated
  envelopes) yields zero false accepts.

### Phase 2 — Reducer spec & Haskell reference reducer (2–3 weeks)

- Port domain rules (§3) as a pure `step :: Projection → Envelope → Either Reject Projection`
  covering: membership, elections/quorum certificates, collections (open/pledge/accept/
  amend/close/fail), purchases, credits/debits/settlements, catalog LWW.
- Property tests: confluence under reordering (modulo causal validity), replay determinism,
  invariant preservation (balance conservation per closed session).
- Fixture export: reducer fixtures as JSON/CBOR vectors consumed later by PureScript tests.
- **Gate G2:** legacy scenario suite (hand-transcribed from `Eventi/*` behaviors and
  `Console` interactions) reproduces expected projections; all properties green.

### Phase 3 — Coordinator MVP (3–4 weeks)

- Ingest pipeline, SQLite store, WS hub, HTTP catch-up, Accept/Reject receipts.
- Genesis ceremony CLI: create group record (initial referente pubkeys, policy knobs).
- Admin CLI: `reproject`, `export-log`, `stats`.
- **Gate G3:** two synthetic clients (haskell scripts) exchange 10k events through the
  coordinator; kill -9 at random points ⇒ zero lost accepted events, projections always
  reproducible by replay; duplicate/idempotent submissions handled.

### Phase 4 — PureScript client core (4–5 weeks)

- Spago 2 workspace with purescript-overlay flake inputs; npm budget enforcement script in CI.
- `Domain/` + `Reduce/` compiled from spec, tested against Phase-2 fixtures (shared vectors —
  the critical equivalence proof).
- Crypto FFI (WebCrypto Ed25519, BLAKE3 WASM), canonical CBOR encoder with vectors.
- IndexedDB log store + snapshot checkpoints; sync agent (WS + backfill) against the Phase-3
  coordinator; rejection handling and resubmission repair.
- **Gate G4:** property + fixture parity suite green in `spago test`; headless browser test
  (Playwright, dev-only dep) exercises enroll→append→offline→resync roundtrip.

### Phase 5 — Halogen application & migration bridge (5–6 weeks)

- Full component tree (§7.6): member flows first, referente flows second, governance last.
- **Legacy data migration:** one-off importer (Haskell, in `tools/migration/`):
  - parse legacy value-file logs (`Lib/Valuedfiles` formats) and sqlite report tables;
  - synthesize genesis + historical events: membership roster → `MemberAdmitted` (signed by
    a migration key recorded in genesis as trusted issuer), open balances → `CreditIssued`
    batches, current catalog → `CatalogUpserted`, open collections → `CollectionOpened` +
    `CommitmentProposed` reconstruction where attributable;
  - produced log ships with an attestation manifest; members see imported history clearly
    badged as *imported* in the UI.
- Parallel-run period: legacy server stays authoritative for production while pilot group
  exercises the new stack read-write on staging.
- **Gate G5:** pilot group completes a real order cycle (impegni → acquisto → accrediti)
  entirely on the new stack; migrated balances reconcile to the cent against legacy reports;
  Playwright suite covers member + referente happy paths and main rejection paths.

### Phase 6 — Hardening & decentralization options (ongoing)

- Multi-device sync via coordinator (already free) + optional member↔member gossip transport
  (envelope format already supports it).
- Coordinator federation/mirroring; coordinator discovery via DNS + genesis record pinning.
- Key rotation UX, backup restore drills, `.rglog` audit viewer page.
- Deprecation plan: freeze legacy `server` (security-fixes only) once ≥ 2 groups run
  production workloads on the new stack for one full order cycle each; archive xhtml/SCGI
  modules.

### Milestone summary

| Gate | Proves | Blocking? |
|---|---|---|
| G1 | Crypto/encoding correctness, cross-language parity foundation | yes |
| G2 | Domain semantics faithfully modernized | yes |
| G3 | Coordinator durability & protocol soundness | yes |
| G4 | Client-side equivalence with server validation | yes |
| G5 | End-to-end real-world usability + data migration fidelity | cut-over decision |

---

## 10. Test & Verification Gates

- **Golden vectors:** canonical encoding, signature, and reducer fixtures live in
  `/vectors/` (versioned, referenced by hash in this doc's future revisions). Both TS-driven
  PureScript tests and Haskell tests consume the same files.
- **Property suites (Haskell, `reactivegas-core`):** roundtrip, malleability, confluence,
  replay determinism, balance conservation, authorization negative tests.
- **Property suites (PureScript):** same fixture parity + reducer properties via
  `purescript-quickcheck` equivalent (`quickcheck-laws`/`strongcheck` style harness).
- **Integration (CI):** spin coordinator + scripted clients; chaos kill tests; slow-consumer
  backfill tests.
- **Browser E2E (Playwright, dev-dependency only):** enrollment, offline append/resync,
  member & referente journeys, import badging.
- **Performance smoke (not a gate):** 10k-event replay < 30 s coordinator; boot-from-checkpoint
  < 1 s for 100k-event logs.
- **Security review checklist per phase:** no key material leaves client; no non-canonical
  accept path; rejection reasons leak no policy internals beyond necessity; admin surface
  authenticated separately.

---

## 11. Risks & Mitigations

| Risk | Impact | Mitigation |
|---|---|---|
| WebCrypto Ed25519 availability gaps (older Safari/Firefox) | Enrollment failures | Feature-detect; fallback to libsodium WASM path behind same `Crypto/Sign` interface |
| Dual-implementation drift (PS vs HS reducers) | Divergent acceptance ⇒ forks | Single fixture corpus in CI on both sides (G4); any reducer change must regenerate vectors |
| Members losing devices/keys | Locked-out history | Mandatory backup download at enrollment; referente-assisted re-admission flow (`MemberReAdmitted` linking old MemberId) |
| Legacy log parsing ambiguity (`Read` strings) | Migration errors | Importer validates against legacy state snapshots (sqlite report tables as cross-check); unmatched items quarantined into a review report rather than guessed |
| Scope creep in catalog/quantity algebra (`Voci`) | Delayed MVP | Port quantity arithmetic verbatim first (property tests against legacy `Voci/Test.hs` cases), refactor later |
| Coordinator perceived as authority (social regression) | Trust-model erosion | UI always exposes local verification + export; docs emphasize cache status; admin actions logged as ordinary events where possible |

---

## 12. Glossary

| Term | Meaning |
|---|---|
| **GAS / Gruppo di Acquisto Solidale** | Solidarity purchasing group (the user community) |
| **Referente** | Order manager elected by the group; signs phase-transition events |
| **Impegno** | Member's money commitment toward a collective order |
| **Accredito / Addebito** | Credit / debit movement on a member's balance |
| **Assenso / Dissenso** | Consent / dissent vote in group elections |
| **Voci** | Product catalog entries (items with units and prices) |
| **Causa / Indice** | Legacy term/identifier for an economic activity instance; now ULID aggregates |
| **Sincronizzatore** | Legacy privileged sync node; eliminated as a trust root |
| **Envelope** | Canonical, signed, hash-identified event container (§6.2) |
| **Projection** | Materialized view obtained by folding validated events; rebuildable |
| **Coordinator** | Stateless-trust relay + projection cache (the new backend role) |
| **`.rglog`** | Portable CBOR stream export of an event log |

---

*End of document. Maintainers should treat §6.2 canonicalization and the `/vectors/` corpus
as the compatibility contract: changes there require a version bump and a migration note.*
