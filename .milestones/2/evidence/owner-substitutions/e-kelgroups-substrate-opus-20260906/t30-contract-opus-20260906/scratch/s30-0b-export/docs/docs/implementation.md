# kelgroups — Implementation Plan

## Nix Setup

- **haskell.nix** with GHC 9.8.4
- [keri-hs](https://github.com/paolino/keri-hs) as flake input (KERI events, CESR encoding, Ed25519 crypto, KEL primitives)
- [keri-purs](https://github.com/paolino/keri-purs) as spago git dependency (PureScript KERI events, CESR, Ed25519, KEL replay)
- [keri-lean](https://github.com/paolino/keri-lean) as lake git dependency (generic KERI types for Lean proofs)
- Dev shell includes: cabal, fourmolu, hlint, hoogle, cabal-fmt, just, lean4, mkdocs, purescript, spago

## Cabal Package

**`kelgroups.cabal`** — library + test suite + executable:

- Library depends on `base`, `containers`, `text`, `bytestring`, `sqlite-simple`, `stm`, `aeson`, `http-types`, `wai`, `keri-hs`
- Executable depends on `kelgroups`, `warp`, `stm`, `text`
- Test suite uses `hspec` + `QuickCheck` + `temporary` + `directory` + `warp` + `http-client` + `http-types` + `aeson` + `async` + `stm`

## Library Modules

| Module | Role |
|---|---|
| `KelGroups.Types` | Core types: `Role`, `Member`, `RoleDef`, `GroupConfig` |
| `KelGroups.Event` | `GroupEvent a`, `BaseEvent`, `Proposal` |
| `KelGroups.State` | `GroupState a`, `adminCount`, `majority`, `isAdmin` |
| `KelGroups.Fold` | KEL fold: `foldGroup`, `applyEvent`, `AppFold` |
| `KelGroups.Validate` | Event validation with `ValidationError` ADT |
| `KelGroups.Bootstrap` | `AuthMode` detection (bootstrap vs normal) |
| `KelGroups.Trivial` | Trivial instance: `a = ()`, no app roles |
| `KelGroups.Store` | SQLite-backed KEL store with KERI events, digest chain, and server identity |
| `KelGroups.Server` | WAI application: routing, KERI event construction, SSE streaming |
| `KelGroups.Server.JSON` | Orphan `ToJSON`/`FromJSON` instances + HTTP types (`Submission`, `AppendResult`, `ServerError`) |

### Type Sketch

```haskell
data Role = Admin | AppRole RoleName

data Member = Member
  { memberKey :: Text
  , memberRoles :: Set Role
  }

data RoleDef a = RoleDef
  { canAdd :: a -> Bool
  , canRemove :: a -> Bool
  }

newtype GroupConfig a = GroupConfig
  { roleDefs :: Map RoleName (RoleDef a)
  }
```

### Events

```haskell
data GroupEvent a = Base BaseEvent | App a

data BaseEvent
  = Propose Proposal
  | Approve ProposalId

data Proposal
  = IntroduceMember Text (Set Role)
  | RemoveMember Text
  | ChangeRoles Text (Set Role)
```

### State

```haskell
data GroupState a = GroupState
  { members :: Map Text Member
  , pendingProposals :: Map ProposalId PendingProposal
  , appFold :: a
  }

data AuthMode = Bootstrap | Normal

authMode :: GroupState a -> AuthMode
authMode gs
  | adminCount gs == 0 = Bootstrap
  | otherwise = Normal
```

### Fold

```haskell
foldGroup
  :: AppFold a -> a -> [(Text, GroupEvent a)] -> GroupState a

type AppFold a = a -> a -> a
```

### Validation

```haskell
validateEvent
  :: GroupConfig a -> GroupState a -> Text -> GroupEvent a
  -> Either ValidationError ()
```

### Store

```haskell
data KELStore a = KELStore
  { storeConn :: Connection
  , stateVar :: TVar (GroupState a)
  , tipVar :: TVar (Maybe ChainTip)
  , lengthVar :: TVar Int
  , serverKeyPair :: KeyPair       -- Ed25519 server identity
  , serverCesrKey :: Text          -- CESR-encoded server public key
  }

data ChainTip = ChainTip
  { tipPrefix :: Text, tipSeqNo :: Int, tipDigest :: Text }

openKEL :: FromJSON a => AppFold a -> a -> FilePath -> IO (KELStore a)
closeKEL :: KELStore a -> IO ()
appendEvent :: ToJSON a => KELStore a -> AppFold a -> Text -> Event -> Text -> GroupEvent a -> IO ()
readState :: KELStore a -> IO (GroupState a)
readEventsFrom :: KELStore a -> Int -> IO [StoredEvent]
kelLength :: KELStore a -> IO Int
chainTip :: KELStore a -> IO (Maybe ChainTip)
```

Events are stored as KERI canonical JSON (via `serializeEvent` from keri-hs) in SQLite alongside the group event anchor, signer key, signature, and denormalized chain metadata (prefix, sequence number, digest). The in-memory `TVar` state is updated incrementally on each append.

On first `openKEL`, the store generates a server Ed25519 keypair (persisted in a singleton `server_identity` table) and creates an L1 inception event (event 0) signed by the server key. The group identifier is the inception event's SAID (available as `tipPrefix` immediately after open). On subsequent opens, the keypair is loaded from the table and all group events are replayed from the `group_event` column to rebuild in-memory state. The chain tip is recovered from the last row's metadata.

### Server

HTTP interface via warp + wai with JSON encoding (aeson).

| Endpoint | Method | Description |
|---|---|---|
| `/condition` | GET | Current group state + auth mode |
| `/events?after=N` | GET | First event after sequence number N |
| `/events` | POST | Submit a `Submission` (signer + signature + priorDigest + event) |
| `/info` | GET | Public admin emails, pending status, server key, group identifier |
| `/stream` | GET | SSE stream — emits `event: new` with `{"sn":N}` on each append |

**SSE mechanism:** Each client gets a `dupTChan` copy of the broadcast channel. Disconnection is handled by warp (thread dies, TChan is GC'd).

**POST flow:** Parse JSON → check auth mode (bootstrap requires passphrase, normal requires member) → construct KERI event (inception for bootstrap, interaction for normal) → verify Ed25519 signature against serialized KERI event → check `priorDigest` matches current chain tip (stale-tip detection) → validate business event → append to store → broadcast sequence number → respond with `AppendResult`.

**Error codes:** 400 (bad JSON), 401 (wrong/missing passphrase), 403 (non-member access), 404 (unknown route or no event), 409 (stale tip — another client appended first), 422 (validation error).

**Executable:** `kelgroups-server <port> <db-path> <passphrase>` — opens a SQLite KEL, creates broadcast channel, runs warp.

## Lean 4 Proofs

Generic KERI types (`Digest`, `SAID`, `Key`, `KELEvent`, `KEL`, `hashChainValid`) are imported from [keri-lean](https://github.com/paolino/keri-lean). kelgroups-specific types and all 9 proof files are local.

Invariants proven in `lean/KelGroups/Invariants.lean`:

| Theorem | Statement |
|---|---|
| `bootstrap_iff_zero_admins` | `authMode gs = bootstrap ↔ adminCount gs = 0` |
| `normal_iff_positive_admins` | `authMode gs = normal ↔ adminCount gs ≠ 0` |
| `empty_is_bootstrap` | `authMode emptyState = bootstrap` |
| `majority_zero` .. `majority_three` | Concrete majority values |
| `majority_le` | `majority n ≤ n` |
| `majority_pos` | `n > 0 → majority n > 0` |
| `remove_all_triggers_bootstrap` | Empty members → bootstrap |
| `admin_member_means_normal` | Admin in members → normal mode |

Transition invariants proven in `lean/KelGroups/TransitionInvariants.lean`:

| Theorem | Statement |
|---|---|
| `enact_introduce_admin_exits_bootstrap` | Introducing admin → adminCount > 0 |
| `enact_introduce_admin_count` | Fresh admin key → adminCount increments by 1 |
| `enact_introduce_nonadmin_count` | Non-admin introduce → adminCount unchanged |
| `enact_preserves_pendingProposals` | enact only touches members |
| `enact_remove_preserves_normal` | adminCount ≥ 2 and remove → adminCount ≥ 1 |

## QuickCheck Properties

Properties and integration tests organized by layer:

| Test module | Scope | Count |
|---|---|---|
| `InvariantsSpec` | Pure state invariants (Lean mirrors) | 11 |
| `TransitionInvariantsSpec` | Pure transition invariants (Lean mirrors) | 8 |
| `FoldInvariantsSpec` | Fold-level properties (app events, approve, changeRoles) | 5 |
| `ValidateSpec` | Validation rule coverage (bootstrap, membership, roles) | 14 |
| `StoreSpec` | Store mechanics (roundtrip, fold consistency, readEventsFrom, kelLength) | 6 |
| `StoreInvariantsSpec` | Lean invariants through KERI event + SQLite roundtrip | 13 |
| `ServerSpec` | HTTP endpoints, SSE, auth, validation errors | 13 |
| `E2ESpec` | End-to-end multi-admin governance flows | 14 |
| `MultiClientSpec` | Concurrent clients, stale-tip rejection, SSE notifications | 3 |

**Total: 87 tests.**

### Store-through DSL

`StoreTestDSL` provides combinators that mirror Lean quantifier patterns:

```haskell
-- Lean: theorem foo (gs : GroupState) : P gs
onReachable :: (GroupState () -> Bool) -> Property

-- Lean: theorem foo (gs : GroupState) (h : Pre gs) : P gs
onReachableWhere :: (GroupState () -> Bool) -> (GroupState () -> Bool) -> Property

-- Lean: theorem foo (gs) (mid) (roles) : P (f gs mid roles)
onReachableWith :: Gen [(Text, GroupEvent ())] -> (GroupState () -> Gen Bool) -> Property
```

The `arbitraryHistory` generator produces valid event histories by tracking state: bootstrap first, then random proposals from live admins. States are reached through the full store pipeline (KERI event construction → SQLite write → reopen → decode → fold).

## CI

- **Build + Test**: `nix develop -c just ci` (format, cabal-fmt, lint, build, test, lean, client)
- **Docs**: MkDocs deployed to GitHub Pages on push to main

## Justfile Recipes

| Recipe | Description |
|---|---|
| `build` | `cabal build all -O0` |
| `test` | `cabal test all -O0 --test-show-details=direct` |
| `format` | `fourmolu -i lib/**/*.hs test/*.hs app/*.hs` |
| `lint` | `hlint lib/` |
| `cabal-fmt` | `cabal-fmt -i kelgroups.cabal` |
| `lean` | `cd lean && lake build` |
| `build-client` | `cd client && npm install && spago build` |
| `bundle-client` | build + bundle PureScript client |
| `test-client` | `cd client && spago -x test.dhall test` |
| `ci` | format + cabal-fmt + lint + build + test + lean + client |
| `docs` | `mkdocs build` |
| `serve` | `cabal run kelgroups-server -O0 -- <port> <db> <pass>` |
| `clean` | cabal clean + lake clean |
