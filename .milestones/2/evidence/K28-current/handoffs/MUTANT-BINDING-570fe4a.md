# MUTANT-BINDING-570fe4a — S28-1 splice bindings to actual RED bytes (r5/v4)

Ticket owner `t28-app-api`. RED commit `570fe4a68f510fad3c9912ea59c1e492f3e11740`
(parent `368b596…`, branch `feat/28-generalize-app-api`, tree clean).
Gate v4 (`GATE_VERSION="G28-1 v4 (r5)"`, hash/HEAD in `NOTE GATE-FROZEN-r5`).
Rule (A-001 §3): property + operation + failure class frozen in r5; locations
bound HERE to actual artifacts, versioned/hashed BEFORE any GREEN mutation
run. Same-requirement splice corrections need no desk round (granted) but
re-bind (new binding version + hashes) before execution. Production locations
(H1–H5) are TBB — production does not exist at RED; they bind at GREEN
submission (BINDING-GREEN) via ANCHOR-ATTEST + leg-5 preconditions (fail
closed). This record covers everything bindable on RED bytes.

## RED files (hashes at RED commit)

- `test/S28DemoApp.hs` blob `17a85fc3…` (164 lines)
- `test/S28AppApiSpec.hs` blob `1f41e119…` (389 lines)
- `kelgroups.cabal` blob `14aa61d6…` (+2 wiring lines)
- `test/Main.hs` blob `ae48c5bc…` (+2 wiring lines)
- RED logs verified: `RED-base-gate.log` `2b64d6bf…`, `RED-commit.log`
  `d5b0a1e2…` (hashes match commit-owner journal). RELIANCE.md read
  (5 rows, 0 enforced; ratify/discard at acceptance).

## M1 — BOUND to RED bytes (applicability proven; kill executes GREEN leg-5 M1)

Preconditions VERIFIED against actual RED `test/S28DemoApp.hs` (quoted):
(a) `^data DemoState` line 55 ✓, `^data DemoEvent` line 62 ✓ (fourmolu-stable
name lines; arms/derivings on following lines); (b) flattened match
(`tr '\n' ' '`) yields `demoIntegration     :: Integration DemoState
DemoEvent DemoProposal DemoError` (actual 144–145 split; pattern
`demoIntegration +:: +Integration …` MATCHES — D3 fix proven on real bytes);
(c) `applyIntegratedEvent` (import :46 + use :164 `demoStep`) and
`emptyState` (import :48 + use :156) mentioned ✓; (d) freshness:
`_m1_boundarySeparates` absent repo-wide ✓. Splice (v4 text, boundary use
`_m1_boundarySeparates` applying `applyIntegratedEvent` to
`emptyState (DemoState 0 [])` — the exact constructor shape exists at :156)
REVIEWED syntactically valid; fourmolu-split-safe (appended at EOF, no line
limit in Haskell). Kill (unification error naming DemoEvent+DemoState+
applyIntegratedEvent, parse excluded) EXECUTES in GREEN leg-5 M1 (needs
compilable production context — impossible-clean on RED bytes: 8 missing
production imports would mask unification; documented, not silently skipped).

## M4 — PROGRAM proven sound (synthetic renamed block); anchor TBB

v4 awk (indent-derived insert) dry-run on synthetic fixture
(`data BaseMutation = RemoveMemberVoted Text | ChangeRolesVoted …`)
produces byte-exact expected hunk (`-- MUTANT-M4` marker + `|
AdmitMemberVoted Text Text (Set Role)` arm, block intact). Production anchor
(`^data BaseMutation` + `ChangeRolesVoted` arm) TBB — no BaseMutation exists
pre-GREEN. Freshness: `AdmitMemberVoted` absent repo-wide ✓. Kill (build
exhaustiveness error) executes GREEN leg-5 M4. D1 clash lines quoted for the
record: historical `Proposal` arms `Event.hs:49/57/61`; full-scan: all other
frozen-new names 0 hits in target files (only the two arms collide).

## M6 — splices proven on RED Store.hs COPY; type-proof deferred fail-closed

Export insert (`^    , closeKEL$`, Store.hs:26 actual) + STM-import insert
(`^    , writeTVar$` actual) dry-run on `/tmp` copy of RED Store.hs: hunks
land exactly (quoted in freeze notes). Backdoor text (arity-correct
`TVar (GroupState s) -> s -> STM ()`, STM body, no IO) fourmolu-PARSES in
situ (`--ghc-opt=-XImportQualifiedPost`; exit-1 was formatting drift only,
no parse error; pristine-copy control fails identically without the flag —
environmental). ghc-no-code single-module probe (p5): ENVIRONMENT-LIMITED
(keri-hs deps unresolvable outside cabal; failed at Fold/Validate imports
before reaching the copy) — honest negative on METHOD, not subject; type
proof vests in GREEN leg-5 M6 (fail-closed: build error → INCONCLUSIVE, fix
under granted authority). Rewire anchor (H5 success write) TBB.
Freshness: `unsafeSetAppStateSTM` absent repo-wide ✓.

## M2/M3/M5 — v3 operations stand; anchors TBB (production absent)

H1/H2+H2b/H3 mandates stand (brief + answer); leg-5 preconditions fail
closed. No re-verification needed (spellings unchanged by D1–D3).

## Registration actuals (v4 extractor run on RED bytes, zero warnings)

distinct 3 / rejecting 3 / atomic 3 / direct-only 3 / agreement 3 / authority
4 = 19 registered, 0 EXTRACT-FAIL (two-layout rule proven: same-line +
fourmolu-split `it|prop` + indented `"name" $ do`). Row files reproducible
via gate leg-4 on any tree containing the RED spec.

## Known-vacuous spots (directed to GREEN refinements R-a/b/c/d, answer §D4)

atomic#3 (`S28AppApiSpec.hs:250-272`, pure-sink, asserts nothing); agreement
props (`:315-336`, both tautological `True`, unforced lets; the `it` at :337
discards its fold); replay-equality it (`:359-365`, reads live+rows then
`pure ()`). Kill-closure depends on R-a/b/c landing (M3→atomic#2 already
toothed; M5/M6 need the refinements). Rejecting dead gs1/_ scaffolding
(~:169-190) flagged for removal. All owned test surface, no new files.

## D5 deadlock (binding-relevant mandate; gate-untouched)

Four-route enumeration on RED+base bytes (direct/propose/approve/app each
require admin/member signer; Lean Validate.lean: no bootstrap arm) proves no
first member is obtainable from empty members under frozen
`openIntegratedKEL … -> s -> FilePath`. H7 shape (`-> GroupState s ->
FilePath` + founding persist/load+verify) frozen in r5 §D5 + answer D5;
implementation + Store-test call sites TBB (GREEN). Gate names no open-args
(no gate impact).

## Freshness baselines (all absent repo-wide at freeze)

`_m1_boundarySeparates`, `AdmitMemberVoted`, `unsafeSetAppStateSTM`: 0 hits
(lib+test+cabal). First appearance must be a gate leg-5 splice (reverted
after) or a defect.
