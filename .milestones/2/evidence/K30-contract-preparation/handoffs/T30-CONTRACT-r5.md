# T30-CONTRACT-r5 — kelgroups #30 substrate vote interface + closure evidence (FOR IMPLEMENTATION AUTHORIZATION)

Ticket preparation owner `t30-contract` (Muse), 2026-09-06. Preparation-only:
read-only inspection + own planning artifacts. No compilation, tests,
mutations, gate runs, dispatch, product edits, commits, push/PR/merge/release,
issue comments, or spend (spend 0 throughout).

- Brief sha256 `f6d857639a4f3d9aa466c081305f43ae9e59bad4fd9dcb790a08bd85fc856416`.
- SUPERSEDES r4 (r1–r4 retained unmodified; all COMPLETEs stand). This r5 is
  the NOTE-006 packet: r4 is NOT accepted, valid earlier corrections stand,
  no broad rewrite, no scope reduction. One principle: **controls reach
  their target layer with the mechanism named, or the limit is stated with
  its deliverable — nothing attributed beyond its layer.**
- Operative base kelgroups `main` @ `933e385df2f2a251bb54a08bb7663f0d41fafb64`
  (S28 LANDED, PR#32; CI+Release SUCCESS; #28 CLOSED). Accepted Lean
  Reactivegas @ `3590c0015b84fd58004bf6fb44dd18b107304c48` (zero-diff EMPTY
  vs `4a6cd87` on Vote + Integration/State/Validate; landings rebind
  explicitly, never anticipated).
- Companions (same frozen content, row-addressable): `handoffs/
  T30-COMMAND-MAP-r5.md` (obligation→command map); `handoffs/
  T30-IDENTITY-MAP-r5.md` (TRUE per-identity table — every row from a file
  read in full); `handoffs/T30-DRIFT-LEG-r5.sh` (ACTUAL drift-leg script
  text for parent review as code). This contract ≠ the ticket owner's final
  immutable gate.

## 0. r5 corrections (falsehoods owned + N5-1–N5-6)

FALSEHOOD LEDGER (each withdrawn with cause — the epic owner verified every
one at source):

- **F1 (blocking):** r4 §7-PERFILE's "`Invariants.lean`: theorems/proofs
  ONLY, expected-empty" is FALSE. The file (1228 lines, READ IN FULL at
  r5) defines `QuestionClean` (:32), `tallyKeysOfQuestion` (:37),
  `tallyKeysOfState` (:39–41), `structure SweepReady` (:46), `structure
  VoteWellFormed` (:59), `preservesQuestionDecide`/`PreservesQuestionSemantics`,
  `sweepDuplicating` (a NAMED Lean-side mutant), and 17 public theorems
  plus ≈40 private lemmas. Cause: the claim described a file never opened
  — the same absence-inventory trap shape as before, stated plainly.
  FIXED: per-IDENTITY table (`T30-IDENTITY-MAP-r5.md`) replaces all
  whole-file empty verdicts (NOTE-006-1).
- **F2:** `Tests.lean` (397 lines, READ IN FULL at r5) was never
  characterized: it holds fixture defs (`viewOf*`, 8 public views),
  builders (`vOpen`, `vCast`), 15 executed witness states/traces,
  admissibility `#guard`s, R57-07 `example`s, R-48/V-2 unruled-consequence
  witnesses with an explicit never-freeze warning, R-53 dual-view witnesses
  marked NOT production-reachable in S62-A, and a cited auditor instrument
  (`nonresponsabile-open.lean`, sha256 `1f7aa80a`). FIXED: per-identity
  rows with exclusion reasons + review-corroboration column (never kills).
- **F3:** r4's frozen 5-file list vs the live 7-file Vote dir (Event,
  Fold, Invariants, State, Tests, Types, Validate — observed read-only).
  FIXED: the frozen extent names all 7 with per-file reasons; the brief's
  5-file evidence list is reconciled as the mirror-obligation set while
  the 2 proof/witness modules stay in the reviewed extent (scope
  unchanged, extent honest).
- **F4:** r4's B15-secondary `.hi` claim is UNREALIZABLE — a
  compiler-failing M10 run writes no fresh `.hi`; stale inheritance was
  never excluded. DELETED (stated removal, never counted as covered).
  REPLACED by the B22/HIDEMO design (NOTE-006-2c): separate GREEN build
  with drift, specified below.
- **F5:** "sequential bind-verify-compare closes TOCTOU" — overclaim.
  REPLACED by immutable-view consumption + stated race limit (NOTE-006-2b).
- **F6:** L2 `lake build` against the Reactivegas checkout as our own
  execution — DROPPED: elaboration validity of the pinned commit is
  upstream's acceptance, not ours to re-prove; we bind pin + bytes.
  (kelgroups' own `just lean` leg rides B21 and is tabulated — NOTE-006-2d.)
- **F7:** "zero added cost" assertions — WITHDRAWN. Leg-unit accounting +
  per-repository call tables below; every sub-invocation named in its leg.
- **F8:** r4 "no-unmet-prerequisite" + 23/22 counts — NOT admitted (per
  NOTE-006). SUPERSEDED by r5's 25/24 with five named above-20
  justifications. No prerequisite, no measurement request: §12
  freeze-validation is plannable exactly as specified.

N5 RESOLUTIONS (valid earlier corrections kept; overrides explicit above):

- **N5-1 (per-identity, never whole-file empty):** `T30-IDENTITY-MAP-r5.md`
  classifies EVERY Lean declaration: mirror obligation (runtime
  vocabulary/state/equations) or stated exclusion reason. Proof-side
  `Prop`s/theorems/proof-helpers/fixture-`def`s/builders/witnesses/
  examples/guards/instances warrant exclusion FROM a runtime mirror — a
  per-identity judgment with a reason, never grounds to claim they don't
  exist. NO Haskell runtime requirement invented for proof-side helpers
  (explicit in every such row). Emission rule's EXACT subject: (i)
  inductive/structure TYPE + CONSTRUCTOR identities of runtime vocabulary
  and state shapes; (ii) EQUATION GROUPS (per-function arm sets) of the
  production fold/validation functions. Corroboration column carries
  review-level consistency pointers (Lean theorem/witness ↔ Haskell REQ —
  eyeballed at review + audit, never gate kills), including: threshold
  theorems → parameterization + no-default fence (with Tests.lean's
  never-freeze-exhibits warning TRANSFERRED as a Haskell review rule);
  refusal theorems → M1/M2 rows; partition → M9 (+ M15 flip side);
  no-expiry premise → REQ-NOEXPIRY (franchise-conjunct removal story
  corroborates R62-11 placement); franchise theorem → M7/M13;
  idempotence theorem + `sweepDuplicating` → new REQ-SWEEP-IDEM + M15
  (the file's own mutant shape transcribed to a test-suite property —
  not invented); R-53 dual-view witnesses → M7/M13 WITH the
  not-production-reachable caveat CARRIED OVER (Haskell post-view fixtures
  likewise observe sensitivity and claim no produced transition — stated
  limit in both rows); auditor-instrument citation → M1/M2 control-shape
  precedent.
- **N5-2a (integrity refusal ≠ extraction proof):** byte-hash is a COMPLETE
  change detector — proof: any added/removed item necessarily alters file
  bytes; hash equality ⇒ byte equality ⇒ no change (airtight FOR
  DETECTION). It proves NOTHING about which item moved: item-level
  attribution is enforced:NONE automatic + MANDATORY re-review deliverable
  (diff review vs the identity table + sign-off; the gate REDs on ANY
  mismatch until signed). The join's job is BASELINE conformance at freeze
  + re-conformance after re-review (both human-verified inputs — stated).
  Nothing attributed beyond its layer, anywhere.
- **N5-2b (sequential ≠ atomic):** drift inputs consumed from IMMUTABLE
  git views (`git show HEAD:<path>` — content-addressed, TOCTOU-free by
  construction). Working-tree conformance (`status --porcelain` +
  `diff HEAD` empty) is a SEPARATE labeled point-in-time sample. Residual
  race (write landing strictly between sample and build) = STATED accepted
  limit (threat model accidental drift; writers are fenced workers; leg-1
  hygiene before/after catches residue).
- **N5-2c (.hi provenance):** the emission/reconciliation instrument is
  NAMED specifically (§7-HI): post-exit-0-build emission per frozen-module
  row, freshness-marker rule (consumed `.hi` newer than pre-build marker
  or REFUSE — stale never inherited), hash-pinned inventory, diff-tripwire
  + review classification. B15-secondary DELETED (F4). Firing demonstration
  rides B22 (GREEN build with real drift). Vote.* selector FIXED: mapping
  rows name containing-modules; frozen module list INCLUDES
  `KelGroups.Event` (BaseMutation/BaseChange rows live there, verified
  from the 52-line full read); new-module rule stated.
- **N5-2d (repository accounting):** per-repository call tables (§9):
  KELGROUPS worktree (every B/S/A execution incl. kelgroups-own `just
  lean` inside B21); REACTIVEGAS checkout (read-only pin/hash/status —
  NO lake build, rationale stated). Compiler-metadata + drift probes get
  concrete commands/counters each (leg-unit accounting: a leg's
  whole-project invocation covers its named in-leg steps; anything outside
  a counted leg carries its own counter).
- **N5-3–N5-6 carried:** per-file rule with actual extent (§7-PERFILE);
  full-audit drift reruns + stale-conditional deletion + itemized SLIM +
  A-REBIND both branches (§9); universals deleted, bounded surface with
  zero new builds (§7-SURFACE); minimal M4a/M4b criteria + classification
  procedure, POSTSWITCH required (§8).

## 1. Objective (one observable — unchanged)

A nondegenerate application opens an app-scoped assent question, casts
ballots as the franchise, and observes the verdict (positive AND negative,
each with explicit cause) plus its closure record through the **integrated**
boundary — refusals before durable effects, replay equality. Test-only
proving instance (as S28); the user demo is `paolino/kelgroups#33`.
Threshold a parameter at every evaluation; exhibits never defaults.

## 2. Scope — rows (FULL #30 scope; per-identity + bounded surface applied)

Accepted behaviours INTACT: converge to the LANDED S28 interface, never
redesign. `Trivial` degenerate presence-only. No unilateral Lean edits. No
threshold default. No expiry. No votable admission. No second store/fold.

| id | requirement | accepted evidence (@3590c001) | deliverable | depends on | Terms statement |
|---|---|---|---|---|---|
| R30-1 | openQuestion (collective + permission-with-designee), responsabile-only admission | `Vote/Event.lean`; `Validate.lean:57` | Haskell #30 | none | intact |
| R30-2 | cast: one-position placement, idempotent re-cast, switch moves | `placeBallot` (`Fold.lean:53-56`); `effectedState` (`:95-100`); `Validate.lean:59-64`; guarded `setInsert` (shared Types.lean — source-verified) | Haskell #30 | none | transcribed; M4a/M4b minimal criteria + classification |
| R30-3 | sweepClosures same-step close + ClosureRecord (tally + franchiseChange); retention AND non-duplication; never silent drop; NO expiry | `sweepStep`/`sweepClosures` (`:65-76`); append-only `closed` + R-51/R-61; idempotence theorem + `sweepDuplicating` (Invariants.lean T6223 section — money-bearing duplication risk is in-scope); no clock (R-54) | Haskell #30 | none | lifecycle neither ahead nor out; duplication covered (M15 — Lean's own mutant shape, not invented) |
| R30-4 | verdictOf: collective threshold @ franchise (legacy order) + permission designee arm (never tally) | `verdictOf` (`State.lean:82-96`); R-46/49/50/64; threshold-congr theorem | Haskell #30 | none | parameter everywhere; exhibits never defaults (R-48 warning transferred) |
| R30-5 | refusals produced now; `notDesignee`/`notProposer` DECLARED (`Validate.lean:41-42`), zero Slice-A sites — unruled INTENTION | 4-ctor error + 3-arm validation (`:56-70`); #81 out-of-scope § | 4-ctor vocabulary; NO producing semantics | NONE (preserved boundary) | UNSCHEDULED; tripwire only |
| R30-6 | franchise from canonical GroupView every evaluation | `franchise`/`franchiseSize`/`isResponsabile`; R62-11; S28 `groupView` | Haskell #30 | none | never a local copy (M13); franchise theorem corroborates at review |
| R30-7 | negative-verdict delivery observable at boundary | `ClosureRecord`; negative arm; S28 hook surface | interface #30; consumption → #76 | #76 effect only | evidence-exposure only |
| R30-8 | vote routing separated from base admission | T6222; non-admitting `pendingBase` (`KelGroups/Event.lean`, verified); S28 direct-only | boundary definition #30 | none | intact (bounded three-part surface property) |
| R30-9 | approve path matches LANDED base rule; V-2 rebind after landing | V-2 settled; #68 OPEN | REBIND after #68; freeze meanwhile | #68 ONLY at `tryEnactBase` + proposer rules | concrete boundary, no blanket block |
| R30-10 | RULED lifecycle — MECHANISM SURFACE ONLY (hooks shaped for extension; 4-cause DATA shape; append-only; atomic discard — S28-existing); never exercised for absent closes | V-5; carried causes (`Types.lean:76-77`); renounce→gs verified no-op (Fold.lean + Tests.lean guard corroborate); #81 §1–§3 | surface + extension validation; CONTENT → #81 | #81 (L-1–L-6); #76 (L-7) | D1: no executable #81 rows |
| R30-10U | UNRULED refusal policy, NOT scheduled; boundary PRESERVED | #81 out-of-scope § | NONE | — | UNSCHEDULED |
| R30-11 | verdict → economic effect | NOTE-016/A-Q001 (ruled, wire missing) | #76 (evidence only) | #76 | neither ahead nor out |
| R30-12 | PureScript client propose/vote (adapt-only) | `kelgroups-client` transport | #30 client additions | none in team | adapt-only INCLUDED; UI/wasm NOT. LIMIT: TEST-boundary roundtrip; production-server roundtrip out of scope |
| R30-13 | Lean proofs | Vote proofs = evidence (incl. the 9 `#print axioms` names — vanishing detected Lean-side); Slice-B producers only where RULED | LEAN-OWNED; zero kelgroups Lean edits | Lean lanes where ruled; none where unruled | no invented obligations |
| R30-14 | denial/dissent observable (L2) | negative closure delivery | interface #30; consumption #76 | #76 | as R30-7 |
| R30-X | NON-GOALS: expiry; theta default; votable admission; second store/fold; UI+wasm; in-kelgroups economics | R-54; R-46/47; INV-62; R9c/R11; ASSENSO gap | none | — | fence + kills + leg 7 |

Downstream obligations (recorded with owners, never executed; acceptance
verifies ONLY "recorded with owner"): L-1 renounce-close, L-2
departure-close atomically, L-3 `.negative`, L-4 exact causes, L-5
retention, L-6 scoping + L-6a + L-6b (owner #81); L-7 refund GATED on #76
(owner #76).

#29 remainder (unchanged): #33 demo (blocked #30; publication separately
gated); #34 release/notes (blocked #33+#30; desk authority); downstream
notes (with #73 lane). `#29` + #73 OPEN.

## 3. Compact spec

Stories S1–S6 (unchanged): open; cast/switch/recast exactly-once; positive
AND negative verdicts with causes + retained, never-duplicated records;
pre-effect refusal (state AND log unchanged); accepted-KEL replay
identical; client propose/vote.

Requirements: R30-1–R30-8 + R30-12 + R30-14 now; R30-9/R30-10-content/
R30-11/R30-13 as dependencies; R30-5-producing + R30-10U unscheduled;
R30-X fences. Parameterized threshold; canonical-view franchise; refusals
first; replay equal; retained + unduplicated closures.

Rejection behaviour: vote-path `notResponsabile`/`questionNotFound`;
substrate-path S28 identities; refusal advances nothing.
`notDesignee`/`notProposer` vocabulary only. No expiry refusal anywhere.

Production-path examples (in-suite, real step/fold): REQ-ADMIT-PATH;
REQ-OPEN-REFUSE + REQ-CAST-NONRESP (exact error, aggregate AND log
unchanged); REQ-NONDECIDE-PERM (recorded-but-not-deciding — exercised,
never widened nor narrowed).

Success: S1–S6 + all §7-REQ through the integrated boundary on the
candidate SHA (§10); `Trivial` intact; full `just ci` green; fresh audit
PASS; no shipped defaults; no silent drops; no duplicates.

## 4. Plan

Single slice S30-1 (FULL boundary: mirrors + wiring + persistence + client
+ proving instance). No S30-2. RED absence proof first (C1/C2 — absence
ONLY); GREEN in envelope (15 mutant runs + M10b enumeration + live drift
machinery + REQ set); B20 omission challenge (sole guard falsification);
B22 HIDEMO (GREEN overlay build with real drift — .hi tripwire firing
demonstration); SLIM S1–S3 itemized; FULL audit (every direction rerun, no
inheritance); draft PR + remote CI; acceptance. Ticket owner freezes the
final gate; §12 freeze-validation is the single handoff check.

Constraints: this packet cost 0; §8 fence; whole-project invocation =
BUILD; S28 concurrency discipline (serialized append, refusal-before-codec;
no second writer); no parallel heavy builds.

Live boundaries (can-fail controls): step/validate agreement; Store/KEL
append + replay (equality; founding guard); client TEST-boundary roundtrip
(limit stated); drift input-binding + pin + join (immutable views; hashes;
clean sample); compiler metadata (.hi tripwire + exhaustiveness). No
behavioural proof by source text, no shrinkable inventories, no
absent-import as behaviour.

Order: S30-1 (B1–B21) → B22 HIDEMO (overlay, discarded) → SLIM S1–S3 on
candidate → audit → PR → #33 → #34. #68 touches only R30-9 on landing.

## 5. Models (compact; no implementation content)

Modules: new `KelGroups.Vote.*` mirrors (depend on `GroupView` + S28
`State`); `KelGroups.Fold` owns `Integration` composition (proving
`AppState` or dedicated fold — ticket owner decides; `BaseProposal`
reading, `proposalMutation → BaseMutation`, `digest`, hook composition
nowhere else); `KelGroups.Store` owns the vote path (existing integrated
path; no second writer/tables); `kelgroups-client` owns propose/vote
(existing transport, adapt-only); proving instance owns the test-only app.
Drift mapping + discovery + REQ list + identity table are frozen gate
artifacts.

Data: `Question{kind,proposer,assents,dissents}` (no time-like field);
`ClosureRecord{questionId,question,verdict≠open,cause}` (4 causes as DATA);
`VoteState{openQuestions,closed-append-only-unduplicated}`;
`Verdict`; `Threshold` (parameter); `QuestionKind{collective,
permission(designee)}`; `ClosureCause{4}`; `VoteEvent{openQuestion,cast,
renounce}` (T6222); `VoteError` (2 produced + 2 vocabulary-only).
Franchise never stored; verdict single site; closure = remove + append
atomically; re-sweep stable (new REQ-SWEEP-IDEM).

Functions (spellings frozen by ticket owner): vocabulary constructors;
`verdictOf`; `franchise`/`franchiseSize`/`isResponsabile`; `lookupQuestion`;
`closureCause`; exhaustive `validateVoteEvent`; `placeBallot`;
`sweepStep`/`sweepClosures`; `effectedState` (authorization-free);
`applyVoteEventChecked` (integrated path uses it, never revalidates);
`foldVote`/`foldFrom`; wiring (`digest`, `proposalMutation`, vote-aware
`appFold`/`baseHook`). `GroupView`/`Threshold` explicit everywhere.

## 6. Tasks (slice S30-1)

- T30-1 RED absence proof (C1/C2, absence ONLY — Vote-absence re-verified
  at r5: zero references in lib/test/app).
- T30-2 vocabulary + state mirror (+ REQ opens/verdicts/reads).
- T30-3 validation mirror (3-arm exhaustive; 4-ctor vocabulary, two
  site-less; tripwire noted).
- T30-4 placement + effects (M4a/M4b minimal criteria; classification;
  open-never-overwrites; renounce no-op preserved).
- T30-5 sweep + closure + retention + non-duplication (PRODUCED causes;
  M9 + M15 distinct sites).
- T30-6 `Integration` wiring (current base).
- T30-7 persistence path (append + replay + founding guard; no second
  writer).
- T30-8 client additions (adapt-only; TEST-boundary; limit stated).
- T30-9 mechanism-surface shaping (L-1–L-7 recorded with owners; zero
  executable #81 rows).
- T30-10 drift mapping + live machinery + REQ set + identity table
  (§7-DRIFT, §7-PERFILE, §7-REQ, identity-map artifact, drift-leg script).
- T30-11 GREEN envelope + omission challenge + HIDEMO + SLIM + CI +
  hygiene.
- T30-12 audit handback + PR (every direction rerun; draft post-GREEN;
  exact-SHA merge at desk).

## 7. Frozen requirement-to-command/control map (binding)

Conventions: whole-project invocation = 1 BUILD. Per-mutant cycle = apply +
run + revert, hash-verified restore (failure aborts exit 3). Hidden
invocations forbidden: every cited result maps to a counted B-row or
PROBE-row (leg-unit accounting: a leg's invocation covers its named in-leg
steps). Predicates: COMPILER-kill (exit≠0 + diagnostic quotes ctor/site +
zero parse-error lines); TEST-kill (exit≠0 + `Failures:` names ≥1
registered REQ-ID; empty/crash/timeout/infra/parse NEVER count);
GREEN-ENUM (exit 0 + lists every allowed ctor). Setup/infra failure =
INCONCLUSIVE abort, never kill. Charge-0 recon free, never evidence. BAN:
regex output never cited as semantic inventory, anywhere.

§7-CMDS (exact; toolchain re-pinned at freeze):
`nix develop .#ci --quiet -c just build`;
`nix develop .#ci --quiet -c cabal test all -O0 --test-show-details=direct`;
`nix develop .#ci --quiet -c just ci` (INCLUDES kelgroups-own `just lean`
sub-step — tabulated, not hidden);
probes `nix develop .#ci --quiet -c cabal test invariants
--test-option=--match --test-option=/S30-<Group>/<REQ-ID>/` (Groups frozen
below — fully determined strings);
`nix --version` + one batch `ghc/cabal/lake/node/spago/just --version`.

§7-REQ (frozen Groups; 26 IDs): `S30-Open`: REQ-OPEN-COLL, REQ-OPEN-PERM,
REQ-OPEN-REFUSE, REQ-OPEN-DUP. `S30-Cast`: REQ-CAST-ASSENT, REQ-CAST-SWITCH,
REQ-CAST-POSTSWITCH, REQ-CAST-RECAST, REQ-CAST-UNKNOWN, REQ-CAST-NONRESP.
`S30-Sweep`: REQ-SWEEP-TALLY, REQ-SWEEP-DISSENT, REQ-SWEEP-FRANCHISE,
REQ-RETAIN, REQ-NOEXPIRY, REQ-SWEEP-IDEM (new: double-sweep == single
sweep; no duplicate records; closed set stable — from Lean's
sweepClosures_idempotent + sweepDuplicating, transcribed as test property).
`S30-Verdict`: REQ-VERDICT-COLL, REQ-VERDICT-PERM. `S30-Franchise`:
REQ-FRANCHISE-CURRENT. `S30-Negative`: REQ-NEG-DELIVER. `S30-Route`:
REQ-ROUTE-ENUM. `S30-Lifecycle`: REQ-HOOK-EXT, REQ-RECORD-SHAPE.
`S30-Client`: REQ-CLIENT-ROUNDTRIP. `S30-Admit`: REQ-ADMIT-PATH,
REQ-NONDECIDE-PERM. Cross-check: every ID registered + executed else RED.
B20 (one ID removed → RED) is the ONLY guard falsification; C1/C2
absence-only.

§7-PERFILE (actual 7-file extent, observed read-only — Event, Fold,
Invariants, State, Tests, Types, Validate): Types/Event/Validate carry
inductives (MUST emit ctor rows); State/Fold carry structures + equation
groups at the enumerated sites (MUST emit); Invariants carries theorems,
Props, proof-helpers, one named mutant (expected-empty of checked kinds —
proof-only module; its identities live in the identity table with
exclusion reasons); Tests carries fixtures/builders/witnesses/guards/
examples (expected-empty — witness-only module; identities tabled).
Empty rules: expected-empty files named (Invariants, Tests); any other
file emitting nothing → RED; empty-GLOBAL → RED always. Full per-identity
rows: `T30-IDENTITY-MAP-r5.md` (every row from a file read in full).

§7-DRIFT (immutable views + named oracles; script text:
`T30-DRIFT-LEG-r5.sh` for parent review as code):
- L1 INPUT BINDING (oracle: sha256sum + git contracts): per-file hashes
  computed over IMMUTABLE `git show HEAD:<path>` streams (TOCTOU-free by
  construction) vs frozen list; HEAD pin per repo; file-set `ls` vs frozen
  7+3 list; mapping self-check (extent files ⊆ identity-table file
  column). Working-tree conformance (`status --porcelain` + `diff HEAD`
  empty) is a SEPARATE labeled point-in-time sample. Residual race STATED
  as accepted limit (accidental-drift model; fenced writers; leg-1
  hygiene before/after catches residue).
- L2 DROPPED as own execution (F6): elaboration validity of the pinned
  commit is upstream's acceptance; we bind pin + bytes. (kelgroups-own
  `just lean` runs inside B21 — tabulated.)
- L3 COVERAGE JOIN (oracle: overlay-demonstrated discrimination): frozen
  mapping (Lean item → Haskell type + executing REQ-IDs; file:line
  PROVENANCE only) vs live Haskell emission + leg-4 log. Unmapped-emitted
  or dangling-mapping → RED.
- L4 .hi TRIPWIRE (oracle: GHC — only compiled code has `.hi`; NOTE-006-2c
  instrument): emission per frozen-module row (Vote.* + `KelGroups.Event`
  for BaseMutation/BaseChange rows + mapped wiring modules) taken ONLY
  post-exit-0-build in the same leg; freshness rule (consumed `.hi` newer
  than pre-build marker or REFUSE — stale never inherited); hash vs
  frozen (drift → RED + mandate review/classification). Firing
  demonstration rides B22 (GREEN build with real drift — specified, not
  asserted). Presence sub-check per mapped type (+ known-absent control
  token — legitimate positive control, not inventory certification).
- L5 ARM TOTALITY (oracle: GHC -Werror, live every build): M10a's break
  doubles as the live demonstration (secondary evidence in B15's log —
  the break, not any `.hi`).
- Haskell function presence = compilation (existence) + REQ-execution
  (exercisedness; mapping rows carry REQ-IDs; cross-check enforces). No
  Haskell semantic-grep exists in this packet.
- Lean declaration patterns = REVIEW PROMPTS only (unreconciled counts
  explained in the signed record; never evidence, never kills).
- Baseline completeness = rigorous review under §7-PERFILE + identity
  table (full reads done at r5 for all 7 files) with signed record →
  enforced:REVIEW, labeled (oracle: record + epic source-verification as
  observed). Regression from baseline = L1+L3+L4+L5 refusal machinery.
- Temporal hole → explicit-rebind process rule (landing ⇒ re-freeze +
  re-demonstration of all five drift probes before acceptance).
- REQUIRED drift probes (named): P-DRIFT-GREEN (live recon GREEN);
  P-DRIFT-ADDBYTE (overlay export + added line ⇒ hash-trigger REDs);
  P-DRIFT-DELBYTE (export − line ⇒ REDs); P-DRIFT-ADDFILE (export + file
  ⇒ file-set REDs); P-DRIFT-JOINMAP (mapping-copy − row vs LIVE B3
  Haskell emission ⇒ join REDs unmapped-live-item). Overlays = `git
  archive` export + ONE intentional edit, export-diff bound, hash+join
  end-to-end — trigger discrimination on source-shaped bytes, NOT review
  correctness. Each control REACHES its target layer: byte-controls→byte
  layer (complete detection, proven); join-control→join layer on live
  data; NOTHING attributed beyond its layer. Item-level attribution =
  enforced:NONE automatic + MANDATORY re-review deliverable (gate REDs on
  ANY mismatch until signed — stated).

§7-HI (NOTE-006-2c specific instrument): EMISSION step lives in B3's leg
(post-exit-0; marker rule); artifacts per frozen-module row
`<module>.hi.dump + sha256` in evidence; RECONCILIATION compares vs frozen
inventory hashes (drift ⇒ RED + review classification); BaseMutation rows
resolve via `KelGroups.Event` dumps (selector fix — verified site);
firing demonstration = B22 (GREEN + drift ⇒ diff fires); auditor mirrors
as A-HIDEMO. Compiler-failing runs promise no `.hi` (stated — F4).

§7-SURFACE (N5-5 bounded, no universals): frozen S30-allowed proposal
surface — each allowed ctor maps (D2 coverage join; new ctor ⇒
P-DRIFT-ADDBYTE-class RED) + each mapping enacts non-admitting (M10b
GREEN-ENUM through totality-witness `case`) + vocabulary closed at
enactment (M10a COMPILER). Review-only remainder named: allowed-set
matches product intent (human judgment, labeled). Direct-only admission
unweakened. Untracked-ctor totality stands as compiler-totality.

§7-MAP (obligation → owner → auditor; RE-RUN vs REVIEW):

| obligation | owner | auditor |
|---|---|---|
| absence T30-1 | B1 + B2 RED (absence ONLY) | A-RED1/A-RED2 frozen-BASE reruns (never candidate calls) |
| R30-1 | B4 + probes | A-TEST (RE-RUN); predicate reviews |
| R30-2 | B4; B6 M2; B7 M3; B8 M4a (SWITCH REDs); B9 M4b (RECAST REDs) + classification of extras (POSTSWITCH REQUIRED; observed signatures = freeze characterization, never pre-stated) | A-TEST; A-K reruns (ALL 15 UNCONDITIONAL) |
| R30-3 | B4 (append + replay); B10 M5; B11 M6; B12 M7merged; B14 M9; B19 M15 sweep-without-removal → REQ-SWEEP-IDEM RED (distinct site/filter-drop vs M9's append-drop, distinct obligation/duplication vs retention, distinct witness — G5 rule satisfied) | A-TEST + A-Ks; M6 = named boundary rerun |
| R30-4 | B4; B13 M8 | A-TEST |
| R30-5 | B3 cold (vocabulary + 3-arm exhaustive) | A-COLD; tripwire review (never kill) |
| R30-6 | B4; B18 M13 snapshot → REQ-FRANCHISE-CURRENT RED (post-view = sensitivity fixture, NOT a produced transition — limit stated) | A-TEST; A-K13 |
| R30-7/14 | B4 boundary + roundtrip | A-TEST; A-K6 |
| R30-8 + bounded surface | B3 (2-arm enactment); B15 M10a (COMPILER/CLOSED-totality; in-log exhaustiveness-fire secondary — NO .hi secondary); M10b GREEN-ENUM named in B4 | A-COLD; A-K10a UNCONDITIONAL; M10b re-checked in A-TEST |
| R30-9 | current-base freeze (B3/B4) | A-REBIND iff #68 landed (author integrates + fresh final-SHA audit; auditor never repairs) |
| R30-10 surface | B4 (hook pre/post; 4-cause DATA; append-only; atomic discard); B16 M11 → REQ-HOOK-EXT RED | A-TEST; A-K11 |
| produced-cause distinction | B12 M7merged (forced-.tally → REQ-SWEEP-FRANCHISE RED; carried excluded) | A-K7 |
| L-1–L-7 | recorded with owners; NO command | record-only review |
| R30-10U/PROD, R30-11 | no command (boundary; no wire/mock) | — |
| R30-12 | B21 leg-6 (`spago build` + `spago test`, TEST-boundary, limit stated); B17 M12 → ROUNDTRIP RED | A-CI; A-K12 |
| R30-13 | B21 `lake build` (kelgroups-own) green only | A-CI |
| drift | 5 REQUIRED probes | binding GREEN + 5 directional reruns (auditor PROBE ×6, named, no inheritance) |
| guard | B20 omission rerun (ONLY falsification) | A-OMIT rerun (+ B20-log read alongside — labeled read) |
| .hi tripwire | B3 emission + hash-pin; B22 firing demo | A-COLD emission mirror; A-HIDEMO firing rerun |
| cold/final | B3 1B + B21 final CI 1B + tracked-clean + Trivial-only + founding guard | A-COLD/A-CI |
| SLIM | S1 slim-build + S2 slim-test + S3 slim-ci (itemized; legs 1/2/2b/7 ride charge-0/probe; drift GREEN probe in cap) | — |

Mutant ledger (15 runs B5–B19): M1 openQuestion-nonresp bypass
(REQ-OPEN-REFUSE); M2 cast-nonresp bypass (REQ-CAST-NONRESP); M3
unknown-accept (REQ-CAST-UNKNOWN); M4a erase-drop (criterion SWITCH REDs);
M4b unguarded-insert (criterion RECAST REDs — inverts the source-verified
guard); M5 tally-suppress (REQ-SWEEP-TALLY); M6 dissent-suppress
(REQ-NEG-DELIVER); M7merged cause-forced-tally (REQ-SWEEP-FRANCHISE;
carried excluded); M8 permission-tally-consult (REQ-VERDICT-PERM); M9
close-and-discard (REQ-RETAIN); M10a admission-ctor-added to BaseMutation
(`KelGroups/Event.lean` site — COMPILER/CLOSED-totality); M11
hook-ignored (REQ-HOOK-EXT); M12 propose-path dropped
(REQ-CLIENT-ROUNDTRIP, client boundary); M13 snapshot-in-payload
(REQ-FRANCHISE-CURRENT); M15 sweep-without-removal (REQ-SWEEP-IDEM —
Lean's sweepDuplicating shape transcribed). M14 RETIRED (merged into M7 —
number not reused). M10b enumeration (GREEN-ENUM in B4): per-ctor
enactment through totality-witness `case`; interface proposition, never
behavioural refusal.

Discovery bounds: quantified Lean equation sites (`placeBallot` 2;
`sweepStep` 2; `effectedState` 3+2; `validateVoteEvent` 3+2; `verdictOf` 2
+ `closureCause` 3; `sweepClosures` shared step). Bounds the SET (new
ctor ⇒ new arm ⇒ new mutant, never silent pass); never row coverage.

Charge-0 recon (free, never evidence): reads; D7 tripwire (review-time;
accidental-introduction → mandate review, never kill); `git
status/diff/rev-parse/log`; `gh issue view`; `--version` pin reads.

## 8. Candidate-independent initial gate design

Fence (ticket owner versions at freeze): WRITABLE — new `KelGroups.Vote.*`
mirrors; `Integration`-wiring points; `Store.hs` vote path (existing
integrated path; no second writer/tables); proving instance; client Api
adapt-only; `kelgroups.cabal` + `test/Main.hs`; drift mapping + discovery
+ REQ list + identity table + drift-leg script (frozen gate artifacts).
FENCED — `lean/**` (both repos' Lean sources — read-only inputs);
historical bodies; `Trivial.hs` (presence-only, uncounted); S28 production
outside named points; `client/` beyond adapt-only; UI/wasm/economics;
release metadata.

Legs: 1 hygiene before/after; 2 identity + self-hash + ancestry (HEAD
descends from frozen slice base); 2b pins exact, fail-closed (re-pinned at
freeze); 3 build cold/warm; 4 inventory (≥K groups) + registered (REQ
cross-check: every ID registered, total == file examples, no orphans —
POSTSWITCH + SWEEP-IDEM REQUIRED) + execution (all executed, none pending;
historical green) + exit 0; 5 mutants B5–B19 (tracked-clean committed
candidate at recorded HEAD; apply+run+revert; hash-verified restore;
failure aborts exit 3) under MINIMAL criteria + CLASSIFICATION PROCEDURE
(every extra RED recorded + attributed same-cause/independent/setup —
setup never default; unattributed extras ⇒ INCONCLUSIVE until classified);
DRIFT leg (script artifact; L1 immutable-view binding + L3 join + L4
tripwire + freshness/marker rules + 5 controls); M10b GREEN-ENUM in leg-4
scope; 6 full `just ci` (kelgroups-lean sub-step named); 7 `Trivial`
presence + client CI presence. `set +e`; full log + per-leg sha256; meta
file. B20 = separate counted leg-4 rerun minus one REQ-ID (RED required);
C1/C2 absence-only. Isolation: single-site splice per run, diff-hash
bound, named attribution line per RED example. Freeze deliverable: ticket
owner records OBSERVED mutant signatures as characterization (not
acceptance).

Kill-attribution: COMPILER (M10a) / TEST (behavioural, REQ-ID-naming) /
GREEN-ENUM (M10b). Criterion met + unclassified extras = INCONCLUSIVE
abort, never kill.

Evidence: `run-receipt`-style capture per cited run; self-hash + ancestry
leg 2; mutant diff hashes pre-run; restore hashes post-run; overlay
export-diffs bound; .hi inventory hashes bound; immutable-view byte hashes
bound.

Spend classes: BUILD (whole-project = 1); PROBE (narrow: exact-REQ-ID
`--match`, single-component build, drift runs = 1 vs 24 cap);
CHARGE-0 (enumerated recon — free, never evidence); AUDIT-BUILD/AUDIT-PROBE
(same, auditor cap). No parallel heavy builds; every failed setup/attempt
journaled; no automatic raises (exact gap first).

## 9. Operational classifications + proposed ceilings (FROZEN with §7) +
per-repository call tables (NOTE-006-2d)

Owner fit: B1–B2 RED (2) + GREEN 20 (B3 cold + B4 test + B5–B19 fifteen
runs + M10b named in B4 + B20 omission + B21 CI + B22 HIDEMO overlay-GREEN)
+ SLIM S1–S3 (3) = **25 builds**. Above-20 justifications (each ordered):
M4b←D4 (own recast mutant); M13←D5 (explicit snapshot accounting);
B20←D3 (sole guard falsification); B22←NOTE-006-2c (GREEN-build .hi
firing demo — the only successful-build-with-drift in the campaign);
M15←NOTE-006-1 (Lean's own sweepDuplicating transcribed; money-bearing
duplication hole). M10b in B4; drift overlays ride drift probes.
Probes ≤24: 5 REQUIRED named (P-DRIFT-GREEN/ADDBYTE/DELBYTE/ADDFILE/
JOINMAP) + kill-confirm ≤15 (ambiguous logs only — B-logs already quote
kills) + dispute ≤2 (beyond 2 → BLOCKED question, not more probes) +
transient ≤2, REQUIRED-first. **PROPOSED owner 25/24** (supersedes 23/24).
PROPOSALS pending fit-proof (§12 i–vii + new viii–x) + authorization; gap
returns exact cost, never trimmed scope.

Auditor (pre-dispatch, exact): A-RED1/A-RED2 frozen-BASE 2B; A-COLD
(+ .hi emission mirror steps) 1B; A-TEST (REQ cross-check + M10b re-check)
1B; A-CI 1B; A-K×15 15B (UNCONDITIONAL — stale conditional deleted;
REVIEW ONLY for L-records, tripwire output, B20-log-alongside-A-OMIT —
each named); A-OMIT 1B; A-REBIND conditional 1B (landed: execute;
unlanded: unspent WITH reason); A-RESERVE 1B; A-HIDEMO firing rerun 1B;
drift binding GREEN + 5 directional reruns from probe cap (PROBE ×6,
named). Named sharing: M6/A-K6 and M12/A-K12 are the boundary reruns (no
separate builds). **PROPOSED auditor 24/24** (24B; probes 6 required drift
+ finding-narrowing ≤14 + reconfirm ≤4 = 24). Bottom-up; coverage never
trimmed. Seat fresh Codex-or-Grok (never Muse/GLM/Claude), clean detached
worktree at candidate SHA, argv-pinned model+effort, post-cursor START,
hash-bound report; recommends, ticket owner decides; every repair gets a
fresh auditor.

KELGROUPS worktree calls (the ONLY repo where the gate executes):
B1 `cabal build all --enable-tests -O0` (absence RED); B2 `cabal test
all -O0` (absence RED); B3 `just build` + marker-touch + per-module
`ghc --show-iface` emission + hash-pin steps; B4 `cabal test all`
(+ M10b sub-claim + REQ cross-check steps); B5–B19 mutant apply + build-or-
test + revert + restore-verify steps; B20 leg-4 rerun minus one REQ-ID;
B21 `just ci` (INCL `just lean` kelgroups-proofs sub-step + spago/lake
legs — named, not hidden); B22 scratch-export + overlay-edit + build +
emission + diff-fire + discard; S1–S3 slim-build/test/ci; A-RED1/2,
A-COLD (+ emission), A-TEST, A-CI, A-K×15, A-OMIT, A-REBIND, A-RESERVE,
A-HIDEMO (same shapes, auditor worktrees); drift probes (hash/join/diff
steps, no compilation); `--match` probes (exact REQ-ID strings).
REACTIVEGAS checkout calls (read-only inputs — NO lake build: upstream
acceptance owns elaboration validity of the pinned commit; stated):
`git rev-parse HEAD` (pin), per-file `git show HEAD:<path> | sha256sum`
(byte binding over immutable views), `git status --porcelain` (labeled
point-in-time sample), `git ls-files` (file-set), `git archive` (overlay
exports). Each rides its leg's/probe's counter (leg-unit accounting —
stated rule). NOTHING rides free except enumerated charge-0 recon.

Team (standing fence, commissioned at authorization — NOT by this packet):
Muse ticket owner → Muse commit owner (`draft=NONE`) → fresh Codex-or-Grok
auditor per submission (two max, one bounce, repair re-audited); signed
commits; draft PR post-GREEN; exact-SHA merge at desk.

## 10. Acceptance (binding when commissioned)

Threshold-parameterized verdicts (permission never tallies); retained +
unduplicated records with PRODUCED causes (never silent; no expiry);
refusal pre-effect (aggregate AND log unchanged; accepted-KEL replay
identical); validate/fold agreement (same triple; never historical
validator, never double-wrap); negative delivery at the boundary (both
verdicts with causes via step + append + replay); zero producing sites
(tripwire clean at review); §7-REQ complete (26 IDs registered + executed;
B19 RED observed); drift GREEN + all four directional REDs observed with
pin + immutable byte-hashes + clean sample bound; .hi inventory hash
matches frozen (drift → RED + review); B22 diff-fire observed; M10a
COMPILER-kill (+ in-log exhaustiveness-fire secondary) + M10b GREEN-ENUM
observed; bounded surface closed with review-only remainder labeled;
M4a/M4b criteria met with extras classified (no unattributed REDs);
client additions under client CI (limit stated); `Trivial` intact; full
`just ci` green; tracked-clean both ends; founding guard held; L-1–L-7
recorded with owners (reviewed as record); per-identity table rows all
resolved (mirrored or exclusion-reasoned — no unclassified Lean
declaration in the 7-file extent); fresh audit PASS complete; bounded
claims only.

## 11. Open questions / dependencies (enumerated, not invented)

- #68 → R30-9 rebind + revalidation only.
- #81 (§1–§3; L-7 gated #76) → R30-10 content; unruled exclusions depend on
  nothing.
- #76 → Reactivegas side; kelgroups exposes interface + closure evidence.
- #75 (R3.1) → test input for persistence (threshold = test input), not a
  shipped default.
- Upstream Lean gaps → enumerated here, never invented; landings rebind.

## 12. Provenance + freeze record + freeze-validation (single handoff check)

Sources (read-only; newest governs; no blocking conflicts): v3 mandate+map;
bodies #30 (2026-09-06 correction), #29 (Lean correction), #33, #34; Lean
Vote 7-file extent READ IN FULL at r5 (Types/State/Event/Validate/Fold at
intake + re-verified; Invariants 1228 lines + Tests 397 lines at r5) +
KelGroups/Event.lean (52) + Types.lean (166) full at r5 + State/Validate/
Integration heads (CONTEXT granularity, stated); V-2 + #68 OPEN; #81 body;
R3.1; S28 @ `933e385d` (+ S28 gate v10.2 shape); commissioning note;
NOTE-001/002 (clock + helper rules); NOTE-003 + parent assessment (r2);
NOTE-004 (r3); NOTE-005 (r4); NOTE-006 (this r5 — falsehoods owned above).
Inbox checked before r5 filing (NOTE-006 read + acked; no other unread).
Spend: 0/0/0/0. Skills: orchestrator-contract, ticket-orchestrator,
resolve-ticket (planning only), context-compiler, worker-protocol,
tmux-orchestrator, verification, invariants, gate-script, haskell, nix,
lean4 (read-only).

Freeze-validation (ticket owner, before any GREEN claim): (i) §7-PERFILE
extent re-listed live == frozen 7 + per-file status holds; (ii) Lean pin +
immutable byte-hashes + clean sample GREEN; (iii) mapping rows resolve
live both sides; (iv) 5 drift probes demonstrated (GREEN + 4 REDs);
(v) .hi discovery exactness pre-check (exactly-one fresh `.hi` per frozen
module or INCONCLUSIVE abort) + inventory hash == frozen; (vi) M10b
instrument contains the totality-witness `case` over the frozen allowed
set; (vii) B19 + classification procedure present with named attribution
fields; (viii) B22 scratch pre-check (export + trivial GREEN build before
overlay edit; failure ⇒ BLOCKED, never skip); (ix) M15 instrument present
(filter-drop splice + REQ-SWEEP-IDEM witness); (x) identity-table rows all
resolved (zero unclassified declarations). Any (i–x) failure ⇒ re-freeze,
never proceed. No prerequisite, no measurement request: all of (i–x) is
plannable exactly as specified.

Hashes: base `933e385df2f2a251bb54a08bb7663f0d41fafb64`; Lean
`3590c0015b84fd58004bf6fb44dd18b107304c48`; brief
`f6d857639a4f3d9aa466c081305f43ae9e59bad4fd9dcb790a08bd85fc856416`.
