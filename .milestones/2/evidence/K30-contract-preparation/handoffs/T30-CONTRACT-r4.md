# T30-CONTRACT-r4 — kelgroups #30 substrate vote interface + closure evidence (FOR IMPLEMENTATION AUTHORIZATION)

Ticket preparation owner `t30-contract` (Muse), 2026-09-06. Preparation-only:
read-only inspection + own planning artifacts. No compilation, tests,
mutations, gate runs, dispatch, product edits, commits, push/PR/merge/release,
issue comments, or spend (spend 0 throughout).

- Brief sha256 `f6d857639a4f3d9aa466c081305f43ae9e59bad4fd9dcb790a08bd85fc856416`.
- SUPERSEDES r3 (r1/r2/r3 retained unmodified; all COMPLETEs stand). This r4
  is the NOTE-005 packet: **actual live mechanisms with named oracles, or an
  explicitly labeled limit — never regex certified as semantic inventory,
  never output-copy controls standing in for source-boundary controls, never
  templates, never universals, never predicted truth tables.** No unmet
  prerequisite remains: everything below is plannable without execution, and
  §12 names the single validation the ticket owner performs at freeze.
- Operative base kelgroups `main` @ `933e385df2f2a251bb54a08bb7663f0d41fafb64`
  (S28 LANDED, PR#32; CI+Release SUCCESS; #28 CLOSED). Accepted Lean
  Reactivegas @ `3590c0015b84fd58004bf6fb44dd18b107304c48` (zero-diff EMPTY
  vs `4a6cd87` on Vote + Integration/State/Validate; landings rebind
  explicitly, never anticipated).
- Companion: `handoffs/T30-COMMAND-MAP-r4.md` (same frozen content,
  row-addressable). This contract ≠ the ticket owner's final immutable gate.

## 0. r4 corrections (N5-1–N5-6 — mechanism or labeled limit, nothing else)

- **N5-1 (no certified regex):** r3's file-set + NON-EMPTY could not show
  every required item was discovered (Haskell header-patterns miss arm
  removal; Lean `| lower` matches matches/proofs too). FIXED three ways:
  (a) Haskell arm/type discovery moved to COMPILER METADATA —
  `ghc --show-iface` over B3's own `.hi` products (only compiled code has
  `.hi`; fictitious entries impossible) plus -Werror exhaustiveness live
  every build; all Haskell semantic-grep dropped. (b) Lean declaration
  patterns demoted to REVIEW PROMPTS (unreconciled counts must be explained
  in the record; never evidence, never a kill — the invariants firewall:
  source text is a lead). (c) Lean baseline completeness = rigorous review
  under the §7-PERFILE rule with signed record (enforced:REVIEW, labeled —
  no mechanism claimed where none exists). Regression from the reviewed
  baseline = live refusal machinery (§7-DRIFT). An in-contract BAN: no
  regex output is ever cited as semantic inventory, anywhere.
- **N5-2 (controls through the source boundary):** r3's directional
  controls edited OUTPUT COPIES (join-after-extraction only). FIXED:
  output-copy controls DELETED (superseded, stated). The live run binds
  ACTUAL consumed bytes — per-file `sha256sum` + HEAD pin + enforced-empty
  `git status --porcelain` in BOTH checkouts in-leg (HEAD pins a commit,
  never uncommitted bytes; the hash list + clean-tree bind the working-tree
  bytes; single-leg atomicity, no interleaved writes). Directional
  demonstration moved to INTENTIONAL OVERLAYS, explicitly accounted:
  scratch `git archive` export + ONE synthetic edit, export-diff bound in
  evidence, run through hash-bind + join end-to-end — labeled precisely as
  trigger-discrimination evidence on source-shaped bytes, NOT review
  correctness. TOCTOU stated and closed by single-leg binding.
- **N5-3 (actual extent + per-file rule):** r3 froze a 5-file list against
  a live 7-file Vote dir. FIXED: the frozen extent names the ACTUAL live
  extent, observed read-only at intake — `lean/KelGroups/Vote/` contains
  exactly Event, Fold, Invariants, State, Tests, Types, Validate (7 files;
  nothing whitelisted away, no requirement dropped). §7-PERFILE gives each
  module its reason: Types/Event/Validate (inductives — MUST emit ctor
  rows); State/Fold (structures + equation groups at the enumerated Lean
  sites — MUST emit); Invariants (theorems/proofs ONLY — expected-empty,
  reason stated); Tests (witness executables — expected-empty, reason
  stated). Empty rules explicit: expected-empty files named; any other file
  emitting nothing → RED; empty-GLOBAL → RED always.
- **N5-4 (full-audit accountability):** every drift direction reruns under
  the FULL audit (auditor drift probes: binding GREEN + all 5 directionals,
  named — no inheritance from owner evidence). Stale conditional language
  deleted: all 14 A-K reruns UNCONDITIONAL (REVIEW now applies ONLY to
  records — L-1–L-7 as recorded-with-owner; tripwire output; B19-log read
  alongside the A-OMIT rerun — each named). SLIM itemized (S1 slim-build,
  S2 slim-test, S3 slim-ci — the envelope's three whole-project
  invocations; legs 1/2/2b/7 ride charge-0/probe). A-REBIND both branches
  stated (landed: 1B execute; unlanded: returned unspent WITH reason).
- **N5-5 (bounded G4, universals deleted):** r3's universal
  'UNENFORCEABLE by test' DELETED. The BOUNDED property for this ticket's
  frozen S30-allowed proposal surface: each allowed ctor maps (D2 coverage
  join) + each mapping enacts non-admitting (M10b GREEN-ENUM) + vocabulary
  closed at enactment (M10a COMPILER). Translation-totality needs no new
  run: a new BaseProposal ctor breaks the D2 coverage join (demonstrated by
  P-DRIFT-ADD direction) AND the M10b totality-witness `case` in B4 (stated
  instrument shape). Review-only remainder named: the frozen allowed-set
  matching product intent (human judgment, labeled). Direct-only admission
  unweakened (M10a + route rows intact). Untracked-ctor totality stands as
  compiler-totality however labeled.
- **N5-6 (witnesses, not predictions):** r3's predicted M4a/M4b truth
  tables DELETED. Kill criteria are MINIMAL (M4a: SWITCH REDs; M4b: RECAST
  REDs); co-effects follow the CLASSIFICATION PROCEDURE (§8): every extra
  RED example recorded + attributed (same-cause / independent-defect /
  setup-failure — setup is one category, never the default); a run whose
  criterion REDs beside UNATTRIBUTED extras is INCONCLUSIVE until
  classified. Isolation rule: single-site splice per run, diff-hash bound,
  each RED example gets a named attribution line. POSTSWITCH stays REQUIRED
  (G5 stands); the ticket owner records OBSERVED signatures at freeze as
  characterization (freeze deliverable), never as pre-stated acceptance.
- **Counts unchanged and still justified:** owner 23 (RED 2 + GREEN 18 +
  SLIM 3 = S1–S3 itemized), auditor 22; above-20 builds still M4b←D4,
  M13←D5, B19←D3. N5-5's instrument reuses B4 + D2 (zero added builds);
  N5-1's .hi inventory rides B3/B15 products (zero added builds); N5-2's
  overlays ride the 5 drift probes. No prerequisite, no measurement
  request: every mechanism below is plannable exactly.

## 1. Objective (one observable — unchanged)

A nondegenerate application opens an app-scoped assent question, casts
ballots as the franchise, and observes the verdict (positive AND negative,
each with explicit cause) plus its closure record through the **integrated**
boundary — refusals before durable effects, replay equality. Test-only
proving instance (as S28); the user demo is `paolino/kelgroups#33`.
Threshold a parameter at every evaluation; exhibits never defaults.

## 2. Scope — rows (FULL #30 scope; N5-3 extent + N5-5 bounded surface applied)

Accepted behaviours INTACT: converge to the LANDED S28 interface, never
redesign. `Trivial` degenerate presence-only. No unilateral Lean edits. No
threshold default. No expiry. No votable admission. No second store/fold.

| id | requirement | accepted evidence (@3590c001) | deliverable | depends on | Terms statement |
|---|---|---|---|---|---|
| R30-1 | openQuestion (collective + permission-with-designee), responsabile-only admission | `Vote/Event.lean`; `Validate.lean:57` | Haskell #30 | none | intact |
| R30-2 | cast: one-position placement, idempotent re-cast, switch moves | `placeBallot` (`Fold.lean:53-56`); `effectedState` (`:95-100`); `Validate.lean:59-64`; guarded `setInsert` (`Types.lean:46-47`) | Haskell #30 | none | transcribed; M4a/M4b minimal criteria + classification (N5-6) |
| R30-3 | sweepClosures same-step close + ClosureRecord (tally + franchiseChange); retention, never silent drop; NO expiry | `sweepStep`/`sweepClosures` (`:65-76`); append-only `closed` + R-51/R-61; no clock (R-54) | Haskell #30 | none | lifecycle neither ahead nor out |
| R30-4 | verdictOf: collective threshold @ franchise (legacy order) + permission designee arm (never tally) | `verdictOf` (`State.lean:82-96`); R-46/49/50/64 | Haskell #30 | none | parameter everywhere |
| R30-5 | refusals produced now; `notDesignee`/`notProposer` DECLARED (`Validate.lean:41-42`), zero Slice-A sites — unruled INTENTION | 4-ctor error + 3-arm validation (`:56-70`); #81 out-of-scope § | 4-ctor vocabulary; NO producing semantics | NONE (preserved boundary) | UNSCHEDULED; tripwire only |
| R30-6 | franchise from canonical GroupView every evaluation | `franchise`/`franchiseSize`/`isResponsabile`; R62-11; S28 `groupView` | Haskell #30 | none | never a local copy (M13) |
| R30-7 | negative-verdict delivery observable at boundary | `ClosureRecord`; negative arm; S28 hook surface | interface #30; consumption → #76 | #76 effect only | evidence-exposure only |
| R30-8 | vote routing separated from base admission | T6222; non-admitting `pendingBase`; S28 direct-only | boundary definition #30 | none | intact (N5-5 bounded three-part surface property) |
| R30-9 | approve path matches LANDED base rule; V-2 rebind after landing | V-2 settled; #68 OPEN | REBIND after #68; freeze meanwhile | #68 ONLY at `tryEnactBase` + proposer rules | concrete boundary, no blanket block |
| R30-10 | RULED lifecycle — MECHANISM SURFACE ONLY (hooks shaped for extension; 4-cause DATA shape; append-only; atomic discard — S28-existing); never exercised for absent closes | V-5; carried causes (`Types.lean:76-77`); renounce→gs verified no-op; #81 §1–§3 | surface + extension validation; CONTENT → #81 | #81 (L-1–L-6); #76 (L-7) | D1: no executable #81 rows |
| R30-10U | UNRULED refusal policy, NOT scheduled; boundary PRESERVED | #81 out-of-scope § | NONE | — | UNSCHEDULED |
| R30-11 | verdict → economic effect | NOTE-016/A-Q001 (ruled, wire missing) | #76 (evidence only) | #76 | neither ahead nor out |
| R30-12 | PureScript client propose/vote (adapt-only) | `kelgroups-client` transport | #30 client additions | none in team | adapt-only INCLUDED; UI/wasm NOT. LIMIT: TEST-boundary roundtrip; production-server roundtrip out of scope |
| R30-13 | Lean proofs | Vote proofs = evidence; Slice-B producers only where RULED | LEAN-OWNED; zero kelgroups Lean edits | Lean lanes where ruled; none where unruled | no invented obligations |
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
AND negative verdicts with causes + retained records; pre-effect refusal
(state AND log unchanged); accepted-KEL replay identical; client
propose/vote.

Requirements: R30-1–R30-8 + R30-12 + R30-14 now; R30-9/R30-10-content/
R30-11/R30-13 as dependencies; R30-5-producing + R30-10U unscheduled;
R30-X fences. Parameterized threshold; canonical-view franchise; refusals
first; replay equal; retained closures.

Rejection behaviour: vote-path `notResponsabile`/`questionNotFound`;
substrate-path S28 identities; refusal advances nothing.
`notDesignee`/`notProposer` vocabulary only. No expiry refusal anywhere.

Production-path examples (in-suite, real step/fold): REQ-ADMIT-PATH
(admitted open: state + log); REQ-OPEN-REFUSE + REQ-CAST-NONRESP (exact
error, aggregate AND log unchanged); REQ-NONDECIDE-PERM (recorded-but-
not-deciding: tallies hold the ballot, verdict open — exercised, never
widened nor narrowed).

Success: S1–S6 + all §7-REQ through the integrated boundary on the
candidate SHA (§10); `Trivial` intact; full `just ci` green; fresh audit
PASS; no shipped defaults; no silent drops.

## 4. Plan

Single slice S30-1 (FULL boundary: mirrors + wiring + persistence + client
+ proving instance). No S30-2. RED absence proof first (C1/C2 — absence
ONLY); GREEN in envelope (14 mutant runs + M10b enumeration + live drift
machinery + REQ set); B19 omission challenge (sole guard falsification);
SLIM (S1–S3 itemized); FULL audit (N5-4 independent establishment); draft
PR + remote CI; acceptance. Ticket owner freezes the final gate; the §12
freeze-validation list is the single handoff check.

Constraints: this packet cost 0; §8 fence; whole-project invocation =
BUILD; S28 concurrency discipline (serialized append, refusal-before-codec;
no second writer); no parallel heavy builds.

Live boundaries (can-fail controls): step/validate agreement; Store/KEL
append + replay (equality; founding guard); client TEST-boundary roundtrip
(limit stated); drift input-binding + pin + join (N5-1/N5-2); compiler
metadata inventories (N5-1). No behavioural proof by source text, no
shrinkable inventories, no absent-import as behaviour.

Order: S30-1 → audit → PR → #33 → #34. #68 touches only R30-9 on landing.

## 5. Models (compact; no implementation content)

Modules: new `KelGroups.Vote.*` mirrors (depend on `GroupView` + S28
`State`); `KelGroups.Fold` owns `Integration` composition (proving
`AppState` or dedicated fold — ticket owner decides; `BaseProposal`
reading, `proposalMutation → BaseMutation`, `digest`, hook composition
nowhere else); `KelGroups.Store` owns the vote path (existing integrated
path; no second writer/tables); `kelgroups-client` owns propose/vote
(existing transport, adapt-only); proving instance owns the test-only app.
Drift mapping + discovery + REQ list are frozen gate artifacts.

Data: `Question{kind,proposer,assents,dissents}` (no time-like field);
`ClosureRecord{questionId,question,verdict≠open,cause}` (4 causes as DATA);
`VoteState{openQuestions,closed-append-only}`; `Verdict`; `Threshold`
(parameter); `QuestionKind{collective,permission(designee)}`;
`ClosureCause{4}`; `VoteEvent{openQuestion,cast,renounce}` (T6222);
`VoteError` (2 produced + 2 vocabulary-only). Franchise never stored;
verdict single site; closure = remove + append atomically.

Functions (spellings frozen by ticket owner): vocabulary constructors;
`verdictOf`; `franchise`/`franchiseSize`/`isResponsabile`; `lookupQuestion`;
`closureCause`; exhaustive `validateVoteEvent`; `placeBallot`;
`sweepStep`/`sweepClosures`; `effectedState` (authorization-free);
`applyVoteEventChecked` (integrated path uses it, never revalidates);
`foldVote`/`foldFrom`; wiring (`digest`, `proposalMutation`, vote-aware
`appFold`/`baseHook`). `GroupView`/`Threshold` explicit everywhere.

## 6. Tasks (slice S30-1)

- T30-1 RED absence proof (C1/C2, absence ONLY).
- T30-2 vocabulary + state mirror (+ REQ opens/verdicts/reads).
- T30-3 validation mirror (3-arm exhaustive; 4-ctor vocabulary, two
  site-less; tripwire noted).
- T30-4 placement + effects (M4a/M4b minimal criteria; classification
  procedure; open-never-overwrites; renounce no-op preserved).
- T30-5 sweep + closure + retention (PRODUCED causes only).
- T30-6 `Integration` wiring (current base).
- T30-7 persistence path (append + replay + founding guard; no second
  writer).
- T30-8 client additions (adapt-only; TEST-boundary; limit stated).
- T30-9 mechanism-surface shaping (L-1–L-7 recorded with owners; zero
  executable #81 rows).
- T30-10 drift mapping + live discovery + REQ set (§7-DRIFT, §7-PERFILE,
  §7-REQ frozen names/slugs).
- T30-11 GREEN envelope + omission challenge + SLIM + CI + hygiene.
- T30-12 audit handback + PR (N5-4 establishment; draft post-GREEN;
  exact-SHA merge at desk).

## 7. Frozen requirement-to-command/control map (binding)

Conventions: whole-project invocation = 1 BUILD. Per-mutant cycle = apply +
run + revert, hash-verified restore (failure aborts exit 3). Hidden
invocations forbidden: every cited result maps to a counted B-row or
PROBE-row. Predicates: COMPILER-kill (exit≠0 + diagnostic quotes ctor/site
+ zero parse-error lines); TEST-kill (exit≠0 + `Failures:` names ≥1
registered REQ-ID; empty/crash/timeout/infra/parse NEVER count);
GREEN-ENUM (exit 0 + lists every allowed ctor). Setup/infra failure =
INCONCLUSIVE abort, never kill. Charge-0 recon free, never evidence.

§7-CMDS (exact; toolchain re-pinned at freeze):
`nix develop .#ci --quiet -c just build`;
`nix develop .#ci --quiet -c cabal test all -O0 --test-show-details=direct`;
`nix develop .#ci --quiet -c just ci`;
probes `nix develop .#ci --quiet -c cabal test invariants
--test-option=--match --test-option=/S30-<Group>/<REQ-ID>/` (Groups frozen
below — fully determined strings);
`nix --version` + one batch `ghc/cabal/lake/node/spago/just --version`.

§7-REQ (frozen Groups): `S30-Open`: REQ-OPEN-COLL, REQ-OPEN-PERM,
REQ-OPEN-REFUSE, REQ-OPEN-DUP. `S30-Cast`: REQ-CAST-ASSENT, REQ-CAST-SWITCH,
REQ-CAST-POSTSWITCH, REQ-CAST-RECAST, REQ-CAST-UNKNOWN, REQ-CAST-NONRESP.
`S30-Sweep`: REQ-SWEEP-TALLY, REQ-SWEEP-DISSENT, REQ-SWEEP-FRANCHISE,
REQ-RETAIN, REQ-NOEXPIRY. `S30-Verdict`: REQ-VERDICT-COLL, REQ-VERDICT-PERM.
`S30-Franchise`: REQ-FRANCHISE-CURRENT. `S30-Negative`: REQ-NEG-DELIVER.
`S30-Route`: REQ-ROUTE-ENUM. `S30-Lifecycle`: REQ-HOOK-EXT,
REQ-RECORD-SHAPE. `S30-Client`: REQ-CLIENT-ROUNDTRIP. `S30-Admit`:
REQ-ADMIT-PATH, REQ-NONDECIDE-PERM. Cross-check: every ID registered +
executed else RED. B19 (one ID removed → RED) is the ONLY guard
falsification; C1/C2 absence-only.

§7-PERFILE (N5-3 — actual 7-file extent, observed read-only: Event, Fold,
Invariants, State, Tests, Types, Validate — nothing dropped):
Types/Event/Validate carry inductives (MUST emit ctor rows — vocabulary);
State/Fold carry structures + equation groups at the enumerated sites
(MUST emit structure + equation-group rows); Invariants carries
theorems/proofs ONLY (expected-empty of checked kinds — reason: proof-only
module, no vocabulary/equations of its own); Tests carries witness
executables (expected-empty — reason: witness-only module). Empty rules:
expected-empty files named (Invariants, Tests); any other file emitting
nothing → RED; empty-GLOBAL → RED always. The brief's 5-file evidence list
is the Haskell-mirror obligation set; the 2 proof/witness modules stay in
the reviewed extent with expected-empty status — scope unchanged, extent
honest.

§7-DRIFT (N5-1/N5-2 live machinery — oracles named per mechanism):
- L1 INPUT BINDING (oracle: sha256sum contract + git): per-file
  `sha256sum` over the §7-PERFILE extent + `git rev-parse HEAD` ==
  `3590c001` (Lean) / frozen base (Haskell) + enforced-empty
  `git status --porcelain` in BOTH checkouts in-leg, else RED before any
  comparison. Single-leg atomicity (bind, verify, compare; no interleaved
  writes). Answers N5-2's HEAD critique: the pin is necessary, the hash
  list + clean-tree supply sufficiency for consumed bytes; generator-side
  mutation trips clean-tree; TOCTOU closed by atomicity (stated).
- L2 ELABORATION VALIDITY (oracle: the Lean elaborator): `lake build`
  exit 0 in the pinned checkout — the bound bytes are genuine valid Lean
  (binds validity, never inventory).
- L3 COVERAGE JOIN (oracle: overlay-demonstrated discrimination): frozen
  mapping (Lean item → Haskell type + executing REQ-IDs, with file:line
  PROVENANCE — review aid, never mechanism) joined against live emissions;
  unmapped-emitted or dangling-mapping → RED.
- L4 HASKELL COMPILER METADATA (oracle: GHC — only compiled code has
  `.hi`): `ghc --show-iface` inventory over B3's own `.hi` products for
  `KelGroups.Vote.*` (zero added cost — B3 products), hash-pinned at
  freeze, diff-triggered every build (any byte change → RED + mandate
  review). Directional demonstration rides B15: the admitted-ctor splice
  changes the `.hi` — the inventory diff MUST fire inside B15's log as
  secondary evidence (primary kill stays the COMPILER predicate). No
  separate overlay rebuild needed; stated why.
- L5 ARM TOTALITY (oracle: GHC -Werror, live every build): exhaustiveness
  failures are build REDs; M10a's break doubles as the live demonstration
  that non-exhaustiveness fires (secondary evidence in B15's log).
- Haskell mapped-function presence: compilation proves existence;
  leg-4 execution of the row's REQ-IDs proves exercisedness (mapping row
  carries its REQ-IDs; cross-check enforces). No Haskell semantic-grep
  exists in this packet — dropped entirely (N5-1).
- Lean declaration patterns (`^(structure|inductive)`, inductive arms)
  exist ONLY as REVIEW PROMPTS: unreconciled declaration counts must be
  explained in the signed record; never evidence, never a kill (BAN:
  regex output is never cited as semantic inventory, anywhere).
- Baseline completeness = rigorous review under §7-PERFILE (full reads,
  count-prompts reconciled, signed record) → enforced:REVIEW, labeled
  (oracle: signed record + epic source-verification, observed practice in
  the r1/r2/r3 assessments — no mechanism claimed where none exists).
- Regression from baseline = L1–L5 refusal machinery (live).
- Temporal hole → explicit-rebind process rule (landing ⇒ re-freeze +
  re-demonstration of all five drift probes before acceptance).
- REQUIRED drift probes (named): P-DRIFT-GREEN (live recon GREEN);
  P-DRIFT-ADD (scratch export + synthetic ctor → RED, unmapped-Lean);
  P-DRIFT-SRCOMIT (export − one real line → RED, dangling mapping);
  P-DRIFT-MAPOMIT (mapping − one row → RED, undiscovered-unmapped);
  P-DRIFT-FILEADD (file list + synthetic file → RED). Overlays =
  `git archive HEAD` export + ONE intentional edit, export-diff bound in
  evidence, through hash-bind + join end-to-end — labeled as
  trigger-discrimination evidence on source-shaped bytes, NOT review
  correctness. Output-copy-only controls: DELETED (superseded, stated).

§7-MAP (obligation → owner → auditor; RE-RUN vs REVIEW):

| obligation | owner | auditor |
|---|---|---|
| absence T30-1 | B1 + B2 RED (absence ONLY) | A-RED1/A-RED2 frozen-BASE reruns (never the candidate calls) |
| R30-1 | B4 + probes | A-TEST (RE-RUN); predicate reviews |
| R30-2 | B4; B6 M2; B7 M3; B8 M4a (criterion: SWITCH REDs); B9 M4b (criterion: RECAST REDs) + §8 classification of extras | A-TEST; A-K reruns (ALL 14 UNCONDITIONAL) |
| R30-3 | B4 (append + replay); B10 M5; B11 M6; B12 M7merged; B14 M9 | A-TEST + A-Ks; M6 rerun = named boundary rerun |
| R30-4 | B4; B13 M8 | A-TEST |
| R30-5 | B3 cold (vocabulary + 3-arm exhaustive) | A-COLD; tripwire output REVIEW (never kill) |
| R30-6 | B4; B18 M13 snapshot → REQ-FRANCHISE-CURRENT RED | A-TEST; A-K13 |
| R30-7/14 | B4 boundary + roundtrip | A-TEST; A-K6 |
| R30-8 + bounded surface (N5-5) | B3 (2-arm enactment); B15 M10a (COMPILER/CLOSED-totality); M10b GREEN-ENUM named in B4 (per-ctor enactment THROUGH a totality-witness `case` over the frozen allowed set — instrument shape); translation-totality control = D2 coverage join (new ctor ⇒ unmapped ⇒ P-DRIFT-ADD RED) + witness `case` in B4 | A-COLD; A-K10a (UNCONDITIONAL — stale conditional deleted); M10b re-checked in A-TEST |
| frozen allowed-set matches intent | REVIEW (human judgment, labeled — the named review-only remainder) | record review |
| R30-9 | current-base freeze (B3/B4) | A-REBIND iff #68 landed (author integrates + fresh final-SHA audit; auditor never repairs) |
| R30-10 surface | B4 (hook pre/post; 4-cause DATA; append-only; atomic discard); B16 M11 → REQ-HOOK-EXT RED | A-TEST; A-K11 |
| produced-cause distinction | B12 M7merged (forced-.tally → REQ-SWEEP-FRANCHISE RED; carried excluded) | A-K7 |
| L-1–L-7 | recorded with owners; NO command | record-only review |
| R30-10U/PROD, R30-11 | no command (boundary; no wire/mock) | — |
| R30-12 | B20 leg-6 (`spago build` + `spago test`, TEST-boundary, limit stated); B17 M12 → ROUNDTRIP RED | A-CI; A-K12 |
| R30-13 | B20 `lake build` green only | A-CI |
| drift N5-1/2 | 5 REQUIRED drift probes | binding GREEN rerun + all 5 directional reruns (auditor PROBE ×6, named) |
| guard D3 | B19 omission rerun (ONLY falsification) | A-OMIT rerun (+ B19-log read alongside — labeled read, not second invocation) |
| cold/final | B3 1B + B20 final CI 1B + tracked-clean + Trivial-only + founding guard | A-COLD/A-CI |
| SLIM | S1 slim-build + S2 slim-test + S3 slim-ci (itemized; legs 1/2/2b/7 ride charge-0/probe; drift GREEN recon probe inside cap) | — |

Mutant ledger (14 runs B5–B18): M1 openQuestion-nonresp bypass
(REQ-OPEN-REFUSE); M2 cast-nonresp bypass (REQ-CAST-NONRESP); M3
unknown-accept (REQ-CAST-UNKNOWN); M4a erase-drop (criterion SWITCH REDs;
extras classified per §8 — POSTSWITCH REQUIRED, G5 stands); M4b
unguarded-insert (criterion RECAST REDs); M5 tally-suppress
(REQ-SWEEP-TALLY); M6 dissent-suppress (REQ-NEG-DELIVER); M7merged
cause-forced-tally (REQ-SWEEP-FRANCHISE + produced-cause distinction; site
closureCause; single collapse, counted once); M8 permission-tally-consult
(REQ-VERDICT-PERM); M9 close-and-discard (REQ-RETAIN); M10a
admission-ctor-added (COMPILER/CLOSED-totality + secondary .hi-diff +
exhaustiveness-fire evidence in-log); M11 hook-ignored (REQ-HOOK-EXT);
M12 propose-path dropped (REQ-CLIENT-ROUNDTRIP, client boundary); M13
snapshot-in-payload (REQ-FRANCHISE-CURRENT). M10b enumeration (GREEN-ENUM
in B4): per-ctor enactment through totality-witness `case`; interface
proposition, never behavioural refusal.

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
+ REQ list (frozen gate artifacts). FENCED — `lean/**`; historical bodies;
`Trivial.hs` (presence-only, uncounted); S28 production outside named
points; `client/` beyond adapt-only; UI/wasm/economics; release metadata.

Legs: 1 hygiene before/after; 2 identity + self-hash + ancestry (HEAD
descends from frozen slice base); 2b pins exact, fail-closed (re-pinned at
freeze); 3 build cold/warm; 4 inventory (≥K groups) + registered (REQ
cross-check: every ID registered, total == file examples, no orphans —
POSTSWITCH REQUIRED) + execution (all executed, none pending; historical
green) + exit 0; 5 mutants B5–B18 (tracked-clean committed candidate at
recorded HEAD; apply+run+revert; hash-verified restore; failure aborts
exit 3) under MINIMAL criteria + the CLASSIFICATION PROCEDURE (every extra
RED recorded + attributed same-cause/independent/setup — setup never
default; unattributed extras ⇒ INCONCLUSIVE until classified); DRIFT leg
(L1–L5 + 5 controls); M10b GREEN-ENUM in leg-4 scope; 6 full `just ci`;
7 `Trivial` presence + client CI presence. `set +e`; full log + per-leg
sha256; meta file. B19 = separate counted leg-4 rerun minus one REQ-ID
(RED required); C1/C2 absence-only. Isolation: single-site splice per run,
diff-hash bound, named attribution line per RED example. Freeze
deliverable: ticket owner records OBSERVED mutant signatures as
characterization (not acceptance — N5-6).

Kill-attribution: COMPILER (M10a) / TEST (behavioural, REQ-ID-naming) /
GREEN-ENUM (M10b). Deviation from criterion + unclassified extras =
INCONCLUSIVE abort, never kill.

Evidence: `run-receipt`-style capture per cited run; self-hash + ancestry
leg 2; mutant diff hashes pre-run; restore hashes post-run; overlay
export-diffs bound; .hi inventory hashes bound.

Spend classes: BUILD (whole-project = 1); PROBE (narrow: exact-REQ-ID
`--match`, single-component build, drift runs = 1 vs 24 cap);
CHARGE-0 (enumerated recon — free, never evidence); AUDIT-BUILD/AUDIT-PROBE
(same, auditor cap). No parallel heavy builds; every failed setup/attempt
journaled; no automatic raises (exact gap first).

## 9. Operational classifications + proposed ceilings (FROZEN with §7)

Owner: B1–B2 RED (2) + GREEN 18 (B3 cold + B4 test + B5–B18 fourteen runs +
M10b named in B4 + B19 omission + B20 CI) + SLIM S1–S3 (3) = **23 builds**.
Above-20: M4b←D4, M13←D5, B19←D3 (each ordered). M10b in B4; drift in probe
cap; M14 merged away. Probes ≤24: 5 REQUIRED named
(P-DRIFT-GREEN/ADD/SRCOMIT/MAPOMIT/FILEADD) + kill-confirm ≤14 (ambiguous
logs only — B-logs already quote kills) + dispute ≤3 + transient ≤2,
REQUIRED-first. **PROPOSED owner 23/24** (unchanged from r3 — N5 adds zero
invocations: .hi rides B3/B15 products, overlays ride drift probes,
witness-shape is instrument content). PROPOSALS pending fit-proof at freeze
+ authorization; gap returns exact cost, never trimmed scope.

Auditor (pre-dispatch, exact; N5-4): A-RED1/A-RED2 frozen-BASE 2B; A-COLD
1B; A-TEST (REQ cross-check + M10b re-check) 1B; A-CI 1B; A-K×14 14B
(UNCONDITIONAL — stale conditional deleted); A-OMIT 1B; A-REBIND
conditional 1B (landed: execute; unlanded: unspent WITH reason); A-RESERVE
1B; drift binding GREEN + 5 directional reruns from probe cap (PROBE ×6,
named). Undisputed handling: still RE-RUN for all 14 kills; REVIEW ONLY
for records (L-1–L-7), tripwire output, B19-log-alongside-A-OMIT (each
named). Named sharing: M6/A-K6 and M12/A-K12 are the boundary reruns (no
separate builds). **PROPOSED auditor 22/24** (22B; probes 6 required drift
+ finding-narrowing ≤14 + reconfirm ≤4 = 24). Bottom-up; coverage never
trimmed. Seat fresh Codex-or-Grok (never Muse/GLM/Claude), clean detached
worktree at candidate SHA, argv-pinned model+effort, post-cursor START,
hash-bound report; recommends, ticket owner decides; every repair gets a
fresh auditor.

Team (standing fence, commissioned at authorization — NOT by this packet):
Muse ticket owner → Muse commit owner (`draft=NONE`) → fresh Codex-or-Grok
auditor per submission (two max, one bounce, repair re-audited); signed
commits; draft PR post-GREEN; exact-SHA merge at desk.

## 10. Acceptance (binding when commissioned)

Threshold-parameterized verdicts (permission never tallies); retained
records with PRODUCED causes (never silent; no expiry); refusal pre-effect
(aggregate AND log unchanged; accepted-KEL replay identical); validate/fold
agreement (same triple; never historical validator, never double-wrap);
negative delivery at the boundary (both verdicts with causes via step +
append + replay); zero producing sites (tripwire clean at review); §7-REQ
complete (registered + executed; B19 RED observed); drift GREEN + all four
directional REDs observed with pin + byte-hashes + clean-tree bound;
.hi inventory hash matches frozen (drift → RED + review); M10a
COMPILER-kill (+ in-log .hi-diff + exhaustiveness-fire secondaries) +
M10b GREEN-ENUM observed; N5-5 three-part surface closed with review-only
remainder labeled; M4a/M4b criteria met with extras classified (no
unattributed REDs); client additions under client CI (limit stated);
`Trivial` intact; full `just ci` green; tracked-clean both ends; founding
guard held; L-1–L-7 recorded with owners (reviewed as record); fresh audit
PASS complete; bounded claims only.

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
Vote (7-file live extent observed) + Integration/State/Validate @ `3590c001`
(zero-diff EMPTY vs `4a6cd87`); V-2 + #68 OPEN; #81 body; R3.1; S28 @
`933e385d`; S28 gate v10.2 shape; commissioning note; NOTE-001/002 (clock +
helper rules — helper-stamped STATUS since 04:11:04Z); NOTE-003 + parent
assessment (r2); NOTE-004 (r3); NOTE-005 (this r4 — mechanism or labeled
limit). Inbox checked before r4 filing (NOTE-005 read + acked; no other
unread). Spend: 0/0/0/0. Skills: orchestrator-contract,
ticket-orchestrator, resolve-ticket (planning only), context-compiler,
worker-protocol, tmux-orchestrator, verification, invariants, gate-script,
haskell, nix, lean4 (read-only).

Freeze-validation (ticket owner, before any GREEN claim): (i) §7-PERFILE
extent re-listed live == frozen 7 + per-file status holds; (ii) Lean pin +
byte-hashes + clean-tree GREEN; (iii) mapping rows resolve live both sides;
(iv) 5 drift probes demonstrated (GREEN + 4 REDs); (v) .hi inventory hash
== frozen; (vi) M10b instrument contains the totality-witness `case` over
the frozen allowed set; (vii) B19 procedure + classification procedure
present in the frozen gate with named attribution fields. Any (i–vii)
failure ⇒ re-freeze, never proceed. No prerequisite, no measurement
request: all of (i–vii) is plannable exactly as specified.

Hashes: base `933e385df2f2a251bb54a08bb7663f0d41fafb64`; Lean
`3590c0015b84fd58004bf6fb44dd18b107304c48`; brief
`f6d857639a4f3d9aa466c081305f43ae9e59bad4fd9dcb790a08bd85fc856416`.
