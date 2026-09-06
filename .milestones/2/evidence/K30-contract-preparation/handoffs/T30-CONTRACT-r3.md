# T30-CONTRACT-r3 — kelgroups #30 substrate vote interface + closure evidence (FOR IMPLEMENTATION AUTHORIZATION)

Ticket preparation owner `t30-contract` (Muse), 2026-09-06. Preparation-only:
read-only inspection + own planning artifacts. No compilation, tests,
mutations, gate runs, dispatch, product edits, commits, push/PR/merge/release,
issue comments, or spend (spend 0 throughout).

- Brief sha256 `f6d857639a4f3d9aa466c081305f43ae9e59bad4fd9dcb790a08bd85fc856416`.
- SUPERSEDES r2 (retained with r1, both unmodified; both COMPLETEs stand).
  This r3 is the ONE coherent corrected packet ordered by NOTE-004: not five
  disconnected edits but a single principle applied everywhere — **every
  claim is bound to a live, independently-established mechanism, or carries
  its explicit limit labeled in-contract**. G1–G5 are five instances of that
  principle (live source binding; independent establishment; no templates;
  honest proposition scope; no double-counted rows).
- Operative base kelgroups `main` @ `933e385df2f2a251bb54a08bb7663f0d41fafb64`
  (S28 LANDED, PR#32; CI+Release SUCCESS; #28 CLOSED). Accepted Lean
  Reactivegas @ `3590c0015b84fd58004bf6fb44dd18b107304c48` (zero-diff EMPTY
  vs `4a6cd87` on Vote + Integration/State/Validate; later landings rebind
  explicitly, never anticipated).
- Companion: `handoffs/T30-COMMAND-MAP-r3.md` (same frozen content,
  row-addressable). This contract ≠ the ticket owner's final immutable gate.

## 0. r3 corrections (G1–G5 — one principle, five instances)

- **G1 (live drift or labeled limit):** r2's inventory-vs-inventory
  comparison stayed green when source moved without inventory updates, and
  per-ctor ≥1-arm checks miss arm removals. FIXED: §7-DRIFT now binds the
  LIVE source extent (exact discovery commands over the real trees) + the
  accepted Lean pin (`3590c001`, verified in-leg) to the compared sets, with
  directional controls per direction (source-added, source-omitted,
  mapping-omitted, file-added) demonstrated on real discovery output.
  Source COMPLETENESS (live file-set == frozen file-set + per-file
  non-empty emission) distinguished from mapping COVERAGE (every emitted
  item maps; every mapping row resolves live). file:line is PROVENANCE
  (review aid), never the mechanism — the mechanism is the
  live-discovery ⨝ frozen-mapping join. No enforced:NONE needed: the live
  mechanism is designed below; its temporal hole (post-freeze landings) is
  closed by the explicit-rebind process rule, owned and stated.
- **G2 (independent establishment):** r2 rested most kills on A3-shared +
  REVIEW and blurred GREEN/RED invocations. FIXED: §7-MAP names a DISTINCT
  accounted auditor invocation per obligation — A-RED1/A-RED2 rerun the
  absent-API RED against frozen BASE bytes (separate from A-COLD/A-TEST on
  the candidate — dual-use resolved), all 14 kills get named A-K reruns,
  drift/omission/boundary/conditional/reserve each named. PROPOSED auditor
  ceiling rises to **22/24**, justified bottom-up (§9); coverage was never
  trimmed to fit 12. A-REBIND keeps its rule: author integrates, fresh
  final-SHA audit follows, auditor never repairs.
- **G3 (no templates):** r2's `S30-<Group>/<REQ-ID>` left one variable free
  and counted probes by cap arithmetic. FIXED: describe slugs frozen
  in-contract (§7-REQ Group column), so every `--match` string is fully
  determined; REQUIRED probes enumerated by name (5 drift) with the
  discretionary remainder partitioned into named bounded categories
  (kill-confirm ≤14, dispute ≤3, transient ≤2; total ≤24, REQUIRED-first).
  No fit claim rests on an unresolved count; no ungranted build settles
  anything (this packet costs 0).
- **G4 (honest M10a):** r2's M10a (admission-ctor → incomplete match)
  proved match totality, not impossibility-through-the-public-surface.
  FIXED: M10a's proposition restated as CLOSED-vocabulary totality
  (COMPILER predicate); the actual public-surface property stated as three
  enforceable parts — (i) typed vocabulary (frozen review fact), (ii)
  translation totality via B3 compile + D2 coverage (control: D2 omission),
  (iii) non-vacuity via M10b GREEN-ENUM — with the universal-impossibility
  residual explicitly UNENFORCEABLE by test and enforced via
  change-detection (D2 + tripwire → mandate review). M10b stays distinct;
  neither is a behavioural refusal.
- **G5 (no double counting, no invented work):** POSTSWITCH is REQUIRED,
  and M4a's breakage of it is an accounted co-effect, not hidden: M4a/M4b
  carry predicted FULL signatures (M4a: SWITCH RED + POSTSWITCH RED +
  RECAST GREEN; M4b: RECAST RED + SWITCH/POSTSWITCH GREEN) — deviation means
  INCONCLUSIVE, investigate. M7/M14 were one collapse at one site
  (closureCause): MERGED into a single honestly counted row M7; no mutant
  invented. Runs drop 15→14.
- **Recount + above-20 justification (ordered by "Also noted"):** owner
  builds RED 2 + GREEN 18 + SLIM 3 = **23**. The three builds above the
  counted 20, each traced to its ordering defect: M4b run ← D4 (own mutant
  for recast-idempotence); M13 run ← D5 (explicit franchise-snapshot
  accounting); B19 omission-challenge ← D3 (only truncation-guard
  falsification). M10b rides inside B4 as a named sub-claim; drift rides in
  the probe cap. PROPOSED owner ceiling **23/24** (supersedes 24/24).

## 1. Objective (one observable — unchanged)

A nondegenerate application opens an app-scoped assent question, casts
ballots as the franchise, and observes the verdict (positive AND negative,
each with explicit cause) plus its closure record through the **integrated**
boundary — refusals before durable effects, replay equality. Test-only
proving instance (as S28); the user demo is `paolino/kelgroups#33`, not this
ticket. Threshold is a parameter at every evaluation; exhibits are never
defaults.

## 2. Scope — rows (FULL #30 scope; D1 scoping + G4/G5 honesty applied)

Accepted behaviours INTACT: converge to the LANDED S28 interface
(`Integration` surface, direct-only admission, sealed `commitBaseChange`,
`foldIntegrated`/`foldIntegratedFrom` shared step, validate-then-append,
`GroupView` sole projection), never redesign. `Trivial` degenerate
presence-only. No unilateral Lean edits. No threshold default. No expiry.
No votable admission. No second store/fold.

| id | requirement | accepted evidence (@3590c001) | deliverable | depends on | Terms statement |
|---|---|---|---|---|---|
| R30-1 | openQuestion (collective + permission-with-designee), responsabile-only admission | `Vote/Event.lean` openQuestion; `Validate.lean:57` arm | Haskell #30 | none | intact; no base-rule change |
| R30-2 | cast: one-position placement, idempotent re-cast, switch moves | `placeBallot` (`Fold.lean:53-56`); `effectedState` cast (`:95-100`); `Validate.lean:59-64`; guarded `setInsert` (`Types.lean:46-47`) | Haskell #30 | none | transcribed; M4a/M4b sensitivities distinct with full signatures (G5) |
| R30-3 | sweepClosures same-step close + ClosureRecord (tally + franchiseChange); retention, never silent drop; NO expiry | `sweepStep`/`sweepClosures` (`Fold.lean:65-76`); append-only `closed` + R-51/R-61; no clock (R-54) | Haskell #30 | none | lifecycle content neither ahead of Lean nor out |
| R30-4 | verdictOf: collective threshold @ current franchise (legacy order) + permission designee arm (never tally) | `verdictOf` (`State.lean:82-96`); R-46/R-49/R-50/R-64 | Haskell #30 | none | parameter everywhere; exhibits never defaults |
| R30-5 | refusals `notResponsabile` + `questionNotFound` PRODUCED now; `notDesignee`/`notProposer` DECLARED (`Validate.lean:41-42`), zero Slice-A sites — unruled INTENTION | 4-ctor `VoteError` + 3-arm validation (`:56-70`); `paolino/reactivegas#81` out-of-scope § | 4-ctor vocabulary; NO producing semantics anywhere | NONE (preserved boundary) | UNSCHEDULED: no ticket/promise/edge; tripwire only |
| R30-6 | franchise from canonical GroupView every evaluation | `franchise`/`franchiseSize`/`isResponsabile`; R62-11; S28 `groupView` | Haskell #30 | none | never a payload-local copy (M13) |
| R30-7 | negative-verdict delivery observable at boundary | `ClosureRecord`; `sweepStep` negative arm; S28 hook surface | interface #30; consumption → `paolino/reactivegas#76` | #76 effect only | evidence-exposure only |
| R30-8 | vote routing separated from base admission | T6222 removal; non-admitting `pendingBase`; S28 direct-only | boundary definition #30 | none | intact (G4 three-part property: typed vocabulary + translation totality + non-vacuity) |
| R30-9 | approve path matches LANDED base rule; V-2 rebind after landing | V-2 settled; `paolino/reactivegas#68` OPEN | REBIND after #68; freeze on current base meanwhile | #68 ONLY at: `tryEnactBase` majority + proposer rules (`Fold.hs:347-377`, `majority`/`adminCount`) | separate dependency, concrete boundary, no blanket block |
| R30-10 | RULED lifecycle — MECHANISM SURFACE ONLY: close/record/cause/retention/atomicity hooks SHAPED for extension (4-cause record shape as DATA; post-base hook with exact pre/post views; atomic discard — all S28-existing); validated AS EXTENSION POINTS, never exercised for absent renounce/departure closes | V-5; carried causes (`Types.lean:76-77`); `effectedState` renounce→gs (verified Slice-A no-op); `paolino/reactivegas#81` §1–§3 | mechanism surface + extension validation; CONTENT → #81 | #81 content (L-1–L-6); #76 refund (L-7) | D1: no executable #81 rows (would anticipate unlanded work); carried causes as DATA + downstream obligations |
| R30-10U | UNRULED refusal policy, NOT scheduled; don't-produce boundary PRESERVED | #81 out-of-scope § | NONE | — | UNSCHEDULED preserved boundary |
| R30-11 | verdict → economic effect | NOTE-016/A-Q001 (ruled, wire missing) | `paolino/reactivegas#76` (evidence only) | #76 | neither ahead nor out |
| R30-12 | PureScript client propose/vote (adapt-only) | `kelgroups-client` transport (Fold.purs tension noted, adapt-only) | #30 client additions | none in team | adapt-only INCLUDED; UI/wasm (`#84`/`#82`) and wholesale redesign NOT. LIMIT stated: TEST-boundary roundtrip (S28 precedent); production-server roundtrip out of scope |
| R30-13 | Lean proofs | existing Vote proofs = evidence; Slice-B producers only where RULED | LEAN-OWNED; zero kelgroups Lean edits | Lean lanes where ruled; none where unruled | no invented obligations |
| R30-14 | denial/dissent observable (L2 consumers) | negative closure delivery | interface #30; consumption #76 | #76 | as R30-7 |
| R30-X | NON-GOALS: expiry; theta default; votable admission; second store/fold; UI+wasm; in-kelgroups economics | R-54; R-46/47; INV-62; R9c/R11; ASSENSO gap | none | — | fence (§8) + M8/M9/M10a/M10b + leg 7 |

Downstream obligations (recorded with owners, never executed here; S30
acceptance verifies them ONLY as "recorded with owner"): L-1 renounce-close
(owner #81); L-2 departure-close atomically via post-base hook (owner #81);
L-3 `.negative` (owner #81); L-4 exact causes (owner #81); L-5 retention
(owner #81); L-6 scoping + L-6a coexistence + L-6b non-interference (owner
#81); L-7 refund — GATED on `paolino/reactivegas#76` (owner #76).

#29 remainder (unchanged): demo #33 (blocked #30; publication separately
gated); release/notes #34 (blocked #33+#30; authority with desk); downstream
notes (with `#73` lane on S30 landing). `#29` + `paolino/reactivegas#73`
OPEN.

## 3. Compact spec

Stories S1–S6 (r1/r2 unchanged): open; cast/switch/recast exactly-once;
positive AND negative verdicts with causes + retained records (never silent,
never expiry); non-responsabile refused pre-effect (state AND log
unchanged); accepted-KEL replay identical; client propose/vote.

Requirements: R30-1–R30-8 + R30-12 + R30-14 now; R30-9/R30-10-content/
R30-11/R30-13 as dependencies; R30-5-producing + R30-10U unscheduled;
R30-X fences. Parameterized threshold; canonical-view franchise; refusals
first; replay equal; retained closures.

Rejection behaviour: vote-path `notResponsabile`/`questionNotFound`;
substrate-path S28 identities; refusal advances nothing.
`notDesignee`/`notProposer` vocabulary only. No expiry refusal anywhere.

Production-path examples (executed in-suite through the real step/fold):
REQ-ADMIT-PATH (admitted open: question in state, row in log);
REQ-OPEN-REFUSE + REQ-CAST-NONRESP (exact error, aggregate AND log
unchanged); REQ-NONDECIDE-PERM (non-designee ballot recorded-but-
not-deciding: present in tallies, verdict open — accepted behaviour
exercised, never widened into refusal nor narrowed into ignore).

Success: S1–S6 + all §7-REQ executable through the integrated boundary on
the candidate SHA (§10); `Trivial` intact; full `just ci` green; fresh
audit PASS; no shipped defaults; no silent drops.

## 4. Plan

Single slice S30-1 carrying the FULL boundary (mirrors + `Integration`
wiring + persistence + client additions + proving instance). No S30-2.
RED-equivalence absence proof first (C1/C2 — absence ONLY); GREEN in the
frozen envelope (14 mutant runs + M10b enumeration + live drift discovery +
required-example set); D3 omission challenge B19 (sole truncation-guard
falsification); SLIM; fresh FULL audit (G2 independent establishment);
draft PR + remote CI; acceptance. Ticket owner freezes the final gate.

Constraints: this packet cost 0; §8 fence; whole-project invocation =
BUILD; S28 concurrency discipline (serialized append, refusal-before-codec;
no second writer); no parallel heavy builds.

Live boundaries (all with can-fail controls): step/validate agreement;
Store/KEL append + replay (`foldIntegratedFrom` == live; founding guard);
client Api TEST-boundary roundtrip (limit stated); live drift discovery +
Lean pin (G1). No source-token behavioural proofs, no shrinkable
inventories, no absent-API-import as behavioural evidence.

Order: S30-1 → audit → PR → #33 → #34. #68 touches only the R30-9 boundary
on landing, with revalidation.

## 5. Models (compact; no implementation content)

Modules: new `KelGroups.Vote.*` mirrors (depend on `GroupView` + S28
`State`, never duplicate); `KelGroups.Fold` owns `Integration` composition
(proving-integration `AppState` or dedicated vote-aware fold — ticket owner
decides; `BaseProposal` reading, `proposalMutation → BaseMutation`,
`digest`, post-base hook composition nowhere else); `KelGroups.Store` owns
the vote persistence path (existing integrated path; no second
writer/tables); `kelgroups-client` owns propose/vote additions (existing
transport, adapt-only); proving instance owns the test-only app (never the
#33 demo). Drift mapping + discovery commands are frozen gate artifacts,
not production code.

Data: `Question{kind,proposer,assents,dissents}` (no time-like field);
`ClosureRecord{questionId,question,verdict≠open,cause}` (4 causes as DATA);
`VoteState{openQuestions,closed-append-only}`; `Verdict`; `Threshold`
(parameter); `QuestionKind{collective,permission(designee)}`;
`ClosureCause{tally,franchiseChange,proposerDeparted,renounced}`;
`VoteEvent{openQuestion,cast,renounce}` (T6222 — no membership event);
`VoteError` (2 produced + 2 vocabulary-only). Franchise never stored;
verdict single site; closure = remove + append as one operation.

Functions (exact Haskell spellings frozen by ticket owner): vocabulary
constructors; `verdictOf`; `franchise`/`franchiseSize`/`isResponsabile`;
`lookupQuestion`; `closureCause`; exhaustive `validateVoteEvent`;
`placeBallot`; `sweepStep`/`sweepClosures`; `effectedState`
(authorization-free by architecture); `applyVoteEventChecked` (integrated
path uses it, never revalidates); `foldVote`/`foldFrom`;
`Integration`-wiring (`digest`, `proposalMutation`, vote-aware
`appFold`/`baseHook`). `GroupView`/`Threshold` explicit everywhere.

## 6. Tasks (slice S30-1)

- T30-1 RED-equivalence absence proof (C1/C2, absence ONLY).
- T30-2 vocabulary + state mirror (R30-1/4/6 + REQ opens/verdicts/reads).
- T30-3 validation mirror (R30-1/2/5; 4-ctor vocabulary, two ctors
  site-less; tripwire noted).
- T30-4 placement + effects (R30-2; M4a/M4b distinct signatures;
  open-never-overwrites; renounce no-op preserved).
- T30-5 sweep + closure + retention (R30-3/4/7/14; PRODUCED causes only).
- T30-6 `Integration` wiring (R30-8/6/9-freeze; current base).
- T30-7 persistence path (append + replay + founding guard; no second
  writer).
- T30-8 client additions (R30-12 adapt-only; TEST-boundary; limit stated).
- T30-9 mechanism-surface shaping (R30-10; L-1–L-7 recorded with owners;
  zero executable #81 rows).
- T30-10 drift mapping + discovery + REQ set (G1 live instrument; G3 frozen
  names/describe slugs).
- T30-11 GREEN envelope + omission challenge + SLIM + CI + hygiene (B1–B20,
  drift probes, 3B SLIM, `Trivial` intact, tracked-clean).
- T30-12 audit handback + PR (G2 independent establishment; draft PR
  post-GREEN; exact-SHA merge at desk).

## 7. Frozen requirement-to-command/control map (binding)

Conventions: whole-project invocation = 1 BUILD (expected-RED, warm reruns,
mutant runs, omission rerun all count). Per-mutant cycle = apply + run +
revert, hash-verified restore (failure aborts exit 3). Hidden invocations
forbidden: every cited result maps to a counted B-row or PROBE-row.
Predicates (D6/G-persistent): COMPILER-kill = exit≠0 + diagnostic quotes
ctor/site + zero parse-error lines; TEST-kill = exit≠0 + `Failures:` names
≥1 registered REQ-ID of the row (empty/crash/timeout/infra/parse NEVER
count); GREEN-ENUM = exit 0 + enumeration lists every allowed ctor.
Setup/infra failure = INCONCLUSIVE abort, never kill. Charge-0 recon
(reads, greps incl. tripwire, `git status/diff/rev-parse/log`,
`gh issue view`, `--version`): free, never evidence, never a kill.

§7-CMDS (exact; toolchain re-pinned exactly at freeze):
- `nix develop .#ci --quiet -c just build`
- `nix develop .#ci --quiet -c cabal test all -O0 --test-show-details=direct`
- `nix develop .#ci --quiet -c just ci`
- probes: `nix develop .#ci --quiet -c cabal test invariants
  --test-option=--match --test-option=/S30-<Group>/<REQ-ID>/` — Group slugs
  frozen below, so every string is fully determined (G3 — no free
  variables).
- pins: `nix --version` + one batch `ghc/cabal/lake/node/spago/just
  --version` (values re-pinned at freeze).

§7-REQ (REQUIRED examples + frozen describe Groups — G3 exact):

| Group slug | REQ-IDs |
|---|---|
| `S30-Open` | REQ-OPEN-COLL, REQ-OPEN-PERM, REQ-OPEN-REFUSE, REQ-OPEN-DUP |
| `S30-Cast` | REQ-CAST-ASSENT, REQ-CAST-SWITCH, REQ-CAST-POSTSWITCH, REQ-CAST-RECAST, REQ-CAST-UNKNOWN, REQ-CAST-NONRESP |
| `S30-Sweep` | REQ-SWEEP-TALLY, REQ-SWEEP-DISSENT, REQ-SWEEP-FRANCHISE, REQ-RETAIN, REQ-NOEXPIRY |
| `S30-Verdict` | REQ-VERDICT-COLL, REQ-VERDICT-PERM |
| `S30-Franchise` | REQ-FRANCHISE-CURRENT |
| `S30-Negative` | REQ-NEG-DELIVER |
| `S30-Route` | REQ-ROUTE-ENUM |
| `S30-Lifecycle` | REQ-HOOK-EXT, REQ-RECORD-SHAPE |
| `S30-Client` | REQ-CLIENT-ROUNDTRIP |
| `S30-Admit` | REQ-ADMIT-PATH, REQ-NONDECIDE-PERM |

Gate cross-check: every REQ-ID registered AND executed, else RED. B19
(one REQ-ID removed from spec → RED) is the ONLY truncation-guard
falsification; C1/C2 RED is absence-only.

§7-DRIFT (G1 live mechanism — discovery commands read the REAL trees;
mapping frozen; provenance labeled, never the mechanism):
- LIVE discovery (exact forms; ticket owner pins the two checkout paths at
  freeze — kelgroups worktree + Reactivegas checkout):
  - Lean file-set: `ls lean/KelGroups/Vote/*.lean lean/KelGroups/{Integration,State,Validate}.lean` asserted == frozen FILE LIST (completeness leg; new/renamed file → RED).
  - Lean pin in-leg: `git -C <reactivegas> rev-parse HEAD` == `3590c001…` (else RED — binds the accepted pin to the compared sets).
  - Lean item emission: declaration-site grep over exactly that file set —
    type declarations `^(structure\|inductive)\s+[A-Z][A-Za-z0-9']*` plus
    inductive arms `^\s+[|]\s+[a-z][A-Za-z0-9]*` — with per-file NON-EMPTY
    guard (an empty emission file → RED; the guard against silent
    truncation, itself exercised by the omission controls).
  - Haskell item emission: `^(data\|newtype\|type)\s+[A-Z]` over
    `lib/KelGroups/Vote/*.hs` (+ named wiring symbols in `Fold.hs` listed
    in the mapping, each anchored `^<name>`).
  - RECONCILIATION join: every emitted Lean item has ≥1 mapping row; every
    mapping row resolves live on BOTH sides (Lean item still emitted;
    Haskell symbol still emitted). Mismatch → RED.
- COMPLETENESS (did we capture the whole extent?) = file-set equality +
  per-file non-empty emission. COVERAGE (does everything captured map?) =
  the join. Distinct legs, distinct failures, stated separately.
- Directional controls on REAL discovery output (copies, never fixtures —
  input is the actual emission with one line edited), each a counted PROBE:
  P-DRIFT-GREEN (recon GREEN); P-DRIFT-ADD (output + synthetic ctor →
  RED, unmapped-Lean); P-DRIFT-SRCOMIT (output − one real line → RED,
  mapping-dangles); P-DRIFT-MAPOMIT (mapping − one row → RED, discovered-
  unmapped); P-DRIFT-FILEADD (file list + synthetic file → RED). 5 REQUIRED
  named probes.
- Temporal hole (source lands post-freeze, nobody re-runs): closed by
  PROCESS, owned and stated — any Lean landing triggers the contract's
  explicit-rebind rule ⇒ ticket-owner re-freeze + re-demonstration of all
  five drift probes before further acceptance. The mechanism covers space;
  the process covers time; neither pretends to be the other.
- Haskell exhaustiveness stays IN-LANGUAGE evidence only (labeled; never
  the drift claim). Behavioural rows keep their executed controls; drift
  output is never cited for them.

§7-MAP (obligation → owner command → auditor command; RE-RUN vs REVIEW
labeled — G2/G5):

| obligation | owner (class) | auditor (class) |
|---|---|---|
| absence T30-1 | B1 build-absence + B2 test-absence RED (BUILD ×2, absence ONLY) | A-RED1/A-RED2 vs frozen BASE bytes, detached (RE-RUN ×2 — dual-use resolved: NEVER the candidate GREEN calls) |
| R30-1 | B4 shared + probes | A-TEST (RE-RUN); kill evidence REVIEW vs D6 predicates |
| R30-2 | B4 + B6 M2 + B7 M3 + B8 M4a + B9 M4b | A-TEST; A-K reruns incl. M4b (RE-RUN); rest REVIEW |
| R30-3 | B4 (Store append + replay) + B10 M5 + B11 M6 + B12 M7merged + B14 M9 | A-TEST + A-Ks (RE-RUN); boundary via M6 rerun (named sharing, not hidden) |
| R30-4 | B4 + B13 M8 | A-TEST; REVIEW |
| R30-5 | B3 cold (vocabulary + 3-arm exhaustive compile) | A-COLD (RE-RUN); tripwire REVIEW (never a kill) |
| R30-6 | B4 + B18 M13 snapshot-in-payload → REQ-FRANCHISE-CURRENT RED | A-TEST; A-K13 (RE-RUN) |
| R30-7/14 | B4 at integrated boundary + persistence roundtrip | A-TEST; A-K6 (RE-RUN, named boundary sharing) |
| R30-8 | B3 (2-arm enactment); B15 M10a (COMPILER, CLOSED-vocabulary totality — G4 honest scope); M10b GREEN-ENUM named sub-claim in B4 | A-COLD; A-K10a when disputed (RE-RUN); M10b re-checked in A-TEST (RE-RUN) |
| public-surface (G4) | (i) typed vocabulary — frozen review fact (labeled, not executed); (ii) translation totality — B3 compile + D2 coverage mapping (control: P-DRIFT-SRCOMIT/MAPOMIT); (iii) non-vacuity — M10b | A-COLD + drift rerun (RE-RUN); residual universal-impossibility labeled UNENFORCEABLE-by-test → change-detection (D2 + tripwire → mandate review) |
| R30-9 | freeze on current base (B3/B4) | A-REBIND conditional (RE-RUN iff #68 landed; author integrates + fresh final-SHA audit follows; auditor never repairs — rule kept) |
| R30-10 surface | B4 (hook runs exact pre/post; 4-cause DATA shape; append-only; atomic discard); B16 M11 hook-ignored → REQ-HOOK-EXT RED | A-TEST; A-K11 (RE-RUN) |
| produced-cause distinction | B12 M7merged: closureCause forced-.tally → REQ-SWEEP-FRANCHISE RED (carried causes excluded, D1) | A-K7 (RE-RUN) |
| L-1–L-7 | recorded with owners (#81; L-7 gated #76). NO command — explicitly not established here | verified as recorded-with-owner (REVIEW of record only) |
| R30-10U/PROD | no command (preserved boundary; tripwire recon) | — |
| R30-11 | no wire, no mock | — |
| R30-12 | B20 leg-6 (`spago build` + `spago test`, TEST-boundary, limit stated); B17 M12 dropped-propose-path → REQ-CLIENT-ROUNDTRIP RED | A-CI (RE-RUN); A-K12 (RE-RUN) |
| R30-13 | B20 `lake build` green only | A-CI (RE-RUN) |
| drift G1 | 5 REQUIRED drift probes (above) | drift rerun + omission spot-check (auditor PROBE ×2, named) |
| guard D3 | B19 omission-challenge (BUILD; ONLY guard falsification) | A-OMIT rerun (RE-RUN ×1) |
| cold/final | B3 cold 1B (COLD/WARM logged) + B20 final `just ci` 1B + tracked-clean + `Trivial` presence-only + founding guard | A-COLD/A-CI (RE-RUN) |
| SLIM | 3B identical-envelope (legs 1,2,2b,3,4,6,7 analog + drift GREEN recon probe) | — |

Mutant ledger (14 runs B5–B18): M1 openQuestion-nonresp bypass
(TEST/REQ-OPEN-REFUSE); M2 cast-nonresp bypass (REQ-CAST-NONRESP); M3
unknown-accept (REQ-CAST-UNKNOWN); M4a erase-drop — PREDICTED SIGNATURE
SWITCH RED + POSTSWITCH RED (accounted co-effect, same cause: voter in two
lists) + RECAST GREEN; kill criterion SWITCH REDs, deviation =
INCONCLUSIVE (proves the mutant hit its site); M4b unguarded-insert —
SIGNATURE RECAST RED + SWITCH/POSTSWITCH GREEN (discriminating); M5
tally-suppress (REQ-SWEEP-TALLY); M6 dissent-suppress (REQ-NEG-DELIVER);
M7merged cause-forced-tally (REQ-SWEEP-FRANCHISE + produced-cause
distinction; site closureCause — the single collapse, counted once, G5);
M8 permission-tally-consult (REQ-VERDICT-PERM); M9 close-and-discard
(REQ-RETAIN); M10a admission-ctor-added (COMPILER/CLOSED-totality, G4
scope); M11 hook-ignored (REQ-HOOK-EXT restoration); M12 propose-path
dropped (REQ-CLIENT-ROUNDTRIP, client boundary); M13
membership-snapshot-in-payload (REQ-FRANCHISE-CURRENT). M10b enumeration
(GREEN-ENUM in B4): every ALLOWED public ctor encodable + enacted;
interface proposition, never behavioural refusal.

Discovery bounds: extent quantified over observed Lean equation sites
(`placeBallot` 2 `Fold.lean:53-56`; `sweepStep` 2 `:65-66`; `effectedState`
3+2 `:89-101`; `validateVoteEvent` 3+2 `Validate.lean:57-70`; `verdictOf` 2
`State.lean:87-96` + `closureCause` 3 `:111-113`; `sweepClosures` shared
step `:72-76`). Discovery bounds the SET (new ctor ⇒ new arm ⇒ new mutant,
never silent pass); never row coverage (every REQ-ID keeps its command).

Charge-0 recon (free, never evidence): reads; D7 tripwire
(`grep -rn "notDesignee\|notProposer" lib/ --include=*.hs` minus vocabulary
declaration — review-time; sole job accidental-introduction detection →
mandate review, never kill); `git status/diff/rev-parse/log`; `gh issue
view`; `--version` pin reads.

## 8. Candidate-independent initial gate design

Fence (ticket owner versions at freeze): WRITABLE — new `KelGroups.Vote.*`
mirrors; `Integration`-wiring points (`Fold.hs` composition; `State/Event`
only where wiring requires); `Store.hs` vote path (existing integrated
path; no second writer/tables); proving instance (`S30*Spec` + app);
client Api additions (adapt-only); `kelgroups.cabal` + `test/Main.hs`;
drift mapping + discovery + REQ list (frozen gate artifacts, not
production). FENCED — `lean/**`; historical bodies; `Trivial.hs`
(presence-only, uncounted); S28 production outside named points; `client/`
beyond adapt-only; UI/wasm/economics; release metadata.

Legs (K/M frozen from the actual spec): 1 hygiene before/after; 2 identity
+ self-hash (blank-normalized) + ancestry (HEAD descends from frozen slice
base); 2b pins exact, fail-closed (re-pinned at freeze); 3 build cold/warm;
4 inventory (≥K vote groups) + registered (§7-REQ cross-check: every REQ-ID
registered, total == file examples, no orphans — POSTSWITCH REQUIRED here,
G5) + execution (all registered executed, none pending; historical green) +
exit 0; 5 mutants B5–B18 (entry: tracked-clean committed candidate at
recorded HEAD; apply+run+revert; hash-verified restore every exit;
restoration failure aborts exit 3) with per-mutant predicted signatures
(M4a/M4b full signatures; others primary-witness + GREEN-remainder);
DRIFT leg (live discovery + pin + join + 5 controls); M10b GREEN-ENUM in
leg-4 scope; 6 full `just ci`; 7 `Trivial` presence + client CI presence.
`set +e`; full log + per-leg sha256 evidence; meta file
(version/HEAD/evidence). B19 = separate counted leg-4 rerun minus one
REQ-ID (RED required); C1/C2 absence-only, never cited for the guard.

Kill-attribution: COMPILER (M10a) / TEST (behavioural, REQ-ID-naming —
M4a criterion SWITCH REDs with POSTSWITCH-RED co-effect predicted; M4b
RECAST REDs with SWITCH/POSTSWITCH GREEN predicted) / GREEN-ENUM (M10b).
Deviation from a predicted signature = INCONCLUSIVE abort, never a kill.

Evidence: `run-receipt`-style capture (command hash, exit, duration,
evidence hash, bytes, lines, path) per cited run; self-hash leg 2;
ancestry leg 2; mutant diff hashes pre-run; restore hashes post-run.

Spend classes: BUILD (whole-project = 1); PROBE (narrow: exact-REQ-ID
`--match`, single-component build, drift runs = 1 vs 24-probe cap);
CHARGE-0 (enumerated recon — free, never evidence); AUDIT-BUILD/AUDIT-PROBE
(same, auditor cap). No parallel heavy builds; every failed setup/attempt
journaled; no automatic raises (exact gap first).

## 9. Operational classifications + proposed ceilings (FROZEN with §7)

Owner fit: B1–B2 RED (2) + GREEN 18 (B3 cold + B4 test + B5–B18 fourteen
runs + M10b named in B4 + B19 omission + B20 CI) + SLIM 3 = **23 builds**.
Above-20 justification (each ordered, none invented): M4b run ← D4 (own
recast-idempotence mutant); M13 run ← D5 (explicit snapshot accounting);
B19 omission ← D3 (sole guard falsification). M10b inside B4; drift inside
probe cap; M14 merged away (G5). Probes ≤24: 5 REQUIRED named
(P-DRIFT-GREEN/ADD/SRCOMIT/MAPOMIT/FILEADD) + discretionary bounded
categories kill-confirm ≤14 (ambiguous logs only — B-run logs already quote
kills), dispute ≤3, transient ≤2; REQUIRED-first. **PROPOSED owner 23/24**
(supersedes 24/24). PROPOSALS pending fit-proof at freeze + authorization;
gap returns exact cost, never trimmed scope.

Auditor table (pre-dispatch, exact; G2 full establishment): A-RED1/A-RED2
frozen-BASE RED reruns 2B; A-COLD 1B; A-TEST (incl. REQ cross-check + M10b
re-check) 1B; A-CI 1B; A-K1–K14 fourteen kill reruns 14B; A-OMIT 1B;
A-REBIND conditional 1B (iff #68 landed: author integrates + fresh
final-SHA audit follows; auditor never repairs — rule kept); A-RESERVE
repair-verification 1B; drift rerun + spot-check from probe cap (PROBE ×2).
Undisputed-kill handling: still RE-RUN (all 14 named above) — REVIEW applies
only to records (L-1–L-7 as recorded-with-owner; tripwire output; B19
log read alongside A-OMIT rerun). Sharing stated per row (M6/A-K6 and
M12/A-K12 are the named boundary reruns — no separate boundary builds).
**PROPOSED auditor 22/24** (2+3+14+1+1+1 builds; probes 2 required +
finding-narrowing ≤14 + reconfirm ≤8 = 24). Bottom-up justified; coverage
never trimmed to fit a smaller number. Auditor probes: A-P-DRIFT rerun +
spot-check REQUIRED (2); rest bounded discretionary.

Team (standing fence, commissioned at authorization — NOT by this packet):
Muse ticket owner → Muse commit owner (`draft=NONE`) → fresh Codex-or-Grok
auditor per submission (two audited submissions max, one findings bounce,
repair re-audited); signed commits; draft PR post-GREEN; exact-SHA merge at
desk.

## 10. Acceptance (binding when commissioned)

Threshold-parameterized verdicts (permission never tallies); retained
records with PRODUCED causes (never silent; no expiry); refusal pre-effect
(aggregate AND log unchanged; accepted-KEL replay identical); validate/fold
agreement (same triple; never historical validator, never double-wrap);
negative delivery at the integrated boundary (both verdicts with causes via
step + append + replay); zero producing sites for dormant ctors (tripwire
clean at review); §7-REQ complete (registered + executed; B19 RED
observed); drift GREEN + all four directional REDs observed with the Lean
pin bound; M10a COMPILER-kill + M10b GREEN-ENUM observed; G4 three-part
surface property closed with residual labeled; M4a/M4b predicted signatures
observed exactly; client additions under client CI (TEST-boundary limit
stated); `Trivial` intact; full `just ci` green; tracked-clean both ends;
founding guard held; L-1–L-7 recorded with owners (reviewed as record);
fresh audit PASS complete; bounded claims only.

## 11. Open questions / dependencies (enumerated, not invented)

- `paolino/reactivegas#68` → R30-9 rebind + revalidation only.
- `paolino/reactivegas#81` (§1–§3; L-7 gated #76) → R30-10 content;
  unruled exclusions depend on nothing.
- `paolino/reactivegas#76` → Reactivegas side; kelgroups exposes interface
  + closure evidence only.
- `paolino/reactivegas#75` (R3.1) → test input for persistence (threshold =
  test input), not a shipped default.
- Upstream Lean gaps → enumerated here, never invented; landings rebind.

## 12. Provenance + freeze record

Sources (read-only; newest governs; no blocking conflicts): v3 mandate+map
(v1/v2 history); bodies #30 (2026-09-06 correction), #29 (2026-09-06 Lean
correction), #33, #34; Lean Vote + Integration/State/Validate @ `3590c001`
(zero-diff EMPTY vs `4a6cd87`); V-2 + #68 OPEN; #81 body; R3.1; S28 @
`933e385d`; S28 gate v10.2 shape; commissioning note (honest-count +
preparation boundary); NOTE-001/002 (clock + helper rules — helper-stamped
STATUS since 04:11:04Z); NOTE-003 + parent `T30-R1-ASSESSMENT.md` (r2 —
retained); NOTE-004 (this r3 — one coherent packet, G1–G5). Inbox checked
before r3 filing (NOTE-004 read + acked; no other unread). Spend: 0/0/0/0.
Skills: orchestrator-contract, ticket-orchestrator, resolve-ticket
(planning only), context-compiler, worker-protocol, tmux-orchestrator,
verification, invariants, gate-script, haskell, nix, lean4 (read-only).

Hashes (sha256): base `933e385df2f2a251bb54a08bb7663f0d41fafb64`; Lean
`3590c0015b84fd58004bf6fb44dd18b107304c48`; brief
`f6d857639a4f3d9aa466c081305f43ae9e59bad4fd9dcb790a08bd85fc856416`.
