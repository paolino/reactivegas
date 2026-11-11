# T30-CONTRACT-r2 — kelgroups #30 substrate vote interface + closure evidence (FOR IMPLEMENTATION AUTHORIZATION)

Ticket preparation owner `t30-contract` (Muse), 2026-09-06. Preparation-only:
read-only inspection + own planning artifacts. No compilation, tests,
mutations, gate runs, dispatch, product edits, commits, push/PR/merge/release,
issue comments, or spend (spend 0 throughout).

- Brief: `/tmp/reactivegas/ms2/e-kelgroups-substrate/t30-contract/brief.md`
  sha256 `f6d857639a4f3d9aa466c081305f43ae9e59bad4fd9dcb790a08bd85fc856416`.
- SUPERSEDES `T30-CONTRACT-r1.md` + `T30-COMMAND-MAP-r1.md` (both retained
  unmodified as history; r1 COMPLETE 04:11:17 stands as history). This r2 is
  the bounded correction run ordered by NOTE-003 per the parent assessment
  `handoffs/T30-R1-ASSESSMENT.md` (epic owner, read in full — it governs
  where summaries differ). Scope work from r1 stands; the seven instrument
  defects D1–D7 below are fixed, nothing else changed.
- Operative base: kelgroups `main` @
  `933e385df2f2a251bb54a08bb7663f0d41fafb64` (S28 LANDED via PR#32 guarded
  squash; post-merge CI+Release SUCCESS; #28 CLOSED post-scope-verdict).
- Accepted Lean: Reactivegas @ `3590c0015b84fd58004bf6fb44dd18b107304c48`;
  zero-diff re-verified EMPTY vs `4a6cd87` on `lean/KelGroups/Vote/` +
  `Integration.lean` + `State.lean` + `Validate.lean`. Later landings rebind
  explicitly; nothing here anticipates them.
- Companion frozen map: `handoffs/T30-COMMAND-MAP-r2.md` (same frozen
  content, row-addressable). This contract ≠ the ticket owner's final
  immutable gate: the ticket owner authors that instrument at commissioning
  from this packet.

## 0. Corrections to r1 (binding — D1–D7, assessment-governed)

- **D1 (blocking):** r1 §2-R30-10 + §7-M11/cause-collapse demanded
  renounce-close/departure-close + carried-cause distinction as EXECUTABLE
  rows, but accepted `effectedState` maps `renounce → gs` and carried causes
  produce no closures — a renamed-cause mutant cannot reach an absent
  producer, so passing would anticipate unlanded `paolino/reactivegas#81`
  implementation. FIXED: executable rows test ONLY the Slice-A mechanism
  surface that exists, AS extension points (§2-R30-10, §7-M11/M14); #81
  content moves to explicit downstream integration obligations L-1–L-7 (L-7
  gated on `paolino/reactivegas#76`), recorded with owners, never executed
  here.
- **D2:** r1's "later Lean ctor breaks Haskell matches" is false across
  separately maintained trees — no such mechanism exists. FIXED: Haskell
  exhaustiveness stays as in-language evidence ONLY (labeled, never the
  drift claim); the drift boundary is now a REAL frozen-inventory
  reconciliation instrument with a can-fail omission control (§7-DRIFT).
- **D3:** r1 legs were count-based with zero in-contract required-example
  namings. FIXED: REQUIRED examples named per row in-contract (§7-REQ),
  independent of emitted/registered sets; gate omission challenge added
  (remove one named example from all sets → gate REDs, explicitly counted
  run B20); RED-equivalence stays absence-only, never cited as
  truncation-guard falsification. M10 split into M10a narrow-absence
  (compile-RED) + M10b positive-enumeration (compile-GREEN), own
  instruments, both labeled interface propositions (never behavioural
  refusals).
- **D4:** r1's combined placement entry hid that `setInsert` is guarded
  (`Types.lean:46-47` `contains` check) — dropping erase-other-list keeps
  idempotent recast. FIXED: TWO separately justified sensitivities, M4a
  switch-moves (erase-drop) + M4b recast-idempotence OWN mutant
  (unguarded-insert variant); post-switch validity (REQ-CAST-POSTSWITCH)
  distinguished from ordinary recast idempotence (REQ-CAST-RECAST) in
  witness naming. The non-entailment observation is challenge history
  informing the design, never a kill.
- **D5 (counting):** r1 co-covered franchise-snapshot by prose and left the
  audit table inheritance vague. FIXED: franchise-snapshot is an explicit
  counted run (M13); cause-collapse re-scoped post-D1 (M14, produced causes
  only); §7-MAP enumerates EVERY obligation→establishing-command with
  RE-RUN vs EVIDENCE-REVIEW labeled and hidden invocations forbidden; the
  auditor table maps every obligation the same way. Discovery bounds the
  mutant SET, never row coverage.
- **D6:** r1's kill rule universally demanded a `Failures:` section a
  compiler rejection never produces. FIXED: TWO predicates — COMPILER
  (M10a-bound) vs TEST (all behavioural); exact command arguments
  everywhere (§7-CMDS, no `… --match` vagueness — match strings use the
  §7-REQ names). Parse/infra/timeout-as-kill prohibition retained for both.
- **D7:** r1's construction-site grep operated as a gate while asserting
  source queries never establish rows. FIXED: current accepted behaviour
  (incl. non-deciding cases) preserved with production-path EXAMPLES
  executed through the real step/fold in-suite (REQ-ADMIT-PATH,
  REQ-NONDECIDE-PERM + refusal examples); the grep is demoted to a
  recon-only review tripwire whose sole job is accidental-introduction
  detection (triggers mandate review, never a kill). Producing the refusals
  stays unscheduled.
- **Recount (honest):** r1's 12-entry/20-build fit becomes r2's 15 mutant
  runs + M10b enumeration + D3 omission challenge: RED 2 + GREEN 19 + SLIM 3
  = **24 builds** (§9). The 20/24 figure is superseded the same way 18/24
  was. Both PROPOSED, pending fit-proof at freeze + authorization.

## 1. Objective (one observable — unchanged from r1)

A nondegenerate application opens an app-scoped assent question, casts
ballots as the franchise, and observes the verdict (positive AND negative,
each with its explicit cause) plus its closure record through the
**integrated** boundary — with refusals before durable effects and replay
equality. Test-only proving instance (as S28); the runnable user demo is the
#29 follow-on `paolino/kelgroups#33`, not this ticket. Threshold stays a
parameter in every verdict evaluation; exhibits are never defaults.

## 2. Scope — rows (FULL inherited #30 scope preserved; D1 re-scoping applied)

Accepted behaviours stay INTACT: converge to the LANDED S28 interface
(`Integration` surface, direct-only admission, sealed `commitBaseChange`,
`foldIntegrated`/`foldIntegratedFrom` shared step,
`openIntegratedKEL`/`appendIntegratedEvent` validate-then-append,
`GroupView` sole projection), never redesign it. `Trivial` stays degenerate
presence-only. No unilateral Lean edits. No shipped threshold default. No
expiry. No votable admission. No second store/fold.

| id | requirement (reconciled) | accepted evidence (Lean @3590c001) | deliverable / owner | depends on | Terms statement |
|---|---|---|---|---|---|
| R30-1 | openQuestion (collective + permission-with-designee), responsabile-only admission | `Vote/Event.lean`: `openQuestion(questionId)(kind)`; `Vote/Validate.lean:57` openQuestion arm | kelgroups Haskell #30 | none | intact; no base-rule change |
| R30-2 | cast assent/dissent: one-position placement, idempotent re-cast, switch moves voter | `Vote/Fold.lean:53-56` `placeBallot`; `effectedState` cast arm (`:95-100`); `Validate.lean:59-64` cast arm; guarded `setInsert` (`Types.lean:46-47`) | kelgroups Haskell #30 | none | transcribed, not reinvented; D4 sensitivities kept distinct |
| R30-3 | sweepClosures same-step close + appended ClosureRecord (tally + franchiseChange); retention, never silent drop; NO expiry | `Vote/Fold.lean:65-76` `sweepStep`/`sweepClosures`; `Vote/State.lean` `closed` append-only + R-51/R-61; no clock/no time-like field (R-54) | kelgroups Haskell #30 | none | later lifecycle content neither implemented ahead of Lean nor marked permanently out |
| R30-4 | verdictOf: collective threshold @ current franchise (legacy order) + permission designee arm (never tally) | `Vote/State.lean:82-96` `verdictOf`; R-46/R-49/R-50/R-64 | kelgroups Haskell #30 | none | threshold parameter everywhere; exhibits never defaults |
| R30-5 | refusal identities: `notResponsabile` + `questionNotFound` PRODUCED now. `notDesignee` + `notProposer` DECLARED (`Vote/Validate.lean:41-42`) with zero Slice-A construction sites — unruled INTENTION, not a dependency, not a promise | `VoteError` 4 ctors + 3-arm `validateVoteEvent` (`:56-70`); `paolino/reactivegas#81` out-of-scope § | Haskell carries the 4-ctor vocabulary; NO producing semantics scheduled anywhere | NONE (unruled; don't-produce boundary preserved) | UNSCHEDULED: no ticket, no promise, no edge; D7 tripwire only |
| R30-6 | franchise from canonical GroupView every evaluation (no local copy) | `franchise`/`franchiseSize`/`isResponsabile` over `GroupView.admins`; R62-11; S28 `groupView` | kelgroups Haskell #30 | none | reads S28 `GroupView`, never a payload-local copy (M13 guards) |
| R30-7 | negative-verdict delivery observable at the boundary | `ClosureRecord{verdict,cause}`; `sweepStep` negative arm; S28 hook surface | interface #30; consumption → `paolino/reactivegas#76` | `paolino/reactivegas#76` for effect only | evidence-exposure only here |
| R30-8 | vote routing separated from base admission | T6222 removal; non-admitting `pendingBase` typed by `BaseMutation`; S28 direct-only | boundary definition #30 | none | intact (M10a/M10b guard both directions) |
| R30-9 | approve path matches LANDED base rule; V-2 rebind after landing | V-2 ruling settled; `paolino/reactivegas#68` OPEN | REBIND after `paolino/reactivegas#68`; freeze on current base meanwhile | `paolino/reactivegas#68`, concrete boundary ONLY: `tryEnactBase` majority + proposer rules (`lib/KelGroups/Fold.hs:347-377`, `majority`/`adminCount`) | separate dependency on the actual approval path; no blanket block |
| R30-10 | RULED proposer lifecycle — MECHANISM SURFACE ONLY now: the close/record/cause/retention/atomicity hooks exist SHAPED for extension (closure-record shape carries all 4 causes as DATA; post-base hook composition runs with exact pre/post views; refusal discards atomically — all S28-existing behaviour); validated AS EXTENSION POINTS, never exercised for absent renounce-close/departure-close behaviour | V-5 ruling; `renounce` event + carried `renounced`/`proposerDeparted` causes (`Vote/Types.lean:76-77`); accepted `effectedState` `renounce → gs` (Slice-A no-op, verified); `paolino/reactivegas#81` scope §1–§3 | kelgroups exposes + extension-validates the mechanism surface; closure CONTENT → `paolino/reactivegas#81` | `paolino/reactivegas#81` for content (obligations L-1–L-6 below); `paolino/reactivegas#76` for refund (L-7) | D1: no executable close-on-renounce/departure rows now (would anticipate unlanded implementation); carried causes handled as DATA shape + downstream obligations, neither implemented ahead of Lean nor marked out |
| R30-10U | UNRULED refusal policy, NOT scheduled: non-proposer `renounce` reading; non-designee ballot refusal. Don't-produce behaviour PRESERVED; open operator questions both directions | `paolino/reactivegas#81` out-of-scope § | NONE — no ticket, no promise, no edge | — | UNSCHEDULED preserved boundary |
| R30-11 | verdict → economic effect (grant/deny/backdonate, target/polarity/provenance/one-use) | NOTE-016/A-Q001 (ruled, wire missing) | `paolino/reactivegas#76` (evidence exposed only) | `paolino/reactivegas#76` | neither ahead of Lean nor out |
| R30-12 | PureScript client proposing/voting app questions (API + minimal views, adapt-only) | `client/kelgroups-client` (`Api.purs` transport; `Fold.purs` tension noted, adapt-only) | kelgroups #30 client additions | none in team | adapt-only INCLUDED; Reactivegas UI + wasm (`paolino/reactivegas#84`/`paolino/reactivegas#82`) and wholesale `Fold.purs` redesign NOT. LIMIT (stated, not silent): roundtrip proven at the TEST boundary (S28 substrate precedent); production-server roundtrip out of scope |
| R30-13 | Lean proof obligations | existing Vote proofs = evidence; Slice-B PRODUCERS only where RULED | LEAN-OWNED (Reactivegas lanes); zero unilateral kelgroups Lean edits | Lean lanes where ruled; none where unruled | no invented obligations for unruled identities |
| R30-14 | denial/dissent observable for L2-style consumers | negative closure delivery | interface #30; consumption `paolino/reactivegas#76` | `paolino/reactivegas#76` | same as R30-7 |
| R30-X | NON-GOALS (guarded): expiry; theta default; votable admission; second store/fold; Reactivegas UI + wasm; Reactivegas economics in kelgroups | R-54; R-46/R-47; S28 direct-only/INV-62; R9c/R11; ASSENSO gap | none | — | fence (§8) + M8/M9/M10a/M10b kills + leg 7 |

Downstream integration obligations (D1 — recorded with owners, never
executed here; verified at S30 acceptance ONLY as "recorded with owner",
never as behaviour): L-1 proposer-`renounce` closes (owner:
`paolino/reactivegas#81` ticket); L-2 proposer departure closes atomically
via post-base hook (owner: #81); L-3 both closures `.negative` (owner: #81);
L-4 causes exactly `.renounced`/`.proposerDeparted` distinguished from
`.tally`/`.franchiseChange` (owner: #81); L-5 retention in `closed` (owner:
#81); L-6 scoping (V-5 rule closes only the departing/renouncing proposer's
own — owner: #81) + L-6a coexistence + L-6b renounce non-interference
(owners: #81); L-7 escrow refund on V-5 closure — GATED on
`paolino/reactivegas#76` (owner: #76, blocked until the closure→refund wire
exists). This ticket's mechanism surface (hook composition, 4-cause record
shape, append-only retention, atomic discard) is what those obligations
extend rather than redesign.

#29 owned remainder (unchanged): demo (`paolino/kelgroups#33`, blocked by
#30; publication separately gated); release/notes (`paolino/kelgroups#34`,
blocked by #33 + #30; authority with desk only); downstream notes (with the
`paolino/reactivegas#73` lane on S30 landing). `#29` + `#73` stay OPEN.

## 3. Compact spec (D7 production-path examples included)

Stories S1–S6 (r1, unchanged): open question; cast/switch/recast with
exactly-once ballot accounting; observe positive AND negative verdicts with
explicit causes plus retained records (never silent drop, never expiry);
non-responsabile refused before anything durable (state AND log unchanged);
replay accepted KEL → identical state; client propose/vote through
`kelgroups-client` additions.

Requirements: §2 rows R30-1–R30-8 + R30-12 + R30-14 now; R30-9/R30-10-content/
R30-11/R30-13 as explicit dependencies; R30-5-producing + R30-10U
unscheduled with preserved boundary; all R30-X fences. Threshold
parameterized; franchise from canonical `GroupView`; refusals precede
effects; replay equal; closures retained with causes.

Rejection behaviour (exact identities): vote-path `notResponsabile` /
`questionNotFound`; substrate-path S28 identities; refusal advances nothing.
`notDesignee`/`notProposer` vocabulary only, never produced. No expiry
refusal exists anywhere.

Production-path examples (D7 — executed in-suite through the real
step/fold, §7-REQ): REQ-ADMIT-PATH (responsabile opens through the admitted
path: state shows the open question, log shows the row); REQ-OPEN-REFUSE +
REQ-CAST-NONRESP (refusals through the real validation boundary: exact
error, aggregate AND log unchanged); REQ-NONDECIDE-PERM (non-designee ballot
on a permission question recorded-but-not-deciding: ballot present in the
tallies, verdict stays open — today's accepted behaviour, exercised, never
widened into a refusal nor narrowed into an ignore).

Observable success: S1–S6 + §7-REQ all executable through the integrated
boundary on the candidate SHA (§10); `Trivial` intact degenerate-only; full
`just ci` green; fresh independent audit PASS; no shipped defaults; no
silent drops.

## 4. Plan (unchanged strategy; D1/D2/D3 instruments folded in)

Single coherent slice S30-1 carrying the FULL
vote/integration/replay/closure/client boundary (Haskell mirrors +
`Integration` wiring + persistence path + client Api additions + test-only
proving instance). No S30-2 deferral slice. RED-equivalence first (absence
proof ONLY — §7 C1/C2); then GREEN inside the frozen envelope (15 mutant
runs + M10b enumeration + drift reconciliation + required-example set);
then D3 omission challenge (B20, separate counted run — the ONLY
truncation-guard falsification); then SLIM; then fresh FULL audit; then
draft PR + remote CI; then acceptance. Ticket owner freezes the final
immutable gate from this packet.

Constraints: preparation fence honoured (this packet cost 0); §8 fence
(minimal writable set; S28 production read-only except named integration
points; `lean/**` untouched; historical bodies untouched; `Trivial.hs`
presence-only; no UI/wasm/economics); every whole-project invocation is
BUILD-class; concurrency inherits S28 (serialized append, refusal-before-
codec F3 order; vote appends use the same path, no second writer); no
parallel heavy builds.

Live boundaries exercised (all with can-fail controls): pure step/validate
agreement; integrated Store/KEL append + replay (`foldIntegratedFrom` ==
live; founding guard); client Api roundtrip at the TEST boundary (limit
stated in R30-12); drift reconciliation (D2 instrument). No source-token
search, no shrinkable fixture inventory, no absent-API import failure as
behavioural evidence.

Ordered slices: S30-1 → audit → PR → #33 → #34. `paolino/reactivegas#68`
rebind touches only the R30-9 boundary on landing, with revalidation.

## 5. Models (compact; no implementation content — unchanged from r1)

Modules: new `KelGroups.Vote.*` mirrors (depend on `GroupView` + S28
`State`, never duplicate); `KelGroups.Fold` owns `Integration`-wiring
composition (vote payload as proving-integration `AppState`, or dedicated
vote-aware app fold — ticket owner decides; `BaseProposal` reading,
`proposalMutation → BaseMutation`, `digest`, post-base hook composition
nowhere else); `KelGroups.Store` owns the vote persistence path (existing
integrated validate-then-append + replay; no second writer/tables);
`kelgroups-client` owns propose/vote Api additions (existing transport,
adapt-only); test proving instance owns the nondegenerate proving app
(test-only, never the #33 demo). Drift inventories (§7-DRIFT) live beside
the gate as frozen ticket-owner artifacts, not production code.

Data: `Question{kind,proposer,assents,dissents}` (no time-like field);
`ClosureRecord{questionId,question,verdict≠open,cause}` (all 4 causes as
DATA — D1); `VoteState{openQuestions,closed-append-only}`;
`Verdict{positive,negative,open}`; `Threshold = Nat→Nat` (parameter);
`QuestionKind{collective,permission(designee)}`;
`ClosureCause{tally,franchiseChange,proposerDeparted,renounced}`;
`VoteEvent{openQuestion,cast,renounce}` (no membership event — T6222);
`VoteError{notResponsabile,questionNotFound,notDesignee,notProposer}`
(last two vocabulary-only). Franchise never stored; verdict single site;
closure = remove-from-open + append-record as one operation.

Functions (new/changed signatures only; exact Haskell spellings frozen by
ticket owner): vote vocabulary constructors; `verdictOf : Threshold →
GroupView → Question → Verdict`; `franchise`/`franchiseSize`/
`isResponsabile` over `GroupView`; `lookupQuestion`; `closureCause`;
`validateVoteEvent` (exhaustive, no wildcard); `placeBallot`;
`sweepStep`/`sweepClosures`; `effectedState` (authorization-free by
architecture); `applyVoteEventChecked` (single checked step; integrated
production path uses it, never revalidates); `foldVote`/`foldFrom`;
`Integration`-wiring (`digest`, `proposalMutation → BaseMutation`,
vote-aware `appFold`/`baseHook` composition). `GroupView`/`Threshold`
explicit everywhere.

## 6. Tasks (stable IDs, slice S30-1)

- T30-1 RED-equivalence absence proof (C1/C2 — absence ONLY, never cited
  for truncation; D3).
- T30-2 vocabulary + state mirror (R30-1/4/6 + §7-REQ opens/verdicts/
  franchise reads; no time-like field).
- T30-3 validation mirror (R30-1/2/5: exhaustive 3-arm validation; 4-ctor
  vocabulary, two ctors with zero construction sites; D7 tripwire noted).
- T30-4 placement + effects (R30-2: M4a/M4b sensitivities kept distinct;
  open-never-overwrites; renounce Slice-A no-op effect preserved).
- T30-5 sweep + closure + retention (R30-3/4/7/14: same-step close, record
  with PRODUCED cause, append-only retention, no expiry).
- T30-6 `Integration` wiring (R30-8/6/9-freeze; freeze on current base).
- T30-7 persistence path (append + replay equality + founding guard; no
  second writer).
- T30-8 client additions (R30-12 adapt-only; TEST-boundary roundtrip; limit
  stated).
- T30-9 mechanism-surface shaping (R30-10: hook/record/retention/atomicity
  extension points; L-1–L-7 recorded with owners; zero executable #81 rows).
- T30-10 drift inventories + required-example set (D2 frozen inventories;
  D3 §7-REQ list frozen in gate).
- T30-11 GREEN envelope + omission challenge + SLIM + full CI + hygiene
  (B1–B21, drift recon, B20, 3B SLIM, `Trivial` intact, tracked-clean).
- T30-12 audit handback + PR (fresh FULL audit; draft PR post-GREEN only;
  exact-SHA merge at desk).

## 7. Frozen requirement-to-command/control map (binding — D1–D6 applied)

Conventions: whole-project invocation = 1 BUILD (expected-RED, warm reruns,
mutant runs, omission-challenge rerun all count). Per-mutant cycle = apply +
run + revert, hash-verified restore (failure aborts exit 3, no fallthrough).
Hidden invocations forbidden: EVERY cited result maps to a counted B-row or
PROBE-row below; no uncounted `--match`, no uncompiled instrument, no
uncounted positive control. Predicates (D6): COMPILER-kill = exit≠0 AND
diagnostic quotes ctor/site AND zero parse-error lines (setup/infra failure
= INCONCLUSIVE abort, never kill); TEST-kill = exit≠0 AND `Failures:` names
≥1 registered §7-REQ example of the row (empty `Failures:`, crash, timeout,
infra, parse NEVER count); GREEN-ENUM (M10b) = exit 0 AND enumeration lists
every allowed ctor. Charge-0 recon (reads, greps incl. the D7 tripwire,
`git status/diff/rev-parse/log`, `gh issue view`, `--version` pin reads):
free, never evidence, never a kill.

§7-CMDS (exact arguments; toolchain re-pinned exactly at freeze):
- `nix develop .#ci --quiet -c just build` (= `cabal build all -O0`)
- `nix develop .#ci --quiet -c cabal test all -O0 --test-show-details=direct`
- `nix develop .#ci --quiet -c just ci` (format + cabal-fmt + lint + build +
  test + `cd lean && lake build` + `cd client && spago build` + `spago test
  -p kelgroups-client`)
- probes: `nix develop .#ci --quiet -c cabal test invariants
  --test-option=--match --test-option=/S30-<Group>/<REQ-ID>/` with REQ-ID
  from §7-REQ (exact strings in-contract — no `… --match` vagueness).
- pins: `nix --version` + one batch `ghc/cabal/lake/node/spago/just
  --version` (S28 leg-2b shape; values re-pinned at freeze, never carried
  over).

§7-REQ (REQUIRED examples, named in-contract, independent of emitted/
registered sets — D3): REQ-OPEN-COLL, REQ-OPEN-PERM, REQ-OPEN-REFUSE,
REQ-OPEN-DUP, REQ-CAST-ASSENT, REQ-CAST-SWITCH, REQ-CAST-POSTSWITCH,
REQ-CAST-RECAST, REQ-CAST-UNKNOWN, REQ-CAST-NONRESP, REQ-SWEEP-TALLY,
REQ-SWEEP-DISSENT, REQ-SWEEP-FRANCHISE, REQ-RETAIN, REQ-NOEXPIRY,
REQ-VERDICT-COLL, REQ-VERDICT-PERM, REQ-FRANCHISE-CURRENT, REQ-NEG-DELIVER,
REQ-ROUTE-ENUM (M10b positive claim), REQ-HOOK-EXT, REQ-RECORD-SHAPE,
REQ-CLIENT-ROUNDTRIP, REQ-ADMIT-PATH, REQ-NONDECIDE-PERM. Gate cross-check:
every REQ-ID appears in the registered set AND the execution log, else RED;
B20 omission challenge (one REQ-ID removed from spec → RED) is the ONLY
truncation-guard falsification.

§7-DRIFT (D2 REAL cross-language check): frozen `lean-vote-ctors.inventory`
(every Vote-subtree inductive ctor at freeze, `file:line`) +
`hs-vote-matches.inventory` (every Haskell mirror match site, `file:line`);
reconciliation leg joins them (each Lean ctor ≥1 Haskell arm; each Haskell
arm maps to a Lean ctor or a mandate-referenced extension). Omission
control: delete one entry from either inventory → reconciliation REDs.
Establishing: recon GREEN run (PROBE P-DRIFT-1) + omission RED run (PROBE
P-DRIFT-2). Haskell exhaustiveness stays IN-LANGUAGE evidence only
(labeled; never the drift claim).

§7-MAP (obligation → establishing command(s); auditor column marks RE-RUN
vs EVIDENCE-REVIEW — D5):

| obligation | owner establishing (class) | auditor establishing (class) |
|---|---|---|
| absence (T30-1) | B1 build-absence RED + B2 test-absence RED (BUILD ×2; absence ONLY) | A2/A3 independent rerun (RE-RUN); evidence attribution check (REVIEW) |
| R30-1 opens/refusal | B4 leg-4 run (shared) + probes per REQ | A3 (RE-RUN); kill-evidence review (REVIEW) |
| R30-2 placement | B4 shared + probes; M2/M3/M4a/M4b runs (B6–B9) | A3 (RE-RUN); ≤5 kill re-runs incl. M4b when disputed (RE-RUN); rest REVIEW |
| R30-3 sweep/retain | B4 shared (incl. Store append + replay); M5/M6/M7/M9 runs (B10–B12,B14) | A3 + A10 boundary rerun (RE-RUN); rest REVIEW |
| R30-4 verdict | B4 shared; M8 run (B13) | A3 (RE-RUN); REVIEW |
| R30-5 refusals | B3 cold build (vocabulary compiles; 3-arm exhaustive) | A2 (RE-RUN); tripwire output reviewed, never a kill (REVIEW) |
| R30-6 franchise | B4 shared; M13 franchise-snapshot run (B18) | A3 (RE-RUN); REVIEW |
| R30-7/14 negative delivery | B4 shared at integrated boundary + persistence roundtrip | A3 + A10 (RE-RUN) |
| R30-8 separation | B3 (2-arm enactment compiles); M10a compile-RED (B15, COMPILER predicate); M10b GREEN-ENUM in B4 (named sub-claim) | A2 (RE-RUN); M10a re-run when disputed (RE-RUN); M10b enumeration re-checked in A3 (RE-RUN) |
| R30-9 approve/V-2 | freeze on current base (B3/B4); conditional rebind check (AUDIT-side A11 iff #68 landed) | A11 conditional (RE-RUN iff landed, else returned unspent with reason) |
| R30-10 mechanism surface | B4 shared (hook composition runs; record shape carries 4 causes as DATA); M11 hook-ignored run (B16, restoration witness); M14 produced-cause collapse run (B19, tally/franchiseChange only) | A3 (RE-RUN); REVIEW |
| L-1–L-7 downstream | recorded with owners (no command — explicitly NOT established here) | verified as recorded-with-owner (REVIEW of the record, never behaviour) |
| R30-10U/R30-5-prod | no command (preserved boundary; tripwire recon only) | — |
| R30-11 | no wire, no mock | — |
| R30-12 client | B21 leg-6 (`spago build` + `spago test`; TEST-boundary roundtrip); M12 run (B17) | A4 (RE-RUN); REVIEW |
| R30-13 Lean | B21 (`lake build` green only) | A4 (RE-RUN) |
| drift (D2) | P-DRIFT-1 GREEN + P-DRIFT-2 omission-RED (PROBE ×2) | recon rerun (PROBE, from auditor probe cap) |
| truncation guard (D3) | B20 omission-challenge rerun (BUILD ×1; ONLY falsification of the guard) | B20 evidence review (REVIEW) + D3 cross-check in A3 scope |
| cold/final | B3 cold 1B (COLD/WARM logged) + B21 final `just ci` 1B + tracked-clean + `Trivial` presence-only + founding guard | A2/A4 (RE-RUN) |
| SLIM | 3B identical-envelope (legs 1,2,2b,3,4,6,7 analog) | — |

Mutant run ledger (15 runs, B5–B19): M1 openQuestion-nonresp bypass
(TEST/REQ-OPEN-REFUSE); M2 cast-nonresp bypass (TEST/REQ-CAST-NONRESP); M3
unknown-question accept (TEST/REQ-CAST-UNKNOWN); M4a erase-drop
(TEST/REQ-CAST-SWITCH; REQ-CAST-RECAST still passing recorded as challenge
history, never a kill); M4b unguarded-insert (TEST/REQ-CAST-RECAST;
post-switch validity REQ-CAST-POSTSWITCH distinguished in naming); M5
tally-suppress (TEST/REQ-SWEEP-TALLY); M6 dissent-suppress
(TEST/REQ-NEG-DELIVER); M7 cause-collapse-to-tally
(TEST/REQ-SWEEP-FRANCHISE); M8 permission-tally-consult
(TEST/REQ-VERDICT-PERM); M9 close-and-discard (TEST/REQ-RETAIN); M10a
admission-ctor-added (COMPILER/narrow interface-existence ONLY); M11
hook-ignored-always-commit (TEST/REQ-HOOK-EXT restoration); M12 dropped
propose-path (TEST/REQ-CLIENT-ROUNDTRIP at client boundary); M13
franchise-snapshot-in-payload (TEST/REQ-FRANCHISE-CURRENT); M14
tally/franchiseChange collapse (TEST/cause-distinction; carried causes
excluded per D1). M10b positive enumeration (GREEN-ENUM in B4): every
ALLOWED public proposal ctor encodable + enacted; interface proposition,
never behavioural refusal.

Discovery bounds: extent quantified over observed Lean equation sites
(`placeBallot` 2 arms `Fold.lean:53-56`; `sweepStep` 2 `:65-66`;
`effectedState` 3+2 `:89-101`; `validateVoteEvent` 3+2
`Validate.lean:57-70`; `verdictOf` 2 `State.lean:87-96` + `closureCause` 3
`:111-113`; `sweepClosures` shared step `Fold.lean:72-76`). Discovery bounds
the mutant SET (new ctor ⇒ new match arm ⇒ new mutant required, never
silent pass); it never reduces row coverage (every §7-REQ keeps its
establishing command above).

Charge-0 recon (free, never evidence): reads, greps incl. D7 tripwire
(`grep -rn "notDesignee\|notProposer" lib/ --include=*.hs` minus the
vocabulary-declaration file — review-time tripwire; sole job:
accidental-introduction detection → mandate review, never a kill),
`git status/diff/rev-parse/log`, `gh issue view`, `--version` pin reads.

## 8. Candidate-independent initial gate design (D2/D3/D6/D7 applied)

Fence (proposed writable set; ticket owner versions at freeze): WRITABLE —
new `KelGroups.Vote.*` mirrors, `Integration`-wiring integration points
(`Fold.hs` composition; `State.hs`/`Event.hs` only where wiring requires),
`Store.hs` vote persistence path (existing integrated path, no second
writer/tables), test proving instance (new `S30*Spec` + proving-app),
client Api additions (adapt-only), `kelgroups.cabal` + `test/Main.hs`
registration, drift inventories + required-example list (frozen gate
artifacts, not production). FENCED — `lean/**`, historical
`Proposal`/`BaseEvent`/`GroupEvent`/`validateEvent` bodies, `Trivial.hs`
(presence-only, uncounted), S28 production outside named points,
`client/` beyond adapt-only, UI/wasm/economics, release metadata.

Legs (K/M frozen by ticket owner from the actual spec): 1 hygiene
before/after; 2 identity + self-hash (blank-normalized) + ancestry (HEAD
descends from frozen slice base); 2b pins exact, fail-closed (re-pinned at
freeze); 3 build cold/warm; 4 inventory (≥K vote groups) + registered
(§7-REQ cross-check: every REQ-ID registered, total == file examples, no
orphans) + execution (every registered REQ-ID executed, none pending;
historical suites green) + exit 0; 5 mutants M1–M14 per §7 ledger with entry
(tracked-clean committed candidate at recorded HEAD), per-run
apply+run+revert, hash-verified restore every exit path, restoration failure
aborts exit 3; DRIFT reconciliation + omission control (P-DRIFT-1/2);
M10b GREEN-ENUM check inside leg 4 scope; 6 full `just ci`; 7 `Trivial`
presence (exports present, zero slice refs — not counted) + client CI
presence. `set +e` throughout; full log + per-leg evidence with sha256;
meta file binding version/HEAD/evidence. B20 omission challenge is a
separate counted rerun of leg 4 against a one-REQ-removed spec (RED
required); C1/C2 RED stays absence-only and is never cited for the guard.

Kill-attribution (D6): COMPILER predicate for M10a (exit≠0 + diagnostic
quotes ctor/site + zero parse-error lines); TEST predicate for all
behavioural (exit≠0 + `Failures:` names ≥1 registered REQ-ID of the row —
M4a/M4b each name their own; M4a additionally checks REQ-CAST-POSTSWITCH
passes as labeled challenge-history context, never as the kill);
GREEN-ENUM predicate for M10b. Setup/infra failure, crash, timeout, parse
error = INCONCLUSIVE abort in all three, never a kill.

Evidence bindings: `run-receipt`-style capture (command hash, exit,
duration, evidence hash, bytes, lines, path) for every cited run; evidence
dir per campaign; self-hash in leg 2; ancestry in leg 2; mutant diff hashes
pre-run; restore hashes post-run.

Spend classes: BUILD (whole-project invocation — charged 1); PROBE (narrow
execution: `--match` with exact §7-REQ strings, single-component build,
drift recon runs — charged 1 against the 24-probe cap); CHARGE-0
(enumerated recon — free, never evidence); AUDIT-BUILD/AUDIT-PROBE (same
inside the auditor seat against the auditor cap). No parallel heavy builds;
every failed setup/attempt journaled; no automatic raises (exact gap
returned before exceeding).

## 9. Operational classifications + proposed ceilings (FROZEN with the §7 map)

Bottom-up fit: RED 2 (B1+C1? — precisely: B1 C1 build-absence + B2 C2
test-absence) + GREEN 19 (B3 cold + B4 full test + B5–B19 fifteen mutant
runs + M10b enumeration inside B4 as named sub-claim + P-DRIFT-1/2 probes
from the probe cap + B20 omission challenge + B21 final CI) + SLIM 3 = **24
builds**. Probes ≤24 counted (P-DRIFT-1/2 + per-REQ `--match` reruns +
dispute narrowing; charge-0 enumerated and free). **PROPOSED owner ceiling:
24/24.** Supersedes 20/24 (which undercounted M4b, M13-as-run, M14,
M10b-establishment and B20). Caps remain PROPOSALS pending fit-proof at
freeze + authorization; gap returns exact workload/cost, never trimmed
scope.

Auditor command table (pre-dispatch, exact; fits 12/24): A1 pins+identity
recon (0B); A2 cold build 1B; A3 full test incl. §7-REQ cross-check +
M10b re-check 1B; A4 full CI 1B; A5–A9 ≤5 disputed-kill re-runs 5B
(selection criteria frozen pre-dispatch: dispute first, then M4b/M10a/M11
risk order, then rotation; undisputed kills get EVIDENCE-REVIEW against
the D6 predicates — labeled review, never assumed); A10 boundary reruns
(Store replay + client roundtrip) 2B; A11 D3 omission-challenge evidence
review + conditional R30-9 rebind re-run (BUILD iff #68 landed, else
returned unspent with reason) 1B; A12 repair-verification reserve 1B;
D2 recon rerun from the auditor probe cap (PROBE ×1 + omission spot-check
×1). **PROPOSED auditor ceiling: 12/24.** Seat: fresh Codex-or-Grok (never
Muse/GLM/Claude), clean detached worktree at candidate SHA, argv-pinned
model+effort, post-cursor START, hash-bound report; auditor recommends,
ticket owner decides; every repair gets a fresh auditor.

Team (standing fence, commissioned at authorization — NOT by this packet):
Muse ticket owner → Muse commit owner (`draft=NONE`) → fresh Codex-or-Grok
auditor per submission (max two audited submissions, one findings bounce,
repair re-audited); signed commits; draft PR post-GREEN only; exact-SHA
merge at desk.

## 10. Acceptance (binding when commissioned)

Executable controls on the candidate SHA: threshold-parameterized verdicts
(permission never tallies); explicit closure records retained with
PRODUCED causes (never silent drop; no expiry); refusal before durable
effects (aggregate AND log unchanged; accepted-KEL replay identical);
validate/fold agreement (same aggregate/event/signer; never historical
`validateEvent`, never same-wrapper-twice); negative delivery observable at
the integrated boundary (positive AND negative with causes through step +
append + replay); no dormant-constructor production sites (tripwire clean
at review); §7-REQ complete (every ID registered + executed; B20 RED
observed); drift reconciliation GREEN + omission RED observed; M10a
COMPILER-kill + M10b GREEN-ENUM observed; client additions under client CI
(TEST-boundary limit stated); `Trivial` intact; full `just ci` green;
tracked-clean before/after; founding guard held; L-1–L-7 recorded with
owners (reviewed as record, never as behaviour); fresh audit PASS with
complete matrix; bounded claims only.

## 11. Open questions / dependencies (enumerated, not invented)

- `paolino/reactivegas#68` (V-2) → R30-9 boundary rebind + revalidation
  only. Nothing else waits.
- `paolino/reactivegas#81` (V-5 content §1–§3; L-7 gated on #76) → R30-10
  content; unruled exclusions depend on nothing.
- `paolino/reactivegas#76` (composition wire) → Reactivegas side; kelgroups
  exposes interface + closure evidence only.
- `paolino/reactivegas#75` (R3.1 replay context) → test input for the
  persistence boundary (threshold = test input), not a shipped default.
- Upstream Lean gaps → enumerated here, never invented; later landings
  rebind explicitly.

## 12. Provenance + freeze record

Sources read (read-only; newest governs; no blocking conflicts): epic
`handoffs/T30-MANDATE-v3.md` + `T30-REQUIREMENT-MAP-v3.md` (v1/v2 history);
bodies `paolino/kelgroups#30` (2026-09-06 correction), `#29` (2026-09-06
Lean correction), `#33`, `#34`; accepted Lean Vote + Integration/State/
Validate @ `3590c001` (zero-diff EMPTY vs `4a6cd87`); V-2 ruling +
`paolino/reactivegas#68` OPEN; `paolino/reactivegas#81` body; R3.1
(threshold = test input); S28 interface @ `933e385d`; S28 gate v10.2 shape;
commissioning note (honest-count + preparation boundary — honoured);
NOTE-001 + NOTE-002 (clock + helper rules — honoured: helper-stamped STATUS
from 04:11:04Z); parent assessment `T30-R1-ASSESSMENT.md` (governs this
r2). Inbox checked before r2 filing (NOTE-003 read + acked; no other
unread). Spend: 0 builds, 0 probes, 0 mutations, 0 gate runs. Skills:
orchestrator-contract, ticket-orchestrator, resolve-ticket (planning only),
context-compiler, worker-protocol, tmux-orchestrator, verification,
invariants, gate-script, haskell, nix, lean4 (read-only).

Frozen content hashes (sha256, at write time):
- base HEAD `933e385df2f2a251bb54a08bb7663f0d41fafb64`
- Lean `3590c0015b84fd58004bf6fb44dd18b107304c48`
- brief `f6d857639a4f3d9aa466c081305f43ae9e59bad4fd9dcb790a08bd85fc856416`
