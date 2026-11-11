# T30-CONTRACT-r7 — kelgroups #30 substrate vote interface + closure evidence (FOR IMPLEMENTATION AUTHORIZATION)

Ticket preparation owner `t30-contract` (Muse), 2026-09-06. Preparation-only:
read-only inspection + own planning artifacts. No compilation, tests,
mutations, gate runs, dispatch, product edits, commits, push/PR/merge/release,
issue comments, or spend (spend 0 product builds; 2 plumbing invocations on
the closed pf1 campaign, own counter).

- Brief sha256 `f6d857639a4f3d9aa466c081305f43ae9e59bad4fd9dcb790a08bd85fc856416`.
- SUPERSEDES r6 (r1–r6 retained unmodified; all prior terminals stand as
  history). This r7 is the NOTE-008 successor packet: the six verified
  blockers fixed by design in a coherent successor (script + harness +
  exact fixture set + corrected scope/counts), then ONE single final
  synthetic-campaign request for desk ruling (§12-R7). No third harness
  execution has occurred here; no measurement/product allowance taken.
- Operative base kelgroups `main` @ `933e385df2f2a251bb54a08bb7663f0d41fafb64`
  (S28 LANDED, PR#32; CI+Release SUCCESS; #28 CLOSED). Accepted Lean
  Reactivegas @ `3590c0015b84fd58004bf6fb44dd18b107304c48` (zero-diff EMPTY
  vs `4a6cd87`; landings rebind explicitly, never anticipated).
- Companions: `T30-COMMAND-MAP-r7.md` (obligation→command map);
  `T30-IDENTITY-MAP-r5.md` + `T30-IDENTITY-APPENDIX-r6.md` (true
  per-identity ground — retained, unchanged, still current: no scope or
  Lean change since); `T30-DRIFT-LEG-r7.sh` (successor script text);
  `t30-contract/scratch/pf7/run.sh` (successor harness + exact fixture
  set, in-file). This contract ≠ the ticket owner's final immutable gate.

## 0. r7 records (review + campaign truth + fixes + printf correction)

i. REVIEW RECORD: NOTE-008's six blockers verified against the r6 bytes
before any writing (all confirmed: HEAD:-refs with unused HS pin;
count-defeat by lose-one+duplicate-one and no live-minus enumeration;
REQ name-only grep; BRE-literal row-count; timestamp-only freshness +
equal-time + silent modes + re-read staleness; overgeneralized printf
attribution). Valid earlier corrections (D1–D7, G1–G5, N5-1–N5-6, F1–F8,
pf1 campaign truth) stand.
ii. PRINTF CORRECTION (log-evidenced facts ONLY — no generalization):
retained logs show `printf: --: invalid option` errors; source audit shows
THREE leading-dash-format sites (Vote-loop format-leading-dashes,
base-loop bare-`--`, P11 bare-`--`) — the r6 account's "2 sites" was
itself imprecise, owned here. Per-site behavior beyond the logged errors
is NOT established (the cascade after the first failure is unreconstructed
— stated, not filled in). Successor eliminates the entire class (no format
may begin with `-`; heredoc/`%s`-arg forms only) + fixture smoke
assertions (`test -s` after every generated artifact — silent corruption
becomes loud setup failure) + tool-allowlist self-check (fail-fast).
iii. HEADER OVERCLAIM REMOVED: r6 script's "demonstrated" header line is
withdrawn in the r7 successor (honest status instead); r6 bytes preserved
untouched.
iv. FIXES (blocker → design, each in the r7 artifacts):
(1) frozen-oid reads (`git show $FROZEN_OID:path`, content-addressed) +
separate HEAD-position checks (reference-vs-content never conflated) +
STATED hash assumption (SHA-256 second-preimage; a break breaks L1/L4
binding and nothing else); (2) live-minus-mapping: mapping row-uniqueness
(same-size swap attacks refused) + source-hash tripwire per mapped module
(immutable-view reads; catches unexported additions the .hi tripwire
cannot see) + same-size/duplication campaign fixtures; (3) REQ
exact-success records (`PASS: <id> OK` full-line fixed-string;
FAILED/SKIPPED/bare names never match) + adversarial leg4 fixtures;
(4) POSIX BRE row-count (`^[[:space:]]*\(#.*\)\{0,1\}$`) + comments-only
fixture as parsed-construct proof (a literal-paren reading yields a
different outcome); (5) receipt precondition (`exit=0` bound before any
emission — touch-faked outputs refused) + freshness-marker rule retained
AND-combined + unknown-MODE refusal (exit 3) + unconditional re-emit
(never reuse; stale outputs not freshened by re-reading); (6) attribution
per this section — log-evidenced facts only.
v. DELTAS vs r6 (all else carried — scope/spec/plan/models/tasks/REQ-26/
fences unchanged): probe tables rebalanced within caps (owner drift
required 5→7: +SAMESIZE +SRCADD; kill-confirm ≤15→≤13 priority-ordered;
dispute/transient unchanged; auditor drift required 6→8, narrowing
≤14→≤12, reconfirm unchanged); builds IDENTICAL (owner 26, auditor 25);
above-20 justifications IDENTICAL (6 items); per-repo tables + B22a/b +
A-HIDEMOa/b carried; falsehood ledger carried (F1–F8) + F10 (this
section's printf precision correction).
vi. COUNTS HELD: owner builds 26/24, auditor builds 25/24, probes 24/24
both — rebalanced, never exceeded, row coverage IDENTICAL (15 kill runs +
all 26 REQ rows + all drift directions; only discretionary probe
partitioning shifted with priority rules).

## 1. Objective (one observable — unchanged)

A nondegenerate application opens an app-scoped assent question, casts
ballots as the franchise, and observes the verdict (positive AND negative,
each with explicit cause) plus its closure record through the **integrated**
boundary — refusals before durable effects, replay equality. Test-only
proving instance (as S28); the user demo is `paolino/kelgroups#33`.
Threshold a parameter at every evaluation; exhibits never defaults.

## 2. Scope — rows (FULL #30 scope; per-identity ground carried)

Accepted behaviours INTACT: converge to the LANDED S28 interface, never
redesign. `Trivial` degenerate presence-only. No unilateral Lean edits. No
threshold default. No expiry. No votable admission. No second store/fold.
Ground: identity-map r5 + appendix r6 (every Lean declaration mirrored or
exclusion-reasoned; zero Haskell requirements for proof-side helpers;
corroboration review-only; R-48 warning transferred; post-view caveat
carried; M15 grounded in Lean's sweepDuplicating).

R30-1 openQuestion (collective + permission-with-designee),
responsabile-only admission → Haskell #30. R30-2 placement/switch/
idempotence (guarded `setInsert` source-verified; M4a/M4b minimal criteria
+ classification; POSTSWITCH required; observed = freeze
characterization). R30-3 sweep/closure/retention AND non-duplication (M9
append-drop vs M15 filter-drop — distinct sites/duties/witnesses).
R30-4 verdictOf (parameter everywhere; exhibits never defaults). R30-5
refusals produced now; `notDesignee`/`notProposer` vocabulary-only,
UNSCHEDULED (tripwire only). R30-6 canonical-view franchise (M13;
post-view = sensitivity fixture, limit stated). R30-7/14 negative delivery
at boundary (consumption → #76). R30-8 route separation (bounded three-part
surface property). R30-9 LANDED base rule; V-2 rebind after #68 ONLY at
`tryEnactBase` + proposer rules. R30-10 mechanism surface ONLY (content →
#81 L-1–L-6; refund → #76 L-7; all recorded, never executed). R30-10U/11:
unscheduled / evidence-only. R30-12 client adapt-only (TEST-boundary
roundtrip; production-server roundtrip out of scope). R30-13 Lean-owned
where ruled (9 `#print axioms` persist Lean-side — referenced). R30-X
non-goals fenced. L-1–L-7 recorded with owners (reviewed as record). #29
remainder: #33 demo (blocked #30); #34 release/notes (blocked #33+#30; desk
authority); downstream notes (with #73). `#29` + #73 OPEN.

## 3. Compact spec (carried)

S1–S6: open; cast/switch/recast exactly-once; positive AND negative
verdicts with causes + retained, never-duplicated records; pre-effect
refusal (state AND log unchanged); accepted-KEL replay identical; client
propose/vote. Requirements R30-1–R30-8 + R30-12 + R30-14 now;
dependencies as §2; unscheduled items preserved; R30-X fences. Rejection
identities exact; vocabulary-only ctors never produced; no expiry refusal.
Production-path examples REQ-ADMIT-PATH, REQ-OPEN-REFUSE, REQ-CAST-NONRESP,
REQ-NONDECIDE-PERM (exercised, never widened/narrowed). Success: S1–S6 +
all 26 §7-REQ through the boundary on the candidate SHA; `Trivial`
intact; full `just ci` green; fresh audit PASS; no shipped defaults; no
silent drops; no duplicates.

## 4. Plan (carried; B22a/b split stands)

Single slice S30-1 (FULL boundary). No S30-2. RED absence proof (C1/C2 —
absence ONLY); GREEN in envelope (15 mutant runs + M10b enumeration + live
drift machinery + REQ set); B20 omission challenge (sole guard
falsification); B22a baseline scratch GREEN then B22b overlay (GREEN build
with real drift — .hi tripwire firing demonstration; TWO counted
invocations); SLIM S1–S3 itemized; FULL audit (every direction rerun, no
inheritance); draft PR + remote CI; acceptance. Ticket owner freezes the
final gate; §12 freeze-validation is the handoff check (+ shell-
portability (xi) + receipt-shape + POSIX-proof items).

Constraints: this packet cost 0 product builds (2 plumbing invocations on
the closed pf1 campaign, own counter; the R7 campaign awaits its own
grant); §8 fence; whole-project invocation = BUILD; S28 concurrency
discipline; no parallel heavy builds.

Live boundaries (can-fail controls): step/validate agreement; Store/KEL
append + replay (equality; founding guard); client TEST-boundary roundtrip
(limit stated); drift input-binding + pin + join (immutable frozen-oid
views; hashes under the stated assumption; clean samples); compiler
metadata (.hi tripwire + exhaustiveness + source-hash channel). No
behavioural proof by source text, no shrinkable inventories, no
absent-import as behaviour.

Order: S30-1 (B1–B21) → B22a → B22b (overlay, discarded) → SLIM S1–S3 on
candidate → audit → PR → #33 → #34. #68 touches only R30-9 on landing.

## 5. Models (compact; no implementation content — carried)

New `KelGroups.Vote.*` mirrors (depend on `GroupView` + S28 `State`);
`KelGroups.Fold` owns `Integration` composition (proving `AppState` or
dedicated fold — ticket owner decides; `BaseProposal` reading,
`proposalMutation → BaseMutation`, `digest`, hook composition nowhere
else); `KelGroups.Store` owns the vote path (existing integrated path; no
second writer/tables); `kelgroups-client` owns propose/vote (existing
transport, adapt-only); proving instance owns the test-only app. Drift
mapping + discovery + REQ list + identity ground + drift-leg script are
frozen gate artifacts.

Data: `Question{kind,proposer,assents,dissents}` (no time-like field);
`ClosureRecord{questionId,question,verdict≠open,cause}` (4 causes as DATA);
`VoteState{openQuestions,closed-append-only-unduplicated}`;
`Verdict`; `Threshold` (parameter); `QuestionKind{collective,
permission(designee)}`; `ClosureCause{4}`; `VoteEvent{openQuestion,cast,
renounce}` (T6222); `VoteError` (2 produced + 2 vocabulary-only).
Franchise never stored; verdict single site; closure = remove + append
atomically; re-sweep stable (REQ-SWEEP-IDEM).

Functions (spellings frozen by ticket owner): vocabulary constructors;
`verdictOf`; `franchise`/`franchiseSize`/`isResponsabile`; `lookupQuestion`;
`closureCause`; exhaustive `validateVoteEvent`; `placeBallot`;
`sweepStep`/`sweepClosures`; `effectedState` (authorization-free);
`applyVoteEventChecked` (integrated path uses it, never revalidates);
`foldVote`/`foldFrom`; wiring (`digest`, `proposalMutation`, vote-aware
`appFold`/`baseHook`). `GroupView`/`Threshold` explicit everywhere.

## 6. Tasks (slice S30-1 — carried)

T30-1 RED absence (C1/C2, absence ONLY — Vote-absence re-verified).
T30-2/3/4/5 (vocabulary; validation; placement + classification;
sweep/closure/retention/non-duplication) + T30-6 wiring (current base) +
T30-7 persistence (append + replay + founding guard; no second writer) +
T30-8 client (adapt-only; limit stated) + T30-9 mechanism surface (L-1–L-7
recorded; zero executable #81 rows) + T30-10 drift mapping + live
machinery + REQ set + identity ground + drift-leg script (r7 successor) +
T30-11 GREEN envelope + omission challenge + B22a/b + SLIM + CI + hygiene
+ T30-12 audit handback + PR (every direction rerun; draft post-GREEN;
exact-SHA merge at desk).

## 7. Frozen requirement-to-command/control map (binding — r7 deltas applied)

Conventions: whole-project invocation = 1 BUILD. Per-mutant cycle = apply +
run + revert, hash-verified restore (failure aborts exit 3). Hidden
invocations forbidden (leg-unit accounting). Predicates: COMPILER-kill
(exit≠0 + diagnostic quotes ctor/site + zero parse-error lines); TEST-kill
(exit≠0 + `Failures:` names ≥1 registered REQ-ID; empty/crash/timeout/
infra/parse NEVER count); GREEN-ENUM (exit 0 + lists every allowed ctor).
Setup/infra failure = INCONCLUSIVE abort, never kill. Charge-0 recon free,
never evidence. BAN: regex output never cited as semantic inventory.
Portability: POSIX sh + POSIX BRE; no leading-dash formats (stated rule
from F9/F10).

§7-CMDS (exact; re-pinned at freeze): `nix develop .#ci --quiet -c just
build`; `... cabal test all -O0 --test-show-details=direct`; `... just ci`
(INCL kelgroups-own `just lean` — tabulated); probes `... cabal test
invariants --test-option=--match --test-option=/S30-<Group>/<REQ-ID>/`
(Groups frozen — fully determined); `nix --version` + batch versions.

§7-REQ (26 IDs, frozen Groups — carried). Every ID registered + executed
else RED. B20 (one ID removed → RED) ONLY guard falsification; C1/C2
absence-only. REQ records in leg-4 log are exact-success lines
`PASS: <id> OK` (full-line fixed-string; FAILED/SKIPPED/bare names never
match — adversarial fixtures demonstrate both directions).

§7-PERFILE (actual 7-file extent, all read in full — carried): inductives
MUST emit; structures + equation groups MUST emit; Invariants/Tests
expected-empty (proof-/witness-only; identities tabled + 40-name appendix).
Other-file-empty → RED; empty-global → RED always. Projection statement
(appendix §C): 12 paths bound (7 Vote + Integration/State/Validate/Event/
Types); Fold/Invariants/Tests (KelGroups) projected out with zero-Vote-
identifier verification + per-file reason.

§7-DRIFT (frozen-oid reads + named oracles; script r7 — UNDEMONSTRATED,
demonstration pending §12-R7): L1 binding (immutable `git show
$FROZEN_OID:path` streams both repos + HEAD-position checks + file-sets
(both scopes) + mapping self-check + labeled clean samples; residual race
stated; hash assumption stated). L2-as-execution DROPPED. L3 join (frozen
mapping ⨝ live emission + leg-4 exact-success log; NONEMPTY + EXACT-COUNT
+ row-UNIQUENESS anti-vacuity gates; unmapped/dangling → RED). L4 .hi
tripwire (GHC oracle; receipt-preconditioned emission per frozen-module
row incl. `KelGroups.Event`; freshness-marker rule; exactly-one-or-RED;
unconditional re-emit; hash-pin; diff→review; firing demo rides B22a/b —
a failing build promises no `.hi`, stated) + source-hash tripwire per
mapped module (immutable-view reads; catches unexported additions —
INDEPENDENT channel, demonstrated by the source-add fixture with .hi
clean). L5 arm totality (GHC -Werror, live; M10a's break in B15's log —
the break only). Function presence = compilation + REQ-execution. Lean
patterns = REVIEW PROMPTS only. Baseline = review + signed record
(enforced:REVIEW). Temporal hole → explicit-rebind rule. REQUIRED drift
probes (7): P-DRIFT-GREEN/ADDBYTE/DELBYTE/ADDFILE/JOINMAP/SAMESIZE/SRCADD
(archive export + ONE edit, export-diff bound — trigger discrimination on
source-shaped bytes, NOT review correctness). Output-copy controls
DELETED. Item attribution = enforced:NONE automatic + MANDATORY re-review
(RED on ANY mismatch until signed).

§7-HI (specific instrument): emission in B3's leg (post-exit-0 evidenced
by receipt; marker rule); per-row `<module>.hi.dump + sha256`;
reconciliation vs frozen (selector fix: Vote.* + `KelGroups.Event` +
mapped wiring modules); firing demonstration = B22a (baseline GREEN
proves export integrity) then B22b (overlay drift ⇒ diff fires); auditor
mirrors A-HIDEMOa/b.

§7-SURFACE (bounded, no universals): frozen allowed set — each ctor maps
(D2 join; new ctor ⇒ drift RED) + each mapping enacts non-admitting (M10b
GREEN-ENUM through totality-witness `case`) + vocabulary closed at
enactment (M10a COMPILER). Review-only remainder named (allowed-set
matches intent). Direct-only unweakened. Untracked-ctor totality stands as
compiler-totality.

§7-MAP (obligation → owner → auditor; RE-RUN vs REVIEW): absence B1+B2 |
A-RED1/A-RED2. R30-1 B4+probes | A-TEST. R30-2 B4 + B6 M2 + B7 M3 + B8 M4a
(SWITCH REDs) + B9 M4b (RECAST REDs; inverts source-verified guard) +
classification (POSTSWITCH REQUIRED; observed = freeze characterization) |
A-TEST + 15 UNCONDITIONAL A-K. R30-3 B4 + B10 M5 + B11 M6 + B12 M7merged +
B14 M9 + B19 M15 (filter-drop vs append-drop; duplication vs retention;
Lean's own mutant shape) | A-TEST + A-Ks (M6 = named boundary rerun).
R30-4 B4 + B13 M8 | A-TEST. R30-5 B3 | A-COLD + tripwire review. R30-6 B4
+ B18 M13 (post-view = sensitivity fixture, limit stated) | A-TEST +
A-K13. R30-7/14 B4 | A-TEST + A-K6. R30-8 + surface B3 + B15 M10a
(`KelGroups/Event.lean` site; COMPILER/CLOSED-totality; in-log
exhaustiveness-fire secondary — NO .hi secondary) + M10b named in B4 |
A-COLD + A-K10a UNCONDITIONAL + M10b in A-TEST. R30-9 freeze | A-REBIND
iff #68 (author integrates + fresh final-SHA audit; auditor never
repairs). R30-10 surface B4 + B16 M11 | A-TEST + A-K11. Produced-cause
distinction B12 M7merged (carried excluded) | A-K7. L-1–L-7 recorded, NO
command | record-only review. R30-10U/PROD, R30-11 no command. R30-12 B21
leg-6 (limit stated) + B17 M12 | A-CI + A-K12. R30-13 B21 kelgroups-`lake
build` | A-CI (+ 9 `#print axioms` persist Lean-side — referenced). Drift
7 REQUIRED probes | binding GREEN + 7 directional reruns (auditor PROBE
×8: binding + directionals, named, no inheritance). Guard B20 | A-OMIT (+
labeled B19-log read). .hi tripwire B3 emission + hash-pin; B22a baseline
+ B22b firing demo | A-COLD mirror + A-HIDEMOa/b. Cold/final B3 + B21 +
tracked-clean + Trivial-only + founding guard | A-COLD/A-CI. SLIM S1
slim-build + S2 slim-test + S3 slim-ci (itemized).

Mutant ledger (15 runs B5–B19): M1 (REQ-OPEN-REFUSE); M2 (REQ-CAST-
NONRESP); M3 (REQ-CAST-UNKNOWN); M4a erase-drop (SWITCH REDs); M4b
unguarded-insert (RECAST REDs); M5 (REQ-SWEEP-TALLY); M6 (REQ-NEG-DELIVER);
M7merged forced-.tally (REQ-SWEEP-FRANCHISE; carried excluded); M8
(REQ-VERDICT-PERM); M9 close-and-discard (REQ-RETAIN); M10a admission-ctor
(COMPILER/CLOSED-totality); M11 hook-ignored (REQ-HOOK-EXT); M12
propose-path dropped (client boundary); M13 snapshot (REQ-FRANCHISE-
CURRENT); M15 sweep-without-removal (REQ-SWEEP-IDEM). M14 RETIRED (merged;
number not reused). M10b GREEN-ENUM in B4 (totality-witness `case`;
interface proposition, never behavioural refusal).

Discovery bounds: quantified Lean equation sites (placeBallot 2;
sweepStep 2; effectedState 3+2; validateVoteEvent 3+2; verdictOf 2 +
closureCause 3; sweepClosures shared step). Bounds the SET, never coverage.

Charge-0 recon (free, never evidence): reads; D7 tripwire (review-time;
accidental-introduction → mandate review, never kill); `git
status/diff/rev-parse/log`; `gh issue view`; `--version` pin reads.

## 8. Candidate-independent initial gate design (r7 script status noted)

Fence (ticket owner versions at freeze): WRITABLE — new `KelGroups.Vote.*`
mirrors; `Integration`-wiring points; `Store.hs` vote path (existing
integrated path; no second writer/tables); proving instance; client Api
adapt-only; `kelgroups.cabal` + `test/Main.hs`; drift mapping + discovery
+ REQ list + identity ground + drift-leg script (frozen gate artifacts).
FENCED — `lean/**` (both repos, read-only inputs); historical bodies;
`Trivial.hs` (presence-only, uncounted); S28 production outside named
points; `client/` beyond adapt-only; UI/wasm/economics; release metadata.

Legs: 1 hygiene before/after; 2 identity + self-hash + ancestry; 2b pins
exact, fail-closed; 3 build cold/warm; 4 inventory + registered (REQ
cross-check incl. exact-success records) + execution (all executed, none
pending; historical green) + exit 0; 5 mutants B5–B19 (tracked-clean
candidate; apply+run+revert; hash-verified restore; failure aborts exit 3)
under MINIMAL criteria + CLASSIFICATION PROCEDURE (extras recorded +
attributed same-cause/independent/setup — setup never default;
unattributed extras ⇒ INCONCLUSIVE); DRIFT leg (r7 script text —
UNDEMONSTRATED; §12-R7 campaign pending; freeze runs it only after a
granted GREEN); M10b GREEN-ENUM in leg-4 scope; 6 full `just ci`; 7
`Trivial` presence + client CI presence. `set +e`; full log + per-leg
sha256; meta file. B20 separate counted leg-4 rerun minus one REQ-ID (RED
required); C1/C2 absence-only. Isolation: single-site splice, diff-hash
bound, named attribution per RED. Freeze deliverables: OBSERVED mutant
signatures as characterization (not acceptance) + expected-line capture +
witness-`case` presence.

Kill-attribution: COMPILER / TEST (REQ-ID-naming) / GREEN-ENUM. Criterion
met + unclassified extras = INCONCLUSIVE abort, never kill.

Evidence: `run-receipt`-style capture per cited run; self-hash + ancestry;
mutant diffs pre-run; restores post-run; overlay export-diffs bound; .hi
hashes bound; immutable-view hashes bound; build receipts bound.

Spend classes: BUILD (whole-project = 1); PROBE (narrow = 1 vs 24 cap);
CHARGE-0 (enumerated recon — free, never evidence); AUDIT-BUILD/AUDIT-PROBE
(same, auditor cap). No parallel heavy builds; every failed setup/attempt
journaled; no automatic raises (exact gap first).

## 9. Operational classifications + proposed ceilings (FROZEN with §7) +
per-repository call tables

Owner fit: B1–B2 RED (2) + GREEN 22 (B3 cold + B4 test + B5–B19 fifteen
runs + M10b named in B4 + B20 omission + B21 CI + B22a baseline + B22b
overlay) + SLIM S1–S3 (3) = **26 builds**. Above-20 justifications (each
ordered): M4b←D4, M13←D5, B20←D3, B22a+B22b←NOTE-006-2c + NOTE-007(d),
M15←NOTE-006-1. M10b in B4; drift overlays ride drift probes. Probes ≤24:
7 REQUIRED named (P-DRIFT-GREEN/ADDBYTE/DELBYTE/ADDFILE/JOINMAP/SAMESIZE/
SRCADD) + kill-confirm ≤13 (ambiguous logs only, priority-ordered:
disputed/ambiguous first) + dispute ≤2 (beyond → BLOCKED) + transient ≤2,
REQUIRED-first. **PROPOSED owner 26/24** (held from r6 — NOTE-008 fixes
are logic-level, zero new invocations). PROPOSALS pending fit-proof (§12
i–xi + receipt/POSIX/portability items) + authorization; gap returns exact
cost, never trimmed scope.

Auditor (pre-dispatch, exact): A-RED1/A-RED2 2B; A-COLD (+ emission
mirror) + A-TEST (+ M10b re-check) + A-CI 3B; A-K×15 15B (UNCONDITIONAL;
REVIEW ONLY for L-records, tripwire output, B20-log-alongside-A-OMIT —
each named); A-OMIT 1B; A-REBIND conditional 1B (landed: execute;
unlanded: unspent WITH reason); A-RESERVE 1B; A-HIDEMOa baseline +
A-HIDEMOb overlay 2B; drift binding GREEN + 7 directional reruns from
probe cap (PROBE ×8, named). Named sharing: M6/A-K6 and M12/A-K12 are the
boundary reruns (no separate builds). **PROPOSED auditor 25/24** (25B;
probes 8 required drift + finding-narrowing ≤12 (priority-ordered) +
reconfirm ≤4 = 24). Bottom-up; coverage never trimmed. Seat fresh
Codex-or-Grok (never Muse/GLM/Claude), clean detached worktree at
candidate SHA, argv-pinned model+effort, post-cursor START, hash-bound
report; recommends, ticket owner decides; every repair gets a fresh
auditor.

KELGROUPS worktree calls (ONLY repo where the gate executes): B1
build-absence; B2 test-absence; B3 `just build` + marker + receipt capture
+ per-module `ghc --show-iface` emission + hash-pin steps; B4 full test (+
M10b + cross-check incl. exact-success records); B5–B19 apply + build-or-
test + revert + restore-verify; B20 leg-4 rerun minus one REQ-ID; B21
`just ci` (INCL kelgroups-own `just lean` — tabulated); B22a
scratch-export + baseline GREEN build; B22b overlay edit + build +
emission + diff-fire + discard; S1–S3; A-RED1/2, A-COLD (+ emission),
A-TEST, A-CI, A-K×15, A-OMIT, A-REBIND, A-RESERVE, A-HIDEMOa/b; drift
probes (hash/join/diff steps, no compilation); `--match` probes (exact
REQ-ID strings). REACTIVEGAS checkout calls (read-only — NO lake build:
upstream owns pinned-commit validity): `rev-parse`, per-file `git show
$FROZEN_OID:<path> | sha256sum`, `status --porcelain`, `ls-files`,
`git archive` (each rides its leg's/probe's counter — leg-unit accounting,
stated). NOTHING rides free except enumerated charge-0 recon.

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
(tripwire clean at review); §7-REQ complete (26 IDs registered + executed
with exact-success records; B19 RED observed); drift GREEN + all six
directional REDs observed with pin + immutable byte-hashes + clean samples
bound; .hi inventory hash matches frozen (B22a GREEN + B22b diff-fire
observed); source-hash channel demonstrated independent (SRCADD: source
RED + .hi clean); M10a COMPILER-kill (+ in-log exhaustiveness-fire
secondary) + M10b GREEN-ENUM observed; bounded surface closed with
review-only remainder labeled; M4a/M4b criteria met with extras classified
(no unattributed REDs); client additions under client CI (limit stated);
`Trivial` intact; full `just ci` green; tracked-clean both ends; founding
guard held; L-1–L-7 recorded with owners (reviewed as record);
per-identity rows all resolved (mirrored or exclusion-reasoned — zero
unclassified declarations); fresh audit PASS complete; bounded claims only.

## 11. Open questions / dependencies (enumerated, not invented)

- #68 → R30-9 rebind + revalidation only.
- #81 (§1–§3; L-7 gated #76) → R30-10 content; unruled exclusions depend on
  nothing.
- #76 → Reactivegas side; kelgroups exposes interface + closure evidence.
- #75 (R3.1) → test input for persistence (threshold = test input), not a
  shipped default.
- Upstream Lean gaps → enumerated here, never invented; landings rebind.

## 12. Provenance + freeze record + freeze-validation + SINGLE FINAL REQUEST

Sources (read-only; newest governs; no blocking conflicts): v3 mandate+map;
bodies #30 (2026-09-06 correction), #29 (Lean correction), #33, #34; Lean
Vote 7-file extent READ IN FULL (Types/State/Event/Validate/Fold at intake
+ re-verified; Invariants 1228 + Tests 397 at r5) + KelGroups/Event.lean
(52) + Types.lean (166) full + Fold/Invariants/Tests zero-Vote verification
+ declaration cross-checks + r7-construct audit at r7 + State/Validate/
Integration heads (CONTEXT granularity, stated); V-2 + #68 OPEN; #81 body;
R3.1; S28 @ `933e385d` (+ v10.2 shape); commissioning note; NOTE-001/002
(clock + helper rules); NOTE-003 + assessment (r2); NOTE-004 (r3); NOTE-005
(r4); NOTE-006 (r5 — falsehoods owned); NOTE-007 (r6 campaign — grant
terms honored, 2 invocations, outcome recorded); NOTE-008 (this r7 —
blockers verified at source before writing; printf facts corrected to
log-evidenced scope). Inbox checked before r7 filing (NOTE-008 read +
acked; no other unread). Spend: 0 product builds (2 plumbing invocations
closed pf1 campaign, own counter; this r7 consumed ZERO executions —
reads + writes only). Skills: orchestrator-contract, ticket-orchestrator,
resolve-ticket (planning only), context-compiler, worker-protocol,
tmux-orchestrator, verification, invariants, gate-script, haskell, nix,
lean4 (read-only).

Freeze-validation (ticket owner, before any GREEN claim): (i) extent
re-listed live == frozen 7+5 + per-file status holds; (ii) Lean pin +
immutable byte-hashes + clean samples GREEN (both repos); (iii) mapping
rows resolve live both sides (exact lines + exact-success REQs +
uniqueness + count); (iv) 7 drift probes demonstrated (GREEN + 6 REDs);
(v) .hi discovery exactness pre-check (exactly-one fresh `.hi` per frozen
module or INCONCLUSIVE abort) + inventory hash == frozen + receipt
exit=0 bound; (vi) M10b totality-witness `case` over frozen allowed set;
(vii) B19 + classification procedure with named attribution fields;
(viii) B22 scratch pre-check (export + trivial GREEN build before overlay
edit; failure ⇒ BLOCKED, never skip); (ix) M15 instrument (filter-drop
splice + REQ-SWEEP-IDEM witness); (x) identity rows all resolved; (xi)
shell-portability pre-check (campaign shell runs the F9 probe set:
tool-allowlist + printf-sanity + smoke assertions on first fixtures;
NO leading-dash formats anywhere; POSIX BRE constructs only). Any (i–xi)
failure ⇒ re-freeze, never proceed.

§12-R7 SINGLE FINAL SYNTHETIC-CAMPAIGN REQUEST R7-CAMPAIGN-01 (for desk
ruling — NO execution has occurred here; terminal demonstration item):
ALL INPUTS HASHED (r7 script + runner + contract + map + retained appendix
— hashes below; fixtures are generated by the hashed runner, no static
fixture bytes exist; prior invocations retained: inv1 = §0/contract record
+ STATUS lines (raw per-case files overwritten by inv2's idempotent start
— stated plainly); inv2 = complete tree `t30-contract/scratch/pf1/`
preserved untouched — the R7 campaign runs greenfield in
`t30-contract/scratch/pf7/`, so no prep move is needed). EXACT COMMAND
(single): `bash t30-contract/scratch/pf7/run.sh` from
`/tmp/reactivegas/ms2/e-kelgroups-substrate/` (equivalently absolute path
as invoked). PREDICTED OUTCOMES (per case; branch rule after): S0
self-check PASS; A1 GREEN FINAL PASS (traversed=4 frozen=4); A2 count RED;
A3 vacuity RED; A4 row_count-0 RED (parsed-construct proof); A5 uniqueness
RED with count PASSING (independence proof); A6 count RED (5≠4); A7/A8/A9/
A10 emission/selection/freshness REDs; A11 GREEN (overwrite proof); A12/
A13 receipt REFUSE exit 3; A14 mode REFUSE exit 3; A15 RED naming B+C
(A passes); A16 leg4-success RED; A17 clean-sample RED; A18 pin+hash REDs;
A19 pin RED; A20 hash RED + bound diff; A21 GREEN; A22 source-hash RED +
.hi pinned PASS (channel independence); A23a–d exit≠0 + global no-PASS
(A23e); A24 exact-line RED; A25 single REFUSE naming ALL missing vars.
BRANCH RULES: SUITE PASS (every case AS-PREDICTED + absence asserts hold)
⇒ plumbing demonstrated for all exercised paths (never compiler-output
compatibility, never semantic coverage — stub labels stand); ANY
MISPREDICT ⇒ evidence preserved in place + return for a NEW grant (never
silent, never auto-repaired, never a further ungranted run). COST: 0
builds (shell+git+coreutils+grep/sed/diff/tar plumbing, synthetic
fixtures, owned scratch), ≈120s wall, no network, no credentials, no
project execution, no product-source contact. PREMISE: byte-identical
inputs as hashed below. Actual-compiler prerequisites P1 (.hi dump shape
+ expected-line capture), P2 (scratch-rebuild viability, B22a pre-check),
P3 (totality-witness `case` shape), P5 (shell portability at freeze) all
remain freeze-owned inside the authorized campaign — so NO compiler-
measurement request is filed now (stated, not smuggled).

Hashes: base `933e385df2f2a251bb54a08bb7663f0d41fafb64`; Lean
`3590c0015b84fd58004bf6fb44dd18b107304c48`; brief
`f6d857639a4f3d9aa466c081305f43ae9e59bad4fd9dcb790a08bd85fc856416`.
