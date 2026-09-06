# T30-CONTRACT-r6 — kelgroups #30 substrate vote interface + closure evidence (FOR IMPLEMENTATION AUTHORIZATION)

Ticket preparation owner `t30-contract` (Muse), 2026-09-06. Preparation-only:
read-only inspection + own planning artifacts. No compilation, tests,
mutations, gate runs, dispatch, product edits, commits, push/PR/merge/release,
issue comments, or spend (spend 0 throughout; the NOTE-007 preflight's two
harness invocations were shell+git plumbing on synthetic fixtures — their OWN
counter, historical 0 product builds stays 0).

- Brief sha256 `f6d857639a4f3d9aa466c081305f43ae9e59bad4fd9dcb790a08bd85fc856416`.
- SUPERSEDES r5 (r1–r5 retained unmodified; all COMPLETEs stand as history).
  This r6 is the NOTE-007 packet: repaired drift-leg text
  (`T30-DRIFT-LEG-r6.sh`, byte-stable) + campaign record with an HONEST
  demonstration verdict + per-identity appendix + projection statement +
  prerequisites + corrected totals. Valid earlier corrections stand; no
  broad rewrite, no scope reduction. One principle, unchanged: controls
  reach their target layer with the mechanism named, or the limit is stated
  with its deliverable.
- Operative base kelgroups `main` @ `933e385df2f2a251bb54a08bb7663f0d41fafb64`
  (S28 LANDED, PR#32; CI+Release SUCCESS; #28 CLOSED). Accepted Lean
  Reactivegas @ `3590c0015b84fd58004bf6fb44dd18b107304c48` (zero-diff EMPTY
  vs `4a6cd87`; landings rebind explicitly, never anticipated).
- Companions: `T30-COMMAND-MAP-r6.md` (obligation→command map);
  `T30-IDENTITY-MAP-r5.md` (true per-identity table, retained) +
  `T30-IDENTITY-APPENDIX-r6.md` (40 private names enumerated + verified
  KelGroups lists + reconciled projection statement + corroboration
  register); `T30-DRIFT-LEG-r6.sh` (repaired script text — status:
  REPAIRED-BUT-UNDEMONSTRATED except §0/iv findings, stated below).
  This contract ≠ the ticket owner's final immutable gate.

## 0. r6 records (campaign truth + deltas vs r5 + new falsehood ledger entry)

i. GRANT TERMS HONORED: ONE synthetic-fixture preflight + max ONE repair
rerun (TWO harness invocations, both consumed — NO third attempt available
to this seat); NO Lean/GHC/Cabal/Nix project execution, NO builds, NO
product-source changes; work in owned runtime scratch only
(`t30-contract/scratch/pf1/`); versions + per-case evidence preserved
(`cases/<P>/setup.log|stdout|stderr|exit` + `ev/SUITE.log`).
ii. INVOCATION 1 (full suite): FAILED on runner PATH handling
(`PATH=$stub:/usr/bin:/bin` hid NixOS coreutils — every case SETUP-FAILED
identically; SUITE FAIL exit 1). Established narrowly: per-case isolation
+ evidence capture work under total failure; zero FINAL PASS lines across
all broken setups (verdict aggregation held). Runner-only defect.
iii. INVOCATION 2 (post-repair rerun): FAILED on runner fixture
generation — this environment's bash printf REJECTS `--`
(`printf: --: invalid option`; 2 runner sites), corrupting fixtures
(empty extent files → frozen-value cascade → every case MISPREDICT, SUITE
FAIL exit 1). Blast radius VERIFIED confined to the runner (grep: zero
`printf --` occurrences in `T30-DRIFT-LEG-r6.sh`). Root cause + blast
radius + cascade chain all evidenced in preserved files.
iv. NARROW TRUE FINDINGS (the only demonstrated claims — everything else
about the script is UNDEMONSTRATED): (a) unbound-config refusal works as
designed (P9c: exact message names the variable, exit 1, empty stdout, no
PASS — fail-closed `:?` guards DEMONSTRATED); single-variable-first
reporting observed (config order) — recorded as a freeze-owned improvement
note, NOT changed (changing the artifact now would create an
undemonstrated version — worse); (b) no broken setup in either invocation
ever printed FINAL PASS (P9d held twice + SUITE FAIL exits).
v. CONSEQUENCE: the r6 script's repaired logic (vacuity gates,
exact-count, both pins, 12-path extent, exactly-one selection, no silent
paths, overlay executability) is REPAIRED-BUT-UNDEMONSTRATED. No positive-
path case (baseline GREEN, overlays, discrimination, directions) has
executed even once. NO prose-acceptance is taken: the single terminal
deliverable on this point is the FROZEN re-run request (§12-R), pending a
grant only the parent can give. An explicitly identified unmet
demonstration BEATS another false completion.
vi. DELTAS vs r5 (all else carried): B22 itemized as TWO invocations
(B22a baseline scratch GREEN + B22b overlay — NOTE-007(d); auditor mirrors
A-HIDEMOa/b); totals owner 26/24 + auditor 25/24 with six above-20
justifications; probe tables unchanged (24/24 both); per-identity appendix
+ projection statement added; prerequisites P1–P4 (+P5 portability from
lesson iii); freeze-validation gains (xi) shell-portability pre-check.
vii. F9 (new ledger entry): fixture-generation shell portability assumed,
not verified (`printf --` + `tee`/`mkdir` PATH dependence). Owned as a
runner defect (never touched the script under test); converted to
freeze-item (xi), not re-attempted here.

## 1. Objective (one observable — unchanged)

A nondegenerate application opens an app-scoped assent question, casts
ballots as the franchise, and observes the verdict (positive AND negative,
each with explicit cause) plus its closure record through the **integrated**
boundary — refusals before durable effects, replay equality. Test-only
proving instance (as S28); the user demo is `paolino/kelgroups#33`.
Threshold a parameter at every evaluation; exhibits never defaults.

## 2. Scope — rows (FULL #30 scope; per-identity + bounded surface carried)

Accepted behaviours INTACT: converge to the LANDED S28 interface, never
redesign. `Trivial` degenerate presence-only. No unilateral Lean edits. No
threshold default. No expiry. No votable admission. No second store/fold.
Per-identity ground: `T30-IDENTITY-MAP-r5.md` + `T30-IDENTITY-APPENDIX-r6.md`
(every Lean declaration mirrored or exclusion-reasoned; zero Haskell
requirements for proof-side helpers; corroboration column review-only).

R30-1 openQuestion (collective + permission-with-designee),
responsabile-only admission → Haskell #30. R30-2 cast placement/switch/
idempotence (guarded `setInsert` source-verified; M4a/M4b minimal criteria
+ classification). R30-3 sweep/closure/retention AND non-duplication
(M9 + M15 distinct sites; Lean's sweepDuplicating transcribed as test
property). R30-4 verdictOf (parameter everywhere; R-48 warning
transferred). R30-5 refusals produced now; `notDesignee`/`notProposer`
vocabulary-only, UNSCHEDULED (tripwire only). R30-6 canonical-view
franchise (M13; post-view = sensitivity fixture, limit stated). R30-7/14
negative delivery at boundary (consumption → #76). R30-8 route separation
(bounded three-part surface property). R30-9 LANDED base rule; V-2 rebind
after #68 ONLY at `tryEnactBase` + proposer rules. R30-10 mechanism
surface ONLY (content → #81 L-1–L-6; refund → #76 L-7; all recorded, never
executed). R30-10U/11: unscheduled / evidence-only. R30-12 client
adapt-only (TEST-boundary roundtrip; production-server roundtrip out of
scope). R30-13 Lean-owned where ruled (9 `#print axioms` names persist
Lean-side — referenced mechanism). R30-X non-goals fenced. L-1–L-7
recorded with owners (reviewed as record). #29 remainder: #33 demo
(blocked #30); #34 release/notes (blocked #33+#30; desk authority);
downstream notes (with #73). `#29` + #73 OPEN.

## 3. Compact spec (unchanged)

S1–S6: open; cast/switch/recast exactly-once; positive AND negative
verdicts with causes + retained, never-duplicated records; pre-effect
refusal (state AND log unchanged); accepted-KEL replay identical; client
propose/vote. Requirements R30-1–R30-8 + R30-12 + R30-14 now;
dependencies as §2; unscheduled items preserved; R30-X fences.
Rejection identities exact; vocabulary-only ctors never produced; no
expiry refusal. Production-path examples REQ-ADMIT-PATH, REQ-OPEN-REFUSE,
REQ-CAST-NONRESP, REQ-NONDECIDE-PERM (exercised, never widened/narrowed).
Success: S1–S6 + all 26 §7-REQ through the boundary on the candidate SHA;
`Trivial` intact; full `just ci` green; fresh audit PASS; no shipped
defaults; no silent drops; no duplicates.

## 4. Plan (B22a/b split the only change)

Single slice S30-1 (FULL boundary). No S30-2. RED absence proof (C1/C2 —
absence ONLY); GREEN in envelope (15 mutant runs + M10b enumeration + live
drift machinery + REQ set); B20 omission challenge (sole guard
falsification); B22a baseline scratch GREEN then B22b overlay (GREEN build
with real drift — .hi tripwire firing demonstration; TWO counted
invocations); SLIM S1–S3 itemized; FULL audit (every direction rerun, no
inheritance); draft PR + remote CI; acceptance. Ticket owner freezes the
final gate; §12 freeze-validation is the handoff check (now incl. (xi)).

Constraints: this packet cost 0 product builds (2 plumbing invocations,
own counter); §8 fence; whole-project invocation = BUILD; S28 concurrency
discipline; no parallel heavy builds.

Live boundaries (can-fail controls): step/validate agreement; Store/KEL
append + replay (equality; founding guard); client TEST-boundary roundtrip
(limit stated); drift input-binding + pin + join (immutable views; hashes;
clean sample); compiler metadata (.hi tripwire + exhaustiveness). No
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
mapping + discovery + REQ list + identity table + appendix + drift-leg
script are frozen gate artifacts.

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

## 6. Tasks (slice S30-1 — carried, B22a/b split noted)

T30-1 RED absence (C1/C2, absence ONLY — Vote-absence re-verified at r5).
T30-2/3/4/5/6/7/8/9/10 as r5 (M4a/M4b minimal criteria; M9+M15 distinct;
current base; append+replay+founding; adapt-only client; mechanism surface
+ L-record; drift mapping + live machinery + REQ set + identity table +
appendix + drift-leg script). T30-11 GREEN envelope + omission challenge +
B22a baseline + B22b overlay + SLIM + CI + hygiene. T30-12 audit handback
+ PR (every direction rerun; draft post-GREEN; exact-SHA merge at desk).

## 7. Frozen requirement-to-command/control map (binding — r5 carried,
B22a/b + projection statement added)

Conventions (unchanged): whole-project invocation = 1 BUILD. Per-mutant
cycle = apply + run + revert, hash-verified restore (failure aborts exit
3). Hidden invocations forbidden (leg-unit accounting). Predicates:
COMPILER-kill / TEST-kill / GREEN-ENUM (INCONCLUSIVE abort, never kill).
Charge-0 recon free, never evidence. BAN: regex output never cited as
semantic inventory.

§7-CMDS (exact; re-pinned at freeze): `nix develop .#ci --quiet -c just
build`; `... cabal test all -O0 --test-show-details=direct`; `... just ci`
(INCL kelgroups-own `just lean` — tabulated); probes `... cabal test
invariants --test-option=--match --test-option=/S30-<Group>/<REQ-ID>/`
(Groups frozen — fully determined); `nix --version` + batch versions.

§7-REQ (26 IDs, frozen Groups — carried): S30-Open (4), S30-Cast (6),
S30-Sweep (6, incl. REQ-SWEEP-IDEM), S30-Verdict (2), S30-Franchise (1),
S30-Negative (1), S30-Route (1), S30-Lifecycle (2), S30-Client (1),
S30-Admit (2). Every ID registered + executed else RED. B20 (one ID
removed → RED) ONLY guard falsification; C1/C2 absence-only.

§7-PERFILE (actual 7-file extent, all read in full): Types/Event/Validate
inductives MUST emit; State/Fold structures + equation groups MUST emit;
Invariants expected-empty (proof-only — identities tabled, §A of
appendix); Tests expected-empty (witness-only — identities tabled).
Other-file-empty → RED; empty-global → RED always. Full rows:
identity-map + appendix (rule subject: ctor/arm identities + equation
groups, stated).

§7-DRIFT (immutable views + named oracles; script text r6 — status
REPAIRED-BUT-UNDEMONSTRATED except §0/iv): L1 binding (immutable-view
hashes + HEAD pins both repos + file-set + mapping self-check + labeled
clean sample; residual race stated); L2-as-execution DROPPED (upstream
owns pinned-commit validity); L3 join (frozen mapping ⨝ live emission +
leg-4 log; unmapped/dangling → RED); L4 .hi tripwire (GHC oracle;
post-exit-0 emission per frozen-module row incl. `KelGroups.Event`;
freshness-marker refusal; hash-pin; diff→review; firing demo rides B22a/b
— a failing build promises no `.hi`, stated); L5 arm totality (GHC
-Werror, live; demonstrated via M10a's break in B15's log — the break
only). Function presence = compilation + REQ-execution. Lean patterns =
REVIEW PROMPTS only. Baseline = review + signed record (enforced:REVIEW).
Temporal hole → explicit-rebind rule. REQUIRED drift probes (5):
P-DRIFT-GREEN/ADDBYTE/DELBYTE/ADDFILE/JOINMAP (archive export + ONE edit,
export-diff bound — trigger discrimination on source-shaped bytes, NOT
review correctness). Output-copy controls DELETED. Item attribution =
enforced:NONE automatic + MANDATORY re-review (RED on ANY mismatch until
signed).

§7-HI (specific instrument): emission in B3's leg (post-exit-0; marker
rule); per-row `<module>.hi.dump + sha256`; reconciliation vs frozen
(selector fix: Vote.* + `KelGroups.Event` + mapped wiring modules);
firing demonstration = B22a (baseline GREEN proves export integrity) then
B22b (overlay drift ⇒ diff fires); auditor mirrors A-HIDEMOa/b.

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
build` | A-CI (+ 9 `#print axioms` names persist Lean-side — referenced).
Drift 5 REQUIRED probes | binding GREEN + 5 directional reruns (auditor
PROBE ×6, named). Guard B20 | A-OMIT (+ labeled B19-log read). .hi
tripwire B3 emission + hash-pin; B22a baseline + B22b firing demo |
A-COLD mirror + A-HIDEMOa/b. Cold/final B3 + B21 + tracked-clean +
Trivial-only + founding guard | A-COLD/A-CI. SLIM S1 slim-build + S2
slim-test + S3 slim-ci (itemized).

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

## 8. Candidate-independent initial gate design (r6 script status noted)

Fence (ticket owner versions at freeze): WRITABLE — new `KelGroups.Vote.*`
mirrors; `Integration`-wiring points; `Store.hs` vote path (existing
integrated path; no second writer/tables); proving instance; client Api
adapt-only; `kelgroups.cabal` + `test/Main.hs`; drift mapping + discovery
+ REQ list + identity table + appendix + drift-leg script (frozen gate
artifacts). FENCED — `lean/**` (both repos, read-only inputs); historical
bodies; `Trivial.hs` (presence-only, uncounted); S28 production outside
named points; `client/` beyond adapt-only; UI/wasm/economics; release
metadata.

Legs: 1 hygiene before/after; 2 identity + self-hash + ancestry; 2b pins
exact, fail-closed; 3 build cold/warm; 4 inventory + registered (REQ
cross-check) + execution (all executed, none pending; historical green) +
exit 0; 5 mutants B5–B19 (tracked-clean candidate; apply+run+revert;
hash-verified restore; failure aborts exit 3) under MINIMAL criteria +
CLASSIFICATION PROCEDURE (extras recorded + attributed same-cause/
independent/setup — setup never default; unattributed extras ⇒
INCONCLUSIVE); DRIFT leg (r6 script text — REPAIRED-BUT-UNDEMONSTRATED
except §0/iv; freeze runs §12-R rerun procedure before trusting it);
M10b GREEN-ENUM in leg-4 scope; 6 full `just ci`; 7 `Trivial` presence +
client CI presence. `set +e`; full log + per-leg sha256; meta file. B20
separate counted leg-4 rerun minus one REQ-ID (RED required); C1/C2
absence-only. Isolation: single-site splice, diff-hash bound, named
attribution per RED. Freeze deliverable: OBSERVED mutant signatures as
characterization (not acceptance).

Kill-attribution: COMPILER / TEST (REQ-ID-naming) / GREEN-ENUM. Criterion
met + unclassified extras = INCONCLUSIVE abort, never kill.

Evidence: `run-receipt`-style capture per cited run; self-hash + ancestry;
mutant diffs pre-run; restores post-run; overlay export-diffs bound; .hi
hashes bound; immutable-view hashes bound.

Spend classes: BUILD (whole-project = 1); PROBE (narrow = 1 vs 24 cap);
CHARGE-0 (enumerated recon — free, never evidence); AUDIT-BUILD/AUDIT-PROBE
(same, auditor cap). No parallel heavy builds; every failed setup/attempt
journaled; no automatic raises (exact gap first).

## 9. Operational classifications + proposed ceilings (FROZEN with §7) +
per-repository call tables

Owner fit: B1–B2 RED (2) + GREEN 21 (B3 cold + B4 test + B5–B19 fifteen
runs + M10b named in B4 + B20 omission + B21 CI + B22a baseline + B22b
overlay) + SLIM S1–S3 (3) = **26 builds**. Above-20 justifications (each
ordered): M4b←D4, M13←D5, B20←D3, B22a+B22b←NOTE-006-2c + NOTE-007(d)
(GREEN-build firing demo, TWO invocations), M15←NOTE-006-1 (money-bearing
duplication; Lean's own mutant shape). M10b in B4; drift overlays ride
drift probes. Probes ≤24: 5 REQUIRED named + kill-confirm ≤15 (ambiguous
logs only) + dispute ≤2 (beyond → BLOCKED) + transient ≤2, REQUIRED-first.
**PROPOSED owner 26/24** (supersedes 25/24). PROPOSALS pending fit-proof
(§12 i–xi) + authorization; gap returns exact cost, never trimmed scope.

Auditor (pre-dispatch, exact): A-RED1/A-RED2 2B; A-COLD (+ emission
mirror) + A-TEST (+ M10b re-check) + A-CI 3B; A-K×15 15B (UNCONDITIONAL;
REVIEW ONLY for L-records, tripwire output, B20-log-alongside-A-OMIT —
each named); A-OMIT 1B; A-REBIND conditional 1B (landed: execute;
unlanded: unspent WITH reason); A-RESERVE 1B; A-HIDEMOa baseline + A-HIDEMOb
overlay 2B; drift binding GREEN + 5 directional reruns from probe cap
(PROBE ×6, named). Named sharing: M6/A-K6 and M12/A-K12 are the boundary
reruns (no separate builds). **PROPOSED auditor 25/24** (25B; probes 6
required drift + finding-narrowing ≤14 + reconfirm ≤4 = 24). Bottom-up;
coverage never trimmed. Seat fresh Codex-or-Grok (never Muse/GLM/Claude),
clean detached worktree at candidate SHA, argv-pinned model+effort,
post-cursor START, hash-bound report; recommends, ticket owner decides;
every repair gets a fresh auditor.

KELGROUPS worktree calls (ONLY repo where the gate executes): B1
build-absence; B2 test-absence; B3 `just build` + marker + per-module
`ghc --show-iface` emission + hash-pin steps; B4 full test (+ M10b +
cross-check steps); B5–B19 apply + build-or-test + revert + restore-verify;
B20 leg-4 rerun minus one REQ-ID; B21 `just ci` (INCL kelgroups-own `just
lean` sub-step — tabulated); B22a scratch-export + baseline GREEN build;
B22b overlay edit + build + emission + diff-fire + discard; S1–S3;
A-RED1/2, A-COLD (+ emission), A-TEST, A-CI, A-K×15, A-OMIT, A-REBIND,
A-RESERVE, A-HIDEMOa/b; drift probes (hash/join/diff steps, no
compilation); `--match` probes (exact REQ-ID strings). REACTIVEGAS checkout
calls (read-only — NO lake build: upstream owns pinned-commit validity):
`rev-parse`, per-file `git show HEAD:<path> | sha256sum`, `status
--porcelain`, `ls-files`, `git archive` (each rides its leg's/probe's
counter — leg-unit accounting, stated). NOTHING rides free except
enumerated charge-0 recon.

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
matches frozen (B22a GREEN + B22b diff-fire observed); M10a COMPILER-kill
(+ in-log exhaustiveness-fire secondary) + M10b GREEN-ENUM observed;
bounded surface closed with review-only remainder labeled; M4a/M4b
criteria met with extras classified (no unattributed REDs); client
additions under client CI (limit stated); `Trivial` intact; full `just ci`
green; tracked-clean both ends; founding guard held; L-1–L-7 recorded with
owners (reviewed as record); per-identity rows all resolved (mirrored or
exclusion-reasoned — zero unclassified declarations in the 7-file extent);
fresh audit PASS complete; bounded claims only.

## 11. Open questions / dependencies (enumerated, not invented)

- #68 → R30-9 rebind + revalidation only.
- #81 (§1–§3; L-7 gated #76) → R30-10 content; unruled exclusions depend on
  nothing.
- #76 → Reactivegas side; kelgroups exposes interface + closure evidence.
- #75 (R3.1) → test input for persistence (threshold = test input), not a
  shipped default.
- Upstream Lean gaps → enumerated here, never invented; landings rebind.

## 12. Provenance + freeze record + freeze-validation + FROZEN re-run request

Sources (read-only; newest governs; no blocking conflicts): v3 mandate+map;
bodies #30 (2026-09-06 correction), #29 (Lean correction), #33, #34; Lean
Vote 7-file extent READ IN FULL (Types/State/Event/Validate/Fold at intake
+ re-verified; Invariants 1228 + Tests 397 at r5) + KelGroups/Event.lean
(52) + Types.lean (166) full + Fold/Invariants/Tests zero-Vote verification
+ declaration cross-checks at r6 + State/Validate/Integration heads
(CONTEXT granularity, stated); V-2 + #68 OPEN; #81 body; R3.1; S28 @
`933e385d` (+ v10.2 shape); commissioning note; NOTE-001/002 (clock +
helper rules); NOTE-003 + assessment (r2); NOTE-004 (r3); NOTE-005 (r4);
NOTE-006 (r5 — falsehoods owned); NOTE-007 (this r6 — grant terms honored,
two invocations consumed, outcome §0). Inbox checked before r6 filing
(NOTE-007 read + acked; no other unread). Spend: 0 product builds (2
plumbing invocations, own counter). Skills: orchestrator-contract,
ticket-orchestrator, resolve-ticket (planning only), context-compiler,
worker-protocol, tmux-orchestrator, verification, invariants, gate-script,
haskell, nix, lean4 (read-only).

Freeze-validation (ticket owner, before any GREEN claim): (i) extent
re-listed live == frozen 7+5 + per-file status holds; (ii) Lean pin +
immutable byte-hashes + clean sample GREEN; (iii) mapping rows resolve
live both sides; (iv) 5 drift probes demonstrated; (v) .hi discovery
exactness pre-check (exactly-one fresh `.hi` per frozen module or
INCONCLUSIVE abort) + inventory hash == frozen; (vi) M10b totality-witness
`case` over frozen allowed set; (vii) B19 + classification procedure with
named attribution fields; (viii) B22 scratch pre-check (export + trivial
GREEN build before overlay edit; failure ⇒ BLOCKED, never skip); (ix) M15
instrument (filter-drop splice + REQ-SWEEP-IDEM witness); (x) identity rows
all resolved; (xi) shell-portability pre-check (runner + leg parse under
the campaign shell; NO `printf --`, NO unlisted-tool dependence — F9
lesson). Any (i–xi) failure ⇒ re-freeze, never proceed.

§12-R FROZEN RE-RUN REQUEST R6-RERUN-01 (the single terminal deliverable
on demonstration; EXACT — no discretion bundled): precondition prep (exact,
shell file-ops, not a harness run): `mv scratch/pf1/cases
scratch/pf1/cases-att2` (preserve invocation-2 evidence). Runner fix
(exact 2-site diff, heredoc form — no behavior change beyond POSIX
portability): (1) replace `printf '-- fixture base %s\ndef fxBase%s :
Nat := 1\n' "$f" "$f" > "$d/lean/lean/KelGroups/$f.lean"` with
`cat > "$d/lean/lean/KelGroups/$f.lean" <<EOF
-- fixture base $f
def fxBase$f : Nat := 1
EOF`; (2) replace `printf -- "-- overlay edit\n" >>
"$CASES/P11/ovl-edit/lean/KelGroups/Vote/Types.lean"` with `printf '%s\n'
"-- overlay edit" >> "$CASES/P11/ovl-edit/lean/KelGroups/Vote/Types.lean"`.
Command (exact): `bash scratch/pf1/run.sh` from the runtime root.
Classification: 0 builds (shell+git plumbing, synthetic fixtures, owned
scratch), ≈60s wall, no network, no credentials, no project execution.
Predicted: SUITE PASS all cases AS-PREDICTED. Branch rule: any MISPREDICT
⇒ evidence preserved + return for a NEW grant (never silent, never
auto-repaired, never a third ungranted run). Premise: r6 script
byte-identical (no script change bundled — same artifact premise).
Standing rules honored: stubbed metadata stays labeled plumbing-only;
GREEN would demonstrate plumbing (paths/counts/comparisons/exits), never
compiler-output compatibility or semantic coverage; actual-compiler
prerequisites remain P1 (.hi dump shape + expected-line capture, freeze-
validated), P2 (scratch-rebuild viability, B22a pre-check), P3
(totality-witness `case` shape, freeze) — all validatable inside the
authorized campaign, so NO compiler-measurement request is filed now.

Hashes: base `933e385df2f2a251bb54a08bb7663f0d41fafb64`; Lean
`3590c0015b84fd58004bf6fb44dd18b107304c48`; brief
`f6d857639a4f3d9aa466c081305f43ae9e59bad4fd9dcb790a08bd85fc856416`.
