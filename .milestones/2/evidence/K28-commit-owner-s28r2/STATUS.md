# STATUS — commit-owner-s28r2 (S28-R2 F3 repair implementation)

Worker: commit-owner-s28r2. Seat: pane %545 (continuing process, S28-R2 campaign).
Family: Muse. Branch: fix/28-r2-refusal-order (@ 3af3d06…, clean).

## Spend ledgers (separate S28-R2 campaign; refunds nothing)

- Builds: 0 / 14 substantive cap (GREEN 11B + SLIM 3B planned, both on instruction only).
- Targeted: 0 / 24 (diagnostic narrow-compiles ≤4, trigger 3, journal-each).
- Recon (reads/hashes/greps/diffs) + formatters charge-0.

## Events

- START commit-owner-s28r2 — mandate (S28-R2-COMMAND-PLAN frozen NOTE-033),
  gate v10 (G28-1 v10 S28R2-plan, M8 fail-closed TBB noted — no pre-repair
  gate run, it refuses by design until v10.1), finding (s28r1b AUDIT-REPORT
  F3 + P2.log tail: eager `evaluate` at Store.hs:623 preempts the required
  nonmember `Left (NotAMember)` with AUDIT-SEED-SERIALIZATION; tuple
  (0,0,0,0,0) = changed refusal, not corruption) read. Auditor's
  codecRefusal semantics absorbed (member-throw observable post-acceptance;
  nonmember exact-Left; zero tuple). Own S28-R1 record re-read as context.
  Worktree clean @ 3af3d06 on fix/28-r2-refusal-order. Inbox empty.
- RED-equivalence cited (inherited, no fresh RED runs): F3 P2 exit-1
  (`7e9bdb49…`) at 3af3d06 — defect evidence only, not replay, not RED of
  the new checkers. Proceeding directly to GREEN implementation per mandate.
- Repair shape (authorized reference: decision→force serialized
  post-validation in-lock, no lock change; v1 outside-lock-decision
  REJECTED): fresh-read → decision FIRST in-lock; encode-forcing AFTER
  Right-decision, BEFORE INSERT, SAME lock hold; Left short-circuits with
  the codec never forced. P2'' rendezvous is harness-side (decoupled
  instrument per mandate §3.4); no rendezvous-only prod workaround kept.
- Permanent checks (row-2, same-line, descriptive): faulting-codec member
  throws + nonmember exact-Left + zero-tuple, with a pinned-try helper.
  M8 binds concrete at BINDING-GREEN (validate/encode region kept
  greppable). M6 region repair-adjacent (anchors kept, rebinds at
  BINDING-GREEN). M1-M5/M7 anchors preserved (Fold/Event/Demo untouched).

## Implementation record (all charge-0; probes held at 0/4)

- Store.hs (2 regions, one edit call): Haddock concurrency paragraph
  rewritten (decision-first authoritative order); `let`+`evaluate` moved
  from pre-lock into the `Right` branch post-decision, pre-INSERT, same
  `withMVar` hold. Success-write lines byte-identical, same column.
- S28AppApiSpec.hs (6 regions, one edit call + one anchor-tightened
  retry): 4 imports (Control.Exception, ToJSON, Integration, KELStore,
  demoBaseHook); FaultingCodec + faultingIntegration + tryFaultingAppend
  helpers; 3 row-2 its. Retry reason: row-6 replay tail shared the
  3-line anchor (recorded, not hidden).
- Charge-0 scans: fence exactly 2 files; M6-literal x1 / M7-target x1 /
  no-backdoor / Event+Fold+Types+Demo+gate untouched; registration
  3/7/3/9/3/5 = 30, 0 EXTRACT-FAIL, same 30 == file 30;
  fourmolu `--mode check` exit 0 both files after one `-i` normalization
  of the helper sig (reviewed, canonical); hlint Store.hs `No hints`.
- Full diff self-reviewed (layout columns, import use, Eq/Show/arity,
  shadowing, M8/M6 greppability). Narrow-compile probes deliberately
  unspent (bare-ghc cannot resolve nix-store deps; lib-only cabal would
  be whole-project-class); GREEN is the compiler falsifier.
- Commit `55e95fc` signed, tree `998050ff…`, tracked-clean. No push/PR/
  merge/comments/remote writes. ONE submission (SUBMISSION.md) filed;
  awaiting BINDING-GREEN (M8 v10.1 bind + M6 ruling + GREEN 11B).
  No second GREEN unprompted.

## NOTE-001 revision plan (NOTE-034 binding; 55e95fc = preliminary history)

- [SUPERSEDED by NOTE-002/NOTE-003 settlement below — the overlap-inference
  clause is withdrawn; measurements retained as co-occurrence data only.]
  Overlap test (row-2, NEW): worker A loops DemoAdd-1 until stopped; main
  polls kelLength (public API) for A>=5, records Lbefore, forks worker B
  (exactly 200 DemoAdd-2), joins B (300s timeout), records Lafter, records
  the co-occurrence delta Lafter-Lbefore-200 (A commits observed while B
  ran — the delta brackets fork/join, not the vulnerable interval, so it
  is NOT an overlap proof; counterexample on record), stops A, joins A
  (300s), then exact conservation: every
  result Right, counter == a+2b, length == rows == decoded == a+b, per-worker
  decoded multiset (a x DemoAdd-1, 200 x DemoAdd-2), members/pendingBase/
  pendingProposals exact, sorted-log multiset exact, replay == live exact
  (log order pins id-order == commit order). No prod-order accommodation
  (plain total codecs, no unsafePerformIO, no barrier in ToJSON);
  orchestration purely external (MVars/timeout/poll). Labeled assurance
  scope: no defect claimed in 55e95fc. No sqlite-simple in test deps, so no
  direct seq_no read — order covered via id-ordered replay equality (noted
  as boundary). Mutant scan: passes under M2/M3/M5/M7/M8 (admin IEApp only,
  no hook/refusal/vote/codec-order sensitivity); fails under M6 (extra, fine).
- M8-attribution fix: SUBMISSION resubmission replaces the one wrong clause
  with the NOTE-001 exact sentence; preservation lines + STATUS :21/:33 stand.

## Revision record (shipped; FINAL submission filed)

- Overlap test shipped in spec (4 regions: 3 imports + test block).
  Self-caught defect: first insertion dropped the member-throws `it`
  header (shared 3-line tail anchor); caught by recount (7 not 8),
  restored single-line, re-verified 0 deletions vs HEAD + 8 row-2 names
  + same-line 31 == file 31. fourmolu check exit 0 + hlint No hints on
  final bytes (no -i needed this revision).
- SUBMISSION.md: M8 clause swapped to the NOTE-001 exact sentence;
  ANCHOR-ATTEST revised (registration 3/8/3/9/3/5 = 31, new test named,
  anchors re-verified on final bytes: M6 x1/x1/x1, M7 x1, decision-first
  x1, post-decision-force x1, Event/Fold/Types/Demo/gate untouched since
  start); SHAs updated to FINAL `bdc9895`/tree `d11a2fdf`.
- Commit `bdc9895` signed (spec-only +115/−0), tracked-clean. No push/PR/
  merge/comments/remote writes. No GREEN runs (per NOTE-001 §3).
- Spend FINAL: builds 0/14, targeted 0/24 (narrow compiles 0/4 unspent),
  formatters charge-0 x5 total (3 + 2 this revision). Awaiting BINDING-GREEN
  (M8 v10.1 bind + M6 ruling + GREEN 11B). No second GREEN unprompted.

## NOTE-002 revision record (shipped; sensitivity executed below)

- Spec: test renamed (overlapping: 0 hits); comment relabeled
  co-occurrence receipt + bdc9895 assurance label + bracket cleanup +
  order boundary; bracket acquire/release/use as planned (imports:
  ThreadId/killThread/tryPutMVar, bracket, Data.IORef fully used).
  Exactly one test header swapped; registration 3/8/3/9/3/5 = 31.
  fourmolu check exit 0 (one -i import expansion, reviewed); hlint clean.
- STATUS:68 + SUBMISSION:96 reasoning corrected to receipt language at
  FINAL resubmission (this file's plan paragraph above + SUBMISSION
  Revision section); test :305 renamed in code.
- Commit `2af23d2` signed (spec-only +130/−104: block replacement),
  tracked-clean. S1/S2 execute on these bytes.

## Sensitivity record (S1 RED on skew / S2 GREEN on fixed)

- Skew (transient, Store.hs only, never committed): 84a2dae-shape
  stale-reads pre-lock through the valid path; restored byte-exact
  (blob-hash == HEAD blob) with spec fixes retained in tree.
- S1-attempt1 (`S1-skew.log`, 1 targeted): suite would not compile —
  `Integration(..)` ctor import missing. Parse-level checks cannot see
  this class; review missed it. Defect mine, disclosed.
- S1-attempt2 (`S1-skew-retry.log`, 1 targeted): two inferred-only type
  imports redundant under Werror=unused-imports. Fixed one-line each.
- S1-attempt3 (`S1-skew-retry2.log`, 1 targeted): exit 1, Failures:
  quotes `concurrent appends conserve every committed transition` on
  counter expected 1100 got 700 (successes bound, updates lost — the
  class signature), no SETUP line, 0.02s, prompt exit (cleanup held).
  Sensitivity DEMONSTRATED. Pre-declared ≤3 bound exceeded by one on
  compile-defect iterations (not on sensitivity retries); global 24-fit
  unaffected — recorded here, not hidden.
- Restore verified; fourmolu check exit 0; commit `e1f34a2` signed
  (spec-only +3/−10 import fixes), tracked-clean.
- S2 (`S2-fixed.log`, 1 targeted): identical command on `e1f34a2` →
  exit 0, 1 example 0 failures, 0.03s, no hangs.
- SUBMISSION.md finalized: candidate `e1f34a2`/tree `4f5b26f5`,
  ANCHOR-ATTEST recount 31, sensitivity receipts, corrected spend.
- Spend FINAL: builds 0/14, targeted 4/24 (S1 x3 + S2 x1, commands +
  obligations journaled), probes 0/4, formatters charge-0 throughout.
  Awaiting BINDING-GREEN (M8 v10.1 bind + M6 ruling + GREEN 11B).
  No GREEN runs; no second GREEN unprompted.

## NOTE-003 settlement record (paperwork only; no commit; no GREEN runs)

- Binding block cited exactly in SUBMISSION Settlement §a (RED receipt
  `c223e443…` hash-verified prefix; checker e1f34a2; command exact;
  mutant bytes LABELED RECONSTRUCTION — never execution identity).
- Operative wording: STATUS:68 + SUBMISSION:97 marked SUPERSEDED with
  replacement paragraphs (measurements retained as co-occurrence data,
  inference dropped, scope kept, 4-vs-3 deviation + reconstruction +
  limits reported — no sanitized account).
- Limits stated (not executed): setup-failure paths unexecuted,
  closeKEL-throw double-failure unexecuted (overclaim withdrawn),
  kill-backup unneeded-to-date; queueing/propagation qualified exactly.
- Spend stands 0/14 + 4/24; this settlement paperwork free. Fit holds
  with margin unallocated. BINDING-ready.

## NOTE-005 record (spelling + relay + narrow check)

- Probe P-NARROW (1 from dev ≤4; obligation: replicate blanket
  -Wall -Werror on the spec module pre-GREEN without burning legs):
  EXACT command:
  `nix develop .#ci --quiet -c ghc -fno-code -v0 -Wall -Werror
  -XGHC2021 -XDerivingStrategies -XLambdaCase -XOverloadedStrings
  -XStrictData -ilib -itest -package-db
  dist-newstyle/packagedb/ghc-9.8.4 -hide-package kelgroups -package
  keri-hs test/S28AppApiSpec.hs`
  (construction: nix global db exposes all Hackage deps; local db
  exposes keri-hs inplace; kelgroups hidden so KelGroups.* resolve to
  edited sources; -fno-code emits nothing). Outcome recorded below.

## NOTE-004 settlement record (code fix + matrix + narrowed claims)

- Fix (test file): masked spawn+register at :334/:369 (`mask` parent,
  `restore`d unmasked child); evidence `mask $` x2, `restore (` x2,
  `uninterruptibleMask` 0 hits — residual window closed, not limited.
- Matrix filed in SUBMISSION Settlement-2 §2: P1/P2 EXERCISED with
  per-step walks (S2 exit 0; S1-attempt3 Failures quote as no-masking
  evidence); P3/P4/P5 ARGUED with explicit limits (setup-timeouts
  unexecuted; Left-path unexecuted with S2 put-then-exit analog;
  cancellation unfired). No path silent.
- Claims narrowed per 3(i)-(iv): receipt prompt HERE-only (masking
  evidence + receipt≠death); propagates observed + receipt/double-failure
  bounded; sqlite-LIMIT (ascribed, unpinned); observations as observations.
- :68/:97 verified before filing (markers present; sweep clean).
- Commit `e4022c2` signed (test-only +9/−5), tracked-clean. Spend:
  builds 0/14, targeted 4/24, probes 0/4, formatters charge-0. No GREEN runs.

## NOTE-002 receipt + bounded plan (NOTE-035 binding; history preserved)

- Withdrawn: length-delta as overlap proof (counterexample accepted —
  brackets fork/join, not the vulnerable interval; even an in-span commit
  would not prove a shared window). Relabeled co-occurrence receipt at all
  three locations: test renamed `overlapping…` → `concurrent…` (:305),
  STATUS:68 reasoning, SUBMISSION:96 claim. Assertions/structure/discipline
  stand; no defect claimed in bdc9895.
- Sensitivity (executed, §2): skew mutant reintroducing stale-READS through
  the valid path (84a2dae shape: snapshot readState+kelLength pre-lock,
  decision pre-lock, encode/INSERT/commit in-lock; M6's stale-WRITE class
  excluded by construction) + single-match runs:
  S1 `nix develop .#ci --quiet -c cabal test invariants -O0
  --test-show-details=direct --test-option=--match
  --test-option="/concurrent appends conserve every committed transition/"`
  on skew bytes — expect exit≠0 with Failures: quoting the test name on a
  CONSERVATION mismatch (counter/length/rows/replay), never a SETUP line;
  S2 identical command on restored fixed bytes — expect exit 0.
  Costs: S1+S2 = 2 targeted (0/24 now); at most ONE S1 retry (≤3 total)
  then STOP + EXACT gap report (no silent spend, no ad-hoc gate).
  Restore verified by blob-hash equality + tracked status, journaled.
- Cleanup (§3, test-only): bracket acquire (store/MVars/ref/fork A) →
  release (tryPutMVar stop [idempotent queue], killThread leftovers via
  ref [queues immediately], closeKEL last) → use (A-active poll, fork B
  + record tid, joins, asserts; closeKEL removed from body). Propagation:
  release takes nothing blocking, so the original failure propagates —
  EXCEPT the unexecuted closeKEL-throw double-failure (limit: IO-close
  throw-free unproven; do NOT read 'non-throwing closes' anywhere in this
  file — that overclaim is withdrawn here). Kill-then-close rests on
  sqlite-simple bracketed statements; the residual window is documented
  in the limit list, not hidden.

## NOTE-005 settlement record (spelling + narrow check + §2 relay)
- Spelling: outer tidA/tidB → _tidA/_tidB (inner registered tids kept).
  Self-caught: first attempt inserted a bogus `tidA <- pure ()` line —
  caught on immediate re-read pre-execution, corrected to pure rename,
  diff re-verified (+8/−5 exact). fourmolu check exit 0.
- P-NARROW (1 probe, exact command journaled §NOTE-005 record): exit 0,
  zero diagnostics — blanket -Wunused-do-bind risk cleared and full spec
  module + local sources typecheck. `P-narrow.log` retained. Feasible path
  taken (no STOP needed).
- §2 relay recorded in SUBMISSION Settlement-3: (a) site-A NEVER
  unprotected (bracket acquire masked) — exactly ONE real gap (site B);
  my 'both gapped' language corrected, redundant-idempotent-A kept;
  (b) P2 restated assertion-surfaced + post-join exit, live-worker cleanup
  UNEXECUTED (done-MVar ≠ death-ack); (c) P4 split P4a/P4b argued+limited;
  (d) comment swapped to the NOTE-005 exact sentence (bundled).
- Commit `ab25cd1` signed (test-only +8/−5), tracked-clean. Anchors
  re-verified (M6 x1/x1/x1, M7 x1, decision x1, force x1), recount 31.
- Spend FINAL: builds 0/14, targeted 4/24, probes 1/4 (trigger 3
  untouched), formatters charge-0. No GREEN runs. BINDING-ready.

## NOTE-040 STEP 1 record (narrow pre-check, 1 probe)

- EXACT command: P-narrow mechanism with target swapped to the v10.2
  M8-mutated copy:
  `nix develop .#ci --quiet -c ghc -fno-code -v0 -Wall -Werror
  -XGHC2021 -XDerivingStrategies -XLambdaCase -XOverloadedStrings
  -XStrictData -ilib -itest -package-db
  dist-newstyle/packagedb/ghc-9.8.4 -hide-package kelgroups -package
  keri-hs /tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/handoffs/m8v102-mutated-Store.hs`
- Exit 0, zero diagnostics (`P-narrow-m8v102.log` retained, silent).
  Feasible; no STOP. Probes 1/4 → 2/4. Proceeding to STEP 2.

## NOTE-040 STEP 2 record (STOP — frozen-runner HEAD gate, no spend)

- Ran `handoffs/isolated-m8-runner.sh` EXACTLY as instructed → exit 3
  `ABORT: HEAD mismatch`, pre-evidence-setup (no ISO-LOG created), pre-any
  nix invocation (0 builds, 1B M8 allocation UNSPENT). No other builds.
- Cause (read from the frozen script, recon only): `CAND_EXPECT=` pins
  `3af3d06…` (S28-R2 start bytes), but the worktree mandate + BINDING are
  `ab25cd1` (repaired candidate). Further: at `3af3d06` the runner's own
  v10.2 preconditions cannot hold (no decision-first shape — lets sit at
  col 4 pre-lock — and no row-2 faulting-codec controls for kill_check),
  so checking out start bytes to satisfy the gate would fail closed
  pointlessly. No checkout attempted, no script touched (frozen), no
  workaround improvised — restriction-blocks-work: recorded, work stopped.
- Post-state verified: HEAD still `ab25cd1`, tracked-clean, no stash,
  no diff. Tree untouched by the abort.
- Finding UP: runner needs owner correction (CAND_EXPECT vs candidate
  under test) + re-instruction. Its result does not exist; nothing here
  replaces M8 in the full gate.
- Spend: builds 11/14 (GREEN 11B; SLIM 3B + ISO-M8 1B unspent),
  targeted 4/24, probes 2/4. Awaiting direction.

## GREEN record (v10.1, HEAD ab25cd1, OVERALL_FAIL=1 — M8 splice defect)

- Evidence: `handoffs/evidence/20260906T020313Z-ab25cd1-` (`gate-full.log`
  + `leg3-build` + `leg4-test` + `leg5-M1-build/M2..M8-test` + `leg6-ci`
  + `meta.txt` + `registered/row1-6.txt`). One gate invocation, no other builds.
- Legs 1/2/2b PASS (hygiene; header==normalized `dd1d62d6…`; ancestry;
  all 7 pins). Leg 3 PASS (exit 0). Leg 4 PASS (REGISTERED 31 == file
  31, all executed none pending, exit 0 — candidate fully green incl.
  concurrent + faulting-codec controls). M1–M7 ALL PASS with witness
  quotes (M1 unification; M2 rejecting-step; M3 atomic-hook; M4
  exhaustiveness; M5 agreement+MUTANT-M5; M6 authority replay-equality;
  M7 direct-only double-kill incl. prop falsified after 2 tests).
  Leg 6 PASS (`just ci` exit 0: 135+91 examples, Lean 17 jobs, client).
  Leg 7 PASS. Leg 1-after PASS (tracked-clean; all mutants restored
  hash-verified).
- LEG-FAIL 5-M8 (sole failure): v10.1 splice is defective against the
  mandated decision-first shape. It replaces the post-decision force
  (NOFORCE, matched) and inserts `_ <- evaluate payloadText` PREFORCE
  before the `case` line — where `payloadText` is unbound (the `let`
  block lives post-decision inside the Right branch). Result GHC-88464
  compile error → EMPTY Failures section → INCONCLUSIVE (never a kill),
  exit 1. A faithful F3-reorder mutant must hoist the `let` bindings
  with the force; the frozen splice moves the force without them.
  Candidate defect: NONE (refusal-ordering is exactly the mandated
  decision-first shape; M8 preconditions all held).
- Disposition UP: gate evolution belongs to the ticket owner (corrected
  v10.x splice + re-freeze + re-falsification + re-run per mandate §4).
  No owner repair indicated; no candidate change made post-run.
- Spend: builds 11/14 (GREEN 11B consumed; SLIM 3B unspent).
  Targeted 4/24, probes 1/4. HEAD still `ab25cd1`, tracked-clean.
2026-09-06T02:32:08Z  BLOCKED  HANDBACK NOTE-040: STEP1 narrow exit-0 (ghc -fno-code -v0 -Wall -Werror +deps, target m8v102-mutated-Store.hs, P-narrow-m8v102.log); STEP2 v1-runner exit-3 preflight HEAD-refusal, zero builds, no ISO-LOG; no STEP2-COMPLETE exists (absence recorded, not forged); tree ab25cd1 clean; books 11/26+4/24+2/4

## Books (NOTE-041 — superseded 14 ceiling no longer printed)
- Builds 11/26 spent (GREEN 11B; SLIM 3B + isolated 1B + gate 11B newly
  available = 15: EXACT fit, zero margin). Failed v1 attempt cost ZERO
  builds (refused pre-mutation) — books 11, not 12. Targeted 4/24.
  Diagnostic 2/4 (P-narrow + P-narrow-m8v102 spent).

## NOTE-041 rerun record (isolated M8 v2 — LEG-PASS)
- Runner v2 (CAND ab25cd1, strengthened preflight): PREFLIGHT-OK logged.
- Mutant: lets+force hoisted pre-decision (persisted
  `20260906T023215Z-ab25cd1-m8-mutant.diff`, staged-compare IDENTICAL).
- M8 test exit 1, GHC-errors 0, Failures quotes BOTH row-2 faulting
  refused controls (`nonmember keeps exact refusal` + `zero state, counts
  and rows`) via the seeded codec error — exception-replaces-required-Left
  as attested. KILL-QUOTE M8 PASS. Restored byte-exact (Store.hs).
- ISOLATED-M8-RESULT LEG5_OK=1 OVERALL_FAIL=0. ISO-LOG
  `20260906T023215Z-ab25cd1-isolated-M8.log`, M8-test log
  `20260906T023215Z-ab25cd1-leg5-M8-test.log`, PRELOG
  `20260906T023215Z-ab25cd1-preflight.log`, row2 registered file — all
  under handoffs/evidence/. Post-HEAD ab25cd1, tracked-clean.
- Result does NOT replace M8 in the full gate (per terms).
- Spend: builds 12/26 (GREEN 11B + isolated 1B; SLIM 3B + gate 11B = 14
  available, zero margin past them), targeted 4/24, diagnostic 2/4.

## NOTE-042 STAGE 1 record (full gate v10.2 GREEN pass, 11B)
- Evidence: handoffs/evidence/20260906T023750Z-ab25cd1- (gate-full + leg3
  + leg4 + leg5-M1..M8 + leg6-ci + meta + registered). GATE-EXIT=0,
  OVERALL_FAIL=0. One invocation; fence Store.hs+spec held throughout.
- Legs 1/2/2b PASS (hygiene; header==normalized 12f392b6…; ancestry; 7 pins).
- Leg 3 PASS exit 0. Leg 4 PASS (31 registered == file, all executed none
  pending, exit 0 — candidate green incl. concurrent + faulting controls).
- Leg 5: M1 unification / M2 rejecting-step / M3 atomic-hook / M4
  exhaustiveness / M5 agreement / M6 authority / M7 direct-only (prop
  falsified after 6 tests) / M8 faulting-refused (GHC-errors 0, BOTH
  refused controls quoted) — ALL PASS, tree hash-restored.
- CONDITION verified: full-gate M8 diff digest
  37be8bccc9f50e48275cca01be21d9aec686d404a205092b711acc279a8274c1
  MATCHES retained 37be8bccc9f50e48 — no retain-and-judge path needed.
- Leg 6 PASS (just ci exit 0: 135+91 examples, Lean 17 jobs, client).
  Leg 7 PASS. Leg 1-after PASS (tracked-clean).
- Spend: builds 12+11=23/26. Proceeding to STAGE 2 (SLIM 3B of remaining 3).

## NOTE-042 STAGE 2 record (SLIM 3B, all exit 0)
- `nix develop .#ci --quiet -c just build` → SLIM2-BUILD-EXIT=0 (Up to date).
- `nix develop .#ci --quiet -c cabal test all -O0 --test-show-details=direct`
  → SLIM2-TEST-EXIT=0 (135 examples 0 failures + 91 keri-hs).
- `nix develop .#ci --quiet -c just ci` → SLIM2-CI-EXIT=0 (formatters clean,
  hlint No hints, Lean 17 jobs, client build+test green).
- Logs (echoed + EXIT-trailed): slim2-build.log, slim2-test.log, slim2-ci.log.
- Spend: builds 23+3=26/26 (EXACT fit, zero margin).

## NOTE-042 STAGE 3 record (final freeze)
- HEAD ab25cd11b554bcd5ba64ca56a050c2eb21432d3c, tree
  e52114c1f7a676073303ff76caa8f22821e0b2a3 (matches submission-cited
  tree), tracked-clean. Fence across full range 3af3d06..HEAD: exactly
  lib/KelGroups/Store.hs + test/S28AppApiSpec.hs (+270/−14). No production
  edits beyond fence. No spend beyond ceilings.
2026-09-06T02:44:48Z  COMPLETE  GREEN-SLIM-FREEZE candidate=ab25cd1 tree=e52114c1 gate=G28-1-v10.2 norm=12f392b6 full=a7ca9dc4 M8diff=37be8bccc9f50e48 legs=11B-pass SLIM=3B-pass spend=26of26B-targets probes=2of4dev

## Correction to the terminal event above (self-caught, same round)
- The event's `full=a7ca9dc4` cites the v10 full-hash prefix from mandate
  §4, NOT a measured v10.2 full hash — v10.2 re-freeze changed gate bytes,
  so that prefix must NOT be read as v10.2's full identity. Binding gate
  identity is the normalized self-hash `12f392b6…` (header == executable
  bytes, leg-2 verified). Corrected here, original event stands.
