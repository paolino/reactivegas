# S28-R2 SUBMISSION — commit-owner-s28r2 (F3 repair, ONE submission)

Worker: commit-owner-s28r2. Seat: pane %545 (continuing process, S28-R2 campaign).
Family: Muse. draft=NONE. Authority: ticket owner t28-app-api per NOTE-033.
Date: 2026-09-06. Parked idle after this file; no second GREEN unprompted.

## SHAs (base / start / candidate / gate / mandate)

- Base (accepted origin/main): `368b596fef0b6d393c2ac7afc631d236c55d86d1`
- S28-R2 start (zero acceptance): `3af3d065b7d0c54f03d89b8c05d8b8acd4a53db4`
- Candidate (FINAL resubmission, signed, local only):
  `ab25cd11b554bcd5ba64ca56a050c2eb21432d3c`
  (parent `e4022c2…`, spec-only rename+comment +8/−5), tree
  `e52114c1f7a676073303ff76caa8f22821e0b2a3`
  (preliminary `55e95fc…` + `bdc9895…` + `2af23d2…` + `e1f34a2…` +
  `e4022c2…` preserved as history).
- Gate: `G28-1 v10 (S28R2-plan)` (M8 fail-closed TBB; v10.1 binds concrete
  M8 pre-GREEN at BINDING-GREEN).
- Mandate: S28-R2-COMMAND-PLAN (frozen 2026-09-06, NOTE-033).
- No push/PR/merge/comments/remote writes. Branch `fix/28-r2-refusal-order`,
  tracked-clean at commit.

## RED-equivalence (inherited, no fresh RED runs)

F3 P2 exit-1 (`7e9bdb49…`) at `3af3d06` — defect evidence only (changed
refusal, tuple (0,0,0,0,0)), not replay, not RED of the new checkers.
F1/F2 original REDs are history; re-proof happens at new bytes via
M6/M7/P2'' at GREEN/audit, never inherited as verdicts.

## F3 repair — mechanism (Store.hs only)

One serialized transition, same lock hold, pre-lock scope NONE: fresh
`readState` → authoritative decision (`applyIntegratedEvent`) FIRST →
on `Left`, return with the codec never forced → on `Right`, force the
payload encode (`evaluate`, post-acceptance, pre-INSERT) → fresh
`kelLength` → SQL INSERT → atomic TVar commit. The v1 outside-lock-decision
shape was never shipped; the rejected alternative is refusal-ordering only,
lock unchanged. Preserved: exact nonmember `Left` (never throw), faulting
member throws observably post-acceptance with zero state/counts/rows, real
SQL failure with lock release, concurrent agreement + reopening (existing
suites untouched and green-path unchanged for accepted ordinary codecs).
No capability claim from finite schedules; no rendezvous-only workaround
(P2'' instrument is harness-side, decoupled per mandate §3.4).

## Permanent checks — mechanism (S28AppApiSpec.hs, row 2 only)

Test-only `FaultingCodec` (`toJSON` = seeded error, never needed on
refusal) + `faultingIntegration` (accepting app fold, production
`demoBaseHook`/digest/mutation/reserved) + pinned-try helper
`tryFaultingAppend`. Three same-line descriptive its inside
`S28-1 rejecting step before append`: member throws observably with zero
tuple; nonmember keeps the exact `Left (NotAMember "outsider")` refusal
(wrapper AND pure-boundary cross-check) with zero tuple; combined
member-throw + nonmember-refusal on one store asserting the full zero
tuple including decode-count and replay==live; plus the NOTE-001/002
conservation it (external A-loops/B-bursts orchestration, length-delta
co-occurrence receipt — overlap inference withdrawn per settlement §b,
exact conservation incl. multisets, full-state fields,
replay == live). F1-regression it and all
other rows untouched. M8 kill mapping: the F3-reorder splice (encode
before decision) makes the refusal its throw instead of `Left` (hspec
records the error under Failures: with the row-2 refused-control name),
with the tuple assertions unreached (never observed-unequal): the kill
quotes via exception-replaces-required-Left.

## ANCHOR-ATTEST (committed `ab25cd1` bytes; Store.hs == `55e95fc` bytes)

- M6 region: success literal
  `writeTVar (stateVar store) (irState result)` ×1, same column as start
  bytes (only the let/force block moved; the write lines did not shift);
  `gs` in scope at the line; closeKEL-export ×1; writeTVar-import ×1;
  `^appendIntegratedEvent` present; freshness clean → v10-M6 expected
  applicable as-is; rebind ruling stays the owner's at BINDING-GREEN.
- M8-greppable validate/encode region (new shape noted): decision line
  `case applyIntegratedEvent integration gs signer event of` with `Left`
  short-circuit first, then `Right` branch holding `let payloadJson…` +
  `_ <- evaluate payloadText` before `n <- kelLength store`. The concrete
  F3-reorder splice (owner-authored, v10.1) targets this region.
- M1-M5 anchors: `S28DemoApp.hs` (A6/boundary) + `Fold.hs` (H1/H2/H3) +
  `Event.hs` (H4') + `Types.hs`, all byte-identical (verified via
  `git diff --quiet` each). M7 target
  `Map.adjust (\m -> m{memberRoles = roles}) key (members gs)` ×1,
  `^enactMutation ` present, freshness clean.
- Fence: `git diff --stat` names exactly `Store.hs` + `S28AppApiSpec.hs`;
  `gate.sh` untouched. No E-class helpers needed (SomeException/try,
  ToJSON, MVar/evaluate all in `base`; no cabal change).
- Registration self-count (gate extractor, committed bytes):
  3/8/3/9/3/5 = 31, 0 EXTRACT-FAIL, same-line 31 == file 31 (no orphans).
  New: `concurrent appends conserve every committed transition` (row 2;
  `overlapping` fully withdrawn, 0 hits).
- Hygiene: fourmolu `--mode check` exit 0 both files (one `-i`
  normalization of the over-long helper sig, reviewed); hlint `No hints`
  on `Store.hs`.

## Revision (NOTE-001/NOTE-034, pre-FINAL; 55e95fc = preliminary history)

- [SUPERSEDED by NOTE-002/NOTE-003 settlement §b below — the
  overlap-inference clause is withdrawn; measurements retained as
  co-occurrence data only. Original paragraph preserved for the record:]
  Added row-2 overlap conservation test (see ANCHOR-ATTEST registration).
  Design: external orchestration only (forkIO/MVars/timeout/poll over
  public API); overlap proven by length-delta (A commit inside B's span,
  else SETUP failure); exact conservation incl. per-worker multisets,
  full-state fields, sorted-log multiset, replay == live; generous
  labeled setup bounds (300s joins, 30s activation poll); assurance-scope
  label in-test; order boundary documented (no direct seq_no read —
  test deps lack sqlite-simple). One self-caught authoring defect:
  first insertion dropped the member-throws `it` header (anchor shared
  with row-6 tail shape); caught by recount (7, not 8), restored,
  re-verified 0 deletions vs HEAD and 8 row-2 names (recorded, not hidden).
- M8-attribution clause corrected to the NOTE-001 exact sentence above;
  all preservation lines and STATUS :21/:33 assertions stand untouched.

## Settlement (NOTE-003/NOTE-036 — binding verdict, wording, limits, spend)

### (a) Binding verdict (cited exactly; fresh audit re-establishes regardless)

- RED receipt BOUND: `S1-skew-retry2.log` (`c223e443…`, retained) — exit 1,
  Failures: quotes `concurrent appends conserve every committed transition`,
  counter 1100 vs 700, no SETUP line, 0.02s.
- Checker BOUND: e1f34a2 spec bytes (attempt3 tree == e1f34a2 via the
  +3/−10 import-identity + journal order).
- Command EXACT: journal-quoted `nix develop .#ci --quiet -c cabal test
  invariants -O0 --test-show-details=direct --test-option=--match
  --test-option="/concurrent appends conserve every committed transition/"`
  (log-consistent: cabal profile + match hint + 1 example).
- Mutant semantics SPECIFIED (journal-quoted 84a2dae-shape) BUT mutant
  BYTES = RECONSTRUCTION (no retained diff; S1 logs quote spec-errors
  only — the skew compiled cleanly). LABELED so here, never historical
  execution identity.

### (b) Operative wording (SUPERSEDED markings applied)

- STATUS:68 + SUBMISSION Revision paragraph above are marked SUPERSEDED
  for their overlap-inference clauses. Replacement operative text: the
  length-delta (Lafter−Lbefore−200 ≥ 1 with worker-B success bound) is
  retained as observed co-occurrence data — A commits observed while B
  ran; the overlap inference is DROPPED (counterexample stands: the
  delta brackets fork/join, not the vulnerable interval). No scope drift
  (assurance-scope kept: no defect claimed in bdc9895/e1f34a2). No false
  sanitization: the 4-vs-3 targeted deviation (4 spent vs 3 pre-declared
  bound — self-reported breach on compile-defect iterations, global fit
  unaffected with 20 remaining), the RECONSTRUCTION label above, and the
  limit list below are reported honestly; no 'zero hits' account is given
  beyond the verified `overlapping` 0-hits rename receipt.

### (c) Cleanup limits (stated precisely for independent review)

- Verified on e1f34a2 bytes: bracket release runs on every executed exit;
  both workers registered (`workerRef` [tidB,tidA]) and killed after the
  stop-signal; closeKEL last. TESTED: positive (S2, no hangs) +
  semantic-negative (S1-attempt3 prompt exit, no hang).
- LIMIT 1 — setup-failure exits UNEXECUTED: zero SETUP lines in any run;
  the 30s await-timeout + 300s join-timeout paths never fired. No claim
  is made about their behavior beyond the code as reviewed.
- LIMIT 2 — closeKEL-throw double-failure UNEXECUTED: IO-close throw-free
  is unproven. Do NOT read 'non-throwing closes' anywhere in this
  submission — that overclaim is withdrawn (see STATUS cleanup fix).
- LIMIT 3 — kill-backup unneeded-to-date: the graceful primary
  (tryPutMVar stop → self-exit) is what TESTED paths exercised; the
  killThread backup never had to terminate a live worker. 'Non-blocking'
  holds in the queueing sense (tryPutMVar/killThread queue immediately);
  'always propagates' holds except the unexecuted double-failure (Limit 2).
- No executed setup-failure proofs chosen (limits stated instead, per the
  NOTE-003 option); no commit accompanies this paperwork (none needed).

## Settlement-2 (NOTE-004/NOTE-037 — atomicity fix, path matrix, narrowed claims)

### 1. Spawn+register atomicity (FIXED, both sites)

- `mask $ \restore -> forkIO (restore worker) + register` at :334 (A)
  and :369 (B): parent masked across fork+register (no async interleave),
  child restored to unmasked worker (killable). Masking evidence
  (grep-verified on committed bytes): `mask $` x2 at spawn sites,
  `restore (` x2, `uninterruptibleMask` 0 hits — no mask of any kind in
  worker bodies (loops/poll/joins). Residual fork→register window CLOSED
  by construction (not a limit).

### 2. Five-path release walk matrix (steps: tryPutMVar → readIORef →
    mapM_ killThread → closeKEL)

- P1 positive — EXERCISED (S2, exit 0, 0.03s): use returns normally (A
  stopped via putMVar + joined with count; B done + joined). Release:
  tryPutMVar finds the flag full (tryReadMVar is non-consuming) → False,
  no block; readIORef → [tidB,tidA]; kills hit finished threads (safe
  no-ops, receipt immediate on dead targets); closeKEL on a quiescent
  store, once. Observed, not universal.
- P2 semantic-negative — EXERCISED (S1-attempt3, exit 1 with Failures
  quote, prompt 0.02s total): use throws HUnitFailure at the counter
  assertion while A still loops. Release: tryPutMVar fills the empty
  flag (A's next check sees stop, puts doneA, self-exits); kills hit one
  finished + one spinning worker (spinning worker killable: mask-free
  bodies cycling MVar/STM/SQLite ops — receipt prompt HERE); closeKEL;
  the original HUnitFailure propagates (the Failures quote IS the
  evidence no masking occurred — a masked failure would have shown as
  timeout/hang, not a quoted assertion). Observed, not universal.
- P3 setup-timeout — UNEXECUTED (zero SETUP lines; 30s await + 300s join
  paths never fired). ARGUED: bracket runs release on throw (language
  guarantee); identical step order as P1/P2; stop needs no registration
  (flag MVar addressed directly); kills cover any registered prefix
  (acquire always registers tidA before use — [tidA] minimum); close
  last. LIMIT: unexecuted; receipt-boundedness sharpest here (a truly
  wedged worker stalls release at killThread receipt — see narrowed (i)).
- P4 worker-exception/Left — UNEXECUTED (every S1/S2 append Right).
  Walked by reading: worker puts done(Left msg) and self-terminates
  (single put, loop exits — no spin); main joins normally, throws
  expectationFailure("worker … refused"), release runs (stop-fill
  harmless, kills on dead threads safe, close), message propagates.
  LIMIT: unexecuted end-to-end; closest TESTED analog is A's flag-stop
  self-exit + main join in S2 (put-then-exit + join identical, payload
  Right-vs-Left the only delta).
- P5 async-cancellation — UNEXECUTED (no canceller exists in-test).
  ARGUED: main runs unmasked (hspec installs none; our two `mask` sites
  restore prior state on block exit, so polls/joins/asserts are
  unmasked → cancellation deliverable); unwind runs release unmasked →
  kills deliverable; forked⇒registered holds under parent-side mask.
  LIMIT: never fired; receipt-boundedness + double-failure apply.

### 3. Narrowed claims (universals retracted)

- (i) Was 'queues immediately'/'takes nothing blocking' → now: for THESE
  workers/MVars, the stop-signal queues without blocking (tryPutMVar
  never blocks, by type) and kill-receipt was prompt in the two observed
  shutdowns. Caveat stated and bounded: killThread waits for delivery
  (throwTo receipt semantics) — receipt is prompt HERE because worker
  bodies are grep-verified mask-free (see §1 evidence), NOT universally;
  receipt is not death (unwind/finalize continues past receipt while
  release proceeds to close — residual, see close-order limit).
- (ii) Was 'propagates' → now: propagation OBSERVED in P1 (normal
  return) and P2 (HUnitFailure survived with Failures quote); bounded by
  killThread receipt (P3 sharpest) PLUS the double-failure exception
  (a closeKEL throw during unwind would replace the original — Limit 2
  of Settlement §c, still unexecuted).
- (iii) Was 'rests on sqlite-simple bracketed statements' → now a LIMIT:
  ascribed, not evidenced. Which library statements finalize what during
  a killed worker's unwind was NOT pinned to a read source/version
  in-campaign. The two TESTED shutdowns showed no close error —
  observations, not proof.
- (iv) All shutdown verdicts are observations (two prompt shutdowns),
  never universals. No universal killThread/closeKEL properties claimed.

### Verification + spend (this settlement)

- Narrow GHC check P-NARROW (1 probe): exact command journaled in
  STATUS; exit 0 with zero diagnostics on working-tree bytes (= `ab25cd1`
  post-commit: no edits between probe and commit). Proves the `_tidA`/
  `_tidB` rename clears the blanket `-Wunused-do-bind` leg-3 risk (same
  -Wall -Werror flags) AND typechecks the full spec module plus all
  locally-sourced deps (Store.hs unchanged since S2's build; only spec
  delta re-verified). `P-narrow.log` retained. No STOP needed (feasible).
- :68/:97 operative fixes re-verified before filing (SUPERSEDED markers
  present; sweep finds no unmarked overlap-proof claims — one residual
  hit sits inside the marked history block by design).
- Anchors re-verified on committed bytes: M6 x1/x1/x1, M7 x1,
  decision-first x1, post-decision-force x1; Fold/Event/Types/Demo/gate
  untouched since start. Registration recount 3/8/3/9/3/5 = 31 (names
  unchanged by the masking edit). fourmolu check exit 0, hlint No hints.
- Sensitivity note: S1/S2 executed on e1f34a2 test logic; e4022c2 delta
  is spawn-masking only (+9/−5, reviewed, format/lint/registration
  re-verified) — full re-establishment belongs to GREEN leg-4.
  ab25cd1 delta (rename+comment, +8/−5) likewise review+format verified;
  the narrow GHC check below typechecked these exact bytes.
## Settlement-3 (NOTE-005/NOTE-038 — spelling, narrow check, §2 relay)

- Spelling: outer `tidA`/`tidB` → `_tidA`/`_tidB` (bound-but-unused under
  blanket -Werror; inner registered tids unchanged, semantics unchanged).
  Self-caught process note: my first edit attempt inserted a bogus
  `tidA <- pure ()` line alongside the rename — caught on immediate
  re-read before any execution, corrected to a pure rename, diff
  re-verified (+8/−5 exact). Recorded, not hidden.
- §2(a) site-A correction RECORDED: site A was NEVER unprotected
  (bracket acquire runs masked — my NOTE-004 fix there is
  redundant-but-harmless and KEPT per instruction, no churn-back);
  exactly ONE real gap existed (site B, unmasked `use`), repaired by the
  mask. Earlier 'both sites gapped' language stands corrected here.
- §2(b) P2 restated: assertion-surfaced (Failures quote = failure signal
  survived release) + exited post-joins with workers done — NOT
  live-worker cleanup (that stays UNEXECUTED where evidence says so;
  a done-MVar arrival is not a death acknowledgment: A may have been
  mid-unwind while killThread/closeKEL ran). Matrix P2 walk reads with
  this restraint.
- §2(c) P4 split: P4a returned-Left (worker puts done(Left), self-exits;
  main joins normally, throws with message, release runs — argued +
  limit: unexecuted end-to-end, S2 put-then-exit analog noted) vs P4b
  thrown-worker-exception (worker dies without putMVar → main's takeMVar
  blocks → 300s join-timeout fires SETUP → release → propagates — argued
  + limit: unexecuted, no worker ever threw). Both accounting, never
  coverage, never waiver.
- §2(d) comment replaced with the NOTE-005 exact sentence (test edit,
  bundled; fourmolu-clean). Nothing required removed silently.
- Anchors re-verified on `ab25cd1`: M6 x1/x1/x1, M7 x1, decision-first
  x1, post-decision-force x1; rest untouched. Recount 31, no new tests.
- Spend: body edits free; narrow check 1 probe (P-NARROW exit 0);
  builds 0/14; targeted 4/24; probes 1/4 (trigger 3 untouched).
  Formatters charge-0. Commit `ab25cd1` (test file). Fit holds.
- Revision costs: source edits free; narrow compiles 0/4 (still unspent);
  formatters charge-0 x2 this revision (fourmolu check + hlint scan;
  no -i needed). Spend unchanged: builds 0/14, targeted 0/24.

## Sensitivity receipts (NOTE-002 §2, executed in-campaign)

- Skew: transient 84a2dae-shape patch (snapshot readState+kelLength +
  decision pre-lock; same lock/INSERT/commit/TVar path — M6's
  stale-WRITE class excluded by construction), never committed,
  restored byte-exact (blob-hash equality vs HEAD, journaled).
- S1 `cabal test invariants -O0 --test-show-details=direct
  --test-option=--match
  --test-option="/concurrent appends conserve every committed transition/"`
  on skew bytes → exit 1, `S1-skew-retry2.log`: Failures: quotes the test
  on a CONSERVATION mismatch (counter expected 1100, got 700; all B
  successes bound while updates lost — the lost-update signature), no
  SETUP line, 0.02s, prompt exit (bracket cleanup held). Two prior S1
  attempts hit repair defects the parse-level checks could not see
  (`Integration(..)` ctor import; two inferred-only type imports under
  Werror=unused-imports) — fixed one-line each, never committed;
  disclosed as a pre-declared-bound deviation (see STATUS).
- S2 identical command on restored fixed bytes (`e1f34a2`) → exit 0,
  `S2-fixed.log`, 1 example 0 failures, 0.03s, no hangs.
- Timeout/setup: none observed in any run; classifications kept separate
  (SETUP-labeled bounds exist in-test; none fired).

## Spend ledger (final for this submission; S28-R2 separate ledger)

Builds 0/14 (GREEN 11B + SLIM 3B unspent; no whole-project invocation —
GREEN executes ONLY on ticket-owner instruction). Targeted 4/24 (S1 x3
attempts + S2 x1, each journaled with command + obligation; the S1
pre-declared ≤3 bound was exceeded by one on compile-defect iterations —
disclosed in STATUS, global fit unaffected with 20 remaining).
Diagnostic narrow-compiles 1/4 (P-NARROW exit 0; trigger 3 untouched —
held in reserve for repair iterations if GREEN finds a defect). Formatters charge-0 throughout
(checks/scans plus two -i normalizations, each journaled). Recon charge-0.
Margin unallocated (not permission). Settlement paperwork (NOTE-003): free;
spend stands 0/14 + 4/24; no commit (paperwork revision needs none).

## Residual doubts (honest limits)

1. Full GREEN still pending: S1/S2 compiled the lib + suite and ran the
   matched test (so the submitted bytes typecheck as built), but legs 3,
   4, M1–M8, 6 remain the full falsifiers; any RED there is a repair defect.
   Lesson recorded: parse-level checks missed two import defects that the
   first counted S1 run caught — review-only confidence is now explicitly
   discounted in this campaign's method.
2. New-checker can-fail is demonstrated at GREEN via the bound M8 splice
   (counted leg-5 execution), not by any uncounted run here.
3. The old encode-time rendezvous instrument cannot run against the new
   order (barrier inside the hold would not complete); the decoupled P2''
   instrument is auditor-side per mandate §3.4 — production governs test.
4. `KELStore` shape unchanged in this campaign (lock field carried from
   start bytes); no API change beyond the S28-R1 baseline.

## Await

BINDING-GREEN review: M8 concrete splice authorship + v10.1 re-freeze and
dry-run falsification pre-GREEN (mandate §3.6/§4), M6-rebind ruling, GREEN
11B instruction, then fresh-auditor seating. Inbox empty at submission;
questions: none; beyond-mandate work: none attempted.
