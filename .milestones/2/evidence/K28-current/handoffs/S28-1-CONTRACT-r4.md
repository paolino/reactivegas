# S28-1 frozen ticket contract r4 — kelgroups #28 (G28-1 v3)

Ticket owner `t28-app-api` (Muse, `%534`), parent epic `paolino/kelgroups#29`
(`%532`), runtime `/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/`.
Brief `df98d099…`. Pinned origin/main = frozen base
`368b596fef0b6d393c2ac7afc631d236c55d86d1` (HEAD == pinned at freeze; `/gate.sh`
ignored via shared `info/exclude`). Worktree `/code/kelgroups-issue-28`,
branch `feat/28-generalize-app-api`. Frozen 2026-09-05 per NOTE-003 + A-001,
before ANY campaign run. This r4 SUPERSEDES r3 for execution; r1/r2/r3
(+ backups `e164a121`, v2) PRESERVED unmodified. V-2 settled/unlanded:
freeze current base, rebind after #68, never anticipate.

## A-001 dispositions (binding)

BUDGET (desk): **16 substantive invocations / 24 targeted executions TOTAL**,
standing spend preserved at **0 builds / 4 probes**. Charge-0 CONFIRMED for
read-only recon, hash checks, `bash -n`, version interrogations — r3 §3 rows
kept, annotated (p1–p4 interrogations are class-0 under the confirmed rule;
recorded 0/4 stands, nothing erased). Whole-project invocations are
BUILD-class even warm/expected-RED; nested full builds recorded distinctly,
enclosing shell never double-counted (r3 tables already comply — annotated).
Fit: RED 3 + GREEN 9 + SLIM-final 3 = **15 worst case** (headroom 1); the old
second-GREEN row is VOID (ONE submission: first candidate gets one full
audit; findings RETURN FOR DESK DISPOSITION, no standing repair allowance).
Slim final ONLY on an envelope IDENTICAL to the mutation-tested submission —
prove by equal `HEAD^{tree}` before/after any squash (pure history rewrite);
ANY code/test/gate/dependency/toolchain/base change → re-establish affected
controls + full audit of the ACTUAL final; never waive to fit, BLOCK instead.
Auditor: fresh FULL Codex `gpt-6-astra/high`, **12 substantive / 24
targeted**; cold prerequisites + full gate + mandatory CI incl. nested calls
reconciled IN WRITING before auditor launch; owner kills never inherited as
evidence; no auto raises.

SPLICES (confirmed defects, fixed §6/gate v3): M5 `=> gs` was Lean syntax —
v3 freezes the Haskell operation with enumerated spellings + selection rule.
M1 `_= id` proved declarations only — v3 binds the negative control to an
`applyIntegratedEvent` boundary use (demo instantiation kept as positive
evidence, labeled, never relabeled). M6 rewired an STM site with IO — v3
uses an STM-disciplined backdoor + rewiring (same monad/site discipline).

BINDING RULE (desk item 4): freeze PROPERTY + MUTATION OPERATION + EXPECTED
FAILURE CLASS (this contract); bind concrete locations to ACTUAL artifacts
under ticket-owner ownership, versioned/hashed BEFORE the execution phase
that needs them. Requirement change → escalate. Correcting a mechanically
impossible splice under the SAME requirement needs NO desk round (granted) —
but the corrected splice re-binds (new binding version + hashes) before its
mutant executes. Staged binding (only coherent order — production does not
exist at RED): (i) r4 freeze: operations + TBB slots (this file); (ii) RED
acceptance: absence proof (pristine-base legs + RED-commit Not-in-scope log)
+ REGISTERED appendix filled from RED-tree source; M1 stays TBB (demo file
compiles only with production present); (iii) GREEN-submission pre-mutation:
BINDING record (`handoffs/MUTANT-BINDING-<cand-short>.md`: candidate file
hashes + per-splice precondition outputs + landing-site context quotes),
verified by ME against the candidate (real syntax, real target, real
attribution) BEFORE leg-5 executes; (iv) GREEN acceptance: EXECUTED+KILLED
filled with quotes. To make landing near-certain, the commit-owner brief
MANDATES the harness spellings below as implementation requirements (mirroring
Lean/current-Store style); leg-5 preconditions verify the mandate at runtime
(fail closed). Mandate ≠ verified fact: the distinction is checked at the
named step, never assumed.

AUTHORIZATION (quoted scope): correct same-scope instruments → r4 + gate v3,
reconcile costs append-only, DISPATCH the fresh Muse commit owner through RED
without another checkpoint; before ANY final-green mutation run, verify real
syntax/splice-target/failure-attribution IN THE ACTUAL CANDIDATE. Fences hold
(§3).

## 1. Reconciliation (per r3 §1, carried; zero-extent + remote-green stand)

## 2. Objective (per r3 §2; ONE coherent slice; candidate-commit discipline
per F4-B: submissions are committed local history; leg 5 touches only
tracked-clean trees at recorded HEADs with hash-verified restore)

## 3. Surface, fences, expenditure (append-only reconciliation)

Owned/forbidden/fences per r2 §3 (unchanged). C5 table rows kept; annotated:
GREEN×2 row VOID (one-submission ruling); leg-2b/version rows charge-0
(confirmed); nested-distinct complied. Standing plan under the 16-cap:

| invocation | charge | running |
|---|---|---|
| spent recon p1–p4 (class-0 under confirmed rule; recorded standing kept) | 0B/4P recorded | B0 P4 |
| RED envelope, legs 1–7 on base (leg 5 refuses at entry pre-spend) | 3B (legs 3,4,6) | B3 P4 |
| GREEN envelope, legs 1–7 on submission 1 | 9B (legs 3,4,5×6,6) | B12 P4 |
| SLIM final via run-receipt, legs 1–4+6–7 (identical-envelope proof required) | 3B | B15 P4 |

Worst case 15/16 builds, 4/24 probes. Auditor envelope 12/24 separate
(pre-launch written reconciliation required). Any deviation needing more →
BLOCK with this table + gap (no waiver).

## 4. Frozen Haskell API (per r3 §4, unchanged) + mandated harness spellings

API: GroupView sole projection; DirectCommand sole admission; BaseMutation
non-admitting exhaustive; BaseChange; IntegratedEvent distinct `IE-` params;
PendingBase + historical PendingProposal marked; GroupState +`pendingBase`,
`appFold` holds AppState; `ReservedKey` + three validators, no bootstrap arm;
IntegratedAppFold/BaseHook/IntegratedError/IntegratedResult/Integration/
commitBaseChange/tryEnactBase/applyIntegratedEvent/foldIntegrated;
openIntegratedKEL/appendIntegratedEvent validate-then-append; Trivial
unchanged; Bootstrap/Server compile-fix-only-if-forced.

MANDATED spellings (commit-owner brief requirements; leg-5 preconditions
verify at runtime, fail closed): (H1) app route guard literally
`if isMemberInView signer view then` (count==1 in Fold.hs); (H2)
foldIntegrated written as explicit `case … of` (or equations) over the step
result with refusal arm literally `Left _ -> gs`, accumulator named `gs`
(count==1 in its block); accepted alternative (H2b) `either (const gs) <K>`
exactly once in its block (selection: exactly one of H2/H2b present, else
FAIL); (H3) commitBaseChange as `commitBaseChange`-headed equations + `::`
signature adjacent; (H4) BaseMutation block containing a `ChangeRoles Text
(Set Role)` arm; (H5) appendIntegratedEvent success block shaped like
historical `atomically $ do` with a `gs` binding and a success write
literally `writeTVar (stateVar store) (irState result)`; (H6) demo file per
§5 incl. `demoIntegration :: Integration DemoState DemoEvent DemoProposal
DemoError` + imports of `applyIntegratedEvent`, `emptyState`,
`IntegratedEvent`, `Integration`, `IntegratedError`, `IntegratedResult`.

Anchor status at freeze: A7 (`, closeKEL` export line, Store.hs:26), A8 (TVar/atomically/readTVar/writeTVar imports + `appFold` field), and A8b (`^    , writeTVar$` import line as STM-insert site) DISCOVERED; all production/test spellings
TBB per the staged binding above.

## 5. Test-only demo + spec format rule (per r3 §5; demo imports extended by
H6 for M1's boundary use)

## 6. Requirements → witnesses + killers (v3 operations)

Standing kill rule per r3 §6 (Failures:-quoted registered examples;
empty-section/crash/parse-error never count). M1/M4 are build-kills
(unification / exhaustiveness errors quoted, parse errors excluded).

| # | witnesses | killer v3 operation + failure class |
|---|---|---|
| 1 | P1/R1 (demo at distinct params; non-member refused); demo instantiation = positive evidence ONLY | M1: append `-- MUTANT-M1` + `_m1_boundarySeparates :: Integration DemoState DemoEvent DemoProposal DemoError -> IntegratedEvent DemoState DemoState -> Text -> Either (IntegratedError DemoError) (IntegratedResult DemoState)` / body applying `applyIntegratedEvent` to `emptyState (DemoState 0 [])`. Preconditions: A6 trio + `applyIntegratedEvent` + `emptyState` in demo file. Kill = nonzero build + unification/mismatch error naming DemoEvent + DemoState + `applyIntegratedEvent` (boundary use). |
| 2 | P2/R2/A2 | M2: precondition H1 count==1; insert `-- MUTANT-M2` line + `if True` preserving tail. Kill = test RED + Failures: naming a registered rejecting-step example. |
| 3 | P3/R3/A3 | M3: awk stub (signature kept) `commitBaseChange _ _ post change = Right (IntegratedResult post (Just change))` + `-- MUTANT-M3` line. Kill = test RED + Failures: naming a registered atomic-hook example. |
| 4 | P4/R4-type-level/A4 | M4: insert `-- MUTANT-M4` line + `\| AdmitMemberVoted Text Text (Set Role)` into BaseMutation block (indent derived). Kill = nonzero build + exhaustiveness error quoted (`non-exhaustive`, `-Werror`, `enactMutation`/`validateBaseMutation`/`AdmitMemberVoted`). |
| 5 | P5/A5 (R5 N/A) | M5: selection over H2/H2b (exactly one present in block, count==1): (a) `Left _ -> gs` → `Left _ -> error "MUTANT-M5"`; (b) `either (const gs)` → `either (const (error "MUTANT-M5"))`. Kill = test RED + Failures: naming a registered agreement example. |
| 6 | P6/R6 (full-log replay equality + historical uninstantiability) | M6 (test): (i) export insert `, unsafeSetAppStateSTM` + `-- MUTANT-M6-EXPORT` (anchor A7); (iib) STM-import insert `, STM` + `-- MUTANT-M6-IMPORT` (anchor A8b); (ii) append STM backdoor `-- MUTANT-M6` + `unsafeSetAppStateSTM :: TVar (GroupState s) -> STM ()` / `unsafeSetAppStateSTM var newApp = readTVar var >>= \gs -> writeTVar var (gs { appFold = newApp })` (TVar+GroupState already imported; STM via (iib)); (iii) rewire H5 success write to `unsafeSetAppStateSTM (stateVar store) (appFold gs)` + `-- MUTANT-M6-REWIRE` (splice count == 4). Kill = test RED + Failures: naming a registered authority example (outside verdict quoted). |

## 7. Gate G28-1 v3 (`GATE_VERSION="G28-1 v3 (r4)"`)

v2 mechanics carried (evidence teeing + hashes, PIPESTATUS, entry-refuse,
hash-verified restore + `exit 3` abort, kill_check with registered names,
exact pins + exit-first + stop-before-spend, cold measurement, leg-4
registered/executed/#PENDING/0-failures) with v3 changes: M1/M5/M6 splices
+ checks per §6; leg-4 RED two-step semantics (pristine-base: inventory 0 +
historical exit-0 = absence; RED-commit Not-in-scope log = owner-verified
absence proof, checked by ME not the gate); FROZEN_BASE `368b596…`.
Frozen mechanism quotes (backup authoritative): stop-guard
`IDENTITY-FAIL: stopping before any build/mutation`; entry
`leg5 entry REFUSED`; restore `restored byte-exact`; abort `ABORT(exit 3)`;
kill `KILL-QUOTE(` / `MUTANT-FAILURE(` / `MUTANT-INCONCLUSIVE(`.

RED execution (first run, base): legs 3,6,7 green + leg-4 inventory 0 with
historical exit-0 + leg-5 entry refusal, gate hash quoted; then RED-commit
bundle (tests referencing the frozen API fail with Not-in-scope naming frozen
names — verified by ME against typo class). GREEN: exit 0, all registered
executed, six kills quoted. No push without epic authorization; draft PR only
after GREEN + fresh FULL audit (12/24, pre-reconciled in writing; owner kills
never inherited).

## 8. S30 surface (per r3 §8; NOT built)

## 9. Risks + rebind (per r3 §9; plus: hspec-rendering drift → parsing-only
correction under same requirement (granted authority) + re-freeze +
re-falsification before its mutant executes)

## 10. Supervision (per r3 §10; commit-owner brief requires ANCHOR-ATTEST in
PROOF-COMPLETE; immediate-child-only; local-files-only upward)

## Appendix R (per r3; REGISTERED fills at RED acceptance from RED-tree
source; EXECUTED/KILLED at GREEN acceptance with quotes)
