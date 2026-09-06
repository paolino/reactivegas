# T30-INV1-ASSESSMENT — what pf8 invocation 1 established, refuted, and left open

Author: ticket preparation owner `t30-contract-opus-20260906` (pane `%572`,
`claude --dangerously-skip-permissions --model claude-opus-5[1m] --effort high`).
Subject: the ONLY execution of the r8 campaign, preserved at
`/tmp/reactivegas/ms2/e-kelgroups-substrate/t30-contract/scratch/pf8/`.
Every citation is `<file>:<line>` under that predecessor root, read read-only.

This is an independent reading of the raw streams. The predecessor's diagnosis
(handoff §5a) and the epic owner's five quoted observations were treated as
leads and are each confirmed, corrected, or extended below by name.

## 0. Control: the reported counter is reproducible from the preserved streams

The runner `tee`'d only its last line, so its 28 per-case verdict lines were
NOT preserved (`scratch/pf8/ev/` contains `SUITE.log` and nothing else). They
are recomputable, because `run_case`/`assert_absent` are deterministic over
`exit`, `stdout`, `stderr`, all of which were preserved.

`evidence/replay-verdicts.sh` re-applies both predicates verbatim and returns
`mispredicts=17`, identical to `ev/SUITE.log`'s
`===== SUITE: FAIL (setup-failures=0 mispredicts=17) =====`. Full output:
`evidence/REPLAY-VERDICTS.txt`. Everything below rests on that agreement.

Mispredicting rows (17 increments over 14 cases): A1 ×2, A2, A5, A6, A10,
A11 ×2, A15, A16, A17, A18, A19, A20, A21 ×2, A24.
As-predicted rows: A3, A4, A7, A8, A9, A12, A13, A14, A22, A23a–d, A23e, A25.

**The runner's own output not being captured is itself a preservation defect**
of invocation 1: the binding required complete raw streams and the harness's
own classification was lost. It survived only because it is recomputable.

## 1. The 17 mismatches are four causes, not one

### C-1 fixture dirt: the hs fixture repo was never clean (28/28 cases)

`cases/A1/stdout:8` — `DRIFT-FAIL: 1-clean-hs: uncommitted bytes in kelgroups
tree`. Cause, established mechanically rather than by reading: the fixture hs
repo commits `lib/**` only (`scratch/pf8/run.sh:86`) while `dist-newstyle/…
/Types.hi` is written afterwards (`run.sh:133`) with no `.gitignore` anywhere
in the tree. Porcelain of a COPY of `cases/A1/hs` (the preserved tree was not
touched) returns exactly one entry: `?? dist-newstyle/`.

This line appears in **every one of the 28 cases, live and overlay**, because
`1-clean-hs` reads `KELGROUPS_WORKTREE` in both modes (leg r8:173–177).

Consequence, and it is larger than the row itself: **every exit-1 verdict and
every `FINAL: RED` in invocation 1 is over-determined.** Two of the fourteen
as-predicted rows (A7, A22) are scored partly on `FINAL: RED`, which the dirt
alone produces. Exit codes were never attributable in this run; only the named
per-check lines are.

### C-2 fixture nondeterminism: `cp -r` inverted the freshness relation

`cases/A1/stdout:39` — `DRIFT-FAIL: 3-fresh: …/Types.hi OLDER than pre-build
marker`. Measured mtimes:

| path | template | case A1 |
|---|---|---|
| `frozen/BUILD_MARKER` | 06:47:21.806614803 | 06:47:21.836616655 |
| `hs/…/Types.hi` | 06:47:21.808614927 | 06:47:21.834616532 |

The template order is correct (.hi newer by 2 ms). Every case is built with
`cp -r "$FX/tmpl/." "$CASES/<n>"`, which does not preserve mtimes and copies
`frozen/` after `hs/`, so the relation inverts. Not random — an artefact of
copy order — but equally fatal, and invisible to any check that ran.

### C-3 the cascade: one freshness RED silenced the whole D-3/D-4 layer

`3-fresh` fails with `continue` (leg r8:252–253), so no dump is emitted; D-4
then hits `[ -f "$dump" ] || refuse` (leg r8:297) and exits. That is
`cases/A1/stdout:41`, and the identical three-line shape appears in A2, A5,
A6, A10, A15, A16, A17, A18, A19, A24 — **23 of 28 cases exit 3 for this one
reason.** A7 escaped only because `: > Types.hi` reset the file's mtime to
now (measured: A7 marker 23.363710910 < .hi 23.364710971), i.e. by accident of
its own mutation, not by a sound baseline.

### C-4 one prediction string, one stray executable line

- A20 (`run.sh:278`) predicted `1-hash lean/KelGroups/Vote/Types.lean`; the
  leg emits `1-hash: <path> differs from frozen bytes …` (r8:194). The colon.
  The mechanism fired correctly (`cases/A20/stdout:20`); the expected value
  was wrong. This is the whole of A20's mismatch.
- Every `cases/*/stderr` carries two lines: `T30-DRIFT-LEG-r8.sh: line 53:
  id: No such file or directory` and `line 53: differently.: command not
  found`. r8 line 53 lacks its leading `#`, so a documentation line is
  executed as shell on every run: the backticks run `PASS:` with `<id>` read
  as an input redirection, then `differently.` is not a command. Under
  `set +e` nothing propagates and `OVERALL_FAIL` is untouched — **no verdict
  impact, confirmed**. Its real significance is different: it proves r8 was
  bound and shipped **without ever having been executed**, and the epic
  owner's line-by-line mechanical preflight did not catch it.

## 2. `setup-failures=0` — is the counter a defect?

Not in its own terms. `SETUP_FAILS` counts a non-zero `SETUP_FN`, a missing
`CASE_ENV_OK`, and a malformed case root (`run.sh:182–184`). None of those
occurred: the fixture construction succeeded exactly as written.

The defect is one level up. **Nothing asserted that the fixture it built was
sound**, and nothing made the baseline decide the suite. A1 is the control
every other case depends on; it went RED and the suite reported 27 further
verdicts as though they carried information. A zero on a counter that cannot
observe the failure class in play is not evidence that the class is absent —
`setup-failures=0` is exactly that zero. The repair adds the two template
invariants (hs porcelain empty, marker older than the .hi) and a BASELINE gate
that makes A1's outcome decide the suite verdict.

## 3. Per-mechanism verdicts

`E` established, `R` refuted, `U` unestablished. Line-level evidence only —
never an exit code, for the reason in C-1.

| # | required mechanism | verdict | evidence |
|---|---|---|---|
| M1 | baseline GREEN | **R** | `A1/exit`=3, no `FINAL:` line at all. Cause C-1+C-2+C-3, not leg logic |
| M2 | deleted mapping row → count RED | **U** | `A2/stdout:*` — D-4 refused before the count gate |
| M3 | empty mapping → vacuity REFUSAL | **E** | `A3/exit`=3 with `ZERO data rows` + `vacuous pass REFUSED` |
| M4 | comments-only → 0 data rows (POSIX ERE parsed as ERE) | **E** | `A4/exit`=3, `ZERO data rows`; a literal-paren reading would have counted ≥1 |
| M5 | missing/unreadable inputs → REFUSAL | **E** | A8 `ZERO .hi candidates`; A13 `BUILD_RECEIPT absent`; A23a `BUILD_MARKER absent`; A23b/c `not a regular file`; A23d/A25 single message naming ALL |
| M6 | duplicate producer artifact → REFUSAL | **E** | `A9` `ambiguous selection REFUSED` |
| M7 | receipt exit≠0 → REFUSAL (no producer) | **E** | `A12` `no producer evidence` |
| M8 | unknown MODE → REFUSAL, no live fallthrough | **E** | `A14` `unknown MODE` |
| M9 | exact-line vs substring discrimination | **U** | A24 never reached traversal. A7 shows the exact-line check failing on an absent line, which is weaker |
| M10 | row uniqueness (lose-one + duplicate-one) | **U** | A5 never reached the uniqueness gate |
| M11 | exact-count integrity | **U** | A2/A6 never reached the count gate |
| M12 | stale .hi → RED | **U, and vacuous as run** | `A10/stdout` fires `3-fresh` — but so does the pristine baseline (C-2). The control is destroyed; the observation is not attributable to the injected staleness |
| M13 | empty dump → RED | **E** | `A7/stdout:39` `3-emit: empty dump …` |
| M14 | emission overwrites a pre-seeded dump (no inheritance) | **R** | `A11/stdout:39` freshness RED skipped emission; `cases/A11/ev/hi-KelGroups_Vote_Types.dump` still reads `POISON`, and D-4 consumed it (`A11/stdout:41,43,45`) |
| M15 | per-REQ exact-success discrimination (PASS vs FAILED/SKIPPED/bare) | **U** | A15 never reached the per-REQ loop |
| M16 | leg-4 log with zero successes → RED | **E** | `A16/stdout` `4-nonempty: … ZERO successful execution records`; positive side from A7/A11/A20/A21/A22 `4-exec REQ-A/B/C successfully executed` |
| M17 | dirty lean tree → clean-sample RED | **E, properly controlled** | A1 `1-clean` PASS on a clean tree vs `A17/stdout:7` `1-clean: uncommitted bytes present` |
| M18 | committed change → position RED while frozen-oid content reads stay PASS | **E, properly controlled** | `A18/stdout` `1-position-lean: HEAD [aafc5ba1…] != frozen [ea35bab7…]`, and `differs from frozen bytes` ABSENT (replay: A18's absence assert passed). Reference-vs-content separation demonstrated |
| M19 | full-oid exactness (7-char oid rejected) | **E** | `A19/stdout` `1-position-lean: HEAD [ea35bab747def…] != frozen [ea35bab]` |
| M20 | `0-overlay-base` refuses an unfounded overlay | **U — never falsified** | A20/A21/A22 all take the PASS branch; **no fixture ever exercises the refusal**. A gate with no negative control |
| M21 | overlay lean edit → hash trigger with file attribution | **E, controlled** | `A20/stdout:20` `1-hash: lean/KelGroups/Vote/Types.lean differs …` vs A21 all `1-hash` PASS |
| M22 | overlay hs unexported addition → source-hash channel fires | **E, controlled** | `A22/stdout:23` `1-hash-hs: lib/KelGroups/Vote/Types.hs differs (incl. unexported edits)` vs A21 both `1-hash-hs` PASS |
| M22b | that channel is INDEPENDENT of the `.hi` tripwire | **U** | `A22/stdout` `3-skipped in overlay (no build products …)`. The tripwire was not running, and a channel that is not running cannot be shown to be silent. In live mode no case ever reached a successful emission (M1). **Unestablished in every mode as run** |
| M23 | overlay GREEN path | **U** | A21 is green on every check except the C-1 dirt |
| M24 | no setup failure prints `FINAL: PASS` | **E but vacuous as run** | true of `A23*/stdout` — and no case anywhere in the run printed `FINAL: PASS`, so the guard could not be distinguished from a suite that never passes |
| M25 | fail-closed plumbing / no unpropagated failure | **partially R** | the leg executes its own header (C-4). No verdict impact; the header's portability guarantees are unverified |
| M26 | TAXONOMY-v1 bound and honoured | **R** | twelve cases exit 3 with `DRIFT-FAIL` lines already in the stream (A1, A2, A5, A6, A10, A15, A16, A17, A18, A19, A24, A11's class). `refuse()` (r8:63) exits 3 unconditionally and discards the rendered verdict. Under the taxonomy's own words — *exit 3 REFUSAL — no verdict possible* — a run that has already rendered a subject failure is misclassified. This is the dual of NOTE-009 §5's rule: a setup failure is never a domain kill, **and a domain kill must never be reported as a setup failure** |

## 4. Findings beyond the predecessor's and the epic owner's leads

- **F-1 (extends §5a(iv)).** A20's mismatch is *entirely* the prediction
  string; the mechanism is one of the few that demonstrably fired.
- **F-2 (corrects §5a(v)).** A22's redesign works — but the independence
  claim it was built to prove is not reachable in overlay mode at all,
  because D-3 is skipped there by design. §5a(v) treats A22 as the fix; it
  is the fix for attribution, not for independence.
- **F-3 (new).** `1-clean-hs` over-determines every RED in the suite, so the
  fourteen "as-predicted" rows overstate attribution too (A7, A22).
- **F-4 (new).** The join reads a dump bound to nothing. `[ -f "$dump" ]`
  (r8:297) is the only precondition; `cases/A11/ev/…dump` proves a file this
  run did not emit was consumed. In live mode that cannot currently produce a
  false GREEN — but only because every emission-skip path happens to set
  `OVERALL_FAIL` first. That is an incidental ordering, `enforced: NONE` as an
  invariant, and one reordering away from a silent pass on stale metadata.
- **F-5 (new).** `0-overlay-base` has never been shown able to fail (M20).
- **F-6 (new).** `4-livedir` (r8:317–319) is a restatement of `OVERALL_FAIL`,
  not a check: it cannot fail on its own and prints a NOTE. Its comment says
  "reconciled here, never inherited" while the code inherits. Direction B is
  in fact carried by the byte layer (any added/removed file moves a file-set,
  any changed byte moves a hash), which is a sound argument — but then the
  honest record is "direction B enforced by the byte layer; `4-livedir` is a
  report line, `enforced: NONE`", not a reconciliation. NOTE-007(b) and
  NOTE-008 asked for a live-minus-mapping set difference; r8 computes none.
- **F-7 (new).** The harness's own per-case output was not preserved (§0).

## 5. What a green synthetic suite would and would not mean

Unchanged and restated because the repair makes a green plausible: passing
pf8r means the exercised shell/git plumbing behaved as predicted on synthetic
fixtures under a stubbed `ghc` shim. It is never compiler compatibility,
never compiler discovery, never semantic coverage, never mapping completeness,
never product readiness. `run.sh:23–25` says so and it stays true.
