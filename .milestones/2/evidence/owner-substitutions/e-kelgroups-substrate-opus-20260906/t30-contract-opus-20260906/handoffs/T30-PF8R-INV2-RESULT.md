# T30-PF8R-INV2-RESULT — the single authorized repair rerun: executed, BASELINE BROKEN

Author: ticket preparation owner `t30-contract-opus-20260906`, pane `%572`.
Authority: NOTE-002 preflight binding (epic owner `%532`), TAXONOMY-v2 accepted
for this campaign's single rerun. Executed **once**, exactly as bound.

## 1. What ran

Command, verbatim as bound, CWD = this runtime root:

```sh
bash scratch/pf8r/run.sh > scratch/pf8r/RUN.log 2>&1; echo "runner-exit=$?" >> scratch/pf8r/RUN.log
```

Bindings re-verified by this seat immediately before launch and matched:
`scratch/pf8r/run.sh` = `524f355dd7ca0106d862768042901a4f083ba604e5f078ea54858874a8c4f611`;
`scratch/pf8r/T30-DRIFT-LEG-r9.sh` = `69c529ca22e1a798e1ffb1810902243d9f3c2a9223da50d1eef66b2a39833a25`.
`pf8r` held exactly two files and zero case dirs before the run.

Counter, actual: synthetic-preflight **2 of 2 spent**. Historical pf1 2 spent.
**Aggregate 4 of 4 — the campaign budget is exhausted.** Product builds **0**.

Result: `runner-exit=1`.
`ev/SUITE.log` = `===== SUITE: FAIL (baseline=BROKEN setup-failures=0 mispredicts=11) =====`

Evidence: `scratch/pf8r/RUN.log` (sha256
`14e26a364319339644796d3062e62be3fa650a6dca7d1cec0bb30420bf4e038e`) now carries
the harness's own 31 per-case verdict lines — F-7 from invocation 1 is closed.
Complete tree manifest: `evidence/PF8R-INV2-MANIFEST.sha256` (3832 files,
sha256 `bee490948e1a1a1e0d05baa4fb1532c6ae1181ee6fda7539b4dc639727da11f3`).

## 2. Branch rule invoked: BASELINE BROKEN → report the diagnostic and stop

Packet §6, accepted in NOTE-002 §5. A1 was not GREEN, so the mechanism rows are
not read as campaign verdicts. This section is the diagnostic; §4 states exactly
what that does and does not invalidate, because the contamination is narrower
than invocation 1's and the boundary is mechanically determined.

## 3. The diagnostic: FIX-2 was the wrong fix, and my own assertion could not see it

`cases/A1/stdout:39` — `DRIFT-FAIL: 3-fresh: …/Types.hi OLDER than pre-build
marker`. The same check as invocation 1, for a *different* reason, and the
reason is my error, not the predecessor's.

Measured, after the run:

| path | template `fx/tmpl` | case `A1` after `cp -r tmpl/. A1` |
|---|---|---|
| `frozen/BUILD_MARKER` | **2000-01-01 00:00:00** | 2026-09-06 07:30:01.375797056 |
| `hs/…/Types.hi` | 2026-09-06 07:30:01.344795140 | 2026-09-06 07:30:01.373796932 |

Direct evaluation of the leg's own predicate: on the **template** `hi -ot
marker` is FALSE (correct); on **case A1** it is TRUE (RED).

FIX-2 set the marker to a fixed old date so the relation would not depend on
copy order. **`cp -r` does not preserve mtimes, so it discards the fixed date
too** — the copy stamps *both* files with the copy time and, copying `frozen/`
after `hs/`, re-creates the same 2 ms inversion. The premise "a fixed old
marker makes the relation independent of copy semantics" is simply false, and
it was mine.

The second half of the failure is the more instructive one. FIX-2 shipped with
a template invariant — *marker must be older than the .hi* — and that assertion
**passed truthfully while proving nothing**, because I placed it on the
artifact whose mtimes the copy throws away. It is scoped to less than the thing
it guards: the leg reads the *case* tree, the assertion read the *template*.
`setup-failures=0` is therefore correct again, and again uninformative — the
identical shape I criticised in the invocation-1 assessment §2, reproduced one
layer down by the person who wrote that criticism.

**The fix a successor would make** (recorded as a fact, not as a request):
re-stamp the marker inside `case_env`, which runs per case *after* the copy —

```sh
[ -f "$d/frozen/BUILD_MARKER" ] && touch -d "2000-01-01T00:00:00" "$d/frozen/BUILD_MARKER"
```

The `-f` guard keeps A23a (marker deliberately removed) intact; A10 (.hi at
1999) and A27 (.hi at 1999 after `case_env`) both still read older-than;
A7 (`: >` truncation, mtime now) still reads fresh. `cp -a` in all 31 setup
branches is the alternative. **And the invariant assertion must move with it,
into `case_env`, so it is asserted on the tree the leg actually reads.**

## 4. What the run does and does not invalidate

All **11** mispredicts are the live-mode cases that depend on D-3 emission or
on D-4's traversal of an emitted dump: A1 (×2), A2, A3, A4, A5, A6, A11 (×2),
A15, A24. A3/A4 are there only because TAXONOMY-v2 correctly promotes their
exit to 1 once the earlier freshness RED has rendered a verdict. That is a
complete attribution of every mismatch to the single FIX-2 defect — no residue.

Rows that never enter the contaminated path are named below with the argument
for their independence. **I am not scoring them as campaign verdicts** — the
branch rule I filed says the baseline decides that, and it is the epic owner's
to relax or uphold.

- **Overlay mode never reaches D-3** (`3-skipped in overlay`, A20/A21/A22
  line 39). A21 produced `FINAL: PASS (traversed=4 frozen=4 leg4pass=3)` and
  exit 0 — the first `FINAL: PASS` this campaign has ever produced.
- **A28** refuses at `0-overlay-base` with `OVERALL_FAIL=0`, before D-1
  (`0-overlay-base: export base [52418cb6…] != frozen [7b087768…] (unfounded
  overlay)`), exit 3. With A20/A21/A22 on the PASS side this is the negative
  control that F-5 said did not exist.
- **A7** has **no** `3-fresh` line: `: > Types.hi` re-stamps the file after the
  copy, so A7 reached emission and ran the whole D-3→D-4 pipeline end to end —
  `3-emit: empty dump`, three `4-type … ABSENT`, `traversed=4 frozen=4
  leg4pass=3`. It is evidence that the pipeline executes when the .hi is fresh.
- **A17/A18/A19** fire in D-1, and **A16**'s `4-nonempty` fires in D-4 before
  the traversal; each named line precedes the contaminated path.
- **A26** REDs on `1-clean-hs` against A1 line 8's `1-clean-hs: kelgroups tree
  clean` — a controlled pair in both directions.

## 5. The three fixes that are demonstrated by this run

Instrument properties, not mechanism verdicts:

- **FIX-1 works, both directions.** `cases/A1/stdout:8` PASS on a clean fixture
  repo; `cases/A26/stdout` FAIL on a deliberately dirtied one. The invocation-1
  defect that REDded 28/28 cases is closed and its falsification retained.
- **FIX-4 works.** **Zero** of the 31 cases has a non-empty `stderr`. The leg no
  longer executes its own documentation.
- **FIX-5 works exactly as specified.** `DRIFT-NOTE: taxonomy-v2 precedence —
  refusal follows a rendered verdict; exiting RED (1), reason above` appears in
  A1, A2, A3, A4, A5, A6, A10, A11, A15, A16, A17, A18, A19, A24, A26, A27 — sixteen of the thirty-one. A10/A16/A17/A18/A19 scored AS-PREDICTED at exit 1
  precisely because of it — under v1 they would have returned 3. The
  inconsistency named in NOTE-002 §2.2 is resolved in the direction predicted.
- **FIX-6 fires and blocks.** `cases/A27/stdout` — `4-provenance: dump for
  KelGroups.Vote.Types was not emitted by this run (inherited artifact
  refused)`, with the forbidden `4-type Verdict exact` **absent**. The inherited
  dump was refused instead of joined. One honest limit: A27 injected staleness
  into an already-stale fixture, so the *refusal* is demonstrated while the
  attribution to A27's own injection is not — the same way A10's control was
  destroyed in invocation 1.

## 6. Mechanism status after invocation 2

Unchanged from the invocation-1 assessment for every row that needed a green
baseline. Still **UNESTABLISHED**: count integrity (A2/A6), exact-line vs
substring (A24), row uniqueness (A5), stale-product RED with an intact control
(A10), no-inheritance by overwrite (A11), per-REQ exact-success discrimination
(A15), baseline GREEN itself (A1), and the entire compiler layer.

Newly **ESTABLISHED as instrument properties** (§5): FIX-1 both directions,
FIX-4, FIX-5 precedence, FIX-6 refusal. Argued-independent and awaiting the
epic owner's ruling under §4: overlay GREEN path (A21), `0-overlay-base`
falsification (A28), D-3→D-4 pipeline execution on a fresh .hi (A7).

## 7. Residual scope, restated

A green pf8r would still mean only exercised shell/git plumbing on synthetic
fixtures under a stubbed `ghc`. It is not compiler compatibility, not compiler
discovery, not semantic coverage, not mapping completeness, not product
readiness. The next compiler prerequisite and its cost are unchanged by this
run: **P1 = B3 (1 owner build)** for real `.hi` selection uniqueness, hash-pin
stability and freshness discipline; **P2 = B22a + B22b (2 owner builds)** for
the tripwire's can-fail and channel independence — **3 product builds**, all
inside the ungranted owner budget, none payable inside this fence.

Owner 26/24 and auditor 25/24 remain PROPOSALS. `#30` implementation and audit
remain UNGRANTED. Contract stays frozen at r8: TAXONOMY-v2 was accepted for
this rerun only, and contract §8 and the command-map taxonomy block are **not**
amended here.

## 8. Stop

The campaign budget is exhausted: 4 of 4. No outcome authorizes another
invocation from this seat and none is requested. §3 records the one-line fix so
that any successor campaign the epic owner may choose to authorize is
mechanical rather than exploratory.
