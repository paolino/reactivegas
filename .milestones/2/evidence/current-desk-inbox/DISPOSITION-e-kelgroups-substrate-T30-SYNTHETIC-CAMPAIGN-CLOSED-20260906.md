# T30 synthetic campaign: executed, closed, and the exact next prerequisite

Epic owner `%532` (Opus), kelgroups `#29` / Reactivegas `#73`. Local file only; nothing typed into `%510`.
This is the consolidated disposition NOTE-003 asked for. No new grant is taken or implied.

## 1. What was executed

The single authorized repair rerun ran **once**, under my preflight binding
(`t30-contract-opus-20260906/inbox/NOTE-002-preflight-binding-invocation-2.md`, sha `f3e888b6…`).

```
bash scratch/pf8r/run.sh > scratch/pf8r/RUN.log 2>&1; echo "runner-exit=$?" >> scratch/pf8r/RUN.log
runner-exit=1
===== SUITE: FAIL (baseline=BROKEN setup-failures=0 mispredicts=11) =====
```

Bound artifacts, re-hashed by me before the binding and matching:
runner `524f355dd7ca0106d862768042901a4f083ba604e5f078ea54858874a8c4f611`,
leg-r9 `69c529ca22e1a798e1ffb1810902243d9f3c2a9223da50d1eef66b2a39833a25`.
Result artifact `T30-PF8R-INV2-RESULT.md` sha `be6904099c264694b1e890513091b237dd86951b44f6e95ad8e13df0744eb8cb`.

**Counters, actual:** synthetic campaign **2 of 2 spent**; historical pf1 **2 spent**;
**aggregate 4 of 4 — exhausted.** Product builds **0**. No refund, no reset, no quiet third.

## 2. Why the baseline broke — measured, not inferred

`cases/A1/stdout:39` — `3-fresh: …Types.hi OLDER than pre-build marker`. I measured the mtimes myself:

| path | template `fx/tmpl` | case `A1` after `cp -r tmpl/. A1` |
|---|---|---|
| `frozen/BUILD_MARKER` | 2000-01-01 00:00:00 | 2026-09-06 07:30:01.375797056 |
| `hs/…/Types.hi` | 2026-09-06 07:30:01.344795140 | 2026-09-06 07:30:01.373796932 |

FIX-2 stamped the marker with a fixed old date so the relation would not depend on copy order.
**`cp -r` does not preserve mtimes, so it discards the fixed date too**, re-creating the 2 ms inversion in
every copied case. The template invariant that was supposed to catch this passed truthfully and proved
nothing, because it was asserted on the template while the leg reads the **case**.

**This is the same defect shape twice, one layer apart** — a check whose subject is not the artifact under
test. The predecessor's `setup-failures=0` could not observe the fixture class in play; the successor's
template invariant could not observe the copied tree. I have entered it in the epic invariant ledger as a
recurring shape, because it will recur again in the `#30` gate if nothing names it.

## 3. Ruling on the argued-independent rows

The ticket owner declined to score them and referred the decision up. **Ruling: they stand as instrument
properties, and the suite verdict stays FAIL with `baseline=BROKEN`.** The two are not in tension: the
baseline gate exists to make rows that *traverse* the contaminated path unattributable, and the boundary is
mechanically determined rather than a matter of judgement — a row whose named line executes strictly before
D-3, or in a mode that never reaches D-3, cannot be contaminated by a D-3 input defect.

Verified by me directly in the raw streams, not accepted from the report:

- **A21** — exit 0, `FINAL: PASS (traversed=4 frozen=4 leg4pass=3)`, zero `DRIFT-FAIL`/`DRIFT-REFUSE`.
  The first `FINAL: PASS` this campaign has produced. Overlay skips D-3 by design.
- **A28** — exit 3, `0-overlay-base: export base [52418cb6…] != frozen [7b087768…] (unfounded overlay)`,
  with `OVERALL_FAIL=0`, before D-1. This is the negative control finding F-5 said did not exist.
- **A7** — exit 1, **zero** `3-fresh` lines; D-3 and D-4 both ran end to end (`3-emit: empty dump`, three
  `4-type … ABSENT`, `traversed=4 frozen=4`). The pipeline executes when the `.hi` is fresh.
- **FIX-4** — **0 of 31** cases has a non-empty `stderr`. The leg no longer executes its own documentation.
- **FIX-5** — the precedence note fires, and A10/A16/A17/A18/A19 scored as-predicted at exit 1 where v1 would
  have returned 3. The taxonomy/prediction inconsistency named in my binding is resolved in the predicted
  direction.
- **FIX-6** — `4-provenance` refused the inherited dump in A27 with the forbidden `4-type Verdict exact`
  absent. The honest limit the ticket owner stated is upheld: the refusal is demonstrated, the attribution
  to A27's own injection is not.

**Not established, and not upgraded by any of the above:** count integrity, exact-line vs substring, row
uniqueness, stale-product RED with an intact control, no-inheritance by overwrite, per-REQ exact-success
discrimination, baseline GREEN itself — and the entire compiler layer. Eleven mispredicts, all attributed
to the single FIX-2 defect with no residue.

## 4. The exact next prerequisite and its cost

**Unchanged by this run, and it is not another synthetic invocation.** Every metadata path in the campaign
runs through a stub `ghc` that serves fixture bytes, so no amount of synthetic green touches the compiler.

- **P1 — B3, one owner build.** `nix develop .#ci --quiet -c just build` + marker + receipt +
  per-module `ghc --show-iface` emission + hash-pin. Establishes real `.hi` selection uniqueness in a real
  `dist-newstyle`, hash-pin stability across rebuilds, and freshness discipline against real timestamps.
- **P2 — B22a + B22b, two owner builds.** The only way the `.hi` tripwire's can-fail and M22b channel
  independence are ever demonstrated, since overlay skips D-3 by design and no synthetic live case can make
  a real interface drift.

**Cost: 3 product builds, all inside the UNGRANTED owner budget, none payable inside this preparation
fence.** The prerequisite is therefore an authorization decision, not a technical one.

## 5. Smallest concrete recovery, with my recommendation

The recovery is now mechanical rather than exploratory, and it is two lines: re-stamp the marker inside
`case_env` (which runs per case, **after** the copy), and **move the invariant assertion there too** so it is
asserted on the tree the leg actually reads — `cp -a` in all 31 branches is the equivalent alternative. Its
effect on every affected case is enumerable in advance (A23a's deliberate removal survives the `-f` guard;
A10 and A27 still read older-than; A7 still reads fresh). Cost: **one synthetic invocation**, which the
exhausted campaign cannot pay — a new grant, which is the desk's, not mine.

**My recommendation is conditional, and I would not spend a call reflexively.** If `#30` commissioning is
not imminent, one further synthetic invocation is worth it: it would resolve all eleven rows and hand the
eventual owner a demonstrated drift gate instead of an argued one. **If `#30` is to be commissioned soon, do
not grant it** — the same demonstration comes free inside the owner budget alongside B3/B22a/B22b, and a
separate synthetic call would be redundant spend. Either way the campaign as run is closed, and the failure
is recorded rather than retried.

## 6. Preservation and fences, controlled

Predecessor `pf8` evidence re-hashed by me against values I recorded **before** the child's work, not
against the child's own manifest: `SUITE.log` `0f2ef27d…`, `A1/stdout` `543b1e8b…`, `pf8/run.sh`
`62025c17…`, leg-r8 `f0afa32b…` — all four unchanged. `/code/kelgroups` at `933e385d`, porcelain empty.
`/code/reactivegas` at `3590c001` with one pre-existing untracked `sessioni`, first observed and recorded by
the ticket owner, not created by this lane.

Contract remains **frozen at r8**. TAXONOMY-v2 was accepted for that single rerun only; contract §8 and the
command-map taxonomy block are **not** amended, and a separate versioned re-freeze is required before r9 is
used for any `#30` acceptance. Owner 26/24 and auditor 25/24 remain **PROPOSALS**; `#30` implementation and
audit remain **UNGRANTED**. `#33`/`#34` stay filed-blocked. No merge, comment, publication, release or
acceptance follows from this work. Settled rows untouched: composition/lifecycle authority; `#68` V-2
settled-but-unlanded as a rebind boundary; no shipped theta; `notProposer`/`notDesignee` unscheduled;
`#76`/`#81` neither implemented ahead of Lean nor permanently excluded.

## 7. Two supervision defects of mine, recorded

Both were mine, neither was a child failure, and both are corrected in my standing practice: an unbounded
`wait-status` that blocked for over five minutes, and a cursor re-taken with `wc -l` after an event burst so
that a terminal `COMPLETE` fell before the cursor and could never match. The rule now is: retain the last
consumed cursor, process all unconsumed records including terminal state first, state the phase before
arming, and never re-derive a cursor merely because a wait expired.

## 8. State

Ticket owner `t30-contract-opus-20260906` (`%572`) is `COMPLETE` and idle — nothing running, no children,
no dispatch. `t28-app-api` stays parked. `#29` and Reactivegas `#73` stay open. Awaiting the desk on exactly
one question: commission `#30` under the proposed ceilings, or grant the single synthetic recovery call, or
neither.
