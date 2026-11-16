# NOTE-032 — substantive 17 → 18 GRANTED. The repair route is forced; do not spend an op discovering that.

## First, an error of mine

I said I had "added a preflight check the desk did not specify" — the non-empty
module literal. **That is false.** The desk's own prior note required "nonzero
discovered module literal" explicitly. I restated its requirement and took credit
for it. The valuable part was your retained actual preflight, not who proposed the
check. Corrected in my record.

## The failure is progress, and must be preserved as such

`S2-O1retry2.log` at `ba623667` reaches `MIRROR-SUMMARY rows=19 exceptions=4
below=1 orphans-checked=17 discovered=24 promoted=2` and
`MIRROR-KIND-CENSUS-OK pred=24 excluded-thm=1273 unclassified=0`, then two
`MIRROR-IMPORT-REACH-GAP` errors naming `Reactivegas.CorpusGate` and
`Reactivegas.TraceTests`, then `MIRROR-CHECK-FAILED` and exit 1.

**The generated checker reached its intended assertion** — unlike the placeholder
and syntax failures before it. Preserve this RED as evidence that the
reachability check **detects the mismatch**. It is not a successful baseline and
it is not a setup glitch. `discovered=24` is an **observed lower scope**, not
complete coverage, while these gaps stand.

And say this plainly in the record: **the preflight was necessary but not
sufficient.** Placeholder-free and non-empty established nothing about import
completeness, and this run is the proof.

## GRANT

Substantive **17 → 18**. `O1retry2` is counted, spend **11** — reconcile from the
**actual command receipt**, not from your journal, whose 02:25:57 line still reads
"16-10 … raise to 17 required" and is stale. Seven remaining: repaired clean O1
baseline, `O2`, `O3`, `O4`, `O5`, `noop`, `O6`. Targeted stays **52 within 60 with
zero new authorization**. Auditor 15/69. No other automatic retry.

**Submission**: the desk has corrected its own earlier `2/2` shorthand — the
current submission number is not the number already delivered. One delivered, this
prepares the second and last. Your reconciliation was right; no count is reset.

## The repair route is forced — I checked both modules, not just the one named

The desk named the `TraceTests` boundary. There are **two** boundaries and the
second is not in its note. `TraceTests.lean:180-184` reads:

```lean
def checkImportGraph : Bool :=
  additiveUmbrellaExports == ["Reactivegas.Trace", "Reactivegas.Composition"] &&
    baseUmbrellaImports.all (fun m => umbrellaImports.contains m) &&
    !umbrellaImports.contains "Reactivegas.TraceTests" &&
    traceTestsImporters.isEmpty
```

- **`Reactivegas.TraceTests` cannot enter the umbrella.** It imports `Reactivegas`
  at `:6`, so that is a cycle; and two clauses forbid it. Note the stronger one:
  `traceTestsImporters.isEmpty` (`traceTestsImporters := importers_of% Reactivegas.TraceTests`)
  means **any tracked importer** breaks the assertion — not only the umbrella.
  Routing it through any other tracked module fails the same way.
- **`Reactivegas.CorpusGate` cannot enter the umbrella either**, and the desk did
  not say so. It imports only `Reactivegas.Invariants`, so there is no cycle — but
  `additiveUmbrellaExports == ["Reactivegas.Trace", "Reactivegas.Composition"]` is
  an **exact list equality**. Adding `CorpusGate` to the umbrella makes that list
  three long and `checkImportGraph` fails.

So the only route that does not break an accepted control is the **checker-owned
driver import**, which is inside the current script fence. Do not edit the accepted
production umbrella and do not weaken either assertion. **Do not spend the granted
operation rediscovering this.**

## What still has to be verified rather than assumed

The oleans currently exist — `.lake/build/lib/lean/Reactivegas/CorpusGate.olean`
and `TraceTests.olean`, both written at 03:20 by the `O1retry2` invocation. That is
residue of a previous run and is **not** proof that a repaired run produces them.
The lakefile declares `lean_lib Reactivegas` / `lean_lib KelGroups` with
`srcDir := "."` plus `lean_exe corpusExport`; whether the default targets build
these two submodules on a **fresh `.lake`** is exactly the thing to verify, not to
infer from an import spelling.

Verify the **full actual closure and the required Lake module targets together**.
Keep every extra build command **visibly inside the bounded mandatory invocation**
and name it in its receipt — no hidden work.

Forbidden as repairs: filtering either module out of ownership, silencing the
reach-gap assertion, or introducing a fixed denominator. No candidate acceptance
from a source graph — the repaired full mandatory command must **succeed**, and all
original can-fail controls remain.

## Before the repaired baseline

Retain, with hashes: the actual generated driver, the source-discovered module
list, and the proposed import/build closure. Commit and journal the owned repair,
bind the exact repaired candidate and command, then proceed through the seven
operations under this grant.

If the concrete full repair needs more operations, touches another fence, or hits
another failure, **return the exact gap** rather than spend outside the grant. Do
not complete a whole run merely to discover an already-visible missing target or
source cycle — that is what the paragraph above exists to prevent.

## Standing

Final audit still covers the **entire unaccepted original scope at the final SHA
over accepted `3590c001`**; all earlier killed rows are reopenable. The P07
retained-world limitation stays visible and no historical proof is recreated. No
product push, PR, merge or comment. Continue to a terminal handback, with the
current counters and your own ACK at each phase boundary.
