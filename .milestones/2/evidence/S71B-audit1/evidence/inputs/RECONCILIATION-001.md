# Reconciliation — COMPLIANCE-REPORT.md against #66, #72 and PLAN-TO-MILESTONE-2.md

Read in full: `NOTE-001`, issue #66, epic #72, `artifacts/PLAN-TO-MILESTONE-2.md`.
The assessment report was written before NOTE-001 arrived and is at
`handoffs/COMPLIANCE-REPORT.md`. Nothing below re-opens it; this reconciles it to
the public record and the plan.

**#66 and the brief do not differ.** Same measured-gap table, same five scope
items, same acceptance ("assessment first"). No divergence to resolve.

## One thing is time-critical and is not in Track A

The plan makes `reactivegas.trace/v1` the conformance oracle for **D3**, and says
it "should be built early: built late, it gets built to fit whatever the code
already does."

**The frozen corpus currently carries a wrong claim on six of the fourteen
guards** (report §3). `Trace.lean`'s manifest resolves inversion candidates with
`Name.mkSimple`, so the six inversions declared inside `namespace Reactivegas`
are not found, and every refusal of `openPurchase`, `deposit`, `withdraw`,
`transferCassa`, `donate`, `backdonate` emits `"declaration":"UNPROVED"`. Those
six are exactly the `requiredInversions` — the only theorems CI proves tight and
axiom-clean. In the shipped seed corpus:

```
1  "guard":{"declaration":"step_close_inv","id":"closePurchase"}
1  "guard":{"declaration":"UNPROVED","id":"withdraw"}
```

Half the refusal rows in the frozen oracle mislabel a proved guard as unproved.

This bears on two lanes I must not touch:

- **C1 is in flight** and re-emits `LEAN_TRACES_V1`. Re-emitting before the fix
  bakes `UNPROVED` into the simulator's evidence surface.
- **D3** will be built to replay this corpus. Fixing the manifest changes the
  `declaration` bytes for six guards, so the corpus moves. Cheaper now than after
  an implementation is conformant to it.

`Trace.lean:121-124` states the intended contract — "adding a correctly named
accepted inversion shrinks `missing` with no [edit to this file]". The bug is a
failure against that stated contract, not a design disagreement.

**I am not acting on this.** It is a parent decision about lane sequencing, and
C1/#70 and D3/#67 are not mine.

## Track A items, restated against what A1 measured

| plan item | status after A1 | correction |
|---|---|---|
| **A2** — re-take axiom receipts on fresh `.lake`, *if A1 finds contamination* | **discharged; the condition is false** | No contamination. A1 took a total receipt on a genuinely fresh `.lake`: 1213 theorems, 0 `sorryAx`, standard axioms only. What remains is not a re-measurement but a **gate** — CI checks axioms for 6 theorems and never requires a fresh build. |
| **A3** — mutant coverage "over all 163 theorems" | **denominator is wrong** | 163 is the repo's own gate constant, which excludes `private theorem` by design. Discovered extent: **239 declarations = 163 non-private + 76 private**, 224 unique names. Quantifying over 163 repeats the manifest-of-members defect this milestone keeps catching. Measured coverage: **6 of 224 (≈3%)** carry a tracked in-repo mutant. |
| **A4** — `MUTANTS.md`, `LEAN-CLARITY.md` with the void recorded | **stands, with added urgency** | The mutation evidence for #48/#54/#57/#59/#62 is real but lives untracked under `/tmp/reactivegas/ms2/…` and is keyed to `INV-` rows, not theorems. It is one `/tmp` sweep from gone. A4 is partly a rescue, not only a write-up. |

Two report findings have no home in the plan's A1–A4:

- **§3** the instrument disagreement (above) — touches C1 and D3, not just A.
- **§4** three checks that are `by decide`-green under names describing
  properties they do not compute (`i57TrustNoSorry`, `kelGroupsHasNoReactivegasImport`,
  `leanToolchainMatchesPin`). The properties themselves *are* enforced, by
  `scripts/check-lean-toolchain` and `nix/lean-dependency-direction.sh`; these
  three are decoys wearing those guards' names with no residual recorded.

Both are corrections to things that currently read green, which is why they rank
above the new gates in the report's §10 ordering.

## State

Assessment complete. No slice started, no seat launched, nothing pushed, tree
clean at `e6c5924`. Awaiting the parent's ordering decision — and, separately,
a decision on whether §3 is sequenced ahead of C1's re-emission.

---

## Correction — 2026-09-05, after NOTE-003

**The C1 half of the urgency claim above is wrong. It is withdrawn.**

This document asserted: *"C1 is in flight and re-emits `LEAN_TRACES_V1`.
Re-emitting before the fix bakes `UNPROVED` into the simulator's evidence
surface."* That was reasoned from the fact that the trace envelope carries a
`declaration` field, not measured against what the simulator lane actually
consumes.

Desk `%510` had the simulator owner measure it against candidate `af9c1e5`, and
this lane verified the load-bearing line independently rather than accepting the
report:

```
lean/TraceDriverV1.lean:2:import Reactivegas.Step
```

The economic driver imports `Reactivegas.Step`; the vote driver imports
`KelGroups.Vote.Fold` and `KelGroups.Vote.Validate`. **Neither imports
`Reactivegas.Trace`,** so neither passes through the guard manifest S1 repairs.
The lane's corpora hold 41 `applied` and 2 `refused` rows, and both refusals are
`KelGroups.Vote` validation identities carrying `"error":"notResponsabile"` and
`"error":"questionNotFound"` — not `Reactivegas` guard rows. The re-emitted
corpus never had a `declaration` field to mislabel.

**What survives, and is now stronger.** S1 retains priority for #74's final
corpus freeze, and that half no longer rests on this lane's reasoning: #67
verified that the committed `withdraw` refusal does contain `UNPROVED`.

**What this changes:** the rationale, not the work. No repair scope is
cancelled, and the S1 candidate is unaffected. The lesson is against the
argument's shape — a dependency asserted from the *producer's* schema rather
than measured at the *consumer* is a guess wearing a measurement's clothes,
which is the defect this assessment spent its whole length catching in other
people's checks.
