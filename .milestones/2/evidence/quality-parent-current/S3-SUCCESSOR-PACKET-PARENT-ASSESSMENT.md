# S3 successor packet — parent assessment against the full contract

Owner `%503`. Author `%580` (`muse-spark-1.3-contributor`/`xhigh`), START
2026-09-06T07:25:19Z, **COMPLETE 07:40:31Z**. One submission, as commissioned.
Packet manifest `9f6a6bc5be903a79d668c6e0d07b5306579a113af438b56a23dfc2a69c38eeca`.

**This is my assessment, not an acceptance.** S3 is not closed, no Phase 2 is
granted, no execution ceiling is granted.

## 1. What I verified at source

| check | result |
|---|---|
| manifest integrity | **102/102 entries verify** from the packet directory; **no self-entry** |
| execution budget | **no `.lake`, no `.olean` anywhere in the seat root** — consistent with zero project execution. STATUS holds exactly two events, START and COMPLETE |
| row identity preservation | **207** `oldRow` records, **207** carry `successorPrimary` — every old row ID has a successor reference |
| observation discipline | **1 EXECUTED, 113 PREDICTED, 93 STATIC = 207.** Exactly one executed observation across the whole registry, bound to SS-0. Nothing inflated |
| declaration sites | sampled `KelGroups/Invariants.lean:628`, `KelGroups/Vote/Invariants.lean:810`, `:957`, `:967`, `:980` against the **bound source `3590c001`** — **all five resolve to real theorem headers in the correct files** |
| SS-0 span binding | row 1 records `span "197-209 (error at 209:4; header :197)"`. Verified against `3590c001`: `:197` is `theorem step_grant_inv`, `:209` is `exact hx.symm`. The span lesson is applied, not recited |
| semantic vs script separation | row 1 carries `semanticOutcome` and `scriptOutcome` as **separate fields** with the exact diagnostic recorded apart — NOTE-071 correction 1 applied |
| shape ≠ witness | row 1's note states *"Expected counterexample shape (permitted=false + authorized grant) is prediction, not executed concrete witness; never relabelled OBSERVED"* — correction 2 applied verbatim in effect |
| cost scope | `measuredAnchors` carries actual ms (15980 / 19819 / 3125 / 2476) with *"Incremental mutant (19.8s) slower than cold (16.0s) refutes universal cheaper-incremental; does not establish stable comparative cost"* — correction 3 applied |
| unknowns | historical fields carry `UNKNOWN (historical; bounded search recorded, not fabricated)`; reuse status reads `UNUSABLE-AS-COVERAGE`. **Not fabricated to make the validator green** |
| `#eval` discipline | the check elaboration is stated as elaborating a proposition over fully-qualified `Reactivegas.checkSweepIdempotent`; the plan states `#eval` is never relabelled as runtime |

## 2. The rejection controls — the part that actually matters

Eight fixtures, each tripping **exactly its own class** with the checker's own
message: `MISSING-IDENTITY-MAPPING` + `DUPLICATE-IDENTITY-MAPPING`,
`UNRESOLVED-REFERENCE`, `UNSUPPORTED-OBSERVED`, `MISMATCHED-COST`,
`COUNT-DRIFT`, `COST-OMISSION`, `DUPLICATE-MULTI-ATOM-CREDIT`,
`MISSING-COST-KIND`. Each names the specific offending row, operation or count.

These are controls that **can** fail, and they fail for their intended reason
rather than incidentally. The `UNSUPPORTED-OBSERVED` control encodes the SS-0
correction structurally — *"only Row1 EXECUTED-OBSERVED bound to SS0 is
allowed"* — so the prediction/observation boundary is enforced by the instrument,
not merely asserted in prose. The output states in its own words that success
establishes **data consistency, not semantic correctness**.

This is the executable instrument the commission asked for, not another narrative.

## 3. My findings — for the independent review to adjudicate, not blocking verdicts

**A-01 — `FALSE-AT-WITNESS` is an undefined label asserting a witness on 66
rows.** It appears in `row-outcomes.json` and `operations.json` (66 rows carry it,
16 carry `TRUE-PRESERVED`) and is **defined nowhere** — not in
`SUCCESSOR-PACKET.md`, not in the validator. Exactly **one** row has an executed
observation. The validator's `UNSUPPORTED-OBSERVED` check tests only the literal
substring `OBSERVED` in the outcome fields, so a label whose *name* asserts a
witness passes untouched. `observationKind` separately says `PREDICTED` on those
rows, which is the saving grace — but a machine-readable registry exporting 66
`FALSE-AT-WITNESS` rows with no definition is the same F-03 confusion the original
audit found, in a new spelling. **Either define it as an expected-falsification
class or rename it.**

**A-02 — `firstFailureIsolation` leads with the refuted universal.** The field
opens *"Single-mutant full build halts at FIRST failing obligation in elaboration
order"* before its own parenthetical corrects it: *"continuation refutes universal
stop but does not prove every later check executed."* Read whole, the sentence is
**sound and correctly bounded in both directions**, and the packet cross-references
the continuation evidence in five files. Read as a lead clause — which is how a
field summary gets quoted — it states the thing SS-0 disproved. A legibility
defect in a load-bearing field, not a false claim.

**A-03 — two explicit `OPEN` ownership entries against 563.** `ownership.json` is
558 `PRESERVED`, 3 `CORRECTED`, **2 `OPEN`**. NOTE-071 requires unknown ownership
rows to stay explicitly incomplete rather than be silently excluded. Two is low
enough to warrant testing whether unresolved ownership was genuinely that rare, or
whether some was quietly resolved. I have not established either.

## 4. Two errors of my own, caught during verification

Recorded because the method matters more than the outcome.

1. I grepped `"Invariants.lean:[0-9]*"` across the packet and checked the hits
   against `Reactivegas/Invariants.lean`. The pattern **drops the directory
   prefix**, so `KelGroups/Invariants.lean:628` and three
   `KelGroups/Vote/Invariants.lean` sites were checked against the wrong file and
   appeared to point at blank lines and mid-proof tactic lines. I was one step
   from filing a fabricated defect. Re-checked against the correct files at
   `3590c001`: **all resolve to real theorem headers.**
2. I counted distinct `oldRow` values with a trailing `[0-9]*$` pattern that emits
   an **empty match** on non-matching lines, producing "208 distinct from 207
   occurrences" — an arithmetic impossibility that was my grep, not the data.

Both are the same class as the earlier `grep -rc` counting error: **a pattern that
silently changes what is being counted.** Neither reached a report.

## 5. Disposition

The packet **meets the commission's form**: one frozen submission, zero
executions, SS-1 through SS-6 delivered as one coherent packet, 207 identities
mapped, an executable validator with eight controls that fail for their intended
reasons, unknowns preserved as unknown, and the four NOTE-071 interpretation
corrections applied in the data rather than recited in prose.

Whether it meets the commission's **substance** — whether the 26 planned
operations can actually execute, observe what they claim, and account for every
counted invocation — is for the independent static instrument review, which I
commission next. **A-01, A-02 and A-03 go to that review as my findings, and every
inherited row remains challengeable.**

**S3 is not closed. No Phase 2. No execution ceiling. None of the old 18 / 143+1
figures is granted, and no replacement number is inferred from this packet.**
