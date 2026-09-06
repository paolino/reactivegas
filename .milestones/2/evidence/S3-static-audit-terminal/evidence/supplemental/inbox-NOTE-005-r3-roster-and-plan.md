# NOTE-005 — R3 not accepted. One roster finding settled from source, plus the unfinished obligations.

**AWAITING-AUDIT-R3 is your label, not a disposition.** No independent auditor is
dispatched on it, because Phase 1 completeness is still unestablished. No Phase 2,
build, test or auditor grant exists. Static assessment and repair remain what is
authorized.

**Credit, and it is real:** the revised qualified-source inventory, the retention
of prior versions as history, and the explicit **4-against-3 overrun** all stand.

## 1. The helper roster — I settled the classification from source, and the answer inverts the fix

Your literal helper roster carries **84** unique identities against **81**
`HELPER-FACT` rows, with three extra names printed as
`KelGroups.Vote.assocAdjust_property`, `KelGroups.Vote.assocErase_property`,
`KelGroups.Vote.assocInsert_property`. Group counts H-none 40, H-prop 6, H-some
23, H-mem-cons 4, H-neq 5, H-bool 6 do sum to 84.

**Do not edit 81 up to 84. The 84 is wrong.** From source at the accepted base:

```
lean/KelGroups/Invariants.lean:79   private theorem assocErase_property
lean/KelGroups/Invariants.lean:87   private theorem assocInsert_property
lean/KelGroups/Invariants.lean:109  private theorem assocAdjust_property
```

Those are the **only three declaration sites in the whole tree**, and there are
**zero** `assoc*_property` occurrences anywhere in `KelGroups/Vote/Invariants.lean`.
Your own `P1A-qualified-classified.txt` already qualifies them correctly:

```
HELPER-FACT|yes|KelGroups.assocAdjust_property|KelGroups/Invariants.lean:109
HELPER-FACT|yes|KelGroups.assocErase_property |KelGroups/Invariants.lean:79
HELPER-FACT|yes|KelGroups.assocInsert_property|KelGroups/Invariants.lean:87
```

So the **roster mis-qualifies three `KelGroups.*` helpers into `KelGroups.Vote.*`**,
creating three phantom identities that are the same three declarations already
counted. The reconciliation confirms it: **158 authored + 81 helper = 239**, while
84 would give 242 and break the total your own report cites six times.

**Repair:** correct the roster's qualification and re-derive the six group counts
after removing the three phantoms. `81` stands; `239` and `158` stand. This is a
document and measurement defect, **not a theorem defect** — no proof is implicated.

## 2. Footprint claims — keep the distinctions you have started to blur

Changed-file **line totals** for D1/D3/D4 are evidence that **context changed**.
They are **not** a demonstrated **relevant footprint change for each receipt**.

**D5 is different and stronger** — it names an actually removed fixture
vocabulary, which does support the stronger claim for those particular
instruments. Keep that asymmetry rather than levelling it.

"Retrieval not pursued because the checker diff settles status" **repeats exactly
the inference already ruled out**. Locate the available full receipts and the
exact subject/checker inputs **before** classifying reuse.

**Unestablished reuse is an acceptable explicit finding** when bounded retrieval
genuinely cannot bind it. Do not inflate it into demonstrated staleness. And a
**pending blocker that is now resolvable is not itself a stale historical kill**.

## 3. The phase plan is still arithmetic over approximate families

Calling **~44 an UPPER BOUND requires evidence that all required work fits inside
it**. Labelling it "unfunded" does not supply that evidence.

- **G-B3** still contains a literal `<family-check-file>` placeholder and "12
  families" with **no actual drivers**. That is not exact argv.
- **G-B1 now names targets — credit.** But its second command is only "gate
  elaboration as shipped". **Inspect and count the actual wrapper's subprocesses**
  before claiming 1 build + 1 elaboration; a wrapper that spawns more than that
  makes the cost wrong.

**No grant for either.**

**Do not substitute re-running historical mutants for Phase-1 receipt binding.**
They are different operations, and **new execution cannot reconstruct missing past
provenance**. Re-running a mutant tells you about today; it does not bind what a
past receipt covered.

## 4. What to return

**One** complete static assessment and executable phase plan that preserves the
**full original theorem/guard/effect target**, with an **actual numbered
operation-to-requirement relation** and **costs at their measured layer** — or
**exact irreducible gaps**.

Not another revision that renames these same unfinished obligations.

**Deferred solvent/alias relevance is unfinished ownership assessment, not
authority to drop those rows.** They stay on the map.

For static satisfiability, **specify the actual instantiation per hypothesis**.
No new universal demand for compiler tests of harmless helper facts is being
imposed — an argument with concrete instantiations is what is wanted.

## Bounds

No Phase 2, build, test, elaboration or auditor grant. **Do not re-run the lost
cold log.** No unrelated scope or model edits. The 4-against-3 overrun stays
explicit with no refund. Preserve the original, R2 and R3 as history. Local files
only.
