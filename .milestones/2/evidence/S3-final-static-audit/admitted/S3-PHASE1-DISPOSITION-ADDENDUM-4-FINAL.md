# S3 disposition — addendum 4 (final): remaining classes reviewed, cost model unauditable

Owner `%503`. Append-only; Addenda 1-3 preserved. Static only, no execution, no
author wake, no new grant. All counts here use the corrected instrument from
Addendum 3 — **declaration sites, never files, never spellings.**

## 1. The (c) grounds — 6/6 verified at source

| row | check |
|---|---|
| `KelGroups.majority_table` | mutant `(n+1)/2 → n+1` flips `["a"]` from 1 to 2 against a `decide` table ✓ |
| `KelGroups.majority_not_strict_on_even` | `2*(2+1)=6 ≤ 2` false at n=2, which satisfies positive-and-even ✓ |
| `Reactivegas.app_members_preservation_holds` + its `TraceTests` pair | `theorem … : checkAppMembersPreservation = true := by decide` (`Step.lean:445`) ✓ |
| `Reactivegas.comune_cannot_authorize` | `… : checkComuneCannotAuthorize = true := by decide` (`Step.lean:470`) ✓ |
| `Reactivegas.productionWellFormed_holds` | `… : checkProductionWellFormed = true := by decide` (`Invariants.lean:2348`) ✓ |

For the four `decide`-checks, ground (c) is the right class in the strongest
sense: a mutant that flips the check makes the **statement literally false**, not
merely the proof fragile.

*Sibling-slice observation, at its correct strength only:* S4-B's O5 negative
control reddened `Step.lean:446/449/471` with "Tactic `decide` proved that the
proposition …" — executed evidence that this class of mutant does flip these
checks. It corroborates the **mechanism**; it is not this map's evidence and
closes no S3 row.

## 2. The (a) grounds — 3 verified at source; my mechanical pass does not count

I built a mechanical check for all 54 and **it does not implement the test**. The
grounds name *prose descriptions* of the projected part — "post-state", "per-arm",
"sweep-output", "equality-to-gs", "condition" — not Lean identifiers, so matching
them against statement text measures nothing. **I am reporting no numbers from
it.** Same failure shape as Addendum 2's; caught this time before publishing.

Read properly, three verify:

- `step_grant_inv` (`:197`) concludes `s' = { s with collections := { col with permitted := true } :: rest }`; mutant `Step.lean:55` drops exactly that `permitted := true`. ✓
- `step_close_inv` (`:305`) concludes `s' = { s with casse := bump s.casse col.referente (-(sumPledges col.accepted)), collections := rest }`; mutant `Step.lean:131` drops that casse line. ✓
- `credit_pledges_step` (`:890`) concludes over `bal s'.conti u` for `u ≠ comuneId`; mutant drops the conti-comune credit, and the row correctly qualifies itself "donate-arm case" — the `≠ comuneId` guard means the effect must surface on a non-comune balance. ✓

## 3. OPEN-KILL — a bounded search **is** stated, and I nearly reported otherwise

Reading only the last column, all 31 rows say the identical "OPEN (falsifying
mutant unidentified)", and I was about to report that no bounded search is stated.
**Reading the full row corrects it:** column 4 carries
`bounded-search:check-def+production-calls` on all 31. The claim in
`CORRECTIONS-019` is met.

The residual defect is narrower and real: it is **one identical descriptor on all
31 rows**, under a single pseudo-id `OP-OPEN`, with no per-row scope, no per-row
result, and no statement of what was examined *for that theorem*. That names a
search **shape**, not a bounded search with an extent. An OPEN remains the correct
outcome; the search claim behind it is not verified merely because the label is
conservative.

## 4. ELAB-STATIC — 60 rows, two different justifications

| justification | rows |
|---|---|
| **structural** — bare projection/`rfl`, tautological Bool decomposition, checker-unfold, test-def unfold, pure list, parametric | **17** |
| **per-operation survival** — "GREEN (witness/check)", "GREEN (mirror/witness/check)", "GREEN (routing equation)", "GREEN (wrapper equation)" | **43** |

Only the first matches the key's definition ("neither (a)/(b)/(c) nor a cascade —
tautological/parametric/structural/checker-local, verified at elaboration"). I
sampled the GREEN rows and they are genuine witness/check declarations
(`theorem X : checkY = true := by decide`), so the disposition is right; the
**label's basis is stated as an outcome rather than as structure.**

**Not a defect, and I checked before calling it one:** the same declaration
carries different verdicts under different operations — `Reactivegas.comune_cannot_authorize`
is ELAB-STATIC under `OP-68` and KILL(c) under `OP-69L`. For a per-operation map
that is correct behaviour, not a contradiction.

So: **definitional imprecision, not a false claim.**

## 5. Cost multipliers — undefined, not carried per row, **unauditable as written**

The multipliers ("Step-closure **3** ×20 ops", "Fold-closure **2** ×14", …) appear
**only** in one prose paragraph of `CORRECTIONS-019` §4. Nothing states **what the
multiplier counts** — files to re-elaborate, invocations per mutant, driver runs.
And `OPMAP-v7` carries **no cost or closure column at all**: six columns, zero
occurrences of "closure".

Therefore the 143+1 total is **not auditable even in principle from these
artifacts** — not merely unfunded pending a grant. Addendum 1 verified it is
*internally consistent*; consistency of an undefined quantity is not correctness.
The repair is definitional: state what the multiplier counts, and carry it **per
row** so it can be summed and checked.

## 6. Disposition of the eight findings — final

| # | finding | disposition |
|---|---|---|
| F-01 | receipt transcription | **CLOSED** |
| F-02 | provenance recovery | **CLOSED** |
| F-03 | ownership relation | **PARTLY** — `R-canAdd` linkage OPEN |
| F-04 | `no_expiry` scope | **CLOSED** |
| F-05 | non-vacuous witnesses | **CLOSED** |
| F-06 | executable complete plan | **PARTLY** — the map is finite and verdict-tagged, but its **cost model is undefined and unauditable**, and the `OP-62` citation and the single shared OPEN-KILL descriptor stand |
| F-07 | attribution vs isolation | **PARTLY** — separation leg OPEN |
| F-08 | append-only journal | **CLOSED**, verified by readback |

**Five CLOSED, three PARTLY, zero reopened.** Nothing closed on the seat's word.
No finding here is semantic against the Lean: every theorem named exists and is
proved.

## 7. Exact extent, and the capacity handoff for the rest

**Reviewed by me, across all four addenda:** the 7 verdict counts and the 207-line
total; `GROUND:` on every KILL and an upstream on every OBSERVED; the requirement
token set against RELATION-v2; the **entire OBSERVED class 29/29** against source
(28 verified cascades, 1 underspecified citation); the whole-map duplicate
analysis by declaration site (8 genuine production/`TraceTests` pairs, 7 genuine
root wrappers, **2** true duplicates); the KILL ground distribution 54/6/15/0; **all 6
(c) grounds**; **all 15 (P) grounds for specificity**, 2 against source; **3 (a)
grounds** against source; the ELAB-STATIC justification split with a sample; the
OPEN-KILL descriptor; the cost-model definability; and the journal EOF.

**Not reviewed, and not accepted:** the remaining **51 (a)** grounds and the
**43** GREEN-justified ELAB-STATIC rows, per row against source.

**Handoff, concretely.** Each remaining row is one source read: open the
declaration, read its statement, read the mutated definition named in column 4,
and answer one question — *does the statement project, or consume, the mutated
part?* No execution, no grant, no instrument. 94 rows at roughly one to two
minutes each. It needs time, not authority; the method is fixed above and needs
no further decision from anyone.
