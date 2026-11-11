# S3 final static packet — full parent assessment and successor recommendation

Owner `%503`. Static; no project code run, no fourth submission inferred, cap
stands at 3/3 spent. Verified at source; the four desk challenges are **checked,
not relayed**, and one of them is not borne out by the bytes.

## A. The four challenges, adjudicated

### A1. The "first failing obligation" universal — CONFIRMED FALSE

The packet's **own** retained `P1C-build2-incremental.log` refutes it: line 159
records `error: Reactivegas/Invariants.lean:407:11`, and lines 167-172 record
`info:` axiom outputs at **1639, 1640, 1641, 2351, 2352, 2353** — all later in the
same file. **Elaboration demonstrably continued past the failing obligation.**

`COST-MODEL` r3's claim that a build halts at its first failing obligation, and
every `PROOF-BLOCKED` inferred from an earlier same-file line, rest on a universal
its own evidence contradicts. The distinction to keep: **a failed imported module
and an earlier theorem error in the same elaboration are different mechanisms.**
Equally, this log does not prove every later mutant witness executes, and source
order does not prove none can. Execution-availability must be reassessed per
dependency against actual evidence — and no extra isolation work may be
manufactured from the false universal either.

### A2. `admissionPreservation_holds` is NOT value-insensitive — CONFIRMED

`checkAdmissionPreservation := admissionPreservationReachable` (`:2303`);
`admissionPreservationReachable := checkAdminAdmissionReachable &&
checkAppMembersPreservation` (`:1933-1934`); and `checkAppMembersPreservation`
(`Step.lean:415`+) compares `result.state.members == preservationGroup.members`.

The map's **own** OP-69 production-members-wipe is already claimed to falsify that
check — so it falsifies the conjunction, hence the row. Saying "the Validate atoms
leave admin admission unchanged" examines one conjunct and stops. OP-58B on the
integrated app branch needs the same direct analysis. **Static semantic challenge,
not an executed kill.**

### A3. Internal contradictions — three CONFIRMED, one NOT BORNE OUT

- **CONFIRMED.** `ELAB-GREEN-43` ends "No row challenged as killable by a current
  mutant", while `canonical_economy_holds` is now **KILL** in `OPMAP-v9`. Flat
  contradiction inside one frozen packet.
- **CONFIRMED.** `COST-MODEL.md:40` still reads "OPEN-KILL 31: NO-EXECUTION (OPEN
  stands…)" against the new **10**.
- **CONFIRMED, and worse than stated.** `OP-58` predicts
  `tryEnactBase_runs_hook` **PREDICTED-SURVIVE** ("proof never consumes the ≥
  shape"), yet `OP-58 base_change_runs_hook` is **OBSERVED / EVID:CASCADE** citing
  that same row as upstream. **A cascade from a row predicted to survive has no
  failure to cascade from.** It must be resolved as either a mere consumer
  relationship or an actual predicted failed dependency — it cannot inherit a kill
  from a survivor. **And that row appears twice, identically** — a duplicate the
  challenge did not note.
- **NOT BORNE OUT.** The claim that `OPEN-EXTENTS` "opening/ending still say all 31
  remain OPEN" does not match the bytes. Its header reads
  *"OPEN-EXTENTS-31r3 … VERDICT now lives in OPMAP-v9 (21 rows converted to
  PREDICTED-KILL(c), 10 stay OPEN)"*, and its ending reads *"the v7 single
  descriptor is withdrawn"* with per-row VALUE-FLIP / VALUE-STANDS / REACHABILITY
  sections. The file was rewritten. Only the **filename** still carries "-31", as
  an extent record for all 31 rows. I report what the bytes say.

### A4. The measurement request is not executable as written — CONFIRMED

- `U-CHECK` is `import Reactivegas.Invariants` + `#eval checkSweepIdempotent`,
  **unqualified**, while the declaration lives inside `namespace Reactivegas`.
  **Bind the fully-qualified target.**
- It is labelled *isolated decide elaboration* but the command is a **`#eval`**.
  Those are different costs; state exactly which is measured and claim no
  proof-elaboration isolation from an evaluation timer.
- The eight cycles are called incremental rebuilds, yet the **one cold build is
  listed after them**, so no clean-built scratch baseline is established before
  cycle 1. **State order and cache provenance, and retain setup costs.**
- 18 is now the correct count for the enumerated calls, but **a coherent count is
  not operation identity or full costing**: `U-REPLAY` appears **zero** times — it
  is still unmeasured and absent from the request.

## B. Against the original obligations

**D1-D6 and the eight findings.** My standing disposition is five CLOSED
(F-01, F-02, F-04, F-05, F-08) and three PARTLY (F-03, F-06, F-07). Submission 3
changes none of them to CLOSED, and **F-06 hardens**: the map is now
evidence-tagged per row and the cost units are defined, but the cost model still
carries a false universal (A1), a stale 31 (A3), an unmeasured `U-REPLAY` and a
non-executable request (A4). **F-03** and **F-07** are unmoved.

**What submission 3 genuinely fixed**, verified earlier at source and not
withdrawn: `canonical_economy_holds` now KILL at the OP-23 atom with the correct
witness and marked predicted; the `NO-MUTANT` class re-audited so OPEN fell 31→10
**by identifying falsifiers, not by manufacturing kills** — I checked all 21; the
OP-11 conservation over-claim removed; the seventh `EVID:` column on **all 207**
rows; bare `RED` eliminated; manifest 6/6; effective requirement set 158.

**No known blocking row is being converted into acceptance or into a named
residual to stay inside the cap.** A1-A4 are outstanding defects, recorded as
such.

## C. Recommendation on the exhausted commission

**Commission the already-authorized fresh FULL STATIC audit over the frozen
packet now, as a findings-bearing candidate — not as an acceptance run** — with
A1-A4 bound into its brief as **known-outstanding inputs it must weigh rather
than rediscover**.

Reasoning:

1. Earlier I refused to send an auditor over a packet whose defects were known to
   me and not to it — that would launder them through an independent verdict.
   **That objection is answered by naming them in the brief.** The auditor is not
   being sent in blind; it is being told what is already known and asked what else
   is true.
2. A fourth author round would still be **self-assessment**, and the cap exists
   precisely to stop that regress. Three submissions have produced real
   convergence; a fourth is unlikely to produce independence.
3. The remaining defects are of a kind an independent family is **better** placed
   to adjudicate than the author is: a false universal about elaboration
   behaviour, an insensitivity claim contradicted by a conjunct, three internal
   contradictions, and a measurement request that must be executable before it is
   ever granted.

**Binding conditions for that audit**: it covers the **complete** returned packet
and the original mandate, not the repaired lines and not my labels; **no finding
may be downgraded to a residual to fit any cap**; audit-side spend is stated
separately and never inferred from the author's submission count; and acceptance
does **not** follow from the audit alone — it returns findings, and the desk
decides.

**If the audit concludes the packet cannot bear findings**, that is a successor
scope-and-ceiling question for the desk, and I will return it as one rather than
absorb it.

---

## CORRECTION (append-only) — two of my claims in §A3 were wrong

Both carried forward with the assessment, not edited away.

### 1. My "NOT BORNE OUT" was itself not borne out

`OPEN-EXTENTS-31.txt` **does** contain the stale assertions:

```
:8   … flips this check within the stated extent) — the OPEN verdict stands in all 31.
:47  # End: 31 rows, OPEN stands in all 31, each with its examined extent above.
```

while `:1` says "21 rows converted to PREDICTED-KILL(c), 10 stay OPEN". The
manifest still verifies 6/6, so this is the same version, not a different one.

**The defect is contradictory current prose inside one file — exactly as
challenged — and not a filename artifact.** My conclusion came from sampling the
header and tail rather than searching the body for the claim. Withdrawn.

### 2. The duplicated row does not exist

`OPMAP-v9` has **exactly one** `KelGroups.base_change_runs_hook` row, at line
**207**, confirmed by line-numbered search over all 207 lines.

My "appears twice identically" came from misreading my own terminal output: I ran
a `^OP-58` grep and a `base_change_runs_hook` grep in the same command block, and
the row printed once in each. **Two greps, one row.** Withdrawn, and the packet's
denominator is unchanged by it.

**The survive-versus-cascade contradiction stands** on its own: `OP-58` predicts
`tryEnactBase_runs_hook` PREDICTED-SURVIVE while `base_change_runs_hook` is
OBSERVED/CASCADE citing it as upstream.

### The pattern, since it is twice in one assessment

Both errors are the same shape: **a conclusion drawn from a sample of a file or
of my own output, presented as a fact about the whole.** `head`/`tail` is not a
search, and two grep hits in one block are not two rows. Where I have insisted
others read the full artifact before asserting, I did not.
