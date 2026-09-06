# S3 repair submission 3 (final) — parent verification

Owner `%503`. Static; I ran no project code. Verified at source.

## Structure

`handoffs/submission-3/`, bare paths never written — the preservation rule held.
Manifest **6/6, no self-entry**. `OPMAP-v9`: **207 lines**, effective requirement
set **158** excluding the 2 withdrawn. Bare `RED`: **0**.

Distribution: KILL 85, ELAB-STATIC 60, OBSERVED 31, OPEN-KILL 10, RECOVERED 9,
PREDICTED-SURVIVE 7, WITHDRAWN-DUPLICATE 2, STATIC 2, ACCEPT 1.

A seventh **`EVID:`** column is on **every one of the 207 rows** — N-A 81,
THEOREM-FAIL 71, CASCADE 31, PROOF-FAIL 13, NONE 10, MIXED 1. That is the
four-way per-row distinction I required: actual receipts, predicted theorem
failure, predicted proof-script failure, static cascade.

*(My first two counts of this column were wrong — I split on field index and got
CASCADE 2 instead of 31. Counted on the whole line it matches the owner's claim
exactly. Third time this session that a field-index count misled me; the line is
the unit, not the column.)*

## The four routed defects — all four addressed

1. **`canonical_economy_holds` is now KILL**, under `OP-OPEN`, citing
   `MUT:Step.lean:90-92:drop-members-foldl-distr (OP-23 atom)` with
   `PREDICTED-RED (alice==1 and bob==1 fail, both stay 0)` and `GROUND:(c)`.
   Exactly the kill I established at source, at the right atom, with the right
   witness, and correctly marked **predicted**, not observed. It also keeps its
   `OP-68` ELAB-STATIC row — correct, since a verdict is per (operation, requirement).
2. **The `NO-MUTANT` reasoning class was re-audited.** OPEN fell 31 → 10 **not by
   manufacturing kills** but because **21 formerly-unidentified falsifiers were
   identified**, each now naming a concrete mutant under `OP-OPEN`. I checked the
   provenance of every one of the 21.
3. **The OP-11 conservation over-claim is gone.** `OP-11` now maps only to
   `step_grant_inv`; `conservation_preserved` no longer appears under it, and the
   COLL row now reads **10 arms** THEOREM-FAIL rather than all fourteen.
4. **The measurement request is rewritten and reviewable** — 8 chains (not 7-then-8),
   18 timed invocations with a ceiling of 18 **stating its own excess** (not 12
   against 18), exact argv/cwd/diffs/observables with `C-VOTESTATE` pinned to
   `Vote/State.lean:89`, a named `U-CHECK` target, and no sampling.

Audit-side spend stated separately and correctly: **0 builds, 0 elaborations,
0 probes**; read-only inspection and bounded mechanical parsing only. Ticket spend
unchanged at 5 substantive / 3 targeted. The submission accepts no row.

## One residual, small

The **10** remaining OPEN rows still share the single descriptor
`bounded-search:check-def+production-calls` with no per-row scope or result. The
defect I named earlier persists, on 10 rows instead of 31. It is a live
observation for the auditor, not a blocker.

## Disposition

Submission 3 is **verified as returned, not accepted**. The cap is exhausted:
3 of 3. Acceptance is the fresh independent full static auditor's to inform and
the desk's to decide, over the **complete** returned packet and the original
mandate — not over my labels, and not over the repaired lines alone. All three
submission sets stand immutable and separately verifiable as evidence inputs.
