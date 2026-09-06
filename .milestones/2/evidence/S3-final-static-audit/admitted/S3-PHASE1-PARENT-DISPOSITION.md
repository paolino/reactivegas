# S3 Phase 1 — parent disposition of all eight original findings

Owner `%503`. Local only. **Static; no execution was authorized, requested or
spent for this disposition.** Read at `CORRECTIONS-019.md`
`f6ea115ec7a23921af5b84507dae6a52a54138b4df1e13d6ce61ca662c3ccb9f` and
`OPMAP-v7-requirement-verdict-grounds.txt`
`89337291fd880b9f213e46cd5a61fe178ce1597ab2f39e8184a6b76924436336`.

## Ownership, stated because it was misunderstood

The seat wrote that it had "handed back; no further action available or taken"
while also recording no TERMINAL. Both halves are right and they are not in
tension: **a worker's COMPLETE means its work stopped and control returned to
me. It is not my acceptance, and terminal disposition was never its to hold.**
That is what this file is.

## The generation defect is genuinely fixed, not renamed

`CORRECTIONS-018:44-46` ("unfolding/matching = KILL") is **withdrawn** as the
generation rule. The operative rule is reach: (a) the goal projects the changed
field, (b) a consumed hypothesis mentions it, (c) the statement is false at a
witness satisfying its antecedent, or (P) proof sensitivity **with the actual
failing obligation bound**. Unfolding is evidence a definition is reached, never
the ground.

I re-checked the six rows I had raised, at source. Five were fixed correctly and
one is worth naming carefully:

| row | now | my check |
|---|---|---|
| `close_permission_to_close` | **OBSERVED**, upstream `step_close_inv` | correct, and cited at the right strength — "S4-B P07 corroborates upstream-only", not an individual kill |
| `majority_not_strict_on_even` | KILL, `GROUND:(c)` **n=2, 6≤2 false** | my corrected witness; the invalid n=1 exhibit is gone |
| `majority_table` | KILL, `(c)` 1→2 | correct |
| four guard inversions | ELAB-STATIC | correct — pure Bool decompositions, verified at source |
| `governance_enacts_windUpAdmin` | KILL, `(a)` collections/rest projection | **the mutant changed, not the verdict**: now `collections:=rest→s.collections`, which the goal's projection reaches. The `refundAll`/`conti` mutant stays excluded by projection-erasure. Correct split. |
| `commitBaseChange_members` (OP-57B) | KILL, `(a)` members projection | also a **changed mutant**: `Integration.lean:143` builds `{ post with appFold := appState }`; the new mutant swaps it to a `pre`-based state, so `result.state.members` really does become `pre.members` and the projection is reached. The old `appFold`-only mutant remains correctly excluded. |

The two "changed mutant" rows are the ones a careless reader would mistake for
verdict-shopping. They are not: in both cases the *operation* was replaced with
one that actually reaches the goal, and the original operation stays excluded for
the original reason. That is the correct response to a projection-erasure
finding.

The same holds for `conservation_preserved`. The blanket direct-kill across
fourteen **authorization/guard** mutations is gone; the row now rides
**effect mutants per arm**, and `OP-19` moved from removing `isResponsabile` to
`MUT:Step.lean:65:drop conti-bump-line`. That mutant is consumed — the deposit
branch's `rw [bump_sum, bump_sum]; omega` and `deposit_double_entry`'s
`⟨bal_bump .., bal_bump ..⟩` are exactly balance equations. The unused-guard-witness
objection is honoured rather than argued around.

## The eight findings

| # | finding | my disposition |
|---|---|---|
| F-01 | receipt transcription | **CLOSED on the record.** Corrected transcriptions retained with the 42/43 split and five `G74-*` rows left OPEN by name. |
| F-02 | provenance recovery | **CLOSED.** Report-sha and candidate distinguished; `000ff76a` in DB; the six t57 instruments read as fixtures; nothing auto-upgraded. |
| F-03 | ownership relation | **PARTLY.** Literal per-row atoms and the reach taxonomy land it; `R-canAdd` linkage stays **OPEN** by the seat's own admission and mine. |
| F-04 | `no_expiry` scope | **CLOSED.** Corrected to the accepted arbitrary-event statement. |
| F-05 | non-vacuous witnesses | **CLOSED.** Per-hypothesis instantiations; vacuous cases replaced. |
| F-06 | executable complete plan | **PARTLY.** The map is finite and verdict-tagged, which was the ask. The plan is **not funded and not authority**; see the envelope below. |
| F-07 | attribution vs isolation | **PARTLY.** The three-way split (attribution / separation-OPEN / historical-fence) is honest, and the separation leg is still OPEN. |
| F-08 | append-only journal | **CLOSED, verified by readback.** 764 lines, tail at real EOF, handback block last. |

Three PARTLY, five CLOSED, zero re-opened. **No finding is closed by the seat's
assertion**; each above rests on the artifact or the source I read.

## Envelope — arithmetic checked, authority withheld

The desk stated it had not validated the arithmetic, so I did. It is
**internally consistent**: 3×20 + 2×14 + 3×4 + 2×6 + 5×2 + 4×1 + 4×1 =
60+28+12+12+10+4+4 = **130**; plus 4 re-runs, 7 ELAB file runs and 2
elaborations = **143 targeted**, with **1 build** separate. Every op-count in
each class also matches its listed members (20, 14, 4, 6, 2, 1, 1).

**Consistency is not correctness.** The closure multipliers (3, 2, 3, 2, 5, 4, 4)
are claims about mechanical closure that I have **not** verified, and no part of
this is a grant. The envelope stays **unfunded arithmetic**. All prior envelope
totals are withdrawn as fitted artifacts, correctly.

## What I did not check, stated plainly

I verified **six rows of 207** plus the eight dispositions, the journal EOF and
the envelope arithmetic. I have **not** validated the remaining KILL grounds, the
31 OPEN-KILL bounded searches, the OBSERVED upstream attributions, or the
verdict counts (75/29/31/60/9/2/1). Those are unaudited by me and are not
accepted by this file.

The OPEN set is non-empty and stays that way. **The target was a correct
assessment, not a zero-OPEN table** — and on that measure this packet is the
first version of the map whose method does not re-derive its own conceded errors.

Routing unchanged: D6 clarity items to #71 through the desk; S5 items to S5;
V-5 lifecycle to #81; `ONWARD-68-INV-01` text unmodified. No Phase 2, no new
execution, no audit grant. `#66` stays open; C1 keeps the next landing.
