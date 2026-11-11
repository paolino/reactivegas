# S3 disposition — addendum 2: the OBSERVED class closed, and the duplicate defect measured

Owner `%503`. Static only; no execution, no author wake. Continues
`S3-PHASE1-PARENT-DISPOSITION.md` (`57506531…`) and Addendum 1 (`6bfdd1a7…`)
against `OPMAP-v7` (`89337291…`). `%558` is terminal and was not contacted.

## 1. The OBSERVED class — all 29 examined, one of my four stated gaps closed

I checked every OBSERVED row by asking whether the theorem's proof actually
consumes the named upstream, reading the source.

| outcome | rows |
|---|---|
| **verified proof-call cascade** — the named upstream appears in the proof body | **24** |
| **alias, not a cascade** — "upstream KelGroups counterpart" | **4** |
| **names a lemma class, not a row** — "upstream threshold lemma" (`OP-62`) | **1** |

Three of the 24 needed a wider source window than my first pass used. **That was
my measurement limitation, not a defect in the map** — `open_mem`, `sweepReady`
and `foldEvents` all appear in their proofs once the window covers the whole
proof. I record it so the number is not read as 21 + 3 doubtful.

The one weak citation is `OP-62 KelGroups.enact_implies_threshold_met`, whose
ground is "upstream **threshold lemma**". The map's own promise is that OBSERVED
names the upstream **row**. A lemma class is not a row.

## 2. The duplicate defect is systematic — and half of the look-alikes are legitimate

Addendum 1 found two misnamespaced duplicates. Measuring the whole map: **17 base
names appear under more than one spelling**, and they split cleanly.

**Group A — 8 genuine pairs, two declarations each. NOT a defect.**
`app_members_preservation_holds`, `app_members_preservation_mutant_caught`,
`base_change_can_close_without_ballot`, `base_departure_applies_cleanup`,
`base_recompute_reachable_holds`, `direct_admission_only_holds`,
`sweep_idempotent_mutant_caught`, `sweep_idempotent_witness` — each exists twice
in source, once in a `Reactivegas/*` module and once in `Reactivegas/TraceTests.lean`
(the production theorem and its test-owned oracle). Two spellings, two
declarations, two real obligations.

**Group B — 9 spurious pairs, ONE declaration each.**
`approvals_nodup`, `baseHook_votes`, `base_change_recomputes_votes`,
`enact_implies_threshold_met`, `majority_not_strict_on_even`, `majority_table`,
`member_key_coherent`, `members_change_implies_enacted`, `proposer_mem_approvals`
— each has exactly **one** `theorem` declaration in exactly one file, yet appears
in the map under two spellings.

**The trap, stated because a careless fix would cause the damage:** collapsing
duplicate base names wholesale would destroy the 8 legitimate Group A
obligations. The discriminator is the source, not the name — *how many
declarations exist*, never *how many spellings*.

Four of the Group B pairs carry **different verdicts on the same declaration** —
one spelling KILL, the other OBSERVED "upstream counterpart":

```
OP-67  KelGroups.majority_table                 KILL      (c) 1→2
OP-67a majority_table                           OBSERVED  upstream counterpart
OP-67  KelGroups.majority_not_strict_on_even    KILL      (c) false at n=2
OP-67a majority_not_strict_on_even              OBSERVED  upstream counterpart
OP-60  KelGroups.members_change_implies_enacted KILL      (P) removeMember-case
OP-64a members_change_implies_enacted           OBSERVED  upstream counterpart
OP-62  KelGroups.enact_implies_threshold_met    OBSERVED  upstream threshold lemma
OP-64c enact_implies_threshold_met              OBSERVED  upstream counterpart
```

A same-declaration alias is **not** the OBSERVED the key defines ("the proof
calls a sibling lemma whose failure cascades"). There is no sibling; there is one
theorem.

**Consequence:** the map's 160 requirement tokens correspond to **151 distinct
declarations**. The "158 authored, machine-audited, 0 helpers" statistic cannot be
right at the same time as this, and the audit that produced it verified *presence
of a token*, never *a distinct declaration behind it*.

## 3. KILL structural properties — verified mechanically

All **75** KILL rows carry a bracketed ground class, none malformed:

| ground | rows |
|---|---|
| (a) goal projects the changed field | **54** |
| (c) statement false at a witness | **6** |
| (P) proof sensitivity | **15** |
| (b) consumed hypothesis mentions it | **0** |

(b) being unused is an observation, not a defect.

All **15** (P) rows name a **specific** failing-obligation shape — cast-case arm
reasoning, ok-arm routing-unfold, induction-unification, threshold-split shape,
removeMember-case exact-term, assoc-lemma application, `assocErase` application,
threshold-equation, split-shape. None is a bare "proof sensitivity" label, which
is what NOTE-018 required.

I sampled two against the source and both are accurate:
`tryEnact_preserves_wellFormed` (`KelGroups/Invariants.lean:228`) really does
`by_cases threshold : pending.approvals.length ≥ majority gs` and `simpa … using`
in both branches, so the `≥ → >` mutant genuinely changes the split it rewrites
with; `foldFrom_preserves_qid` (`Vote/Invariants.lean:786`) really proceeds by
`induction events`, which the `accum→current` mutant unifies against.

**Naming a shape is not binding the obligation term.** These are specific and
checkable in principle, and far better than a label — but confirming each
actually fails under its mutant needs a per-row source read or execution, and
I have done two.

## 4. Effect on the disposition

**F-06 stays PARTLY**, now with a third named reason: the requirement basis is
inflated by 9 same-declaration duplicates. **F-03 stays PARTLY.** Nothing moves,
nothing closes. **None of this is a semantic finding against the Lean** — every
theorem named here exists and is proved. It is a defect in the map's identity
handling and in the audit that certified it.

## 5. Extent, exactly

**Reviewed by me across both addenda:** all 7 verdict counts; the 207-line total;
`GROUND:` present on every KILL and an upstream on every OBSERVED; the full
requirement-token set against RELATION-v2; **the entire OBSERVED class, 29/29,
against source**; the KILL ground-class distribution; **all 15 (P) grounds for
specificity**, 2 of them against source; the whole-map duplicate-spelling analysis
against source declaration counts; plus the 6 rows in the base disposition and the
journal EOF.

**Not reviewed by me:** the content of the 54 (a) and 6 (c) KILL grounds beyond
those sampled; the 31 OPEN-KILL bounded searches; the 60 ELAB-STATIC
classifications; the closure multipliers behind the unfunded 143+1 envelope.
**Not accepted here.**

**Smallest thing that would settle the remainder**, if it is wanted: a per-row
source read of the 54 (a) grounds (does the goal actually project the mutated
field) and of the 60 ELAB-STATIC rows (is the statement really unreachable by any
declared mutant). Static, no execution, no grant — only time. The 31 OPEN-KILL
rows need no review to be honest: an OPEN is already the correct outcome.
