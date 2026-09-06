# Plan — #71 design record rewrite

Base e6c59242. One OWNER slice (docs + untracked gate proof, no production code).
Draft=NONE. Commit owner muse, auditor grok-4.6.

## Strategy

1. Baseline citation assessment against current stale `state-machine.md`:
   derive State/Event/Route/step/canCloseGroup/check-category facts at source,
   run citation discovery, show gate RED for intended reasons (stale fields,
   15-vs-14, missing GroupView, L1 deleted transition, law/witness conflation,
   missing Voci/authority/composition-limits). No full builds; cheap static
   discovery only in planning.
2. Rewrite `docs/en/design/state-machine.md` against merged model; carry
   `kelgroups-vote-machine.md` forward only if re-derivation finds drift
   (e-lean-compliance reports 30/30 clean — verify, do not trust).
   Every declaration-like claim gets a marker block with pinned file:line +
   commit; prose states law-vs-witness, composition/vote limits, Voci non-goal,
   dated authority, current-vs-ruled pending table.
3. Author the immediate-citation checker as the slice's executable companion:
   discovery over docs markers + Lean discovered extent, fail-closed on missing
   Lean prerequisite, negative controls for malformed/unknown/missing.
   The checker itself is documented; the gate invokes it.
4. canCloseGroup: report-only classification with product-intent references
   (Q-001/Q-2/Q-6/comune/stall/V-series); no Lean edit, no new theorem.
5. Slice lands current-honest; reconciliation task re-pins exact source blobs
   and refreshes current-vs-ruled rows after each of S1/#68/#69 merges.
   Final closure reconciles accepted milestone slice inputs; never mark stale
   snapshot complete because prose passes.

## Slices

- S71-A (this ticket, single slice): rewritten record + failing-closed citation
  gate + negative controls + pending-table + reconciliation hook. Bisect-safe:
  docs only; gate untracked/ignored; no Lean/Haskell/simulator touch.

## Live boundaries

None — no runtime, no network, no credentials. Lean reads are static file +
optional `lake env` resolution in implementation (budgeted); planning uses
grep/reads only.

## Constraints

Freeze holds for other lanes; this lane is the sole docs/en/design/ writer.
Quantify over discovered extent everywhere. Later rulings supersede earlier.
No anticipation of unmerged semantics.

## Risks

- Pending merges invalidate pins → mitigated by pending-table + re-pin task.
- Citation syntax drift → mitigated by discovery + negative controls.
- Over-claiming finite witnesses as laws → mitigated by R71-05 marking rule.
