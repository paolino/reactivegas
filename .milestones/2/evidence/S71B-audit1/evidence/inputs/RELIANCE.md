# Reliance declaration — t71-design-record/commit-owner-s2 (S71-B, docs-only successor)

Slice touches only `docs/en/design/state-machine.md` prose plus
docs-companion citation markers (state-machine + vote-machine marker
additions ONLY as briefed). It depends on the following being true about
code it did not write and does not own. `enforced: NONE` rows are named
assumptions, not gaps to close in this slice.

## Rows

```text
INV-71-PINNED-LEAN
invariant:  Lean sources at 4a6cd87f read as recorded (authorizedStep
  Predicates.lean:74 proves author role only, ignores state/args;
  grant/deny pullCollection prerequisite first Step.lean:53/57, absent id
  refused; close_spends_referente Invariants.lean:679 proves cassa decrease
  bal s'.casse col.referente = bal s.casse col.referente - sumPledges;
  deposit guard 0 <= v (triplet -1 refused / 0 accepted / +1 accepted);
  bump appends (u,d) when absent so accepted zero deposit stores (u,0),(a,0);
  Event 14; canCloseGroup orphan single usage).
severity:   ADVISORY (docs prose; a drifted pin makes prose stale, moves no money)
enforced:   PARTIAL — gate v5 legs 2-6 + leg 11 PIN line checks + leg 14
  witness driver re-derive the same sources; full elaboration trust is
  leg-12 `just ci`, not this file.
```

```text
INV-71-GATE-V5-IMMUTABLE
invariant:  ./gate.sh v5 stays byte-identical through the slice
  (sha256=88074d4f8fbefdbfc83589fc044f6fa81a439ec309744ed843b4784fa250ebed,
  PIN=4a6cd87f).
severity:   ADVISORY
enforced:   PARTIAL — sha256 checked before each run; gate owned read-only by
  ticket owner, never edited here.
```

```text
INV-71-SOLE-WRITER
invariant:  No other lane edits docs/en/design/ during S71-B (freeze holds).
severity:   ADVISORY
enforced:   NONE — freeze is a social ruling, not a mechanical lock. Guarded
  only by git status/diff inspection before freeze/COMPLETE; concurrent edits
  would be detected, not prevented.
```

```text
INV-71-CLEAN-TREE
invariant:  Worktree base 90dae99 is clean and no other process writes the
  tree mid-slice.
severity:   ADVISORY
enforced:   PARTIAL — git status --short --branch + git diff --check before
  every freeze; concurrent writers would show as dirty state, not as proof.
```

```text
INV-71-HANDOFF-NOT-TRUSTED
invariant:  Handoff counts (109 markers, 22 flagged spans, 8 VM rows) are
  leads only; every cited extent below is re-derived at source (claimscan
  DISC, table cell counts, fence signatures, witness driver outputs).
severity:   ADVISORY
enforced:   PARTIAL — re-derivation commands recorded in STATUS/receipts;
  uncited list re-derived by running gate leg 13, not by trusting the brief's
  22-count.
```

## Summary

count=5 enforced-partial=4 none=1. No BLOCKING row: docs prose cannot move
chain state, money, or signatures. No reliance found false; no
CONTRACT-CHALLENGE.
