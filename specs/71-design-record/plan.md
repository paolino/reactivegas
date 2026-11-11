# Plan — #71, version 2

Base origin/master ddb1703 merged at 63e686c. One slice, OWNER topology under the
operator team of 2026-11-08: ticket owner (claude opus 5.5, high), commit owner
(claude sonnet 5.5, high), persistent auditor (claude sonnet 5.5, high).
draft=NONE. gate-authors=NONE (operator team 2026-11-08): the ticket owner
authors the frozen gate.

## Strategy

1. RED: the checker exists, is wired into a hosted step, and goes red on the
   current record for its intended reasons (stale or unresolved citations,
   uncited declarations); the controls (cold tree, stale citation, uncited
   declaration) are red through the real checker.
2. GREEN: the record is rewritten against master until the checker is green and
   every record requirement in `spec.md` holds.
3. Push; hosted CI is the net. The ticket owner states the measured hosted cost
   of the new step in the PR body.

## Constraints

- No file under `lean/` changes (the checker's Lean helper, if any, lives outside
  the library roots so the mutant ledger and the axiom/mirror checks are
  untouched).
- Every local Lean invocation goes through the milestone lock and memory cap:
  `flock /tmp/reactivegas/ms2/lean.lock systemd-run --user --scope -q -p MemoryMax=24G -p MemorySwapMax=0 -- env LEAN_NUM_THREADS=4 <cmd>`.
  No Lean language server.
- The "Lean mutant ledger" job is near its time limit: nothing is added to it.

## Slices

- S71-C (single slice): checker + hosted wiring + controls + rewritten record.
  Bisect-safe: documentation and a new check only.
