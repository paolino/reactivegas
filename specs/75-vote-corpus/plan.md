# Plan — #75

## Strategy

One OWNER slice, `vote-corpus-with-replay-context`, on `222ad9a`. Additive
Lean modules plus the exporter, the verify recipe and the frozen files. No
existing model module changes. Re-freeze (re-emit, re-run gate) after #69 lands.

Checkpoints for the persistent auditor: RED bundle; each acceptance line in
`tasks.md` GREEN; pre-push.

## Constraints

- Lean jobs only through the milestone locks (LEAN-BATCH-RULE v2):
  ordinary builds `flock /tmp/reactivegas/ms2/lean.lock systemd-run --user
  --scope -q -p MemoryMax=24G -p MemorySwapMax=0 -- env LEAN_NUM_THREADS=4 <cmd>`.
  No lean-lsp, no `lake serve`.
- `just lean` keeps its own controls (dependency direction, inversion coverage,
  axioms, trace agreement, mirrors). New modules satisfy them; none is weakened.
- The mutant ledger (`scripts/lean-mutants/run`) censuses every tracked Lean
  module. New modules must keep it valid: prefer executable checks to new
  `theorem`s, and prove the census still passes with
  `scripts/lean-mutants/run --census` (long lock) before the pre-push
  checkpoint. No existing model module is edited.
- The corpus is emitted only by the exporter executable; the gate never writes
  the committed files.
- The replayer never evaluates the policy declaration; the policy check never
  reads the table through the writer's code path.
- Point-of-query domain check: no pre-scan that predicts which sizes are
  queried (R3.1 §4).
- `lean-corpus-verify` stays one target, extended; hosted step "Verify corpus
  emission" runs it. Budget: verify ≤ 10 minutes on the hosted runner.

## Live boundaries

Hosted CI job "Build and check" (`just lean`, `just lean-corpus-verify`),
job "Lean mutant ledger" (unchanged inputs expected). Local final sanity:
`nix develop --quiet -c just ci` once on the final candidate.

## Downstream (recorded, not done here)

#66 S5-C finite-history correspondence and #70 simulator Lean-replay oracle
consume this corpus. Corpus verification stays its own fast target.
