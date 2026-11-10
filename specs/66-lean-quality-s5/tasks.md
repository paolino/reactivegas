# Tasks — #66 S5

## Exact inversions

- [x] T501 Hold every bound event inversion to its converse, selected from the discovered extent (tightness-over-discovered-extent, every-inversion-exact)
- [x] T502 State every live guard conjunct in the inversion conclusions, including `stalled` for pledge, accept-pledge and close-purchase; grant and deny held to the same converse (live-guard-conjuncts-stated)
- [x] T503 Extend the negative control to a non-exact conclusion on a constructor outside the former hand list (tightness-negative-control)
- [x] T504 Re-derive the mutant ledger on the changed statements (ledger-current)

## Observed refusals

- [x] T511 A statement observing each of the six surviving refusal constructors, or a recorded reason it is unobservable (refusal-observed, ledger-current)
- [x] T512 The converse negative control rejects only for the omitted conjunct; a malformed weakening fails the run (converse-control-rejects-for-its-reason)
- [x] T513 Mutants on the `stalled` guard of pledge, accept-pledge and close-purchase, each killing its inversion (stall-guard-mutants, ledger-current)
- [x] T514 Correct the comment in `scripts/check-lean-axioms` that still names the removed hand list

## Question retention

- [x] T521 Open questions retained outside the proposer-departure closures (question-retained, ledger-current)

## Statement triage

- [x] T531 Census of `unconstrained` rows at slice start, one row per theorem
- [x] T532 Each row killed or its reason recorded, the reason stored and rendered by the ledger (unconstrained-triaged, ledger-current)
- [x] T533 The simulation machine builder's `comuneReserved` site observed or reasoned (comune-reserved-second-site)
- [x] T534 The unconditional bootstrap refusal stated or reasoned (bootstrap-refusal-stated)
- [x] T535 Hosted ledger wall time projected from a timed local run (ledger-wall-time-projected)

## Mirror auxiliaries

- [x] T541 Mirror checker excludes compiler auxiliaries by kind, with its negative control (mirror-auxiliaries-by-kind)

## Finite history

- [ ] T551 Integrated validator with checked genesis and its negative controls (genesis-checked, validation-not-decision)
- [ ] T552 Acceptance implies production history, refused steps preserve state (validated-history-is-production-history, refusal-preserves-state)
- [ ] T553 Every production-history aggregate has an accepted history (production-history-has-validated-witness)
- [ ] T554 Fixed-view validator and its `Reach` correspondence both ways, with the non-empty-initial control (fixed-view-history-reaches)
- [ ] T555 The vote corpus bytes validated in a hosted step, with planted controls (vote-corpus-validated)
- [ ] T556 Ledger rows for the new theorems, re-derived ledger, hosted wall time projected (finite-history-ledger, ledger-current, ledger-wall-time-projected)
