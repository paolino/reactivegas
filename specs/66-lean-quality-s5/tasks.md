# Tasks — #66 S5

## Exact inversions

- [x] T501 Hold every bound event inversion to its converse, selected from the discovered extent (tightness-over-discovered-extent, every-inversion-exact)
- [x] T502 State every live guard conjunct in the inversion conclusions, including `stalled` for pledge, accept-pledge and close-purchase; grant and deny held to the same converse (live-guard-conjuncts-stated)
- [x] T503 Extend the negative control to a non-exact conclusion on a constructor outside the former hand list (tightness-negative-control)
- [x] T504 Re-derive the mutant ledger on the changed statements (ledger-current)

## Observed refusals

- [ ] T511 A statement observing each of the six surviving refusal constructors, or a recorded reason it is unobservable (refusal-observed, ledger-current)

## Question retention

- [ ] T521 Open questions retained outside the proposer-departure closures (question-retained, ledger-current)

## Statement triage

- [ ] T531 Census of `unconstrained` rows at slice start, one row per theorem
- [ ] T532 Each row strengthened or its reason recorded (unconstrained-triaged, ledger-current)

## Mirror auxiliaries

- [ ] T541 Mirror checker excludes compiler auxiliaries by kind, with its negative control (mirror-auxiliaries-by-kind)
