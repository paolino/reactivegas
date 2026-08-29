# Tasks — #57 structural vote validation

Artifact ceiling: 70 lines / 5 KiB. Only the ticket owner checks behavior-task
boxes, and only after a fresh independent audit passes the exact candidate.

## Planning and gate

- [ ] **T5700** Freeze the compact #57 mandate, exact source fence, gate
      contract, inherited evidence hashes, and Lean 4.25.0 baseline.

## Slice S57-A — structural validation

- [ ] **T5710** Put effect and sweep behind the single production validation
      boundary; rejection is exact arbitrary-state identity. (R57-01, R57-03)
- [ ] **T5711** Make signer authorization total and exhaustive over the current
      `VoteEvent` surface, with only the existing bootstrap admission
      capability before a franchise exists. (R57-02, R57-04)
- [ ] **T5712** Prove universal `inadmissible_is_noop` and
      `nonresponsabile_event_noop`; exercise all six constructors and all three
      member/role events. (R57-03, R57-04, R57-05)
- [ ] **T5713** Replace the no-expiry constructor whitelist with the semantic
      preservation relation and cover the retained preserving member event.
      (R57-07)
- [ ] **T5714** Freshly re-demonstrate PARTITION, DISJOINT, NOSTALE, FRANCHISE,
      and POLICYFREE with their inherited frozen instruments or exact
      hash-bound equivalents. (R57-06)
- [ ] **T5715** Pass the exhaustive-surface and seeded-bypass controls, print
      clean axiom sets, preserve frozen blobs/dependency direction, and pass
      focused build plus full repository CI under Lean 4.25.0. (R57-08…R57-10)
