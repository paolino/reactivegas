# Campaign ledger — reactivegas #54 slice 1, auditor s2

builds_budget: 12
builds_spent: 7
s1_builds_spent: 5
stopped: none
state: open

| ID | Severity | Verdict | Row state | Killer / note |
|---|---|---|---|---|
| R-1 | BLOCKING | PASS | OPEN | carried s1; root + imports |
| R-2 | BLOCKING | PASS | KILLED | carried s1; G1b injected import |
| R-2b | BLOCKING | PASS | KILLED | carried s1; tracked checker |
| R-3 | BLOCKING | PASS | OPEN | carried s1; legal-direction 0 |
| R-4 | BLOCKING | PASS | KILLED | carried s1; G2b type error |
| R-5 | BLOCKING | PASS | OPEN | carried s1 |
| R-6 | BLOCKING | PASS | OPEN | carried s1 |
| R-7 | BLOCKING | PASS | OPEN | carried s1 |
| R-8 | BLOCKING | PASS | OPEN | repair: `finishEnact` still only from `tryEnactDetailed` then-branch |
| R-9 | BLOCKING | PASS | OPEN | carried s1 |
| R-10 | BLOCKING | PASS | OPEN | carried s1 |
| R-11 | BLOCKING | PASS | KILLED | M1 `majority (finishEnact …)`; shipped admin-intro guard false; old fold tests true |
| R-12 | BLOCKING | PASS | OPEN | carried s1 |
| R-13 | BLOCKING | PASS | OPEN | carried s1 |
| R-14 | BLOCKING | PASS | OPEN | carried s1; VI-8 leftover pending relies on this |
| R-15 | BLOCKING | PASS | OPEN | no-op payloads still guarded; recording of those paths added |
| R-16 | BLOCKING | PASS | OPEN | carried s1 |
| R-17 | BLOCKING | PASS | OPEN | carried s1; pins pre-state `appFold` |
| R-18 | BLOCKING | PASS | OPEN | carried s1 |
| R-19 | BLOCKING | PASS | OPEN | carried s1 |
| R-20 | BLOCKING | PASS | OPEN | carried s1 |
| R-21 | BLOCKING | PASS | OPEN | carried s1 |
| R-22 | BLOCKING | PASS | OPEN | carried s1 |
| R-23 | BLOCKING | PASS | OPEN | carried s1 |
| R-24 | BLOCKING | PASS | OPEN | carried s1 |
| R-25 | BLOCKING | PASS | OPEN | G5 now 30 lean: anchors (was 29) |
| R-26 | BLOCKING | PASS | OPEN | carried s1 |
| R-27 | BLOCKING | PASS | OPEN | carried s1 |
| R-28 | BLOCKING | PASS | OPEN | `enact_implies_threshold_met` now `[propext]`; still ⊆ allowed set |
| R-29 | BLOCKING | PASS | KILLED | carried s1; G4 now 40 Tests.lean guards |
| VI-1 | BLOCKING | PASS | OPEN | carried s1 |
| VI-2 | BLOCKING | PASS | OPEN | carried s1 |
| VI-3 | BLOCKING | PASS | KILLED | production `applyEventDetailed`; M1-full `exact threshold` type-mismatch at :338 |
| VI-4 | BLOCKING | PASS | OPEN | still compiles; M3 dropped-recording leaves unsolved goals here |
| VI-5 | BLOCKING | PASS | OPEN | carried s1 |
| VI-6 | BLOCKING | PASS | OPEN | now a validated production fold; independent walker `vi6Reachable` |
| VI-7 | BLOCKING | PASS | KILLED | production fold + detailed step; leftover-literal mutant reddens shipped guard |
| EP-DENY | BLOCKING | PASS | OPEN | carried s1 |
| EP-DIGEST | BLOCKING | PASS | OPEN | carried s1 |
| EP-CESR | BLOCKING | PASS | OPEN | carried s1 |
| EP-LAST-ADMIN | BLOCKING | PASS | OPEN | carried s1; VI-8 refutation uses this gap |
| EP-ROLE-PRED | BLOCKING | PASS | OPEN | carried s1 |
| EP-REDUNDANT-LOOKUP | BLOCKING | PASS | OPEN | carried s1 |
