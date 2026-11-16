# Campaign ledger — #57 S57-A

Gate base: `bb3ac41a1456c50b1bba7dafd522c174461b42ea`.
Audit build budget: `builds_spent=8`, `builds_budget=20`.
Commit-owner builds are outside this audit budget; every auditor receipt names
Lean 4.25.0.

| Row | Severity | State | Required killing evidence |
|---|---|---|---|
| INV-57-BOUNDARY | BLOCKING | KILLED | effect/sweep bypass mutant; six-arm checker; F-001 guard RED→GREEN |
| INV-57-NOOP | BLOCKING | KILLED | arbitrary stale-state rejection identity; sweep discriminator |
| INV-57-AUTH | BLOCKING | KILLED | all six current events plus stranger/removeMember production trace |
| INV-57-EXHAUSTIVE | BLOCKING | KILLED | new constructor/effect rejected at authorization boundary; no wildcard |
| INV-57-NOEXPIRY | ADVISORY-BUT-REQUIRED | KILLED | preserving admission plus three semantic discrimination witnesses |
| INV-54-PARTITION | BLOCKING | KILLED | fresh silent-deletion mutant |
| INV-54-DISJOINT | BLOCKING | KILLED | fresh both-tallies mutant |
| INV-54-NOSTALE | BLOCKING | KILLED | fresh non-ballot sweep omission mutant |
| INV-54-FRANCHISE | BLOCKING | KILLED | fresh unfranchised recast mutant; admitted-event proof dependency |
| INV-54-POLICYFREE | BLOCKING | KILLED | fresh hard-coded threshold mutant |

No inherited row is terminal under #57. `RESIDUAL` is forbidden for blocking
rows. INV-57-NOEXPIRY must close; the issue permits an explicit advisory
residual only if a concrete obstruction is reported and accepted before audit.

Campaign closed at the final submission set-point:

- submission-1 report `6a4985eeb95c440dfbf891c86bb49dce06ee928aeacd636141c755deb4e813ba`
  found F-001 and closed 9/10 rows;
- gate-v3 rejected-candidate receipt
  `0fa6d82c5f30613314e0099f220f5aa9c2d3576953e6df8d23448d3c083810b0`
  made F-001 permanently discriminating;
- submission-2 report `c3d54428eab8ad2e6b6a85f7e0feb2a19620a25c96fd31efc7ba6fef6981e3dd`
  passed all 10 rows with no residuals;
- final-commit gate receipt
  `5d2bae3c5ae6ebe9bfde022e8ca9878663842e9a8bcf65f7a56adb6cb19ddcc5`
  passed focused proofs, three positive instruments, six named mutants, and
  full repository CI under Lean 4.25.0.

Terminal state: `CLOSED`, `stopped=SET-POINT`, `KILLED=10`, `RESIDUAL=0`,
`BLOCKED=0`, `OPEN=0`, `builds_spent=8/20`.
