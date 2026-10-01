# Functions model — #66 S3

Only new entry points visible to CI and to the auditor. Internal helpers are
the commit owner's.

| id | name | arguments | result / effect |
|---|---|---|---|
| F-1 | `just lean-mutants` | none | runs F-2 then F-3 on the checked-out head; exit 0 iff R-1…R-13 hold; prints one summary line `LEAN-MUTANTS discovered=<n> excluded=<n> helper=<n> required=<n> executed=<n> killed=<n> open=<n> survived=<n>` |
| F-2 | runner, normal mode | `repoRoot` | executes every mutant (E-2) in isolation, validates E-3 and E-4, re-renders M-5 and compares with the committed files, verifies the tree is restored |
| F-3 | runner, negative-control mode | `repoRoot` | executes each R-10 control and requires each to fail with its own named rule; exit 0 iff every control failed for its own reason |

Constraint: F-2 and F-3 write only under a temporary directory or `lean/.lake`;
tracked files are byte-identical after either.
