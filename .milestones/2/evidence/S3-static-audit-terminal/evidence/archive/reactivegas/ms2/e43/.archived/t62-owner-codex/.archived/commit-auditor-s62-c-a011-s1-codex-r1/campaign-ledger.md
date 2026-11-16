# S62-C A011 submission-1 audit campaign ledger

- Candidate: `b90161cffb478db0855e81e0bc3ab23818bba161`
- Source ledger: `/tmp/reactivegas/ms2/e43/t62-owner-codex/campaign-ledger-s62-c-a011.md`
- Source ledger SHA-256: `ec8389404c5a32fb0401f2f5c7756f8f828e1dec44d5aacafbc46b884f050034`
- Severity: all seven rows `BLOCKING`; `RESIDUAL` forbidden
- Builds after audit: `15/40`; this audit `3/3`

| Row | Verdict | Terminal state | Bound evidence |
| --- | --- | --- | --- |
| `G62-C-THEOREMS` | PASS | `KILLED` (inherited) | Historical declaration remains `ab9b4aadb52fbbcdb62bb8de39f62acbc76f0ffbfa4c8eeb5d1d79f6fff334f4`; fresh full ticket receipt `d369103c357b5a3f4f3d49107bccf8e676ccde832ea893dbd5aaccf790b92b1b`. |
| `G62-C-ECONOMY` | PASS | `KILLED` (inherited) | Non-degenerate canonical economy proof carried at `76c32a87d1099fe0d3fb3cbae84fa9e408afaa6e9d454a433e4e6c02999b5b21`; fresh full ticket receipt passes. |
| `G62-C-EXHAUSTIVE` | PASS | `KILLED` (inherited) | Constructor seeds execute in fresh full ticket receipt `d369103c357b5a3f4f3d49107bccf8e676ccde832ea893dbd5aaccf790b92b1b`. |
| `G62-C-TRUST-CI` | PASS | `KILLED` (inherited) | Released toolchain blobs match; fresh mismatch control, dependency direction, zero escape hatches, and allowed-axiom build pass in `d369103c357b5a3f4f3d49107bccf8e676ccde832ea893dbd5aaccf790b92b1b`. |
| `I57-01-BOUNDARY` | FAIL | `BLOCKED` | Production currently has one checked decision and the bypass mutant is killed, but the reached duplicate produces the same state while `checkVoteApplyDuplicateCaught` remains true behind disconnected `checked_decisions := 1`; focused evidence `d53064ca7972a0a747e1da6c85671573d90becfb206fe079345b639751ba0f04`. |
| `G62-C-INHERITED57` | PASS | `KILLED` | Production-rooted non-degenerate franchise cast-admission and caller-threshold controls both distinguish their targeted mutants; DISJOINT remains inherited killed. Focused evidence `d53064ca7972a0a747e1da6c85671573d90becfb206fe079345b639751ba0f04`. |
| `G62-C-TRACE` | FAIL | `BLOCKED` | No executable JSON decode/replay exists; omitted-state emitter, corrupted member-cleanup value, and degenerate historical pending-store coordinate survive the relevant shipped acceptance/value oracles. Focused evidence `d53064ca7972a0a747e1da6c85671573d90becfb206fe079345b639751ba0f04`. |

Campaign is `CLOSED`, stopped by `SET-POINT`: `rows=7 killed=5 residual=0 blocked=2 open=0`.
