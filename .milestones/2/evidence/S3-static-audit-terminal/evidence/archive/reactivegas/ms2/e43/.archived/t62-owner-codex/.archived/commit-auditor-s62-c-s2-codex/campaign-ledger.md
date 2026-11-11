# S62-C submission 2 carried audit campaign ledger

Candidate: `7c2379d52798aeccf64149264d912a33e8978431`
Submission: `2/2` (FINAL, repair-scoped)
Rejected candidate before rebase: `2f2a327f4e1dafa27216aeabe1d36095ea25bcd8`
Rejected candidate after exact release rebase: `855219762c623dc75d1d0bd6f4a73310ed813b82`
Pre-integration repaired candidate: `ae1a2700822d1b522dc282d61665b69c29553179`
Released dependency: `d7a3e05116f40920f3d78daf3e1818ad17c74a74`

Ticket cumulative build position at dispatch: `34/40`. This auditor may use at
most `3` substantive builds and must not exceed `37/40`. Ceiling raises remain
`1/2` ticket-wide and `0/2` audit-specific.

| Row | Severity | Carried state | Submission-2 active scope |
| --- | --- | --- | --- |
| `G62-C-THEOREMS` | BLOCKING | `KILLED` | Carried terminal; do not reopen. |
| `G62-C-ECONOMY` | BLOCKING | `KILLED` | Carried terminal; do not reopen. |
| `G62-C-TRACE` | BLOCKING | `BLOCKED` | Active: F-TRACE, serialized sequential integrated replay and four named mutants. |
| `G62-C-EXHAUSTIVE` | BLOCKING | `KILLED` | Carried terminal; do not reopen. |
| `G62-C-INHERITED57` | BLOCKING | `BLOCKED` | Active: F-I57-ONE-DECISION, F-I57-INTEGRATED-LEGS, and I57-10 live toolchain boundary. |
| `G62-C-TRUST-CI` | BLOCKING | `BLOCKED` | Active: F-I57-TOOLCHAIN; running toolchain must equal pin and mismatch control must fail. |

No row may become a residual. Submission 2 is final. Update this ledger with
fresh independent evidence, exact terminal states, and the campaign stop
condition.

## Submission-2 final settlement

| Row | Final state | Independent settlement |
| --- | --- | --- |
| `G62-C-THEOREMS` | `KILLED` | Carried terminal without reopening; historical bytes and full gate remain exact. |
| `G62-C-ECONOMY` | `KILLED` | Carried terminal without reopening; repair did not touch the row boundary. |
| `G62-C-TRACE` | `BLOCKED` | The emitter omits stored fields, replay consumes in-memory steps rather than serialized data, and pending/economic/vote state is not compared. |
| `G62-C-EXHAUSTIVE` | `KILLED` | Carried terminal without reopening; seeded constructor controls remain green. |
| `G62-C-INHERITED57` | `BLOCKED` | Production witnesses now reach the integrated fold, but I57-01 duplicate, I57-06-FRANCHISE cast-admission, and I57-06-POLICYFREE threshold-threading mutants are not discriminated. |
| `G62-C-TRUST-CI` | `KILLED` | Live pin/runtime comparator reports `4.25.0 = 4.25.0`; its mutated-pin negative control fails and local/CI wiring executes it. |

Touched inherited legs: `I57-06-DISJOINT=KILLED`,
`I57-10-TOOLCHAIN=KILLED`, `I57-01-BOUNDARY=BLOCKED`,
`I57-06-FRANCHISE=BLOCKED`, and `I57-06-POLICYFREE=BLOCKED`. All other
inherited #57 subleg states are preserved from submission 1.

Campaign `CLOSED`, stopped by `SET-POINT`: rows `6`, killed `4`, residual `0`,
blocked `2`, open `0`. Builds `37/40` ticket-wide, `3/3` in this audit
(`cold,warm,warm`). Primary scoped evidence:
`evidence/repair-instrument-candidate-v2.log` sha256
`0853cf233302166be3e7133c0097f4a26a4c09f2a172617351d8c23f4e340f88` and
`evidence/toolchain-preflight.log` sha256
`b6117b60dfe019f8dd528dbcfd77de5b2c0fa23135c0c3af37bb1ee4096d5d15`.
