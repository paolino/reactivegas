# S62-C A013 campaign ledger — initial

- Base: `6a56f90115e7624830da769da55a1ce1a3c5f5e3`
- Source A011 final ledger SHA-256:
  `5423fa5664733442eb01e6303d1c3ee80a32e0c238d1987122daec1260462ade`
- Rows: 7 blocking; residual forbidden
- Initial state: killed 6, open 1, blocked 0, residual 0
- Builds carried: `23/40`; ceiling raises `0/2`

| Row | Initial state | A013 obligation |
| --- | --- | --- |
| `G62-C-THEOREMS` | `KILLED` inherited | Carry exact production blobs; do not reopen. |
| `G62-C-ECONOMY` | `KILLED` inherited | Carry exact production blobs; do not reopen. |
| `G62-C-EXHAUSTIVE` | `KILLED` inherited | Carry exact production blobs; do not reopen. |
| `G62-C-TRUST-CI` | `KILLED` inherited | Carry exact trust boundary; re-run shipped CI. |
| `I57-01-BOUNDARY` | `KILLED` inherited | Carry exact one-decision/bypass/duplicate control; do not reopen. |
| `G62-C-INHERITED57` | `KILLED` inherited | Carry all inherited #57 rows including DISJOINT. |
| `G62-C-TRACE` | `OPEN` | Tracked CI executes corpus evaluator; targeted corpus mutant makes that same tracked CI RED; restored candidate GREEN. |

Stop only when all seven rows are `KILLED`, or with an exact blocking fact.
