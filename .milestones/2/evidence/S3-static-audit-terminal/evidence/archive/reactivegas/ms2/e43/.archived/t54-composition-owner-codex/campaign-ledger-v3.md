# Slice-2 composition campaign ledger

Gate v3 SHA-256: `2e1aea23b1e4905a0c4a9a801537748dda0c768e91068e242caaa3743b514620`

| Row | Severity | Preflight | Candidate audit | Terminal rule |
|---|---|---|---|---|
| EVENT-EXHAUSTIVE | BLOCKING | KILLED | OPEN | KILLED only |
| VERDICT-EXHAUSTIVE | BLOCKING | KILLED | OPEN | KILLED only |
| R-2/R-3-LAYERING | BLOCKING | KILLED/GREEN | OPEN | reverse KILLED and legal GREEN |
| PRODUCTION-ENACTMENT | BLOCKING | KILLED | OPEN | KILLED only |
| PRODUCTION-VERDICT | BLOCKING | KILLED | OPEN | KILLED only |
| ROUTE-THRESHOLD | BLOCKING | KILLED | OPEN | KILLED only |
| DIRECT-NOT-VOTE-DERIVED | BLOCKING | KILLED | OPEN | KILLED only |
| FENCE-DEBT-PROOF-TRUST | BLOCKING | RED (production absent) | OPEN | GREEN only |
| FULL-CI | BLOCKING | baseline GREEN | OPEN | GREEN only |

Preflight builds: 7/7 spent. Auditor submission budget: 9 evidence builds,
covering one positive focused build, seven named mutation rows, and one full CI
build. No BLOCKING row may terminate RESIDUAL, BLOCKED, or OPEN. A second
submission receives a fresh 9-build budget only after ticket-owner-authorized
repair.
