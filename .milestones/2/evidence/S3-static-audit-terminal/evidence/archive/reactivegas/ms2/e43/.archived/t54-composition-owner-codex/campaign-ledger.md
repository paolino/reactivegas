# Slice-2 composition campaign ledger

Gate v4 SHA-256: `9f690b6cfb2464e9e3af6e10e62d89337ae04bda9e48327134bd5251b72926cc`

Fresh campaign after v3 audit disposition `CHALLENGED`. The v3 campaign is
preserved in `campaign-ledger-v3.md`; no candidate repair submission was
consumed because its two open rows were ticket-owner gate/budget defects.

| Row | Severity | Preflight | Candidate audit | Terminal rule |
|---|---|---|---|---|
| EVENT-EXHAUSTIVE | BLOCKING | KILLED | KILLED | KILLED only |
| VERDICT-EXHAUSTIVE | BLOCKING | KILLED | KILLED | KILLED only |
| R-2/R-3-LAYERING | BLOCKING | KILLED/GREEN | KILLED/GREEN | reverse KILLED and legal GREEN |
| PRODUCTION-ENACTMENT | BLOCKING | KILLED | KILLED | KILLED only |
| PRODUCTION-VERDICT | BLOCKING | KILLED | KILLED | KILLED only |
| ROUTE-THRESHOLD | BLOCKING | KILLED | KILLED | KILLED only |
| DIRECT-NOT-VOTE-DERIVED | BLOCKING | KILLED | KILLED | KILLED only |
| FENCE-DEBT-PROOF-TRUST | BLOCKING | RED (production absent) | GREEN | GREEN only |
| FULL-CI | BLOCKING | baseline GREEN | GREEN | GREEN only |

Preflight builds: 7/7 spent. Fresh auditor campaign budget: 9 evidence builds,
covering one cold positive build, seven named mutation rows, and one full CI
build. No BLOCKING row may terminate RESIDUAL, BLOCKED, or OPEN. The accepted
v3 semantic mutation evidence is a read-only seed, not a substitute for the
fresh auditor's own terminal matrix.

Campaign terminal: `AUDIT-PASS`, report SHA-256
`9fa2754e826847fa6e181cf724d23627307b150e6f07a8cffbbe4510f074ddef`.
Rows 9/9 terminal: killed 7, green 2, residual 0, blocked 0, open 0.
Auditor builds 8/9; the reserve remained unused. Final squashed commit
`c8c4dd8903cca817c814e9f84e9ff21ceba2de0c` reran positive and full CI GREEN.
