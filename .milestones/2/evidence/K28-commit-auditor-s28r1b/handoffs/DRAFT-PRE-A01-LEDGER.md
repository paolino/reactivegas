# Requirement ledger — S28-R1 replacement preflight

Candidate: `3af3d065b7d0c54f03d89b8c05d8b8acd4a53db4`.
Accepted base: `368b596fef0b6d393c2ac7afc631d236c55d86d1`.
Builds 0/12; targeted 0/24. No row is closed by source inspection or inherited
owner evidence. CB-001 prevents admission to the full execution campaign.

| Row | Severity | Carried state | This run | Coverage / limit |
|---|---|---|---|---|
| R1 distinct state/event, signer, canonical views | BLOCKING | OPEN | BLOCKED | Relevant API/demo read for command fit; no exact-value trace executed. |
| R2 refusal before append and durable acceptance | BLOCKING | BLOCKED, F1 | BLOCKED | StoreProbe source read; no repaired-candidate concurrency or refusal run. |
| R3 sealed atomic base hook | BLOCKING | OPEN | BLOCKED | Hook signature and transition implementation read; no recording-hook trace executed. |
| R4 direct-only admission and non-insertion effects | BLOCKING | BLOCKED, F2 | BLOCKED | Prior effect instrument and report read; no candidate/shadow compilation or effect run. |
| R5 validate/fold and accepted lifecycle agreement | BLOCKING | OPEN | BLOCKED, CB-001 | Required empty-start fold cannot express founding-aware accepted lifecycle; exact entrypoint mapping unresolved before spend. |
| R6 integrated authority and complete log replay | BLOCKING | BLOCKED, F1 | BLOCKED | Existing replay test inspected for entrypoint context; no fresh replay or authority verification. |

Carried campaign remains OPEN: three OPEN rows, three BLOCKED rows, no accepted
residuals. This run has zero completed requirement judgments or fresh mutant
kills; its six BLOCKED statuses describe this run's coverage, not a rewriting
of the carried ledger or six product findings.

| Reliance | Declared severity | This run | Named limit |
|---|---|---|---|
| HISTORICAL-FOLD | ADVISORY | BLOCKED | Historical suites and full historical-diff review not executed/completed; beyond-suites UNJUDGED. |
| CESR-KEY-VALIDITY | BLOCKING | BLOCKED | Validator source read for guard context only; key tests not rerun, decoder domain UNJUDGED. |
| STORE-STM-DISCIPLINE | BLOCKING | BLOCKED | F1 not rechecked; eight-pair/control/stress campaign unexecuted. |
| MAJORITY-FRANCHISE | BLOCKING | BLOCKED | MAJ-C and pending-entry test not executed; no current-franchise or enactment verdict. |
| HISTORICAL-APPFOLD-SHAPE | ADVISORY | BLOCKED | No fresh compilation; semantics UNJUDGED. |

No OPEN requirement becomes a residual. No owner gate, old kill, or source
search supplies acceptance. No new product invariant or unrelated finding is
ratified. Evidence identities and receipt details are in EVIDENCE-INVENTORY
and evidence/input-manifest.json.
