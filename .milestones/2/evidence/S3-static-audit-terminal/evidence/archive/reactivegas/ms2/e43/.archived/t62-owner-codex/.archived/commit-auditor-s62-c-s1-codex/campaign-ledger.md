# S62-C submission 1 audit campaign ledger

Candidate: `2f2a327f4e1dafa27216aeabe1d36095ea25bcd8`
Submission: `1/2` (full scope)
Auditor build allowance: at most `3` substantive build/elaboration runs.
Ticket S62-C cumulative ceiling: owner spent `28/40`; auditor runs count upward from 28 and must not exceed 40.
Ceiling raises: `1/2` ticket-wide; audit-specific raises `0/2`.

All rows below are BLOCKING. A row may close only as `KILLED` or `BLOCKED`, never `RESIDUAL`.

| Row | Severity | State | Independent audit evidence |
| --- | --- | --- | --- |
| G62-C-THEOREMS | BLOCKING | KILLED | Historical block independently equals accepted `ab9b4...`; focused proof prints only allowed axioms; omission/source controls are live. `evidence/static-trust-and-history-v2.log` sha256 `968fc50f...`, `evidence/fresh-focused-proof.log` sha256 `76c32a87...`. |
| G62-C-ECONOMY | BLOCKING | KILLED | Explicit caller auth reaches production; canonical two-member/ghost-value fixture distinguishes payload-local membership and the proved check elaborates. `evidence/fresh-focused-proof.log` sha256 `76c32a87...`. |
| G62-C-TRACE | BLOCKING | BLOCKED | Candidate has no integrated JSON type/emitter/replayer; `replayIntegratedCorpus` is length-only and its events are independently mapped from one initial state. F-TRACE. `evidence/semantic-findings.log` sha256 `9a4ae25d...`, frozen instrument sha256 `6dee1ad1...`. |
| G62-C-EXHAUSTIVE | BLOCKING | KILLED | App/direct/proposal/base-change/vote/verdict eliminations are closed and wildcard-free; fresh constructor selftest and full ticket gate pass. `evidence/fresh-full-ticket-gate.log` sha256 `28d6a8fb...`. |
| G62-C-INHERITED57 | BLOCKING | BLOCKED | F-I57-ONE-DECISION: admitted vote route validates twice. F-I57-INTEGRATED-LEGS: DISJOINT, FRANCHISE, POLICYFREE checks do not reach the integrated transition and are insensitive to path mutations. F-I57-TOOLCHAIN: pin `4.27.0` differs from executing Lean `4.25.0`. `evidence/semantic-findings.log` sha256 `9a4ae25d...`, frozen instrument sha256 `6dee1ad1...`, `evidence/toolchain-pin-compare.log` sha256 `b018190f...`. |
| G62-C-TRUST-CI | BLOCKING | BLOCKED | Escape-hatch, axiom, history, source, direction, and CI checks pass, but the required toolchain identity does not: pin `4.27.0`, executing Lean `4.25.0`; the shipped Bool checks unrelated domain values. F-I57-TOOLCHAIN. `evidence/toolchain-pin-compare.log` sha256 `b018190f...`. |

The #47 BackdonateAuth policy truth value is advisory and unresolved; the blocking obligation is only that production receives an explicit caller-supplied value with no default or invented policy.

Campaign state: `CLOSED`, stopped by `SET-POINT`; `killed=3 residual=0 blocked=3 open=0`.
Blocking rows carry exact candidate facts and must be resolved by the one permitted repair submission; none is residualized.
