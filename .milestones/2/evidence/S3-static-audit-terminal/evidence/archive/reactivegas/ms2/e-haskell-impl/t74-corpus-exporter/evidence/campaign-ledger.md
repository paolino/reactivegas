# Mutation campaign ledger — `reactivegas#74` slice S74

Ticket-wide ledger, carried across submissions by successive auditors.
Severity fixed by the ticket owner at spec time (undeclared → BLOCKING).
A BLOCKING row terminates only as KILLED or BLOCKED, never RESIDUAL.

Build budget: 3 building audits ticket-wide (~9.3 GiB peak). Spent: 0.
Ceiling raises: 0/2.

| Invariant | Severity | State | Killing mutant / evidence | Owner |
|---|---|---|---|---|
| G74-CALLS-EXISTING — exporter calls `seedCorpus`/`emitIntegratedCorpus`, no restatement; `lean_exe` present | BLOCKING (second corpus = oracle-integrity basis for money-path conformance) | OPEN | — | audit-s1 |
| G74-ENVELOPE-CLOSED — `Trace` untouched; wrapper is view/auth (+initial for integrated) and nothing else | BLOCKING (unreplayable oracle = no conformance) | OPEN | — | audit-s1 |
| G74-VERIFY-FAILS-CLOSED — export+verify pass clean; one-byte mutation and manifest corruption each force non-zero with byte-identical restore | BLOCKING (blind drift detector arbitrates Lean vs Haskell) | OPEN | — | audit-s1 |
| G74-RECORD-HONEST — `docs/` byte-identical to base; coverage evidence in owner root | ADVISORY (prose routing; content judged by auditor+T.O.) | OPEN | — | audit-s1 |
| G74-ADDITIVE-ONLY — no model/theorem/guard/step/`Trace`/content change; fence paths untouched | BLOCKING (model integrity) | OPEN | — | audit-s1 |
