# #62 mutation campaign ledger

Build budget counts substantive auditor builds only: `builds_spent=6`,
`builds_budget=6` across submission 1 and final submission 2. Ceiling raises:
`0/2` for audit lanes.

| Row | Severity | State | Evidence / residual |
| --- | --- | --- | --- |
| INV-62-ONE-STORE | BLOCKING | KILLED | Final fresh audit report `3a7b355a260b`: sole `GroupState.members`, membership-free app/vote payloads; frozen duplicate-field seeds for `users`, `responsabili`, and vote `members` all RED; gate `71c8588c120d`. |
| INV-62-PAYLOAD-ONLY | BLOCKING | KILLED | Actual member-writing transition mutant false-preservation claim RED `060790281562`; independent payload/member/vote carry probe GREEN `7dbd79ca45d3`; production result remains payload-only. |
| INV-62-ONE-KEY | BLOCKING | KILLED | Submission-1 audit exposed production `comuneId` authorization; final independent probe requires valid non-comune success plus guarded boot/root comune refusal, GREEN `7dbd79ca45d3`; no identity bridge remains. |
| INV-62-HISTORICAL | BLOCKING | OPEN | S62-A byte boundary independently PASS: exact declaration hashes `ab9b4aadb52f` and production call graph is isolated. Mutation-terminal integrated-theorem evidence remains intentionally deferred to `G62-C-THEOREMS`. |
| INV-62-DIRECT-ONLY | BLOCKING | OPEN | pending S62-B |
| INV-62-ATOMIC-HOOK | BLOCKING | OPEN | pending S62-B |
| INV-62-V3-BASE | BLOCKING | OPEN | pending S62-B |
| INV-62-CLOSED-SUMS | BLOCKING | OPEN | pending S62-B/S62-C |
| INV-62-PROOF-TRUST | BLOCKING | OPEN | pending S62-C |

Rows are terminal only when a fresh auditor binds a demonstrated killing
mutant to the candidate. No blocking row may become a residual.

## Commit-owner lane history

- Attempt 1: GLM pane `%201`, rotated write-clean after intake/RED receipt but
  before a durable RED event or tracked edit; no build-audit budget consumed.
- Attempt 2: Claude Opus pane `%204`, active replacement owner for S62-A.
- Attempt 2 outcome: clean capacity handoff at `834c123`; KelGroups integration
  and Vote invariants complete, candidate not compiling, no audit consumed.
- Attempt 3: Grok pane `%206`, active successor owner for the S62-A mechanical
  remainder under ceiling raise 1/2.
- Attempt 3 submission 1: candidate `6fa3ca7`, fresh Codex audit report
  `047dae3ed0af`, rejected with F-01…F-04; one final repair bounce consumed.
- Attempt 3 submission 2 FINAL: candidate `000ff76`, tree `1cd78049`, fresh
  Codex audit report `3a7b355a260b`, verdict PASS with 8/8 rows and 0 findings.

## S62-C submission 1

- Candidate: `2f2a327f4e1dafa27216aeabe1d36095ea25bcd8`.
- Fresh Codex auditor report: `fc255066cc66e118d1d456f3549d7d3573704e53e46396254bb0fb0e8a9e4d68`.
- Campaign: `CLOSED` at set point; `killed=3`, `blocked=3`, `residual=0`, `open=0`.
- Blocking findings: `I57-01-BOUNDARY` (duplicate production validation),
  `G62-C-TRACE` (no sequential serialized integrated replay),
  `G62-C-INHERITED57` (DISJOINT/FRANCHISE/POLICYFREE do not exercise the
  integrated path), and `G62-C-TRUST-CI` (tracked Lean 4.27.0 pin differs
  from executing Lean 4.25.0).
- Ticket cumulative build accounting: owner submitted at `28/40`; auditor
  consumed `3/3`, ending at `31/40`; exactly `9` substantive builds remain.
- One in-scope owner repair submission is authorized for the first three
  findings only. The toolchain finding is outside the #62 writable fence and
  holds submission-2 audit until a separate milestone-owned contract repair.

## S62-C submission 2 preflight

- Owner repair candidate before dependency integration:
  `ae1a2700822d1b522dc282d61665b69c29553179`, receipt
  `dabd12ed28d7ebe452417b70c12ced0d798ef26d2a08265087cf3bd5132e2efe`.
- Released toolchain merge: `d7a3e05116f40920f3d78daf3e1818ad17c74a74`.
- Rebased combined candidate: `7c2379d52798aeccf64149264d912a33e8978431`,
  tree `3f58fc04cacc4421d8a3e0271a9bbbd516bdd6f4`.
- The complete #62 patch is byte-identical before and after integration:
  sha256 `22ebc8d37070104df58a18fa96fbee293a9d72b2556b4902d9961fe0a51acb49`.
- Carried terminal rows: `G62-C-THEOREMS`, `G62-C-ECONOMY`, and
  `G62-C-EXHAUSTIVE` remain `KILLED` and are not reopened.
- Final audit active rows/findings: `G62-C-TRACE` / F-TRACE,
  `G62-C-INHERITED57` / F-I57-ONE-DECISION + F-I57-INTEGRATED-LEGS +
  I57-10, and `G62-C-TRUST-CI` / F-I57-TOOLCHAIN. All remain BLOCKING until
  the fresh submission-2 auditor demonstrates the shipped properties and
  negative controls.
- Cumulative builds at dispatch preflight: `34/40`; auditor allowance `3`.

## Prospective seat policy (NOTE-017)

Effective from NOTE-017, future commit-owner dispatch preference is GLM first,
then Codex, then Claude only when alternation forces it, with Grok last and a
durable rationale required for why neither GLM nor Codex fits. A GLM
provider-intake failure is escalated immediately to the machine owner and falls
back to Codex, never Grok. Auditors prefer Codex, then Claude, and are never
GLM. GLM remains barred from production secrets and every GLM candidate
requires a fresh non-GLM auditor, Codex first.

Cumulative authoritative START accounting from the ticket ledger is:

- Grok: `4` (`%206`, `%227`, `%230`, `%232`);
- GLM: `1` (`%201`);
- Codex: `6` (ticket owner `%195`; auditors `%209`, `%214`, `%231`, `%233`,
  `%240`);
- Claude: `3` (`%204`, `%220`, `%222`);
- GLM provider-intake attempts without START: `1` (`%229`).

S62-A/B acceptance is unchanged. Existing Grok owner `%232` and the terminal
Codex audit `%240` are not rotated or revoked. The epic one-shot GLM allocation
remains `AVAILABLE+UNASSIGNED`; NOTE-017 starts no process. Every ticket-level
terminal receipt written after NOTE-017 carries the complete counts above.
