# Strip-only campaign ledger

Campaign: NOTE-032 + NOTE-033 under NOTE-036 lane-scoped RELEASE.

- exact_base: `a9c946208a07f839a1a5bd39d809433db63120e4`
- builds_budget: `3`
- builds_spent: `3`
- submissions_max: `2`
- submissions_spent: `2`
- ceiling_raises: `0`
- terminal: `PASS — SET-POINT`

2026-08-30T~07:55Z build 1/3 — commit-owner-fable-strips: full frozen gate v4
on GREEN candidate 16e4cc5a76d8a2f538a92f3ae923a9f9ec0627ee (RED ea6ac2e +
GREEN committed, teaching gate prod+selftest exit 0, tree clean); purpose:
the brief's step 5 pre-submission full verification.

Submission 1 candidate: `16e4cc5a76d8a2f538a92f3ae923a9f9ec0627ee`.
The ticket owner declared all seven strip-only rows `ADVISORY` before fresh
audit: they constrain teaching UX and proof presentation, not chain state,
money semantics, or signatures. Every row nevertheless must reach a terminal
state before acceptance; no residual is pre-authorized.

| Row | Severity | State | Evidence |
|---|---|---|---|
| STRIP-READER | ADVISORY | KILLED | submission-2 audit: arrival-register and two-cassiere identity mutants rejected |
| STRIP-PROOF-BOUNDARY | ADVISORY | KILLED | submission-2 audit: accepted claim arrays bound to rendered glyph data; drift rejected |
| STRIP-SHOW | ADVISORY | KILLED | always-visible `voto` mutant rejected at leave phase |
| STRIP-RETIRE | ADVISORY | KILLED | submission-2 audit: all five condition dismissals remain absent across reload/restore |
| STRIP-CASSA | ADVISORY | KILLED | suppressed negative-cassa strip mutant rejected |
| STRIP-LIFECYCLE | ADVISORY | KILLED | no-op condition dismissal mutant rejected across five rows |
| STRIP-SAFETY-UX | ADVISORY | KILLED | page console-error mutant rejected |

2026-08-30T~07:48Z build 2/3 — commit-auditor-codex-strips-s1: one fresh
full frozen gate v4 reproduction in detached worktree at candidate `16e4cc5`;
purpose: submission-1 independent verification. Pre-build free space:
`204178739200` bytes. The auditor must record cache temperature, duration,
exit, receipt hash, and post-build free space; it may not spend build 3.

Submission-1 audit terminal FINDINGS at candidate `16e4cc5`: report archived
at `.archived/commit-auditor-codex-strips-s1/handoffs/AUDIT-REPORT.md`,
SHA-256 `c8bd82f634a7748d4a597909da92f99543d2363d523560bc679e7a197b49d99f`.
Full gate exit 0 in 121669 ms, cache cold, receipt SHA-256
`a6ae01399b5421148c22d250977b133458cfe486e2f3e7bfaaa6065e13b91028`;
post-build free space `204096110592` bytes. Campaign remains OPEN: four rows
KILLED, three rows OPEN, four named proof findings. Build 3/3 is reserved for
the fresh submission-2 auditor; the owner repair must use focused tests only.

Submission 2 repaired candidate:
`7923e58a83e953c51193659a6f4d44fea9d76143`, tree
`05bf192f8f9a4361d8d06b3d4de9f67011180c50`; exact repair delta
`16e4cc5..7923e58`, teaching gate only. Repair receipt SHA-256
`875c783b6d1d4cb381ebb630778acd3cebf38a5040d5b78d26f21252d9408c7f`.

2026-08-30T~08:29Z build 3/3 — commit-auditor-codex-strips-s2: final fresh
full frozen gate v4 reproduction in detached worktree at repaired candidate
`7923e58`; purpose: submission-2 repair-scoped verification and terminal
campaign decision. No further build, submission, repair, auditor, or ceiling
raise is authorized in this ticket.

Submission-2 audit terminal PASS at candidate
`7923e58a83e953c51193659a6f4d44fea9d76143`: all seven rows KILLED,
residual `0`, campaign closed by SET-POINT. Focused production exit 0
(`4609f32d...`), 15-control selftest exit 0 (`1aa2e62f...`), and frozen
gate v4 exit 0 in 243003 ms (`092225fb...`). Report archived at
`.archived/commit-auditor-codex-strips-s2/handoffs/AUDIT-REPORT.md`, SHA-256
`d824145c88456a0eb40cf41cd7f768fb1375d5fffb15aeaeaee6c8243ffffa30`.
The optional fresh instrument's one Chromium profile-cleanup race is retained
as `OD-S2-001`, RECORDED, NOT-OPENED; it did not alter the required PASS
evidence or clean candidate tree.

Append-only spend log follows. A worker must record the purpose before each
expensive full-gate build and update the counters in the header in the same
edit. Focused browser/test iterations that do not invoke the full frozen gate
do not consume this build budget.
