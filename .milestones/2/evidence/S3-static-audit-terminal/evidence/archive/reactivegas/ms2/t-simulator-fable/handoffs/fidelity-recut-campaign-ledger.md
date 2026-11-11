# Fidelity re-cut campaign ledger

Campaign: NOTE-041 prefix-vacuity re-cut.

- exact_base: `b32ae15f2894f14daf6352be6b233254f823ce95`
- exact_base_tree: `1e893cd0d2b8d185723f9308b9a5e69db935cef6`
- builds_budget: `3`
- builds_spent: `3`
- submissions_max: `2`
- submissions_spent: `1`
- ceiling_raises: `0`
- terminal: `ACCEPTED`

Build 1 was spent by the Grok fallback commit owner after the preferred GLM
provider failed twice before START. Build 2 passed under the fresh Codex
auditor. Build 3 passed under the ticket owner for final acceptance. No fourth
build, third submission, second repair, or ceiling raise is authorized without
a new operator ruling.

| Row | Severity | State | Observable terminal proof |
|---|---|---|---|
| FID-PREFIX-COMPLETION | BLOCKING | KILLED | Candidate `5e3ebaa2`; owner, auditor, and ticket-owner frozen-v7 builds exit 0; audit report SHA-256 `a61d4b1d71bd58903abdf4c8ff65c48810a956dcb419f84067524d192032f0da`; final receipt SHA-256 `e136f75153f3c5e3199abc1bb3bba6e56c14c620c6d7c6b05b95bfe5b8390c62`; old prefix mutant reproduced and rejected by real v7 validator; restored environment flag full 14/14 |

The prior NOTE-038/039 campaign's eight rows are inherited as `KILLED`, not
reopened. The re-cut owner and auditor must prove their implementation paths
are unchanged and re-demonstrate the frozen v7 gate, but must not repeat broad
economic, governance, pin, or machine discovery.

The campaign ends only when FID-PREFIX-COMPLETION is `KILLED`, or the bounded
two-submission ladder ends with findings and another re-cut. No residual is
authorized for this blocking gate-integrity row.
