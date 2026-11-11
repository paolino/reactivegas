# Campaign ledger — simulator batch 1, submission 1

Auditor: grok-4.6 pane %190. Ticket builds before this audit: 0/3.
This audit spent 1 building audit (`nix develop … lake build` + warm reuse). Stopped=SET-POINT.

| Invariant | Severity | Row state | Killing mutant / negative control | Evidence | Notes |
|---|---|---|---|---|---|
| INV-PROVENANCE | BLOCKING | KILLED | extra-path filter (8 allowed names only; empty remainder) | `git diff --name-status d44d353..a9c9462`; porcelain empty before/after | base→610f7d9→a9c9462; tree 1f2ac82 |
| INV-PIN-STABILITY | BLOCKING | KILLED | production SHA grep: no `fcd4dc3` / no branch fallback | pin-failures + claim-prod | old SHA fixture-only in claim-gate selftest |
| INV-PIN-FAILURES | BLOCKING | KILLED | unresolvable `f{40}`; moved reachable `ea15d8d` tree mismatch; orphan `fcd4dc3` | instruments/pin-failures.mjs sha256=7ab7e89a…; instrument-pin.log sha256=c5d11b81… | three distinct reasons |
| INV-CITATION-BOUNDARY | BLOCKING | KILLED | pin-status flip `voteDerived_iff_not_direct` provato→enunciato | claim-selftest.log sha256=635486e4… | join-vote-econ remains NON PROVATO; witness is definizione |
| INV-CLAIM-COVERAGE | BLOCKING | KILLED | `backdonate: []`; donate route drift | claim-selftest; mutants.mjs sha256=ee97aa78… | 18/18 constructors |
| INV-GOVERNANCE-MODEL | BLOCKING | KILLED | noop `verifyGovernedSeq`; live `runAttempt` without credit | operator-causality.mjs sha256=7587dc7f…; playwright selftest | refusal before `attempt()` |
| INV-OPERATOR-SCENARIO | BLOCKING | KILLED | 01 empty kel/base; match `senza enactment fedele del canale base` | scenario-prod.log sha256=3ee034c6…; operator-causality | 4 addUser + 4 elect; economic machine still applies |
| INV-RED-GREEN-CAUSALITY | BLOCKING | KILLED | `RG_SCENARIO_GOVERNANCE=off`; noop-gov mutant ACCEPTs 01 | instrument-operator.log sha256=f889dcee… | scenario still expects refused |
| INV-ONE-CORE | BLOCKING | KILLED | forked `@@CORE:machine@@` slice | mutants.mjs; build-check.log sha256=be565467… | 13 slices byte-identical |
| INV-SCENARIO-SUITE | BLOCKING | KILLED | empty dir; non-v1; omitted asserts; truncated seq; mutated poststate; comune in `initState` | scenario-selftest.log sha256=dd07f595…; mutants | no skip-flag exists; empty covered |
| INV-PUBLISHABLE-BOUNDARY | BLOCKING | KILLED | live `?selftest=1` at 1280 and 390 | playwright-selftest.json sha256=2fae6c37… | 1 document request each; 0 console/page errors; no overflow |
| INV-GATE-WIRING | BLOCKING | KILLED | frozen gate-v3 exit 0; cold-cache trace RED then warm GREEN | gate-v3.log sha256=362626af…; trace-prod.log (cold miss) | lake + claim/trace/vote/scenario prod+selftest |
