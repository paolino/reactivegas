# Machine-fidelity campaign ledger

Campaign: NOTE-038 + NOTE-039 under the 2026-08-30 seat policy.

- exact_base: `7923e58a83e953c51193659a6f4d44fea9d76143`
- exact_base_tree: `05bf192f8f9a4361d8d06b3d4de9f67011180c50`
- builds_budget: `3`
- builds_spent: `3`
- submissions_max: `2`
- submissions_spent: `2`
- ceiling_raises: `0`
- terminal: `RECUT`

Build 1 was the commit owner's complete GREEN submission. Build 2 was the
fresh submission-1 audit. After that audit returned blocking findings, Build 3
is reserved for the fresh submission-2 repair auditor; the ticket owner will
accept or re-cut from that exact cold-cache receipt and hash-only/read-only
checks, without a fourth build. Record purpose, candidate, cache temperature,
duration, exit, receipt hash, and free space before and after each spend. No
fourth build, third submission, second repair, extra auditor, or ceiling raise
is authorized.

| Row | Severity | State | Observable terminal proof |
|---|---|---|---|
| FID-PIN-FRESH | BLOCKING | KILLED | Submission-2 occurrence-checked repair instrument killed the original disconnect/removal, ambiguous-addition, and repoint mutants through `checkMachineCoverage` → `pinnedConstructors`; permanent stale-pin control still names path and both blobs |
| FID-MACHINE-COVERAGE | BLOCKING | KILLED | pinned 18, exact four dated `retired-by-#62/R62-08`, remaining 14 execute through real `attempt` |
| FID-CAN-FAIL | BLOCKING | KILLED | removing one live `attempt` case makes both frozen and permanent coverage RED for `unknown event tag` |
| FID-DONATE | BLOCKING | KILLED | positive donation raises author cassa and reserved non-member comune conto equally; invalid author/value refused |
| FID-BACKDONATE | BLOCKING | KILLED | positive affordable share raises every current member by `w`, lowers comune by `n*w`, changes no cassa; invalid/insolvent cases refused |
| FID-GOVERNANCE-BOUNDARY | BLOCKING | KILLED | `donate` direct; `backdonate` remains app-decided and cannot bypass governed-sequence evidence; no invented proved join |
| FID-SINGLE-CORE | BLOCKING | KILLED | scenario runner imports the shared core and generated page slices are byte-identical |
| FID-RETIRE-62 | BLOCKING | KILLED | no implementation or repair of the four R62-08 membership/role events; exemption is visible, exact, and dated |

The campaign ends only when every row is KILLED, or when the bounded audit
ladder ends with findings and the ticket is re-cut. No residual is authorized
for these money/fidelity rows.

## Spend log

### Build 1/3 — planned

- purpose: commit-owner GREEN submission (frozen v6 gate once)
- worker: commit-owner-fidelity-grok / grok-4.6
- candidate: uncommitted GREEN on RED `5e9cd2a6713c5be46df841667c655a707936ee67`
- cache: lake `.lake` 11M, warm (claim-gate lake already run this session)
- free_space_before: root_avail=55852945408 code_avail=191G
- command: `bash /tmp/reactivegas/ms2/t-simulator-fable/handoffs/gate-v6-machine-fidelity.sh`
- receipt: `/tmp/reactivegas/ms2/t-simulator-fable/commit-owner-fidelity-grok/evidence/green/gate-v6.log`
- result: exit=0 duration_ms=220841 evidence_sha256=`b3be4095e04c462697caccc1de0086db85795ea96c73cfa5d11c86f6eef15369`
- free_space_after: root_avail=55852146688
- candidate: `d645cbd4e2e9a1ffbab3838cce30f73949ec6502` tree=`9147cf8fa82f84b67f09bdc32374ea92a22f9364`

### Build 2/3 — planned

- purpose: fresh Codex commit audit, frozen v6 gate once
- worker: commit-auditor-codex-fidelity-s1 / codex
- candidate: `d645cbd4e2e9a1ffbab3838cce30f73949ec6502` tree=`9147cf8fa82f84b67f09bdc32374ea92a22f9364`
- cache: cold/incomplete; `lean/.lake` contains only 135412 bytes of lakefile configuration and no Reactivegas `.olean`
- free_space_before: root_avail=55823953920 code_avail=204026208256
- command: `bash /tmp/reactivegas/ms2/t-simulator-fable/handoffs/gate-v6-machine-fidelity.sh`
- receipt: `/tmp/reactivegas/ms2/t-simulator-fable/commit-auditor-codex-fidelity-s1/evidence/build/gate-v6.log`
- result: exit=1 duration_ms=7260 evidence_sha256=`2d203e68d9c795eff5dd43f342950c5fdb7f4d9309eea823805a0173cf11f21d`
- free_space_after: root_avail=55823781888 code_avail=204026208256

## Submission 1 disposition

- verdict: `FINDINGS`, two blocking findings, repair `1/1` authorized
- report: `/tmp/reactivegas/ms2/t-simulator-fable/.archived/commit-auditor-codex-fidelity-s1/handoffs/AUDIT-REPORT.md`
- report_sha256: `e9e65cbe0fe3bec6394cb9bc97ba21af4952ab8b3f645fc21711b4e9be4a81d4`
- rejected_candidate: `d645cbd4e2e9a1ffbab3838cce30f73949ec6502`
- open: FID-PIN-FRESH manifest/source disconnect; clean cold-cache focused/full-gate reproducibility boundary touching FID-MACHINE-COVERAGE and FID-CAN-FAIL
- submission_2_scope: repair delta plus the two named findings only; seven terminal mutation rows are carried and broad discovery does not restart
- build_3_purpose: fresh submission-2 auditor, exact frozen gate once from a clean cold-cache detached worktree

## Build 3/3 — final spend

- purpose: fresh submission-2 Codex repair audit, exact frozen v6 gate once
- worker: commit-auditor-codex-fidelity-s2 / codex (`gpt-5.6-sol`, `xhigh`)
- candidate: `b32ae15f2894f14daf6352be6b233254f823ce95` tree=`1e893cd0d2b8d185723f9308b9a5e69db935cef6`
- cache: cold tracked-input-only checkout; `lean/.lake` absent, zero `.olean`, clean porcelain
- free_space_before: root_avail=55820337152 code_avail=203950829568
- command: `/code/llm-settings/shared/skills/gate-script/scripts/run-receipt /tmp/reactivegas/ms2/t-simulator-fable/commit-auditor-codex-fidelity-s2/evidence/build/gate-v6-cold.log -- bash /tmp/reactivegas/ms2/t-simulator-fable/handoffs/gate-v6-machine-fidelity.sh`
- receipt: `/tmp/reactivegas/ms2/t-simulator-fable/commit-auditor-codex-fidelity-s2/evidence/build/gate-v6-cold.log`
- result: exit=0 duration_ms=249654 evidence_sha256=`2b045219c2414d12f2997f258d5523511d7a31e2e9a7a243121fb9669ac38988`
- observed: claim production `machine=14/14 pinned=18 retired=4`; claim selftest 14 controls including all five machine controls; scenario, teaching, economic trace, and vote trace production/selftests; final Lean `Build completed successfully (24 jobs).`
- free_space_after: root_avail=55820558336 code_avail=203939049472; `lean/.lake`=10931143 bytes, 22 `.olean` total / 5 under Reactivegas

## Submission 2 disposition

- verdict: `FINDINGS`, one blocking touched-boundary finding; submission `2/2`, repair `1/1`, disposition `RECUT`
- campaign: `SET-POINT`; all eight declared BLOCKING rows are `KILLED`, with no residual or open campaign row
- repaired FID-PIN-FRESH: PASS; instrument `/tmp/reactivegas/ms2/t-simulator-fable/commit-auditor-codex-fidelity-s2/audit-repair-mutations.mjs` SHA-256 `81f64fb7b20ed6e87a500ab2264c3c2940e9db1c4053818d2f7e305208f052d2`, receipt SHA-256 `52ed49c99df53aa655b7e2aa4a646ae34814d3a8f87d5e01f790ac7e5b46da18`
- cold focused/full boundary: PASS under the normal unset environment; exact cold full-gate receipt above
- blocking boundary: `RG_CLAIM_GATE_JS_ONLY=1` makes shipped `--selftest` return 0 before its 14-control completion marker, while the unchanged frozen gate accepts the shorter machine-control prefix; predicate receipt SHA-256 `4c89e08ed8b23bda6418d9a4e0acd69deb1f4b5ded266f6b55fcfdf71293be27`
- report inputs: mandate `5476d9212db46da9f4a60019814137d0b4f72f05957e3e37a0f4b99280e66767`; prior report `e9e65cbe0fe3bec6394cb9bc97ba21af4952ab8b3f645fc21711b4e9be4a81d4`; repair note `62fd955d8258a51bb82441bb1965bd439363db28f5ad3bf7c97ed96de1418876`; owner receipt `7ffc33bf1ed531a30e772f9847fc24b90a3bb863f4e01cfc631194fb12fa9385`; frozen gate `848c01f58f0dd10a0ab7eb77d6a741be1781c62369a63ee38bbcd64c10981545`; oracle `6fb36f9f5d51e404de4f454320c7825061a8f4a0a6550e4a248db57caa9e3730`; provenance receipt `a2481ca699b8861077bf3b6dd230cb817eafb947824121f7107aa67b4d14bcb1`; cold preflight `fd4bc5a61684e48598ba8f779ebf5fbf407db6c90c4b9430b2daadece7043fcb`; cold postflight `d8230e74a4f1ed72478835c518dade3fe407b9fde4e629c6c9188a003b3664fd`
- report path: `/tmp/reactivegas/ms2/t-simulator-fable/commit-auditor-codex-fidelity-s2/handoffs/AUDIT-REPORT.md`
- no third submission, second repair, fourth build, residual, push, PR, merge, or publish is authorized
