# Commit audit — S28-R1 preflight

- Verdict: AUDIT-CONTRACT-BLOCKED; reason=invalid-auditor-dispatch (different ticket window).
- Worker: commit-auditor-s28r1; marker AUDIT-S28R1; submission 1, FULL scope requested.
- Accepted base: 368b596fef0b6d393c2ac7afc631d236c55d86d1.
- Candidate: 3af3d065b7d0c54f03d89b8c05d8b8acd4a53db4; tree b4eb37f2187c39db4950072e309c0d125d5c27af. Detached and clean at identity checks.
- Mandate v2: eff38e3bca5022a0bcacdbc37deec44766917c755d2ac68fc96c7fdcfdc3c9e0.
- Gate v9 full: dcbc8c2b8eefa111b5b71873be8d87fa95de2369642e6224417f9544e5a8e815; normalized: 3c433effb967052aa91aef2302268c05ab27b3d0f3e54c979504d6978611d340. Both match the brief. Gate was not executed.
- Builds: 0/12; targeted: 0/24. All commands were read-only identity/recon or local artifact writes. No mutations, compilation, tests, remote writes, or owner-seat contact.

## Contract blocker

Live tmux evidence places this auditor pane %566 in reactivegas:15, window kelgroups-s28r1-audit; bound owners %534 and %545 are both in reactivegas:11, window kelgroups. The model/process argv is pinned to Codex gpt-6-astra with model_reasoning_effort=high and the requested worktree; the process and pane differ from both owners. However, commit-auditor/SKILL.md requires the auditor pane to resolve to the same ticket window before START. No exception appears in the brief or read commission notes. Source authority and live outputs are retained in evidence/input-manifest.json and evidence/command-receipts.json.

START was withheld; the full brief acknowledgement was appended as NOTE. The skill's preflight stop is returned without a semantic verdict. Q-001 records the required parent disposition. The auditor did not move panes or repair the dispatch.

## Coverage and limits

All six requirement rows and five reliances are BLOCKED for this run, as enumerated in REQUIREMENT-LEDGER.md. No inherited gate kill or owner success is accepted as a repaired-candidate verdict. F1/F2 have not been rechecked. Candidate source/diff, full gate body, owner verification logs, and prior terminal report were not substantively reviewed after the seat blocker; only identity, authority material, and carried ledger context were inspected. Thus canonical view values, successful hooks, proposal/approval agreement, concurrency conservation, voted effects, automation paths, and all five reliances remain unassessed here.

The mandate's exact command-to-obligation fit remains to be adjudicated before a lawful START. Its allowance for freedom witnesses is not evidence that those witnesses ran. No conclusion of impossibility or of adequate fit is made by this preflight report.

Failure modes altered: NOT EXAMINED — preflight prevented candidate review. Candidate invariants, advisories, onward discoveries and semantic blocking findings: none issued.

Campaign remains OPEN, stopped before execution (neither set-point, tail nor budget exhaustion). Previous campaign and owner spending are not reset or refunded. This is the single terminal preflight report; no automatic second audit is authorized. The ticket owner owns dispatch reconciliation and acceptance.

## Retention

No build trees were created or retired; bytes reclaimed=0. The detached candidate worktree is unchanged and remains under ticket-owner control; its measured size is recorded in evidence/preflight-6.log. All locally created evidence is retained.
