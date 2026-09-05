# Tasks — #68 proposer-is-not-an-assent

Slice S1 (OWNER, single slice). Task IDs stable; stamp only after acceptance.

## Planning (ticket owner, no builds)

- [x] T68-01 Bootstrap lane: runtime, worktree /code/reactivegas-issue-68,
  branch feat/68-proposer-assent at e6c5924, START with pane/argv/identity.
- [x] T68-02 Discovered extent probes (read-only) + spec/plan/tasks draft.
- [x] T68-03 Gate v1 authored (ruling-certain rows; superseded by v2).
- [x] T68-07 Gate v3 + fenced witness oracle
  specs/68-proposer-assent/witness-t68.lean authored (unfalsified until
  base-RED run).
- [x] T68-04 Q-001 filed (proposer self-approval + n=1 semantics).
- [x] T68-05 Planning commit (spec/plan/tasks only, no production, no gate
  commit — gate.sh stays untracked/ignored).
- [x] T68-06a A-001 ruling received (option A; B/C as negative witnesses).
- [ ] T68-06b Desk release for implementation (Q-002; held on simulator campaign).

## Implementation (commit owner, after release)

- [ ] T68-10 Empty-open on both propose paths (Fold + Integration).
- [ ] T68-11 A-001 regime (approval bar/guard + sole-admin exception).
- [ ] T68-12 WellFormed restatement + preservation proofs, both paths.
- [ ] T68-13 Dependent theorems restated/re-proved; majority theorems
  meaning-identical; wrappers updated.
- [ ] T68-14 Reachable executable witnesses R68-08 (positive + negatives).
- [ ] T68-15 Mutation control: proposer-credit mutant REDs (both paths).
- [ ] T68-16 Full local gate GREEN (toolchain-contract, just lean,
  lean-corpus-gate) within 6-build budget; PROOF-COMPLETE submission.

## Acceptance (ticket owner)

- [ ] T68-20 Fresh codex audit per submission (gpt-6-astra/high, isolated
  snapshot, pane-bound START, live argv verified).
- [ ] T68-21 Accept or single repair bounce + fresh re-audit; task stamp;
  quiet final verification; push exact SHA; draft PR; CI green.
- [ ] T68-22 Design-content handoff (cited) for #71; scope reconciliation;
  resume handback; merge-permission request (no merge by this lane).
