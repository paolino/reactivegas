# Tasks — #68 proposer-is-not-an-assent

Slice S1 (OWNER, single slice). Task IDs stable; stamp only after acceptance.

## Planning (ticket owner, no builds)

- [x] T68-01 Bootstrap lane: runtime, worktree /code/reactivegas-issue-68,
  branch feat/68-proposer-assent at e6c5924, START with pane/argv/identity.
- [x] T68-02 Discovered extent probes (read-only) + spec/plan/tasks draft.
- [x] T68-03 Gate v1 authored (ruling-certain rows; superseded by v2).
- [x] T68-07 Gate v3 + fenced witness oracle
  specs/68-proposer-assent/witness-t68.lean authored; vacuity-hardened
  (pending-shape preconditions); base falsification COMPLETE (legs 0-3
  green, leg-4 tripwire 4 sites, driver 10/13 RED semantic, 1 build spent).
- [x] T68-04 Q-001 filed (proposer self-approval + n=1 semantics).
- [x] T68-05 Planning commit (spec/plan/tasks only, no production, no gate
  commit — gate.sh stays untracked/ignored).
- [x] T68-06a A-001 ruling received (option A; B/C as negative witnesses).
- [x] T68-06b Desk release for implementation RECEIVED (Q-002/A-002).

## Implementation (commit owner, after release)

- [x] T68-10 Empty-open on both propose paths (Fold + Integration).
- [x] T68-11 A-001 regime (approval bar/guard + sole-admin exception).
- [x] T68-12 WellFormed restatement + preservation proofs, both paths.
- [x] T68-13 Dependent theorems restated/re-proved; majority theorems
  meaning-identical; wrappers updated.
- [x] T68-14 Reachable executable witnesses R68-08 (positive + negatives).
- [x] T68-15 Mutation control: proposer-credit mutant REDs (both paths).
- [x] T68-16 Full local gate GREEN (toolchain-contract, just lean,
  lean-corpus-gate); PROOF-COMPLETE submission. Budget history (replaces
  the original within-6 wording): opened 6/submission, raised 6→10
  (NOTE-005, mutant campaign + integration) then 10→14 (NOTE-008, repair
  + just-ci + reverify); final spend 13/14 full-gate, targeted classes
  receipted, reserve 14 held for final-tree verification.

## Acceptance (ticket owner)

- [x] T68-20 Fresh independent audit per submission (s1 codex, s2 grok per
  desk NOTE-009; gpt-6-astra/high resp. grok-4.6/high, isolated snapshot,
  pane-bound START, live argv verified).
- [x] T68-21 Accept (audit-2 PASS + clean reconciliation) after single repair
  bounce + fresh FULL re-audit; task stamp; quiet final verification
  (existing gate-14 receipt verified to final tree, no rerun per desk);
  push exact SHA; draft PR; CI green re-verified on final head before
  ready-marking.
- [x] T68-22 Design-content handoff (cited) for #71; scope reconciliation;
  resume handback; merge-permission request transmitted in handback
  (no merge by this lane; desk authorization inbound).
- [x] T68-25 Single repair submission (desk NOTE-008): F01 corrections 1-5
  (unconditional raw structural theorems + prefix-admissibility proof or
  retained limits + TraceAdmissible antecedent enumeration + no
  promotion maneuver + required 7-event worker regression), proof
  doc-comments (no signature change), F02 reproof (both paths rebound to
  repaired bytes, rebuild-before-run both sides, retained artifacts),
  full gate GREEN + just-ci on final bytes, re-freeze, PROOF-COMPLETE
  submission 2. Budgets: full-gate ceiling 14 cumulative; targeted ≤24
  with individual receipts.
