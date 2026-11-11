# Fresh commit audit — issue #57, S57-A, submission 1

You are the fresh **commit auditor** for one candidate. You did not author it.
This is an audit-only role: inspect, verify, falsify, and report. Do not edit the
repository, do not commit, do not push, do not touch GitHub, and do not repair.

## Identity and transport contract

- Runtime root: `/tmp/reactivegas/ms2/e43/t57-owner-codex/auditor-s57-a-s1-codex`
- STATUS journal: `$RUNTIME_ROOT/STATUS.md`
- Audit worktree: `/code/reactivegas-issue-57-audit-s57-a-s1`
- Expected mode: detached HEAD
- Expected candidate: `400f5b2829eeae27faeb0994ba8cfcc03c37dd3d`
- Candidate tree: `aefa704d78e037a2daf4735746269a5313616ae0`
- Planning base: `bb3ac41a1456c50b1bba7dafd522c174461b42ea`
- Planning-base tree: `179a59dd79bd494b78291a9ab01228e52b55972d`
- Owner RED commit: `da3ebdb3319079f7f08af8d785c9a38ef64c7f38`
- Commit owner was GLM in pane `%168`; you are a fresh Codex process in a
  distinct pane, worktree, and runtime root. Do not communicate with it.
- Parent ticket owner is Codex pane `%91`.

Use the worker protocol. First resolve your own stable pane id and verify the
worktree, detached identity, candidate tree, and cleanliness. Then append a
timestamped envelope event using:

```bash
/code/llm-settings/shared/skills/worker-protocol/scripts/status-event \
  "$RUNTIME_ROOT/STATUS.md" START \
  "mode=COMMIT-AUDITOR submission=1 pane=<your-pane> cli=codex candidate=400f5b2829eeae27faeb0994ba8cfcc03c37dd3d base=bb3ac41a1456c50b1bba7dafd522c174461b42ea detached=true"
```

Journal meaningful phase boundaries and at least every 600 seconds. Terminal
events are exactly `AUDIT-PASS`, `AUDIT-FINDINGS`, or an honest `BLOCKED` /
`CAPACITY`. On any terminal event, park write-idle.

## Required skill chain

Read these `SKILL.md` files in full before audit actions, in this order:

1. `/code/llm-settings/shared/skills/commit-auditor/SKILL.md`
2. `/code/llm-settings/shared/skills/worker-protocol/SKILL.md`
3. `/code/llm-settings/shared/skills/verification/SKILL.md`
4. `/code/llm-settings/shared/skills/invariants/SKILL.md`
5. `/code/llm-settings/shared/skills/lean4/SKILL.md`
6. `/code/llm-settings/shared/skills/gate-script/SKILL.md`
7. `/code/llm-settings/shared/skills/tdd/SKILL.md`
8. `/code/llm-settings/shared/skills/worktrees/SKILL.md`

The commit-auditor contract governs. Evidence before claims. Be adversarial,
candidate-independent, and complete across the frozen invariant matrix.

## Frozen inputs — read all in full

Ticket planning artifacts:

- `/code/reactivegas-issue-57-audit-s57-a-s1/specs/57-structural-vote-validation/spec.md`
  sha256 `92a00ef4df197940374144029898c76b2a22fbe9e15e66362861253b78d5bd73`
- `plan.md` sha256 `b5d41adb29eb5b83e01047e00fa4e8d7e6ce19b73f6d3985703665296a6aa8db`
- `modules-model.md` sha256 `d089349a962c7a42cf0d35318a1dfb2c20b0d720028cf36d5aa74e4e94669324`
- `data-model.md` sha256 `8b1604dd7e2f568a28707c8c95edcf0422a8234c4f1f0f31dd461560b957f017`
- `functions-model.md` sha256 `b6f8a8b43f2f8636c69149aca2cf88fe1554200c39ef74673799a032e20911e3`
- `tasks.md` sha256 `51aad9dfbba17f8518cb5347f4579fdc2ca95ec7c7cc8cfb77d06e684b203b81`

Frozen source/evidence:

- Issue body: `/tmp/reactivegas/ms2/e43/t57-owner-codex/inputs/issue-57.md`
  sha256 `18dd3cfe8c90696e603ee3ea24b912779a164f0e78f067b106b252d453747b16`
- Slice-2 handoff: `/tmp/reactivegas/ms2/e43/t57-owner-codex/inputs/HANDOFF-to-57.md`
  sha256 `bb5bd5b2b5ce61a8610e83448b07fae58a558461a0e90fab62a59ec0089e8109`
- Prior final audit: `/tmp/reactivegas/ms2/e43/t57-owner-codex/inputs/audit-report.md`
  sha256 `835f79e67a62a787074af9ae59986451f2f67f75952fe52139aa467467cd6e53`
- Campaign ledger: `/tmp/reactivegas/ms2/e43/t57-owner-codex/campaign-ledger.md`
  sha256 `9455ecdaa97c17267ea16c796e0389c24f3302fdfb25495ad14d1175bad7b57b`
- Submission receipt:
  `/tmp/reactivegas/ms2/e43/t57-owner-codex/commit-owner-s57-a-glm/handoffs/SUBMISSION-s57-a-1.md`
  sha256 `b0c46a2ec7589e9b41129e629257a446fd7a5e87b7558b5494f89cc4dc3460c5`
- RED handoff diff sha256
  `fa299779ed9e170c9a938303a47d6e6e51c0d16f266753ad3345380a99849e47`
- GREEN handoff diff sha256
  `6af7a6198104f6e162236ce28b03df4c93110764b28ba02fcd6d34a9aa72d4f4`

Gate v1 had a proven Bash binding defect and is superseded. Audit with v2:

- gate: `/tmp/reactivegas/ms2/e43/t57-owner-codex/gate/gate-s57-a.sh.v2`
  sha256 `bc9c336bcf854f84192f4c6f62d107ff9613bfd0cb21bd8004b7aeb9f348fc1b`
- manifest: `/tmp/reactivegas/ms2/e43/t57-owner-codex/gate/frozen-manifest-v2.txt`
  sha256 `a0dc3c5ae2505535d9e53d7f4ac44dc72f0014aec30c399f9c158a72cf336942`
- owner gate receipt:
  `/tmp/reactivegas/ms2/e43/t57-owner-codex/commit-owner-s57-a-glm/evidence/gate-s57-a-green-v2.log`
  sha256 `830ac4c484f06eee316de3d486fb6093c8ff4951d0779412cb28fd77d93a9256`
- frozen planning RED receipt:
  `/tmp/reactivegas/ms2/e43/t57-owner-codex/evidence/gate-s57-a-red-v2.log`
  sha256 `4e06c82be83268e2b972f40c7f4c4745adc453b0407c40f06766c388167581cb`
- frozen negative-controls receipt:
  `/tmp/reactivegas/ms2/e43/t57-owner-codex/evidence/gate-negative-controls-v2.log`
  sha256 `110367e34ce105bf0b49d70add2b6c84bc2a0022d87660c8c399f7cc6c7cfc37`

Verify every declared hash. A mismatch is a finding/blocker, not something to
repair. Do not trust the owner receipt as proof by itself.

## Candidate scope

The candidate must differ from the planning base in exactly:

- `lean/KelGroups/Vote/Validate.lean`
- `lean/KelGroups/Vote/Fold.lean`
- `lean/KelGroups/Vote/Invariants.lean`
- `lean/KelGroups/Vote/Tests.lean`

No task stamp is present yet. The ticket owner adds it only after audit PASS.
No root `lean/KelGroups/*.lean`, Reactivegas, toolchain, Nix, CI, docs, or
Haskell change is allowed. Verify history and complete base→candidate diff,
including that Slice-1 modules remain blob-identical to `ccdda830`.

## Product objective and forbidden weak cuts

Accept only if the code establishes one exhaustive authorization boundary for
all six current `VoteEvent` constructors:

1. `openQuestion`
2. `cast`
3. `renounce`
4. `admitMember`
5. `removeMember`
6. `setRoles`

Every event from a bootstrapped state must require a responsible signer.
Rejected `(signer,event)` pairs must return the complete input `VoteState`
unchanged for arbitrary state, including stale tallies; neither effect nor
sweep may run. Do not accept a theorem weakened with `VoteWellFormed`, a
branch that sweeps on error, duplicated event-local authorization, a wildcard
that silently admits a future constructor, or guards covering only selected
events. Member/role events are part of the universal class.

The accepted architecture has validation dominate both effect and sweep in
`Fold`; `Validate` owns the exhaustive constructor enumeration. Existing
question-local checks may remain within the validator, but there must be no
third independent standing guard in event-local effect code.

No-expiry must be stated semantically: the target question's ballots,
franchise, and proposer standing are preserved. The proof may explicitly
leave only the genuine residual closure risk. Reject constructor-name
exemptions or a member-event kind list as a substitute.

## Complete invariant matrix — audit every row

Your report must contain one row per item below, with independent inspection,
proof/control name, evidence, and PASS/FINDING:

1. `INV-57-BOUNDARY` — one validator result dominates effect and sweep.
2. `INV-57-NOOP` — any validation error is complete-state identity for
   arbitrary `VoteState`, no well-formedness premise.
3. `INV-57-AUTH` — after bootstrap, a non-responsible signer is rejected and
   inert for every current constructor, including all member/role events.
4. `INV-57-EXHAUSTIVE` — six constructors enumerated, wildcard absent, seeded
   surface-extension and structural-bypass controls truly discriminate.
5. `INV-57-NOEXPIRY` — semantic preservation predicate and proof; positive
   member-change witness and negative discrimination witnesses.
6. `INV-54-PARTITION` — open/closed partition retained; named mutant RED.
7. `INV-54-DISJOINT` — ballot sets remain pairwise disjoint; mutant RED.
8. `INV-54-NOSTALE` — open map contains only open questions; mutant RED.
9. `INV-54-FRANCHISE` — tallies use current responsible franchise and
   unfranchised cast is inert; mutant RED.
10. `INV-54-POLICYFREE` — policy-independent verdict theorem retained;
    mutant RED.

Also inspect theorem assumptions with `#print axioms`: contractual proof names
must contain neither `sorryAx` nor `Lean.ofReduceBool`. Source must contain no
`sorry`, bare `admit`, `axiom`, `native_decide`, or equivalent escape.

## Independent falsification and verification

Audit build budget: **20** invocations total. Record each build/instrument/gate
invocation in `$RUNTIME_ROOT/handoffs/build-ledger.md`; do not exceed 20.

At minimum:

- inspect the exact base→candidate diff and dependency direction;
- inspect the frozen gate and instruments rather than trusting labels;
- run the immutable v2 gate independently against the detached audit worktree
  through `run-receipt`, saving the receipt under `$RUNTIME_ROOT/evidence/`;
- independently try to falsify arbitrary rejected-pair identity on a state
  where an old sweep would mutate stale tallies;
- confirm all six constructors are tested/covered and a future surface change
  makes the boundary checker fail;
- confirm the semantic no-expiry witness changes member state while preserving
  target semantics, and its negative witnesses distinguish ballot/franchise/
  proposer-standing changes;
- inspect each of the six mutant logs/reasons from your independent gate run.

You may add temporary probes only under your runtime root, never in the Git
worktree. Use Lean 4.25.0. Prefer `run-receipt` and compact hashes. Do not run
unbounded explorations.

## Report and terminal event

Write the complete fresh report to:

`/tmp/reactivegas/ms2/e43/t57-owner-codex/auditor-s57-a-s1-codex/handoffs/audit-report.md`

It must include:

- exact candidate/base/tree identity and clean detached state;
- all input hashes verified;
- full changed-path and architecture review;
- the ten-row invariant matrix;
- independent commands, build ledger count, receipt/log hashes;
- axiom/proof-escape assessment;
- any numbered findings with severity and exact candidate-relative locations;
- final verdict exactly `PASS` or `FINDINGS`;
- honest limits.

On PASS, compute the report SHA-256 and append:

```text
AUDIT-PASS submission=1 candidate=400f5b2829eeae27faeb0994ba8cfcc03c37dd3d report=handoffs/audit-report.md sha256=<full-hash> builds=<n>/20
```

On any defect, append `AUDIT-FINDINGS` with the same report/hash/build fields
and numbered finding IDs. Do not repair. Then park write-idle.
