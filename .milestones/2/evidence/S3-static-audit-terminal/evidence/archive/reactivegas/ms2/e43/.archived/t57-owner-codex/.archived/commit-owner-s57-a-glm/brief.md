# Commit-owner brief — reactivegas #57 S57-A structural validation

Compiled: 2026-08-29T12:58:00Z

## Identity and authority

- Role: accountable commit owner for issue `paolino/reactivegas#57`, slice
  `S57-A`, tasks `T5710`…`T5715`.
- Worker ID: `commit-owner-s57-a-glm`.
- Parent: #57 ticket owner, family `codex`, pane `%91`, window
  `reactivegas:7` (`reactivegas-e43-t57-structural-validation`).
- Your pane: `%168`; it must differ from `%91` and resolve to the same window.
- Exact identity:
  `family=glm harness=pi provider=zai model=glm-5.3-flash effort=max`.
- Launch: `glm --approve`.
- Worktree: `/code/reactivegas-issue-57`.
- Branch: `fix/57-structural-vote-validation`.
- Runtime root:
  `/tmp/reactivegas/ms2/e43/t57-owner-codex/commit-owner-s57-a-glm`.
- Pre-slice base: `bb3ac41a1456c50b1bba7dafd522c174461b42ea`,
  tree `179a59dd79bd494b78291a9ab01228e52b55972d`.
- Draft policy: `draft=NONE`. Do not dispatch any child.
- You own RED, implementation, local commits, one authorized audit repair if
  forwarded, final squash after acceptance, and compact evidence receipts.
- You may not edit planning, task boxes, gate, remote metadata, or push.

Before reading implementation context, verify pane/family/window/base and append:

```text
START mode=COMMIT-OWNER pane=%168 cli=glm parent_cli=codex alternate=true base=bb3ac41a gate=f020731a draft=NONE harness=pi provider=zai model=glm-5.3-flash effort=max
CONTRACT-STATS bytes=<n> lines=<n> tokens=<Pi-reported-or-unavailable>
```

## Required skill load chain

Read completely before implementation: `commit-owner`, `worker-protocol`,
`tdd`, `gate-script`, `verification`, `invariants`, `worktrees`, and `lean4`.
No recursive orchestration is authorized because `draft=NONE`.

## Objective

Make the production vote step cross one total exhaustive validation boundary
before either event effect or closure sweep. Every rejected signer/event pair
must be exact complete-state identity for an arbitrary pre-state. Once a
franchise exists, every event signed by a non-responsabile must be rejected;
`admitMember`, `removeMember`, and `setRoles` are inside this universal class.

Close the no-expiry coverage gap with a semantic preservation premise, then
re-demonstrate all five inherited invariant rows against the repaired fold.

## Authoritative contract and provenance

Planning is frozen at commit `bb3ac41a` under
`specs/57-structural-vote-validation/`:

| Artifact | SHA-256 | Lines / bytes |
|---|---|---:|
| `spec.md` | `92a00ef4e36cdbebdfe76bf6196c48998e3a95399b0004bd3498dc2ec75654cd` | 95 / 5498 |
| `plan.md` | `b5d41adbbf8305ce7199062634b20b267a2fda9b08c358e2b9593bd0931b207b` | 74 / 3733 |
| `modules-model.md` | `d089349a1e19562c088ae4205be6289283703beac082720b4dbd2e7a05d0ee9e` | 35 / 1953 |
| `data-model.md` | `8b1604dd0e1d7dd066611c419b3439e8091aa5b1ff5e310108f56cadc9454720` | 55 / 2399 |
| `functions-model.md` | `b6f8a8b42d6c6476f1766b631e0da0d2b3017c338bdf0cfb994ba6c680e9913c` | 60 / 2782 |
| `tasks.md` | `51aad9dfa2e2eca2d87fe43bf9217325a5ac21784bdf00f14b98155704ca005d` | 29 / 1557 |

Read all six. The inherited #54 sources below are evidence, not authority that
may weaken #57:

- frozen issue:
  `/tmp/reactivegas/ms2/e43/artifacts/issue-slice-a-structural-validation-recut.md`,
  SHA-256 `18dd3cfe9ae6f42a5ca1324419436893f87106e17f449034a1fc2791b21cedf9`;
- handoff:
  `/tmp/reactivegas/ms2/e43/t54-vote-coverage/handoffs/HANDOFF-to-57.md`,
  SHA-256 `bb5bd5b2bf49aad2d24b3d71b17e8e16b464d0ba0674aed428fa5c826f2c4c64`;
- rejected audit:
  `/tmp/reactivegas/ms2/e43/t54-vote-coverage/auditor-slice-a-s2/handoffs/audit-report.md`,
  SHA-256 `835f79e6ec605871ca64b3cee2d72b55e495fb02d852b65215522eb4280fc3de`.

## Selected architecture contract

### Modules

- `Vote.Validate` owns the one total exhaustive signer/event authorization
  decision, never effects or side registries.
- `Vote.Fold` owns the sole production boundary; rejection returns the exact
  input before effect and sweep. It owns no duplicated authorization.
- `Vote.Invariants` owns arbitrary-state rejection identity, the universal
  non-responsabile corollary, semantic no-expiry, and inherited fold theorems.
- `Vote.Tests` owns production-reachable point oracles, never production
  definitions.

### Data and functions

- `VoteEvent` remains the closed six-constructor surface; `VoteState` remains
  the complete members/open/closed state.
- With nonempty franchise, admissibility success implies the signer is a
  current responsabile for every constructor.
- Empty-franchise admission retains only the existing bootstrap capability
  needed to reach a franchise. Do not implement Slice-B R-66/R-67 shape.
- `PreservesQuestionSemantics` observes target ballots, franchise, and proposer
  standing; it is not a constructor whitelist.
- Keep `validateVoteEvent`'s public signature; make its event decision total,
  exhaustive, and wildcard-free.
- Keep `applyVoteEvent`'s public signature; validation error returns `gs`
  exactly before both effect and sweep.
- Change `inadmissible_is_noop` to arbitrary `gs` plus an exact validation-error
  premise; remove `VoteWellFormed`.
- Add `nonresponsabile_event_noop` over arbitrary event, nonempty franchise,
  and `isResponsabile signer gs = false`.
- Add `PreservesQuestionSemantics` with the exact signature in
  `functions-model.md`, and make `no_expiry` use it.

If any placement or signature is impossible in the four-file fence, file a
contract question before widening or substituting another design.

## Invariant debrief

Campaign ledger:
`/tmp/reactivegas/ms2/e43/t57-owner-codex/campaign-ledger.md`, SHA-256
`9455ecdaa892393c59a4c0bdc809e459907335d773bddb358753873f57137999`.
Audit budget: `builds_spent=0`, `builds_budget=20`; your own builds are not
charged, but this is the ladder you park against.

| Invariant | Severity | Required success |
|---|---|---|
| `INV-57-BOUNDARY` | BLOCKING | one validation decision dominates effect and sweep |
| `INV-57-NOOP` | BLOCKING | arbitrary rejected pairs are complete-state identity without well-formedness |
| `INV-57-AUTH` | BLOCKING | every non-responsabile event is inert after bootstrap; all six constructors exercised |
| `INV-57-EXHAUSTIVE` | BLOCKING | new constructor plus effect cannot bypass authorization |
| `INV-57-NOEXPIRY` | ADVISORY-BUT-REQUIRED | preserving non-admin admission satisfies semantic premise |
| `INV-54-PARTITION` | BLOCKING | fresh silent-deletion mutant red |
| `INV-54-DISJOINT` | BLOCKING | fresh both-tallies mutant red |
| `INV-54-NOSTALE` | BLOCKING | fresh skipped-sweep mutant red |
| `INV-54-FRANCHISE` | BLOCKING | fresh unfranchised-recast mutant red |
| `INV-54-POLICYFREE` | BLOCKING | fresh hard-coded-policy mutant red |

Write the required reliance declaration before RED. A false reliance is a
contract challenge; `enforced: NONE` is legal for a real but unenforced
assumption and does not widen this slice.

## Immutable gate and RED evidence

- Gate v1:
  `/tmp/reactivegas/ms2/e43/t57-owner-codex/gate/gate-s57-a.sh.v1`
- Gate SHA-256:
  `f020731a0948880ee14fb39e4a9da6333de871f21672c5d89427bcaeed9028ff`
- Manifest:
  `/tmp/reactivegas/ms2/e43/t57-owner-codex/gate/frozen-manifest-v1.txt`,
  SHA-256 `fc81eb6995031e6da25b154a3097a4e6ea79491ccfac36303e2c9266f92e2b66`.
- RED receipt: `evidence/gate-s57-a-red-v2.log`, SHA-256
  `4e06c82be83268e2b972f40c7f4c4745adc453b0407c40f06766c388167581cb`.
- Negative controls: `evidence/gate-negative-controls-v2.log`, SHA-256
  `110367e34ce105bf0b49d70add2b6c84bc2a0022d87660c8c399f7cc6c7cfc37`,
  10/10 under Lean 4.25.0.

The gate and runtime instruments are read-only. The retained #54 R-45
instrument is seed evidence only; #57 versions are bound in the manifest.

## Writable and forbidden scope

Writable files, exactly:

- `lean/KelGroups/Vote/Validate.lean`
- `lean/KelGroups/Vote/Fold.lean`
- `lean/KelGroups/Vote/Invariants.lean`
- `lean/KelGroups/Vote/Tests.lean`

Do not edit `specs/**` or task boxes. The ticket owner stages the task stamp
only after audit acceptance.

Hard forbidden: the seven direct `lean/KelGroups/*.lean` Slice-1 modules;
every other `lean/KelGroups/Vote/**` file; `lean/KelGroups.lean`;
`lean/Reactivegas/**`; `lean/lean-toolchain`; Nix, CI, docs, Haskell, upstream
kelgroups, #47/#48/#54 lanes; composition; push, PR/issue edits, rebase, merge,
force/reset/clean.

You are not alone in the codebase; do not revert edits made by others.

## RED, GREEN, and verification

1. Verify clean exact base and all hashes.
2. Write a reliance declaration under `handoffs/` and journal it.
3. Add the complete permanent proof/test bundle first. Run the focused command
   and confirm RED for the intended missing behavior. Commit one RED bundle.
4. Implement the smallest architecture satisfying the frozen contract.
5. Run every cheap focused/readiness check before the evidence gate.
6. Run the immutable gate through `run-receipt`.
7. Freeze RED and GREEN handoffs and create a clean local candidate commit.

Focused command:

```sh
nix develop --quiet --no-write-lock-file -c bash -c \
  'cd lean && lake build KelGroups.Vote.Invariants KelGroups.Vote.Tests'
```

Immutable gate:

```sh
/tmp/reactivegas/ms2/e43/t57-owner-codex/gate/gate-s57-a.sh.v1 \
  /code/reactivegas-issue-57
```

Full repository check: `nix develop --quiet --no-write-lock-file -c just ci`.

Every receipt says Lean 4.25.0 and binds command, candidate/tree, exit,
duration, evidence path/hash, and counts. Inspect raw failures; report compact
receipts upward.

## Budgets and final commit

- Maximum two audited submissions; one parent-authorized repair after first
  findings, none after second findings.
- Changed implementation/proof paths: at most 4.
- Candidate ceiling: 900 additions+deletions over pre-slice base.
- Packet ceiling: 320 lines / 24 KiB.
- No draft/tool tokens or salvage: record `draft=NONE`, salvage ratio `0`.
- Record Pi-reported input/output tokens at terminal if available; do not
  invent cost or token data.
- Final subject: `fix(57): structurally validate vote events`.
- Final body explains universal boundary, arbitrary-state no-op, semantic
  no-expiry, and inherited controls.
- Final trailer: `Tasks: T5710, T5711, T5712, T5713, T5714, T5715`.

## Terminal protocol

Journal material milestones to your `STATUS.md`:

```text
START ...
CONTRACT-STATS ...
RELIANCE-DECLARED ...
RED ...
RED-COMMIT ...
GREEN ...
GREEN-COMMIT ...
PROOF-COMPLETE submission=1 base=bb3ac41a red=<sha> candidate=<sha> receipt=<path> handoff=<path>
```

Then park write-idle. On forwarded `AUDIT-REPAIR-AUTHORIZED`, verify the report
hash, repair once, submit as submission 2, and park again. Do not build while
the auditor runs.

Every inability to continue ends durably:

- architecture/signature/gate/fence mismatch → question + `BLOCKED`;
- scope escape → `SCOPE-FAIL` and preserve state;
- capacity terminal → `COMPLETE capacity-limit` with mechanical handoff (this
  allows the pinned Grok successor);
- successful submission → `PROOF-COMPLETE` and write-idle parking;
- no third submission and no push.

Before any terminal event, check inbox notes. Questions go under
`questions/Q-NNN-*.md`; the parent answers under `answers/` and must receive
your `RESUMED` acknowledgement.
