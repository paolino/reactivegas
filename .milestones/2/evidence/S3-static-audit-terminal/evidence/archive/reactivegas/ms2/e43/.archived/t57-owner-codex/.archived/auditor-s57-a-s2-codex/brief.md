# Fresh commit audit — issue #57, S57-A, submission 2 of 2

You are the fresh **commit auditor** for the final repair submission. You did
not author either candidate and must not reuse submission-1 auditor context.
Audit only: inspect, verify, falsify, and report. Do not edit the repository,
commit, repair, push, or touch GitHub. There is no third submission: any finding
is terminal for this cut.

## Identity and worker protocol

- Runtime root: `/tmp/reactivegas/ms2/e43/t57-owner-codex/auditor-s57-a-s2-codex`
- STATUS: `$RUNTIME_ROOT/STATUS.md`
- Detached audit worktree: `/code/reactivegas-issue-57-audit-s57-a-s2`
- Planning base: `bb3ac41a1456c50b1bba7dafd522c174461b42ea`
- Rejected submission 1: `400f5b2829eeae27faeb0994ba8cfcc03c37dd3d`
- Final candidate: `9d68abb0930bb31d9bcd1116979765e974547ffd`
- Final candidate tree: `1ea6902b61932bbac6b87586a3e333840c9b7a60`
- Commit owner: GLM pane `%168`; ticket owner: Codex pane `%91`.
- You must be a fresh Codex process in a new distinct pane.

Resolve your stable pane id; verify detached HEAD, candidate/tree, cleanliness,
and runtime isolation. Then append:

```bash
/code/llm-settings/shared/skills/worker-protocol/scripts/status-event \
  "$RUNTIME_ROOT/STATUS.md" START \
  "mode=COMMIT-AUDITOR submission=2 pane=<pane> cli=codex candidate=9d68abb0930bb31d9bcd1116979765e974547ffd base=bb3ac41a1456c50b1bba7dafd522c174461b42ea detached=true final_submission=true"
```

Journal phase boundaries and at least every 600 seconds. Terminal events are
`AUDIT-PASS`, `AUDIT-FINDINGS`, or honest `BLOCKED`/`CAPACITY`; park write-idle
after one. Do not communicate with the commit owner.

## Required skills — read in full, in order

1. `/code/llm-settings/shared/skills/commit-auditor/SKILL.md`
2. `/code/llm-settings/shared/skills/worker-protocol/SKILL.md`
3. `/code/llm-settings/shared/skills/verification/SKILL.md`
4. `/code/llm-settings/shared/skills/invariants/SKILL.md`
5. `/code/llm-settings/shared/skills/lean4/SKILL.md`
6. `/code/llm-settings/shared/skills/gate-script/SKILL.md`
7. `/code/llm-settings/shared/skills/tdd/SKILL.md`
8. `/code/llm-settings/shared/skills/worktrees/SKILL.md`

The commit-auditor contract governs. Verify evidence, distrust labels, and
cover the whole mandate, not only the repaired lines.

## Authoritative frozen inputs — verify and read in full

- Issue body:
  `/tmp/reactivegas/ms2/e43/artifacts/issue-slice-a-structural-validation-recut.md`
  sha256 `18dd3cfe9ae6f42a5ca1324419436893f87106e17f449034a1fc2791b21cedf9`
- Resurrection handoff:
  `/tmp/reactivegas/ms2/e43/t54-vote-coverage/handoffs/HANDOFF-to-57.md`
  sha256 `bb5bd5b2bf49aad2d24b3d71b17e8e16b464d0ba0674aed428fa5c826f2c4c64`
- Final #54 audit:
  `/tmp/reactivegas/ms2/e43/t54-vote-coverage/auditor-slice-a-s2/handoffs/audit-report.md`
  sha256 `835f79e6ec605871ca64b3cee2d72b55e495fb02d852b65215522eb4280fc3de`
- Final #54 campaign ledger:
  `/tmp/reactivegas/ms2/e43/t54-vote-coverage/auditor-slice-a-s2/handoffs/campaign-ledger.md`
  sha256 `9667b9f048dbb02fc2a9aa09c40139d3674b340005efd113c95c0c267df33d98`
- #57 campaign ledger:
  `/tmp/reactivegas/ms2/e43/t57-owner-codex/campaign-ledger.md`
  sha256 `9455ecdaa892393c59a4c0bdc809e459907335d773bddb358753873f57137999`

Planning artifacts in
`/code/reactivegas-issue-57-audit-s57-a-s2/specs/57-structural-vote-validation/`:

- `spec.md` `92a00ef4e36cdbebdfe76bf6196c48998e3a95399b0004bd3498dc2ec75654cd`
- `plan.md` `b5d41adbbf8305ce7199062634b20b267a2fda9b08c358e2b9593bd0931b207b`
- `modules-model.md` `d089349a1e19562c088ae4205be6289283703beac082720b4dbd2e7a05d0ee9e`
- `data-model.md` `8b1604dd0e1d7dd066611c419b3439e8091aa5b1ff5e310108f56cadc9454720`
- `functions-model.md` `b6f8a8b42d6c6476f1766b631e0da0d2b3017c338bdf0cfb994ba6c680e9913c`
- `tasks.md` `51aad9dfa2e2eca2d87fe43bf9217325a5ac21784bdf00f14b98155704ca005d`

Submission and audit chain:

- Owner submission receipt:
  `/tmp/reactivegas/ms2/e43/t57-owner-codex/commit-owner-s57-a-glm/handoffs/SUBMISSION-s57-a-1.md`
  sha256 `73518209c64901dda2d2149f99b9f18968a3c366a5bdea5dfff025e1594cabb2`
- Repair diff:
  `/tmp/reactivegas/ms2/e43/t57-owner-codex/commit-owner-s57-a-glm/handoffs/repair-s57-a-f001.diff`
  sha256 `ee4e96f403e1b80b3a55f672023113f53e5fc40b1cec0e52e9ff728c9aa89477`
- Repair manifest:
  `/tmp/reactivegas/ms2/e43/t57-owner-codex/commit-owner-s57-a-glm/handoffs/repair-s57-a-f001.diff.manifest`
  sha256 `7f367a1daeb0b393b1fc8a2e1db86a97192dcbcd8537232dcf4d0d57a3087a83`
- Submission-1 audit report:
  `/tmp/reactivegas/ms2/e43/t57-owner-codex/auditor-s57-a-s1-codex/handoffs/audit-report.md`
  sha256 `6a4985eeb95c440dfbf891c86bb49dce06ee928aeacd636141c755deb4e813ba`
- Its sole blocking finding is F-001; the report is evidence, not authority.

Immutable final gate:

- gate `/tmp/reactivegas/ms2/e43/t57-owner-codex/gate/gate-s57-a.sh.v3`
  sha256 `96fba04a558994d57d6bb12a5ee9fbfcdab442f460e46ab9ccaeee45124f2997`
- manifest `/tmp/reactivegas/ms2/e43/t57-owner-codex/gate/frozen-manifest-v3.txt`
  sha256 `b628b5673f031671393e6fe5ef30d53b4c1034bdd44e0aa3b3ee6a6172f52a9a`
- F-001 RED receipt on rejected candidate:
  `/tmp/reactivegas/ms2/e43/t57-owner-codex/evidence/gate-s57-a-f001-red.log`
  sha256 `0fa6d82c5f30613314e0099f220f5aa9c2d3576953e6df8d23448d3c083810b0`
- Owner repair GREEN receipt:
  `/tmp/reactivegas/ms2/e43/t57-owner-codex/commit-owner-s57-a-glm/evidence/gate-s57-a-green-v3-repair.log`
  sha256 `fc554d8e8b7f5534fa66c8aa40dfb1168f272561b5c83462b3a8f8103656da0d`
- Planning RED receipt `4e06c82be83268e2b972f40c7f4c4745adc453b0407c40f06766c388167581cb`
- Negative-controls receipt `110367e34ce105bf0b49d70add2b6c84bc2a0022d87660c8c399f7cc6c7cfc37`

A hash mismatch is a finding/blocker. Do not substitute inputs or repair.

## Scope and full product contract

Audit complete `bb3ac41a..9d68abb`, and separately inspect repair delta
`400f5b2..9d68abb`. Full candidate paths must remain exactly
`Vote/{Validate,Fold,Invariants,Tests}.lean`; repair delta must be exactly
`Fold.lean` and `Invariants.lean`. No task stamp exists yet. Slice-1 root
modules must stay blob-identical to `ccdda830`; no toolchain/Nix/CI/docs/
Haskell/Reactivegas changes.

Accept only one exhaustive authorization boundary for all six `VoteEvent`
constructors: `openQuestion`, `cast`, `renounce`, `admitMember`,
`removeMember`, `setRoles`. After bootstrap, every event requires a responsible
signer. Any validation error must return arbitrary complete `VoteState`
unchanged—including a stale state that a sweep would mutate—with neither
effect nor sweep reached. Reject well-formedness weakening, wildcard arms,
event-kind exemptions, or partial member-event coverage.

`Validate` alone owns authorization. `applyVoteEvent` must let validation
dominate both effect and sweep. `effectedState` must contain no independent
standing/authorization decision. Specifically verify F-001 is genuinely
removed, not renamed or relocated, and that proof adaptations consume the
validator `.ok` premise rather than reintroducing a hidden effect guard.

No-expiry must remain semantic over target ballots, current franchise, and
proposer standing, with a positive member-changing witness and negative
discrimination witnesses. All five inherited #54 invariants remain mandatory.

## Complete matrix — every row required

1. `INV-57-BOUNDARY`: one validation result dominates effect and sweep;
   event effects authorization-free; F-001 closed structurally and semantically.
2. `INV-57-NOOP`: arbitrary-state exact identity on any validation error, no
   `VoteWellFormed` premise; stale-tally discriminator.
3. `INV-57-AUTH`: every current constructor, including three member/role
   events, rejected and inert for a non-responsible signer after bootstrap.
4. `INV-57-EXHAUSTIVE`: six arms, no wildcard, future-surface and BYPASS
   controls discriminate.
5. `INV-57-NOEXPIRY`: semantic predicate, positive member-change witness,
   negative ballot/franchise/proposer-standing witnesses.
6. `INV-54-PARTITION`: retained; named mutant RED.
7. `INV-54-DISJOINT`: retained; named mutant RED.
8. `INV-54-NOSTALE`: retained; named mutant RED.
9. `INV-54-FRANCHISE`: admitted-event proof dependency is sound; current
   franchise and unfranchised-cast identity retained; named mutant RED.
10. `INV-54-POLICYFREE`: retained; named mutant RED.

Check all contractual `#print axioms` outputs and source proof escapes. Neither
`sorryAx`, `Lean.ofReduceBool`, `sorry`, bare `admit`, custom `axiom`, nor
`native_decide` is acceptable.

## Independent verification

Budget: 20 build/instrument/gate invocations total. Record all in
`$RUNTIME_ROOT/handoffs/build-ledger.md`.

At minimum:

- inspect complete and repair diffs plus proof call sites;
- inspect v3 source and ensure its new F-001 check truly fails the rejected
  candidate and passes only when the event-effect region lacks authorization;
- run immutable gate v3 once from this pristine detached worktree through
  `run-receipt`; unlike v2 it must build before probes and pass cold;
- independently falsify or structurally probe F-001 removal and the admitted
  premise used by tally-growth/franchise proof;
- confirm arbitrary stale-state rejected identity, six-constructor coverage,
  future-surface failure, no-expiry witnesses, and all six named mutant reasons;
- keep the Git worktree clean; temporary probes only under runtime root.

## Report and terminal protocol

Write:

`/tmp/reactivegas/ms2/e43/t57-owner-codex/auditor-s57-a-s2-codex/handoffs/audit-report.md`

The report must contain exact identities/hashes, scope and architecture review,
complete ten-row matrix, explicit F-001 closure analysis, independent receipts,
build count, proof trust, numbered findings if any, honest limits, and final
verdict exactly `PASS` or `FINDINGS`.

On PASS append:

`AUDIT-PASS submission=2 candidate=9d68abb0930bb31d9bcd1116979765e974547ffd report=handoffs/audit-report.md sha256=<hash> builds=<n>/20 final_submission=true`

On a defect append `AUDIT-FINDINGS` with report/hash/builds/finding IDs and
`final_submission=true`. No repair is authorized. Park write-idle.
