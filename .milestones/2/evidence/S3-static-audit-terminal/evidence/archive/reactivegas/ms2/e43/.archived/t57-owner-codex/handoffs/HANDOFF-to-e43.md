# Handoff — issue #57 structurally validate every vote event

Terminal state: **ACCEPTED LOCAL CANDIDATE**. No remote action was taken.

## Exact Git identity

- Repository: `paolino/reactivegas`
- Worktree: `/code/reactivegas-issue-57`
- Branch: `fix/57-structural-vote-validation`
- Final commit: `13b44bcb89567596c8b0d953838b1500ece1f4ef`
- Parent/planning base: `bb3ac41a1456c50b1bba7dafd522c174461b42ea`
- Final tree: `35e821c591ea120c9fd2ec168d444c41fcdbf7dc`
- Subject: `fix(57): structurally validate vote events`
- Worktree/index: clean
- History: exactly one implementation commit after the planning commit
- Remote state: not pushed; no PR opened/updated; issue/project/merge state
  untouched by this ticket owner

Changed paths are exactly:

- `lean/KelGroups/Vote/Validate.lean`
- `lean/KelGroups/Vote/Fold.lean`
- `lean/KelGroups/Vote/Invariants.lean`
- `lean/KelGroups/Vote/Tests.lean`
- `specs/57-structural-vote-validation/tasks.md`

Diff: 293 insertions / 340 deletions. T5700 and T5710…T5715 are checked.

## Accepted behavior and proofs

- `validateVoteEvent` is the sole total authorization boundary over all six
  current `VoteEvent` constructors, without wildcard or side registry.
- After bootstrap, every event—including all member/role events—requires a
  current responsabile; only empty-franchise admission retains bootstrap
  capability.
- `applyVoteEvent` gates both effect and sweep; any validation error returns
  the complete arbitrary input `VoteState` definitionally unchanged, even when
  an old sweep would mutate stale tallies.
- `effectedState` is authorization-free. The rejected submission retained a
  cast-local standing guard (F-001); the repair removed it and made the
  tally-growth proof consume the validator `.ok` premise instead.
- No-expiry is expressed by semantic preservation of target question ballots,
  current franchise, and proposer standing, with positive and negative
  discrimination witnesses.
- PARTITION, DISJOINT, NOSTALE, FRANCHISE, and POLICYFREE were freshly
  re-demonstrated with their named mutants.
- Contractual theorem axiom sets contain only `propext`, `Classical.choice`,
  and `Quot.sound`; no proof escapes are present.

## Audit history

Submission 1 at `400f5b2829eeae27faeb0994ba8cfcc03c37dd3d`:

- fresh audit verdict: FINDINGS
- report SHA-256:
  `6a4985eeb95c440dfbf891c86bb49dce06ee928aeacd636141c755deb4e813ba`
- sole finding F-001: duplicated cast authorization in `effectedState`
- gate v3 made that property permanently RED on the rejected candidate:
  `0fa6d82c5f30613314e0099f220f5aa9c2d3576953e6df8d23448d3c083810b0`

Submission 2 repair candidate at `9d68abb0930bb31d9bcd1116979765e974547ffd`:

- fresh final audit verdict: PASS, 10/10 rows KILLED, no findings/residuals
- report:
  `/tmp/reactivegas/ms2/e43/t57-owner-codex/auditor-s57-a-s2-codex/handoffs/audit-report.md`
- report SHA-256:
  `c3d54428eab8ad2e6b6a85f7e0feb2a19620a25c96fd31efc7ba6fef6981e3dd`
- independent cold gate-v3 receipt SHA-256:
  `c5e38f000a9849cbd15ab460e5f83d6be3809e9eca75fd063566b8e590c425eb`
- independent F-001 architecture probe SHA-256:
  `d22261ff926263e46f8be3e72817f53ecce3982e4b8683878ecac59fcbda0192`

Campaign ledger is CLOSED at SET-POINT, 10 KILLED / 0 RESIDUAL / 0 BLOCKED /
0 OPEN, audit builds 8/20:

- `/tmp/reactivegas/ms2/e43/t57-owner-codex/campaign-ledger.md`
- SHA-256 `758584a315bc132da4ca8781e46cb0737ac8d3475c954de2ae6a026a299b188c`

## Final verification

- Immutable gate v3:
  `/tmp/reactivegas/ms2/e43/t57-owner-codex/gate/gate-s57-a.sh.v3`
- gate SHA-256:
  `96fba04a558994d57d6bb12a5ee9fbfcdab442f460e46ab9ccaeee45124f2997`
- manifest SHA-256:
  `b628b5673f031671393e6fe5ef30d53b4c1034bdd44e0aa3b3ee6a6172f52a9a`
- exact-final-commit receipt:
  `/tmp/reactivegas/ms2/e43/t57-owner-codex/commit-owner-s57-a-glm/evidence/gate-s57-a-final-commit.log`
- receipt SHA-256:
  `5d2bae3c5ae6ebe9bfde022e8ca9878663842e9a8bcf65f7a56adb6cb19ddcc5`
- result: Lean 4.25.0; source/path/dependency checks; event surface and
  authorization-free effect checks; focused proofs and nine axiom prints;
  three GREEN instruments; six named mutants RED; full repository CI (24
  jobs); final marker `gate: GREEN issue=57 slice=S57-A Lean-4.25.0`.

## Gate provenance notes

- v1 contained a Bash `local`/`set -u` self-reference and could not reach its
  instruments; v2 mechanically split the bindings.
- A pristine detached audit then exposed that v2 probed before building
  `.olean` files. v3 builds before external probes and adds the F-001
  structural control. These were versioned gate repairs; no frozen gate was
  edited in place.

## Parent action

The local candidate is ready for epic-owner review and any separately
authorized remote workflow. This handoff does not grant or perform push, PR,
merge, issue closure, project-status change, or composition work.
