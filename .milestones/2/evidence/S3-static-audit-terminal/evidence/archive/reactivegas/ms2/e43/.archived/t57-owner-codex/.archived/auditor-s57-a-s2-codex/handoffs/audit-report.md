# Commit Audit — issue #57, S57-A, submission 2

- Submission: `2/2` (final)
- Planning base: `bb3ac41a1456c50b1bba7dafd522c174461b42ea`
- Planning-base tree: `179a59dd79bd494b78291a9ab01228e52b55972d`
- Rejected submission 1: `400f5b2829eeae27faeb0994ba8cfcc03c37dd3d`
- Rejected tree: `aefa704d78e037a2daf4735746269a5313616ae0`
- Candidate: `9d68abb0930bb31d9bcd1116979765e974547ffd`
- Candidate tree: `1ea6902b61932bbac6b87586a3e333840c9b7a60`
- Scope: complete `bb3ac41a..9d68abb` provenance and fence review; active repair audit `400f5b2..9d68abb` plus F-001 and every invariant boundary touched by that repair; the complete ten-row matrix is preserved below
- Verdict: `PASS`
- Audit loop: submission `2/2`; next submission forbidden
- Campaign: `CLOSED`, ended by `SET-POINT`; 10 KILLED / 0 RESIDUAL / 0 BLOCKED / 0 OPEN
- Audit invocations: `2/20`; compiled gate runs: `1`, cold
- Toolchain: Lean `4.25.0`
- Build ledger: `handoffs/build-ledger.md`, sha256 `f74ebae9b872b961f94dac378ec83e021e62dd76fb346734ce657369890d5881`

## Identity and frozen inputs

The audit ran in fresh Codex pane `%175`, in tmux window
`reactivegas:7:reactivegas-e43-t57-structural-validation`, distinct from ticket
owner `%91` and GLM commit owner `%168`. The audit worktree was clean and
detached at the exact candidate before inspection, before and after every
execution, and at report freeze.

Every authoritative binding in the brief matched its declared SHA-256:

| Input | SHA-256 |
|---|---|
| issue body | `18dd3cfe9ae6f42a5ca1324419436893f87106e17f449034a1fc2791b21cedf9` |
| resurrection handoff | `bb5bd5b2bf49aad2d24b3d71b17e8e16b464d0ba0674aed428fa5c826f2c4c64` |
| final #54 audit | `835f79e6ec605871ca64b3cee2d72b55e495fb02d852b65215522eb4280fc3de` |
| final #54 campaign ledger | `9667b9f048dbb02fc2a9aa09c40139d3674b340005efd113c95c0c267df33d98` |
| #57 campaign ledger | `9455ecdaa892393c59a4c0bdc809e459907335d773bddb358753873f57137999` |
| `spec.md` | `92a00ef4e36cdbebdfe76bf6196c48998e3a95399b0004bd3498dc2ec75654cd` |
| `plan.md` | `b5d41adbbf8305ce7199062634b20b267a2fda9b08c358e2b9593bd0931b207b` |
| `modules-model.md` | `d089349a1e19562c088ae4205be6289283703beac082720b4dbd2e7a05d0ee9e` |
| `data-model.md` | `8b1604dd0e1d7dd066611c419b3439e8091aa5b1ff5e310108f56cadc9454720` |
| `functions-model.md` | `b6f8a8b42d6c6476f1766b631e0da0d2b3017c338bdf0cfb994ba6c680e9913c` |
| `tasks.md` | `51aad9dfa2e2eca2d87fe43bf9217325a5ac21784bdf00f14b98155704ca005d` |
| owner submission receipt | `73518209c64901dda2d2149f99b9f18968a3c366a5bdea5dfff025e1594cabb2` |
| repair diff | `ee4e96f403e1b80b3a55f672023113f53e5fc40b1cec0e52e9ff728c9aa89477` |
| repair manifest | `7f367a1daeb0b393b1fc8a2e1db86a97192dcbcd8537232dcf4d0d57a3087a83` |
| submission-1 audit | `6a4985eeb95c440dfbf891c86bb49dce06ee928aeacd636141c755deb4e813ba` |
| immutable gate v3 | `96fba04a558994d57d6bb12a5ee9fbfcdab442f460e46ab9ccaeee45124f2997` |
| frozen manifest v3 | `b628b5673f031671393e6fe5ef30d53b4c1034bdd44e0aa3b3ee6a6172f52a9a` |
| F-001 rejected-candidate RED receipt | `0fa6d82c5f30613314e0099f220f5aa9c2d3576953e6df8d23448d3c083810b0` |
| owner repair GREEN receipt | `fc554d8e8b7f5534fa66c8aa40dfb1168f272561b5c83462b3a8f8103656da0d` |
| planning RED receipt | `4e06c82be83268e2b972f40c7f4c4745adc453b0407c40f06766c388167581cb` |
| negative-controls receipt | `110367e34ce105bf0b49d70add2b6c84bc2a0022d87660c8c399f7cc6c7cfc37` |

## Provenance, scope, and architecture

`git merge-base --is-ancestor` proves the candidate descends from the named
base through exactly the owner RED, submission-1 GREEN, and F-001 repair
commits. The complete base-to-candidate change set is exactly four ordinary
`100644` files:

- `lean/KelGroups/Vote/Validate.lean`
- `lean/KelGroups/Vote/Fold.lean`
- `lean/KelGroups/Vote/Invariants.lean`
- `lean/KelGroups/Vote/Tests.lean`

The repair delta is exactly `Fold.lean` and `Invariants.lean`, as declared;
the manifest is structurally valid and `verify-commit-handoff` proved the
candidate contains exactly its frozen blobs. There is no task stamp, mode or
link change, or toolchain, Nix, CI, documentation, Haskell, Reactivegas, or
other forbidden-path change. All seven Slice-1 root module blobs match
`ccdda830` exactly. The dependency-direction check found nine positive-control
imports and no `Reactivegas.*` import under `lean/KelGroups/`.

`validateVoteEvent` owns one total explicit authorization match over the six
constructors: `openQuestion`, `cast`, `renounce`, `admitMember`,
`removeMember`, and `setRoles`. It has no wildcard or side registry. With a
nonempty franchise every arm requires a current responsabile; only
empty-franchise `admitMember` retains bootstrap capability.

`applyVoteEvent` matches that validation result once. Its error arm returns the
input `VoteState` definitionally, before either effect or sweep; its `.ok` arm
alone calls `effectedState` and then `sweepClosures`. The complete
`effectedState` region is authorization-free. Its calls are state-effect
operations (`lookupQuestion`, `placeBallot`, association-list updates), not a
renamed standing decision.

## Explicit F-001 closure analysis

The v3 source check scans the exact `effectedState` region and rejects the
`isResponsabile` guard. Its frozen receipt is RED on rejected candidate
`400f5b2` for `effectedState retains event-local authorization guard`; the
same unchanged gate is GREEN on `9d68abb`.

Independent instrument `instruments/probe-f001-architecture.sh` sha256
`dc737c8de52bdcc0a5447fee806ab2eb23c963ba549af48b81ffb6c0dcfae5df`
preflighted the rejected seed and then inspected the candidate. It observed
one authorization identifier in the rejected effect region, zero in the
candidate effect region, nine positive-control occurrences in the validator,
all six effect arms, and no wildcard. It also verified that
`effectedState_tally_growth` now takes
`validateVoteEvent ... = Except.ok ()`, consumes that premise to eliminate an
unauthorized cast, and receives the validator equation `hval` at its production
fold call site. Receipt sha256:
`d22261ff926263e46f8be3e72817f53ecce3982e4b8683878ecac59fcbda0192`.

F-001 is removed, not renamed or relocated. Authorization remains solely in
`Validate`; the proof adaptation derives cast-time standing from the
validator's admitted premise rather than restoring an effect guard.

## Invariant matrix

| Invariant | Severity | Verdict | Row state | Proof and independent evidence |
|---|---|---|---|---|
| `INV-57-BOUNDARY` | BLOCKING | PASS | KILLED | The single validation result dominates effect and sweep; the repaired effect region is authorization-free. F-001 rejected RED `0fa6d82c…`, independent probe `d22261ff…`, and BYPASS mutant RED in cold gate receipt `c5e38f00…`. |
| `INV-57-NOOP` | BLOCKING | PASS | KILLED | `inadmissible_is_noop` quantifies arbitrary state/error with only the validator error equation. `arbitrary-and-surface-noop.lean` sha256 `5c28e801…` uses a stale, already-positive open question that a sweep would close; rejection returns the exact stale state. Its planning-base negative control and candidate GREEN both execute in the frozen evidence chain. |
| `INV-57-AUTH` | BLOCKING | PASS | KILLED | `nonresponsabile_event_noop` is universal over `VoteEvent`. Tests and `arbitrary-and-surface-noop.lean` use all six distinct constructors after bootstrap; explicit member/role no-ops and `r45-production-noop.lean` sha256 `bbd07047…` cover `admitMember`, `removeMember`, and `setRoles` with nondegenerate state. |
| `INV-57-EXHAUSTIVE` | BLOCKING | PASS | KILLED | Event source and validator each expose six explicit arms with no wildcard. Checker sha256 `f0ea4974…`; frozen future-surface negative control fails on added `auditBypass` at the authorization boundary; negative-controls receipt `110367e3…`. |
| `INV-57-NOEXPIRY` | ADVISORY-BUT-REQUIRED | PASS | KILLED | `PreservesQuestionSemantics` observes the exact target question, current franchise, and proposer standing through production `applyVoteEvent`. Positive non-admin admission witness changes membership while preserving all three; separate tests discriminate a franchise change, target-ballot change, and proposer-standing change. Instrument sha256 `19d0d4d7…`. |
| `INV-54-PARTITION` | BLOCKING | PASS | KILLED | `questions_partition` retains prefix-observed IDs and open/closed partition. Fresh silent-deletion mutant reached `MUTATION-APPLIED:PARTITION` and went RED; instrument `b6ca019c…`, candidate gate `c5e38f00…`. |
| `INV-54-DISJOINT` | BLOCKING | PASS | KILLED | `ballots_nodup_disjoint` covers open and closed questions. Both-tallies mutant reached `MUTATION-APPLIED:DISJOINT` and went RED; instrument `ef886d9b…`, candidate gate `c5e38f00…`. |
| `INV-54-NOSTALE` | BLOCKING | PASS | KILLED | `open_questions_are_open` is carried by the production fold. Non-ballot sweep-omission mutant reached `MUTATION-APPLIED:NOSTALE` and went RED; instrument `096c2054…`, candidate gate `c5e38f00…`. |
| `INV-54-FRANCHISE` | BLOCKING | PASS | KILLED | `franchise_of_tallies` traces every tally key to its cast-time production prefix. The repaired `effectedState_tally_growth` consumes validator admission. Unfranchised-recast mutant reached `MUTATION-APPLIED:FRANCHISE` and went RED; instrument `c38bf439…`, candidate gate `c5e38f00…`. |
| `INV-54-POLICYFREE` | BLOCKING | PASS | KILLED | `verdictOf_threshold_congr` depends only on agreement at the current franchise size. Hard-coded legacy-threshold mutant reached `MUTATION-APPLIED:POLICYFREE` and went RED; instrument `db324526…`, candidate gate `c5e38f00…`. |

All witnesses use distinguishable non-default values: bootstrapped nonempty
franchises, distinct signers and constructors, nonempty tallies, threshold-
sensitive stale state, changed membership, and deliberately disagreeing
threshold policies. The comparisons do not collapse to shared empty/default
fixtures.

## Proof trust

The cold focused build printed all nine contractual theorem axiom sets:
`foldVote_wellFormed`, `ballots_nodup_disjoint`,
`open_questions_are_open`, `questions_partition`, `no_expiry`,
`franchise_of_tallies`, `verdictOf_threshold_congr`,
`inadmissible_is_noop`, and `nonresponsabile_event_noop`. Their dependencies
are limited to `propext`, `Classical.choice`, and `Quot.sound`.

A positive-control source search found the contractual theorem, axiom print,
validator, and event effect. The same search found zero `sorry`, bare `admit`,
custom `axiom`, `native_decide`, `sorryAx`, or `Lean.ofReduceBool` under
`lean/KelGroups/Vote`.

## Verification receipts

| # | Command | Cache | Exit | Duration | Evidence |
|---:|---|---|---:|---:|---|
| 1 | immutable `gate-s57-a.sh.v3 /code/reactivegas-issue-57-audit-s57-a-s2` through `run-receipt` | cold | 0 | 105.342s | `evidence/independent-gate-v3.log` sha256 `c5e38f000a9849cbd15ab460e5f83d6be3809e9eca75fd063566b8e590c425eb`; 644709 bytes / 15246 lines |
| 2 | independent rejected-vs-candidate F-001 architecture instrument | n/a | 0 | 0.075s | `evidence/probe-f001-architecture.log` sha256 `d22261ff926263e46f8be3e72817f53ecce3982e4b8683878ecac59fcbda0192` |

The immutable gate built before probes on the pristine audit tree, completed
the focused build (9 jobs), ran three GREEN instruments and all six named RED
mutants for `PARTITION`, `DISJOINT`, `NOSTALE`, `FRANCHISE`, `POLICYFREE`, and
`BYPASS`, then completed full repository CI (24 jobs). Its final marker is
exactly `gate: GREEN issue=57 slice=S57-A Lean-4.25.0`. Free space was
153909276672 bytes before and 152180793344 bytes after the cold run.

## Blocking findings

None.

## Residuals and advisories

None.

## Candidate invariants and onward discoveries

None.

## Honest limits

- The v3 F-001 check is intentionally identifier-based; by itself it cannot
  prove that a future contributor did not hide standing behind a differently
  named helper. For this exact candidate, full source and call-site inspection
  found no helper, side registry, renamed guard, or relocated authorization;
  the semantic universal proofs and negative controls additionally exercise
  the production boundary.
- Mutation claims are limited to the six named frozen mutants, the planning
  RED controls, and the independent rejected-candidate F-001 seed. No claim of
  exhaustive mutant-class coverage is made.
- This audit establishes properties of the frozen Lean vote model and issue
  mandate. Reactivegas/KelGroups end-to-end composition is explicitly outside
  this slice.

## Final verdict

PASS
