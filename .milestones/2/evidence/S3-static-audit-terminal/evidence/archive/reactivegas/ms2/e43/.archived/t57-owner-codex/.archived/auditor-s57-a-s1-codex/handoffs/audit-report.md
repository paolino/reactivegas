# Commit Audit — issue #57, S57-A, submission 1

- Submission: `1/2`
- Base: `bb3ac41a1456c50b1bba7dafd522c174461b42ea`
- Base tree: `179a59dd79bd494b78291a9ab01228e52b55972d`
- Owner RED: `da3ebdb3319079f7f08af8d785c9a38ef64c7f38`
- Candidate: `400f5b2829eeae27faeb0994ba8cfcc03c37dd3d`
- Candidate tree: `aefa704d78e037a2daf4735746269a5313616ae0`
- Scope: FULL `bb3ac41a..400f5b28`
- Verdict: **FINDINGS**
- Audit loop: submission `1/2`; one repair submission is allowed by the role contract
- Campaign: **OPEN** — one blocking row remains open; no tail-stop
- Builds/invocations: `6/20`; first candidate build cold, later executions warm
- Toolchain: Lean `4.25.0`

## Dispatch correction and identity

The original brief contained stale/truncated path and hash transcriptions for
the issue/#54 inputs and planning artifacts. Before any build (`0/20`), the
auditor stopped with Q-001. Ticket-owner answer A-001 durably superseded only
those declarations. All 11 corrected bindings were independently re-hashed
and matched before `RESUMED`; candidate, base, gate v2, and gate semantics did
not change.

The audit ran in pane `%172`, distinct from ticket owner `%91` and GLM commit
owner `%168`. The worktree was and remains clean, detached, and exactly at the
candidate SHA/tree above.

## Frozen input verification

Every authoritative declaration matched its SHA-256:

| Input | SHA-256 | Result |
|---|---|---|
| corrected issue body | `18dd3cfe9ae6f42a5ca1324419436893f87106e17f449034a1fc2791b21cedf9` | PASS |
| corrected resurrection handoff | `bb5bd5b2bf49aad2d24b3d71b17e8e16b464d0ba0674aed428fa5c826f2c4c64` | PASS |
| corrected final #54 audit | `835f79e6ec605871ca64b3cee2d72b55e495fb02d852b65215522eb4280fc3de` | PASS |
| corrected final #54 ledger | `9667b9f048dbb02fc2a9aa09c40139d3674b340005efd113c95c0c267df33d98` | PASS |
| corrected #57 ledger | `9455ecdaa892393c59a4c0bdc809e459907335d773bddb358753873f57137999` | PASS |
| corrected `spec.md` | `92a00ef4e36cdbebdfe76bf6196c48998e3a95399b0004bd3498dc2ec75654cd` | PASS |
| corrected `plan.md` | `b5d41adbbf8305ce7199062634b20b267a2fda9b08c358e2b9593bd0931b207b` | PASS |
| corrected `modules-model.md` | `d089349a1e19562c088ae4205be6289283703beac082720b4dbd2e7a05d0ee9e` | PASS |
| corrected `data-model.md` | `8b1604dd0e1d7dd066611c419b3439e8091aa5b1ff5e310108f56cadc9454720` | PASS |
| corrected `functions-model.md` | `b6f8a8b42d6c6476f1766b631e0da0d2b3017c338bdf0cfb994ba6c680e9913c` | PASS |
| corrected `tasks.md` | `51aad9dfa2e2eca2d87fe43bf9217325a5ac21784bdf00f14b98155704ca005d` | PASS |
| submission receipt | `b0c46a2ec7589e9b41129e629257a446fd7a5e87b7558b5494f89cc4dc3460c5` | PASS |
| RED handoff diff | `fa299779ed9e170c9a938303a47d6e6e51c0d16f266753ad3345380a99849e47` | PASS |
| GREEN handoff diff | `6af7a6198104f6e162236ce28b03df4c93110764b28ba02fcd6d34a9aa72d4f4` | PASS |
| gate v2 | `bc9c336bcf854f84192f4c6f62d107ff9613bfd0cb21bd8004b7aeb9f348fc1b` | PASS |
| manifest v2 | `a0dc3c5ae2505535d9e53d7f4ac44dc72f0014aec30c399f9c158a72cf336942` | PASS |
| owner GREEN gate receipt | `830ac4c484f06eee316de3d486fb6093c8ff4951d0779412cb28fd77d93a9256` | PASS |
| planning RED receipt | `4e06c82be83268e2b972f40c7f4c4745adc453b0407c40f06766c388167581cb` | PASS |
| frozen negative-controls receipt | `110367e34ce105bf0b49d70add2b6c84bc2a0022d87660c8c399f7cc6c7cfc37` | PASS |

The complete frozen packet measured 109,331 bytes / 1,932 lines; token count
was unavailable.

## Provenance, scope, and architecture

`git merge-base --is-ancestor` proves the candidate descends from the named
base through exactly two commits: the RED commit and the GREEN commit. Both
`verify-commit-handoff` checks passed. The complete changed-path set is exactly:

- `lean/KelGroups/Vote/Validate.lean` (`+29/-11`)
- `lean/KelGroups/Vote/Fold.lean` (`+14/-18`)
- `lean/KelGroups/Vote/Invariants.lean` (`+95/-236`)
- `lean/KelGroups/Vote/Tests.lean` (`+104/-13`)

All are ordinary `100644` files. There is no task stamp and no forbidden
source, toolchain, Nix, CI, docs, Haskell, or Reactivegas change. The seven
root Slice-1 blobs (`Types`, `Event`, `State`, `Fold`, `Validate`,
`Invariants`, `Tests`) match `ccdda830` exactly. Dependency direction passed
with nine positive-control imports and no `Reactivegas` import under
`lean/KelGroups/`.

`applyVoteEvent` at `Vote/Fold.lean:111-115` has one validation match whose
error branch returns `gs` exactly and whose success branch alone reaches
`effectedState` and `sweepClosures`. `validateVoteEvent` at
`Vote/Validate.lean:49-72` explicitly enumerates all six constructors with no
wildcard. However, the event effect still retains a separate cast-specific
signer authorization guard; see finding F-001.

## Invariant matrix

| Invariant | Severity | Independent inspection / control | Evidence | Verdict / row state |
|---|---|---|---|---|
| `INV-57-BOUNDARY` | BLOCKING | Validation dominates both effect and sweep, and BYPASS goes RED; nevertheless `effectedState` retains an independent cast authorization guard forbidden by the architecture. | `Vote/Fold.lean:89-96`; BYPASS log `af60de39e9198f3c17f2f6333c9189c4a71925f854c91fb5eb94d555878f651e` | **FINDING / OPEN** |
| `INV-57-NOOP` | BLOCKING | `inadmissible_is_noop` quantifies arbitrary state and error with no well-formedness premise. A runtime-only RED→GREEN probe used a state whose stale tally makes `sweepClosures` mutate it. | theorem `Vote/Invariants.lean:1007-1011`; RED `cd60312c92cc850421806468a2d10da376288fd916bb25e4dd550be23251cb4a`; GREEN `e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855` | PASS / KILLED |
| `INV-57-AUTH` | BLOCKING | `nonresponsabile_event_noop` is universal over `VoteEvent`; production witnesses use all six distinct constructors and explicitly cover all three member/role events. Values include a nonempty franchise, open question, stranger signer, and a threshold-sensitive removal. | `Vote/Invariants.lean:1017-1028`; `Vote/Tests.lean:269-320`; gate receipt `5ac10a79342d78be62ea7ca5706097175904d25eafa054412dcb4be9a6451ffc` | PASS / KILLED |
| `INV-57-EXHAUSTIVE` | BLOCKING | Event source has six constructors; validator has six explicit arms and no wildcard. Independent `auditBypass` constructor-plus-effect seed fails specifically at the authorization boundary. | checker `f0ea4974fdf7aa0e2b307594b0c2c216d51fbd1dc5c58c41f8a552ec5668a25f`; receipt `60baa9cd30fa25d93600849695680a0d2e69a26aacde1f9ed3de9962275485f3` | PASS / KILLED |
| `INV-57-NOEXPIRY` | ADVISORY-BUT-REQUIRED | `PreservesQuestionSemantics` observes exact target question value, franchise, and proposer standing rather than constructor name. A non-admin admission changes membership while preserving all three; separate non-degenerate witnesses reject franchise, target-ballot, and proposer-standing changes. | `Vote/Invariants.lean:892-946`; `Vote/Tests.lean:325-349`; instrument `19d0d4d7204ea684813e7c12770b702db425912adcb71f35bfecc70ff1b4b181` | PASS / KILLED |
| `INV-54-PARTITION` | BLOCKING | `questions_partition` retains open/closed nodup, disjointness, non-open closures, and prefix ID preservation. Silent-deletion mutant proves a lost ID. | theorem `Vote/Invariants.lean:871-888`; RED `fcbd02d28fbe5f48119714b1440aef82dd4ffaae4d1ab70e7053f86d85e8aae8` | PASS / KILLED |
| `INV-54-DISJOINT` | BLOCKING | The theorem covers open and closed `QuestionClean`. Mutant deliberately keeps a switcher in both nonempty tallies and the disjoint assertion goes RED. | theorem `Vote/Invariants.lean:855-859`; RED `a031c50de7555e425369068cb0c51d48f9203819cedb7ab0699200dc872d433c` | PASS / KILLED |
| `INV-54-NOSTALE` | BLOCKING | Every final open-map entry has verdict `open`. Mutant omits sweep on a member removal and leaves a positive question stale. | theorem `Vote/Invariants.lean:863-867`; RED `db07341b2c96639957bf94140ecb796926adb3ad0b5bf1db78a321e966efd514` | PASS / KILLED |
| `INV-54-FRANCHISE` | BLOCKING | `franchise_of_tallies` names the actual cast event and cast-time prefix; `unfranchised_cast_noop` is arbitrary-state identity. Mutant permits a standing-lost voter to switch a nonempty tally. | theorem `Vote/Invariants.lean:1163-1177`; RED `17765e37a6fd2a357f0f1b287f7e5e4c63cb25048c1a56daf0fdd912b9c9b1bf` | PASS / KILLED |
| `INV-54-POLICYFREE` | BLOCKING | The theorem depends only on threshold agreement at the actual franchise size. A hard-coded legacy threshold disagrees under zero policy and goes RED. | theorem `Vote/Invariants.lean:397-406`; RED `27c344eed98736d341dbf94d7333cab70fb3381e39964e6a7fc6dcd7c0f1c85f` | PASS / KILLED |

## Proof trust and value coverage

The focused build independently printed all nine contractual axiom sets.
They contain only `propext`, `Classical.choice`, and `Quot.sound`; neither
`sorryAx` nor `Lean.ofReduceBool` appears. A positive-control search found the
contractual theorem, while the same method found no `sorry`, bare `admit`,
custom `axiom`, `native_decide`, `sorryAx`, or `Lean.ofReduceBool` under
`lean/KelGroups/Vote`.

The controls do not collapse to shared empty/default values: authorization
uses six distinct event values on a reachable bootstrapped/open state; the
stale-state probe has two responsabili, one assent, and an already-positive
question; the R-45 witness has three responsabili and one assent; no-expiry
changes membership while retaining a nonempty ballot and separately changes
each semantic axis; inherited mutants exercise nonempty IDs, tallies,
franchises, and disagreeing threshold policies.

## Blocking findings

1. **F-001 — `INV-57-BOUNDARY`, event-local authorization remains duplicated.**
   `lean/KelGroups/Vote/Fold.lean:89-96` checks
   `isResponsabile signer gs` inside the `.cast` arm of `effectedState`.
   The frozen mandate explicitly requires `Validate` to own the exhaustive
   authorization decision and forbids a third independent standing guard in
   event-local effect code. The candidate therefore has two authorization
   mechanisms: the universal validator plus a cast-only guard embedded in the
   effect. This is currently redundant on the production path, but it is the
   prohibited architecture and can drift independently for one constructor.
   **Property class:** every event effect is authorization-free and assumes an
   already-admitted event; all signer authorization occurs only in the total
   exhaustive `validateVoteEvent` boundary. A permanent source/structural
   control should reject signer-authorization predicates inside
   `effectedState`, while the existing production rejection proofs retain the
   behavioral guarantee. Severity: BLOCKING.

## Verification receipts

| # | Command | Exit | Duration | Evidence |
|---:|---|---:|---:|---|
| 1 | immutable gate v2 on cold audit tree | 1 | 2.439s | `evidence/independent-gate-v2.log` `7b6c8f5d062b55db69b2220723fa20283c41ca62c42be91bca2af1a9c5826f06` |
| 2 | Lean 4.25.0 focused Vote build | 0 | 6.556s | `evidence/focused-vote-build.log` `3919030b7e298024cf371bff3e786c1c788a4a36363c511f7f6045f62cd1a368` |
| 3 | immutable gate v2, warm after required focused build | 0 | 99.742s | `evidence/independent-gate-v2-warm.log` `5ac10a79342d78be62ea7ca5706097175904d25eafa054412dcb4be9a6451ffc` |
| 4 | stale-state old-boundary probe | 1 expected | 1.567s | `evidence/stale-rejection-bypass-red.log` `cd60312c92cc850421806468a2d10da376288fd916bb25e4dd550be23251cb4a` |
| 5 | stale-state candidate probe | 0 | 1.541s | `evidence/stale-rejection-candidate-green.log` `e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855` |
| 6 | future-constructor surface control | 0 | 0.168s | `evidence/future-surface-negative-control.log` `60baa9cd30fa25d93600849695680a0d2e69a26aacde1f9ed3de9962275485f3` |

Complete accounting is in `handoffs/build-ledger.md`.

## Honest limits

- The immutable gate runs external instruments before its own build. On the
  pristine no-olean audit tree it failed with `unknown module prefix
  'KelGroups'`. The separately required focused build established the candidate
  oleans, after which the unchanged gate passed. This is disclosed as an
  ordering/readiness limitation, not used to weaken F-001 or any semantic row.
- Mutation claims are limited to the named controls above; no exhaustive
  mutant-class claim is made.
- This report judges the Lean model and the frozen issue mandate, not
  Reactivegas/KelGroups end-to-end composition, which is explicitly out of
  scope.

## Final verdict

**FINDINGS**
