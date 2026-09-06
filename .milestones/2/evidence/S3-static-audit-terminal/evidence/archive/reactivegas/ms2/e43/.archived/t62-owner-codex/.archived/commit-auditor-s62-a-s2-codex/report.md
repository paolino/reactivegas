# Commit audit — reactivegas #62 S62-A, submission 2 (FINAL)

Verdict: **PASS** (`0` findings; F-01…F-04 closed; `8` PASS rows,
`0` FINDING rows, `0` BLOCKED rows).

This is an independent audit recommendation for candidate
`000ff76a52b3972f232ef18fbeaa96ac6a6b0f20`. The ticket owner retains the
acceptance decision. Submission 2 is final; a further repair/audit round is
not authorized by this campaign.

## Identity, provenance, and scope

- Scope: repair delta
  `6fa3ca77029086e39a19ff43edb1b6cdf56bc81b..000ff76a52b3972f232ef18fbeaa96ac6a6b0f20`,
  named findings F-01…F-04, and the complete original eight-row S62-A
  regression matrix.
- Plan/base: `fd5c8e036d92e3425f836f6bbbeeb68a3a9fee1e`; rejected submission 1:
  `6fa3ca77029086e39a19ff43edb1b6cdf56bc81b`; candidate tree:
  `1cd780494fdbe98718f6ef480fcb6753a3e34226`.
- Audit worktree was detached and clean before, between, and after all runs.
  Auditor pane `%214`, ticket-owner pane `%195`, and parked Grok owner pane
  `%206` are distinct and share ticket window
  `reactivegas:9` / `reactivegas-e43-t62-one-membership`.
- Owner receipt SHA-256:
  `68a96657a46a3b3ba7e470859f6a505849bef300d7fff616aded6b860c851c7b`
  (claims only). Prior report SHA-256:
  `047dae3ed0af81dc2c2623878d78c49f425339a4c9942c25f86edcefc824900d`.
- `green-s2.diff` SHA-256:
  `c4d7584ed7e129297b5d08504506e02ca9a7c89a8ad3d54021ef000877e760d9`;
  manifest SHA-256:
  `99933eff860b1ac8c54fd558679d6c5aed38d186a8b367ca124859e19ecbcd03`.
  The manifest is structurally valid and `verify-commit-handoff` independently
  proved the candidate contains exactly that repair over `6fa3ca7`.
- The repair changes exactly six regular `100644` files, all within the Lean
  fence: `KelGroups/Invariants.lean`, and Reactivegas `State`, `Step`, `Trace`,
  `TraceTests`, and `Types`. Planning/gate/history paths are unchanged and no
  remote-tracking branch contains the candidate.
- Frozen gate SHA-256:
  `2fd98ffb762f219b9e151413c2b9acf2c5e4eb71e7949341d088a5d85f7c57e8`;
  common gate SHA-256:
  `32be7416c3807c5026d0c6ce243593e9106f31800efa5cc77b70974c95972177`.
  All six frozen planning artifacts were read in full and are unchanged from
  the plan/base (41,502 bytes total).
- Ceiling raises: `1/2`. This audit used the submission-2 brief's explicit
  `3/3` substantive-run allowance (`cache=cold,warm,warm`). The ticket-wide
  mutation campaign remains open only for deferred S62-B/C rows; this final
  S62-A audit does not close or reinterpret those rows.

## Prior findings as property classes

| Finding | Verdict | Property-class judgment and evidence |
| --- | --- | --- |
| F-01 payload completeness | **CLOSED** | `Reactivegas.State` now contains economy plus membership-free `VoteState` (`lean/Reactivegas/State.lean:21-36`; `lean/KelGroups/Vote/State.lean:48-58`). Every concrete `step` success is a record update of the input payload, and `Reactivegas.apply` carries that result. The rooted witness uses a nonempty question, two distinct members, and a donation that moves economy while preserving votes and members (`lean/Reactivegas/Step.lean:241-265`). Independent `checkPayloadCarry` passed. No member/franchise store was added. |
| F-02 reserved comune boundary | **CLOSED** | The concrete production boundary checks `productionWellFormed` before calling the generic substrate transition and checks the result again (`lean/Reactivegas/Step.lean:197-231`). `boot` rejects a member association keyed by `comuneId` (`:204-210`). The negative witness constructs `comuneId` as an admin and proves both boot and signed donation refusal (`:294-313`), while the independent probe also requires a valid non-comune boot and successful app transition. The only concrete call to `KelGroups.applyIntegratedEvent (integration …)` is inside this guard. The unguarded generic `KelGroups.applyIntegratedEvent`/`foldIntegrated` remain policy-free substrate primitives, not the declared Reactivegas root. Direct admission remains deferred. |
| F-03 mutation sensitivity | **CLOSED** | `Reactivegas.memberWritingApply` is an actual transition-result mutant that writes `result.state.members`, not an alternate expected fixture (`lean/Reactivegas/Step.lean:267-286`). Its non-vacuity check requires changed members, changed payload, and preserved nonempty vote payload; the rooted GREEN theorems are in `Reactivegas.Step`, imported by `Reactivegas.lean`, and were elaborated by the frozen gate's full-CI leg (`:288-292`). The gate-visible TraceTests names alias those rooted checks (`lean/Reactivegas/TraceTests.lean:1007-1034`). Independent false-preservation instrument `de5375f9db22f88226f4adcc7d82a022f2f77e8cd0322a42a76d7ac0065c867a` went RED because Lean proved the claimed equality false, then GREEN instrument `82b304a5c37e46b809ea3af95a013711d78fba7e177a924199ac4606c6680787` proved the mutant actually executed. |
| F-04 executable production fold | **CLOSED** | The production `step`, `appFold`, `integration`, and `apply` receive `BackdonateAuth` explicitly (`lean/Reactivegas/Step.lean:47-54,179-195,217-231`); they do not select or reference the unresolved #47 policy. The independent probe executed the production root with both true and false authorization and observed success/refusal, so the parameter is executable and genuinely consulted. Fresh axiom output reports no axioms for `Reactivegas.appFold`, `Reactivegas.apply`, the rooted preservation theorem, comune theorem, or member-writing mutant. It reports `[sorryAx]` only for `backdonateAuthorized` and `stepEvent`, whose only edge is the isolated legacy wrapper (`:150-175`). |

## Complete eight-row S62-A audit matrix

| # | Contract | Verdict | Judgment |
| ---: | --- | --- | --- |
| 1 | One store (`INV-62-ONE-STORE`) | **PASS** | `GroupState.members` remains the sole writable current member relation; `Member.roles` is the sole current role assignment. `GroupView.members` is the specified read-only projection. Reactivegas state has only economy plus membership-free votes, and the frozen duplicate-field selftest killed `users`, `responsabili`, and vote-`members` seeds. |
| 2 | Payload-only (`INV-62-PAYLOAD-ONLY`) | **PASS** | `IntegratedAppFold` takes signer/pre/post views and returns only `Except AppError AppState` (`lean/KelGroups/Integration.lean:45-58`). Concrete State is complete and `applyIntegratedEvent` replaces only `appFold` (`:115-130`). F-01 is closed without adding a second group/member result. |
| 3 | One key (`INV-62-ONE-KEY`) | **PASS** | All participant/account/signer/proposer/voter identities are `KelGroups.Key`; no `Reactivegas.UserId` or identity bridge exists. `CollId : Nat` remains only an economic identifier. The concrete root refuses the reserved key before authorization, closing F-02. |
| 4 | Vote boundary | **PASS** | `VoteState` stores only open questions and closure records; franchise and verdict derive from explicit `GroupView` (`lean/KelGroups/Vote/State.lean:51-75`). The three vote-local membership/role constructors remain temporarily present but are exhaustively refused (`lean/KelGroups/Vote/Validate.lean:57-76`), as allowed until S62-B. Embedding `VoteState` in the app payload adds no membership copy. |
| 5 | Historical bytes (`INV-62-HISTORICAL`) | **PASS** | The exact `baseEnacted_threshold_met` block at `lean/Reactivegas/Composition.lean:111-124` is byte-identical to `c50f5275`; both extracted blocks hash to `ab9b4aadb52fbbcdb62bb8de39f62acbc76f0ffbfa4c8eeb5d1d79f6fff334f4`. Neither concrete production root nor app fold calls `applyEventDetailed`. |
| 6 | App preservation | **PASS** | Generic theorem `app_event_preserves_members` remains production-definition-bound (`lean/KelGroups/Invariants.lean:568-586`). The concrete production witness is non-vacuous, preserves a nonempty vote payload, and is paired with an actual member-writing mutant, an independently observed RED false claim, and rooted GREEN theorems. |
| 7 | Transition routing | **PASS** | `direct`, `propose`, and `approve` still return distinct `baseUnavailable` errors (`lean/KelGroups/Integration.lean:120-130`). The four legacy Reactivegas membership/role constructors refuse in `stepEvent` (`lean/Reactivegas/Step.lean:150-175`), and the three vote-local constructors refuse. The repair adds no admission, proposal restriction, cleanup/recompute hook, or base-transition V-3 behavior from S62-B/C. |
| 8 | Scope/trust | **PASS** | Exact six-file repair, authorized modes/paths only, clean detached tree, no new proof escape hatch, historical bytes intact, full CI GREEN. Fresh axiom and execution evidence proves the production root is independent of the inherited legacy `sorry`; the repair introduced no gate/spec/task edit or premature later-slice semantics. |

## Repair-delta regression and production call graph

Every repair file has a finding-scoped reason: `State` and `Trace` carry and
serialize the vote payload (F-01); `Types` declares the reserved-root boundary
(F-02); `Step` implements F-01…F-04 and their rooted witnesses;
`TraceTests` exposes the F-03 rooted aliases to the frozen scanner; and the
`KelGroups.Invariants` comment distinguishes the generic theorem surface from
the concrete guarded root. No unrelated behavioral widening was found.

The concrete path is:

```text
Reactivegas.apply
  -> productionWellFormed pre-state (reject comune member)
  -> KelGroups.applyIntegratedEvent (Reactivegas.integration θ auth)
       -> canonical signer membership check
       -> Reactivegas.appFold auth
            -> Reactivegas.step ... auth
                 -> State { economy, votes }
  -> productionWellFormed post-state
```

Static call-graph search finds no other concrete use of `Reactivegas.integration`
or `KelGroups.applyIntegratedEvent (integration …)`. The separate legacy path
is `stepEvent -> step ... backdonateAuthorized`; historical Composition uses
`applyEventDetailed`. The vote functions remain explicit-view substrate
functions, with production base recomputation intentionally deferred to S62-B.

## Test/value coverage and frozen instruments

- Positive payload/root control: two distinguishable members, one nonempty
  open question, successful donation, changed economy, identical votes and
  members, and `change = none`.
- Reserved-key negative control: a concrete association
  `(comuneId, admin-member)` is rejected by both `boot` and `apply`; a valid
  non-comune aggregate succeeds, ruling out blanket rejection.
- Mutation control: the shipped mutant writes the transition result, and the
  false-preservation theorem fails specifically because the resulting members
  differ. The GREEN detector also requires the payload to have moved.
- Executability control: backdonation succeeds with explicit `true` auth and
  rejects with explicit `false` auth on the same production state; balances,
  votes, and members are checked.
- Frozen instruments:
  `instruments/false-preservation.lean` SHA-256
  `de5375f9db22f88226f4adcc7d82a022f2f77e8cd0322a42a76d7ac0065c867a`;
  `instruments/repair-properties.lean` SHA-256
  `82b304a5c37e46b809ea3af95a013711d78fba7e177a924199ac4606c6680787`.

## Verification receipts

| # | Exact wrapped command | Expected/actual exit | Duration | Cache | Command SHA-256 | Evidence SHA-256 / path |
| ---: | --- | ---: | ---: | --- | --- | --- |
| 1 | `/tmp/reactivegas/ms2/e43/t62-owner-codex/gates/gate-s62-a.sh` | `0 / 0` | `96321 ms` | cold | `1669fd2f74bd7d71440df3acda4360e8709c6a25fcc23523ca4628480e43ae32` | `71c8588c120d7a68ec267bab53e3360a4f084bde805d0411d1a5db835274bcd4` / `evidence/slice-gate-s62-a.log` (640754 bytes, 15180 lines) |
| 2 | `nix develop --quiet -c bash -lc 'cd lean && lake env lean /tmp/reactivegas/ms2/e43/t62-owner-codex/commit-auditor-s62-a-s2-codex/instruments/false-preservation.lean'` | `1 / 1` (intended RED) | `10839 ms` | warm | `6afc1e5e6f3519910a2096f367f50a5d7f5d46301d88efcbda23b845acfde0cd` | `060790281562dfd07796ba81777cd5d47803e5a999f59d1686a79d542603bee0` / `evidence/false-preservation.log` (347 bytes, 6 lines) |
| 3 | `nix develop --quiet -c bash -lc 'cd lean && lake env lean /tmp/reactivegas/ms2/e43/t62-owner-codex/commit-auditor-s62-a-s2-codex/instruments/repair-properties.lean'` | `0 / 0` | `2648 ms` | warm | `bb640daca32b167a2bdf451504986a602d1d6db0d06b1c44ecb9ab0e04d66e32` | `7dbd79ca45d37203cc1cac19b2b783be6e66d7df13d801a2da56725d532faddf` / `evidence/repair-properties.log` (494 bytes, 8 lines) |

Run 1 ends `SLICE-GATE S62-A PASS rows=4 full-ci=pass`; full CI built
27 Lean jobs and printed no axiom dependencies for the concrete production
definitions/theorems. Run 2's only error says `decide` proved the alleged
member equality false. Run 3 prints
`AUDIT-PROPERTIES PASS payload=carried comune=refused mutant=applied
backdonate-auth=explicit`, followed by the expected production-none / legacy-
`sorryAx` split. Free checks also passed: four direct gate-row readiness calls,
duplicate-field seeded selftest, `git diff --check`, historical `cmp`, manifest
validation, exact commit handoff, path/mode checks, and positive-controlled
absence scans.

## Findings, residuals, and onward discoveries

- Blocking findings: **None**.
- Advisory residuals in S62-A: **None**.
- Onward discoveries: **None**. Deferred S62-B/C behavior is mandated future
  scope, not a discovery or residual from this repair.

## Recommendation

Recommend **PASS** for candidate
`000ff76a52b3972f232ef18fbeaa96ac6a6b0f20` as S62-A submission 2. All four
named property classes are closed, every original audit row passes, the repair
has no later-slice semantic regression, and the production root is executable
and axiom-clean while the allowed inherited debt remains isolated to the
legacy path.
