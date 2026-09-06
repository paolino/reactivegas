# Commit audit — reactivegas #62 S62-A, submission 1

Verdict: **FINDINGS** (`4` findings; `4` PASS rows, `4` FINDING rows, `0` BLOCKED rows).

Candidate `6fa3ca77029086e39a19ff43edb1b6cdf56bc81b` is mechanically exact and the frozen S62-A gate is GREEN, but the candidate does not satisfy the complete S62-A semantic/trust contract. This is an independent audit recommendation only; the ticket owner retains the acceptance decision.

## Immutable identity and evidence boundary

- Original base: `c50f5275a42453ebc87a0c7011b3d8470fba4006`
- Plan/base: `fd5c8e036d92e3425f836f6bbbeeb68a3a9fee1e`
- RED: `d4c7b92a6da1c7d80fc9766faa0dae852b58551c`
- Provenance/parent: `834c12358f127f4e792a3b47ce41f745bf3e5cc8`
- Candidate: `6fa3ca77029086e39a19ff43edb1b6cdf56bc81b`
- Candidate tree: `4fa3b036b607009dd18842a16377d03b8a4d15e5`
- Detached audit worktree: clean before and after the audit (`git status --porcelain=v1 --untracked-files=all` empty).
- `verify-commit-handoff` independently returned: `OK: 6fa3ca7... contains exactly the frozen candidate and allowed extras.` All nine candidate blobs match `green.diff.manifest`.
- Candidate diff SHA-256 from provenance: `ac46ecf92cbd996fb1330ee147b85e903bc6201dc605d4b92a3afdf0d8f1c8ab`, equal to the owner `green.diff` hash.
- Owner receipt SHA-256: `a7a0538ff3146511fc93d6f2aa4c59b9df42c243026cae65f06530c39093f8a8` (treated as claims).
- Frozen slice gate SHA-256: `2fd98ffb762f219b9e151413c2b9acf2c5e4eb71e7949341d088a5d85f7c57e8`.
- Frozen common gate SHA-256: `32be7416c3807c5026d0c6ce243593e9106f31800efa5cc77b70974c95972177`.

The brief also names a ticket `research` artifact. `git ls-tree -r fd5c8e... specs/62-one-membership-model` contains only `spec.md`, `plan.md`, `modules-model.md`, `data-model.md`, `functions-model.md`, and `tasks.md`; no research file exists in the frozen planning tree or ticket runtime. This input discrepancy did not block the audit because the six available authoritative artifacts define the evaluated contract.

## Eight-row audit matrix

| # | Contract | Verdict | Judgment and primary evidence |
|---|---|---|---|
| 1 | `INV-62-ONE-STORE` | **PASS** | `KelGroups.GroupState.members` is the only state-owned current membership relation (`lean/KelGroups/State.lean:11-15`), and `Member.roles` is its role assignment (`lean/KelGroups/Types.lean:25-29`). `GroupView.members` is the intended read-only projection (`Types.lean:74-110`). `Reactivegas.State` has only `conti`, `casse`, and `collections` (`Reactivegas/State.lean:20-27`); `VoteState` has only `openQuestions` and `closed` (`KelGroups/Vote/State.lean:48-54`). Broad field/writer scans found no app/vote membership store. |
| 2 | `INV-62-PAYLOAD-ONLY` | **FINDING** | The type boundary itself is sound: `IntegratedAppFold` receives signer and pre/post `GroupView` and returns `Except AppError AppState` (`KelGroups/Integration.lean:45-58`); `applyIntegratedEvent` replaces only `appFold` (`:115-130`); concrete `Reactivegas.appFold` returns `State` (`Reactivegas/Step.lean:170-184`). **F-01:** the concrete `State` omits the vote payload that S62-A explicitly requires, so the production aggregate is economy-only rather than the specified app payload. |
| 3 | `INV-62-ONE-KEY` | **FINDING** | All inspected participant/account/signer/proposer/voter fields use `KelGroups.Key`; no `Reactivegas.UserId` or `Nat`/`String` identity bridge remains. `CollId : Nat` is an economic identifier, as allowed. `comuneId : Key` is an account key (`Reactivegas/Types.lean:13-27`). **F-02:** no guarded integrated boot prevents that reserved key from being supplied as a canonical member/admin, after which the production authorization path accepts it. |
| 4 | S62-A vote boundary | **PASS** | `VoteState` is membership-free (`KelGroups/Vote/State.lean:48-54`). Franchise and verdict derive from explicit `GroupView` (`:60-95`); validation reads the same view and refuses all three retained membership events (`KelGroups/Vote/Validate.lean:53-76`); fold/sweep take the view explicitly (`KelGroups/Vote/Fold.lean:62-78,114-134`). The franchise-change witnesses explicitly compare pre/post fixture views and disclaim base reachability (`KelGroups/Vote/Tests.lean:17-23,149-189,301-318`), as required for S62-A. |
| 5 | `INV-62-HISTORICAL` | **PASS** | Process-substitution `cmp` of the exact `baseEnacted_threshold_met` declaration against `c50f5275` exited `0`; the block is at `Reactivegas/Composition.lean:111-124`. The new call graph below has no edge to `applyEventDetailed`; historical uses remain in `Composition`/legacy proof surfaces. |
| 6 | App preservation | **FINDING** | `AppEvent` has exactly the fourteen economic constructors, no author, and no membership constructor (`Reactivegas/Types.lean:75-93`). The concrete production check runs `IntegratedEvent.app (donate 30)`, requires payload movement, members equality, and no base change (`Reactivegas/TraceTests.lean:1028-1055`); the generic theorem directly unfolds `applyIntegratedEvent` (`KelGroups/Invariants.lean:557-584`). The targeted TraceTests run was GREEN (`34` checks, `0` failures). **F-03:** the claimed negative control does not execute a member-writing fold/transition mutant, and the frozen gate only scans for a definition name. |
| 7 | Transition routing | **PASS** | New direct/propose/approve routes explicitly return `baseUnavailable` (`KelGroups/Integration.lean:120-130`). The legacy `stepEvent` refuses all four retained Reactivegas membership/role constructors and delegates only fourteen economic events (`Reactivegas/Step.lean:145-168`). Vote-local membership constructors are likewise refused. No S62-B direct-admission, proposal, cleanup/recompute, or base V-3 reachability is claimed or implemented. |
| 8 | Scope/trust | **FINDING** | Scope, provenance, modes, planning files, historical bytes, and local remote-ref checks pass: candidate parent is exactly `834c123...`; exactly nine authorized Lean paths changed; no task/gate/planning path changed; no remote-tracking branch contains the candidate. No new custom `axiom`, `admit`, `native_decide`, or `Lean.ofReduceBool` was found. **F-04:** `Reactivegas.appFold`, the new production fold, depends on inherited `sorryAx` through `backdonateAuthorized`; full CI warns but accepts it. This violates the brief's no-proof-escape-hatch trust row. |

## Findings

### F-01 — Production `AppState` omits the required vote payload

- **Files/lines:** authoritative S62-A plan `specs/62-one-membership-model/plan.md:43-55`, especially `:47-49`; data model `data-model.md:40-53`; candidate `lean/Reactivegas/State.lean:20-32`; concrete instantiation `lean/Reactivegas/Step.lean:170-184`; separate vote payload `lean/KelGroups/Vote/State.lean:48-58`.
- **Observed defect:** the plan says S62-A makes Reactivegas state “a payload with economy plus vote questions/closures.” The data model lists `conti`, `casse`, `collections`, vote open questions, and vote closures as the exact app payload. Candidate `State` contains only the first three, and `Reactivegas.integration` therefore instantiates `GroupState State` with no place for `VoteState`.
- **Concrete failure scenario:** open questions/closures can exist only in a separate `VoteState`. An `IntegratedEvent.app` transition carries and returns `GroupState Reactivegas.State`, so it cannot preserve, observe, or transport those questions. A later base hook cannot atomically recompute that missing payload without changing the S62-A aggregate shape. This finding demands only the S62-A payload boundary, not S62-B cleanup/recomputation behavior.
- **Property class:** architectural boundary / aggregate state completeness.
- **Instrument:** source/call-graph finding; no build mutation required.

### F-02 — Reserved comune exclusion is fixture/predicate-only, not production boot-bound

- **Files/lines:** requirement `specs/62-one-membership-model/data-model.md:28-38`; `comuneId` and its fixture-only comment `lean/Reactivegas/Types.lean:16-21`; arbitrary production input and arbitrary fold initial state `lean/KelGroups/Integration.lean:115-145`; member/admin checks `:124-130`; economic admin guard and donation effect `lean/Reactivegas/Step.lean:27-29,83-88`; separate predicate reachability guard `lean/Reactivegas/Predicates.lean:90-97`.
- **Observed defect:** the only exclusion is a proposition required by the legacy economic `Reach.boot` and by selected fixtures. Neither `applyIntegratedEvent` nor `foldIntegrated` requires a guarded initial aggregate; both accept any `GroupState State`.
- **Concrete failure scenario:** construct the public initial aggregate with `members = [(comuneId, Member comuneId ... [adminRole])]`, then sign `IntegratedEvent.app (AppEvent.donate 1)` as `comuneId`. `applyIntegratedEvent` sees a member; `Reactivegas.appFold` sees an admin; the donation branch succeeds. Thus the reserved account key can be a member/admin/signer in the production model even though direct admission is correctly deferred to S62-B.
- **Property class:** production reachability / reserved-identity authorization invariant.
- **Instrument:** the attempted probe is `/tmp/reactivegas/ms2/e43/t62-owner-codex/commit-auditor-s62-a-codex/instruments/audit-probes.lean`, SHA-256 `e5816553683c78bef19690ed2c7937d8296e3065ef75f31afcae332a31d25a13`. Its semantic guards are **not accepted as executed evidence** because the command exited `1` on the unrelated production `sorry` (F-04); the failure scenario above follows directly from the enumerated production branches.

### F-03 — App-preservation negative control is not a member-writing fold mutant and is not gate-wired

- **Files/lines:** candidate fixture mutation and control `lean/Reactivegas/TraceTests.lean:1037-1068`; frozen row `/tmp/reactivegas/ms2/e43/t62-owner-codex/gates/gate-common.sh:96-99`; full CI shape `justfile:56-70`.
- **Observed defect:** `appPreservationGroupMutant` merely removes one member from an expected fixture. `checkAppMembersPreservationMutant` still calls the unmodified `appPreservationResult` and asks that its members differ from that fixture. It never calls a mutated app fold or a mutated `applyIntegratedEvent`. The frozen row checks only that `app_event_preserves_members` and `checkAppMembersPreservation` names exist. `just ci` runs `lake build`, while `TraceTests.lean` explicitly is not rooted by the umbrella, so the frozen gate does not execute this candidate control at all.
- **Concrete failure scenario:** replace `checkAppMembersPreservationMutant` with `true`, or remove `app_members_preservation_mutant_caught` while leaving `checkAppMembersPreservation` defined. The frozen S62-A gate still passes. More fundamentally, the current GREEN control supplies no RED receipt from a definition mutant that writes `result.state.members`, which is the exact T6214 requirement.
- **Property class:** mutation sensitivity / negative-control and gate wiring.
- **Instrument:** source inspection is decisive. The audit probe contains a real member-writing result mutant beside a restatement of the candidate control, but its semantic guards were not accepted because F-04 made the command exit `1`; no false PASS is claimed from the probe's static marker.

### F-04 — The new production app fold depends on `sorryAx`

- **Files/lines:** `lean/Reactivegas/Step.lean:31-42` (`backdonateAuthorized := sorry`) and `:170-184` (`Reactivegas.appFold` calls `step`); candidate explanation `lean/Reactivegas/TraceTests.lean:17-38,1023-1025`; frozen gate evidence line `15030` warns `declaration uses 'sorry'`.
- **Observed defect:** the `sorry` predates this candidate (`git blame` attributes the declaration to `f2fb7bfc`), but S62-A newly places the entire `step` function behind the production `Reactivegas.appFold`. It is therefore a production dependency, not merely unrelated historical proof debt.
- **Concrete failure scenario:** compiling/evaluating an imported production app-fold probe fails with `cannot evaluate code because 'backdonateAuthorized' uses 'sorry'`; the backdonate branch itself has no proved or executable authorization result. Full `lake build` remains GREEN because Lean treats `sorry` as a warning.
- **Property class:** proof trust / runtime executability / escape hatch.
- **Instrument:** `/tmp/reactivegas/ms2/e43/t62-owner-codex/commit-auditor-s62-a-codex/instruments/audit-probes.lean`, SHA-256 `e5816553683c78bef19690ed2c7937d8296e3065ef75f31afcae332a31d25a13`; receipt evidence `/tmp/reactivegas/ms2/e43/t62-owner-codex/commit-auditor-s62-a-codex/evidence/audit-probes-final.log`, SHA-256 `697bb55b8c7f1f7b96d9250a9b4b9a33681acba50f970dd8abd1a390f50f68c1`, exit `1`. Its `#print axioms` output reports `backdonateAuthorized` and `Reactivegas.appFold` depend on `[sorryAx]`; it reports the generic `KelGroups.app_event_preserves_members` theorem depends only on allowed `[propext]`. The log's static `AUDIT-PROBES PASS` line is explicitly discarded because earlier guards errored and the command exited nonzero.

## Production call-graph judgment

The actual S62-A app path is:

```text
IntegratedEvent.app AppEvent
  -> KelGroups.applyIntegratedEvent
       -> membership validation against groupView gs
       -> integration.appFold
            = Reactivegas.appFold
              -> Reactivegas.step
                   -> Reactivegas.State (economy only)
       -> { gs with appFold := appState }, change := none
```

This path is canonical for members and does not call `stepEvent`, `KelGroups.applyEventDetailed`, or the historical `AppFold α`. The three base routes reject. The fourteen-constructor `AppEvent` is clean and signer-free.

The vote path remains separate:

```text
explicit GroupView + separate VoteState
  -> validateVoteEvent / applyVoteEvent / foldVote / sweepClosures
```

That separation is acceptable for the S62-A vote-view unit witnesses, but the absence of `VoteState` from concrete `Reactivegas.State` is F-01 because the authoritative S62-A aggregate requires economy plus vote payload.

The legacy path is isolated from the new app path:

```text
legacy Reactivegas.Event
  -> stepEvent (four membership constructors reject; fourteen economics delegate)
  -> legacy invariants/trace only

Reactivegas.Composition historical evidence
  -> KelGroups.applyEventDetailed / separate foldVote witness
```

## Negative-control judgment

The generic theorem is meaningful, directly production-bound, and has a non-vacuous successful donate witness whose app payload changes. Its reported axiom set is allowed (`[propext]`). The candidate's positive check independently elaborated in the targeted TraceTests run.

The named mutant control does **not** meet T6214: it perturbs an expected member list, not the member-writing production definition. There is no executed, hash-bound RED receipt showing a member-writing `appFold`/`applyIntegratedEvent` mutant fails. The frozen row is additionally presence-only and does not build `TraceTests.lean`. Therefore a GREEN frozen gate is not evidence of mutation sensitivity for this property.

The frozen `verify-negative-controls.sh` was inspected (SHA-256 `afd2830709e9985eb54789dc23c81de85499275942df174c5a582fa42e80aa5c`). It is a baseline-row/seed verifier that writes to the ticket owner's evidence directory; running it on the GREEN candidate would be both semantically inapplicable and outside this auditor's instrumentation root, so it was not executed.

## Exact verification receipts

### Charged substantive runs (budget `3/3`)

| # | Exact wrapped command | Exit | Duration | Command SHA-256 | Evidence SHA-256 | Evidence |
|---|---|---:|---:|---|---|---|
| 1 | `/tmp/reactivegas/ms2/e43/t62-owner-codex/gates/gate-s62-a.sh` | `0` | `96535 ms` | `1669fd2f74bd7d71440df3acda4360e8709c6a25fcc23523ca4628480e43ae32` | `333f5aaaa86a3a00293eb715564b4febea16a0fa18b7010db2760d1f9b0991c5` | `evidence/slice-gate-s62-a.log` (`640371` bytes, `15176` lines) |
| 2 | `nix develop --quiet -c bash -lc 'cd /code/reactivegas-issue-62-audit-s62-a/lean && lake env lean Reactivegas/TraceTests.lean'` | `0` | `11259 ms` | `33534b3458cc9d592cc1994391dc1c51376c1d1c98f2037fea0c29b4678ebfe3` | `cf1503c157a272d3cc1f3a5027db5195d8c1057ed7946c0217046f9b5dcefeab` | `evidence/tracetests.log` (`11802` bytes, `9` lines; `checks=34 failures=0`) |
| 3 | `nix develop --quiet -c bash -lc 'cd /code/reactivegas-issue-62-audit-s62-a/lean && lake env lean /tmp/reactivegas/ms2/e43/t62-owner-codex/commit-auditor-s62-a-codex/instruments/audit-probes.lean'` | `1` | `2616 ms` | `8bf222aac23334d9608d8097b01e24aba28e86020eb76d2cf0e2e7bfcc7e4f31` | `697bb55b8c7f1f7b96d9250a9b4b9a33681acba50f970dd8abd1a390f50f68c1` | `evidence/audit-probes-final.log` (`883` bytes, `7` lines) |

Run 1 ends `SLICE-GATE S62-A PASS rows=4 full-ci=pass`; all four frozen rows pass. Run 2 independently elaborates the unrooted TraceTests harness. Run 3 is an intentional audit probe whose nonzero outcome is reported, not softened; it establishes F-04's executability/trust failure and does not establish its own semantic guard claims.

Two uncharged setup attempts are preserved but not cited as semantic evidence: `evidence/audit-probes.log` exited `127` before Lean (`lake` outside Nix), and `evidence/audit-probes-nix.log` exited before candidate elaboration because the detached tree had no module cache prior to the frozen gate. They did not consume a substantive build result.

### Free/read-only checks

- Four direct common-gate row readiness calls: all PASS; duplicate seed selftest printed `FALSIFY-OK ... users,responsabili,vote-members`.
- Historical theorem process-substitution `cmp`: exit `0`.
- `git diff --check`: exit `0`.
- `verify-commit-handoff 6fa3ca7... green.diff`: exit `0`.
- Nine manifest blob checks: `9/9` PASS.
- Current full-scope escape scan: exactly one executable hit, `Reactivegas/Step.lean:41 ... := sorry`; no custom axiom declaration or other escape spelling.

## Recommendation

Do not accept submission 1 as S62-A. A repair submission should, without implementing S62-B behavior early:

1. give the concrete Reactivegas app payload its required membership-free vote questions/closures component;
2. bind production integrated boot/initial reachability so `comuneId` cannot be a member, admin, signer, proposer, or voter;
3. replace the comparator-fixture “mutant” with an actually executed member-writing production-definition mutant and ensure the parent-owned frozen acceptance signal runs it; and
4. remove or isolate the inherited `sorry` so the new production `Reactivegas.appFold` has no `sorryAx` dependency and is executable.

Because the gate is parent-owned and frozen, any acceptance-control revision belongs to the ticket owner; the candidate must not repair the frozen gate itself.
