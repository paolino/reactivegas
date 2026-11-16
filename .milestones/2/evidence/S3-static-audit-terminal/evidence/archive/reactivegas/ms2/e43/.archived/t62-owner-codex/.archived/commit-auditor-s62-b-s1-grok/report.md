# Commit Audit

- Submission: 1 / 2
- Base (S62-A): `7573293972264b00a8c3af27a19bfb832002b1d3`
- RED: `55ec53e93993b5069fce616b3cfc601b256cd35d`
- Candidate: `b7c02fdc581f21acbd7a172c5bca918f81eb2739`
- Tree: `382e9cb6ed0e7f3f0aa78c4243409dfec99ebbe0`
- Mandate artifacts: spec `de23962b`, plan `adbb19cd`, tasks `61ea72c8`, functions `2965fce0`, data `bcd72fa2`, modules `614068ca`
- Scope: FULL `7573293..b7c02fd` (S62-B / T6220–T6226 only)
- Verdict: PASS
- Audit loop: submission `1/2`; next submission `ALLOWED`
- Ceiling raises: `1/2` (owner lane; this audit raised none)
- Campaign: OPEN — ended by none; one independent killing mutant this round
- Builds: this audit `3/3` charged, `cache=cold` gate then `cache=warm` probe/mutant
- Path fence: 20 files, all `lean/KelGroups/**` or `lean/Reactivegas/**`. `base→candidate` diff sha256 `a4ef5188afb0fc4f8bb24d3bafad0155c50dc0b1cf84e9b0232e592e7fdbbb4c`. GREEN handoff sha256 `207c0b3257f2bb24815e46a57495e720016f914599f575eecee9d6e72c8a760c` matches `RED..HEAD`.

Worktree `/code/reactivegas-issue-62-audit-s62-b-s1` was detached, clean, and at the exact candidate before and after every command.

## Invariant matrix

| Row | Severity | Verdict | Row state | Proof / evidence |
|---|---|---|---|---|
| ATOMIC-HOOK | BLOCKING | PASS | OPEN | `KelGroups.base_change_runs_hook` (`lean/KelGroups/Invariants.lean:831`) exhaustive over `IntegratedEvent`; every successful reported change is `commitBaseChange` (`Integration.lean:139-144`) which runs the hook on `groupView pre` / `groupView post` and discards `post` on `.error`. Hook refusal is atomic: `checkHookRejectionIsAtomic` (`Reactivegas/Invariants.lean:1379-1384`). Axioms: `[propext]`. |
| DIRECT-ADMIT | BLOCKING | PASS | OPEN | `validateDirectAdmission` (`Validate.lean:142-149`): admin, then reserved (own identity), then duplicate. Reachable: `checkAdminAdmissionReachable` (`Invariants.lean:1243`), `checkNonAdminAdmissionRefused` (`1255`, exact `.notAnAdmin`), `checkComuneAdmissionRefused` (`1264`, `.reservedKey`), `checkDuplicateAdmissionRefused` (`1271`). `direct_admission_requires_admin` / `non_admin_admission_is_noop`. Gate row PASS is not this row's proof (Q-001). |
| PROPOSAL-RESTRICTION | BLOCKING | PASS | OPEN | `Reactivegas.Proposal` is `{departure, changeRoles}` (`Types.lean:113-116`). `proposalMutation` / `proposalDigest` (`Step.lean:265-275`) are wildcard-free onto `BaseMutation` (no admission constructor). Gate `G62-B-NO-INTRODUCE` selftest: `FALSIFY-OK proposal-introduction seed=introduceMember`. |
| NO-APP-MEMBERSHIP | BLOCKING | PASS | OPEN | `AppEvent` is the fourteen economic constructors (`Types.lean:74-89`); no `addUser`/`electResponsabile`/`removeResponsabile`/`removeMember`. `VoteEvent` is `{openQuestion, cast, renounce}` (`Vote/Event.lean:23-27`); no `admitMember`/`removeMember`/`setRoles`. `rg` absence confirmed on the candidate. Signer is the fold argument, not an event field. |
| ONE-INSERTION-PATH | BLOCKING | PASS | KILLED | `membership_growth_is_direct_admission` (`KelGroups/Invariants.lean:751-797`) cases the whole `applyIntegratedEvent` graph: app preserves members; propose/approve use `tryEnactBase_preserves_absence` / `enactMutation_preserves_absence`; a direct admit of a *different* key cannot grow `key`. Axioms: `[propext]`. Independent mutant (below) reddens the voted arm. Constructor count is not the proof (Q-001). |
| SEALED-CONSEQUENCES | BLOCKING | PASS | OPEN | `economicCleanup` exhaustive over `BaseChange` (`Step.lean:230-243`): admit no-op, departure absorbs conto and winds up a departing admin, role-change winds up admin loss. `baseHook` (`254-259`) cleanup then `sweepClosures θ post`. General theorem `base_change_recomputes_votes` (`1519-1545`) axioms `[propext]`. Promoted sweep: `sweepClosures_idempotent` (`Vote/Invariants.lean:1182`) `[propext]`; `checkSweepIdempotent` non-vacuous on V-3 payload; `sweepDuplicating` + `checkSweepIdempotentMutant` is a shipped, decided negative control. |
| V3-BASE | BLOCKING | PASS | OPEN | `checkV3BaseReachable` (`Invariants.lean:1425-1444`) runs two signed production events (`Reactivegas.apply`): propose departure of eve (pending, members/votes unchanged), then approve. No `VoteEvent`, tallies byte-identical to `v3Question`, verdict `open → positive`, one closure record. `base_change_can_close_without_ballot` is `by decide` of that Bool (elaborated in the frozen gate). Not a fabricated closure fixture. |
| CLOSED-SUMS | BLOCKING | PASS | OPEN | Wildcard-free production matches: `applyIntegratedEvent`, `enactMutation`, `mutationChange`, `economicCleanup`, `proposalMutation`, `proposalDigest`, `step`/`stepEvent` over `AppEvent`/`Event`, `validateVoteEvent`, `validateBaseMutation`, `route`/`voteDerived`/`appVerdictAllows`. Negative controls: gate introduceMember selftest; independent voted-insertion mutant reddens `enactMutation_preserves_absence`. |
| HISTORY/SCOPE/TRUST | BLOCKING | PASS | OPEN | `baseEnacted_threshold_met` byte-identical to `c50f5275` (sha256 `ab9b4aadb52fbbcdb62bb8de39f62acbc76f0ffbfa4c8eeb5d1d79f6fff334f4`, 757 bytes). 20 Lean files only. Inherited `sorry` is `Reactivegas.Step.backdonateAuthorized` (`Step.lean:44`, #47/Q-007); production `appFold` prints no axioms. Independent axiom probe: no `sorryAx` on S62-B theorems. `nix/lean-dependency-direction.sh` OK, control imports=15. |
| RELIANCE | BLOCKING | PASS | OPEN | Ticket-owner NOTE-002: six ratified + promoted sweep; admin-count discarded and not reopened. See reliance section. |

## Frozen gate / Q-001

Gate files verified at run: `gate-s62-b.sh` sha256 `b2d89eecc8116a08580648720e8a1bfabc7c7cc668fb56a59e0f51883ba9d605`, `gate-common.sh` sha256 `32be7416c3807c5026d0c6ce243593e9106f31800efa5cc77b70974c95972177`.

Q-001 is **substantiated and nonblocking**. `row_B_DIRECT_ADMIT` (`gate-common.sh:121-126`) runs `[[ "$count" -eq 1 ]] || fail ...` **without** `|| return`. Independent overlay with a second `DirectCommand.alsoAdmit` constructor:

```
gate: FAIL DirectCommand constructor count=2 expected=1
GATE-ROW G62-B-DIRECT-ADMIT PASS
```

exit 0. Evidence `evidence/q001-direct-admit-row.log` sha256 `402608f0c6693fa78b8cd10c4d351f29e134f1cec20f72df775e125dfa748c8f`. A gate PASS cannot pass DIRECT-ADMIT or ONE-INSERTION-PATH; those rows are judged from the elaborated theorem, the transition graph, and the mutant below. Remaining DIRECT-ADMIT legs (theorem names + reachable checks) still run and can fail.

## Mutation

Instrument (frozen, not shipped): `instruments/mutant-voted-insertion/KelGroups/Integration.lean` sha256 `88ee1dd0c7d08971c9d230b1ed3225296ea74fba19b539fe20e6056f78319a47`.

Property class: **a voted base route grows membership**. `enactMutation.changeRoles` also `assocInsert "smuggled" …`. Mutant olean compiled (applied). Re-elaborating candidate `KelGroups/Invariants.lean` against that olean:

```
KelGroups/Invariants.lean:718:6: error: Type mismatch
  assocLookup_adjust_of_none ...
but is expected to have type
  lookupMember key (enactMutation gs (BaseMutation.changeRoles other roles)) = none
MUTANT-INVARIANTS-EXIT=1
```

That is `enactMutation_preserves_absence`, the lemma `membership_growth_is_direct_admission` uses for propose/approve. Candidate Integration sha256 `a23b27e8265024368e7b39399d4111548f9ce56ce18782550799d76199c706e2` (unmutated). Evidence `evidence/mutant-voted-insertion.log` sha256 `ed1c4d603a3cd10d0634439c33ae64b8dcf08a5023b87d33c8d65925fdb93a5c`. GREEN control: frozen gate built `KelGroups.Invariants` (job 16).

## Reliance

| Row | Ruling | Audit |
|---|---|---|
| INV-62-B-CANONICAL-VIEW-IS-TOTAL | ratified | `groupView` (`State.lean:86`) copies `members` only. `GroupView` (`Types.lean:134-136`) has no `GroupState` path. Hook result type is payload/`Except`. `base_change_runs_hook` binds those views. |
| INV-62-B-ASSOC-HELPERS-ARE-KEY-FAITHFUL | ratified (PARTIAL) | `assocLookup_insert_self` / `assocLookup_insert_of_none` / erase-of-none / adjust-of-none proved in `Types.lean`. Growth proof uses them. Not re-mutated. |
| INV-62-B-PENDING-STORE-IS-NOT-A-MEMBERSHIP-STORE | ratified | `PendingBase.mutation : BaseMutation` (`State.lean:26-32`). Integrated approve reads `pendingBase`, not `pendingProposals`. `applyIntegratedEvent` does not call `applyEventDetailed`. |
| INV-62-B-COMUNE-IS-RESERVED-BY-THE-ROOT | ratified | `productionWellFormed` / `apply` entry+exit (`Step.lean:297-327`). Admission refuses reserved before duplicate (`Validate.lean:146-147`). `checkComuneAdmissionRefused`. |
| INV-62-B-STEP-GUARDS-ARE-VIEW-SCOPED | ratified (PARTIAL) | `step` (`Step.lean:56-151`) reads `isResponsabile view` / `GroupView.isMember` / `memberKeys view` only. No `.users`/`.responsabili`. |
| INV-62-B-HISTORICAL-BLOCK-IS-BYTE-FROZEN | ratified | Independent `cmp` vs `c50f5275`; hash `ab9b4aadb52f…`. |
| INV-62-B-SWEEP-IS-IDEMPOTENT-ON-A-SWEPT-PAYLOAD | promoted into T6223 | Theorem + non-vacuous witness + shipped duplicating mutant, all elaborated. |
| INV-62-B-ADMINCOUNT-AGREES-WITH-VIEW | discarded | Not a reliance of this change; not reopened. |

## Blocking findings

None.

## Verification receipts

| Command | Exit | Duration | Evidence |
|---|---:|---:|---|
| cheap G62-B rows (readiness, free) | 0 | n/a | all six PASS; worktree porcelain empty |
| `timeout 15m gates/gate-s62-b.sh` (`cache=cold`, `just ci`) | 0 | 97215 ms | `evidence/gate-s62-b.log` sha256 `68485d9bcd735b12c716362165f65ee636fbb051e4f634585891514b6b97662d`; `SLICE-GATE S62-B PASS rows=6 full-ci=pass`; lake 27/27 |
| Q-001 overlay `gate-common.sh row G62-B-DIRECT-ADMIT` | 0 | n/a | `evidence/q001-direct-admit-row.log` sha256 `402608f0c6693fa78b8cd10c4d351f29e134f1cec20f72df775e125dfa748c8f` |
| `lake env lean instruments/axiom-probe.lean` (`cache=warm`) | 1 | 1635 ms | `evidence/axiom-probe-2.log` sha256 `7bc44d743bcd71ef03dd5c57d4a06adce52b3e3feef421746341f43ca72573db`. Eleven S62-B prints succeeded (`propext` or no axioms, no `sorryAx`). Exit 1 is probe identifier `Reactivegas.backdonateAuthorized` (the sorry is `Reactivegas.Step.backdonateAuthorized`). First probe cwd miss is not cited. |
| voted-insertion mutant + Invariants re-elab (`cache=warm`) | 1 | 2593 ms | `evidence/mutant-voted-insertion.log` sha256 `ed1c4d603a3cd10d0634439c33ae64b8dcf08a5023b87d33c8d65925fdb93a5c` |

`free_space` before gate 203949703168 bytes; after 203873857536. Porcelain empty throughout.

## Advisories

- Q-001 constructor-count leg remains inert. Future gate revision is outside this immutable slice; do not treat it as S62-C work.
- `Reactivegas/TraceTests.lean` is not a lake job (27 jobs, no TraceTests). S62-B Bools/`decide` theorems live in `Reactivegas.Invariants`, which the gate built. Not an S62-C requirement.
- `membership_growth_is_direct_admission`'s *exported* conclusion is admin ∧ not-reserved ∧ `change = some (.memberAdmitted key)`, not `event = .direct (.admitMember key …)`. The proof's exhaustiveness is load-bearing; the mutant kills that exhaustiveness. Statement-shape, not a current second route.
- The only successful admission fixture uses `roles=[]` and asserts the new member is not admin (`Invariants.lean:1243-1250`). The command *can* store admin roles (`admitMemberInto` / ignored `_roles` in the validator). Unwitnessed, not a defect.
- `Proposal.departure` is a scanner-facing name; `proposalMutation` maps it to `BaseMutation.removeMember`.

## Candidate invariants

None.

## Onward discoveries — outside this ticket

None. `RECORDED, NOT-OPENED`: n/a.

## Residuals

None terminated as `RESIDUAL` this round. Remaining BLOCKING campaign rows stay `OPEN` because this envelope authorized one independent killing mutant, not one per row.

## Recommendation

Accept candidate `b7c02fdc581f21acbd7a172c5bca918f81eb2739` for S62-B. Do not repair. Do not promote the advisories into S62-C.
