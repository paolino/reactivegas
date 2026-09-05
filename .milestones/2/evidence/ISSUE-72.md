## Milestone outcome

Operator ruling, 2026-09-05: milestone 2 is the Haskell implementation of the Lean model using kelgroups as backend.

A stranger obtains the published coordinator artifact, runs it, and drives a gruppo through **election → collection → pledge → assenso → purchase → refund** without touching source.

The milestone description additionally requires a **released browser client and CLI** implementing the laws end-to-end for one gruppo. One pure Haskell core supplies the native coordinator, wasm32-wasi browser build and native CLI. The six-step coordinator test is necessary and not sufficient for this completion boundary.

This outcome is not achieved. The existing legacy server artifact and successful Lean/simulator gates do not establish it. D1 assessed the application and substrate; D2/D3 are blocked on #73. The earlier statement that the specification was substantially complete omitted ruled runtime composition and vote-lifecycle work, now explicit below.

## Work map

| Issue | Required result |
|---|---|
| #67 | Haskell coordinator on kelgroups: D1 assessment, D2 substrate integration, D3 economic conformance, D4 six-step surface, D5 published artifact and stranger acceptance |
| #73 | Upstream kelgroups contract needed by D2/D3; cross-repository desk ownership awaits operator ratification |
| #66 | Lean quality: trace resolution, total clean-build axiom gate, actual mutant ledger, Prop/Bool correspondence, statement completeness and retention assessment |
| #68 | Proposer does not supply an assent; separate explicit sole-admin approval; majority arithmetic unchanged |
| #69 | Member sovereignty over creating and correcting/retracting a pending pledge; ruled accepted-pledge boundary |
| #70 | Simulator rebind, reachable random scenario generator, faithful reflection of landed #68/#69, pending/accepted distinction visible before action |
| #71 | Current design record, false closure claim removed, discovered-extent cold fail-closed citation checker, explicit laws/witnesses and ruled-undelivered behavior |
| #74 | Provisional frozen exporter with live-value binding, closed emitted keys and byte/hash controls; final re-emission after #66 S1 |
| #75 | Signed integrated vote corpus through the production root, explicit policy replay context, discovered coverage and later composition/lifecycle observables |
| #76 | Actual vote-to-economic composition: target/payload, polarity, provenance and single consumption for grant/deny/backdonate |
| #81 | V-5 proposer renounce/departure negative closures and retained causes, plus negative continuation/refund through #76 |
| #82 | Same pure core built natively and wasm32-wasi, with a can-fail semantic module-set equality gate |
| #83 | Native CLI using the coordinator HTTP API and same core, released and independently exercised |
| #84 | Reactivegas browser client in the existing kelgroups-client PureScript shell, using the wasm core, released and independently exercised |
| #51 | T012 remains: re-audit inherited mandate, retain stranger fetch, publish hardened follow-up under explicit authority; new-coordinator release is distinct |

#75, #76, #81, #82, #83 and #84 are standalone desk-owned planning entries. Their creation does not dispatch implementation. #74 is a child of #67.

## Dependencies

- #66 S1 landed via PR #79 at `4a6cd87fcbc3e4a536bbc9f240f5efe5704022af`, with tree equality to audited `fa01779`; #66 remains open for S2–S5. #74 is released for combined checker repair, regeneration and fresh submission-2 audit. Draft PR #78 is not accepted.
- #70 C1 does not consume S1's trace metadata and can land independently. Its complete resulting candidate needs acceptance covering the previously unaccepted prefix. Prioritize C1 before semantic #68/#69 landings.
- #69 follows accepted #68. #70 reflects semantic changes only after they land.
- #71 can prepare independently, but final citations and claims must describe the accepted model after relevant landings. It alone owns `docs/en/design/` while open.
- #81's closure/cause work can be specified independently; its refund acceptance depends on #76. Final #75 corpus includes composition/lifecycle observables after they exist.
- #74 delivers provisional export machinery. Final conformance bytes/context follow relevant #68/#69/#76/#81 semantics and the accepted #75 replay contract.
- #67: D1 → #73/D2 → D3 against the accepted corpus → D4 → D5. #82 constrains D2 dependencies before implementation; #83 depends on D4 defining the HTTP API; #84 requires the shared wasm build and actual client integration. Browser/CLI acceptance accompanies D5.
- Multi-gruppo MVP uses multiple instances. A multi-tenant surface and live-2018 migration are outside this milestone.

## Conformance and evidence boundaries

The frozen trace envelope is not a complete replay contract by itself. The two #74 corpora lack signed `.app` vote events; the simulator already has signed vote journeys to reuse, so the older claim that no corpus exercises votes was too broad. #75 lifts those journeys through `Reactivegas.apply` and binds test policy inputs explicitly. Naming a Lean threshold symbol is insufficient for a Haskell replayer.

Runtime route classification is already present; it does not prove the grant/deny/backdonate effects actually consume valid vote closures. #76 supplies that connection. V-5 already rules negative closure and refund on proposer renounce/departure; #81 supplies the unfinished behavior. Dormant refusal names do not authorize new refusal rules.

Current compiled axiom checks do not establish historical cache provenance. Previously quoted mutant-coverage and Prop/mirror denominators were withdrawn after independent review. #66 must quantify over the discovered actual extent, with reachable witnesses and can-fail controls. Present questions having open verdicts is not a retention theorem.

## Open decisions

- Ratify the limited sibling kelgroups lane for #73; no upstream implementation is commissioned under this desk yet. `kelgroups-client` already exists within `paolino/kelgroups` at main `368b596fef0b6d393c2ac7afc631d236c55d86d1`; absence of a standalone repository did not imply a missing component or third repository. Required Reactivegas UI/wasm changes remain to be scoped, with no broader implementation grant inferred.
- Shipped vote threshold policy remains unselected; a parameterized test policy does not select the product default.
- Voci inclusion remains an explicit product question; the inherited six-step non-goal and legacy functionality remain documented.
- Refusing non-proposer renounce or non-designee ballots is unruled; #81 does not infer either from dormant errors.

## Closure

#47, #48 and #59 are closed with their supersession recorded; surviving work is in #69/#71. #43 is not respawned. The three unmerged #47 drafts are retained as superseded historical evidence in a verified Git bundle and aggregate patch; the stale draft will not be merged. Its worktree and branch remain intact.

Each changed candidate requires its scoped independent audit, required gates, exact identity and current CI before authorized landing. Partial PRs do not close broader issues. Final milestone acceptance requires a source-bound published **Haskell coordinator**, a fresh download/run without source, and observed effects across all six actions. The released browser and CLI must also implement and exercise the laws end-to-end against that coordinator, using the shared core. Lean or simulator success cannot substitute for those runs.
