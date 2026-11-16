# Reactivegas milestone 2 — current delivery plan

Updated 2026-09-06 by the milestone desk. The prior handover plan is preserved
as PLAN-TO-MILESTONE-2-before-astra.md. Live ownership and exact candidates are
in ../LEDGER.md; append-only decisions and evidence are in ../STATUS.md.

## Outcome and completion boundary

A stranger obtains the published Haskell coordinator using kelgroups, runs it,
and drives election → collection → pledge → assenso → purchase → refund without
source. This coordinator test is necessary, but the milestone description also
requires a released browser client and CLI implementing the laws end-to-end for
one gruppo. One pure Haskell core serves the native coordinator, wasm32-wasi
browser build and native CLI; the CLI uses the coordinator HTTP API.
The operator explicitly ruled Haskell implementation into milestone 2.
That scope is settled; the older plan's question about moving it to milestone 3
is superseded. The operator has authorized the limited sibling kelgroups #29/#28/#30 team; the integrated rejecting API slice has landed through kelgroups PR32; the full substrate contract remains incomplete.

The active operator goal is now to finish M2 in full. Lean quality, simulation and planning are necessary parts; the released Haskell coordinator, browser and CLI stranger outcome is required for completion. The legacy server and Lean/simulator gates do not establish that outcome. The currently published legacy
server and Lean/simulator green gates are not evidence for that outcome.

## Work map and acceptance evidence

| Work | Issue / owner | Completion evidence still required |
|---|---|---|
| Quality | #66, quality owner | S1 trace resolution, S2 total clean-build compiled axiom gate and honest negative controls, S3 theorem-keyed mutant ledger/clarity record, S4 actual Prop/Bool correspondence, S5 statement completeness/retention assessment; fresh audit and landed commits for each changed slice |
| Proposer assent | #68, standalone owner | zero at opening; proposer cannot supply assent above one admin; sole admin can explicitly approve separately; unchanged majority formula; reachable historical/integrated witnesses and fresh audit before landing |
| Pledge sovereignty | #69, not yet commissioned | intake prepared in ASK-t69-pledge-sovereignty.md; member may create their own pledge, correct/retract while pending (zero refunds and removes the pending entry); after acceptance the ruled referente boundary; accepted model and proofs, current trace evidence, fresh audit and landing; no revival of #48's retired departure design |
| Simulator | #70, simulator owner | C1 faithful rebind and fresh final-candidate audit covering unaccepted prefix; C2 reachable random scenario generator and nonvacuous coverage; C3 reflect #68/#69 only after landing; C4 pending/accepted distinction visible before user action; audited final implementation landed |
| Design record | #71, standalone owner | rewrite to actual merged model; remove false closure claim; executable cold fail-closed citation checking over discovered cited extent; laws/witnesses/authority distinguished; current versus ruled-undelivered behavior clear; final pins refreshed after relevant model landings |
| Haskell implementation | #67, Haskell owner | D1 assessment delivered; D2 substrate integration, D3 economic replay/refusals, D4 usable six-step coordinator surface, D5 published artifact and independent stranger run remain |
| Upstream dependency | #73, kelgroups epic #29 owner %532 | kelgroups #29/#28/#30 deliver the rejecting fold, shared identity/membership/roles, GroupView, sealed atomic hook, pendingBase/direct paths and vote lifecycle actually required by the accepted model; no local vendoring substitute |
| Historical exporter | #74, child of #67 | CLOSED as superseded by accepted #86/PR87; old candidates never accepted, PR78 closed unmerged, historical evidence retained |
| Exporter successor | #86, child of #67 | ACCEPTED and LANDED via PR87 at d67032313acf3699cc50358a057391b88d002192, tree identical to fully audited38c6d06; issue closed completed. Actual CI invocation, clean-shell tooling, whole wrapper value binding and arity repairs verified. Frozen corpus remains provisional on semantic and replay-context dependencies; not final vote/economic conformance |
| Integrated vote corpus | #75, desk-owned, not dispatched | signed votes through production Reactivegas.apply; reuse simulator vote journeys; discovered constructor/refusal extent; explicit replay context as test input; later composition/lifecycle rows; final regenerated corpus after semantic dependencies |
| Runtime composition | #76, standalone owner597, commissioning | grant/deny/backdonate are actually vote-derived with target/payload/polarity/provenance/one-use consumption bound; reachable refusal witnesses and can-fail controls; no unilateral fallback |
| V-5 lifecycle | #81, desk-owned, not dispatched | proposer renounce/departure close negative with correct causes and retained records; atomic effect; negative continuation and escrow refund through #76; corpus rows through #75 |
| Portable core | #82, desk-owned, not dispatched | native and wasm32-wasi builds use the same semantic module set; a deliberate divergence fails the gate; incompatible dependencies are findings against core design, not grounds for a fork |
| CLI | #83, desk-owned, not dispatched | native client linked to the same core, driving the defined coordinator HTTP API; released artifact and independent end-to-end journey |
| Browser | #84, desk-owned, not dispatched | Reactivegas browser UI in the existing kelgroups-client PureScript shell, using the shared wasm core; measured integration gaps, released bundle and independent end-to-end journey |
| Release identity | #51, existing release work | T012 remains: re-audit inherited mandate, retain live stranger fetch, publish hardened follow-up under explicit merge/publication authority; distinct from D5's new coordinator release |

## Quality findings incorporated into the plan

Fresh current compiled axiom checks support current trust only; historical
mutation contamination was not adjudicated. Prior 6/224 mutant-coverage and
19-Prop/no-mirror claims were withdrawn after independent review. Do not reuse
those denominators. Discover actual declarations and consumers, distinguish
public/private/compiled extents, and bind each claimed mutant to the production
definition and a nonzero tested denominator.

Goals.lean and decisions.md filenames are optional; their substantive statement
and dated authority content is required. Do not manufacture decidability for
arbitrary reachability or create a blanket mirror-count quota. A theorem saying
present questions have open verdicts does not prove undecided question retention.
#66 S5 assesses that obligation; #81 handles the ruled unfinished lifecycle.

## Dependencies and landing order

Current accepted state: S4 LANDED via PR89 at efef604de87b2a1efae51e84d1a9150e585c1db0, accepted tree caaa0488f39a6afb2553680a11fd6bfd86d1c90b. C1 full gate-v17 passes, but final CI exposes a registered-root import-reach checker defect; #92 owns that repair and must precede C1 final validation. S3 instruments are under blind inspection. Core #90 baseline CI passes and its implementation mandate is frozen, proceeding on accepted substrate primitives. C1 still precedes #68, then #69. Detailed earlier campaign paragraphs below are historical; LEDGER.md carries current state.

1. S1, S2R, exporter successor and S4 are landed. #66 remains open for S3, S5 and the #92 checker integration repair. Earlier rejected campaigns, spend and findings remain in STATUS and the retained prior plan; no historical receipt confers acceptance on later bytes.
2. C1 has a full gate-v17 GREEN on 48f76d9, but final CI fails at the registered-root import-reach boundary. Repair #92 precedes C1 final validation and its fresh full audit of the complete unaccepted prefix from 6879970fdb1a797263843387e14704eaa1e3a2e7. All seventeen audit rows remain falsifiable. C1 lands before #68/#69 changes.
3. #68 has an audited READY candidate at PR80, with no merge grant until C1. Commission #69 on the accepted #68 base. The
   simulator follows each landed semantic change; it must not anticipate one.
4. #66 S2–S5 and #71 can prepare independently within their file fences. #71
   alone writes docs/en/design. Final records and receipts must describe the
   then-current accepted model; partial PRs do not close broader issues.
5. #76 is now commissioned for isolated Lean implementation of vote-derived grant/deny/backdonate with target, polarity, provenance and one-use consumption. Landing remains after #92/C1/#68/#69, with final-base revalidation. #81 cannot close without its negative continuation and refund; #75 gains the composition/lifecycle observables after acceptance.
6. #74 is historical and superseded by landed #86. The exported corpus remains provisional. Final semantic conformance freezes
   only after relevant #68/#69/#76/#81 changes and an accepted #75 replay-context
   contract. A finite threshold table is test input, not a shipped default.
7. D2 money-custody implementation (deposit, withdraw, transferCassa, donate) is now RELEASED against already accepted primitives, in one shared core. The blanket whole-#73 dependency was sequencing, not required for this slice. Remaining integrated #67 D2/D3 depend on the accepted interfaces they actually consume; no vote candidate consumption or #73 acceptance is implied. D3 must replay accepted corpus/context from the
   actual Haskell implementation, including refusals and atomic composition.
   D4 exposes the full six-step journey; D5 packages and independently exercises
   the published coordinator from a clean directory without source.
8. #82 constrains D2's dependency choices from the start. #83 depends on D4's
   concrete HTTP API. #84 uses the existing kelgroups monorepo client package;
   its Reactivegas UI and wasm integration remain work, not established capability.
   Browser/CLI release acceptance is required alongside D5. Multiple instances
   provide multi-gruppo MVP; no multi-tenant surface or live-2018 migration.

## Open decisions and explicit limits

- Operator ratified the limited sibling kelgroups team for #29/#28/#30. Kelgroups#28 is ACCEPTED, LANDED and CLOSED through PR32 at933e385df2f2a251bb54a08bb7663f0d41fafb64; the landed tree equals auditedab25cd1, candidate and postmerge CI are green, five canonical guards passed. The original refusal-channel blocker is resolved. Owner remeasurement finds19/26 historical interface names present and7 vote names absent; no percentage-completion claim follows. #30 draft is being corrected against original client/proof obligations, current-versus-unlanded lifecycle semantics and a genuinely costed plan. Broader#29 runnable demo is now kelgroups#33 and major release/downstream notes are#34, both filed and undelivered. Full #30 implementation is now authorized byNOTE007 through the Opus epic/ticket owners, owner28/22 and auditor25/24 cumulative, max2submissions, two counted existing-module compiler checks first, fullscope preserved; two existing-module compiler operations completed and desk verified raw hashes; actual export grammar requires a gate repair before Vote implementation. No merge/release grant. The four-arm #90 slice uses already accepted primitives; remaining D2/D3 require the accepted contracts they actually consume. Necessary existing kelgroups-client support belongs in that authorized team; broader Reactivegas UI/wasm remains#84/#82. No third repository.
- Shipped vote threshold default is not selected by #68. It must be ruled or
  exposed as an explicit supported configuration before D4/D5 need it.
- Voci catalogue is explicitly excluded from M2 and assigned to M3 by the operator on 2026-09-06. The follow-up is https://github.com/paolino/reactivegas/issues/91, verified OPEN in milestone number 3. Preserve the legacy scope and dated non-goal in the design record; no M3 implementation is commissioned here.
- Non-proposer renounce refusal and non-designee ballot refusal are unruled.
  Dormant VoteError constructors are not authority. Preserve the current
  boundary until a ruling changes it; they do not block the ruled V-5 work.
- #43 stays unrespawned. Closed #47/#48/#59 are superseded by #69/#71 as stated
  in their issue dispositions; old local branch artifacts remain preserved.
  The three unmerged #47 drafts are explicitly retained as superseded historical
  evidence in artifacts/retained-issue-47 (verified Git bundle and aggregate
  patch). They are not for merging; worktree/branch remain intact.

## Acceptance and release controls

Every changed candidate has an exact SHA, journaled identity, immutable gate,
recorded budget/submissions, full required local CI and fresh independent audit.
The milestone desk alone grants exact-SHA merge authorization; owners execute
it with current remote CI and merge guards. Publishing is separate from merging.
A draft PR, a complete plan, an old acceptance mapped across a rebase, or a green
aggregate checker alone does not prove the underlying behavior.

The final milestone acceptance packet must bind the released artifact identity
to the accepted source, then record a fresh download/run and all six actions,
including their meaningful state effects and required refusals. It must also
bind and exercise the released browser and CLI against that coordinator, proving
the shared core requirement. No source build, Lean execution or simulator
interaction substitutes for those user-facing runs.

## Current continuation

See ../LEDGER.md for current lane identities, exact candidates, budgets and next actions. This plan owns requirements and dependency order; the ledger owns live state.
