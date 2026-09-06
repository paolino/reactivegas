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
| Runtime composition | #76, desk-owned, not dispatched | grant/deny/backdonate are actually vote-derived with target/payload/polarity/provenance/one-use consumption bound; reachable refusal witnesses and can-fail controls; no unilateral fallback |
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

Current accepted state: S4 LANDED via PR89 at efef604de87b2a1efae51e84d1a9150e585c1db0, accepted tree caaa0488f39a6afb2553680a11fd6bfd86d1c90b. Actual-base C1 validation and S3 recovery are released. C1 still precedes #68, then #69. The detailed campaign paragraph below is retained historical evidence; current status is authoritative in LEDGER.md.

Earlier reservation (2026-09-06): S2R LANDED via PR88 at3590c001. S4 may land next after complete final-candidate acceptance, remote CI and an exact desk merge grant; C1 follows and still precedes #68/#69. This desk scheduling change is recorded in LANDING-ORDER-S4-BEFORE-PARKED-C1-20260906.md: C1 was parked on its layout choice/control gap when this reservation was made. The operator has now chosen expanding rings and the bounded repair is released; there is no active C1 audit. C1 owner final validation completed on that accepted base and fresh full independent audit returned partial FINDINGS with three blocking defects and thirteen unjudged rows; bounded repair is authorized, with expanding geometry now ruled and the bounded repair released; #71 final audit waits the final accepted model/quality base while static repairs continue. See NEXT-LANDING-SEQUENCE-20260905.md for exact wake conditions. This avoids invalidating newly commissioned audits through known overlapping source and CI changes.

1. #66 S1 LANDED via PR79 at4a6cd87; exporter successor #86 LANDED via PR87 atd670323, accepted tree d033effe. #66 remains open. Old #74 and PR78 are superseded, evidence preserved. S2 old three-submission campaign rejected/exhausted; S2R correctedab617d8 completed fresh full-candidate review plus the explicitly bounded retained-evidence supplement:32rows closed, reportc71dda1e, finalmanifest1f88b132. PR88 LANDED3590c001 on2026-09-05T21:12:18Z, singleparentd670323, exact auditedtree44a1f0bc and signatureverified. The actual merge_guard tool was invoked with allsix guards passed. Retained evidence is byte-bound, not relabelled a fresh mutation replay. S4-B submission2 candidate94bb7bb completed owner validation18 substantive/52 targeted and its fresh full audit. The auditor returned AUDIT-FINDINGS at2026-09-06T06:17:01Z: executed C1-C26 controls closed in their amended scopes, final cold CI exit0, with F-001 Reach consumer-axis authority OPEN. This is a classification/authority blocker, not a false-proof or mandatory-oracle finding. Parent disposition returned; RG-S4-REACH-20260906 settles the consumer boundary with finite-history correspondence retained under S5. The comment-only exceptional third submission is committed04eb6c7d with retained final-CI output; owner19/19 substantive is spent. The fresh Codex astra/high FULL STATIC audit terminated07:06:01 atAUDIT-FINDINGS43db9049 (FS-01 clean-finalCI binding,FS-02 closuremap). Deskverified74/74manifest93fa7d84. Later exactcleancommittedCI completed07:07:14 exit0, logfbc50e0d verified; owner20substantive spent. NOTE072 authorizes one fresh zero-execution static final-evidence supplement afterparentmaprepair and explicitly allows documentation/range/hash effects while requiring non-commentlogicpreservation and finalsource/compiledconsistency. No oldauditorrestart or forcedPASS; no acceptance or merge. Audit spend12/12 substantive and73/80 targeted, historical6/59 retained; three historical limitations remain OPEN. Report874727c2 and665-entry manifesteb055309 independently hash-verified by the desk. S3 author static submissions3/3 spent; the fresh full static auditor returned AUDIT-FINDINGS06:19:25Z, three original findings closed and five partly unresolved/blocking. The proposed broad measurement campaign is unexecutable as printed. A separately bounded SS0 experiment attempted OP1 and failed before Lean because Nix resolved the wrong cwd; historical spend is now6substantive/3targeted. That one retry executed the full sequence0/1/0/0 in42s, with the mutated Step compiling and a type mismatch inside step_grant_inv, followed by restoredGREEN and the isolatedcheckGREEN. Cumulative9/4spent; no more retries, S3closure or Phase2grant. Parentassessmentread, exactsemantic-versus-script limitscorrected; NOTE071 nowcommissions onefreshstaticSS1–SS6artifactauthor(one submission,zeroexecution), thenonefreshfullstaticreview. Allremainingfullobligationsstand. S5 statement completeness remains required. Every final candidate integrates the actual accepted base and re-establishes affected evidence before independent audit.
2. #70 C1 final owner validation on accepted LANDED S2R completed at9717405; the fresh FULL audit returned partial FINDINGS: actor substitution during simulator replay, reachable layout overlap and handler discovery blindness; identity/discovery repairs have focused owner evidence and the expanding-ring geometry repair is now released under the operator ruling; no acceptance. The audit covers the entire unaccepted candidate from original base 6879970fdb1a797263843387e14704eaa1e3a2e7, including the inherited implementation and accepted-base integration; prior killed rows remain falsifiable. C1 lands before #68/#69 semantic changes. The trace drivers do not import Reactivegas.Trace, but shared Invariants and mandatory CI changes make S2R landing relevant to final evidence.
3. #68 has an audited READY candidate at PR80, with no merge grant until C1. Commission #69 on the accepted #68 base. The
   simulator follows each landed semantic change; it must not anticipate one.
4. #66 S2–S5 and #71 can prepare independently within their file fences. #71
   alone writes docs/en/design. Final records and receipts must describe the
   then-current accepted model; partial PRs do not close broader issues.
5. #76 and #81 complete already-ruled semantics. Closure/cause work can be
   specified independently, but #81 cannot close without #76's continuation
   and refund. #75's final corpus includes these observables when landed.
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

- Operator ratified the limited sibling kelgroups team for #29/#28/#30. Kelgroups#28 is ACCEPTED, LANDED and CLOSED through PR32 at933e385df2f2a251bb54a08bb7663f0d41fafb64; the landed tree equals auditedab25cd1, candidate and postmerge CI are green, five canonical guards passed. The original refusal-channel blocker is resolved. Owner remeasurement finds19/26 historical interface names present and7 vote names absent; no percentage-completion claim follows. #30 draft is being corrected against original client/proof obligations, current-versus-unlanded lifecycle semantics and a genuinely costed plan. Broader#29 runnable demo is now kelgroups#33 and major release/downstream notes are#34, both filed and undelivered. Full #30 implementation is now authorized byNOTE007 through the Opus epic/ticket owners, owner28/22 and auditor25/24 cumulative, max2submissions, two counted existing-module compiler checks first, fullscope preserved; two existing-module compiler operations completed and desk verified raw hashes; actual export grammar requires a gate repair before Vote implementation. No merge/release grant. D2/D3 still require the whole accepted#73 contract. Necessary existing kelgroups-client support belongs in that authorized team; broader Reactivegas UI/wasm remains#84/#82. No third repository.
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

## Current lanes and next actions

| Lane | Actual state and next action |
|---|---|
| Quality S4, owner503 | LANDED PR89 at efef604d on 2026-09-06T08:37:18Z. Desk verified exact accepted tree, both parents, master identity and #66 OPEN. S3/S5 and recorded limitations remain; this is bounded S4 completion. |
| Quality S3, owner503 | Instrument recovery commissioned under NOTE-078 and acknowledged at 08:29:05Z. Fresh owner590 started at 08:31:38Z; source preparation is active with zero build/run executions spent. One shared ten-execution budget covers author pilot and inspectors, with at most two submissions and one repair batch. Two blind Codex inspectors follow preparation and a real compiler pilot. Owner503 routed verified S4 landing to child590 at 08:39:08Z; E1 real compiler run at that base exited 0 and repaired patch application passed 8/8 while originals failed 8/8. Final identities and pilot must bind this actual landed base; the original semantic-ownership obligations remain open. |
| Quality S5 | Required finite-history correspondence remains OPEN under RG-S4-REACH-20260906 with #75/#71 dependencies, plus statement completeness/retention and exact-premises assessment. No arbitrary-state Reach decision requirement, no bridge implementation in S4 comment repair. No S5 completion. |
| Kelgroups epic532 | #30 candidate 2a900a8 remains unaccepted. Three blind inspectors reached real verdicts, including a blocking semantics finding. Shared counter 5/8, three remaining with two reserved for delta inspection. Ticket owner572 adjudicates once; epic does not repeat the audit. No merge or product release. |
| Simulator313 | Expanding layout implemented; latest repair 996fe8f8 unaccepted. Full gate failed on geoRan scope; repair is statically checked. Actual S4 base released through NOTE-S4-LANDED-C1-FINAL-VALIDATION. Freeze v17 with unexecuted end-to-end status explicit; existing owner five executions cover first final gate plus CI, no duplicate parent run. All 17 audit rows, full final audit and C2/C3/C4 remain. No new audit or merge grant; historical overruns retained. |
| #68 owner512 | Opus root t68-proposer-assent-opus-20260906, PR80READY d68a783f06b464dc869b27f6494214be1102c347; waits acceptedC1 and exactdeskgrant. Muse implementer519 preserved. #69 intake prepared, not commissioned. |
| #71 owner516 | PR77 remains draft at 77f8be6, waiting on the final accepted model/quality base. Desk resolved the new delta-default conflict: this existing S71-B campaign retains its explicit full-candidate final audit as a recorded exception. No launch, budget reset or new execution grant. Sole docs/en/design writer. |
| Haskell504 | D2 MONEY CUSTODY RELEASED through NOTE-D2-MONEY-CUSTODY-IMPLEMENTATION-RELEASE: deposit, withdraw, transferCassa, donate in ONE pure production core with accepted GroupView-backed predicates and minimal mandatory build/CI wiring. Whole-#73 wait amended only for this slice; voting/integrated coordinator still depend on acceptance. Owner creates/reuses child ticket and dispatches without another planning checkpoint. Shared10 execution campaign, two initial blind inspectors, at most2 submissions/one repair. No merge/deployment. |


Full milestone outcome and dependencies above remain required. The Haskell money-custody dependency re-cut is now an implementation grant. All other acceptance and integration boundaries remain.
