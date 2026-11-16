# T30-IDENTITY-MAP-r5 — per-identity Lean→Haskell classification (kelgroups #30)

Preparation owner `t30-contract`, 2026-09-06. Preparation-only (spend 0).
STATUS: every row below comes from a file READ IN FULL by this seat
(NOTE-006-1: no whole-file verdicts on unopened files — the r4 falsehood
is withdrawn in `T30-CONTRACT-r5.md` §0/F1).

Provenance (exact read granularity): `Vote/Types.lean`, `Vote/State.lean`,
`Vote/Event.lean`, `Vote/Validate.lean`, `Vote/Fold.lean` — full text read
at intake + declaration lists re-verified by name-grep (review aid) at r5;
`Vote/Invariants.lean` (1228 lines) + `Vote/Tests.lean` (397 lines) — full
text read at r5; `KelGroups/Event.lean` (52 lines) + `KelGroups/Types.lean`
(166 lines) — full text read at r5; `KelGroups/State.lean`,
`KelGroups/Validate.lean`, `KelGroups/Integration.lean` — 250-line heads
read (CONTEXT granularity: S28-landed surface, internals owned by the S28
contract; only #30-touched identities named). kelgroups Haskell surface
(`Event/State/Validate/Fold/Store/Types.hs`, `justfile`, `kelgroups.cabal`,
`test/Main.hs`, `client/Api.purs`) read at intake; Vote-absence re-verified
at r5 (zero references to placeBallot/sweepClosures/verdictOf/
ClosureRecord/QuestionKind/VoteState anywhere in lib/test/app).

Rule's EXACT subject (NOTE-006-1): the drift-mapping emission rule targets
(i) inductive/structure TYPE + CONSTRUCTOR identities of runtime vocabulary
and state shapes, and (ii) EQUATION GROUPS (per-function arm sets) of the
production fold/validation functions. It does NOT target Props, theorems
(private or contractual), proof-side computable helpers, fixture /
trace-builder / witness defs, examples, guards, or mechanical instances.
`def`s in Tests.lean are fixture/witness defs → excluded-by-kind with
reason per row below (never grounds to claim they don't exist).

Kind tags: MIRROR = Haskell runtime mirror obligation (new #30 work);
LANDED = already on kelgroups main via S28 (consumed, not re-mirrored);
EXCLUDED = no Haskell obligation, with per-identity reason (proof-side
helpers get NO runtime requirement — explicit throughout);
CORROB = review-level consistency pointer (ticket owner + auditor eyeball;
never a gate kill).

## Vote/Types.lean (namespace KelGroups.Vote)

| Lean identity | kind | Haskell disposition |
|---|---|---|
| abbrev QuestionId (:22) | alias | MIRROR: Haskell question-id type |
| inductive Verdict + positive/negative/open (:29–32) | ctor identities | MIRROR ctors (R30-4; open distinct, never Option) |
| abbrev Threshold (:40) | alias | MIRROR: threshold-parameter type (parameter everywhere) |
| def legacyThreshold (:44) | exhibit | EXCLUDED from mirror (exhibit, never default — R30-X fence); CORROB REQ-VERDICT-* use a parameter |
| def zeroThreshold (:48) | exhibit | EXCLUDED same reason; CORROB Tests.lean R-48 warning transfers as Haskell review rule (never freeze either exhibit as product answer) |
| inductive Ballot + assent/dissent (:53–55) | ctor identities | MIRROR (R30-2) |
| inductive QuestionKind + collective/permission(designee) (:63–65) | ctor identities | MIRROR (R30-1; undesigneeable permission unrepresentable) |
| inductive ClosureCause + tally/franchiseChange/proposerDeparted/renounced (:73–77) | ctor identities | MIRROR all 4 as DATA (D1: carried causes as data, never producers) |

## Vote/State.lean

| Lean identity | kind | Haskell disposition |
|---|---|---|
| structure Question {kind,proposer,assents,dissents} (:31) | state shape | MIRROR record (no time-like field — R-54) |
| structure ClosureRecord (:41) | state shape | MIRROR (R30-3/7; verdict≠open enforced where constructed) |
| structure VoteState {openQuestions,closed} (:51) | state shape | MIRROR (append-only `closed` — R30-3) |
| def emptyVoteState (:57) | fold base | MIRROR (fold base value) |
| def franchise / franchiseSize / isResponsabile (:68–75) | reads over GroupView | MIRROR over S28 GroupView (R30-6; never a local copy — M13) |
| def verdictOf (:85; arms collective/permission) | equation group | MIRROR, SINGLE verdict site (R30-4; legacy order; permission never tallies) |
| def lookupQuestion (:98) | helper | MIRROR helper |
| def closureCause (:109; arms positive/negative/open) | equation group | MIRROR (R30-3; stale-tally rule; M7merged site) |

## Vote/Event.lean

| Lean identity | kind | Haskell disposition |
|---|---|---|
| inductive VoteEvent + openQuestion/cast/renounce (:23–27) | ctor identities | MIRROR exactly 3 ctors (T6222 — no membership event; R30-8) |

## Vote/Validate.lean

| Lean identity | kind | Haskell disposition |
|---|---|---|
| inductive VoteError + 4 ctors (:38–42) | ctor identities | MIRROR all 4 as DATA; producing arms only for first two (R30-5; D7 tripwire on the other two) |
| instance BEq (Except VoteError Unit) (:45) | mechanical instance | EXCLUDED (mechanical derivation; Haskell Eq/deriving likewise mechanical) |
| def validateVoteEvent (:54; 3 arms, no wildcard) | equation group | MIRROR exhaustive, no wildcard (R30-1/2/5; new ctor must stop compilation — R57-02) |

## Vote/Fold.lean

| Lean identity | kind | Haskell disposition |
|---|---|---|
| def placeBallot (:51; 2 arms) | equation group | MIRROR (R30-2; M4a/M4b sites; guarded insert verified in shared `setInsert`, Types.lean) |
| def sweepStep (:62; 2 arms) | equation group | MIRROR (R30-3) |
| def sweepClosures (:74; filter + filterMap-append) | equation group | MIRROR (R30-3; M9 append-site; M15 filter-site — distinct sites, distinct obligations) |
| def effectedState (:87; 3 arms + cast sub-arms; renounce→gs) | equation group | MIRROR incl. renounce Slice-A no-op (D1 verified fact) |
| def applyVoteEventChecked (:107) | checked step | MIRROR as THE production step (integrated path uses it, never revalidates) |
| def applyVoteEvent (:118; erases refusal) | historical | EXCLUDED as reporting shape (conflates refusal with admitted no-op; Haskell refusal = unchanged aggregate via Either path, S28-established) |
| def foldVote / foldFrom (:126/:134) | production fold | MIRROR production fold (+ induction surface) |

## Vote/Invariants.lean (1228 lines — READ IN FULL at r5)

| Lean identity | kind | Haskell disposition |
|---|---|---|
| def QuestionClean :Prop (:32) | proof-side predicate | EXCLUDED by kind (specification language; no runtime counterpart; NO requirement) |
| def tallyKeysOfQuestion / tallyKeysOfState (:37/:39–41; computable List helpers) | proof-side auxiliaries | EXCLUDED (observe tallies for statements; runtime fold never computes them; NO requirement — explicit) |
| structure SweepReady (:46) / VoteWellFormed (:59; extends SweepReady) (:Prop carriers) | proof-side carriers | EXCLUDED by kind (Haskell suite properties are their OWN instrument, not mirrors) |
| ≈40 private theorems/lemmas (Assoc/Key/sweep/step sections) | proofs | EXCLUDED (proofs) |
| PUBLIC theorems — emptyVoteState_sweepReady, emptyVoteState_wellFormed | proofs | EXCLUDED (proof = evidence R30-13); CORROB fold-base properties in B4 suite |
| verdictOf_threshold_congr (R-46 policy-free) | proof | EXCLUDED; CORROB threshold-parameterization + no-default fence |
| applyVoteEvent_preserves_wellFormed, foldVote_wellFormed (R-68 carrier) | proofs | EXCLUDED; CORROB reachable-state discipline |
| ballots_nodup_disjoint (VC-1 one-position) | proof | EXCLUDED; CORROB M4a/M4b |
| open_questions_are_open (VC-4) | proof | EXCLUDED; CORROB sweep-closure completeness |
| questions_partition (R-61) | proof | EXCLUDED; CORROB M9/REQ-RETAIN (+ M15 non-duplication flip side) |
| no_expiry + preservesQuestionDecide (:Bool executable observation) + PreservesQuestionSemantics (:Prop) + instance (R-54/R57-07) | proof + observation defs | EXCLUDED (premise observes via production step; Haskell analog = REQ-NOEXPIRY executed property — analogous concern, INDEPENDENT instrument, already); franchise-conjunct removal story CORROB R62-11 recomputation placement |
| inadmissible_is_noop (R57-03), nonresponsabile_event_noop (R57-04), unfranchised_cast_noop | proofs | EXCLUDED; CORROB REQ-OPEN-REFUSE/REQ-CAST-NONRESP + M1/M2 |
| franchise_of_tallies (INV-54-FRANCHISE, cast-time standing, V-3) | proof | EXCLUDED; CORROB REQ-FRANCHISE-CURRENT + M7/M13 |
| sweepStep_of_open, sweep_filterMap_of_swept, filter_open_idem (private) | proofs | EXCLUDED |
| sweepClosures_idempotent (+ T6223 sealed-recomputation section) | proof | EXCLUDED; CORROB new REQ-SWEEP-IDEM + M15 (below) |
| def sweepDuplicating (NAMED Lean-side mutant, executable) | model negative control | Model evidence that idempotence is real (not vacuous); TRANSCRIBED as Haskell M15 (the file's own mutant shape — not invented, NOTE-006-1 compliant: a test-suite property, never a runtime requirement) |
| sweepDuplicating_duplicates | proof | EXCLUDED; CORROB M15's kill logic |
| 9 `#print axioms` lines (foldVote_wellFormed, ballots_nodup_disjoint, open_questions_are_open, questions_partition, no_expiry, franchise_of_tallies, verdictOf_threshold_congr, inadmissible_is_noop, nonresponsabile_event_noop) | Lean-side vanishing detection | REFERENCED real mechanism, Reactivegas-owned: a vanished/renamed theorem breaks those lines → their build REDs. Our mapping lists the theorems; their persistence is enforced there, never here. |

## Vote/Tests.lean (397 lines — READ IN FULL at r5)

| Lean identity | kind | Haskell disposition |
|---|---|---|
| private adminMember/observerMember, witnessTraceValidFrom/Valid, soleClosure | fixtures/scaffolding | EXCLUDED (Lean test scaffolding; Haskell suite has its own) |
| PUBLIC fixture defs: viewOf, viewOfMixed, oneAdminView, threeAdminView, fourAdminView, fiveAdminView, fourAdminAfterALeftView, aLostAdminView | fixture defs | EXCLUDED-by-kind (rule subject = ctor/arm identities; fixtures aren't in it — stated); Haskell suite owns its fixtures |
| PUBLIC builders vOpen, vCast | trace builders | EXCLUDED same reason |
| Witness states/traces (tieEvents/tiePasses…, zeroEvents/…, departureEvents/Open/CarriesStaleAssents, switchEvents/LeavesOneList, dissentEvents/…Negative, noReviveEvents/…Decided, votePointState, lostStandingEvents/Open, r45PreEvents/Before/After, strangerRejectedQuestionEvents) | executed witnesses | EXCLUDED as fixtures; CITED as review corroboration per corresponding REQ row (same traces must agree — eyeballed at review + audit, never gate kills) |
| `#guard`s (admissibility points, R-45 oracle, stranger class, renounce note) | compile-time checks | Force = upstream `lake build` exit 0 (upstream-owned); Haskell analogs = executed REQ properties (independent instruments) |
| `example`s (R57-07 premise) | proof terms | EXCLUDED; CORROB REQ-NOEXPIRY at review |
| R-48/V-2 sections (tie + zero witnesses marked UNRULED consequences) | unruled-consequence witnesses | CORROB no-default fence; the "never freeze either as product answer" warning TRANSFERS as Haskell review rule (with Lean citation) |
| R-53 + INV-54-FRANCHISE dual-view witnesses (header: post views NOT production-reachable in S62-A) | sensitivity witnesses with reachability caveat | CORROB M7/M13 with the caveat CARRIED OVER: Haskell post-view fixtures likewise observe vote-machine sensitivity and do NOT claim a produced base transition (stated limit in M7/M13 rows) |
| renounce `#guard` (Slice-B note: validates + no effect) | Slice-A fact witness | CORROB D1 renounce-no-op fact |
| Auditor-instrument citation (nonresponsabile-open.lean, sha256 `1f7aa80a`, vs `757dac98`) | Lean-side control precedent | CORROB M1/M2 control SHAPE (complementary mutant), not a requirement |

## KelGroups/Event.lean (52 lines — READ IN FULL; S28-landed CONTEXT)

Historical `Proposal`/`BaseEvent`/`GroupEvent` (untouched #54 evidence —
R30-X). `DirectCommand/admitMember` (landed direct-only — R30-8 context).
`BaseMutation` removeMember/changeRoles (landed non-admitting — M10a's
splice site lives HERE, not under Vote.* — NOTE-006-2(c) selector fix;
reviewed doc fact: "adding an admission constructor stops the exhaustive
enactment matching compiling" corroborates M10a's compiler-totality
framing). `BaseChange` 3 ctors (landed hook evidence; reviewed doc fact:
"adding a fourth stops the hook compiling" corroborates hook-exhaustiveness
context for the R30-10 surface). No new mirror obligations (S28-landed).

## KelGroups/Types.lean (166 lines — READ IN FULL; S28-landed CONTEXT)

Abbrevs Key/Email/RoleName/ProposalId; Admin/Role/Member; isAdminRole/
hasAdmin; RoleDef/GroupConfig; `setInsert` GUARDED
(`if values.contains value then values else value :: values` — D4 premise
source-verified: M4b's unguarded-insert variant inverts this exact guard);
assoc* helpers + 4 assoc theorems (proofs, excluded); GroupView +
lookupMember/isMember/isAdmin/admins/adminCount (landed R30-6 context).
No new mirror obligations.

## KelGroups/{State,Validate,Integration}.lean (CONTEXT granularity)

S28-landed surface, internals owned by the S28 contract. #30-touched
identities (all verified from reads): `PendingBase`, `GroupState`,
`lookupPendingBase`, `groupView` (State); `validateDirectAdmission`,
`validateBaseMutation`, `validateBaseApproval`, `ValidationError`
(Validate); `IntegratedAppFold`, `BaseHook`, `IntegratedEvent`,
`IntegratedError`, `IntegratedResult`, `Integration{reserved,digest,
proposalMutation,appFold,baseHook}`, `commitBaseChange`, `tryEnactBase`,
`applyIntegratedEvent`, `foldIntegrated` (Integration). Consumed, never
re-mirrored.

## Mapping self-check rule (mechanical, over frozen artifacts only)

The drift leg asserts: every §7-PERFILE extent file has ≥1 row in this
table's file column (or expected-empty status); every row resolves (Lean
item verified above; Haskell side per row's stated instrument). New file
with mirrored vocabulary ⇒ row missing ⇒ RED + review. No source parsing
involved — frozen-list comparison only.
