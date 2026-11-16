# S28-1 frozen ticket contract r2 — kelgroups #28 (G28-1)

Ticket owner `t28-app-api` (Muse, `%534`), parent epic `paolino/kelgroups#29`
(owner `%532`), runtime `/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/`.
Brief sha256 `df98d09932edfcabb3465a29c891cfb51386d49ce6ad7cd6aa60748f42396466`.
Pinned origin/main `368b596fef0b6d393c2ac7afc631d236c55d86d1`; frozen base =
the same SHA (no setup commit was needed: `/gate.sh` is already ignored via
the shared `info/exclude`, verified 2026-09-05; HEAD == pinned at freeze). Worktree
`/code/kelgroups-issue-28`, branch `feat/28-generalize-app-api`. Frozen
2026-09-05, before any behavior-changing code, per NOTE-002 re-freeze
checklist. This r2 SUPERSEDES `handoffs/S28-1-CONTRACT.md` (r1, retained
unmodified as evidence; one anchor paragraph was added to r1 then exactly
reverted pre-freeze — the anchors live here in r2 §4 instead). Changes from
later #68/#76/#81 landings require explicit rebind + revalidation. V-2
zero-open/proposer-selfbar is SETTLED but UNLANDED: freeze against current
accepted base, explicit rebind after #68 lands, never anticipate.

## NOTE-002 dispositions (C1–C6)

| correction | disposition |
|---|---|
| C1 client fence | No change: §3 stands (necessary existing client adaptations INCLUDED, adapt-only; no `Fold.purs` redesign, no Reactivegas UI). EPIC-MAP contradiction was epic-side, fixed there. |
| C2 instrument pins | Applied: gate header freezes nix/GHC/cabal/lake-Lean/node/spago/just versions (§7, leg 2b fail-closed). Normalized `GATE_SHA256` projection kept (satisfies self-reference correction). |
| C3 witness relevance | Applied: §6 revised — applicability per cell with N/A reasons; discovered-existing (file:line + status) vs new-S28-1 per property; no coverageless witnesses; leg 4 = inventory + execution proof chain (presence + exit 0 + not-pending + kill excerpts); appendix R maps requirement → discovered → executed → mutant. |
| C4 one coherent slice | Applied: S28-1 carries the FULL promised boundary (before-append refusal, failing-hook atomicity/replay at persistence); NO promised proof deferred. "S28-2 durable-boundary hardening planned" STRUCK. S28-2 is CONTINGENT and UNSCOPED, opening ONLY on a remainder named by S28-1 review. Two was an upper bound, never a second promise. |
| C5 expenditure plan | Applied: planned-expenditure table below (§3); fits 8 builds / 24 probes worst case or BLOCK with gap (it fits — no block). |
| C6 test-only demo | No change: §5 stands (test-only, no executable, no library exposure). #29 runnable demo is a separate follow-on ticket, not S28-1 scope. |

## 1. Reconciliation (frozen inputs read 2026-09-05)

- `LEDGER.md` (2026-09-05T10:50Z) + `artifacts/PLAN-TO-MILESTONE-2.md` + epic
  `EPIC-MAP.md` (as corrected by NOTE-002:C1/C4/C6): stranger six-step outcome
  via published Haskell coordinator on kelgroups; #73 owns rejecting fold,
  GroupView, sealed atomic hook, direct admission, pendingBase, vote
  lifecycle. No vendoring.
- `e-haskell-impl/handoffs/D1-ASSESSMENT.md` (0/26 historical measure, not a
  whitelist), `EPIC43-REQUIREMENT-MAP.md` (R9c/R11: kelgroups-client is an
  in-repo package; Reactivegas UI + core.wasm absent; `Client/Fold.purs`
  second fold in tension with core.wasm; D4 delivery = packaging),
  `ASSENSO-ORACLE-GAP.md` rev2 (composition wire missing; C1–C5 are
  reactivegas#76, NOT here), `REPLAY-CONTEXT-CONTRACT.md` R3.1 (threshold is a
  parameter; replay context is test input, not a shipped default).
- Accepted Lean = Reactivegas `master 4a6cd87` + later rulings. Read exact
  `/code/reactivegas/lean/KelGroups/` modules: `Types.lean` (GroupView sole
  projection), `Event.lean` (DirectCommand sole admission; BaseMutation
  non-admitting — "adding an admission constructor stops the exhaustive
  enactment matching compiling"; BaseChange evidence), `State.lean`
  (PendingBase integrated store; PendingProposal historical; GroupState
  members/pendingProposals/pendingBase/appFold; groupView sole route),
  `Validate.lean` (validateDirectAdmission order
  notAnAdmin/reservedKey/memberAlreadyExists; validateBaseMutation exhaustive;
  validateBaseApproval over pendingBase; NO bootstrap arm on integrated path),
  `Fold.lean` (historical generic fold, no production responsibility),
  `Integration.lean` (IntegratedAppFold signer→pre/post views→state→event→
  Except; BaseHook sealed atomic via commitBaseChange; IntegratedEvent
  distinct params; IntegratedError validation/app; IntegratedResult
  state+Option BaseChange; Integration bundle proposalMutation→BaseMutation;
  foldIntegrated keeps aggregate on error). Vote subtree
  `Vote/{Types,State,Event,Validate,Fold}.lean` as REQUIRED-OF-SUBSTRATE
  vocabulary + production fold (Verdict; Threshold parameter,
  legacyThreshold/zeroThreshold exhibits NOT defaults; Ballot; QuestionKind
  collective/permission-designee; ClosureCause tally/franchiseChange + carried
  proposerDeparted/renounced; placeBallot one-position; sweepClosures same-step
  close with record; validateVoteEvent; no clock/no-expiry). Extent via
  imports/consumers, not an allowlist.
- `questions/A-V2-AND-PLEDGE-AGENCY.md` + `t68-proposer-assent/answers/A-001*`:
  V-2 settled/unlanded (n=1 separate explicit approve; n>1 no proposer-counted
  assent; arithmetic unchanged). `#75/#76/#81`: parameters/test-inputs/deps,
  not S28 scope.
- Dated corrections: #28 `appOnBase`-total/"base never rolls back" STALE →
  sealed atomic BaseHook + rejecting route. #30 immediate-enactment/generic/
  rejection-expiry STALE → routes/QuestionKind/ClosureRecord/no-expiry (S30,
  not S28-1). #29 "legacy laws authoritative" STALE → Lean + rulings win. No
  smuggled notDesignee/notProposer refusals. No theta default.
- Zero-extent control (2026-09-05T11:10Z): grep for
  `IntegratedAppFold|BaseHook|GroupView|IntegratedEvent|commitBaseChange|`
  `pendingBase|DirectCommand|BaseMutation|IntegratedError|IntegratedResult|`
  `foldIntegrated|appendIntegratedEvent` over `lib/`+`test/` = ZERO hits;
  positive control (`AppFold|pendingProposals`) hits 11 files → the zero is
  true absence (new API unbuilt), not a broken instrument.
- Remote base status: `gh run list` main + commit check-runs on 368b596 =
  CI success (2026-08-26 PR#31). Local execution pending in RED (base) and
  GREEN (candidate) runs — see appendix R `executed` column.
- No unresolved authority conflict blocks S28-1. New conflict → BLOCK with
  `questions/Q-NNN-<slug>.md` + competing evidence. Settled rulings not
  reopened. Proceed WITHOUT waiting for #66 repairs or #71 prose.

## 2. Objective (S28-1: the ONE coherent slice)

A real nondegenerate application (a test-only demo instance inside `test/`,
NOT a new shipped executable) has distinct state/event types, sees signer +
read-only sole membership view, gets its domain refusal enforced BEFORE
durable append, and observes base changes through the sealed atomic hook —
proven by frozen gate G28-1 on the exact candidate commit. S28-1 exercises the
real append/replay boundary (`Store`/KEL: accepted events durable with
byte-identical-log refusal proof; tentative-base + failing-hook restoring
pre-state AND pre-log; replay reproducing state), not only a pure helper. NO
promised S28-1 proof is deferred: there is no scoped "S28-2 hardening" slice.
S28-2 is CONTINGENT and UNSCOPED — it opens ONLY on a remainder explicitly
named by S28-1 review (fresh mandate, fresh budget request if so).

## 3. Owned surface, fences, budgets, expenditure

Owned: `lib/KelGroups/{Event,State,Validate,Fold,Types,Store}.hs` (+ minimal
`Bootstrap.hs`/`Server.hs` adaptation ONLY if the `GroupState` field addition
forces a compile fix), `test/` additions, `kelgroups.cabal` test wiring if
needed, `test/Main.hs` spec wiring, `Trivial.hs` UNCHANGED (degenerate
historical presence; still compiles; existing tests pass; explicitly NOT
counted). Necessary existing client adaptations to keep `just ci` green
(`build-client`/`test-client`) are INCLUDED if the library change breaks them
— adapt, do not redesign. Forbidden: Reactivegas browser UI, wholesale
`Client/Fold.purs` deletion/redesign (#84), Reactivegas economics,
vendoring, repo creation, issue/PR comments/reviews/gists, deployment,
publication, release-please merge. `Trivial` is NOT nondegenerate evidence.
`draft=NONE`; no hidden subagents. Auditors NEVER Muse/GLM/Claude. No second
ticket owner on #28. Do not revert others' edits. No gate bypass, no blanket
test exclusion. No parallel heavy builds. Every failed setup/attempt
journaled with command + exit + cause. No automatic raises.

Budgets (binding, initial #28 slice): ticket-owner campaign ≤8 substantive
full build/gate attempts + ≤24 explicitly counted targeted probes; ≤2
submissions; one fresh FULL independent audit per candidate (first: Codex
gpt-6-astra/high), ≤5 substantive audit builds + ≤20 targeted probes per
audit (auditor's envelope, not spent here).

Build-vs-probe rule (frozen): one `gate.sh` envelope execution = 1 BUILD
(it exercises the full tree/CI envelope); each mutant M1–M6
apply+rebuild+test+revert cycle inside it = 1 PROBE (warm-tree focused run:
incremental rebuild of ≤few modules + single-purpose test exec); standalone
focused commands outside any gate run (toolchain/version queries, `--match`
runs, `gh` metadata queries) = 1 PROBE each; file reads, greps,
`git status/rev-parse/diff`, `sha256sum`, `bash -n` = FREE reconnaissance.
Classification is by invocation shape, auditable from logs (cold vs
incremental module counts shown in cabal output).

Planned-expenditure table (worst case = repair bounce used; best case in
parentheses):

| invocation | shape | charged | running totals (worst) |
|---|---|---|---|
| recon p1 toolchain 4-version query (spent) | probe | 1 probe | B0 P1 |
| recon p2 `gh` base-CI status (spent) | probe | 1 probe | B0 P2 |
| recon p3 GHC/spago/nix versions (spent) | probe | 1 probe | B0 P3 |
| RED gate run on setup base, legs 1–5 (leg 4 absent-RED + leg 5 precondition-RED, no mutant rebuilds; leg 6 skipped — base CI already green remotely) | envelope | 1 build | B1 P3 |
| GREEN gate run, submission 1 (legs 1–7) | envelope + 6 mutant cycles | 1 build + 6 probes | B2 P9 |
| GREEN gate run, submission 2 after one repair bounce (only if findings) | envelope + 6 mutant cycles | 1 build + 6 probes | B3 P15 |
| ticket-owner final quiet verification: FULL `gate.sh` via `run-receipt` on accepted tree | envelope + 6 mutant cycles | 1 build + 6 probes | B4 P21 |
| contingency (rebase re-verify if origin/main moves mid-slice) | envelope | ≤1 build | (B5 P21, unplanned) |

Worst-case planned: 4 builds / 21 probes — within 8/24 with headroom 4
builds / 3 probes. Best case (first submission passes): 3 builds / 15
probes. Auditor envelope (5 builds + 20 probes) is SEPARATE and not spent
here; combined feasibility shown by disjoint envelopes. If #68 lands
mid-slice forcing rebind, or second findings close the campaign, the owner
BLOCKs with this table + the concrete gap (new mandate, new budget) — budget
is never a reason to omit acceptance or hide expenditure. Mutant warm-run
speed is bounded by the Haskell skill's measured 23× (cold 364s vs warm
15–17s); even at 3× the measured warm cost the probe count is unchanged
(counts invocations, not minutes).

## 4. Frozen Haskell API (exact names/files)

Historical API KEPT with `HISTORICAL-NON-PRODUCTION` boundary comments
(accepted #54 evidence, NO production responsibility on the new path):
`Proposal(..)` + `IntroduceMember`, `BaseEvent(..)`, `GroupEvent a`,
`AppFold a = a -> a -> a`, `foldGroup`, `applyEvent`, `validateEvent` et al,
`enact`/`applyPropose` over `pendingProposals`, `openKEL`/`appendEvent` raw
path. New production API alongside (no new lib files):

- `Types.hs`: `data GroupView = GroupView { gvMembers :: Map Text Member }`
  (SOLE projection; no payload, no writable path); `lookupMemberInView ::
  Text -> GroupView -> Maybe Member`; `isMemberInView :: Text -> GroupView ->
  Bool`; `isAdminInView :: Text -> GroupView -> Bool`. `Member`/`Role`/
  `GroupConfig` kept.
- `Event.hs`: `data DirectCommand = AdmitMember Text Text (Set Role)` (sole
  insertion vocabulary); `data BaseMutation = RemoveMember Text |
  ChangeRoles Text (Set Role)` (cannot admit; EXHAUSTIVE — the M4 mutant,
  adding an admission constructor, must stop compilation via
  incomplete-patterns under `-Wall -Werror`, exactly the Lean exclusion
  mechanism); `data BaseChange = MemberAdmitted Text | MemberRemoved Text |
  RolesChanged Text`; `data IntegratedEvent bp e = IEDirect DirectCommand |
  IEPropose bp | IEApprove ProposalId | IEApp e` (distinct params; `IE-`
  prefix avoids clashing with historical constructors in-module).
- `State.hs`: `data PendingBase = PendingBase { pbMutation :: BaseMutation,
  pbProposer :: Text, pbApprovals :: Set Text }` (non-admitting-typed);
  `data GroupState s = GroupState { members :: Map Text Member,
  pendingProposals :: Map ProposalId PendingProposal
  {-HISTORICAL-NON-PRODUCTION-}, pendingBase :: Map ProposalId PendingBase
  {-PRODUCTION-}, appFold :: s }` (param now means AppState; field name kept
  for minimal churn); `emptyState :: s -> GroupState s`; `groupView ::
  GroupState s -> GroupView` (sole aggregate→view route);
  `lookupPendingBase :: ProposalId -> GroupState s -> Maybe PendingBase`;
  `adminCount`, `majority`, `isAdmin`, `isMember`, `PendingProposal(..)`,
  `lookupPending` kept.
- `Validate.hs`: `ValidationError` gains `ReservedKey Text`;
  `validateDirectAdmission :: Text {-reserved-} -> GroupState s -> Text
  {-signer-} -> Text {-target-} -> Text {-email-} -> Set Role -> Either
  ValidationError ()` (order: non-admin→`NotAnAdmin`, reserved→`ReservedKey`,
  existing→`MemberAlreadyExists`); `validateBaseMutation :: GroupState s ->
  Text -> BaseMutation -> Either ValidationError ()` (exhaustive);
  `validateBaseApproval :: GroupState s -> Text -> ProposalId -> Either
  ValidationError ()` (reads `pendingBase`). Historical validators kept with
  boundary comments. NO bootstrap arm on the integrated path.
- `Fold.hs`: `type IntegratedAppFold s e err = Text {-signer-} -> GroupView
  -> GroupView {-pre post-} -> s -> e -> Either err s`; `type BaseHook s err
  = BaseChange -> GroupView -> GroupView -> s -> Either err s`; `data
  IntegratedError err = IEValidation ValidationError | IEApp err`; `data
  IntegratedResult s = IntegratedResult { irState :: GroupState s, irChange
  :: Maybe BaseChange }`; `data Integration s e bp err = Integration {
  intReserved :: Text, intDigest :: bp -> ProposalId, intProposalMutation ::
  bp -> BaseMutation, intAppFold :: IntegratedAppFold s e err, intBaseHook
  :: BaseHook s err }`; `commitBaseChange :: Integration s e bp err ->
  GroupState s -> GroupState s -> BaseChange -> Either (IntegratedError err)
  (IntegratedResult s)` (sealed atomic: hook `Err` discards the whole
  transition); `tryEnactBase :: Integration s e bp err -> GroupState s ->
  ProposalId -> Either (IntegratedError err) (IntegratedResult s)`;
  `applyIntegratedEvent :: Integration s e bp err -> GroupState s -> Text ->
  IntegratedEvent bp e -> Either (IntegratedError err) (IntegratedResult s)`
  (SINGLE shared step: app route checks membership then `intAppFold`; base
  routes validate then `commitBaseChange`; successful app event touches ONLY
  payload); `foldIntegrated :: Integration s e bp err -> s -> [(Text,
  IntegratedEvent bp e)] -> GroupState s` (keeps aggregate on `Err`).
- `Store.hs`: `openIntegratedKEL :: (FromJSON s, FromJSON e, FromJSON bp) =>
  Integration s e bp err -> s -> FilePath -> IO (KELStore s)` (replays via
  `foldIntegrated`); `appendIntegratedEvent :: (ToJSON s, ToJSON e, ToJSON
  bp) => KELStore s -> Integration s e bp err -> Text -> IntegratedEvent bp
  e -> IO (Either (IntegratedError err) (IntegratedResult s))`
  (validate-then-append: `applyIntegratedEvent` first; `Err` persists NOTHING
  and touches NO state; `Ok` inserts the SQL row then updates
  stateVar/tip/length). Historical `openKEL`/`appendEvent` kept for existing
  Server/tests. `readState`/`kelLength` reused.
- `Trivial.hs`: UNCHANGED. `Bootstrap.hs`/`Server.hs`: compile-fix only if
  forced, no behavior change, no new decision path.

HARNESS ANCHORS (frozen literals the gate seds verbatim; the implementation
MUST contain them — minimal mechanical prescription mirroring Lean, enabling
the frozen mutants; anything else about bodies is the commit owner's choice):
(A1) `applyIntegratedEvent`'s app route contains exactly `isMemberInView
signer view` (else-branch yields `IEValidation (NotAMember signer)`); (A2)
`foldIntegrated`'s accumulator is named `gs` (Lean: `fun gs signed => ...`)
and its refusal arm is the literal `=> gs`; (A3) `appendIntegratedEvent`'s
persist statement contains the literal `INSERT INTO events`; (A4)
`validateDirectAdmission` keeps the frozen 6-argument shape (for possible
future harness use; no current mutant edits it).

## 5. Test-only demo instance (exact files, NOT shipped)

- `test/S28DemoApp.hs` (`module S28DemoApp`): `data DemoState = DemoState {
  demoCounter :: Int, demoLog :: [Text] }` (Eq/Show/Generic/ToJSON/FromJSON);
  `data DemoEvent = DemoAdd Int | DemoReset | DemoNoop` (same classes) —
  `DemoState ≠ DemoEvent` (`e ≠ s`, distinct declarations); `data DemoError
  = DemoNotAdmin Text | DemoNegative Int | DemoHookRefused Text` (Eq/Show);
  `data DemoProposal = DemoRemove Text | DemoChangeRoles Text (Set Role)`
  (Eq/Show/Generic/ToJSON/FromJSON); `demoProposalMutation :: DemoProposal ->
  BaseMutation`; `demoDigest :: DemoProposal -> ProposalId`; `demoReserved
  :: Text` (fixed reserved key); `demoAppFold :: IntegratedAppFold DemoState
  DemoEvent DemoError` (signer via view: `DemoReset` requires
  `isAdminInView signer preView` else `DemoNotAdmin`; `DemoAdd n` refuses `n
  < 0` with `DemoNegative`; reads ONLY views+state+event, writes ONLY
  `DemoState`); `demoBaseHook :: BaseHook DemoState DemoError` (exact
  pre/post views + change; refuses `MemberRemoved protectedKey` with
  `DemoHookRefused` for a fixed demo member, else appends a visible
  consequence to `demoLog`); `demoIntegration :: Integration DemoState
  DemoEvent DemoProposal DemoError`. JSON instances for Store persistence. No
  executable, no library exposure.
- `test/S28AppApiSpec.hs` (`module S28AppApiSpec (spec)`): six `describe`
  groups with EXACT strings (gate inventory greps this extent):
  `"S28-1 distinct types + signer + GroupView"`, `"S28-1 rejecting step
  before append"`, `"S28-1 atomic hook"`, `"S28-1 direct-only admission"`,
  `"S28-1 validate/fold agreement"`, `"S28-1 no client-decided authority"`.
  QuickCheck with standalone generators (no `Arbitrary` instances) where
  trace properties are natural; agreement traces MUST include non-member and
  domain-invalid events (so the M5 divergence is observable). Wired into
  `kelgroups.cabal` `other-modules` + `test/Main.hs`. A seventh group is
  allowed but never required.
- `test/Generators.hs` positional `GroupState` construction adapted minimally
  for the new `pendingBase` field (empty unless planted).

## 6. Requirements → witnesses + killers (r2: applicability explicit)

Applicability key: P = positive reachable witness; R = refusal witness;
A = atomicity/replay witness; M = can-fail mutant (must go RED). N/A states
the reason — a kind-mismatch, never an omission. Execution proof = leg 4
(exit 0 + slug executed + not-pending); kill proof = leg 5 excerpt. Full
pointer map in appendix R.

| # | requirement | P | R | A | M (must RED) |
|---|---|---|---|---|---|
| 1 | distinct state/event types + signer + sole GroupView | P1 demo compiles with `DemoEvent ≠ DemoState`; member `DemoAdd` authorizes by signer through views (`test/S28DemoApp.hs`, group `S28-1 distinct types + signer + GroupView`) | R1 non-member `IEApp` refused `IEValidation (NotAMember signer)` via the integrated route BEFORE any fold (same group). Shared mechanism with #2-R — stated, not duplicated. | A1: N/A as a SEPARATE replay property — replay agreement for the demo is requirement #5's property (same `foldIntegrated` mechanism); this row asserts (a) the demo constructs through the integrated route only and (b) that route's replay is #5. No duplicate test; appendix R points at #5. | M1 conflate `e=s`: append `_m1_conflate :: DemoEvent -> DemoState` / `_m1_conflate = id` to `test/S28DemoApp.hs` → `cabal build all -O0` RED naming `DemoEvent`/`DemoState` (build-mutant; proves genuine distinctness, not aliases). |
| 2 | rejecting integrated step BEFORE durable append | P2 `appendIntegratedEvent` validates then appends: accepted event durable (`kelLength`+1, `readState` advances, row present) (group `S28-1 rejecting step before append`) | R2 domain-invalid (`DemoAdd (-1)` → `DemoNegative`) AND non-member events return `Err` and are NEVER appended: SQL `COUNT(*)` + file bytes byte-identical + `readState` unchanged (same group) | A2 refused-event log + folded state both unchanged after RETRY + full re-read (`openIntegratedKEL`) — the persistence half of R2, distinguished from #3-A (which is about a TENTATIVE base change, not a refused app event) | M2 bypass membership gate: in `Fold.hs` `applyIntegratedEvent` block replace `isMemberInView signer view` (A1) with `True` → test RED naming `S28-1 rejecting step before append` (non-member now accepted+appended). Shared-mechanism collateral on #1-R1 expected and recorded, not a failure. |
| 3 | sealed atomic hook (`commitBaseChange` discards whole transition on hook refusal) | P3 base change + hook success commits state + `irChange = Some change` evidence (`MemberAdmitted`/`MemberRemoved`/`RolesChanged`) (group `S28-1 atomic hook`) | R3 failing hook (`demoBaseHook` → `DemoHookRefused` on `MemberRemoved protectedKey`) rejects the WHOLE transition (`IEApp` `Err`, `irState` discarded) (same group) | A3 tentative base change + failing hook via `appendIntegratedEvent` → pre-state (`readState` equal) AND pre-log (`kelLength` + `COUNT(*)` + bytes equal) restored; replay confirms (same group). THE S28-1 persistence-atomicity proof — nothing deferred. | M3 ignore hook `Err`: stub `commitBaseChange` equations to `commitBaseChange _ _ post change = Right (IntegratedResult post (Just change))` → test RED naming `S28-1 atomic hook` (failing hook now commits). |
| 4 | direct-only admission (one writable store, one insertion path; voted admission unrepresentable) | P4 direct admit by admin (`IEDirect (AdmitMember …)` + `validateDirectAdmission`) inserts member (group `S28-1 direct-only admission`) | R4 TYPE-LEVEL (compile-time): `BaseMutation` has no admission constructor; `validateBaseMutation`/`enactMutation` exhaustive. There is NO runtime refusal to witness — an unrepresentable event cannot be constructed. The executable correlate: every `BaseMutation` value in generated traces never inserts (property in the same group). | A4: N/A as runtime replay — the claim is structural (储 pending store typed `PendingBase.pbMutation :: BaseMutation`; historical `pendingProposals`/`IntroduceMember` carry `HISTORICAL-NON-PRODUCTION` markers with zero new-path responsibility). The executable correlate (pendingBase round-trips through Store without admitting) rides in P4's Store test. | M4 re-add admission: insert `\| AdmitMember Text Text (Set Role)` into the `BaseMutation` block in `Event.hs` → build RED via incomplete-patterns (`enactMutation`/`validateBaseMutation`) naming `BaseMutation`/`AdmitMember` (build-mutant; the exact Lean exclusion mechanism). |
| 5 | validate/fold agreement (single shared step; replay of accepted KEL never rejects) | P5 accepted event folds identically via direct `applyIntegratedEvent` and single-element `foldIntegrated` (group `S28-1 validate/fold agreement`) | R5: N/A — agreement has no refusal of its own (refusals belong to #2/#3); the property quantifies over accepted events AND records that refused ones leave both paths unchanged (same group, stated as boundary, not a separate refusal row). | A5 QuickCheck over generated integrated traces (INCLUDING non-member and domain-invalid events): iterative `applyIntegratedEvent` states equal `foldIntegrated` state at every prefix; replay of an accepted KEL never rejects (same group). Also covers #1-A1 by reference. | M5 diverge replay error handling: in `Fold.hs` `foldIntegrated` block replace refusal-keep `=> gs` (A2) with `=> error "MUTANT-M5"` (replay crashes where accept keeps) → test RED naming `S28-1 validate/fold agreement` (traces with refusals disagree). |
| 6 | no client-decided authority (integrated boundary sole decider) | P6 demo counter/log verdicts observable ONLY through `applyIntegratedEvent`/`foldIntegrated`/`appendIntegratedEvent` (group `S28-1 no client-decided authority`) | R6 out-of-band write ignored: historical raw `appendEvent (App …)` cannot even be instantiated for the demo (`AppFold DemoState` would need `DemoState -> DemoState -> DemoState` consuming `DemoEvent` — ill-typed), and a direct record update leaves the LOG unchanged so re-read reverts it (same group) | A6: N/A as separate replay — durability-of-only-integrated-verdicts is #2-A2 + #3-A3; this row asserts the negative (no second durable path), proven by M6 + the log-explains-state property in the same group (replayed log via `foldIntegrated` reproduces `readState`). | M6 break durable persist: in `Store.hs` `appendIntegratedEvent` block rename `INSERT INTO events` (A3) to a nonexistent table → accepted appends throw (no durable row) → test RED naming `S28-1 no client-decided authority` (log no longer explains state). Still uses all bindings (compiles; behavior regresses). |

`Trivial` appears in NO positive column. No property is coverageless: every
row has ≥1 new S28-1 property; regression rows additionally cite discovered
existing suites (appendix R).

## 7. Frozen gate G28-1 (immutable; hash-bound header; `gate.sh` untracked+ignored)

`gate.sh` at `/code/kelgroups-issue-28/gate.sh` (untracked; ignored via
`/gate.sh` in the shared `info/exclude` — `git rev-parse --git-common-dir` =
`/code/kelgroups/.git`, exclude line 7, pre-existing, covers all worktrees; no
setup commit required), backed up as
`handoffs/gate.sh.backup`. Run IN ORDER on the exact head with `set +e`
(every leg prints command, exit, diagnosis; final exit nonzero iff any leg
failed). FROZEN_BASE = setup SHA below (origin/main + gitignore only):

- Leg 1 (`tracked hygiene`, before AND after): `git status --porcelain |
  grep -v '^??'` empty. Ignored `gate.sh`/build trees never count.
- Leg 2 (`identity + self-hash`): `git rev-parse HEAD` recorded;
  `GATE_SHA256="<frozen>"` equals `sed 's/^GATE_SHA256=".*"/GATE_SHA256=""/'
  gate.sh | sha256sum` (blank-normalized; documented here to close the
  self-reference). Pinned base `368b596…`; frozen base = setup SHA.
- Leg 2b (`instruments pinned, fail closed` — C2): header freezes
  nix=2.31.3, GHC=9.8.4, cabal=3.16.1.0, lake=5.0.0-src/Lean=4.25.0,
  node=v20.19.6 (devshell), spago=1.0.3, just=1.43.1 (journaled 2026-09-05).
  The leg resolves each via `nix develop .#ci --quiet -c <tool> --version`
  (nix itself directly) and FAILS with diagnosis on any mismatch — a hash
  bound to a wrapper alone does not bind what compiles/tests.
- Leg 3 (`build`): `nix develop .#ci --quiet -c just build` (≡ `cabal build
  all -O0` in the CI shell) exit 0. Cold build is the whole cost; later legs
  are warm.
- Leg 4 (`tests: inventory + execution proof` — C3): discover extent (`find
  test -name '*.hs'`); INVENTORY: ≥6 distinct `S28-1 ` describes (never a
  hardcoded count), each of the six exact slugs present — presence alone
  proves NOTHING further. EXECUTION: `nix develop .#ci --quiet -c cabal test
  all -O0 --test-show-details=direct` exit 0 AND each slug appears in the
  direct output (executed, not skipped) AND no `pending` within 3 lines after
  any slug (not vacuous). Non-vacuity is closed by leg 5 kills.
- Leg 5 (`mutants M1–M6`, one at a time): precondition greps first (fail
  loudly on harness drift); apply; print mutant diff sha256; rebuild+retest
  (warm); require RED — M1/M4: build nonzero naming `DemoEvent|DemoState`
  / `BaseMutation|AdmitMember|incomplete|non-exhaustive`; M2/M3/M5/M6: test
  nonzero with the row's slug + failure markers in output (shared-mechanism
  collateral allowed, corresponding slug required); `git checkout --` the
  touched files; next mutant only when the tracked tree is clean again.
  Record diff hash + failing excerpt (printed for capture).
- Leg 6 (`full just ci`): `nix develop .#ci --quiet -c just ci` — the ACTUAL
  CI command from `.github/workflows/ci.yml` (fourmolu/cabal-fmt in-place ≡
  check when leg 1 stays clean; hlint; build; test; `lake build`; client
  build+test) — exit 0 on the exact head.
- Leg 7 (`Trivial degenerate presence`): exports present; builds (leg 3);
  existing suites pass (leg 4 exit 0); `Trivial.hs` contains NO `S28-1 `
  (explicitly NOT counted).

RED bundle (commit owner, BEFORE green): this exact frozen gate on the setup
base MUST RED leg 4-inventory (0 groups: absence, with historical suites
still exit-0 inside the failing run — absence, not breakage) + leg 5
preconditions (frozen names absent), with the frozen gate hash recorded.
GREEN: same gate exit 0. No push without epic authorization; draft PR only
after GREEN + fresh FULL audit.

No setup commit exists: HEAD == pinned `368b596…` at freeze (verified in
`NOTE GATE-FROZEN`; `git diff` empty, `git status` tracked-empty).

## 8. S30 dependency surface (planned, NOT built)

No vote machine in S28-1. S30 composes against: `IntegratedEvent bp e`'s
distinct `bp` param + `proposalMutation :: bp -> BaseMutation` + `digest`
(question payloads WITHOUT touching the substrate boundary);
`PendingBase`/`BaseMutation` non-admitting shape; `GroupView` sole projection
(franchise derivation point); `BaseHook` atomicity (post-enactment
consequences); `Question`/`Ballot`/`Verdict`/`Threshold`-param/
`ClosureRecord`/`foldVote`/`sweepClosures` to be built there. Upstream Lean
gaps enumerated in the S30 contract after S28 lands — never invented here.

## 9. Residual risks + rebind

- `GroupState` +1 field: positional constructors in `Generators.hs`/specs
  need minimal adaptation (test wiring, no widening). `Server.hs`/
  `Bootstrap.hs` only if the compiler forces it.
- PureScript client consumes Haskell only via HTTP/JSON: if new integrated
  JSON shapes break `build-client`/`test-client`, adapt minimally (no
  `Fold.purs` redesign).
- CESR/`ReservedKey` guard order exactly per Lean; deviation = finding.
- V-2 rebind after #68 lands (approve path + majority + selfbar),
  revalidated; never anticipated.
- No discovery quotas; no `admit` escape hatches.

## 10. Supervision + reporting (binding)

Commit owner + each fresh auditor in distinct `reactivegas:8` panes via
`tmux split-window -d`, `send-pointer`, post-cursor `START` (pane + family +
alternate check); presence ≠ dispatch. Immediate child only; verify every
material claim against artifacts (read files, not events; mechanism, not
intent, for low-discipline families). Upward LOCAL FILES ONLY (`handoffs/` +
`/tmp/reactivegas/ms2/inbox/` pointer + STATUS); NEVER desk `%510` or human
composers. Inbox checked before every phase, expensive command, evidence
freeze, `COMPLETE`. Stale alarm: `monitor-workers` foreground wait,
`TMUX_NOTIFY_TARGET` = ticket pane; receipt = a `STALE`/`NEVER-STARTED` line
naming the child root reaching this pane. Stops terminal or `PARKED:
<reason>; wake=<condition>` or `BLOCKED Q-…`.

## Appendix R — reconciliation map (C3)

`discovered` = existing tests on the setup base covering the historical shape
(status: remote CI green on 368b596 2026-08-26; local execution pending in
RED leg 4 — historical suites must stay exit-0 inside the absent-RED run).
`new` = S28-1 properties (absent on base by the §1 zero-extent control;
present + passing on candidate). `executed`/`killed` are filled from RED then
GREEN receipts; at freeze they state the exact expected evidence.

| req | discovered existing (file:line, covers) | new S28-1 (file) | executed proof (expected) | killing mutant (expected) |
|---|---|---|---|---|
| 1 distinct/signer/view | `test/ValidateSpec.hs:147` "non-member app event rejected" + `:158` "member app event accepted" (HISTORICAL conflated `App a` membership check — ancestor shape, NOT the sealed boundary); `test/FoldSpec.hs:45-46` "App/base separation / app event preserves members" (historical fold payload discipline) | `test/S28AppApiSpec.hs` group `S28-1 distinct types + signer + GroupView` (P1+R1) | leg 4 exit 0 + slug executed + not-pending | M1 build-RED (`DemoEvent`/`DemoState`); M2 collateral on R1 recorded |
| 2 reject-before-append | `test/StoreSpec.hs:134` "roundtrip" + `:187` "fold consistency" + `:340` "kelLength" (historical Store durability: TVar-vs-replay equality, lengths — the durability instrument the new refusal proof reuses); `test/StoreInvariantsSpec.hs:59` store-through invariants | same spec, group `S28-1 rejecting step before append` (P2+R2+A2) | leg 4 exit 0 + slug executed + not-pending | M2 test-RED (slug); M6 collateral possible (shared persist path) |
| 3 atomic hook | NONE on base (no hook exists — zero-extent §1; hook atomicity is entirely new) | same spec, group `S28-1 atomic hook` (P3+R3+A3) | leg 4 exit 0 + slug executed + not-pending | M3 test-RED (slug) |
| 4 direct-only | `test/ValidateSpec.hs:175ff` "Normal mode: proposal constraints" (`:176` introduce-existing rejected, `:199` remove-nonmember) + `:239ff` approval constraints + `test/InvariantsSpec.hs:77ff` majority properties (historical proposal discipline; the voted-admission shape being retired from production) | same spec, group `S28-1 direct-only admission` (P4+R4-type-level+A4-correlate) | leg 4 exit 0 + slug executed + not-pending | M4 build-RED (exhaustiveness) |
| 5 agreement | `test/StoreSpec.hs:187` "fold consistency" (incremental-TVar vs reopened-replay equality — the ancestor of accept→replay agreement, on the historical fold) + `test/TransitionInvariantsSpec.hs:122` "foldGroup_nil" | same spec, group `S28-1 validate/fold agreement` (P5+A5; R5 N/A) | leg 4 exit 0 + slug executed + not-pending | M5 test-RED (slug) |
| 6 no-2nd-authority | `test/StoreSpec.hs:187` "fold consistency" (log-explains-state instrument reused) + `test/ServerSpec.hs`+`test/E2ESpec.hs` lifecycle suites (all verdicts through the single server/Store path — no second writer on base) | same spec, group `S28-1 no client-decided authority` (P6+R6) | leg 4 exit 0 + slug executed + not-pending | M6 test-RED (slug) |
| regression (no breakage) | `test/FoldSpec.hs`, `test/ValidateSpec.hs`, `test/InvariantsSpec.hs`, `test/TransitionInvariantsSpec.hs`, `test/StoreSpec.hs`, `test/StoreInvariantsSpec.hs`, `test/ServerSpec.hs`, `test/E2ESpec.hs`, `test/MultiClientSpec.hs`, `test/JwkSpec.hs` (full historical suite; remote green on base) | none (must stay green untouched in behavior) | leg 4 exit 0 + leg 6 exit 0 on candidate | N/A (any RED here = regression finding, not a kill) |

No row is coverageless: #3 (no discovered ancestor) is covered by three new
properties + M3 kill; all others pair discovered instruments with new
boundary properties. Six slugs alone prove nothing — the proof is exit-0 +
executed + not-pending + per-row kill, all four, per row.
