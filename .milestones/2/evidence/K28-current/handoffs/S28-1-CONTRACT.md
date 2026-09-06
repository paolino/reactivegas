# S28-1 frozen ticket contract — kelgroups #28 slice 1 (G28-1)

Ticket owner `t28-app-api` (Muse, `%534`), parent epic `paolino/kelgroups#29`
(owner `%532`), runtime `/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/`.
Brief sha256 `df98d09932edfcabb3465a29c891cfb51386d49ce6ad7cd6aa60748f42396466`.
Base `368b596fef0b6d393c2ac7afc631d236c55d86d1` (= `origin/main`, verified
2026-09-05), worktree `/code/kelgroups-issue-28`, branch
`feat/28-generalize-app-api`. Frozen 2026-09-05, before any behavior-changing
code. Changes from later #68/#76/#81 landings require explicit rebind +
revalidation. V-2 zero-open/proposer-selfbar is SETTLED but UNLANDED: this
contract freezes against the current accepted base and plans explicit rebind
after #68 lands; it never anticipates candidate semantics.

## 1. Reconciliation (frozen inputs read 2026-09-05)

- `LEDGER.md` (2026-09-05T10:50Z) + `artifacts/PLAN-TO-MILESTONE-2.md` + epic
  `EPIC-MAP.md`: outcome = stranger drives six-step journey via published
  Haskell coordinator on kelgroups; #73 owns rejecting fold, GroupView, sealed
  atomic hook, direct admission, pendingBase, vote lifecycle. No vendoring.
- `e-haskell-impl/handoffs/D1-ASSESSMENT.md`: 0/26 gap is historical measure,
  not a whitelist; discovery-driven extent governs. `EPIC43-REQUIREMENT-MAP.md`
  (R9c/R11 corrections: kelgroups-client is a package inside kelgroups;
  Reactivegas UI + core.wasm absent; `Client/Fold.purs` second base-fold in
  tension with core.wasm; delivery choice for D4 is packaging). 
  `ASSENSO-ORACLE-GAP.md` rev2 (composition wire missing; C1–C5 belong to
  reactivegas#76 — NOT built here). `REPLAY-CONTEXT-CONTRACT.md` R3.1
  (threshold is a parameter; replay context is test input, not a shipped
  default).
- Accepted Lean = Reactivegas `master 4a6cd87` + later operator rulings. Read
  exact modules in `/code/reactivegas/lean/KelGroups/`: `Types.lean`
  (GroupView sole projection), `Event.lean` (DirectCommand sole admission,
  BaseMutation non-admitting, BaseChange evidence), `State.lean`
  (PendingBase integrated store, PendingProposal historical, GroupState with
  members/pendingProposals/pendingBase/appFold, groupView sole route),
  `Validate.lean` (validateDirectAdmission exact order
  notAnAdmin/reservedKey/memberAlreadyExists, validateBaseMutation exhaustive,
  validateBaseApproval over pendingBase, no bootstrap arm on integrated path),
  `Fold.lean` (historical generic fold, no production responsibility),
  `Integration.lean` (IntegratedAppFold signer→pre/post views→state→event→
  Except, BaseHook sealed atomic via commitBaseChange, IntegratedEvent distinct
  params, IntegratedError validation/app, IntegratedResult state+Option
  BaseChange, Integration bundle with proposalMutation→BaseMutation,
  foldIntegrated keeps aggregate on error). Vote subtree `Vote/{Types,State,
  Event,Validate,Fold}.lean` read as REQUIRED-OF-SUBSTRATE vocabulary +
  production fold (Verdict positive/negative/open, Threshold parameter with
  legacyThreshold/zeroThreshold exhibits NOT defaults, Ballot, QuestionKind
  collective/permission-designee, ClosureCause tally/franchiseChange +
  carried proposerDeparted/renounced, placeBallot one-position, sweepClosures
  same-step close with record, validateVoteEvent boundary, no clock/no-expiry).
  Complete relevant extent discovered via imports/consumers, not an allowlist.
- `questions/A-V2-AND-PLEDGE-AGENCY.md` + `t68-proposer-assent/answers/A-001*`:
  V-2 settled/unlanded as above; n=1 sole admin separate explicit approve,
  n>1 proposer cannot supply counted assent, majority arithmetic unchanged.
- `#75/#76/#81`: threshold parameter stays; replay context test input for
  S28-2, not a kelgroups shipped default; composition/lifecycle rows are
  dependencies, not S28 scope.
- Dated corrections applied: #28 `appOnBase` total + "base never rolls back"
  STALE → sealed atomic BaseHook (Except, commitBaseChange discards whole
  transition on hook refusal) + rejecting integrated route. #30
  single-admin immediate enactment / generic proposals / rejection-expiry
  STALE → direct/base/app routes, QuestionKind, explicit ClosureRecord,
  no-expiry/retention (S30 scope, not S28-1). #29 "legacy laws authoritative"
  STALE → Lean + later rulings win. No refusals smuggled from dormant
  notDesignee/notProposer (zero construction sites). No theta default assumed.
- Current kelgroups #28/#30/#29 bodies are STALE INPUTS as corrected above.
  No unresolved authority conflict blocks S28-1. If one surfaces: BLOCK with
  `questions/Q-NNN-<slug>.md` + exact competing evidence; settled rulings are
  not reopened. Contract + implementation proceed WITHOUT waiting for
  reactivegas #66 repairs or #71 prose.

## 2. Objective (S28-1, one observable outcome)

A real nondegenerate application (a test-only demo instance inside `test/`,
NOT a new shipped executable) has distinct state/event types, sees signer +
read-only sole membership view, gets its domain refusal enforced BEFORE
durable append, and observes base changes through the sealed atomic hook —
proven by frozen gate G28-1 on the exact candidate commit. S28-1 already
exercises the real append/replay boundary (`Store`/KEL), not only a pure
helper. S28-2 (durable-boundary hardening) is planned, max 2 slices total,
justified by independently reviewable behavior, never to avoid full
acceptance.

## 3. Owned surface (+ fences)

Owned: `lib/KelGroups/{Event,State,Validate,Fold,Types,Store}.hs` (+ minimal
`Bootstrap.hs`/`Server.hs` adaptation ONLY if the type change forces it),
`test/` additions for the new properties, `kelgroups.cabal` test wiring if
needed, `Trivial.hs` kept compiling as degenerate instance (unchanged,
historical degenerate, explicitly NOT a nondegenerate witness). Necessary
existing client type/API adaptations to keep `just ci` green
(`build-client`/`test-client`) are INCLUDED if the library type change breaks
them — adapt, do not redesign. Forbidden: Reactivegas browser UI, wholesale
`Client/Fold.purs` deletion or base-fold redesign (belongs to #84),
Reactivegas economics in kelgroups, vendoring, repo creation, issue/PR
comments/reviews/gists, deployment, publication, release-please merge.
`Trivial` stays working but is NOT evidence of nondegenerate capability.
Commit owner: `draft=NONE`, no hidden implementation subagents. Auditors are
NEVER Muse/GLM/Claude. No second ticket owner acts on #28. Do not revert
edits made by others. No gate bypass, no blanket test exclusion. No parallel
heavy builds inside the ticket. Budgets binding per initial #28 slice: ticket
owner ≤8 substantive full build/gate attempts + ≤24 counted targeted probes;
one fresh FULL independent audit per candidate (first auditor Codex
gpt-6-astra/high), ≤5 substantive audit builds + ≤20 targeted probes per
audit. Journal every failed setup/attempt with command + exit + cause. No
automatic raises.

## 4. Frozen Haskell API (exact names/files)

Historical API is KEPT with explicit `HISTORICAL-NON-PRODUCTION` boundary
comments (accepted #54 evidence, receives NO production responsibility on the
new path): `Proposal(..)` with `IntroduceMember`, `BaseEvent(..)`,
`GroupEvent a`, `AppFold a = a -> a -> a`, `foldGroup`, `applyEvent`,
`validateEvent`/`validateProposal`/`validateApproval`, `enact`/`applyPropose`
over `pendingProposals`, `openKEL`/`appendEvent` raw path. New production API
alongside in the same owned files (no new lib files, no wholesale redesign):

- `lib/KelGroups/Types.hs`: `data GroupView = GroupView { gvMembers :: Map
  Text Member }` (SOLE membership projection; no app payload, no writable
  path); `lookupMemberInView :: Text -> GroupView -> Maybe Member`;
  `isMemberInView :: Text -> GroupView -> Bool`; `isAdminInView :: Text ->
  GroupView -> Bool`. Existing `Member`/`Role`/`GroupConfig` kept.
- `lib/KelGroups/Event.hs`: `data DirectCommand = AdmitMember Text Text (Set
  Role)` (sole insertion vocabulary); `data BaseMutation = RemoveMember Text
  | ChangeRoles Text (Set Role)` (cannot admit; exhaustive — adding an
  admission constructor stops `enactMutation`/`validateBaseMutation`
  compiling under `-Wall -Werror`); `data BaseChange = MemberAdmitted Text |
  MemberRemoved Text | RolesChanged Text`; `data IntegratedEvent bp e =
  IEDirect DirectCommand | IEPropose bp | IEApprove ProposalId | IEApp e`
  (distinct params; app event cannot be a proposal; `IE-` prefix avoids clash
  with historical constructors in the same module).
- `lib/KelGroups/State.hs`: `data PendingBase = PendingBase { pbMutation ::
  BaseMutation, pbProposer :: Text, pbApprovals :: Set Text }` (typed by
  non-admitting mutation); `data GroupState s = GroupState { members :: Map
  Text Member, pendingProposals :: Map ProposalId PendingProposal
  {-HISTORICAL-NON-PRODUCTION-}, pendingBase :: Map ProposalId PendingBase
  {-PRODUCTION-}, appFold :: s }` (param now means AppState; field name kept
  to minimize churn); `emptyState :: s -> GroupState s` (new field emptied);
  `groupView :: GroupState s -> GroupView` (sole route aggregate→view);
  `lookupPendingBase :: ProposalId -> GroupState s -> Maybe PendingBase`;
  keep `adminCount`, `majority`, `isAdmin`, `isMember`,
  `PendingProposal(..)`, `lookupPending`.
- `lib/KelGroups/Validate.hs`: add `ReservedKey Text` to `ValidationError`;
  `validateDirectAdmission :: Text {-reserved-} -> GroupState s -> Text
  {-signer-} -> Text {-target-} -> Text {-email-} -> Set Role -> Either
  ValidationError ()` (fixed guard order: non-admin→`NotAnAdmin`, reserved→
  `ReservedKey`, existing→`MemberAlreadyExists`); `validateBaseMutation ::
  GroupState s -> Text -> BaseMutation -> Either ValidationError ()`
  (exhaustive over `BaseMutation`); `validateBaseApproval :: GroupState s ->
  Text -> ProposalId -> Either ValidationError ()` (reads `pendingBase`).
  Historical validators kept with boundary comments. No bootstrap arm on the
  integrated path: empty group admits nobody; founding admin arrives via the
  application's guarded initial aggregate.
- `lib/KelGroups/Fold.hs`: `type IntegratedAppFold s e err = Text {-signer-}
  -> GroupView -> GroupView {-pre post-} -> s -> e -> Either err s`;
  `type BaseHook s err = BaseChange -> GroupView -> GroupView -> s ->
  Either err s`; `data IntegratedError err = IEValidation ValidationError |
  IEApp err`; `data IntegratedResult s = IntegratedResult { irState ::
  GroupState s, irChange :: Maybe BaseChange }`; `data Integration s e bp err
  = Integration { intReserved :: Text, intDigest :: bp -> ProposalId,
  intProposalMutation :: bp -> BaseMutation, intAppFold ::
  IntegratedAppFold s e err, intBaseHook :: BaseHook s err }`;
  `commitBaseChange :: Integration s e bp err -> GroupState s -> GroupState s
  -> BaseChange -> Either (IntegratedError err) (IntegratedResult s)`
  (sealed atomic: hook `Err` discards whole transition);
  `tryEnactBase :: Integration s e bp err -> GroupState s -> ProposalId ->
  Either (IntegratedError err) (IntegratedResult s)`;
  `applyIntegratedEvent :: Integration s e bp err -> GroupState s -> Text ->
  IntegratedEvent bp e -> Either (IntegratedError err) (IntegratedResult s)`
  (SINGLE shared step: validates then effects on every route; app route checks
  `isMemberInView`→`NotAMember` then `intAppFold`; base routes validate then
  `commitBaseChange`; success app event touches ONLY app payload, members +
  both pending stores + change evidence untouched);
  `foldIntegrated :: Integration s e bp err -> s -> [(Text, IntegratedEvent bp
  e)] -> GroupState s` (keeps aggregate on `Err`; refusal advances nothing).
- `lib/KelGroups/Store.hs`: `openIntegratedKEL :: (FromJSON s, FromJSON e,
  FromJSON bp) => Integration s e bp err -> s -> FilePath -> IO (KELStore s)`
  (replays stored integrated events via `foldIntegrated`, not the historical
  fold); `appendIntegratedEvent :: (ToJSON s, ToJSON e, ToJSON bp) =>
  KELStore s -> Integration s e bp err -> Text -> IntegratedEvent bp e -> IO
  (Either (IntegratedError err) (IntegratedResult s))` (validate-then-append:
  runs `applyIntegratedEvent` first; on `Err` persists NOTHING and touches NO
  in-memory state — modeled state AND durable log unchanged; on `Ok` inserts
  the SQL row then updates stateVar/tip/length). Historical `openKEL`/
  `appendEvent` kept for existing Server/tests until migration; new tests use
  the integrated path. `readState`/`kelLength` reused (same `GroupState s`).
- `lib/KelGroups/Trivial.hs`: UNCHANGED (degenerate historical presence;
  still compiles; existing tests pass; explicitly NOT counted).
- `lib/KelGroups/Bootstrap.hs`/`Server.hs`: touch ONLY if the `GroupState`
  field addition forces a compile fix (e.g., record construction/update);
  no behavior change, no new decision path, no `Client/Fold.purs` redesign.

## 5. Test-only demo instance (exact files, NOT shipped)

- `test/S28DemoApp.hs` (`module S28DemoApp`): `data DemoState = DemoState {
  demoCounter :: Int, demoLog :: [Text] }` deriving (Eq, Show, Generic,
  ToJSON, FromJSON); `data DemoEvent = DemoAdd Int | DemoReset | DemoNoop`
  deriving (Eq, Show, Generic, ToJSON, FromJSON) — `DemoState ≠ DemoEvent`
  (distinct declarations, `e ≠ s`); `data DemoError = DemoNotAdmin Text |
  DemoNegative Int | DemoHookRefused Text` deriving (Eq, Show);
  `data DemoProposal = DemoRemove Text | DemoChangeRoles Text (Set Role)`
  deriving (Eq, Show, Generic, ToJSON, FromJSON);
  `demoProposalMutation :: DemoProposal -> BaseMutation`;
  `demoDigest :: DemoProposal -> ProposalId`;
  `demoReserved :: Text` (fixed reserved key, e.g. `"reserved"`);
  `demoAppFold :: IntegratedAppFold DemoState DemoEvent DemoError`
  (authorizes by signer via view: `DemoReset` requires `isAdminInView signer
  preView` else `DemoNotAdmin`; `DemoAdd n` refuses `n < 0` with
  `DemoNegative`; `DemoNoop` always ok; reads ONLY the two views + state +
  event, writes ONLY `DemoState`);
  `demoBaseHook :: BaseHook DemoState DemoError` (observes exact pre/post
  views + change; refuses `MemberRemoved protectedKey` with
  `DemoHookRefused` where `protectedKey` is a fixed demo member, else applies
  a visible payload consequence e.g. appends to `demoLog`; returns `Except`);
  `demoIntegration :: Integration DemoState DemoEvent DemoProposal DemoError`
  (bundles the above; `intReserved = demoReserved`). JSON instances for
  Store persistence. No executable, no library exposure (test-only).
- `test/S28AppApiSpec.hs` (`module S28AppApiSpec (spec)`): six `describe`
  groups with EXACT strings (gate greps this extent, never a hardcoded
  count): `"S28-1 distinct types + signer + GroupView"`, `"S28-1 rejecting
  step before append"`, `"S28-1 atomic hook"`, `"S28-1 direct-only
  admission"`, `"S28-1 validate/fold agreement"`, `"S28-1 no
  client-decided authority"`. Each group holds the row's reachable positive,
  refusal, and atomicity/replay properties over `demoIntegration` +
  `applyIntegratedEvent`/`foldIntegrated`/`appendIntegratedEvent`/
  `openIntegratedKEL` (QuickCheck where a trace property is natural, with
  standalone generators, no `Arbitrary` instances). Wired into
  `kelgroups.cabal` test-suite `other-modules` + `test/Main.hs` (`S28AppApiSpec.spec`).
  Six-group presence is machine-checked by the gate; a seventh group is
  allowed but never required.
- Generators reuse `test/Generators.hs` patterns (real CESR keys where
  signatures matter); `GroupState` construction there is adapted minimally
  for the new `pendingBase` field (empty unless the test plants approvals).

## 6. Contract rows → witnesses + can-fail mutants (frozen)

| row | positive witness | refusal witness | atomicity / replay witness | can-fail mutant (must go RED) |
|---|---|---|---|---|
| distinct types + signer + GroupView (`test/S28DemoApp.hs`, `test/S28AppApiSpec.hs` group `S28-1 distinct types + signer + GroupView`) | nondegenerate demo compiles with `e ≠ s` (`DemoEvent ≠ DemoState`); member `DemoAdd` via `demoAppFold` authorizes by signer through `isMemberInView`/`isAdminInView` | non-member `IEApp` event refused `IEValidation (NotAMember signer)` BEFORE any fold; never appended | replay of an accepted integrated KEL via `foldIntegrated`/`openIntegratedKEL` reproduces identical `DemoState` | M1 conflate `e=s`: replace `data DemoEvent …` with `type DemoEvent = DemoState` in `test/S28DemoApp.hs` → `cabal build all -O0` RED (compile failure naming `DemoEvent`/`DemoState` mismatch). Build-mutant. |
| rejecting step before append (Store path `lib/KelGroups/Store.hs` `appendIntegratedEvent`, spec group `S28-1 rejecting step before append`) | `appendIntegratedEvent` validates then appends; accepted event durable (`kelLength` +1, `readState` advances, row present) | domain-invalid app event (`demoAppFold` `Left`, e.g. `DemoAdd (-1)` → `DemoNegative`) returns `IEApp` `Err` and is NEVER appended (SQL `COUNT(*)` + file bytes byte-identical, `readState` unchanged) | refused-event log + folded state both unchanged after retry + full replay (`openIntegratedKEL` re-read) | M2 bypass validate in append path: in `appendIntegratedEvent` (or `applyIntegratedEvent` app route) skip the `isMemberInView`/`demoAppFold` check and always `Ok` → refusal properties RED (non-zero test exit naming `S28-1 rejecting step before append`). Test-mutant. |
| atomic hook (`lib/KelGroups/Fold.hs` `commitBaseChange`, spec group `S28-1 atomic hook`) | base change + hook success commits state + `irChange = Some change` evidence (`MemberAdmitted`/`MemberRemoved`/`RolesChanged`) | failing hook (`demoBaseHook` `DemoHookRefused` on `MemberRemoved protectedKey`) rejects the WHOLE transition (`IEApp` `Err`; `irState` discarded) | tentative base change + failing hook via `appendIntegratedEvent` → pre-state (`readState` equal) AND pre-log (`kelLength` + `COUNT(*)` + bytes equal) restored; replay confirms | M3 ignore hook `Err` and keep post-base: in `commitBaseChange` return `Ok` post-state even on hook `Err` → atomicity properties RED (naming `S28-1 atomic hook`). Test-mutant. |
| direct-only admission (`lib/KelGroups/Event.hs` `BaseMutation`, `lib/KelGroups/State.hs` `PendingBase`, spec group `S28-1 direct-only admission`) | direct admit by admin (`IEDirect (AdmitMember …)` + `validateDirectAdmission`) inserts member | votable admission UNREPRESENTABLE (type-level): `BaseMutation` has no admission constructor; attempted encoding (a `AdmitMember` arm) fails to compile; `validateBaseMutation`/`enactMutation` exhaustive | pending store typed non-admitting (`PendingBase.pbMutation :: BaseMutation`); historical `pendingProposals`/`IntroduceMember` kept with `HISTORICAL-NON-PRODUCTION` comments and ZERO production responsibility on the new path | M4 re-add admission constructor to voted mutation: add `AdmitMember Text Text (Set Role)` to `BaseMutation` with inserting `enactMutation` arm → direct-only properties RED (voted admission now inserts; `S28-1 direct-only admission` fails). Test-mutant (build still succeeds; behavior regresses). |
| validate/fold agreement (single step `applyIntegratedEvent`, spec group `S28-1 validate/fold agreement`) | accepted event folds to same state via direct `applyIntegratedEvent` and via `foldIntegrated` single-element trace | — (no separate refusal row) | property: accept→replay never disagrees — QuickCheck over generated integrated traces: for every accepted prefix, `foldIntegrated` state equals iterative `applyIntegratedEvent` states; replay of an accepted KEL never rejects | M5 fork the step copies: duplicate the app-route check into divergent validate vs fold copies (or make `foldIntegrated` skip validation and call raw effect) → agreement property RED (naming `S28-1 validate/fold agreement`). Test-mutant. |
| no client-decided authority (integrated boundary sole decider, spec group `S28-1 no client-decided authority`) | demo verdicts (counter/log) come ONLY from `applyIntegratedEvent`/`foldIntegrated`/`appendIntegratedEvent` | out-of-band app-state write refused/ignored: historical raw `appendEvent (App …)` does NOT move the integrated `DemoState` observed through the integrated boundary; direct record update is not a decision path | — | M6 second decision path: add exported `unsafeSetAppState :: GroupState DemoState -> DemoState -> GroupState DemoState` (or an alternate `altAppFold` wired into a new `altApply`) and route one test verdict through it → authority properties RED (a verdict not from the integrated boundary is observed; `S28-1 no client-decided authority` fails). Test-mutant. |

`Trivial` is explicitly excluded from every positive column above.

## 7. Frozen gate G28-1 (immutable; hash-bound header; `gate.sh` untracked+ignored)

`gate.sh` lives at `/code/kelgroups-issue-28/gate.sh` (untracked, ignored via
`/gate.sh` in `.gitignore`, executable), backed up as
`handoffs/gate.sh.backup` under this runtime root. It runs IN ORDER on the
exact candidate head with `set +e` capture (every leg prints its command,
exit, and diagnosis; final exit non-zero if any leg failed):

1. `git status --porcelain` filtered to tracked paths (`grep -v '^??'`) empty
   BEFORE and AFTER (ignored `gate.sh` + build trees never count; any tracked
   drift FAILs).
2. `git rev-parse HEAD` recorded; normalized header hash check:
   `GATE_SHA256="<frozen>"` in `gate.sh` equals `sed
   's/^GATE_SHA256=".*"/GATE_SHA256=""/' gate.sh | sha256sum` (blank-normalized
   to avoid self-reference circularity; documented in the gate header).
3. `nix develop .#ci --quiet -c just build` (== `cabal build all -O0` in the
   CI shell) — exit 0. Cold build is the whole cost; later legs reuse the warm
   tree.
4. `nix develop .#ci --quiet -c cabal test all -O0 --test-show-details=direct`
   (== `just test` in the CI shell) — exit 0, with all six `S28-1 …` groups
   present and passing: gate discovers the extent (`find test -name '*.hs'`),
   requires ≥6 distinct `S28-1 ` describes (never a hardcoded count), requires
   each string in the direct test output with 0 failures.
5. Mutants M1–M6 applied ONE at a time → the corresponding property goes RED
   (M1: `cabal build all -O0` non-zero naming `DemoEvent`/`DemoState`; M2–M6:
   `cabal test` non-zero with the row's `S28-1 …` slug + failure in output);
   each mutant reverted via `git checkout -- <files>` before the next; mutant
   diff sha256 + failing output excerpt printed. Warm-tree focused runs count
   as probes, not separate full builds.
6. `nix develop .#ci --quiet -c just ci` FULL (the ACTUAL CI command from
   `.github/workflows/ci.yml`: fourmolu in-place ≡ format-check when leg 1
   stays clean, cabal-fmt in-place ≡ cabal-fmt check when leg 1 stays clean,
   hlint, build, test, `lake build`, client build, client test) — exit 0 on
   the exact head.
7. `Trivial` degenerate presence: `lib/KelGroups/Trivial.hs` still exports
   `trivialConfig`/`trivialFold`/`trivialInitial`, builds (covered by leg 3),
   existing suites pass (covered by leg 4 exit 0), and `Trivial.hs` contains
   NO `S28-1 ` string (explicitly NOT counted as a nondegenerate witness).

RED bundle (commit owner, BEFORE green): this exact frozen gate on the
pre-change base MUST fail for absence (leg 4 finds <6 groups → RED for the
intended reason, not typos), with the frozen gate hash recorded. GREEN: same
gate exit 0 on the candidate. No push without epic authorization; draft PR
only after GREEN + fresh FULL audit.

## 8. S30 dependency surface (planned, NOT built)

S28-1 builds NO vote machine. It plans S30's dependency surface only: the
`Integration` bundle already separates `BaseProposal` as a distinct param with
`proposalMutation :: bp -> BaseMutation` + `digest`, so S30 can instantiate
`bp` with an app-scoped question payload WITHOUT touching the substrate
boundary; `pendingBase`/`PendingBase`/`BaseMutation` non-admitting shape is
the exact surface S30's `Question`/`Ballot`/`Verdict`/`Threshold`-param/
`ClosureRecord`/`foldVote`/`sweepClosures` will compose against; `GroupView`
sole projection + `franchise` derivation point is where S30 reads the
franchise; `BaseHook` atomicity is where S30's post-enactment consequences
will ride. Upstream Lean gaps for #30 are enumerated in the S30 contract after
S28 establishes the actual interface — never invented here. Sibling #30 is NOT
yours; no #30 code in this slice.

## 9. Residual risks + rebind plan

- `GroupState` gains a field: existing `Generators.hs`/specs constructing it
  positionally need minimal adaptation (allowed test wiring; no semantic
  widening). `Server.hs`/`Bootstrap.hs` only if the compiler forces it.
- PureScript `build-client`/`test-client` do not consume Haskell types
  directly, but JSON shapes for the new integrated events could affect them;
  if `just ci` breaks there, adapt minimally (no `Fold.purs` redesign).
- CESR/key handling (`requireValidCesrKey`, `reservedKey`) follows Lean guard
  order exactly; any deviation is a finding.
- V-2 rebind: after #68 lands, the approve path (`validateBaseApproval` +
  `tryEnactBase` majority + proposer-selfbar) is explicitly rebound +
  revalidated; this slice freezes against the current accepted base.
- No hardcoded discovery quotas; no `admit`-style escape hatches; replay of
  an accepted KEL never rejects (tested, not asserted).

## 10. Supervision + reporting (binding)

Commit owner + each fresh auditor in distinct panes of `reactivegas:8` via
`tmux split-window -d`, `send-pointer` delivery, post-cursor `START`
acknowledgement (pane + family + alternate check); pane presence is not
dispatch. Ticket owner supervises the immediate child only and verifies every
material claim against artifacts (read files, not events; low-discipline
briefs specify mechanism). Upward reporting LOCAL FILES ONLY (`handoffs/` +
`/tmp/reactivegas/ms2/inbox/` pointer + STATUS journal); NEVER type into desk
`%510` or any human chat composer. Inbox checked before every phase, before
every expensive command, before freezing evidence, before `COMPLETE`. Known-
stale alarm: `worker-protocol/scripts/monitor-workers` foreground wait with
`TMUX_NOTIFY_TARGET` = ticket pane; demonstrated receipt = a `STALE`/`NEVER-
STARTED` line naming the child root in the monitor output reaching this pane.
Stop conditions per brief §Questions/inbox/escalation are terminal or
`PARKED: <reason>; wake=<condition>` or `BLOCKED Q-…`.
