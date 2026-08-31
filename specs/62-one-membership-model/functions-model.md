# Functions model — #62 one membership and role model

Artifact ceiling: 180 lines / 14 KiB. Only changed/new public signatures and
signature-level constraints/effects are listed.

## `KelGroups.Types` / `KelGroups.State`

- `GroupView` — read-only canonical member/role projection.
- `groupView (state : GroupState AppState) : GroupView` — returns the canonical
  projection and no app payload.
- `lookupMember (key : Key) (view : GroupView) : Option Member`.
- `isMember (key : Key) (view : GroupView) : Bool`.
- `isAdmin (key : Key) (view : GroupView) : Bool`.

Existing state-level lookup names may retain overloads only if the production
integrated signatures above remain unambiguous and no member list is copied.

## `KelGroups.Event` / `KelGroups.Integration`

- `DirectCommand` — closed sum containing only direct member admission.
- `BaseChange` — closed sum for admitted member, removed member, and changed
  roles; each value identifies the affected key.
- `IntegratedEvent (BaseProposal AppEvent : Type)` — closed sum containing a
  direct command, proposal, approval, and app event with distinct proposal/app
  parameters.
- `IntegratedAppFold (AppState AppEvent AppError : Type) :=
  (signer : Key) → (preGroup postGroup : GroupView) →
  (state : AppState) → (event : AppEvent) → Except AppError AppState`.
- `BaseHook (AppState AppError : Type) :=
  (change : BaseChange) → (preGroup postGroup : GroupView) →
  (state : AppState) → Except AppError AppState`.
- `Integration (AppState AppEvent BaseProposal AppError : Type)` — supplies
  the typed app fold, sealed base hook, proposal semantics, and validation
  policy required by the integrated boundary; it exposes no function from an
  unrestricted generic proposal into `BaseProposal`.
- `IntegratedResult (AppState : Type)` — successful aggregate plus optional
  concrete base-change evidence; the aggregate has one canonical members
  relation.
- `applyIntegratedEvent
  (integration : Integration AppState AppEvent BaseProposal AppError)
  (state : GroupState AppState) (signer : Key)
  (event : IntegratedEvent BaseProposal AppEvent) :
  Except (IntegratedError AppError) (IntegratedResult AppState)` — sole
  Reactivegas production transition. Validation dominates effects; app events
  preserve members; every successful base member/role change invokes the
  sealed hook with its exact pre/post views before success is returned.
- `foldIntegrated` — folds signed integrated events through
  `applyIntegratedEvent` from a guarded boot aggregate.

The existing `AppFold α`, `GroupEvent α`, and `applyEventDetailed` signatures
remain historical and unchanged where required by the frozen theorem. They
are not aliases or wrappers for the production integrated types.

## `KelGroups.Validate`

- `validateDirectAdmission (reserved : Key) (state : GroupState AppState)
  (signer target : Key) (email : Email) (roles : List Role) :
  Except ValidationError Unit` — requires a current admin, valid absent target,
  and `target ≠ reserved`; no bootstrap exception.
- integrated proposal, approval, and app authorization functions take the
  current canonical view and remain exhaustive over their respective closed
  sums.

## `KelGroups.Vote.State` / `Validate` / `Fold`

- `franchise (view : GroupView) : List Key`.
- `franchiseSize (view : GroupView) : Nat`.
- `isResponsabile (key : Key) (view : GroupView) : Bool`.
- `verdictOf (threshold : Threshold) (view : GroupView)
  (state : VoteState) (question : Question) : Verdict` — the sole verdict
  site; the threshold remains explicit.
- `validateVoteEvent (threshold : Threshold) (view : GroupView)
  (state : VoteState) (signer : Key) (event : VoteEvent) :
  Except VoteError Unit` — exhaustive over question-only events; no bootstrap
  member event.
- `applyVoteEvent (threshold : Threshold) (view : GroupView)
  (state : VoteState) (signer : Key) (event : VoteEvent) : VoteState` — exact
  vote-payload identity on rejection.
- `sweepClosures (threshold : Threshold) (view : GroupView)
  (state : VoteState) : VoteState` — recomputes all open questions against the
  supplied canonical view.
- `foldVote` / `foldFrom` — retain question-event folding roles and require an
  explicit canonical view; they are not the membership-change reachability
  surface.

## `Reactivegas.Types` / `State` / `Step`

- all participant/signer/account/proposer/voter arguments are
  `KelGroups.Key`; `Reactivegas.UserId` is removed.
- `Reactivegas.Proposal` — closed admission-free member-removal/role-change
  base proposal sum.
- `Reactivegas.Event` — closed app-only economic/vote action sum with no
  signer/author field and no membership/role mutation.
- `Reactivegas.appFold : IntegratedAppFold State Event StepError` — reads
  signer and canonical views and returns app payload/rejection only.
- `Reactivegas.baseHook (threshold : Threshold) : BaseHook State StepError` —
  owns economic departure cleanup and post-view vote recomputation for every
  base change.
- `Reactivegas.integration (threshold : Threshold) :
  Integration State Event Proposal StepError` — the sole production
  instantiation; it exposes the restricted proposal and sealed hook together.
- `step (threshold : Threshold) (group : GroupView) (state : State)
  (signer : Key) (event : Event) : Except StepError State` — app-payload
  transition only; it cannot change the group view.
- member-scoped predicates and theorem signatures take `GroupView` explicitly,
  including `solvent`, `insolvent`, `canCloseGroup`, authorization inversions,
  backdonation distribution/cardinality, and reachability invariants.

## Integrated theorems

- `app_event_preserves_members` — a successful integrated app event preserves
  the aggregate members relation.
- `direct_admission_requires_admin` — successful direct admission implies the
  signer was a current pre-state admin, target was absent, and target differs
  from `comuneId`.
- `non_admin_admission_is_noop` — rejected non-admin direct admission leaves
  the complete aggregate unchanged.
- `base_change_runs_hook` — successful integrated member admission/removal or
  role change exposes the exact pre/post views and the hook-produced payload.
- `base_change_recomputes_votes` — every successful integrated base change has
  vote payload equal to recomputation under its post view.
- `base_departure_applies_cleanup` — concrete successful member removal and
  admin-role loss imply their respective economic cleanup effects.
- `base_change_can_close_without_ballot` — production-reachable V-3 witness:
  unchanged tallies plus a real base role/member transition changes the
  verdict and writes the closure.
- inherited #57 theorem names may change only where their parameter types must
  include `GroupView`; their statements retain the meanings enumerated in
  `plan.md`.

`Reactivegas.Composition.baseEnacted_threshold_met` is not a changed
signature. It remains byte-identical historical evidence and is excluded from
the implementation function list.

## Trace surface

- integrated trace input is a signed `IntegratedEvent Reactivegas.Proposal
  Reactivegas.Event`; signer is serialized once.
- integrated trace state is `GroupState Reactivegas.State`; member/role and
  app payload are serialized once in their owning components.
- trace replay, inventory, and fidelity functions consume
  `applyIntegratedEvent`; they do not reconstruct parallel economic or vote
  states.
- executed value witnesses are named `checkAppMembersPreservation`,
  `checkBaseCleanupReachable`, `checkBaseRecomputeReachable`,
  `checkV3BaseReachable`, `checkAdminAdmissionReachable`,
  `checkNonAdminAdmissionRefused`, `checkComuneAdmissionRefused`,
  `checkDirectAdmissionOnly`, `checkIntegratedTheoremWitness`,
  `checkCanonicalEconomy`, `checkRoleChangeReachable`,
  `checkAdminDepartureCleanup`, `checkMemberDepartureCleanup`, and
  `checkExhaustiveInventories`; each is executed by the rooted trace control
  surface rather than merely declared.
- inherited value witnesses are named `checkI57Boundary`,
  `checkI57Exhaustive`, `checkI57Noop`, `checkI57Auth`, `checkI57R45`,
  `checkI57Partition`, `checkI57Disjoint`, `checkI57NoStale`,
  `checkI57Franchise`, `checkI57PolicyFree`, `checkI57NoExpiry`,
  `checkI57Trust`, `checkI57Direction`, and `checkI57Toolchain`; their mutation
  receipts remain runtime evidence, not source markers.
