import KelGroups.Validate

/-!
# The integrated transition boundary

The sole production transition of an application built on this substrate, and
the typed contracts it is parameterised by.

Three shapes carry the whole of `INV-62-PAYLOAD-ONLY`, and they are types rather
than guards:

* `IntegratedAppFold` returns `Except AppError AppState`.  An app fold *cannot*
  return a group aggregate or a member list, because there is nowhere in its
  result type to put one.  It reads membership through the canonical
  `GroupView`s it is handed and writes nothing but app payload.
* `BaseHook` is the same discipline for the consequences of a committed base
  change: it observes the exact pre/post views and returns app payload or a
  rejection.
* `IntegratedResult` pairs the new aggregate with *optional concrete base-change
  evidence*, so a consumer reads what actually happened to membership rather
  than inferring it from a route label.

This module is generic and policy-free.  It knows no economic account, no
question kind, and no cleanup rule; it invokes the contracts an application
supplies.

## One insertion path, and one pending store that cannot express insertion

Membership grows in exactly one place: `enactAdmission`, reachable only from
the `direct` route behind `validateDirectAdmission`.  The voted routes enact
`BaseMutation`, which has no admission constructor, and they read the
`pendingBase` store, which is typed by it.  So an aggregate handed to this
boundary — however it was constructed — cannot carry a pending approval that
admits anybody.  The exclusion is structural, not a guard that could be
reordered away.

`applyIntegratedEvent` is the Reactivegas production root.  It does not call
`applyEventDetailed`: the historical generic fold keeps its accepted theorem,
its own `pendingProposals` store and its `Proposal.introduceMember`
constructor, and receives no production responsibility.

## Atomicity

A committed base change and its consequences are one transition.  Every
successful membership or role effect goes through `commitBaseChange`, which
runs the sealed hook against the exact pre/post views before any result is
returned; if the hook rejects, the whole transition rejects and the new
aggregate is discarded.  There is no separately signable cleanup or sweep
event, and no order in which the group can move without its consequences.
-/

namespace KelGroups

/-! ## Typed contracts (R62-04) -/

/-- The application transition.  It receives the signer, the canonical pre- and
post-transition views, the app payload and the app event, and returns app
payload or a rejection — never a group or a member list. -/
abbrev IntegratedAppFold (AppState AppEvent AppError : Type) :=
  (signer : Key) → (preGroup postGroup : GroupView) →
  (state : AppState) → (event : AppEvent) → Except AppError AppState

/-- The sealed post-base hook.  It observes one committed base change through
its exact pre/post views and returns the corresponding app payload or a
rejection. -/
abbrev BaseHook (AppState AppError : Type) :=
  (change : BaseChange) → (preGroup postGroup : GroupView) →
  (state : AppState) → Except AppError AppState

/-- The closed integrated event vocabulary.  Base proposal and app event are
*distinct* type parameters, so an app event cannot be a proposal and no
membership action can arrive dressed as app payload. -/
inductive IntegratedEvent (BaseProposal AppEvent : Type) where
  | direct (command : DirectCommand)
  | propose (proposal : BaseProposal)
  | approve (proposalId : ProposalId)
  | app (event : AppEvent)
deriving DecidableEq, BEq, Repr

/-- Rejection identities of the integrated boundary: a substrate admissibility
refusal, or the application's own — the latter covering both a refused app
event and a refused post-base hook. -/
inductive IntegratedError (AppError : Type) where
  | validation (error : ValidationError)
  | app (error : AppError)
deriving DecidableEq, BEq, Repr

/-- Success: the new aggregate, and the concrete base change it committed if it
committed one.  The aggregate carries exactly one members relation. -/
structure IntegratedResult (AppState : Type) where
  state : GroupState AppState
  change : Option BaseChange
deriving DecidableEq, BEq, Repr

/-- The contract bundle an application supplies to the boundary.

It exposes no function from an unrestricted generic `Proposal` into
`BaseProposal`, and none out of it either: `proposalMutation` lands in
`BaseMutation`, which cannot admit.  There is therefore no route by which a
voted admission could be translated into this surface. -/
structure Integration (AppState AppEvent BaseProposal AppError : Type) where
  /-- The one key that may never become a member. -/
  reserved : Key
  /-- Identity of a proposal in the pending store. -/
  digest : BaseProposal → ProposalId
  /-- The application's exhaustive reading of its own restricted proposal as a
  substrate base mutation. -/
  proposalMutation : BaseProposal → BaseMutation
  appFold : IntegratedAppFold AppState AppEvent AppError
  baseHook : BaseHook AppState AppError

/-! ## Base effects -/

variable {AppState AppEvent BaseProposal AppError : Type}

/-- The sole member insertion (R62-06).  Nothing else in this module writes a
new key into the members relation. -/
def admitMemberInto (gs : GroupState AppState) (key : Key) (email : Email)
    (roles : List Role) : GroupState AppState :=
  { gs with members := assocInsert key { key, email, roles } gs.members }

/-- The voted base effects.  Exhaustive over `BaseMutation`; neither arm can
introduce a key, which is `enactMutation_preserves_absence`. -/
def enactMutation (gs : GroupState AppState) :
    BaseMutation → GroupState AppState
  | .removeMember key => { gs with members := assocErase key gs.members }
  | .changeRoles key roles =>
      { gs with members := assocAdjust key (fun member => { member with roles }) gs.members }

/-- The observable evidence a voted mutation commits.  Kept separate from the
effect so a route cannot report one change while performing another. -/
def mutationChange : BaseMutation → BaseChange
  | .removeMember key => .memberRemoved key
  | .changeRoles key _ => .rolesChanged key

/-- Commit a base change together with its consequences, or reject both.  The
hook sees the exact pre and post canonical views and the pre-transition
payload; its output *is* the payload the caller observes. -/
def commitBaseChange (integration : Integration AppState AppEvent BaseProposal AppError)
    (pre post : GroupState AppState) (change : BaseChange) :
    Except (IntegratedError AppError) (IntegratedResult AppState) :=
  match integration.baseHook change (groupView pre) (groupView post) pre.appFold with
  | .ok appState => .ok { state := { post with appFold := appState }, change := some change }
  | .error err => .error (.app err)

/-- Enact a pending base mutation once its approvals reach the majority of the
current franchise; otherwise leave it pending and report no change. -/
def tryEnactBase (integration : Integration AppState AppEvent BaseProposal AppError)
    (gs : GroupState AppState) (proposalId : ProposalId) :
    Except (IntegratedError AppError) (IntegratedResult AppState) :=
  match lookupPendingBase proposalId gs with
  | none => .ok { state := gs, change := none }
  | some pending =>
      if pending.approvals.length ≥ majority gs then
        commitBaseChange integration gs
          (enactMutation { gs with pendingBase := assocErase proposalId gs.pendingBase }
            pending.mutation)
          (mutationChange pending.mutation)
      else .ok { state := gs, change := none }

/-! ## The sole production transition -/

/-- The one integrated transition.

Validation dominates the effect on every route: an app event from a non-member
reaches no fold, and no base route reaches an effect without its admissibility
decision.  A successful app event replaces the app payload and nothing else —
the members relation, both pending stores and the base-change evidence are all
untouched, which is `app_event_preserves_members` and
`app_event_has_no_base_change`.  Every successful base effect goes through
`commitBaseChange`, which is `base_change_runs_hook`. -/
def applyIntegratedEvent
    (integration : Integration AppState AppEvent BaseProposal AppError)
    (gs : GroupState AppState) (signer : Key)
    (event : IntegratedEvent BaseProposal AppEvent) :
    Except (IntegratedError AppError) (IntegratedResult AppState) :=
  match event with
  | .direct (.admitMember key email roles) =>
      match validateDirectAdmission integration.reserved gs signer key email roles with
      | .error err => .error (.validation err)
      | .ok () =>
          commitBaseChange integration gs (admitMemberInto gs key email roles)
            (.memberAdmitted key)
  | .propose proposal =>
      let mutation := integration.proposalMutation proposal
      match validateBaseMutation gs signer mutation with
      | .error err => .error (.validation err)
      | .ok () =>
          let proposalId := integration.digest proposal
          let pending : PendingBase :=
            { mutation, proposer := signer, approvals := [] }
          tryEnactBase integration
            { gs with pendingBase := assocInsert proposalId pending gs.pendingBase }
            proposalId
  | .approve proposalId =>
      match validateBaseApproval gs signer proposalId with
      | .error err => .error (.validation err)
      | .ok () =>
          match lookupPendingBase proposalId gs with
          | none => .error (.validation (.proposalNotFound proposalId))
          | some pending =>
              let approved := { pending with approvals := setInsert signer pending.approvals }
              tryEnactBase integration
                { gs with pendingBase := assocInsert proposalId approved gs.pendingBase }
                proposalId
  | .app appEvent =>
      let view := groupView gs
      if GroupView.isMember signer view then
        match integration.appFold signer view view gs.appFold appEvent with
        | .ok appState => .ok { state := { gs with appFold := appState }, change := none }
        | .error err => .error (.app err)
      else .error (.validation (.notAMember signer))

/-- The integrated fold: every signed integrated event, in order, from a
starting aggregate.  A rejected event leaves the aggregate exactly as it was, so
a refusal cannot advance state. -/
def foldIntegrated
    (integration : Integration AppState AppEvent BaseProposal AppError)
    (initial : GroupState AppState)
    (events : List (Key × IntegratedEvent BaseProposal AppEvent)) :
    GroupState AppState :=
  events.foldl
    (fun gs signed =>
      match applyIntegratedEvent integration gs signed.1 signed.2 with
      | .ok result => result.state
      | .error _ => gs)
    initial

end KelGroups
