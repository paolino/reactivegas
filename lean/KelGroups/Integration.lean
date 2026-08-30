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

## What this slice roots, and what it does not

`applyIntegratedEvent` is the Reactivegas production root from S62-A onward. It
does not call `applyEventDetailed`: the historical generic fold keeps its
accepted theorem and receives no new production responsibility.

The base routes — direct admission, proposal, approval — are *rejected* here,
not silently accepted and not mapped onto a compatibility app event. Their
production semantics, the sealed hook invocation, and the restricted proposal
type are T6220/T6221/T6223 in S62-B. Rejection is the honest state for a slice
in which membership has exactly one writable store and no admission path yet:
nothing can quietly write members while the route is closed.
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
rejection.  Its invocation site is S62-B (T6223); the contract is fixed here so
the application and the substrate agree on it before either depends on it. -/
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

/-- The three base routes, named so a rejection identity is exhaustive and a
future fourth route cannot arrive unnamed. -/
inductive IntegratedRoute where
  | direct
  | propose
  | approve
deriving DecidableEq, BEq, Repr

/-- Rejection identities of the integrated boundary.  `baseUnavailable` is the
named, temporary S62-A state of the three base routes; it is a refusal, never a
no-op, so no caller can mistake it for a committed transition. -/
inductive IntegratedError (AppError : Type) where
  | validation (error : ValidationError)
  | app (error : AppError)
  | baseUnavailable (route : IntegratedRoute)
deriving DecidableEq, BEq, Repr

/-- Success: the new aggregate, and the concrete base change it committed if it
committed one.  The aggregate carries exactly one members relation. -/
structure IntegratedResult (AppState : Type) where
  state : GroupState AppState
  change : Option BaseChange
deriving DecidableEq, BEq, Repr

/-- The contract bundle an application supplies to the boundary.  It exposes no
function from an unrestricted generic proposal into `BaseProposal`: there is no
route by which a voted admission could be translated into this surface.

`BaseProposal` is a parameter of the bundle rather than a field because S62-A
has no live base route; S62-B adds the proposal semantics, validation policy and
the sealed `BaseHook` alongside the routes that invoke them. -/
structure Integration (AppState AppEvent BaseProposal AppError : Type) where
  appFold : IntegratedAppFold AppState AppEvent AppError

/-! ## The sole production transition -/

variable {AppState AppEvent BaseProposal AppError : Type}

/-- The one integrated transition.

Validation dominates the effect: an app event from a non-member reaches no
fold.  A successful app event replaces the app payload and nothing else — the
members relation, the pending proposals and the base-change evidence are all
untouched, which is `app_event_preserves_members` and
`app_event_has_no_base_change`.  The three base routes reject. -/
def applyIntegratedEvent
    (integration : Integration AppState AppEvent BaseProposal AppError)
    (gs : GroupState AppState) (signer : Key)
    (event : IntegratedEvent BaseProposal AppEvent) :
    Except (IntegratedError AppError) (IntegratedResult AppState) :=
  match event with
  | .direct _ => .error (.baseUnavailable .direct)
  | .propose _ => .error (.baseUnavailable .propose)
  | .approve _ => .error (.baseUnavailable .approve)
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
