import Reactivegas.State
import KelGroups.Integration
import KelGroups.Vote.Types
import KelGroups.Vote.Fold

/-!
# The rejecting step function

One total function over the integrated `AppEvent` surface; `none` means
the event is rejected. The signer and the canonical `GroupView` are
explicit: AUTH is `GroupView.isAdmin signer view`, and member-scoped
guards read `GroupView.isMember`. This payload cannot write membership.

The retired legacy membership/role constructors are gone from `Event`
entirely (T6222); nothing here refuses them because nothing can express
them.

`baseHook` is this module's other half: the sealed consequences of a
committed base membership or role change, derived from the real pre/post
canonical views and run inside the same transition (T6223).
-/

/-- Rejection guard: `demand b` succeeds exactly when `b` holds. -/
def demand (b : Bool) : Option Unit := if b then some () else none

/-- Look up an open collection by id. -/
def findCollection (s : State) (c : CollId) : Option Collection :=
  s.collections.find? (fun x => x.id == c)

/-- Is `u` a current responsabile in the canonical view? -/
def isResponsabile (view : KelGroups.GroupView) (u : KelGroups.Key) : Bool :=
  KelGroups.GroupView.isAdmin u view

/-!
### Provisional boundary: the backdonation vote authorization

The vote encoding for the voted equal-share backdonation is an
app-scoped proposal type **owned by issue #47** (open question Q-007).
No faithful encoding exists in this model yet, so this named boundary
is deliberately provisional: its body is proof debt for the next pass,
it selects **no** true/false vote policy, and the `backdonate` step
case routes its enacted-vote condition through this name.
-/
def backdonateAuthorized (s : State) (w : Int) : Bool := sorry

/-- Member keys of the canonical view, in store order. -/
def memberKeys (view : KelGroups.GroupView) : List KelGroups.Key :=
  view.members.map Prod.fst

/-- Caller-supplied backdonation authorization. The #47 true/false policy
is not chosen here; production `appFold` takes this as an explicit
argument so the fold itself does not depend on `sorryAx`. -/
abbrev BackdonateAuth := State → Int → Bool

/-- The rejecting transition of the integrated economic machine. -/
def step (view : KelGroups.GroupView) (s : State) (signer : KelGroups.Key)
    (e : AppEvent) (auth : BackdonateAuth) : Option State :=
  match e with
  | .openPurchase c =>
    if isResponsabile view signer
        && !(s.collections.any (fun x => x.id == c)) then
      some { s with collections := ⟨c, signer, false, [], []⟩ :: s.collections }
    else none
  | .grantPermission c => do
    let (col, rest) ← pullCollection c s.collections
    demand (isResponsabile view signer)
    pure { s with collections := { col with permitted := true } :: rest }
  | .denyPermission c => do
    let (col, rest) ← pullCollection c s.collections
    demand (isResponsabile view signer)
    pure { s with
      conti := refundAll s.conti (col.accepted ++ col.pending),
      collections := rest }
  | .deposit u v =>
    if isResponsabile view signer && KelGroups.GroupView.isMember u view
        && signer != u && decide (0 ≤ v) then
      some { s with conti := bump s.conti u v, casse := bump s.casse signer v }
    else none
  | .withdraw u v =>
    if isResponsabile view signer && KelGroups.GroupView.isMember u view
        && signer != u && decide (bal s.conti u ≥ v)
        && !(decide (stalled s)) then
      some { s with
        conti := bump s.conti u (-v), casse := bump s.casse signer (-v) }
    else none
  | .transferCassa f v =>
    if isResponsabile view signer && isResponsabile view f
        && signer != f && v > 0 then
      some { s with casse := bump (bump s.casse f (-v)) signer v }
    else none
  | .donate v =>
    if isResponsabile view signer && decide (0 < v) then
      some { s with
        casse := bump s.casse signer v,
        conti := bump s.conti comuneId v }
    else none
  | .backdonate w =>
    let members := memberKeys view
    let n : Int := members.length
    if isResponsabile view signer && decide (0 < w)
        && decide (comuneBal s ≥ n * w) && auth s w then
      some { s with
        conti := members.foldl (fun acc u => bump acc u w)
          (bump s.conti comuneId (-(n * w))) }
    else none
  | .pledge u c v => do
    let (col, rest) ← pullCollection c s.collections
    demand (isResponsabile view signer && KelGroups.GroupView.isMember u view
      && !(col.accepted.any (fun p => p.user == u))
      && !(col.pending.any (fun p => p.user == u))
      && decide (0 < v) && decide (bal s.conti u ≥ v)
      && !(decide (stalled s)))
    pure { s with
      conti := bump s.conti u (-v),
      collections := { col with pending := ⟨u, v⟩ :: col.pending } :: rest }
  | .acceptPledge u c => do
    let (col, rest) ← pullCollection c s.collections
    let (v, pend') ← splitUser u col.pending
    demand (isResponsabile view signer && col.referente == signer
      && !(decide (stalled s)))
    pure { s with collections :=
      { col with pending := pend', accepted := ⟨u, v⟩ :: col.accepted } :: rest }
  | .refusePledge u c => do
    let (col, rest) ← pullCollection c s.collections
    let (v, pend') ← splitUser u col.pending
    demand (isResponsabile view signer && col.referente == signer)
    pure { s with
      conti := bump s.conti u v,
      collections := { col with pending := pend' } :: rest }
  | .correctPledge u c v' => do
    let (col, rest) ← pullCollection c s.collections
    let (v, acc') ← splitUser u col.accepted
    demand (isResponsabile view signer && col.referente == signer
      && decide (0 ≤ v') && decide (bal s.conti u + (v - v') ≥ 0))
    pure { s with
      conti := bump s.conti u (v - v'),
      collections := { col with accepted := ⟨u, v'⟩ :: acc' } :: rest }
  | .closePurchase c => do
    let (col, rest) ← pullCollection c s.collections
    demand (isResponsabile view signer && col.referente == signer
      && col.permitted && col.pending.isEmpty && !(decide (stalled s)))
    pure { s with
      casse := bump s.casse col.referente (-(sumPledges col.accepted)),
      collections := rest }
  | .failPurchase c => do
    let (col, rest) ← pullCollection c s.collections
    demand (isResponsabile view signer && col.referente == signer
      && col.pending.isEmpty)
    pure { s with
      conti := refundAll s.conti (col.accepted ++ col.pending),
      collections := rest }

/-- Event-shaped wrapper used by inherited #45/#48 theorems. The fourteen
surviving economic constructors
delegate to the integrated `step`; there are no others left to refuse. This
is not a production root. -/
abbrev stepEvent (view : KelGroups.GroupView) (s : State) (e : Event) :
    Option State :=
  let go (signer : KelGroups.Key) (app : AppEvent) : Option State :=
    step view s signer app backdonateAuthorized
  match e with
  | .openPurchase a c => go a (.openPurchase c)
  | .grantPermission a c => go a (.grantPermission c)
  | .denyPermission a c => go a (.denyPermission c)
  | .deposit a u v => go a (.deposit u v)
  | .withdraw a u v => go a (.withdraw u v)
  | .transferCassa a f v => go a (.transferCassa f v)
  | .donate a v => go a (.donate v)
  | .backdonate a w => go a (.backdonate w)
  | .pledge a u c v => go a (.pledge u c v)
  | .acceptPledge a u c => go a (.acceptPledge u c)
  | .refusePledge a u c => go a (.refusePledge u c)
  | .correctPledge a u c v' => go a (.correctPledge u c v')
  | .closePurchase a c => go a (.closePurchase c)
  | .failPurchase a c => go a (.failPurchase c)

namespace Reactivegas

/-- The integrated app fold: payload or rejection, never a group.
Backdonation authorization is supplied by the caller; this definition
does not mention `backdonateAuthorized` and does not depend on
`sorryAx`. -/
def appFold (auth : BackdonateAuth) :
    KelGroups.IntegratedAppFold State AppEvent StepError :=
  fun signer pre _post s e =>
    match step pre s signer e auth with
    | some s' => .ok s'
    | none => .error StepError.rejected

/-! ## The sealed base hook (T6223, R62-09, R62-10)

The economic and vote consequences of one committed base membership or role
change. It runs *inside* `applyIntegratedEvent`, from the exact pre/post
canonical views, so there is no separately signable cleanup or sweep event and
no window in which the group has moved and its consequences have not.
-/

/-- Absorb a departing member's own claim into the reserved comune account.
Legacy `removeMember`: the leaver's conto moves to the comune with no balance
gate — a zero balance is a no-op movement, not a separate form. -/
def absorbConto (s : State) (key : KelGroups.Key) : State :=
  { s with
    conti := bump (bump s.conti key (-(bal s.conti key))) comuneId (bal s.conti key) }

/-- Wind up a key that has just lost admin status. Legacy
`removeResponsabile`: their open collections are cancelled, every pledge those
collections held is refunded to its pledger, and their cassa claim moves to the
comune. This is the accepted #45/#48 cleanup, now derived from a real base
transition instead of a separately signed event. -/
def windUpAdmin (s : State) (key : KelGroups.Key) : State :=
  let (rest, ps) := stripCollections key s.collections
  { s with
    conti := bump (refundAll s.conti ps) comuneId (-(bal s.casse key)),
    casse := bump s.casse key (-(bal s.casse key)),
    collections := rest }

/-- The economic consequences of one committed base change. Exhaustive over
`KelGroups.BaseChange`: a fourth substrate membership effect would stop this
compiling rather than acquire a silent default.

`none` is a refusal, and because the hook is inside the transition it rejects
the base change with it. A stalled comune refuses departures and admin loss
until a donation cures it, exactly as the legacy guard did.

An admitted member has no economic consequence: they arrive with no conto, no
cassa and no collection. A departure is both a member departure and, when the
leaver held admin, an admin wind-up — the two accepted legacy events were
role-disjoint, and unifying them here is what stops a departing responsabile
escaping either half. -/
def economicCleanup (change : KelGroups.BaseChange)
    (pre post : KelGroups.GroupView) (s : State) : Option State :=
  match change with
  | .memberAdmitted _ => some s
  | .memberRemoved key => do
      demand (!(decide (stalled s)))
      pure (absorbConto
        (if KelGroups.GroupView.isAdmin key pre then windUpAdmin s key else s) key)
  | .rolesChanged key =>
      if KelGroups.GroupView.isAdmin key pre
          && !(KelGroups.GroupView.isAdmin key post) then do
        demand (!(decide (stalled s)))
        pure (windUpAdmin s key)
      else some s

/-- **The sealed post-base hook.** Economic cleanup first, then the vote
recomputation every base change owes the question set: all open questions are
re-evaluated against the *post*-transition franchise, so a question can close
because the electorate changed and no ballot was cast (V-3, R62-11).

The recomputation reads `s.votes`, the pre-transition payload, because
`economicCleanup` never touches the vote payload — writing it this way makes
that independence visible rather than incidental. Sweeping twice at one view
cannot duplicate a closure: `KelGroups.Vote.sweepClosures_idempotent`. -/
def baseHook (θ : KelGroups.Vote.Threshold) : KelGroups.BaseHook State StepError :=
  fun change pre post s =>
    match economicCleanup change pre post s with
    | none => .error StepError.rejected
    | some cleaned =>
        .ok { cleaned with votes := KelGroups.Vote.sweepClosures θ post s.votes }

/-! ## The restricted Reactivegas base proposal (T6221) -/

/-- Identity of a proposal in the pending base store. Exhaustive over
`Proposal`. -/
def proposalDigest : Proposal → KelGroups.ProposalId
  | .departure key => "depart:" ++ key
  | .changeRoles key _ => "roles:" ++ key

/-- The application's reading of its own restricted proposal as a substrate
base mutation. Exhaustive and wildcard-free, and its codomain
`KelGroups.BaseMutation` has no admission constructor — so a seeded
`Proposal.introduceMember` has nowhere to go and stops this compiling. -/
def proposalMutation : Proposal → KelGroups.BaseMutation
  | .departure key => .removeMember key
  | .changeRoles key roles => .changeRoles key roles

/-- The Reactivegas integration bundle: the sole production instantiation of
the substrate boundary. It supplies the reserved key, the restricted proposal's
identity and mutation reading, the app fold, and the sealed base hook together
— an application cannot obtain the boundary while omitting one of them.

`BaseProposal` is `Proposal`, the admission-free sum; the threshold reaches the
boundary only through `baseHook`, since app events do not consult it. -/
def integration (θ : KelGroups.Vote.Threshold) (auth : BackdonateAuth) :
    KelGroups.Integration State AppEvent Proposal StepError where
  reserved := comuneId
  digest := proposalDigest
  proposalMutation := proposalMutation
  appFold := appFold auth
  baseHook := baseHook θ

/-- Production well-formedness: the reserved comune account is not a
canonical member (and therefore cannot be an admin, signer, voter, or
proposer). Direct admission refuses it by its own identity in
`validateDirectAdmission`; this is the complementary boot/input
boundary of the S62-A production root. -/
def productionWellFormed (gs : KelGroups.GroupState State) : Bool :=
  !KelGroups.GroupView.isMember comuneId (KelGroups.groupView gs)

/-- Guarded founding aggregate. `none` when `comuneId` appears in the
supplied member list. -/
def boot (members : List (KelGroups.Key × KelGroups.Member))
    (payload : State) : Option (KelGroups.GroupState State) :=
  let gs : KelGroups.GroupState State :=
    { members, pendingProposals := [], pendingBase := [], appFold := payload }
  if productionWellFormed gs then some gs else none

inductive ProductionError where
  | comuneReserved
  | integrated (error : KelGroups.IntegratedError StepError)
deriving DecidableEq, BEq, Repr

/-- The sole Reactivegas production root. An arbitrary `GroupState State`
that already lists `comuneId` as a member is refused before the
generic integrated fold runs, so that reserved key cannot become
authorized by being smuggled in as the initial aggregate. -/
def apply (θ : KelGroups.Vote.Threshold) (auth : BackdonateAuth)
    (gs : KelGroups.GroupState State) (signer : KelGroups.Key)
    (event : KelGroups.IntegratedEvent Proposal AppEvent) :
    Except ProductionError (KelGroups.IntegratedResult State) :=
  if productionWellFormed gs then
    match KelGroups.applyIntegratedEvent (integration θ auth) gs signer event with
    | .ok result =>
        if productionWellFormed result.state then .ok result
        else .error ProductionError.comuneReserved
    | .error err => .error (ProductionError.integrated err)
  else .error ProductionError.comuneReserved

/-! ## Rooted S62-A production witnesses (lake-built; full CI elaborates them) -/

/-- Probe-only authorization. Not the unruled #47 product policy. -/
def probeAuth : BackdonateAuth := fun _ _ => false

def probeQuestion : KelGroups.Vote.Question :=
  { kind := .collective, proposer := "alice", assents := [], dissents := [] }

def preservationGroup : KelGroups.GroupState State :=
  { members :=
      [ ("alice", { key := "alice", email := "alice@example",
                    roles := [KelGroups.Role.adminRole KelGroups.Admin.publicAdmin] })
      , ("bob", { key := "bob", email := "bob@example", roles := [] }) ]
    pendingProposals := []
    pendingBase := []
    appFold :=
      { State.empty with
        votes := { openQuestions := [("q", probeQuestion)], closed := [] } } }

def preservationDonate :
    Except ProductionError (KelGroups.IntegratedResult State) :=
  apply KelGroups.Vote.legacyThreshold probeAuth preservationGroup "alice"
    (KelGroups.IntegratedEvent.app (AppEvent.donate 30))

/-- Positive production check: members and vote payload stay, economy moves,
no base change. -/
def checkAppMembersPreservation : Bool :=
  match preservationDonate with
  | .ok result =>
      (result.state.members == preservationGroup.members)
        && (result.state.appFold.votes == preservationGroup.appFold.votes)
        && !(result.state.appFold == preservationGroup.appFold)
        && (result.change == none)
  | .error _ => false

/-- Member-writing mutant of the production root: after a successful app
event it corrupts `result.state.members`. This is a mutated transition,
not a comparison fixture. -/
def memberWritingApply :
    Except ProductionError (KelGroups.IntegratedResult State) :=
  match preservationDonate with
  | .ok result =>
      .ok { result with
        state := { result.state with members := result.state.members.tail } }
  | .error err => .error err

/-- The preservation property is false of the member-writing mutant, and
the mutant actually executed (payload moved, members changed). -/
def checkAppMembersPreservationMutant : Bool :=
  match memberWritingApply with
  | .ok result =>
      !(result.state.members == preservationGroup.members)
        && !(result.state.appFold == preservationGroup.appFold)
        && (result.state.appFold.votes == preservationGroup.appFold.votes)
  | .error _ => false

theorem app_members_preservation_holds :
    checkAppMembersPreservation = true := by decide

theorem app_members_preservation_mutant_caught :
    checkAppMembersPreservationMutant = true := by decide

def comuneAdminMember : KelGroups.Member :=
  { key := comuneId, email := "comune@reserved.invalid",
    roles := [KelGroups.Role.adminRole KelGroups.Admin.publicAdmin] }

def comuneBoot : KelGroups.GroupState State :=
  { members := [(comuneId, comuneAdminMember)]
    pendingProposals := []
    pendingBase := []
    appFold := State.empty }

/-- Negative production witness: `comuneId` as founding admin cannot
sign a donate through the production root. -/
def checkComuneCannotAuthorize : Bool :=
  (boot [(comuneId, comuneAdminMember)] State.empty).isNone &&
    (match apply KelGroups.Vote.legacyThreshold probeAuth comuneBoot
        comuneId (KelGroups.IntegratedEvent.app (AppEvent.donate 1)) with
      | .error ProductionError.comuneReserved => true
      | _ => false)

theorem comune_cannot_authorize :
    checkComuneCannotAuthorize = true := by decide

#print axioms appFold
#print axioms apply
#print axioms app_members_preservation_holds
#print axioms comune_cannot_authorize

end Reactivegas
