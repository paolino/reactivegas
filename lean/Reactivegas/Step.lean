import Reactivegas.State
import KelGroups.Integration
import KelGroups.Vote.Types

/-!
# The rejecting step function

One total function over the integrated `AppEvent` surface; `none` means
the event is rejected. The signer and the canonical `GroupView` are
explicit: AUTH is `GroupView.isAdmin signer view`, and member-scoped
guards read `GroupView.isMember`. This payload cannot write membership.

The four legacy `Event` membership/role constructors are not handled
here; they are not constructors of `AppEvent` and are not routed
through the integrated production path (NOTE-001, NOTE-002).

Departure cleanup and base-transition success remain S62-B.
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

/-- The rejecting transition of the integrated economic machine. -/
def step (view : KelGroups.GroupView) (s : State) (signer : KelGroups.Key)
    (e : AppEvent) : Option State :=
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
        && decide (comuneBal s ≥ n * w) && backdonateAuthorized s w then
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

/-- Event-shaped wrapper used by inherited #45/#48 theorems. The four
membership/role constructors refuse; the fourteen economic constructors
delegate to the integrated `step`. This is not a production root. -/
abbrev stepEvent (view : KelGroups.GroupView) (s : State) (e : Event) :
    Option State :=
  match e with
  | .addUser _ _ => none
  | .electResponsabile _ _ => none
  | .removeResponsabile _ _ => none
  | .removeMember _ _ => none
  | .openPurchase a c => step view s a (.openPurchase c)
  | .grantPermission a c => step view s a (.grantPermission c)
  | .denyPermission a c => step view s a (.denyPermission c)
  | .deposit a u v => step view s a (.deposit u v)
  | .withdraw a u v => step view s a (.withdraw u v)
  | .transferCassa a f v => step view s a (.transferCassa f v)
  | .donate a v => step view s a (.donate v)
  | .backdonate a w => step view s a (.backdonate w)
  | .pledge a u c v => step view s a (.pledge u c v)
  | .acceptPledge a u c => step view s a (.acceptPledge u c)
  | .refusePledge a u c => step view s a (.refusePledge u c)
  | .correctPledge a u c v' => step view s a (.correctPledge u c v')
  | .closePurchase a c => step view s a (.closePurchase c)
  | .failPurchase a c => step view s a (.failPurchase c)

namespace Reactivegas

/-- The integrated app fold: payload or rejection, never a group. -/
def appFold : KelGroups.IntegratedAppFold State AppEvent StepError :=
  fun signer pre _post s e =>
    match step pre s signer e with
    | some s' => .ok s'
    | none => .error StepError.rejected

/-- The Reactivegas integration bundle. The threshold is accepted for
signature compatibility with the vote machine; S62-A app events do not
consult it. `BaseProposal` is `Unit`: no live base route in this slice. -/
def integration (_θ : KelGroups.Vote.Threshold) :
    KelGroups.Integration State AppEvent Unit StepError where
  appFold := appFold

end Reactivegas
