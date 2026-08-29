import Reactivegas.Types
import KelGroups.Fold
import KelGroups.Invariants
import KelGroups.Vote.Fold

/-!
# Per-producer composition (issue #54, Slice 2 — NOTE-031 option D)

Classifies each of the eighteen `Reactivegas.Event` constructors by the
producer that actually decides it:

* `direct` — the economic machine alone (twelve events);
* `baseEnacted` — the faithful KelGroups proposal machine enacts it
  (three events); evidence is bound to the production
  `applyEventDetailed(...).enactment` result and its threshold theorem;
* `appDecided` — the required vote machine closes a question and the
  recorded verdict carries it (three events); evidence is bound to a
  production `KelGroups.Vote.ClosureRecord`.

Each event is classified by its actual producer. The two evidence forms
share no premise: no join, shared identity, or identifier bridge exists
between the producer channels.

Status (R-35): enforced: PROVED-IN-MODEL. Everything here is a statement
about the Lean model, not about the running Haskell machine; the later
port must repeat the routing decision at the Haskell boundary, and
nothing in this repository consumes `route` at runtime today.
-/

namespace Reactivegas.Composition

/-! ## Route classification (R-31, R-32) -/

/-- Closed three-way classification of the event producers. -/
inductive Route where
  | direct
  | baseEnacted
  | appDecided
deriving DecidableEq, Repr

/-- Total wildcard-free classifier over all 18 economic event constructors.
Exact inventory: 12 `direct`, 3 `baseEnacted`, 3 `appDecided`. An added
constructor fails to compile here. -/
def route : Event → Route
  | .addUser _ _ => .direct
  | .electResponsabile _ _ => .baseEnacted
  | .removeResponsabile _ _ => .baseEnacted
  | .removeMember _ _ => .baseEnacted
  | .openPurchase _ _ => .direct
  | .grantPermission _ _ => .appDecided
  | .denyPermission _ _ => .appDecided
  | .deposit _ _ _ => .direct
  | .withdraw _ _ _ => .direct
  | .transferCassa _ _ _ => .direct
  | .donate _ _ => .direct
  | .backdonate _ _ => .appDecided
  | .pledge _ _ _ _ => .direct
  | .acceptPledge _ _ _ => .direct
  | .refusePledge _ _ _ => .direct
  | .correctPledge _ _ _ _ => .direct
  | .closePurchase _ _ => .direct
  | .failPurchase _ _ => .direct

/-- Independently total wildcard-free classifier: true exactly for the six
base/app events. `donate` is not vote-derived; `removeMember` and
`backdonate` are, so the accepted #48 additions cannot be omitted. -/
def voteDerived : Event → Bool
  | .addUser _ _ => false
  | .electResponsabile _ _ => true
  | .removeResponsabile _ _ => true
  | .removeMember _ _ => true
  | .openPurchase _ _ => false
  | .grantPermission _ _ => true
  | .denyPermission _ _ => true
  | .deposit _ _ _ => false
  | .withdraw _ _ _ => false
  | .transferCassa _ _ _ => false
  | .donate _ _ => false
  | .backdonate _ _ => true
  | .pledge _ _ _ _ => false
  | .acceptPledge _ _ _ => false
  | .refusePledge _ _ _ => false
  | .correctPledge _ _ _ _ => false
  | .closePurchase _ _ => false
  | .failPurchase _ _ => false

/-- `voteDerived` is true exactly when the event is not `direct`. -/
theorem voteDerived_iff_not_direct (e : Event) :
    voteDerived e = true ↔ route e ≠ .direct := by
  cases e <;> simp [route, voteDerived]

/-! ## Base-enacted evidence (R-33) -/

/-- The faithful base vocabulary: only `changeRoles` and `removeMember`
proposals carry base-enacted economic evidence. An added `Proposal`
constructor fails to compile here, and `introduceMember` — the voted
admission — is excluded by construction: `addUser` stays direct and no
voted-admission path exists (R-36). -/
def baseProposalFaithful : KelGroups.Proposal → Bool
  | .introduceMember _ _ _ => false
  | .removeMember _ => true
  | .changeRoles _ _ => true

/-- Base-enacted evidence: for an event the classifier sends to
`baseEnacted`, a real production call whose `Enactment` is bound by
equality to the actual `applyEventDetailed` result, and whose pending
proposal is inside the faithful vocabulary, the faithful machine's
threshold theorem holds: the enacted approvals met the majority of the
pre-state. The economic event and the `GroupEvent` stay separate
parameters joined by no premise. -/
theorem baseEnacted_threshold_met {α : Type}
    (e : Event)
    (digest : KelGroups.Proposal → KelGroups.ProposalId)
    (appFoldFn : KelGroups.AppFold α) (gs : KelGroups.GroupState α)
    (signer : KelGroups.Key) (event : KelGroups.GroupEvent α)
    (enacted : KelGroups.Enactment α)
    (hroute : route e = .baseEnacted)
    (hfaithful : baseProposalFaithful enacted.pending.proposal = true)
    (h : (KelGroups.applyEventDetailed digest appFoldFn gs signer event).enactment =
      some enacted) :
    baseProposalFaithful enacted.pending.proposal = true ∧
      enacted.pending.approvals.length ≥ KelGroups.majority enacted.preState :=
  ⟨hfaithful,
    KelGroups.enact_implies_threshold_met digest appFoldFn gs signer event enacted h⟩

/-! ## App-decided evidence (R-34) -/

/-- App-decided evidence: inspects a production closure record's verdict
over the closed three-way vocabulary. `open` derives no economic event
(the record is refused), so a recorded closure can carry an event only
from `positive` or `negative`. The faithful threshold theorem is not
used on this route. -/
def appVerdictAllows (record : KelGroups.Vote.ClosureRecord) : Bool :=
  match record.verdict with
  | .positive => true
  | .negative => true
  | .open => false

/-- The verdict elimination is exhaustive and honest: the evidence allows
an event exactly when the production record closed `positive` or
`negative`, never `open`. -/
theorem appDecided_verdict_exhaustive (e : Event)
    (record : KelGroups.Vote.ClosureRecord)
    (hroute : route e = .appDecided) :
    appVerdictAllows record = true ↔
      record.verdict = .positive ∨ record.verdict = .negative := by
  simp only [appVerdictAllows]
  cases record.verdict <;> simp

/-! ## Executed provenance witnesses (R-37) -/

/-- Reaches a real production enactment: a base `removeMember` proposal
through `applyEventDetailed` returns `some`, so the base evidence form is
reachable through the production function. -/
def productionEnactmentWitness : Bool :=
  (KelGroups.applyEventDetailed (fun _ => "pid") (fun value _ => value)
    (KelGroups.emptyState ()) "signer"
    (.base (.propose (.removeMember "member")))).enactment.isSome

/-- Reaches a real production closure: `foldVote` over admitted events
produces one closure record, the app evidence accepts it, and its
recorded verdict is `positive`. -/
def productionVerdictWitness : Bool :=
  let events : List (KelGroups.Key × KelGroups.Vote.VoteEvent) :=
    [("admin", .admitMember "admin" "admin@example" [.adminRole .publicAdmin]),
     ("admin", .openQuestion "question" .collective)]
  match (KelGroups.Vote.foldVote KelGroups.Vote.zeroThreshold events).closed with
  | [record] => appVerdictAllows record && record.verdict == .positive
  | _ => false

/-! ## Exact inventory point checks (R-31, R-32) -/

example : route (.addUser 1 2) = .direct := by rfl
example : route (.electResponsabile 1 2) = .baseEnacted := by rfl
example : route (.removeResponsabile 1 2) = .baseEnacted := by rfl
example : route (.removeMember 1 2) = .baseEnacted := by rfl
example : route (.openPurchase 1 2) = .direct := by rfl
example : route (.grantPermission 1 2) = .appDecided := by rfl
example : route (.denyPermission 1 2) = .appDecided := by rfl
example : route (.deposit 1 2 3) = .direct := by rfl
example : route (.withdraw 1 2 3) = .direct := by rfl
example : route (.transferCassa 1 2 3) = .direct := by rfl
example : route (.donate 1 2) = .direct := by rfl
example : route (.backdonate 1 2) = .appDecided := by rfl
example : route (.pledge 1 2 3 4) = .direct := by rfl
example : route (.acceptPledge 1 2 3) = .direct := by rfl
example : route (.refusePledge 1 2 3) = .direct := by rfl
example : route (.correctPledge 1 2 3 4) = .direct := by rfl
example : route (.closePurchase 1 2) = .direct := by rfl
example : route (.failPurchase 1 2) = .direct := by rfl
example : voteDerived (.donate 1 2) = false := by rfl
example : voteDerived (.removeMember 1 2) = true := by rfl
example : voteDerived (.backdonate 1 2) = true := by rfl

#guard productionEnactmentWitness
#guard productionVerdictWitness

#print axioms voteDerived_iff_not_direct
#print axioms baseEnacted_threshold_met
#print axioms appDecided_verdict_exhaustive

end Reactivegas.Composition
