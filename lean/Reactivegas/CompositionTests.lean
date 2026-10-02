import Reactivegas.Step
import Reactivegas.Composition

/-!
# Vote-derived economic effects through the production root (issue #76)

Every witness runs `Reactivegas.apply`. A closure is obtained only by executing
`openBound`/`openQuestion`/`cast`/`renounce` or a committed base change through
that root; the expected authorization is read back from the closure record the
fold emitted. A planted `ClosureRecord` or `LiveAuth` is adversarial data.

Both consumers are exercised for every row: purchase permission, bound to a
collection id, and voted backdonation, bound to the per-member share `w`.
Each row is one `#guard`, so a failing build names the row.
-/

open Reactivegas

namespace CompositionTests

abbrev Root := KelGroups.GroupState State
abbrev Ev := KelGroups.IntegratedEvent Proposal AppEvent
abbrev Res := Except ProductionError (KelGroups.IntegratedResult State)

def θ : KelGroups.Vote.Threshold := KelGroups.Vote.legacyThreshold

/-- An accepting backdonation callback. It must not authorize anything. -/
def acceptAuth : BackdonateAuth := fun _ _ => true

/-- A refusing backdonation callback. -/
def refuseAuth : BackdonateAuth := fun _ _ => false

def alice : KelGroups.Key := "alice"
def bob : KelGroups.Key := "bob"
def dave : KelGroups.Key := "dave"

def adminRoles : List KelGroups.Role :=
  [KelGroups.Role.adminRole KelGroups.Admin.publicAdmin]

def mem (k : KelGroups.Key) (roles : List KelGroups.Role) :
    KelGroups.Key × KelGroups.Member :=
  (k, { key := k, email := k ++ "@composition.test", roles })

/-- One responsabile (`alice`) and one plain member (`bob`). -/
def founded : Option Root := boot [mem alice adminRoles, mem bob []] State.empty

/-- Two responsabili. -/
def founded2 : Option Root :=
  boot [mem alice adminRoles, mem bob adminRoles] State.empty

/-- Three responsabili: `legacyThreshold 3 = 2`, base majority 2. -/
def founded3 : Option Root :=
  boot [mem alice adminRoles, mem bob adminRoles, mem dave adminRoles] State.empty

def rootAs (auth : BackdonateAuth) (gs : Root) (signer : KelGroups.Key)
    (e : Ev) : Res :=
  apply θ auth gs signer e

def app (signer : KelGroups.Key) (gs : Root) (e : AppEvent) : Res :=
  rootAs acceptAuth gs signer (.app e)

def refused : Res → Bool
  | .error (.integrated (.app .rejected)) => true
  | _ => false

def accepted : Res → Bool
  | .ok _ => true
  | .error _ => false

/-- Run signed integrated events through the root; `none` on any refusal. -/
def run : Option Root → List (KelGroups.Key × Ev) → Option Root
  | none, _ => none
  | some gs, [] => some gs
  | some gs, (signer, e) :: rest =>
      match rootAs acceptAuth gs signer e with
      | .ok r => run (some r.state) rest
      | .error _ => none

def A (e : AppEvent) : KelGroups.Key × Ev := (alice, .app e)

def soleClosed (gs : Root) : Option KelGroups.Vote.ClosureRecord :=
  match gs.appFold.votes.closed with
  | [record] => some record
  | _ => none

/-- The authorization the root holds for the sole closure: exactly one, read
off the record the fold emitted, carrying the bound target. -/
def holdsSole (gs : Root) (target : EconomicTarget) : Bool :=
  match soleClosed gs with
  | some r =>
      gs.appFold.live
        == [{ questionId := r.questionId, target, verdict := r.verdict }]
  | none => false

def collectionPresent (gs : Root) (c : CollId) : Bool :=
  (findCollection gs.appFold c).isSome

def permitted (gs : Root) (c : CollId) : Bool :=
  (findCollection gs.appFold c).map (·.permitted) == some true

def plantClosed (gs : Root) (record : KelGroups.Vote.ClosureRecord) : Root :=
  { gs with appFold :=
      { gs.appFold with votes :=
          { gs.appFold.votes with closed := record :: gs.appFold.votes.closed } } }

def forged (verdict : KelGroups.Vote.Verdict) : KelGroups.Vote.ClosureRecord :=
  { questionId := "forged"
    question := { kind := .collective, proposer := alice, assents := [], dissents := [] }
    verdict
    cause := .tally }

/-! ## Producer controls (unbound questions still close) -/

def checkProducerNegative : Bool :=
  match run founded [A (.openQuestion "q" .collective), A (.cast "q" .dissent)] with
  | some gs => (soleClosed gs).map (·.verdict) == some .negative && gs.appFold.live.isEmpty
  | none => false

def checkProducerPositive : Bool :=
  match run founded [A (.openQuestion "q" .collective), A (.cast "q" .assent)] with
  | some gs => (soleClosed gs).map (·.verdict) == some .positive && gs.appFold.live.isEmpty
  | none => false

/-! ## B-1 positive permission -/

def checkB1_positivePermits : Bool :=
  match run founded
      [A (.openPurchase 1), A (.openBound "q" .collective (.permission 1)),
       A (.cast "q" .assent)] with
  | none => false
  | some gs =>
      (soleClosed gs).map (·.verdict) == some .positive
        && holdsSole gs (.permission 1)
        && !permitted gs 1
        && match app alice gs (.grantPermission 1) with
          | .ok r => permitted r.state 1 && r.state.appFold.live.isEmpty
          | .error _ => false

/-! ## B-2 no backing closure -/

def checkB2_unbackedGrantRefused : Bool :=
  match run founded [A (.openPurchase 1)] with
  | some gs => refused (app alice gs (.grantPermission 1))
  | none => false

def checkB2_unbackedDenyRefused : Bool :=
  match run founded [A (.openPurchase 1)] with
  | some gs =>
      refused (app alice gs (.denyPermission 1))
        && (denyByClosure gs.appFold 1).isNone
  | none => false

/-- Backdonation consumer: refused even when the callback accepts. -/
def checkB2_unbackedBackdonateRefused : Bool :=
  match run founded [A (.donate 10000)] with
  | some gs => refused (app alice gs (.backdonate 5))
  | none => false

/-! ## B-3 fabricated closure -/

def checkB3_fabricatedGrantRefused : Bool :=
  match run founded [A (.openPurchase 1)] with
  | some gs =>
      refused (app alice (plantClosed gs (forged .positive)) (.grantPermission 1))
        && refused (app alice (plantClosed gs (forged .negative)) (.denyPermission 1))
  | none => false

def checkB3_fabricatedBackdonateRefused : Bool :=
  match run founded [A (.donate 10000)] with
  | some gs => refused (app alice (plantClosed gs (forged .positive)) (.backdonate 5))
  | none => false

/-- A production history cannot start from planted closures, bindings or
authorizations. -/
def checkB3_bootRejectsFabricatedOrigin : Bool :=
  let members := [mem alice adminRoles, mem bob []]
  (boot members { State.empty with
      votes := { State.empty.votes with closed := [forged .positive] } }).isNone
    && (boot members { State.empty with
      live := [{ questionId := "forged", target := .permission 1, verdict := .positive }] }).isNone
    && (boot members { State.empty with
      bindings := [("forged", .backdonation 5)] }).isNone
    && (boot members State.empty).isSome

/-! ## B-4 target -/

/-- A closure bound to collection 1 does not permit collection 2; the refusal
leaves the authorization for 1 intact. -/
def checkB4_wrongCollectionRefused : Bool :=
  match run founded
      [A (.openPurchase 1), A (.openPurchase 2),
       A (.openBound "q" .collective (.permission 1)), A (.cast "q" .assent)] with
  | none => false
  | some gs =>
      refused (app alice gs (.grantPermission 2))
        && match app alice gs (.grantPermission 1) with
          | .ok r => permitted r.state 1 && !permitted r.state 2
          | .error _ => false

/-- A collection id reused after its collection left is a new collection: an
unspent authorization for the old one does not carry over. -/
def checkB4_reusedCollectionIdRefused : Bool :=
  match run founded
      [A (.openPurchase 1), A (.openBound "q" .collective (.permission 1)),
       A (.cast "q" .assent), A (.failPurchase 1), A (.openPurchase 1)] with
  | none => false
  | some gs =>
      gs.appFold.live.isEmpty && refused (app alice gs (.grantPermission 1))
        && match run founded
            [A (.openPurchase 1), A (.openBound "q" .collective (.permission 1)),
             A (.failPurchase 1), A (.openPurchase 1), A (.cast "q" .assent)] with
          | some gs' =>
              (soleClosed gs').map (·.verdict) == some .positive
                && gs'.appFold.live.isEmpty
                && refused (app alice gs' (.grantPermission 1))
          | none => false

/-! ## B-5 polarity -/

def checkB5_negativeDoesNotGrant : Bool :=
  match run founded
      [A (.openPurchase 1), A (.openBound "q" .collective (.permission 1)),
       A (.cast "q" .dissent)] with
  | none => false
  | some gs =>
      (soleClosed gs).map (·.verdict) == some .negative
        && holdsSole gs (.permission 1)
        && refused (app alice gs (.grantPermission 1))

def checkB5_positiveDoesNotDeny : Bool :=
  match run founded
      [A (.openPurchase 1), A (.openBound "q" .collective (.permission 1)),
       A (.cast "q" .assent)] with
  | none => false
  | some gs =>
      refused (app alice gs (.denyPermission 1)) && (denyByClosure gs.appFold 1).isNone

/-- The negative continuation refunds both accepted and pending escrow, both
populated by production events before the closure. -/
def checkB5_negativeRefundsAllEscrow : Bool :=
  match run founded2
      [ (alice, .app (.deposit bob 20)), (bob, .app (.deposit alice 30))
      , A (.openPurchase 1), A (.pledge alice 1 7), A (.acceptPledge alice 1)
      , A (.pledge bob 1 11)
      , A (.openBound "q" .collective (.permission 1)), A (.cast "q" .dissent) ] with
  | none => false
  | some gs =>
      holdsSole gs (.permission 1)
        && match app alice gs (.denyPermission 1) with
          | .ok r =>
              !collectionPresent r.state 1
                && bal r.state.appFold.conti alice == 30
                && bal r.state.appFold.conti bob == 20
                && r.state.appFold.live.isEmpty
          | .error _ => false

/-! ## B-6 consumption -/

/-- A spent grant cannot authorize a second one, though the collection is still
there and a second grant is otherwise eligible; nor can it authorize a deny. -/
def checkB6_grantNotReusable : Bool :=
  match run founded
      [A (.openPurchase 1), A (.openBound "q" .collective (.permission 1)),
       A (.cast "q" .assent)] with
  | none => false
  | some gs =>
      match app alice gs (.grantPermission 1) with
      | .error _ => false
      | .ok r =>
          collectionPresent r.state 1
            && refused (app alice r.state (.grantPermission 1))
            && refused (app alice r.state (.denyPermission 1))

/-- Reusing a spent question id cannot revive its authorization: a bound
reopen is refused, and a plain reopen is the vote machine's admitted no-op
and mints nothing. -/
def checkB6_questionIdReuseNoRevive : Bool :=
  match run founded
      [A (.openPurchase 1), A (.openBound "q" .collective (.permission 1)),
       A (.cast "q" .assent), A (.grantPermission 1)] with
  | none => false
  | some gs =>
      refused (app alice gs (.openBound "q" .collective (.permission 1)))
        && match run (some gs) [A (.openQuestion "q" .collective)] with
          | some gs' =>
              gs'.appFold.votes == gs.appFold.votes
                && gs'.appFold.live.isEmpty
                && refused (app alice gs' (.cast "q" .assent))
                && refused (app alice gs' (.grantPermission 1))
          | none => false

/-! ## B-7 open derives nothing -/

def checkB7_openDerivesNothing : Bool :=
  match run founded
      [A (.openPurchase 1), A (.donate 10000),
       A (.openBound "qp" .collective (.permission 1)),
       A (.openBound "qb" .collective (.backdonation 5))] with
  | none => false
  | some gs =>
      gs.appFold.votes.closed.isEmpty
        && gs.appFold.live.isEmpty
        && refused (app alice gs (.grantPermission 1))
        && refused (app alice gs (.denyPermission 1))
        && refused (app alice gs (.backdonate 5))

/-! ## B-8 voted backdonation bound to `w` -/

/-- A positive closure bound to `w = 5` pays `n * w` out of the comune and
`w` to every canonical member, and is spent by it. -/
def checkB8_positiveBackdonates : Bool :=
  match run founded
      [A (.donate 100), A (.openBound "q" .collective (.backdonation 5)),
       A (.cast "q" .assent)] with
  | none => false
  | some gs =>
      holdsSole gs (.backdonation 5)
        && match app alice gs (.backdonate 5) with
          | .ok r =>
              comuneBal r.state.appFold == 90
                && bal r.state.appFold.conti alice == 5
                && bal r.state.appFold.conti bob == 5
                && r.state.appFold.live.isEmpty
          | .error _ => false

/-- The callback's remaining scope: it can veto a closure-authorized
backdonation, leaving the authorization unspent, and it can authorize none. -/
def checkB8_callbackVetoOnly : Bool :=
  match run founded
      [A (.donate 100), A (.openBound "q" .collective (.backdonation 5)),
       A (.cast "q" .assent)] with
  | none => false
  | some gs =>
      refused (rootAs refuseAuth gs alice (.app (.backdonate 5)))
        && accepted (rootAs acceptAuth gs alice (.app (.backdonate 5)))
        && match run founded [A (.donate 100)] with
          | some bare => refused (rootAs acceptAuth bare alice (.app (.backdonate 5)))
          | none => false

/-- B-8a: a closure binding `w = 5` refuses every other share. -/
def checkB8a_wrongShareRefused : Bool :=
  match run founded
      [A (.donate 10000), A (.openBound "q" .collective (.backdonation 5)),
       A (.cast "q" .assent)] with
  | none => false
  | some gs =>
      refused (app alice gs (.backdonate 500))
        && refused (app alice gs (.backdonate 4))
        && accepted (app alice gs (.backdonate 5))

/-- No cross-consumer use: a permission closure for collection 5 is not a
backdonation of 5, and a backdonation closure for `w = 1` is not permission
for collection 1. -/
def checkB8_crossConsumerRefused : Bool :=
  (match run founded
      [A (.donate 10000), A (.openPurchase 5),
       A (.openBound "q" .collective (.permission 5)), A (.cast "q" .assent)] with
    | some gs => refused (app alice gs (.backdonate 5))
    | none => false)
  && (match run founded
      [A (.donate 10000), A (.openPurchase 1),
       A (.openBound "q" .collective (.backdonation 1)), A (.cast "q" .assent)] with
    | some gs => refused (app alice gs (.grantPermission 1))
    | none => false)

def checkB8b_negativeNoBackdonate : Bool :=
  match run founded
      [A (.donate 10000), A (.openBound "q" .collective (.backdonation 5)),
       A (.cast "q" .dissent)] with
  | none => false
  | some gs =>
      (soleClosed gs).map (·.verdict) == some .negative
        && holdsSole gs (.backdonation 5)
        && refused (app alice gs (.backdonate 5))

/-- B-8c: a spent backdonation closure cannot pay again, though the comune
could afford it. -/
def checkB8c_backdonateNotReusable : Bool :=
  match run founded
      [A (.donate 10000), A (.openBound "q" .collective (.backdonation 5)),
       A (.cast "q" .assent)] with
  | none => false
  | some gs =>
      match app alice gs (.backdonate 5) with
      | .error _ => false
      | .ok r =>
          comuneBal r.state.appFold ≥ 10
            && refused (app alice r.state (.backdonate 5))

/-! ## R76-09 the target is fixed by the proposer before any ballot -/

/-- A second bind of an open id is refused, and the first target stands. -/
def checkR9_rebindRefused : Bool :=
  match run founded
      [A (.openPurchase 1), A (.openPurchase 2),
       A (.openBound "q" .collective (.permission 1))] with
  | none => false
  | some gs =>
      refused (app alice gs (.openBound "q" .collective (.permission 2)))
        && match run (some gs) [A (.cast "q" .assent)] with
          | some gs' => holdsSole gs' (.permission 1)
              && refused (app alice gs' (.grantPermission 2))
          | none => false

/-- Binding a question that is already open — after a ballot, or by anyone
other than its proposer — is refused. -/
def checkR9_lateOrForeignBindRefused : Bool :=
  match run founded3
      [A (.openPurchase 1), A (.openQuestion "q" .collective),
       (bob, .app (.cast "q" .assent))] with
  | none => false
  | some gs =>
      (KelGroups.Vote.lookupQuestion "q" gs.appFold.votes).isSome
        && refused (app alice gs (.openBound "q" .collective (.permission 1)))
        && refused (app dave gs (.openBound "q" .collective (.permission 1)))

/-- A permission target must name a present collection; a non-responsabile
cannot open a bound question. -/
def checkR9_unboundableRefused : Bool :=
  match run founded [A (.openPurchase 1)] with
  | none => false
  | some gs =>
      refused (app alice gs (.openBound "q" .collective (.permission 9)))
        && refused (app bob gs (.openBound "q" .collective (.permission 1)))
        && accepted (app alice gs (.openBound "q" .collective (.permission 1)))

/-! ## R76-07 one negative continuation for every negative cause -/

/-- The cause the fold recorded, the negative authorization it minted, and
the continuation refunding through both the interface and the root. -/
def negativeContinues (gs : Root) (cause : KelGroups.Vote.ClosureCause) : Bool :=
  (gs.appFold.votes.closed.map (·.cause)) == [cause]
    && holdsSole gs (.permission 1)
    && (match denyByClosure gs.appFold 1 with
        | some s => (findCollection s 1).isNone && s.live.isEmpty
        | none => false)
    && match app alice gs (.denyPermission 1) with
      | .ok r => !collectionPresent r.state 1 && r.state.appFold.live.isEmpty
      | .error _ => false

def checkR7_tallyContinues : Bool :=
  match run founded
      [A (.openPurchase 1), A (.openBound "q" .collective (.permission 1)),
       A (.cast "q" .dissent)] with
  | some gs => negativeContinues gs .tally
  | none => false

def checkR7_renouncedContinues : Bool :=
  match run founded
      [A (.openPurchase 1), A (.openBound "q" .collective (.permission 1)),
       A (.renounce "q")] with
  | some gs => negativeContinues gs .renounced
  | none => false

def checkR7_proposerDepartedContinues : Bool :=
  match run founded3
      [A (.openPurchase 1), (bob, .app (.openBound "q" .collective (.permission 1))),
       (alice, .propose (.departure bob)), (dave, .approve (proposalDigest (.departure bob)))] with
  | some gs => negativeContinues gs .proposerDeparted
  | none => false

def checkR7_franchiseChangeContinues : Bool :=
  match run founded3
      [A (.openPurchase 1), A (.openBound "q" .collective (.permission 1)),
       (bob, .app (.cast "q" .dissent)),
       (alice, .propose (.changeRoles bob [])),
       (dave, .approve (proposalDigest (.changeRoles bob [])))] with
  | some gs => negativeContinues gs .franchiseChange
  | none => false

/-! ## B-9 classifier (must stay) -/

def checkB9_classifier : Bool :=
  (Composition.route (.grantPermission alice 1) == .appDecided)
    && (Composition.route (.denyPermission alice 1) == .appDecided)
    && (Composition.route (.backdonate alice 5) == .appDecided)
    && (Composition.voteDerived (.grantPermission alice 1) == true)
    && (Composition.voteDerived (.donate alice 1) == false)

#guard checkProducerNegative
#guard checkProducerPositive
#guard checkB1_positivePermits
#guard checkB2_unbackedGrantRefused
#guard checkB2_unbackedDenyRefused
#guard checkB2_unbackedBackdonateRefused
#guard checkB3_fabricatedGrantRefused
#guard checkB3_fabricatedBackdonateRefused
#guard checkB3_bootRejectsFabricatedOrigin
#guard checkB4_wrongCollectionRefused
#guard checkB4_reusedCollectionIdRefused
#guard checkB5_negativeDoesNotGrant
#guard checkB5_positiveDoesNotDeny
#guard checkB5_negativeRefundsAllEscrow
#guard checkB6_grantNotReusable
#guard checkB6_questionIdReuseNoRevive
#guard checkB7_openDerivesNothing
#guard checkB8_positiveBackdonates
#guard checkB8_callbackVetoOnly
#guard checkB8a_wrongShareRefused
#guard checkB8_crossConsumerRefused
#guard checkB8b_negativeNoBackdonate
#guard checkB8c_backdonateNotReusable
#guard checkR9_rebindRefused
#guard checkR9_lateOrForeignBindRefused
#guard checkR9_unboundableRefused
#guard checkR7_tallyContinues
#guard checkR7_renouncedContinues
#guard checkR7_proposerDepartedContinues
#guard checkR7_franchiseChangeContinues
#guard checkB9_classifier

end CompositionTests
