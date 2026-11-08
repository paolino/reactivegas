import Reactivegas.Step
import Reactivegas.Composition
import Reactivegas.CompositionRefund

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

/-- Base-channel steps under the production fold's rule: a refused step leaves
the aggregate as it was. A base change is enacted by whichever approval reaches
the majority, so an approval scripted after it may find no pending proposal. -/
def runBase : Option Root → List (KelGroups.Key × Ev) → Option Root
  | none, _ => none
  | some gs, [] => some gs
  | some gs, (signer, e) :: rest =>
      match rootAs acceptAuth gs signer e with
      | .ok r => runBase (some r.state) rest
      | .error _ => runBase (some gs) rest

/-- Two non-proposer approvals among three responsabili: a majority whether or
not the proposer's own signature counts as an approval. -/
def checkR7_proposerDepartedContinues : Bool :=
  match runBase
      (run founded3
        [A (.openPurchase 1), (bob, .app (.openBound "q" .collective (.permission 1)))])
      [(alice, .propose (.departure bob)),
       (dave, .approve (proposalDigest (.departure bob))),
       (bob, .approve (proposalDigest (.departure bob)))] with
  | some gs => negativeContinues gs .proposerDeparted
  | none => false

def checkR7_franchiseChangeContinues : Bool :=
  match runBase
      (run founded3
        [A (.openPurchase 1), A (.openBound "q" .collective (.permission 1)),
         (bob, .app (.cast "q" .dissent))])
      [(alice, .propose (.changeRoles bob [])),
       (dave, .approve (proposalDigest (.changeRoles bob []))),
       (bob, .approve (proposalDigest (.changeRoles bob [])))] with
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


/-! ## L-7: no balance is stranded by a V-5 negative closure (issue #81, S81-B)

Every witness runs the production root. The V-5 closure is obtained only by
executing `renounce` or a committed departure through it; the expected balances
are computed from the pledges the fixture's own collection holds, by
`pledgedBy`, never typed. A refund through a negative closure needs a
responsabile's `denyPermission`, as for every negative closure (#76); a
departing referente's collections are refunded inside the departure instead.

`Transition` is the root under test: the production root, or a mutant of it. One
oracle (`strandingFree`) takes the transition as its argument; production is
accepted and each mutant is rejected by that same function. -/

open Reactivegas.Composition

/-- A root transition under test: the production root, or a mutant of it. -/
abbrev Transition := Root → KelGroups.Key → Ev → Res

/-- The production root. -/
def productionStep : Transition := fun gs signer e => rootAs acceptAuth gs signer e

/-- Run signed integrated events through a transition; `none` on any refusal. -/
def runWith (T : Transition) : Option Root → List (KelGroups.Key × Ev) → Option Root
  | none, _ => none
  | some gs, [] => some gs
  | some gs, (signer, e) :: rest =>
      match T gs signer e with
      | .ok r => runWith T (some r.state) rest
      | .error _ => none

def carol : KelGroups.Key := "carol"

/-- Three responsabili (`alice`, `bob`, `dave`) and one plain member (`carol`). -/
def foundedL7 : Option Root :=
  boot [mem alice adminRoles, mem bob adminRoles, mem dave adminRoles, mem carol []]
    State.empty

def AS (signer : KelGroups.Key) (e : AppEvent) : KelGroups.Key × Ev := (signer, .app e)

/-- Funding: the comune stays solvent through a departing responsabile's cassa,
and three accounts can pledge. -/
def l7Funding : List (KelGroups.Key × Ev) :=
  [ AS alice (.donate 10000), AS bob (.deposit alice 30), AS alice (.deposit carol 20),
    AS alice (.deposit dave 30) ]

/-- Collection 1 (referente `referente`) holds accepted pledges of `alice` and
`carol` and a pending one of `dave`; collection 2 (referente `alice`) holds an
accepted pledge of `dave` and a pending one of `carol`. -/
def l7Escrow (referente : KelGroups.Key) : List (KelGroups.Key × Ev) :=
  [ AS referente (.openPurchase 1), AS alice (.openPurchase 2),
    AS alice (.pledge alice 1 7), AS referente (.acceptPledge alice 1),
    AS alice (.pledge carol 1 5), AS referente (.acceptPledge carol 1),
    AS alice (.pledge dave 1 11),
    AS alice (.pledge dave 2 4), AS alice (.acceptPledge dave 2),
    AS alice (.pledge carol 2 3) ]

/-- `bob` opens the question whose closure authorizes collection 1's permission. -/
def l7Bind : List (KelGroups.Key × Ev) :=
  [AS bob (.openBound "q" .collective (.permission 1))]

def l7Renounce : KelGroups.Key × Ev := AS bob (.renounce "q")

/-- Two approvals of three responsabili enact the departure; the second, the
departing proposer's own, is the step that commits it. -/
def l7DepartPrefix : List (KelGroups.Key × Ev) :=
  [ (alice, .propose (.departure bob)), (dave, .approve (proposalDigest (.departure bob))) ]

def l7DepartStep : KelGroups.Key × Ev := (bob, .approve (proposalDigest (.departure bob)))

/-- Proposer `bob` is not collection 1's referente (`alice`); `bob` renounces. -/
def l7RenounceSteps : List (KelGroups.Key × Ev) :=
  l7Funding ++ l7Escrow alice ++ l7Bind ++ [l7Renounce]

/-- Proposer `bob` is collection 1's referente. -/
def l7RenounceReferenteSteps : List (KelGroups.Key × Ev) :=
  l7Funding ++ l7Escrow bob ++ l7Bind ++ [l7Renounce]

/-- Proposer `bob` is not collection 1's referente; `bob` departs. -/
def l7DepartSteps : List (KelGroups.Key × Ev) :=
  l7Funding ++ l7Escrow alice ++ l7Bind ++ l7DepartPrefix ++ [l7DepartStep]

/-- Proposer `bob` is collection 1's referente: set-up up to the open question,
then the departure. -/
def l7ReferenteSetup : List (KelGroups.Key × Ev) := l7Funding ++ l7Escrow bob ++ l7Bind

def l7ReferenteDeparts : List (KelGroups.Key × Ev) := l7DepartPrefix ++ [l7DepartStep]

/-- **Refund through the negative authorization.** After the steps, the only
closure has `cause`; collection `c` holds at least two pledgers with an accepted
and a pending pledge, and a live negative authorization for `c`; a responsabile's
`denyPermission c` through the transition removes `c`, leaves the other
collection and every authorization and binding unrelated to `c`, hands each
pledger exactly what they pledged, and conserves money. -/
def refundHolds (T : Transition) (steps : List (KelGroups.Key × Ev))
    (cause : KelGroups.Vote.ClosureCause) (c other : CollId) (denier : KelGroups.Key) :
    Bool :=
  match runWith T foundedL7 steps with
  | none => false
  | some g =>
      let s := g.appFold
      match findCollection s c, findCollection s other with
      | some col, some oth =>
          let owners := (col.accepted ++ col.pending).map (·.user)
          (s.votes.closed.map (·.cause) == [cause])
            && !col.accepted.isEmpty && !col.pending.isEmpty
            && decide (2 ≤ owners.eraseDups.length)
            && s.live.any (fun a => a.target == .permission c && a.verdict == .negative)
            && (match T g denier (.app (.denyPermission c)) with
                | .ok r =>
                    let t := r.state.appFold
                    (findCollection t c).isNone
                      && findCollection t other == some oth
                      && owners.all (fun u => bal t.conti u == bal s.conti u + pledgedBy col u)
                      && !(t.live.any (fun a => a.target == .permission c))
                      && !(t.bindings.any (fun b => b.2 == .permission c))
                      && sumBal t.conti + escrowSum t.collections
                        == sumBal s.conti + escrowSum s.collections
                | .error _ => false)
      | _, _ => false

/-- **Refund inside the departure.** A departing proposer who is collection
`c`'s referente takes `c` with them: it is refunded within the departure, every
pledger receives exactly what they pledged, the other collection is untouched,
no authorization or binding names `c` afterwards, and `denyPermission c` is
refused. -/
def refundedInside (T : Transition) (setup closing : List (KelGroups.Key × Ev))
    (cause : KelGroups.Vote.ClosureCause) (c other : CollId) (denier : KelGroups.Key) :
    Bool :=
  match runWith T foundedL7 setup with
  | none => false
  | some g0 =>
      let s := g0.appFold
      match findCollection s c, findCollection s other, runWith T (some g0) closing with
      | some col, some oth, some g1 =>
          let owners := (col.accepted ++ col.pending).map (·.user)
          let t := g1.appFold
          (t.votes.closed.map (·.cause) == [cause])
            && !col.accepted.isEmpty && !col.pending.isEmpty
            && decide (2 ≤ owners.eraseDups.length)
            && (findCollection t c).isNone
            && findCollection t other == some oth
            && owners.all (fun u => bal t.conti u == bal s.conti u + pledgedBy col u)
            && !(t.live.any (fun a => a.target == .permission c))
            && !(t.bindings.any (fun b => b.2 == .permission c))
            && refused (T g1 denier (.app (.denyPermission c)))
      | _, _, _ => false

/-- The oracle of the L-7 mutants: the production root is accepted by it and
each mutant of it is rejected by this same function. -/
def strandingFree (T : Transition) : Bool :=
  refundHolds T l7RenounceSteps .renounced 1 2 dave
    && refundHolds T l7RenounceReferenteSteps .renounced 1 2 dave
    && refundHolds T l7DepartSteps .proposerDeparted 1 2 dave
    && refundedInside T l7ReferenteSetup l7ReferenteDeparts .proposerDeparted 1 2 alice

def checkL7a_renounceRefunds : Bool :=
  refundHolds productionStep l7RenounceSteps .renounced 1 2 dave
    && refundHolds productionStep l7RenounceReferenteSteps .renounced 1 2 dave

def checkL7b_departureRefundsThroughDeny : Bool :=
  refundHolds productionStep l7DepartSteps .proposerDeparted 1 2 dave

def checkL7b_referenteRefundedInside : Bool :=
  refundedInside productionStep l7ReferenteSetup l7ReferenteDeparts .proposerDeparted 1 2 alice

def checkL7_productionStrandsNothing : Bool := strandingFree productionStep

#guard checkL7a_renounceRefunds
#guard checkL7b_departureRefundsThroughDeny
#guard checkL7b_referenteRefundedInside
#guard checkL7_productionStrandsNothing

/-! ### L-7c: the antecedent of the no-stranding theorem is reachable -/

/-- A production history: boot, then the events through the root. -/
def l7History (events : List (KelGroups.Key × Ev)) :
    Option (Σ' g : Root, ProductionHistory θ acceptAuth g) :=
  let members := [mem alice adminRoles, mem bob adminRoles, mem dave adminRoles, mem carol []]
  match hboot : boot members State.empty with
  | some gs => extendHistory gs (.boot members State.empty hboot) events
  | none => none

/-- The step closes `qid` negatively on `c`, and `c` holds an accepted and a
pending pledge afterwards. -/
def l7Closes (g : Root) (signer : KelGroups.Key) (ev : Ev) (c : CollId)
    (qid : KelGroups.Vote.QuestionId) : Bool :=
  match apply θ acceptAuth g signer ev with
  | .ok r =>
      closesNegativeBound g.appFold r.state.appFold ev c qid
        && (findCollection r.state.appFold c).any
          (fun col => !col.accepted.isEmpty && !col.pending.isEmpty)
  | .error _ => false

/-- A V-5 renounce closure with escrow present, reached from a production
history. -/
theorem l7_renounce_reachable :
    (l7History (l7Funding ++ l7Escrow alice ++ l7Bind)).map
      (fun p => l7Closes p.1 bob (.app (.renounce "q")) 1 "q") = some true := by
  decide

/-- A V-5 departure closure with escrow present, reached from a production
history, by the step that commits the departure. -/
theorem l7_departure_reachable :
    (l7History (l7Funding ++ l7Escrow alice ++ l7Bind ++ l7DepartPrefix)).map
      (fun p => l7Closes p.1 bob (.approve (proposalDigest (.departure bob))) 1 "q")
        = some true := by
  decide

theorem l7_antecedent_of_check {events : List (KelGroups.Key × Ev)}
    {signer : KelGroups.Key} {ev : Ev} {c : CollId} {qid : KelGroups.Vote.QuestionId}
    (h : (l7History events).map (fun p => l7Closes p.1 signer ev c qid) = some true) :
    ∃ g : Root, ProductionHistory θ acceptAuth g ∧ ∃ res, apply θ acceptAuth g signer ev = .ok res
      ∧ closesNegativeBound g.appFold res.state.appFold ev c qid = true := by
  cases hh : l7History events with
  | none => simp [hh] at h
  | some p =>
    refine ⟨p.1, p.2, ?_⟩
    simp only [hh, Option.map_some, Option.some.injEq] at h
    unfold l7Closes at h
    split at h
    · next res hres =>
      simp only [Bool.and_eq_true] at h
      exact ⟨res, hres, h.1⟩
    · exact Bool.noConfusion h

/-- The no-stranding conclusion holds, non-vacuously, in a named production
history whose renounce closure leaves escrow in collection 1. -/
theorem l7_renounce_no_stranding :
    ∃ gsN : Root, ProductionHistory θ acceptAuth gsN
      ∧ targetPresent gsN.appFold (.permission 1) = true
      ∧ ∃ a ∈ gsN.appFold.live,
          a.questionId = "q" ∧ a.target = .permission 1 ∧ a.verdict = .negative := by
  obtain ⟨g, hist, res, happly, hcloses⟩ := l7_antecedent_of_check l7_renounce_reachable
  exact ⟨res.state, noStranding_of_history hist happly hcloses (steps := []) rfl⟩

theorem l7_departure_no_stranding :
    ∃ gsN : Root, ProductionHistory θ acceptAuth gsN
      ∧ targetPresent gsN.appFold (.permission 1) = true
      ∧ ∃ a ∈ gsN.appFold.live,
          a.questionId = "q" ∧ a.target = .permission 1 ∧ a.verdict = .negative := by
  obtain ⟨g, hist, res, happly, hcloses⟩ := l7_antecedent_of_check l7_departure_reachable
  exact ⟨res.state, noStranding_of_history hist happly hcloses (steps := []) rfl⟩

/-- The continuation guard can fail: unrelated steps keep the authorization; a
spend, and a step that removes the collection (after the steps that make it
removable succeed), end the continuation. -/
def checkL7c_continuationGuards : Bool :=
  match runWith productionStep foundedL7 l7RenounceSteps with
  | none => false
  | some g =>
      (runHolding θ acceptAuth 1 g
          [AS alice (.donate 5), AS dave (.deposit carol 1)]).isSome
        && (runHolding θ acceptAuth 1 g [AS dave (.denyPermission 1)]).isNone
        && (runWith productionStep (some g)
            [AS alice (.refusePledge dave 1), AS alice (.failPurchase 1)]).isSome
        && (runHolding θ acceptAuth 1 g
            [AS alice (.refusePledge dave 1), AS alice (.failPurchase 1)]).isNone

#guard checkL7c_continuationGuards


/-! ### Mutants of the L-7 refund

Mutation-only definitions: no production definition uses them. Each is the
production root with one edit, run through the same oracle `strandingFree` that
accepts production, and each is rejected by it. -/

/-- Mutation-only (M-MINT): a closure by renounce or by the proposer's departure
mints no authorization, so a bound permission question closed that way leaves
nothing for `denyPermission` to spend. -/
def mintsNothingForV5 : Transition := fun gs signer e =>
  match productionStep gs signer e with
  | .ok r =>
      let s := r.state.appFold
      .ok { r with state := { r.state with appFold := { s with
        live := s.live.filter (fun a => !(s.votes.closed.any (fun rcd =>
          rcd.questionId == a.questionId
            && (rcd.cause == .renounced || rcd.cause == .proposerDeparted)))) } } }
  | .error err => .error err

/-- Mutation-only (M-REFUND): `denyPermission` under the authorization dissolves
the collection but leaves its first pledge unrefunded. -/
def denyLeavesPledge : Transition := fun gs signer e =>
  match productionStep gs signer e, e with
  | .ok r, .app (.denyPermission c) =>
      match findCollection gs.appFold c with
      | some col =>
          match col.accepted ++ col.pending with
          | p :: _ =>
              .ok { r with state := { r.state with appFold := { r.state.appFold with
                conti := bump r.state.appFold.conti p.user (-p.amount) } } }
          | [] => .ok r
      | none => .ok r
  | res, _ => res

/-- Both mutants execute: the mutated run reaches the very closure the production
run reaches, and differs from it only in the edited effect. -/
def checkL7_mutantsExecute : Bool :=
  match runWith productionStep foundedL7 l7RenounceSteps,
      runWith mintsNothingForV5 foundedL7 l7RenounceSteps,
      runWith productionStep foundedL7 l7DepartSteps,
      runWith mintsNothingForV5 foundedL7 l7DepartSteps with
  | some pr, some mr, some pd, some md =>
      pr.appFold.votes == mr.appFold.votes && pd.appFold.votes == md.appFold.votes
        && pr.appFold.live.any (fun a => a.target == .permission 1)
        && pd.appFold.live.any (fun a => a.target == .permission 1)
        && mr.appFold.live.isEmpty && md.appFold.live.isEmpty
        && (match findCollection pr.appFold 1,
            productionStep pr dave (.app (.denyPermission 1)),
            denyLeavesPledge pr dave (.app (.denyPermission 1)) with
          | some col, .ok good, .ok bad =>
              match col.accepted ++ col.pending with
              | q :: _ =>
                  (findCollection bad.state.appFold 1).isNone
                    && bal bad.state.appFold.conti q.user + q.amount
                      == bal good.state.appFold.conti q.user
              | [] => false
          | _, _, _ => false)
  | _, _, _, _ => false

def checkL7_mintMutantRejected : Bool :=
  !(strandingFree mintsNothingForV5)
    && !(refundHolds mintsNothingForV5 l7RenounceSteps .renounced 1 2 dave)
    && !(refundHolds mintsNothingForV5 l7DepartSteps .proposerDeparted 1 2 dave)

def checkL7_refundMutantRejected : Bool :=
  !(strandingFree denyLeavesPledge)
    && !(refundHolds denyLeavesPledge l7RenounceSteps .renounced 1 2 dave)
    && !(refundHolds denyLeavesPledge l7DepartSteps .proposerDeparted 1 2 dave)

theorem l7_production_strands_nothing : strandingFree productionStep = true := by decide

theorem l7_mutants_execute : checkL7_mutantsExecute = true := by decide

theorem l7_mint_mutant_rejected : checkL7_mintMutantRejected = true := by decide

theorem l7_refund_mutant_rejected : checkL7_refundMutantRejected = true := by decide

/-- The no-stranding predicate on a named history: collection `c` is present and
the payload holds the negative authorization carrying `qid` (`holdsNegativeAuth`,
the Bool form of the conclusion of `noStranding_of_history`). -/
def strandsNothing (T : Transition) (steps : List (KelGroups.Key × Ev)) (c : CollId)
    (qid : KelGroups.Vote.QuestionId) : Bool :=
  match runWith T foundedL7 steps with
  | some g => targetPresent g.appFold (.permission c) && holdsNegativeAuth g.appFold c qid
  | none => false

/-- M-MINT falsifies the predicate on both named histories; production satisfies it. -/
def checkL7c_predicateOnMintMutant : Bool :=
  strandsNothing productionStep l7RenounceSteps 1 "q"
    && strandsNothing productionStep l7DepartSteps 1 "q"
    && !(strandsNothing mintsNothingForV5 l7RenounceSteps 1 "q")
    && !(strandsNothing mintsNothingForV5 l7DepartSteps 1 "q")

theorem l7c_predicate_on_mint_mutant : checkL7c_predicateOnMintMutant = true := by decide

end CompositionTests
