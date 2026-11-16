import KelGroups.Types
import KelGroups.Vote.Types

/-!
# Reactivegas domain types

Abstract identifiers, money, and the event vocabulary of the economic
machine extracted from the legacy `Eventi/` reactors (issue #45).

Identity is substrate `KelGroups.Key`. Membership and role live only in
`KelGroups.GroupState.members`; nothing here is a second store.
-/

/-- Identifier of an open economic collection (a purchase). -/
abbrev CollId := Nat

/-- **Reserved comune account key** (issue #48): the common fund lives
at this `KelGroups.Key` inside `conti` — never as a standalone `State`
field, and never as membership. The production root `Reactivegas.apply`
refuses any aggregate that already lists this key as a member; direct
admission of this key remains S62-B (`validateDirectAdmission`). -/
abbrev comuneId : KelGroups.Key := "comune"

/-- A single pledge of money by a user inside a collection. Amounts are
plain integers; the legacy `Euro`/`DEuro` types are integer-valued. -/
structure Pledge where
  user : KelGroups.Key
  amount : Int
deriving DecidableEq, BEq, Repr

/-
The fourteen surviving economic constructors (T6222). The four
membership/role constructors — `addUser`, `electResponsabile`,
`removeResponsabile`, `removeMember` — are gone: membership has exactly
one writable store and one insertion path, and neither is here. They are
not replaced by compatibility routes or inert cases. Their #54 producer
classes leave `Composition.route` with them; the remaining fourteen keep
theirs, eleven `direct` and three `appDecided`. Every identity is
`KelGroups.Key`.
-/
inductive Event where
  /-- Open a purchase: opens the pledge collection plus a majority question. -/
  | openPurchase (author : KelGroups.Key) (c : CollId)
  /-- Interface event: the group assented; closure permission granted. -/
  | grantPermission (author : KelGroups.Key) (c : CollId)
  /-- Interface event: the group dissented; purchase fails with full refunds. -/
  | denyPermission (author : KelGroups.Key) (c : CollId)
  /-- Deposit: move user credit and the acting cashier's cassa together. -/
  | deposit (author user : KelGroups.Key) (v : Int)
  /-- Withdrawal: symmetric to a deposit. -/
  | withdraw (author user : KelGroups.Key) (v : Int)
  /-- Cassa-to-cassa transfer between two responsabili. -/
  | transferCassa (author from_ : KelGroups.Key) (v : Int)
  /-- **Attested donation** (issue #48): the cash arrived first. -/
  | donate (author : KelGroups.Key) (v : Int)
  /-- **Voted equal-share backdonation** (issue #48). -/
  | backdonate (author : KelGroups.Key) (w : Int)
  /-- Pledge: debits the pledger's credit immediately into escrow. -/
  | pledge (author user : KelGroups.Key) (c : CollId) (v : Int)
  /-- Referente consent: move the pledge from pending to accepted. -/
  | acceptPledge (author user : KelGroups.Key) (c : CollId)
  /-- Referente refusal: refund the pending pledge. -/
  | refusePledge (author user : KelGroups.Key) (c : CollId)
  /-- Referente-only correction of an accepted pledge: settle the difference. -/
  | correctPledge (author user : KelGroups.Key) (c : CollId) (v : Int)
  /-- Positive closure: spends the referente's cassa by the collected total. -/
  | closePurchase (author : KelGroups.Key) (c : CollId)
  /-- Referente-initiated failure: refunds every pledge. -/
  | failPurchase (author : KelGroups.Key) (c : CollId)
deriving DecidableEq, Repr

/-- Transitional integrated app-event surface (NOTE-002): the fourteen
surviving economic actions, with no author field — the signer arrives
from the fold — and no membership/role constructor. -/
inductive AppEvent where
  | openPurchase (c : CollId)
  | grantPermission (c : CollId)
  | denyPermission (c : CollId)
  | deposit (user : KelGroups.Key) (v : Int)
  | withdraw (user : KelGroups.Key) (v : Int)
  | transferCassa (from_ : KelGroups.Key) (v : Int)
  | donate (v : Int)
  | backdonate (w : Int)
  | pledge (user : KelGroups.Key) (c : CollId) (v : Int)
  | acceptPledge (user : KelGroups.Key) (c : CollId)
  | refusePledge (user : KelGroups.Key) (c : CollId)
  | correctPledge (user : KelGroups.Key) (c : CollId) (v : Int)
  | closePurchase (c : CollId)
  | failPurchase (c : CollId)
  | openQuestion (questionId : KelGroups.Vote.QuestionId)
      (kind : KelGroups.Vote.QuestionKind)
  | cast (questionId : KelGroups.Vote.QuestionId)
      (ballot : KelGroups.Vote.Ballot)
  | renounce (questionId : KelGroups.Vote.QuestionId)
deriving DecidableEq, BEq, Repr

/-- Rejection identity of the integrated economic step. A single
constructor: the Option-shaped core collapses every guard into one
refusal. -/
inductive StepError where
  | rejected
deriving DecidableEq, BEq, Repr

/-- **The Reactivegas base proposal** (R62-07, T6221): a closed sum over
member departure and role change. There is no admission constructor and no
conversion from an unrestricted generic proposal, so voted admission is not
expressible — not refused at runtime, not representable at all.

Seeding `introduceMember` here does not compile: `Reactivegas.proposalMutation`
and `Reactivegas.proposalDigest` are exhaustive, wildcard-free eliminations
over this sum, and `KelGroups.BaseMutation`, the vocabulary the first lands in,
has no admission constructor to map onto.

`departure` rather than `removeMember`: the substrate effect is
`KelGroups.BaseMutation.removeMember`, and the frozen S62-B scanner reserves
that spelling in this file for the four retired `Event` constructors it exists
to keep out. The name is the proposal's own — what the group decides — and the
mutation it maps to is the substrate's. -/
inductive Proposal where
  | departure (key : KelGroups.Key)
  | changeRoles (key : KelGroups.Key) (roles : List KelGroups.Role)
deriving DecidableEq, BEq, Repr
