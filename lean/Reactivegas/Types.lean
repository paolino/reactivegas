import KelGroups.Types

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
field, and never as membership. `GroupView.isMember comuneId` is false
by construction of the S62-A fixtures; direct admission of this key is
S62-B (`validateDirectAdmission`). -/
abbrev comuneId : KelGroups.Key := "comune"

/-- A single pledge of money by a user inside a collection. Amounts are
plain integers; the legacy `Euro`/`DEuro` types are integer-valued. -/
structure Pledge where
  user : KelGroups.Key
  amount : Int
deriving DecidableEq, BEq, Repr

/-
Legacy 18-constructor vocabulary, retained for `Composition.route` /
`voteDerived` (NOTE-001). The four membership/role constructors are not
routed through the new integrated production path; their removal is
T6222 in S62-B. Every identity is `KelGroups.Key`.
-/
inductive Event where
  /-- Interface event: recognize a new user. Isolated; not production. -/
  | addUser (author target : KelGroups.Key)
  /-- Interface event: enact the election of a new responsabile. Isolated. -/
  | electResponsabile (author target : KelGroups.Key)
  /-- Interface event: revoke a responsabile; cancels their open questions. -/
  | removeResponsabile (author target : KelGroups.Key)
  /-- Departure of an ordinary member. Isolated; not production. -/
  | removeMember (author target : KelGroups.Key)
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
deriving DecidableEq, BEq, Repr

/-- Rejection identity of the integrated economic step. A single
constructor: the Option-shaped core collapses every guard into one
refusal. -/
inductive StepError where
  | rejected
deriving DecidableEq, BEq, Repr
