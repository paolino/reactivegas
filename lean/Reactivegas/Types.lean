/-!
# Reactivegas domain types

Abstract identifiers, money, and the event vocabulary of the economic
machine extracted from the legacy `Eventi/` reactors (issue #45).

Membership and majority-voting mechanics are *interface events* here:
the group layer (kelgroups) supplies their outcomes.
-/

/-- A participant of the group. -/
abbrev UserId := Nat

/-- Identifier of an open economic collection (a purchase). -/
abbrev CollId := Nat

/-- A single pledge of money by a user inside a collection. Amounts are
plain integers; the legacy `Euro`/`DEuro` types are integer-valued. -/
structure Pledge where
  user : UserId
  amount : Int
deriving DecidableEq, Repr

/-
Events of the machine. Every declaration carries an authoring
responsabile (legacy: users do not sign, responsabili declare on their
behalf — AUTH).
-/
inductive Event where
  /-- Interface event: recognize a new user. -/
  | addUser (author target : UserId)
  /-- Interface event: enact the election of a new responsabile. -/
  | electResponsabile (author target : UserId)
  /-- Interface event: revoke a responsabile; cancels their open questions. -/
  | removeResponsabile (author target : UserId)
  /-- Open a purchase: opens the pledge collection plus a majority question. -/
  | openPurchase (author : UserId) (c : CollId)
  /-- Interface event: the group assented; closure permission granted. -/
  | grantPermission (author : UserId) (c : CollId)
  /-- Interface event: the group dissented; purchase fails with full refunds. -/
  | denyPermission (author : UserId) (c : CollId)
  /-- Deposit: move user credit and the acting cashier's cassa together. -/
  | deposit (author user : UserId) (v : Int)
  /-- Withdrawal: symmetric to a deposit. -/
  | withdraw (author user : UserId) (v : Int)
  /-- Cassa-to-cassa transfer between two responsabili. -/
  | transferCassa (author from_ : UserId) (v : Int)
  /-- Pledge: debits the pledger's credit immediately into escrow. -/
  | pledge (author user : UserId) (c : CollId) (v : Int)
  /-- Referente consent: move the pledge from pending to accepted. -/
  | acceptPledge (author user : UserId) (c : CollId)
  /-- Referente refusal: refund the pending pledge. -/
  | refusePledge (author user : UserId) (c : CollId)
  /-- Referente-only correction of an accepted pledge: settle the difference. -/
  | correctPledge (author user : UserId) (c : CollId) (v : Int)
  /-- Positive closure: spends the referente's cassa by the collected total. -/
  | closePurchase (author : UserId) (c : CollId)
  /-- Referente-initiated failure: refunds every pledge. -/
  | failPurchase (author : UserId) (c : CollId)
deriving DecidableEq, Repr
