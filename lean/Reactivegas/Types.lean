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

/-- **Reserved comune account id** (issue #48): the common fund lives
at this `UserId` inside `conti` — never as a standalone `State` field.
It is never a member (`comune_not_a_member`), and it can never be
admitted: `addUser` refuses it and `Reach.boot` requires `r ≠ comuneId`.
Id `0` is simply reserved; no member may ever hold it. -/
abbrev comuneId : UserId := 0

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
  /-- Departure of an ordinary member (issue #48): their own claim on the
group moves to the comune conto and their conto is zeroed. Exactly one
departure constructor per role, and the two are role-disjoint: a target
who is still a responsabile must leave via `removeResponsabile`, so an
ordinary departure can never bypass responsabile cleanup. A departure
is never rejected for a nonzero balance — a zero balance merely makes
the movement a no-op. -/
  | removeMember (author target : UserId)
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
  /-- **Attested donation** (issue #48): the cash arrived first. Raises the
author's cassa and the comune conto together by positive `v`; no second
party, no member credit. Permitted while stalled (it is the sole cure)
and while solvent. -/
  | donate (author : UserId) (v : Int)
  /-- **Voted equal-share backdonation** (issue #48): parameterized by the
per-member share `w`. Every current member's conto rises by exactly `w`
and the comune conto falls by exactly `n*w` where `n` is the number of
current members — no division, no remainder. Refused by affordability
alone while stalled. The enacted-vote encoding is provisional
(`backdonateAuthorized`, Q-007 / #47). -/
  | backdonate (author : UserId) (w : Int)
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
