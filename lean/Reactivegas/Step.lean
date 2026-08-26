import Reactivegas.State

/-!
# The rejecting step function

One total function; `none` means the event is rejected. Guards mirror
the legacy `fallimento` checks in `Eventi/`: every declaration must be
authored by an elected responsabile (AUTH), referente-only operations
check the referente (resolve/close/fail/correct), pledges are unique per
collection (L8), and positive closure needs permission and zero pending
pledges (L2/L4). Beyond the legacy checks, debits that would take an
account below zero are rejected and pledge amounts must stay positive,
so refunds can never push anyone under (L7 solvency).

Economic amendment (issue #48): the comune is a reserved non-member
conto (`comuneId`) inside `conti`; donations attest cash arrival into
`casse` + comune; voted backdonations move exact equal shares `w` out of
the comune conto with no division; departures of either role move the
leaver's own claim into the comune conto with no balance gate — a zero
balance is a no-op movement, not a separate form; the two departure
constructors are role-disjoint, so an ordinary departure cannot bypass
responsabile cleanup; and a negative comune
conto (`stalled`) refuses spending and departure events until a
donation cures it.
-/

/-- Rejection guard: `demand b` succeeds exactly when `b` holds. -/
def demand (b : Bool) : Option Unit := if b then some () else none

/-- Look up an open collection by id. -/
def findCollection (s : State) (c : CollId) : Option Collection :=
  s.collections.find? (fun x => x.id == c)

/-- Is `u` currently an elected responsabile? -/
def isResponsabile (s : State) (u : UserId) : Bool := s.responsabili.contains u

/-!
### Provisional boundary: the backdonation vote authorization

The vote encoding for the voted equal-share backdonation is an
app-scoped proposal type **owned by issue #47** (open question Q-007:
whether Reactivegas closure affects the kelgroups group, and with it the
second app-scoped proposal type and its one-to-one interface event). No
faithful encoding exists in this model yet, so this named boundary is
deliberately provisional: its body is proof debt for the next pass, it
selects **no** true/false vote policy, and the `backdonate` step case routes its
enacted-vote condition through this name. A later pass must replace the
body with the reconciled #47 proposal encoding.
-/
def backdonateAuthorized (s : State) (w : Int) : Bool := sorry

/-- The rejecting transition function of the economic machine. -/
def step (s : State) (e : Event) : Option State :=
  match e with
  | .addUser a u =>
    if isResponsabile s a && !s.users.contains u && u != comuneId then
      some { s with users := s.users ++ [u] }
    else none
  | .electResponsabile a u =>
    if isResponsabile s a && s.users.contains u && !isResponsabile s u then
      some { s with responsabili := u :: s.responsabili }
    else none
  | .removeResponsabile a u =>
    if isResponsabile s a && isResponsabile s u && !(decide (stalled s)) then
      (let (rest, ps) := stripCollections u s.collections;
       some { s with
         responsabili := s.responsabili.erase u,
         conti := bump (refundAll s.conti ps) comuneId (-(bal s.casse u)),
         casse := bump s.casse u (-(bal s.casse u)),
         collections := rest })
    else none
  | .removeMember a u =>
    if isResponsabile s a && s.users.contains u && !(isResponsabile s u)
        && !(decide (stalled s)) then
      some { s with
        users := s.users.erase u,
        conti := bump (bump s.conti u (-(bal s.conti u))) comuneId (bal s.conti u) }
    else none
  | .openPurchase a c =>
    if isResponsabile s a && !(s.collections.any (fun x => x.id == c)) then
      some { s with collections := ⟨c, a, false, [], []⟩ :: s.collections }
    else none
  | .grantPermission a c => do
    let (col, rest) ← pullCollection c s.collections
    demand (isResponsabile s a)
    pure { s with collections := { col with permitted := true } :: rest }
  | .denyPermission a c => do
    let (col, rest) ← pullCollection c s.collections
    demand (isResponsabile s a)
    pure { s with
      conti := refundAll s.conti (col.accepted ++ col.pending),
      collections := rest }
  | .deposit a u v =>
    if isResponsabile s a && s.users.contains u && a != u && decide (0 ≤ v) then
      some { s with conti := bump s.conti u v, casse := bump s.casse a v }
    else none
  | .withdraw a u v =>
    if isResponsabile s a && s.users.contains u && a != u &&
        decide (bal s.conti u ≥ v) && !(decide (stalled s)) then
      some { s with conti := bump s.conti u (-v), casse := bump s.casse a (-v) }
    else none
  | .transferCassa a f v =>
    if isResponsabile s a && isResponsabile s f && a != f && v > 0 then
      some { s with casse := bump (bump s.casse f (-v)) a v }
    else none
  | .donate a v =>
    if isResponsabile s a && decide (0 < v) then
      some { s with
        casse := bump s.casse a v,
        conti := bump s.conti comuneId v }
    else none
  | .backdonate a w =>
    let n : Int := s.users.length
    if isResponsabile s a && decide (0 < w)
        && decide (comuneBal s ≥ n * w) && backdonateAuthorized s w then
      some { s with
        conti := s.users.foldl (fun acc u => bump acc u w)
          (bump s.conti comuneId (-(n * w))) }
    else none
  | .pledge a u c v => do
    let (col, rest) ← pullCollection c s.collections
    demand (isResponsabile s a && s.users.contains u
      && !(col.accepted.any (fun p => p.user == u))
      && !(col.pending.any (fun p => p.user == u))
      && decide (0 < v) && decide (bal s.conti u ≥ v)
      && !(decide (stalled s)))
    pure { s with
      conti := bump s.conti u (-v),
      collections := { col with pending := ⟨u, v⟩ :: col.pending } :: rest }
  | .acceptPledge a u c => do
    let (col, rest) ← pullCollection c s.collections
    let (v, pend') ← splitUser u col.pending
    demand (isResponsabile s a && col.referente == a && !(decide (stalled s)))
    pure { s with collections :=
      { col with pending := pend', accepted := ⟨u, v⟩ :: col.accepted } :: rest }
  | .refusePledge a u c => do
    let (col, rest) ← pullCollection c s.collections
    let (v, pend') ← splitUser u col.pending
    demand (isResponsabile s a && col.referente == a)
    pure { s with
      conti := bump s.conti u v,
      collections := { col with pending := pend' } :: rest }
  | .correctPledge a u c v' => do
    let (col, rest) ← pullCollection c s.collections
    let (v, acc') ← splitUser u col.accepted
    demand (isResponsabile s a && col.referente == a
      && decide (0 ≤ v') && decide (bal s.conti u + (v - v') ≥ 0))
    pure { s with
      conti := bump s.conti u (v - v'),
      collections := { col with accepted := ⟨u, v'⟩ :: acc' } :: rest }
  | .closePurchase a c => do
    let (col, rest) ← pullCollection c s.collections
    demand (isResponsabile s a && col.referente == a && col.permitted
      && col.pending.isEmpty && !(decide (stalled s)))
    pure { s with
      casse := bump s.casse col.referente (-(sumPledges col.accepted)),
      collections := rest }
  | .failPurchase a c => do
    let (col, rest) ← pullCollection c s.collections
    demand (isResponsabile s a && col.referente == a && col.pending.isEmpty)
    pure { s with
      conti := refundAll s.conti (col.accepted ++ col.pending),
      collections := rest }
