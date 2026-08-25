import Reactivegas.State

/-!
# The rejecting step function

One total function; `none` means the event is rejected. Guards mirror
the legacy `fallimento` checks in `Eventi/`: every declaration must be
authored by an elected responsabile (AUTH), referente-only operations
check the referente (resolve/close/fail/correct), pledges are unique per
collection (L8), and positive closure needs permission and zero pending
pledges (L2/L4).
-/

/-- Rejection guard: `demand b` succeeds exactly when `b` holds. -/
def demand (b : Bool) : Option Unit := if b then some () else none

/-- Look up an open collection by id. -/
def findCollection (s : State) (c : CollId) : Option Collection :=
  s.collections.find? (fun x => x.id == c)

/-- Is `u` currently an elected responsabile? -/
def isResponsabile (s : State) (u : UserId) : Bool := s.responsabili.contains u

/-- The rejecting transition function of the economic machine. -/
def step (s : State) (e : Event) : Option State :=
  match e with
  | .addUser a u =>
    if isResponsabile s a && !s.users.contains u then
      some { s with users := s.users ++ [u] }
    else none
  | .electResponsabile a u =>
    if isResponsabile s a && s.users.contains u && !isResponsabile s u then
      some { s with responsabili := u :: s.responsabili }
    else none
  | .removeResponsabile a u =>
    if isResponsabile s a && isResponsabile s u then
      (let (rest, ps) := stripCollections u s.collections;
       some { s with
         responsabili := s.responsabili.erase u,
         conti := refundAll s.conti ps, collections := rest })
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
    if isResponsabile s a && s.users.contains u && a != u then
      some { s with conti := bump s.conti u v, casse := bump s.casse a v }
    else none
  | .withdraw a u v =>
    if isResponsabile s a && s.users.contains u && a != u then
      some { s with conti := bump s.conti u (-v), casse := bump s.casse a (-v) }
    else none
  | .transferCassa a f v =>
    if isResponsabile s a && isResponsabile s f && a != f && v > 0 then
      some { s with casse := bump (bump s.casse f (-v)) a v }
    else none
  | .pledge a u c v => do
    let (col, rest) ← pullCollection c s.collections
    demand (isResponsabile s a && s.users.contains u
      && !(col.accepted.any (fun p => p.user == u))
      && !(col.pending.any (fun p => p.user == u)))
    pure { s with
      conti := bump s.conti u (-v),
      collections := { col with pending := ⟨u, v⟩ :: col.pending } :: rest }
  | .acceptPledge a u c => do
    let (col, rest) ← pullCollection c s.collections
    let (v, pend') ← splitUser u col.pending
    demand (isResponsabile s a && col.referente == a)
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
    demand (isResponsabile s a && col.referente == a)
    pure { s with
      conti := bump s.conti u (v - v'),
      collections := { col with accepted := ⟨u, v'⟩ :: acc' } :: rest }
  | .closePurchase a c => do
    let (col, rest) ← pullCollection c s.collections
    demand (isResponsabile s a && col.referente == a && col.permitted
      && col.pending.isEmpty)
    pure { s with
      casse := bump s.casse col.referente (-(sumPledges col.accepted)),
      collections := rest }
  | .failPurchase a c => do
    let (col, rest) ← pullCollection c s.collections
    demand (isResponsabile s a && col.referente == a && col.pending.isEmpty)
    pure { s with
      conti := refundAll s.conti (col.accepted ++ col.pending),
      collections := rest }
