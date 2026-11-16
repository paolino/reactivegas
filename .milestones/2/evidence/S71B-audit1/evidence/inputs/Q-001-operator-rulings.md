# Q-001 — five operator rulings on #47, plus one milestone blocker

From: milestone owner, reactivegas milestone 2 (`ms2-reactivegas-kelgroups`)
To:   operator
Date: 2026-08-26
Source: `docs/en/design/kelgroups-mapping.md` lines 314–378 at commit 54c1543

None of these is resolvable by any seat in this chain. Q-1 is the only one that
blocks work; the other four block *contract freezes* in `paolino/kelgroups`.
The epic owner's audit of 54c1543 is still running — if it finds a sixth
question or restates one of these, this file is amended, not replaced.

Recommendations below are the document's own, restated in plain words with the
cost of each choice. I am not endorsing them; I am making them decidable.

---

## Q-1 — May a member pledge for themselves?  ⛔ BLOCKS CHILD 3

**The situation.** Your 2026-08-25 "sovereign members" ruling says every utente
holds their own key and pledges are self-service. The Lean machine merged in
#45 proves the opposite: `step_authorized` proves every successful step is
responsabile-authored, and `pledge`'s guard opens with `isResponsabile s a`.
**Two landed artifacts contradict each other today.** #47 cannot fix it — it is
a design record and may not edit Lean.

**Option A (doc's recommendation).** A minimal Lean amendment: for `pledge`
only, relax the guard to `isResponsabile s a || a == u`. The solvency,
uniqueness and conservation proofs survive untouched, because the monetary half
of the guard never mentions the author. Cost: one small ticket in this
milestone that must land *before* child 3 (the Haskell core) freezes.

**Option B.** Make every member an admin, so everyone is a responsabile. Cost:
collapses the role distinction the epic explicitly keeps, and changes the
voting base of every future proposal. The doc rejects this; so do I.

**Option C.** Withdraw the sovereign-members ruling; pledges stay
responsabile-authored. Cost: the browser client can no longer let a member act
alone, which is most of the point of giving members keys.

**What I need:** A, B, or C. If A, I also need a lane for the amendment ticket.

---

## Q-2 — Can you expel a member who owes or is owed money?

**The situation.** Legacy never removed ordinary users at all — only
responsabile revocation existed. kelgroups offers `RemoveMember` outright.
Removing someone holding a nonzero conto, or with live pledges inside other
people's collections, has no legacy-defined meaning.

**Recommendation.** Restrict expulsion to economically settled members (zero
conto, no live pledges): an operator-runbook requirement plus an explicit
`appOnBase` policy that treats a violating removal as a loud failure.
Cost: an admin cannot evict a debtor without first settling them.

**Alternative.** Define the economics of an unsettled removal (write off to the
cassa? socialise across members?). Cost: new semantics with no legacy anchor.

---

## Q-3 — What happens to open questions of a deposed admin?

**The situation.** Legacy cancelled an eliminated responsabile's open questions
at revocation. kelgroups' current fold leaves a removed member's proposals in
place, accumulating approvals against a threshold recomputed from current
admins. Reactivegas needs them resolved, not orphaned.

**Recommendation.** kelgroups#28/#30 specify cancellation-at-enactment: when a
proposer loses admin or membership, the proposal is removed and — if it was an
app-scoped purchase-approval question — delivered to the app as a **denial
verdict**, so the L2 refund path fires exactly as in legacy.
Cost: work lands in `paolino/kelgroups`, not here.

---

## Q-4 — How is a CESR key assigned its `UserId`?

**The situation.** The identity registry needs a deterministic key→`Nat` rule.

**Recommendation.** A monotonic counter in app state, advanced by `addUser`.
Replay reproduces it trivially and the numbers never reach the base layer.
Cost: none material. Freezing it in #28 stops ad-hoc digests leaking into
economic state.

---

## Q-5 — Can a promoter withdraw their own purchase-approval proposal?

**The situation.** Legacy let the promoter close their own assenso early
("rinuncia"), and that close ran the *negative* continuation — for a purchase,
full refunds.

**Recommendation.** #30 supports proposer-withdrawal delivering the same denial
payload as dissent. Until then clients express withdrawal as a campaign for
dissent — semantically equivalent, slower.
Cost: if #30 ships without it, withdrawal is a UX workaround forever.

---

## B-1 — The milestone has no artifact  ⛔ MILESTONE BLOCKER

Not from the document; found in my own intake sweep.

`paolino/reactivegas` has **no release pipeline, no releases, no version tags**.
Epic #43 promises the coordinator "released through the milestone line at epic
close". There is no milestone line. As it stands, this milestone can end with
every issue closed and nothing a stranger can obtain — the outcome test passes
only for people who already have the repository.

**Recommendation.** Authorize a standalone ticket lane for the release pipeline
(release-please manifest mode, per the house pattern), scheduled ahead of
children 3–7. Cost: one lane now.

**Alternative.** Rule publication explicitly out of milestone scope, on the
record. I will record it and stop raising it. What I will not do is leave it
implicit.

**What I need:** authorize the lane, or rule it out of scope.

---

## Q-6 — A cassiere's cassa on demotion  ✅ RULED BY OPERATOR 2026-08-26

**Ruling: guard `removeResponsabile` on `bal s.casse u == 0`.**

### The premise that decides it

`removeResponsabile` is **rotation, not sanction** — responsabili come and go.
It is not a tool for removing someone who misbehaves. No agent in this chain
had that premise; it is stated nowhere in `lean/`, in `state-machine.md`, or in
`kelgroups-mapping.md`, and every analysis done without it reaches the wrong
answer. **It belongs in the design record.**

### Sign convention, also unwritten

`casse` is what a cassiere owes the group: positive = they hold group money,
**negative = the group owes them**. `deposit` raises the receiving cassiere's
cassa (they took custody); `closePurchase` lowers the referente's (they paid
the supplier). Nowhere stated.

### What was rejected, and why

- **Sweep the cassa to the authorising responsabile** (this seat's first
  recommendation): rejected. The notes are physically still with the departing
  cassiere, so the sweep records a balance `a` can then `withdraw` against.
  Stranded-and-unreachable is bad; false-and-spendable is worse.
- **"Add a settlement path authorable from both sides"** (this seat's second
  recommendation): **withdrawn — it was a misreading.** `transferCassa` is
  authored by the receiver, so a positive cassa is *pulled* to zero by whoever
  takes custody, and a negative cassa is raised to zero by the departing
  cassiere authoring their own reimbursement. Both directions already work, and
  the asymmetry is correct: **you sign for what you receive.** Letting a third
  party push into someone's cassa would let them assign a custody liability
  without consent. The "hostile cassiere deadlock" was an artefact of assuming
  demotion is a sanction.

### Consequence

`cassa == 0` is a settlement condition, not a lock: you cannot leave the role
while the books between you and the group are open **in either direction**.
Lean change, so #47 cannot make it — it joins the Q-1 amendment ticket ahead of
child 3.

### Open sub-question routed back to the operator

Does the same principle settle **Q-2**? If leaving the *group* is also
come-and-go rather than expulsion, then "no departure while your conto is
nonzero or you hold live pledges" is the identical rule at the member layer —
which is exactly what Q-2's own recommendation proposes, arrived at from a
premise rather than as a policy patch. See Q-2 above.

## Q-2 + Q-6 extension — the cassa comune  (operator design, 2026-08-26)

**Ruled:** member departure is guarded on `bal s.conti u == 0`, the same
settlement rule as Q-6 one layer down. Departure is come-and-go, not expulsion.

**Designed:** departure is possible anyway. An unsettled departure moves the
leaver's balance into a **cassa comune**.

### Verified fact that reshapes the risk

`Predicates.lean:18` — `solvent s := (∀ u, bal s.conti u ≥ 0) ∧ (∀ pledge, 0 ≤
amount)`; `insolvent s := ∃ u ∈ s.users, bal s.conti u < 0`. **L7 quantifies
over `conti` only. It says nothing about `casse`.** A negative cassa comune
breaks no theorem, and negative casse are already normal (an active referente
runs negative after `closePurchase`).

Consequence for the design record: `kelgroups-mapping.md:271` says the
amendment "bans insolvency **outright**". It does not — it bans negative
*member accounts*. The group as a whole may go negative and always could. #28's
client contract needs that scope, since it decides which refusals a client must
render.

### Modelling that costs nothing

Reserve the cassa comune as a **pseudo-account holding both a `conto` and a
`cassa`**. Then `Σ casse − Σ conti − Σ escrow = 0` is untouched and
`conservation_preserved` (L6) survives verbatim — no change to the flagship
invariant, no new term.

### The three cases are not equally risky

- **Member leaves unsettled.** His conto moves to the comune's conto.
  Conservation trivial; nothing physical moves. **L7 already guarantees no
  member conto is negative**, so a departing member always brings a *positive*
  credit. **The member case can never make the comune negative.** The
  operator's stated risk is provably absent here.
- **Responsabile leaves with negative cassa** (group owes him). Moves to the
  comune; the comune carries the debt in one named place. *Sub-question:*
  should it stay attributed to him — a conto in his name — rather than being
  anonymised into the comune? Anonymising means the group quietly keeps what it
  owed a departed person.
- **Responsabile leaves with positive cassa** (he holds group cash and walks).
  **The only irreducible case.** Whatever is recorded, conservation is
  satisfied — the identity cannot distinguish "held in the common box" from
  "walked out the door". Three honest options:
  - *C1* comune `+X` — records a phantom asset it can later be spent against.
    This is the false-and-spendable failure that got the sweep rejected in Q-6,
    one level up.
  - *C2 (recommended)* name the loss: the comune carries the recognised
    shortfall so its net balance goes **negative by X**. Conservation holds, no
    phantom, and the group is told the exact amount it is short in one
    auditable number. This is the operator's "cassa comune negativa" — and it
    is a feature: the opposite of an invariant that reports health while cash
    is missing.
  - *C3* haircut every member's conto pro-rata. Honest, but members bear it,
    and it only stays L7-legal while `X ≤ Σ conti`.

### Design shape this implies

Two distinct departures, not one with a bypass: **settled departure** (guarded,
`conto == 0` / `cassa == 0`) is the normal path; **forced departure** is a
separate, explicitly named event that dumps into the comune. The guard is never
bypassed — a different, visible act is chosen, and it is attributable.

All of it is Lean; none of it is #47's to make. It joins the Q-1 amendment
ticket ahead of child 3. No base-layer surface is involved, so `kelgroups#28`
is unaffected — the cassa comune is pure app state.

### Operator ruling on the negative comune (2026-08-26)

**A negative cassa comune is the group's insolvency, and the machine must be
able to hold it.** Legal remedies against an absconding cassiere are
out-of-band. If someone runs off with their cassa, the group is insolvent —
that is a fact of the world, and refusing to record it does not undo it.

This forces a distinction the design record must carry:

- **L7 guards what the machine will *do*.** Bruno cannot pledge €30 he does
  not have; the debit is declined. Individual conti never go negative.
- **The cassa comune records what *happened*.** A cassiere absconding already
  occurred, off-ledger. There is nothing to decline.

Both are true and they do not conflict. But `kelgroups-mapping.md:271` says the
amendment "bans insolvency outright" and that there is "no 'recorded with
warning' middle ground" — and group insolvency is *exactly* a recorded state
with a warning, necessarily. §5 is scoped wrong at the group layer, and
`kelgroups#28` freezes against §5.

#### Contract finding — `GroupView` must expose the comune

If the group can be insolvent, a client must be able to show it. Nothing in the
interface contract names a way to read the comune balance. Per `COMMON.md`, an
interface surface this document fails to name is one #28 will not build — this
is that failure in view shape rather than event shape.

#### Open sub-question — who bears the shortfall?

`withdraw a u v` guards `bal s.conti u ≥ v` — the member's account — and
**never guards the cassiere's cassa**. So while the group is insolvent, members
may keep withdrawing against credits that are no longer fully backed, driving
casse deeper negative, first-come-first-served. The machine's current implicit
answer to "who bears the loss" is **"whoever withdraws last"**.

That is a product ruling. It should be made deliberately, not inherited from an
absent guard. Options: leave it (first-come-first-served), freeze withdrawals
while the comune is negative, or haircut conti pro-rata (C3 above) so the loss
is shared at the moment it is recognised.

## The premise underneath all of it (operator, 2026-08-26)

> "we cannot physically avoid those events"

**The machine may gate its own acts. It must never lie about the world.**

Every correction the operator made in this session is one application of it:
the cassa sweep was rejected because it pretended cash had moved; the
`cassa == 0` guard was accepted because demotion is an act the machine performs
and may therefore decline; the negative cassa comune was accepted because
absconding already happened and refusing to record it does not undo it.

### The distinction the AUTH matrix does not carry

`kelgroups-mapping.md:154` gives each `Step` event three columns — authorized
signer, additional guard, channel. There is no column for **what the signature
means**, and the events split cleanly:

- **Authorization** — the declaration *is* the act; refusing it refuses the
  act, and nothing in the world has happened yet. `openPurchase`, `pledge`,
  `acceptPledge`, `refusePledge`, `correctPledge`, `failPurchase`, and the five
  base enactments. A rejecting guard here is a real safeguard.
- **Attestation** — the physical act already happened; the declaration is
  testimony about it. `deposit` (the member already handed over the cash),
  `withdraw` (the notes are already in their hand), `transferCassa` (custody
  already changed — which is exactly why the *receiver* signs). A rejecting
  guard here is not a safeguard: it is a refusal to record reality, and the
  money is gone either way.
- **Mixed** — `closePurchase` both discharges escrow (authorization) and
  attests that the referente paid the supplier. It is also the event that
  routinely drives a referente's cassa negative.

The doc gets close for one row — the note at `:180` says deposits and
withdrawals "record that physical cash entered or left" — but it is a remark on
two rows, not a property of the interface, and #28 freezes against the matrix.

### The concrete consequence, and it is not theoretical

`withdraw` is an attestation row carrying an authorization guard
(`bal s.conti u ≥ v`), and §5 rules that the guard runs in `appStep` at
submission time, so a refused debit "is never appended to the KEL". If the cash
is handed over *before* the event is submitted, a real handover becomes
**unrecordable** — the exact "recorded with warning" middle ground §5 says does
not exist, appearing where it matters most.

The document does not state the ordering. Both are defensible — *declare then
pay* (guard is a real gate) or *pay then declare* (guard must be advisory, and
the client must show the ceiling before the cash moves). But #28 and every
client freeze against whichever is written, and right now nothing is.

**This is #47's to fix** — it is a scope and ordering statement about the
interface contract, not a Lean change. Routed to the epic.

## Who bears the shortfall — RULED (operator, 2026-08-26)

> "Until the cassa comune is negative the group should stall. Anyone can pay
> some money to a responsabile to fix the insolvency."

**While the group is insolvent it stalls. The cure is money paid in.** Neither
first-come-first-served nor a pro-rata haircut: the group stops, and someone
makes it whole.

### The stall has an exact shape — it is the taxonomy above

- **Authorization events stall.** The machine performs no new discretionary
  acts while the group is short: no `openPurchase`, `pledge`, `acceptPledge`,
  `correctPledge`, `closePurchase`.
- **Attestation events stay open.** You cannot refuse to record reality, and
  the cure must remain reachable. `deposit` above all — it *is* the cure.

That is a rule, not a list, and it falls straight out of the operator's own
premise. It is the first place the authorization/attestation split pays for
itself.

### It makes the `withdraw` ordering load-bearing

**You cannot stall an event that records something already done.** So the stall
is only implementable once §5 states whether withdrawal is *declare then pay*
(stallable, a real gate) or *pay then declare* (an attestation that must be
recorded even while insolvent, and the run on the bank is unstoppable by the
machine). The ordering question raised in NOTE-005 is not academic — this
ruling depends on it.

### The cure as described does not work yet — it needs an event that does not exist

`deposit a u v` does `conti u += v` **and** `casse a += v`. Both sides rise
together, so `Σ casse − Σ conti − Σ escrow` is unchanged and **the deficit is
untouched**: the payer has bought themselves credit, not healed the group.

Healing requires raising the group's cash **without creating member credit** —
a *donation*, which the machine has no event for.

And that is the elegant part: **the donation and the cassa comune are duals.**
A donation is exactly an event that does not balance, and the comune is exactly
the account that absorbs the imbalance. One mechanism, two faces. Add the
event, and the cure the operator described becomes expressible.

### Modelling trap — the comune must not be a pseudo-user

`solvent` is `∀ u : UserId, bal s.conti u ≥ 0` — quantified over **all**
`UserId`. Model the cassa comune as a pseudo-user holding a conto and **L7
silently forbids the very state the design needs to represent.** It must be its
own `State` field, and then L6's `conservation` statement must be restated to
include it.

**Consequence for scope:** this amendment touches both flagship theorems —
`conservation_preserved` (L6) and `solvent_preserved` (L7) must be re-proved.
Q-1 was four edits. This is a different size, and child 3 waits behind it.
That sequencing is the milestone's problem and it is now on the record.

## The symmetry, and the one thing it exposes (operator, 2026-08-26)

> "leaving the group as a member with some credit or as a cassiere with some
> credit (cassa negativa) will help present or future insolvency"

The comune is one signed number that nets both directions of departure:

- **Leave owing the group** — cassiere walks with a positive cassa. The comune
  absorbs a **loss**. This is theft, and it causes insolvency.
- **Leave being owed by the group** — member with a positive conto, or cassiere
  with a negative cassa. The comune absorbs a **gain**. This offsets present or
  future insolvency.

So the comune is the group's ledger of who left it better or worse off, and it
nets. That also means the *donation* event identified above may not be a new
mechanism at all: **forfeiting a credit on departure is structurally a
donation** — it raises the group's backing without creating member credit.
Voluntary donation and involuntary forfeit feed the same number.

### RESOLVED — the comune is an anonymous pool (operator, 2026-08-26)

"conterrà i suoi soldi" was wording, not attribution: **the money does not
carry his name. It is just a cassa.** A departure extinguishes the claim into a
pooled box; it does not park a debt owed to the leaver.

The apparent contradiction with "leaving with credit helps insolvency"
therefore does not exist — extinguishing is what was meant throughout, and it
is what makes the offset real. The park-with-expiry option this seat
recommended is **withdrawn**; no claim survives departure and nothing expires.

### Why anonymity is safe here

The balance is anonymous; **the event that produced it is not.** The forced
departure is a named act on the KEL forever — who left, when, how much moved
into the comune, signed. Accountability lives in the log, not in the balance.
That is the same principle as everything else in this design: the machine
records what happened and does not pretend otherwise.

## The design as ruled — consolidated

1. **Settled departure is the normal path**, guarded: member `conto == 0`,
   responsabile `cassa == 0`. You do not leave with the books open.
2. **Unsettled departure is possible anyway**, as a separate named act, and
   moves the balance into the **cassa comune** — an anonymous pool. Not a
   bypass of the guard: a different, visible, signed choice.
3. **The comune is one signed number.** It gains from people who leave owed by
   the group, loses from people who leave owing it.
4. **A negative comune is the group's insolvency.** Representable by design.
   L7 constrains member conti only and never touched casse, so no theorem
   forbids it — but the comune must be its own `State` field, since `solvent`'s
   `∀ u : UserId` would silently forbid a pseudo-user carrying it.
5. **While the comune is negative the group stalls**: authorization events
   stop, attestation events stay open — the cure must stay reachable and
   reality must stay recordable.
6. **The cure is money paid in**, which needs a donation-shaped event that
   raises backing without creating credit. Forfeit-on-departure is the same
   mechanism involuntarily, so it is one event with two faces.

### Still open, and it is #47's not Lean's

**The `withdraw` ordering.** *Declare then pay* makes withdrawal stallable;
*pay then declare* makes it an attestation the machine must record even while
insolvent, and the run on the bank cannot be stopped. Point 5 is not
implementable until §5 says which. See NOTE-005.

## Stall is total (operator, 2026-08-26)

> "Stall is stall. No one can exit until it's unstalled and only introducing
> money in the cassa comune will move the needle. And sadly open orders now can
> only **fail** (or stay open)."

The stall is total. No departures. The single act that lifts it is money paid
into the comune. Open collections may be **failed**, never closed.

### This seat had the risk inverted — correction

An earlier draft of this section argued the opposite (close safe, fail
dangerous) by reasoning about *claims* rather than *cash*. That was wrong, and
the fold says so plainly:

- `closePurchase` — `casse referente −= S`. **Real money leaves the group** and
  goes to the supplier. While insolvent that cash may not exist, and spending
  it deepens the shortfall. **It is the one event that spends.**
- `failPurchase` / `denyPermission` / `refusePledge` — escrow returns to conti.
  **No cash moves at all.** The escrowed money was already the group's
  liability to those members; whether it sits in escrow or in their conto, the
  group owes exactly the same. Total liability `Σ conti + Σ escrow` is
  unchanged. And the members still cannot withdraw it, because withdrawals are
  stalled.

So refunds are cash-neutral and harmless during a stall; **closing is the
outflow.** The operator's rule is the correct one.

### One mechanical consequence — fail alone is not enough

`failPurchase` demands `col.pending.isEmpty`, exactly as `closePurchase` does.
Clearing a pending pledge takes `acceptPledge` or `refusePledge`. If both are
stalled, **a collection holding even one pending pledge cannot be failed
either**, and "fail or stay open" collapses to "stay open" for precisely the
collections most likely to be stuck.

`refusePledge` is a refund and therefore cash-neutral by the reasoning above —
permitting it during a stall costs the group nothing and makes the ruling
actually reachable. `acceptPledge` is money-neutral too (escrow is the sum of
`accepted ++ pending`), but it only serves closing, which is banned. So:
**permit `refusePledge`; there is no need to permit `acceptPledge`.**

### Retraction — the `appOnBase` gap does not exist

An earlier draft claimed the stall could be bypassed by the base layer, because
`appOnBase :: GroupView -> BaseOutcome -> s -> s` returns `s` and cannot reject
a `denyPermission` enactment. **Withdrawn.** Under the corrected analysis
`denyPermission` is a refund, hence cash-neutral, hence safe to apply during a
stall. `grantPermission` only sets a flag; the close it enables is blocked in
`appStep`, which the app controls. No deferred-outcome queue is needed and #28
needs no change on this account.

### Note on how this went wrong three times

Every error this seat made in this session came from reasoning about claims,
liabilities and invariants instead of about **physical cash**: the cassa sweep,
the "hostile cassiere deadlock", and now the close/fail inversion. The
operator's frame — where is the money, who is holding it, what actually leaves
the room — was correct each time. That is the same premise as
"the machine may gate its own acts but must never lie about the world", applied
to analysis rather than to design. Worth stating in the design record: this
machine is about cash, and conservation-style reasoning will mislead a reader
who forgets it.

### Correction 2 — `closePurchase` burns escrow, it does not remove cash

> operator: "closePurchase is not removing cash, is burning escrow"

The fold, read correctly: `casse referente −= S` **paired with the collection
being removed** (`escrow −= S`). Both sides of the pair are already-committed
money. The members' conti were debited at pledge time; that S was earmarked and
non-withdrawable from the moment it was pledged. Closing converts committed
money into goods and discharges the referente's obligation by the same S.
**Nothing that was available to the group is consumed.**

Contrast `withdraw`: `conti u −= v` paired with `casse a −= v`. Here the pair
is *uncommitted* — a member's liquid credit, backed by cash the group could
otherwise use, walks out the door.

So the correct distinction is **committed vs available**, not cash-moving vs
cash-neutral, and this seat's "exactly two events remove cash" generalization
was wrong. `withdraw` is the only event that consumes uncommitted backing.

### Why closing is blocked during a stall (operator, 2026-08-26)

Two reasons, both real:

1. **It protects the referente.** Closing obliges them to pay the supplier S in
   real money. With the group short, their cassa goes negative and they become
   a creditor of a group that cannot repay them. The stall stops the group
   spending a referente's goodwill.
2. **Fairness — escrow must not be an escape route.** A close during a stall
   hands goods to the members who happened to be pledged in, while everyone
   else's money stays frozen. And because `pledge` is itself stalled, **no one
   can buy into the purchase**: the beneficiary set is sealed at the moment the
   stall begins. Closing would systematically reward whoever was already in.

### The stall stated as a principle, not a list

Both reasons are the same rule seen twice, and it is worth recording as the
rule rather than as an event table, because a table invites a reader to argue
about individual rows:

> **While the group is short, no member may improve their position relative to
> another. Only acts that help everyone, or nobody, are permitted.**

Every row follows from it:

- **donation into the comune** — helps everyone; it is the cure, and the only
  thing that lifts the stall;
- **`failPurchase`** — returns escrow to conti that are themselves frozen.
  Everyone stays equally stuck. Permitted;
- **`refusePledge`** — same: a pending pledge returns to a frozen conto, no
  relative gain. Permitted, and necessary, because `failPurchase` demands
  `col.pending.isEmpty` and would otherwise be unreachable for exactly the
  collections most likely to be stuck;
- **`closePurchase`** — goods in hand for a sealed subset. Blocked;
- **`withdraw`** — cash in hand for one member. Blocked;
- **`pledge` / `acceptPledge`** — move a member toward the privileged path.
  Blocked;
- **departures** — leaving is improving your position relative to those who
  stay. Blocked, as ruled.

This formulation is also **checkable**: it states what a gate should assert
about the stalled machine, rather than enumerating cases a future reader may
extend wrongly.

---

## Vote-machine rulings (operator, 2026-08-27)

Sourced from legacy `Eventi/Anagrafe.hs`, not invented.

### V-1 — Who votes: responsabili

> "responsabili votes, those are the one that can do damage, so they vote"

**The franchise tracks the capacity to do damage.** This is why sovereign
members may pledge for themselves (Q-1) but do not vote: pledging risks only
your own conto; governance risks everyone's. Matches legacy (`maggioranza`
counts `Responsabili`) and slice 1 (counts admins = responsabili).

### V-2 — Threshold: arbitrary, but two consequences are not

Operator: *"seems irrelevant, or arbitrary"*. The arithmetic is free. Two
behaviours it produces are still decisions:

- `soglia = (n+1) div 2` gives **2 of 4 — a tie passes**;
- `i == 0` sets the threshold to **0, so everything passes instantly**.

Recorded as open. Neither is arbitrary; both need a deliberate answer.

### V-3 — Recompute the verdict on every state change

> "Every change is correct"

Legacy recomputes only when a vote arrives, while the threshold depends on the
current responsabili — so legacy can sit with a question already past
threshold and not know it. Fixed: the verdict is re-evaluated whenever
anything it depends on changes.

**Consequence to note, not hide: a question can pass because people left.**
Fewer responsabili means a lower threshold, so existing assents can carry it
with no new vote. That is the direct flip side of V-1 — if the damage-capable
shrink, the remaining voices weigh more.

### V-4 — Fix the double count

Legacy's "already voted" guard is commented out, and assenting does
`ps' = r : filter (/= r) ps` while leaving `ns` untouched. A responsabile who
dissents then assents lands in **both** lists and is counted on both sides.
An oversight, not a semantic. **A responsabile appears in exactly one list.**

### V-5 — Proposer leaves or renounces → the question closes

> "If the responsabile leaves or renounce it gets closed"

Resolves **Q-3** (pending questions of a deposed admin) and **Q-5**
(proposer withdrawal / legacy `rinuncia`, `EventoFallimentoAssenso`).

**Closed means the NEGATIVE continuation runs, not silent deletion.** This is
forced, not a preference: a purchase-approval question holds members' money in
escrow, and legacy's `chiudimale` refunds. Deleting the question silently would
leave escrow held against a question that no longer exists.

### V-6 — No vote to admit a member

> operator: "no"

Legacy `NuovoUtente` (`Anagrafe.hs:170`) adds the member directly — no assent.
kelgroups makes `introduceMember` a **proposal**. Legacy wins.

**This is a requirement on `kelgroups#28`**: the substrate must admit a member
without a vote. Currently it cannot.

### Still open from this thread

`Indecidibile` questions never expire — legacy has no timeout and, after V-5,
the only exits are a verdict or the proposer leaving. Is "open until the group
ends" acceptable, or does an undecided question need an expiry?

### V-7 — No expiry. "They can always vote no."

> operator: "They can always vote no?"

Correct, and it closes the last open item from this thread. `maggioranza`
applies the **same** `soglia` to both sides — `length ns >= soglia` closes the
question `Negativo`. No timer is needed.

**V-4 is what makes this true.** Under legacy's double-count bug a responsabile
who had assented landed in both lists when switching, so the "just vote no"
escape was quietly broken. One-list-per-responsabile restores it.

Precision worth keeping: what leaves a question open is **absence, not
obstruction**. Nobody can block a close; a question hangs when responsabili
simply do not vote. Three natural exits exist and none is a timer:

1. a verdict — either side reaching `soglia`;
2. the group shrinking — V-3 recomputes, a lower threshold may carry stale
   assents;
3. the proposer leaving or renouncing — V-5, running the negative continuation.

**This thread is now fully ruled: V-1 … V-7.** Remaining from the wider queue:
V-2's two consequences (tie passes, `i == 0` passes everything), Q-2 expulsion,
Q-4 key→UserId, comune dust at closure, app-vs-base closure lifecycle, and the
closure event's constructor and authorization.
