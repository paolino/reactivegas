# The reactivegas state machine

Current record for the merged model. Pin:
`4a6cd87fcbc3e4a536bbc9f240f5efe5704022af` (S1 #79 landed, 2026-09-05,
lane #71; re-pinned from `e6c59242`, whose Lean content this page
re-derives except where S1 is noted).
It supersedes the pre-merge record, which described fifteen events, a
`users`/`responsabili` State table, and a two-argument step — none of which
match the merged Lean. Where this page states a ruled-but-unmerged change,
it says so in the current-vs-ruled table at the end; implemented-behavior
claims wait for their merge.

The sources live under
[`lean/Reactivegas/`](https://github.com/paolino/reactivegas/tree/main/lean/Reactivegas)
and build with `lake build` (see `just lean`).

## How to cite this page

Every declaration-like claim carries a marker naming the Lean declaration at the pin above (for example, the marker for the State structure). The slice
gate discovers every such marker and requires it to resolve against the
pinned Lean; an unknown marker fails the build. Marker names use the
module path for readability (`lean:KelGroups.GroupView`), but the compiled
constants for the app payload are root `State` (`lean:State`) and `Event` (`lean:Event`) — there is no
`Reactivegas.State` or `Reactivegas.Event` constant (CLOSURE-MAP C7). Where
a name below is root, the marker says the root name.

Claims on this page are marked **law** (universally quantified theorem:
states that for every execution, some property holds) or **finite witness**
(an executable oracle whose whole statement is `check… = true` over a fixed
corpus or exhibit). The two read nothing alike in Lean and must not be
confused in prose; see “Laws versus finite witnesses”.

## State

The whole economic payload (root `lean:State`) is three association lists
plus the vote payload — not four association lists, and there is no
payload-local member or admin copy:

| field | contents |
| --- | --- |
| `conti` | per-key credit balances (`lean:bal` reads them; absent key reads zero) |
| `casse` | cash boxes, one entry per key (`lean:bump` appends/adjusts entries) |
| `collections` | open purchase collections holding escrow |
| `votes` | membership-free vote payload (`lean:KelGroups.Vote.VoteState`) |

Membership and roles live only in `KelGroups.GroupState.members` and are
read through the immutable `lean:KelGroups.GroupView`: a fold handed a view
can read membership and roles and can produce nothing but app payload. The
`lean:GroupView` argument on the transition is the whole point of #62 —
membership is read, never carried.

A collection (`lean:Collection`) carries its id, a referente (responsible
key), a closure-permission flag, and two pledge lists: `accepted` (money
already moved) and `pending` (offers not yet accepted). A pledge
(`lean:Pledge`) is a bare `(user, amount)` pair — no product, no order, no
quantity (see “Voci non-goal”). Balances are plain `Int`; money held in
escrow by a collection is the sum of *both* pledge lists (`lean:escrowOf`),
and across all collections (`lean:escrowSum`). Every collection-modifying
event touches the state through `lean:pullCollection`; refunds run through
`lean:refundAll`, one bump per pledge.

Three balance facts that are easy to get backwards:

- A zero balance for an absent key is read via `lean:bal`, but an accepted zero deposit stores it: from empty, a zero deposit stores `(u, 0)` in `conti` and `(a, 0)` in `casse`. `lean:absorbConto` is a bump, not
  an erase: a departed member's `conti` entry survives at zero. Symmetrically,
  admission creates no `conti`/`casse` entry at all, and the zero a reader
  sees is what `lean:bal` returns for an absent key.
- The common fund is the `conti` balance at the reserved
  `lean:comuneId` — a reserved non-member account inside `conti`, never a
  standalone `State` field (`lean:comuneBal`). The boot state (the `lean:State` empty value) carries no accounts, no collections, no questions.
- **Stall.** `lean:stalled` holds when the comune conto goes negative. While
  stalled the machine refuses departures, admin loss, `closePurchase`,
  `withdraw`, `pledge` and `acceptPledge`; `donate` stays reachable because
  it is the cure, as do `failPurchase` and `refusePledge`. `backdonate` is
  refused by its own affordability guard, with no separate stall condition.

## Events

Root `lean:Event` has **14 event constructors**: `openPurchase`,
`grantPermission`, `denyPermission`, `deposit`, `withdraw`, `transferCassa`,
`donate`, `backdonate`, `pledge`, `acceptPledge`, `refusePledge`,
`correctPledge`, `closePurchase`, `failPurchase`. Every law below quantifies
over these fourteen.

The integrated surface `lean:AppEvent` has seventeen: the same fourteen
without an author field (the signer arrives from the fold) plus
`openQuestion`, `cast` and `renounce`. The application's own restricted
proposal (`lean:Reactivegas/Types.lean:Proposal`) is `departure` / `changeRoles` — there is no
admission constructor. Do not confuse the retired names with the live
substrate vocabulary: `KelGroups.Proposal.removeMember` and
`KelGroups.BaseMutation.removeMember` are live and current; what is gone is
`Reactivegas.Event.removeMember`, together with `addUser`,
`electResponsabile` and `removeResponsabile`.

## Step

The machine is a rejecting step function taking the canonical view, the
signer, and an explicit authorization callback
(`lean:BackdonateAuth`, `State → Int → Bool`, so no definition chooses the
product policy):

```lean
def step      (view : GroupView) (s : State) (signer : Key) (app : AppEvent)
              (auth : BackdonateAuth) : Option State
def stepEvent (view : GroupView) (s : State) (e : Event)
              (auth : BackdonateAuth) : Option State
```

`lean:stepEvent` delegates all fourteen economic constructors to `lean:step`.
The three vote constructors run elsewhere: bare `step` returns none for
`openQuestion`/`cast`/`renounce`; inside `lean:appFold` they run
`lean:voteApply` (see “Vote-lifecycle limits”).

Authorization today, per event, at current source (`lean:step`):

| event | authorized signer + guard |
| --- | --- |
| `openPurchase` | responsabile signer; collection id fresh |
| `grantPermission` / `denyPermission` | `lean:pullCollection` must succeed (absent id refused) first, then responsabile signer (single-signer today; provably vote-derived is ruled, unbuilt — see “Composition”) |
| `deposit` | responsabile signer; `u` a member; signer ≠ `u`; `0 ≤ v` |
| `withdraw` | responsabile signer; `u` a member; signer ≠ `u`; `bal conti u ≥ v`; not stalled |
| `transferCassa` | responsabile signer and responsabile `f`; signer ≠ `f`; `v > 0` |
| `donate` | responsabile signer; `0 < v`; raises the comune conto, creating no member credit |
| `backdonate` | responsabile signer; `0 < w`; `comuneBal ≥ n * w` over the `lean:memberKeys` count; `auth s w` |
| `pledge` | **responsabile signer today**; `u` a member; no existing pledge by `u` in either list; `0 < v`; `bal conti u ≥ v`; not stalled |
| `acceptPledge` / `refusePledge` | responsabile signer who is the collection referente; pledge present in `pending`; `acceptPledge` additionally not stalled |
| `correctPledge` | responsabile signer who is the referente; pledge present in `accepted`; `0 ≤ v'`; `bal conti u + (v − v') ≥ 0` |
| `closePurchase` | responsabile signer who is the referente; `permitted`; `pending` empty; not stalled |
| `failPurchase` | responsabile signer who is the referente; `pending` empty |

Two honest tensions the reader must not miss. First, the 2026-08-25
sovereign-members ruling says pledges are self-service, while `pledge`'s
guard still opens with `isResponsabile` (`lean:Reactivegas/Step.lean:isResponsabile`) — a member cannot pledge for
themselves at all, and `correctPledge` over accepted pledges is
referente-only. That contradiction is ruled to change (#69: signer == `u`
while pending, referente after acceptance) and has not landed; see the
current-vs-ruled table. Second, `grantPermission`/`denyPermission` each
succeed on one responsabile's signature alone today, while their user story
says “the group”. That wire is the unbuilt composition, and the page states
its status as `PROVED-IN-MODEL` until the substrate mirrors it.

## Route and the sealed base hook

`lean:Route` classifies each of the fourteen economic constructors by the
producer that actually decides it (`lean:route`, independently
`lean:voteDerived`): 11 `direct`, 0 `baseEnacted` (unpopulated — no event
routes to it), 3 `appDecided` (`grantPermission`, `denyPermission`,
`backdonate`). The `baseEnacted` constructor survives for the accepted historical
theorem, but no event routes to it since the three base-enacted membership
events retired with `addUser`. A record that lists the route vocabulary
without saying the middle arm is empty will mislead every reader who counts
arms. The faithful base vocabulary (`lean:baseProposalFaithful`) admits only
`changeRoles` and `removeMember`; voted admission is excluded by
construction.

Membership consequences now run inside `lean:baseHook`, in the same
transition as the base change that caused them, derived from the real
pre/post views. `lean:economicCleanup` is exhaustive over the three
`lean:BaseChange` constructors, so a fourth substrate membership effect
stops compilation rather than acquiring a silent default:

- `memberAdmitted` — no economic consequence; the member arrives with no
  conto, no cassa, no collection;
- `memberRemoved` — refused while stalled; otherwise `lean:windUpAdmin`
  when the leaver held admin, then `lean:absorbConto`;
- `rolesChanged` — refused while stalled on admin loss, else
  `lean:windUpAdmin` on admin loss, nothing on admin gain.

`lean:windUpAdmin` cancels the key's open collections, refunds every pledge
they held (`lean:stripCollections` + `lean:refundAll`), and moves the cassa
claim to the comune. That sealed path is what the L1 law below is about.

## Laws

The legacy reactors were the behavioural spec; the laws below are the
safety spec. Each entry keeps its user story and states exactly what the
Lean proves — over fourteen events, through the viewed step, against the
sealed hook.

### AUTH — Only entitled actors can act (law)

**User story.** Bruno is an ordinary member. He tries to close Elena's
collection and withdraw from someone else's account. Every one of those
attempts bounces: the machine simply returns `none`.

**What is proved.** `lean:authorizedStep` states that whenever
`stepEvent view s e auth = some s'`, the event's author has the required role
— and only that: the predicate checks author role alone and ignores state and
args, so an admin authoring for an absent member still satisfies it. That
absent-member illustration is the scope limit, not a Lean defect: the real step
refuses it via membership guards. `lean:step_authorized` proves this role-only
property case by case over the fourteen constructors. Collection lookup comes
first: `lean:pullCollection` must succeed — an absent id is refused before any
signer check — for `grantPermission`/`denyPermission`.

### L1 — Governance enacts removal (law, restated against the hook)

**User story.** The group votes to expel Carl, a member who owes money on
an open purchase. Only the governance process may perform the removal. A
random responsabile cannot delete members to make pledges disappear.

**What is proved.** There is no membership event anymore, so the law
changed shape: `lean:governance_enacts_windUpAdmin` shows that winding up
`u` through the sealed hook always satisfies `lean:governanceEnacts` — no
open collection is left with `u` as referente. The story is still exactly
right; only the mechanism moved from a signable event to the hook. The
finite companion `lean:Reactivegas/Invariants.lean:checkAdminDepartureCleanup` executes the same
departure-and-cleanup journey (the proved check; the same name in TraceTests is an alias).

### L2 — Closure needs permission (law, with a stated limit)

**User story.** Elena opened a collection for a bulk olive-oil order.
Before she closes it, the group must grant closure permission, typically
once the goods arrive and the price is confirmed. Without that click,
closing is impossible.

**What is proved.** Whenever a close event succeeds, the collection
satisfies `lean:permissionToClose`: the permission flag was set *before*
the close and no pledge is still pending
(`lean:close_permission_to_close`). **Limit:** the flag today reflects one
responsabile's signature, not a counted vote. Permission-gated closure is
proved; vote-derived permission is the unbuilt composition below.

### L3 — Escrow at pledge (law)

**User story.** Anna pledges €30 towards the olive oil. Her account shows
€30 less immediately, her offer is listed in the collection, and the
group's total wealth does not change. With only €20, the machine refuses
outright.

**What is proved.** After a successful pledge, three things hold together
(`lean:pledge_escrow_debit`): Anna's balance dropped by exactly `v`, the
collection holds exactly `v` more escrow, witnessed by `lean:escrowHeld`
via `lean:splitUser`, and the global books still balance. The funds check
is part of the guard that `lean:pledge_guard_inv` decomposes (with
`lean:step_correct_inv` for corrections).

### L4 — Closure spends the referente's cassa (law)

**User story.** The oil arrives, permission is granted, Elena closes the
collection. The pooled money moves out of Elena's cash box as referente — the
full escrow decreases her `cassa` by the collected total — who then pays the
supplier with the goods received.

**What is proved.** `lean:close_spends_referente` shows that the closed
collection's full escrow decreases `col.referente`'s `cassa` — a debit of the
collected total from her cash box — when the close event succeeds.

### L5 — Double entry for cash movements (law)

**User story.** Marco deposits €50 into his account via Daniela, a
responsabile: Marco's account rises by €50 *and* Daniela's cash box rises
by €50 — both sides rise together, because the cash box records custody
taken, not cash spent. Withdrawal is the mirror image: both sides fall
together. Boundary: `-1` refused, `0` accepted, `+1` accepted — zero deposits
are permitted.

**What is proved.** Both movements produce the witness required by the
`lean:doubleEntry` predicate: `lean:deposit_double_entry` and
`lean:withdraw_double_entry` construct it directly from each side of the
ledger. (Earlier prose had the cassa direction backwards; the predicate is
authoritative: `bal conti u` and `bal casse a` move by the same `v`.)

### L6 — Conservation (flagship law)

**User story.** Months pass. Dozens of purchases open and close; money
moves between accounts, cash boxes and escrow. The treasurer's invariant
stays true: the machine never minted or burned a cent.

**What is proved.** `lean:conservation` states the source equation
`Σ casse − Σ conti − Σ open escrow = 0`, and
`lean:conservation_preserved` proves that **every one of the 14 event
constructors** preserves it. Each case reduces to arithmetic facts about
the list helpers discharged by `omega`. Any code change that leaks or
duplicates money breaks the build.

### L7 — Solvency is enforced, insolvency unreachable (law, scoped)

**User story.** Bruno has €12 and tries to pledge €30. The machine
refuses: `step` (`lean:step`) returns `none`, no money moves. There is no “recorded with
warning” middle ground for the machine's own acts — an overdrawing debit
never happens.

**What is proved.** The guards reject every debit that would push a member
balance below zero and require non-negative pledge amounts, so refunds can
never push anyone under either. `lean:solvent` captures both halves — and
**only** member conti: the reserved comune conto may legitimately go
negative (the stall) without making the group insolvent, and
`lean:insolvent` names exactly a member balance below zero. Then
`lean:solvent_init` boots it, `lean:solvent_preserved` shows every one of
the 14 event constructors preserves it, `lean:reach_solvent` chains these
along any execution (`lean:Reach`), and `lean:not_insolvent_of_reach`
finishes the job. Insolvency went from reachable-and-reported to
impossible — for member accounts. Group-level shortfall is representable
by design (the negative comune) and is handled by the stall, not by L7.

### L8 — One pledge per user per collection (law)

**User story.** Anna, excited about the olive oil, tries to pledge twice.
The second attempt is refused outright: within one collection a member
speaks with one wallet.

**What is proved.** `lean:uniquePledges` states uniqueness inside one
collection and `lean:allUniquePledges` lifts it to the whole state.
`lean:pledge_rejected_when_member` shows the duplicate returns `none`;
`lean:uniquePledges_pend_cons` shows an accepted pledge onto a
uniqueness-respecting collection preserves uniqueness; and
`lean:pledge_preserves_allUnique` lifts that to whole-state preservation.

## Laws versus finite witnesses

Universally quantified laws (above) prove something about every execution.
Finite witnesses prove something about fixed exhibits: an executable oracle
whose whole statement is `check… = true`. Both are proved, but only the
first kind is a guarantee, and their names read alike — which is exactly
the trap.

Twenty-nine short `check… = true` names form one syntactic category of
finite oracle — and that category is **not a total census** of every finite
witness in the model: `lean:KelGroups.majority_table` and others use
different syntax, and the #66 audit upheld the defect call on exactly this
point while rejecting any corrected percentage offered without a counting
rule. So: where this page says “law”, the Lean quantifies universally;
where it says “finite witness”, the Lean checks a fixed exhibit
(`lean:open_questions_are_open` states only that a question *still present*
has verdict open — it does not assert retention of previously open
questions, which is open statement work S5). No unproved
model-completeness claim appears anywhere on this page.

## Unimplemented runtime composition

The composition module (`Composition.lean`) classifies; it does not run. It imports
`Reactivegas.Types`, `KelGroups.Fold`, `KelGroups.Invariants` and
`KelGroups.Vote.Fold` — never the transition — and nothing imports it
except the library aggregator. Its own header says it plainly: nothing in
this repository consumes the route at runtime today, and the production
transition, invariants, integrated corpus and trace emitter carry no
build-time dependency on it (delete-file control builds clean). Until a
substrate mirrors the classification, the honest status is
**enforced: PROVED-IN-MODEL**, not enforced.

`lean:appDecided_verdict_exhaustive` proves the verdict elimination is
exhaustive and honest — a recorded closure carries an event exactly for
`positive`/`negative`, never `open` (`lean:appVerdictAllows`). Three links
are unbound — reachability, target, and polarity — and the page states them rather than hiding them:

1. **Reachability.** The theorem takes its event and its
   `lean:ClosureRecord` as unrelated inputs. No production transition
   consumes a closure record.
2. **Target.** Nothing ties the closed question to the collection id in the
   event. A closure about one collection satisfies the theorem for an event
   about another.
3. **Polarity.** Nothing maps `positive → grantPermission` and
   `negative → denyPermission` in code.

Because these links are unbuilt, an outcome test run today against a
coordinator faithful to the Lean as written would let assenso pass by one
responsabile calling `grantPermission` alone. It would look green. The
requirement (NOTE-016: grant/deny provably vote-derived; second consumer
voted comune backdonation; A-Q001 option D classify-don't-join, witnessed
by `ClosureRecord.verdict`, total and wildcard-free) is ruled; the
classification is delivered; the wire does not exist. That is follow-up
composition work, not this record's to build.

## Vote-lifecycle limits

The economic step and the vote fold meet at exactly one door.
`lean:appFold` sends the three vote constructors to `lean:voteApply` and
everything else to `step` (`lean:step`); exactly one validation decision
(`lean:applyVoteEventChecked` over `lean:validateVoteEvent`) dominates the
effect (`lean:effectedState`) and the recompute-and-close sweep
(`lean:sweepClosures`). A proposal opens with **empty tallies** — a
deliberate divergence from legacy — and `openQuestion` never overwrites or
revives a decided id.

Two limits the reader must hold:

- **Renounce succeeds and changes nothing — unfinished against a ruling.**
  `effectedState` (`lean:effectedState`) on `renounce`
  is identity, discharged by `rfl` in three inversion proofs, while
  `validateVoteEvent` (`lean:validateVoteEvent`) accepts a renounce by any responsabile on an existing
  question. On the integrated route a renounce is therefore told it worked
  and moves nothing; the bare economic step refuses it outright. V-5 has
  ruled (2026-08-27): proposer renounce or departure closes the question
  with the negative continuation running refunds — and the code does not do
  it yet. `lean:ClosureCause` already carries the `proposerDeparted` and
  `renounced` identities from Slice A for Slice B, but `lean:closureCause`
  returns only tally or franchiseChange. The record states ruled versus
  unfinished, not an open question.
- **`notDesignee` and `notProposer` are declared but constructed nowhere —
  and neither refusal is ruled.**
  `lean:VoteError` declares four errors; `validateVoteEvent` (`lean:validateVoteEvent`) has three arms
  and builds only the first two. They are Slice B forward declarations, not
  corpus gaps — no corpus row can observe them by construction. Current
  behavior is accepted-recorded-non-deciding (a non-designee ballot on a
  permission question is recorded and decides nothing) and accepted-no-op
  (a non-proposer renounce is accepted and moves nothing). The operator open
  question — refuse the non-designee ballot instead, refuse the non-proposer
  renounce instead — is carried neutrally with both readings; the dormant
  constructors evidence an intention, not a requirement, and this page claims
  neither as delivered nor as required.

Coverage, stated plainly. The simulator's `VOTE_TRACES_V1` drives signed
vote events through `validateVoteEvent` (`lean:validateVoteEvent`) on a standalone vote state (fifteen
signed events over the documented journey, handoff pin `af9c1e5`, not in
this repository). The two #74 corpora (`fed19b3` handoff) carry zero signed
vote events: the economic corpus cannot reach votes (`step` (`lean:step`) returns none
for the three constructors), and the integrated corpus exercises
franchise-change (V-3) closure through `baseHook` (`lean:baseHook`)'s sweep without ever
emitting an `.app` event. Assenso is named in the milestone outcome and, in
these two corpora, has no oracle behind it — extending coverage is separate
filed work: #75 tracks the integrated vote corpus through the production root
(planned, not delivered — see the pending table), and #76 the runtime
vote/economic composition wire (planned, not delivered — see “Unimplemented
runtime composition” and the pending table). A green gate here must not be
read as implying vote coverage.

The vote threshold policy θ is open. `lean:legacyThreshold` is
`(n+1)/2` and `lean:zeroThreshold` is constantly zero; the Lean is explicit
that both are exhibits, not defaults, and the threshold is a parameter
everywhere (`lean:s62bThreshold` pins `legacyThreshold` only as the #74
replayer's resolution). #68 must not be read across as a vote default.

## Group closure: classification, not a theorem

`lean:canCloseGroup` (Predicates.lean line 85) is an orphan: the definition
site is its only reference in the discovered Lean extent — no theorem, no
gate, no other definition consumes it. It states three conjuncts: every
member's `conti` balance is zero, there are no open collections, and every
`casse` balance is zero.

Recorded product intent: Q-2 restricts leaving the group to economically
settled members (zero conto, no live pledges), and Q-6 guards role
departure on `cassa == 0` — both settlement rules, neither a group-closure
transition. No ruling establishes closing the group itself as an event, and
this lane invents no theorem, deletes nothing, and implements nothing.

**Verdict: missing guarantee.** The record names closable conditions the
model never establishes or consumes. If the operator rules group closure a
non-goal, that ruling lands here as dated authority; until then the gap
stays named rather than silently accepted.

## Voci non-goal

**Fact.** The legacy catalogue subsystem — twenty `Voci/` paths at the pin
(quantities, units, volumes, weights, packaging, currency, plus the
`Voci/UI` screens), explicitly counting both `Voci/Quantita.hs` and
`Voci/Quantità.hs` as distinct blobs — with `Eventi/Ordine.hs`, and order-bound pledges
(`ImpegnoVincolato` / `CorrezioneImpegnoVincolato` over `[Ordine]`,
commented-out in `Eventi/Impegno.hs`) — has no Lean counterpart. The
model's pledge carries a bare `(user, amount)`: no product, no order, no
quantity.

**Ruling.** Out of scope for milestone 2. The outcome test is election →
collection → pledge → assenso → purchase → refund; no catalogue appears in
it. This is a non-goal, not an omission: an unmodelled subsystem nobody
has decided about reads as settled and is not.

**Reason and cost.** If the catalogue turns out to be load-bearing,
`pledge` gains a payload and `correctPledge` a diff, landing in the Lean
*before* the app fold is written; after D3 that is a conformance-corpus
re-freeze plus a fold rewrite. Cheapest to answer now, most expensive
after D3.

**Open operator question.** Whether the group reaches its outcome test
without a catalogue — inherited, undecided, carried up by the milestone
owner. This record does not pick a side.

## Dated operator authority

Later rulings supersede earlier assertions on the same scope. An issue
title alone never supplies a missing ruling.

- **2026-08-26 (Q-001):** Q-1 sovereign-members pledge question; Q-2
  settled-member departure; Q-3 deposed-admin questions; Q-4 key identity;
  Q-5 proposer withdrawal; B-1 release-pipeline blocker. Q-6: role
  departure guarded on `cassa == 0` (rotation, not sanction; sign
  convention — positive cassa is group money held, negative is group debt).
  Cassa comune design: settled departure is the normal path, unsettled
  departure is a separate named act into an anonymous pooled box; a
  negative comune is representable group shortfall. Total stall while the
  comune is negative — authorization acts stop, attestation stays open
  (`donate` cures; `failPurchase`/`refusePledge` stay reachable because
  refunds move no cash); open orders may fail, never close; the stall
  principle — while the group is short, no member may improve their
  position relative to another.
- **2026-08-27 (V-1…V-7):** V-1 franchise tracks damage capacity
  (responsabili vote); V-2 threshold arithmetic free but its tie-passes and
  zero-passes consequences still need answers; V-3 verdict recomputed on
  every state change (questions can pass because people left); V-4 one
  list per responsabile; V-5 proposer leaves or renounces → question closes
  with the negative continuation running refunds; V-6 no vote to admit a
  member (substrate requirement); V-7 no expiry — either side reaching
  threshold closes the question. Retention caveat: absence of a timer plus
  `.open` for still-present questions does not prove undecided questions are
  retained across every unrelated transition; retention
  statement-completeness is open under #66 S5, and
  `lean:open_questions_are_open` constrains still-present questions only
  (see “Laws versus finite witnesses”).
- **2026-09-05 (V-2 agency + pledge agency):** proposals open at zero
  assents — the proposer is not an assent, the `(n+1)/2` arithmetic
  unchanged, every decision above n=1 needs someone other than the
  proposer; pledge free while pending (`signer == u`, `v' = 0` is
  withdrawal) and referente-gated after acceptance, solvency guard and
  `closePurchase` unchanged, pending-vs-accepted legible in the UI.
- **Composition chain:** NOTE-016 (grant/deny provably vote-derived, two
  consumers, `PROVED-IN-MODEL` caveat), A-Q001 option D (classify, don't
  join: direct / baseEnacted / app-decided, total and wildcard-free),
  NOTE-031 (vote machine accepted; composition dispatched as its own work).

## Current versus ruled (pending merges)

Honest current record; implemented-new-behavior claims wait for their
merge. Exact source pins refresh after each merge; final closure
reconciles accepted milestone slice inputs and never marks a stale
snapshot complete because prose passes a checker.

| id | current behavior at this pin | ruled behavior | source ruling | re-pin condition |
| --- | --- | --- | --- | --- |
| #66 S1 (#79, `4a6cd87f`) | `Trace.lean` manifest missed namespaced declarations, so exactly ONE seeded byte is affected — the withdraw-refusal `declaration:UNPROVED` row (CORPUS-COVERAGE measured 1 UNPROVED + 1 step_close_inv); corpus content provisional | manifest resolves accepted inversions by unqualified name; `scripts/check-trace-coverage-agreement` wired into `just lean`; the byte moves only on #74 re-freeze | RECONCILIATION-001 + CLOSURE-MAP §3 | S1 landed at `4a6cd87f`; re-freeze + re-derive after #74 re-freeze |
| #68 | proposer counts as an assent; at n=2 one carries a proposal alone | proposals open at zero; arithmetic `(n+1)/2` unchanged; n=2 needs someone other than the proposer | A-V2-AND-PLEDGE-AGENCY 2026-09-05 | #68 merge, then re-derive assent/authority rows |
| #69 | `pledge` demands responsabile signer (member cannot pledge for self); `correctPledge` referente-only over accepted | member free while pending (`signer == u`, `v' = 0` withdraws); referente after acceptance; solvency guard, `closePurchase`, and UI-legibility rule as stated | A-V2-AND-PLEDGE-AGENCY 2026-09-05 | #69 merge, then re-derive pledge/correction rows |
| V-5 lifecycle (#81) | renounce accept-and-no-op; `closureCause` (`lean:closureCause`) tally/franchiseChange-only (rows 1–4 unfinished as above) | V-5 closure on proposer renounce/departure + negative continuation + refund | #81 | #81 merge, then re-derive vote-lifecycle rows |

## Reconciliation hook

After each of S1, #68, #69 merges: refresh exact source pins, re-derive
every affected row above against the new tree, re-run the citation gate,
and amend this page. The hook fires on merges, not on prose passing.

## Verifying

```sh
just lean        # cd lean && lake build
nix develop -c just ci   # includes the lean build
```
