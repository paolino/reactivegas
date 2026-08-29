# Spec — #54 Lean kelgroups vote machine and composition theorem

Issue: https://github.com/paolino/reactivegas/issues/54 (parent #43, milestone 2)
Issue body SHA-256 (as stored, LF, no trailing newline):
`81a94a3b209ecf520be635af439d9be8c9d445ac0b5ac5e60ad501ebb7f774f7`
(corrected body, published 2026-08-26 per epic NOTE-018; supersedes
`533b9f69085543da059881b0a209e211b54b8b4d1812f1b14f731c6bc5b654d1`)

## Scope of this document

Both slices are specified. **Only Slice 1 is authorized to run.** Slice 2 is
blocked until the #48 ticket owner delivers the accepted definitions commit and
both frozen economic consumer signatures.

## Fidelity source of truth

The Lean model reproduces the Haskell that exists in `/code/kelgroups` at commit
`368b596` (clean tree), not an idealized vote protocol. Frozen source hashes:

| File | SHA-256 |
|---|---|
| `lib/KelGroups/Types.hs` | `c187d4360ed9fa54a212bd6a77717ce062cc337668d5c55f7cdb2a0e70624ce8` |
| `lib/KelGroups/Event.hs` | `c1d7a0064eb0b61a55bdf89e21bafe399e5fa3445de68b6a7f484510f837104b` |
| `lib/KelGroups/State.hs` | `63c807fdf06f29ed7fc0dee18cf67894004e00a7ed30c901252c67f099b470bc` |
| `lib/KelGroups/Fold.hs` | `3aedf556323db8b197b194502a2d47e093345a0b69e3623ea64b59253684994a` |
| `lib/KelGroups/Validate.hs` | `d82110e8c3ca8f1ea51447b4b45023fecc2085d2f21008fe05a83baf78fb1790` |
| `lib/KelGroups/Bootstrap.hs` | see `research.md`; added by the ticket owner |

`Bootstrap.hs` is **not named in the issue** but is a mandatory fidelity source:
`Validate.hs` imports `AuthMode`/`authMode` from it and bootstrap behaviour —
which the issue does require — is unmodellable without it. This is a source-list
addition, not a semantic change.

Where fidelity and proof elegance conflict, **fidelity wins**. A cleaner voting
model is out of scope and is a rejection reason.

## Motivating gap (why the theorem exists)

In `lean/Reactivegas/Step.lean`, `grantPermission a c` and `denyPermission a c`
each demand only `isResponsabile s a`. One responsabile can unilaterally permit
a closure, or destroy a collection and refund every accepted and pending pledge.
`permissionToClose` asserts the *flag*, never that a majority produced it.

## Requirements — Slice 1 (AUTHORIZED)

Stable IDs. Each is an observable truth about the delivered Lean, not an
instruction about how to write it.

### Structure and boundary

- **R-1** A vote machine exists under `lean/KelGroups/` with a root module
  `lean/KelGroups.lean` importing every submodule.
- **R-2** *(the portability rule — one-way)* No module under `lean/KelGroups/`
  (nor `lean/KelGroups.lean`) imports any `Reactivegas.*` module. This is the
  whole of what keeps the later lift into `paolino/kelgroups` survivable.
- **R-3** *(the legal direction — must not be rejected)* `lean/Reactivegas/`
  **may** import `KelGroups.*`: reactivegas is the application, kelgroups the
  substrate, and the dependency runs app → substrate. The boundary checker must
  be shown **not** to fire on that direction. There is no bridge/`Composition`
  module family; the Slice-2 composition lives under `lean/Reactivegas/` and may
  see both namespaces.
- **R-2b** *(the rule must outlive this ticket)* The R-2 check ships **tracked**
  at `nix/lean-dependency-direction.sh` and is invoked by the `lean` recipe in
  `justfile`, **before** `lake build`, so that both `just ci` and the existing
  GitHub CI step (`nix develop --command just lean`) run it on every build. A
  boundary rule enforced only by an untracked ticket gate dies with the ticket;
  the port survivability it protects does not expire when this PR merges. The
  before-`lake build` ordering is contractual, not stylistic: it is what lets
  the wiring control run cheaply on every gate invocation.
- **R-4** `lean/KelGroups/` is a real `lake build` target: a deliberate type
  error in any file under it makes `cd lean && lake build` exit non-zero.
- **R-5** Vote-machine types, definitions, invariants, and theorems mention no
  `Reactivegas.*` type. The application dimension appears **only** as an
  abstract type parameter `α` with an abstract fold, exactly as the Haskell's
  `GroupState a` / `AppFold a` do.

### Faithful semantics

Each row must hold of the delivered model. `pid` is the proposal digest.

- **R-6 Proposer auto-approval.** Proposing installs a pending proposal whose
  proposer is the signer and whose approvals are exactly `{signer}`.
- **R-7 Propose replaces.** Proposing a proposal whose id already has a pending
  entry *replaces* that entry: proposer becomes the new signer and approvals
  become exactly `{new signer}`. Previously accumulated approvals are lost.
- **R-8 Immediate enactment.** Enactment is attempted after every propose and
  after every approve, never at any other time.
- **R-9 Majority.** `majority = (adminCount + 1) / 2` on natural division:
  0→0, 1→1, 2→1, 3→2, 4→2, 5→3. It is **ceil(n/2)**, and for every even
  positive admin count it is **not a strict majority** (`2 * majority ≤ n`).
- **R-10 Zero admins enact on propose.** With zero admins the threshold is 0, so
  a propose enacts immediately. This is bootstrap.
- **R-11 Threshold is read pre-enactment.** The threshold compared against the
  approval count is computed from the state *before* the proposal's effect is
  applied, with the proposal already inserted/updated.
- **R-12 Approve on an unknown id is a no-op.** The state is returned unchanged.
- **R-13 Duplicate approval does not grow approvals.** Re-approving leaves the
  approval set unchanged; the fold nonetheless re-attempts enactment.
- **R-14 Enactment deletes exactly one pending proposal** — the enacted one.
  Every other pending proposal survives unchanged, including its now-stale
  approval count.
- **R-15 Enactment payloads.** Exactly three: introducing a member *overwrites*
  any existing entry for that key (email and roles wholly replaced); removing a
  member is a no-op when absent; changing roles is a **no-op when the member is
  absent** (adjust, never insert).
- **R-16 Member key coherence.** An introduced member's stored key equals the
  key it is indexed under.
- **R-17 App events.** Folding an application event applies the abstract fold
  and **discards the signer**; it touches no membership or proposal state.

### Faithful validation

- **R-18 Bootstrap proposal validation.** When there are zero admins, only
  introducing a member with a valid key *and* at least one admin role validates;
  every other proposal is rejected as `BootstrapRequiresAdmin`. **The signer is
  not checked at all in bootstrap.**
- **R-19 Normal proposal validation** requires an admin signer, then per payload:
  introduce → valid key, then not-already-a-member, then role-addition
  preconditions; remove → member exists; change roles → member exists, then
  removal preconditions for dropped roles, then addition preconditions for added
  roles.
- **R-20 Approval validation** requires an admin signer, then a known proposal,
  then a signer who has not already approved — rejecting with the distinct
  errors for those three cases.
- **R-21 App event validation** requires only that the signer is a member.
- **R-22 First-error fidelity.** Validation short-circuits left to right; the
  *identity* of the returned error is observable and must match the Haskell's
  for every modelled case.
- **R-23 Admin roles bypass role preconditions.** Adding or removing an admin
  role is always permitted; only application roles consult preconditions, and an
  unknown application role name is permitted.
- **R-24 Validation is not enforcement.** The fold does not validate. Every
  faithful fold behaviour above holds regardless of what validation would say.

### Invariants and refuted candidates

- **VI-1** Every pending proposal's approval set is duplicate-free.
- **VI-2** Every pending proposal's proposer is one of its approvers.
- **VI-3** *(enactment provenance)* Whenever a fold step enacts a proposal, the
  approval count at that instant was at least the pre-enactment threshold. This
  is the property Slice 2 consumes.
- **VI-4** Membership changes **only** through enactment. No fold step alters
  the member map except an enactment that satisfied VI-3.
- **VI-5** Member key coherence (R-16) holds in every reachable state.
- **VI-6** *(REFUTED, witness required)* "Every pending proposal is below the
  current threshold" is **false**. A reachable trace must be exhibited in which
  a proposal remains pending with approvals ≥ the current threshold, produced by
  an enactment that lowered the admin count.
- **VI-7** *(REFUTED, witness required)* "Every approver is an admin" is
  **false** even for fully validated traces, because a bootstrap proposer is
  never checked for membership. A witness trace must be exhibited.

A refuted candidate is delivered as an executed counterexample, not as prose.
Stating VI-6 or VI-7 as a proved theorem is a rejection reason.

### Extension points (named, not invented)

Where the Haskell lacks a behaviour, the model names the gap and stops. Adding
semantics to any of these without a separate ruling is a rejection reason.

- **EP-DENY** — kelgroups has **no dissent, rejection, expiry, or withdrawal
  event**. There is therefore *no vote-machine source for a deny verdict*.
  `denyPermission` in the economic machine has no derivation today. Slice 1
  records this; it is a **Slice 2 blocker** and is escalated to the epic owner.
- **EP-DIGEST** — the proposal id is a Blake2b SAID over the proposal's `show`.
  The model uses an abstract `digest : Proposal → ProposalId`. Equal proposals
  yield equal ids by construction; **injectivity is a hypothesis carried on the
  theorems that need it, never an axiom.**
- **EP-CESR** — key validity is CESR Ed25519 decoding. The model uses an
  abstract predicate parameter.
- **EP-LAST-ADMIN** — nothing prevents a proposal that removes the last admin
  and returns the group to bootstrap. Recorded, not fixed.
- **EP-ROLE-PRED** — `RoleDef` predicates are Haskell functions of the app fold;
  the model carries them as abstract predicates over `α`.
- **EP-REDUNDANT-LOOKUP** — role-change validation re-looks-up a member already
  proved present, so one Haskell error case is unreachable. Recorded.

### Fidelity matrix

- **R-25** A reviewed fidelity matrix ships at
  `docs/en/design/kelgroups-vote-machine.md` with a `mkdocs.yml` nav entry. Every
  row names its Lean declaration as **`lean:<Declaration>`** — an unambiguous
  marker, so the mechanical existence check cannot trip over the backticked
  *Haskell* names sitting in the same table — plus its Haskell anchor (file and
  definition name), and carries one of `FAITHFUL`, `DIVERGENT`, or `EXTENSION`.
  Every requirement ID above appears in at least one row. Every `DIVERGENT` and
  `EXTENSION` row states the consequence for the later port.
- **R-26** The doc claims **no** end-to-end enforcement. Slice-1 status wording
  is scoped to the model only.

### Proof hygiene

- **R-27** Zero `sorry` and zero `axiom` declarations under `lean/KelGroups/`.
- **R-28** Every named theorem's axiom set is exactly a subset of
  `propext`, `Classical.choice`, `Quot.sound`, recorded as gate evidence.
  **`native_decide` is forbidden** — it introduces `Lean.ofReduceBool`.
- **R-29** Point tests execute during `lake build` and are non-vacuous: a false
  point test makes the build red.

## Requirements — Slice 2 (BLOCKED, specified for contract stability)

- **R-30** The composition lives under `lean/Reactivegas/`, which may import
  `KelGroups.*`. R-2 continues to hold: nothing flows back the other way.
- **R-31** *(structural first)* Evaluate **structural** composition before any
  relational theorem: type the economic permission events so they **carry an
  enacted verdict** and cannot be constructed without evidence emitted by the
  vote machine. This replaces the current unilateral `isResponsabile s a`
  guard — the defect leaves the *representable* event space rather than being
  rejected after construction. A relational theorem between independent traces
  is a fallback only, permitted after a concrete impracticality is recorded on
  the record **before** implementation. Convenience or proof elegance is not a
  sufficient reason.
- **R-32** A responsabile cannot inject `grantPermission`/`denyPermission`
  directly through the composed model.
- **R-33** The derivation exposes the enacted verdict's identity, its question,
  and its provenance.
- **R-34** Purchase approval **and** the voted comune backdonation (#48) are
  both enumerated as consumers of the same verdict interface.
- **R-35** Documentation and theorem metadata use exactly
  `enforced: PROVED-IN-MODEL` and retain the later-port caveat. Unqualified
  end-to-end enforcement is never claimed.

Slice 2 cannot start until EP-DENY has a ruling: without a deny verdict source,
R-31 is unstatable for `denyPermission`.

## Out of scope / rejection reasons

- Any write to `/code/kelgroups`.
- Any edit to `lean/Reactivegas/**` or to the economic definitions owned by #48.
- Redesigning the Haskell vote semantics.
- Push, PR creation beyond the ticket-owner-owned draft, or merge.
- Claiming implementation or end-to-end enforcement.

---

# Requirements — Vote-coverage run (AUTHORIZED, 2026-08-29)

New work under the same issue #54 and the same ticket owner. It does **not**
reopen, revise, or invalidate Slice 1, which merged as
`ccdda83085c027c4142a14250cb7fd96a8f08dba`.

## Why a second surface rather than an edit

Slice 1's contract is *fidelity to `/code/kelgroups` at `368b596`*, and its
shipped matrix claims `FAITHFUL` on that basis. The operator's V-1…V-7 rulings
(`/tmp/reactivegas/ms2/questions/Q-001-operator-rulings.md`, SHA-256
`98837654cdf99505d1df093432a8c80d24c67727618f2b0d2864a8a20ded193a`, §"Vote-machine
rulings", lines 585–681) require behaviour that **today's kelgroups does not
have** — V-6 is explicit that "the substrate must admit a member without a
vote. Currently it cannot."

Editing the faithful model in place would make every `FAITHFUL` row in
`docs/en/design/kelgroups-vote-machine.md` false without any check going red.
That is precisely a green signal that no longer entails what it is read to
prove. Therefore:

- `lean/KelGroups/{Types,Event,State,Fold,Validate,Invariants,Tests}.lean`
  are **frozen** by this run. Not one byte changes.
- the required machine ships beside them, under `lean/KelGroups/Vote/`, and
  reuses only `KelGroups.Types` (assoc-list vocabulary, `Member`, `Role`,
  `hasAdmin`) so the two models cannot drift in their shared substrate.

Status vocabulary for the new surface is **`REQUIRED-OF-SUBSTRATE`**: it is
what `kelgroups#28`/`#30` must become, proved as a specification, not a claim
about code that exists. `FAITHFUL` is never claimed for it.

## Ruling absorbed: EP-DENY is no longer open

Slice 1 recorded **EP-DENY** ("kelgroups has no dissent, rejection, expiry or
withdrawal event … no vote-machine source for a deny verdict") as a Slice-2
blocker. V-7 supplies dissent (legacy `Dissenso`, same `soglia` both sides) and
V-5 supplies withdrawal (legacy `EventoFallimentoAssenso`, running the negative
continuation). **EP-DENY is ruled**; this run delivers the deny-verdict source.
Slice 2 remains blocked on its other precondition (#48 consumer signatures) and
is not started here.

## Vocabulary

`responsabile` = a member holding an admin role (`hasAdmin`), identical to
Slice 1's `isAdmin`. "Franchise" is the set of responsabili at the moment a
verdict is computed.

## Structure and boundary

- **R-40** The required machine lives under `lean/KelGroups/Vote/`, is rooted
  from `lean/KelGroups.lean`, and elaborates under `lake build`.
- **R-41** No module under `lean/KelGroups/Vote/` imports any `Reactivegas.*`
  module, and none imports `KelGroups.Fold`, `KelGroups.Validate`, or
  `KelGroups.Invariants`. The first clause is the portability rule (R-2)
  restated for the new subtree; the second is what keeps the faithful model
  unable to acquire required-but-absent semantics by accident.
- **R-42** The tracked checker `nix/lean-dependency-direction.sh` covers the
  new subtree. Its `grep -rnE` over `lean/KelGroups` is already recursive, so
  **no edit is expected**; the run must *demonstrate* coverage rather than
  assume it — a file placed under `lean/KelGroups/Vote/` importing
  `Reactivegas` must make the checker exit non-zero.
- **R-43** *(no vacuous green)* A deliberate elaboration error introduced under
  `lean/KelGroups/Vote/` makes `just lean` red. Slice 1 had to prove this for
  `lean/KelGroups/`; a new directory that no root module imports would compile
  nothing while reporting success.

## V-1 — the franchise is the responsabili

- **R-44** A ballot is admissible only from a current responsabile. A cast by a
  non-responsabile, by a non-member, or by a member without an admin role is
  rejected with a distinct error and is a no-op in the fold.
- **R-45** Pledging-style self-service has no analogue here: there is no path
  by which a non-responsabile influences a verdict.

## V-2 — threshold is a parameter, not a frozen policy

- **R-46** Every verdict computation takes an explicit threshold policy
  `Nat → Nat` mapping the current responsabile count to the required count.
  No policy is hard-coded into the machine, the state, or any invariant.
- **R-47** The legacy policy ships as one **named instance** reproducing
  `maggioranza`: `legacyThreshold n = (n + 1) / 2`, together with the `i == 0`
  case as a separate named instance `zeroThreshold _ = 0`.
- **R-48** V-2's two undecided consequences are delivered as executed witnesses
  **about `legacyThreshold` specifically**, never as machine-wide truths:
  (a) four responsabili, two assents, no dissent ⇒ `positive` — *a tie passes*;
  (b) `zeroThreshold` ⇒ a question opens and closes `positive` in one event
  with no ballot cast at all.
  Both are labelled as consequences of an **unruled** policy choice. Freezing
  either as the product answer is a rejection reason.

## V-3 / V-7 — three outcomes, recomputed always, never expiring

- **R-49** The verdict type has exactly three inhabitants — positive, negative,
  and a third open outcome (legacy `Indecidibile`) — and the open outcome is
  distinct from negative at the type level, not by a boolean flag.
- **R-50** The verdict is computed by comparing the recorded assent tally
  against the threshold first, then the recorded dissent tally against the
  **same** threshold, then falling through to open. This is legacy
  `maggioranza`'s exact order and its exact symmetry.
- **R-51** *(recompute on every state change)* Every open question's verdict is
  re-evaluated after **every** event the machine folds, not only after a
  ballot. Any question that has reached positive or negative closes in that
  same step.
- **R-52** *(the sharp form of R-51)* In every reachable state, every open
  question is open **under the current franchise and the current threshold**. A
  question sitting at or above threshold while still open is unreachable.
  Note the deliberate divergence: this is the negation of Slice 1's refuted
  **VI-6**, which exhibited exactly that stale state as reachable in the
  faithful model. Both are correct about their own machine.
- **R-53** *(the consequence the operator required be modelled honestly)* A
  reachable trace exists in which a question closes **positive** because a
  responsabile was removed — the tally never changed, the threshold fell.
- **R-54** *(no expiry)* There is no clock, deadline, timeout, or age field
  anywhere in the state. An open question stays open under any event that
  changes neither its ballots, nor the franchise, nor its proposer's standing.
  This is stated as a theorem, not as the absence of a field.
- **R-55** The exits from an open question are exactly three — a verdict, a
  franchise change carrying a stale tally past the threshold, and proposer
  departure or renunciation. No fourth exit is representable.

## V-4 — one position per responsabile

- **R-56** Casting assent inserts the voter into the assent list **and removes
  them from the dissent list**; casting dissent is symmetric. Re-casting the
  same position does not change either tally.
- **R-57** In every reachable state, each question's assent and dissent lists
  are duplicate-free **and disjoint**. No responsabile is ever counted twice,
  and the "just vote no" escape V-7 relies on is therefore always available.

## V-5 — proposer departure or renunciation closes, negatively

- **R-58** The proposer may renounce their own open question (legacy
  `EventoFallimentoAssenso` / `rinuncia`). Only the proposer may; another
  responsabile's renunciation is rejected.
- **R-59** Loss of the proposer's responsabile standing, and loss of their
  membership, each close every question they opened.
- **R-60** *(forced, not preferred)* Every such closure records the **negative**
  verdict, so the negative continuation runs. Silent deletion is a rejection
  reason: a purchase-approval question holds members' money in escrow, and a
  question erased without a verdict strands it.
- **R-61** *(no silent deletion, as an invariant)* Every question ever opened is
  in exactly one of two places in every reachable state: the open set, or the
  closure log carrying a verdict. The two partition; neither loses a question.

## V-6 — per-designee permission is not a vote

- **R-62** A question is either **collective** (tallied against the threshold)
  or **permission**, addressed to one named designee (legacy
  `Permesso richiedente designato`). The distinction is in the type.
- **R-63** On a permission question only the designee's ballot is admissible.
  Any other responsabile's cast is rejected with the distinct error legacy
  states as "il responsabile non è tenuto a dare il permesso sulla questione".
- **R-64** A permission question's verdict **never consults the threshold or
  any tally**: designee assent ⇒ positive, designee dissent ⇒ negative, no
  ballot ⇒ open. A permission verdict that could be reached by counting is a
  rejection reason.
- **R-65** The proposer of a permission question may renounce it under R-58,
  closing it negatively under R-60.

## Direct member admission

- **R-66** Admitting a member is a single event that adds the member
  immediately, opening **no** question and consulting **no** threshold — legacy
  `NuovoUtente` (`Eventi/Anagrafe.hs:170`). After exactly one admission event
  the member is present.
- **R-67** No question payload can admit a member. Routing admission through a
  question is a rejection reason; this is the requirement V-6 places on
  `kelgroups#28`, and the Lean models what reactivegas needs.

## Proof and evidence contract

- **R-68** Every theorem is about the **production fold** over event lists. A
  property proved only of a hand-written record literal, or of a state built by
  a test-only constructor that the fold cannot produce, does not discharge its
  requirement.
- **R-69** Every witness required above (R-48a, R-48b, R-53, R-66) is produced
  by executing that production fold, and is checked at elaboration time.
- **R-70** *(controls — each must be proved able to fail)* For each of the
  following, the run demonstrates a mutation that makes the named check red:
  no dissent path; a voter in both tallies; silent deletion of a question;
  recomputation only on ballots; an expiry field; a permission decided by
  majority; admission routed through a question. A check not shown to fail is
  not accepted as evidence for its requirement.
- **R-71** Zero `sorry` and zero custom axioms under `lean/KelGroups/Vote/`.
  Every named theorem's axiom set is a subset of `propext`, `Classical.choice`,
  `Quot.sound`, printed as gate evidence. `native_decide` remains forbidden.

## Out of scope for this run

- `lean/Reactivegas/**` — untouched. Slice 2 composition does **not** start.
- Slice-1's seven `lean/KelGroups/*.lean` modules — frozen.
- Any Haskell behaviour change; `Eventi/Anagrafe.hs` is read-only evidence.
- Push, PR creation or update, issue edit, readiness change, merge.
- Answering V-2's two open policy consequences. They are parameterized and
  reported upward still open.
