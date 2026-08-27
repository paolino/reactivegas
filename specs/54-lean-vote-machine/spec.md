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
