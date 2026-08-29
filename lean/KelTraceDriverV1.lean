import Lean
import KelGroups.Vote.Fold
import KelGroups.Vote.Validate

/-!
# Required vote machine — trace producer (local schema `kelgroups-vote.trace` v1)

Emits the deterministic seed envelope embedded by `economics-simulator.html`
as `VOTE_TRACES_V1`. States and events are serialized by Lean `ToJson`
instances over the authoritative `KelGroups.Vote` fold: each step carries the
explicit input state, the signed event, and either the `applied` post-state
computed by `applyVoteEvent` (with the closure count so consumers can see
sweep-driven closures) or the `refused` `VoteError` from `validateVoteEvent`.
Nothing is hand-written; post-states come from the production fold only.

The threshold is a PARAMETER of the machine (R-46); this corpus names its
choice explicitly in the envelope: `legacyThreshold`, the `(n+1)/2` exhibit.

Reproduce from a clean checkout with:

```sh
cd lean && lake env lean KelTraceDriverV1.lean
```

(`economics-simulator-vote-trace-gate.mjs` at the repository root runs exactly
that, compares fresh output with the embedded fixture, and replays both
through the page's production Vote transcription.)

Seed coverage: franchise building via plain member events (R-66: admission is
never a question), a collective question opened with EMPTY tallies (deliberate
divergence from legacy: no proposer auto-assent), a refused cast by a
non-responsabile and on a missing question, position switching that closes at
threshold (R-56 + R-51), an idempotent re-cast, a dissent-driven NEGATIVE
verdict, a per-person permission question where only the designee's ballot
decides (R-64) while another admin's ballot leaves it open, a
franchise-change closure carrying a stale tally past the threshold (R-55),
a no-op renounce, and a question left OPEN (undecided) in the final state.
If any seeded expectation is violated the driver throws instead of emitting a
usable-looking corpus.
-/

open Lean (ToJson toJson Json)
open KelGroups KelGroups.Vote

deriving instance Lean.ToJson for Admin
deriving instance Lean.ToJson for Role
deriving instance Lean.ToJson for Member
deriving instance Lean.ToJson for Verdict
deriving instance Lean.ToJson for Ballot
deriving instance Lean.ToJson for QuestionKind
deriving instance Lean.ToJson for ClosureCause
deriving instance Lean.ToJson for Question
deriving instance Lean.ToJson for ClosureRecord
deriving instance Lean.ToJson for VoteState
deriving instance Lean.ToJson for VoteEvent
deriving instance Lean.ToJson for VoteError

/-- One seeded signed event with its expected outcome. -/
structure Seed where
  signer : Key
  event : VoteEvent
  expectApplied : Bool

def adminRoles : List Role := [.adminRole .publicAdmin]

def admit (key : Key) : VoteEvent :=
  .admitMember key (key ++ "@toy.example") adminRoles

/-- The seed journey (see module docstring). -/
def seeds : List Seed := [
  ⟨"anna", admit "anna", true⟩,
  ⟨"anna", admit "bruno", true⟩,
  ⟨"anna", admit "elena", true⟩,
  ⟨"anna", admit "carlo", true⟩,                                -- franchise 4, θ=2
  ⟨"anna", .openQuestion "q:permesso-olio" .collective, true⟩,   -- EMPTY tallies
  ⟨"dora", .cast "q:permesso-olio" .assent, false⟩,              -- notResponsabile
  ⟨"anna", .cast "q:nessuna" .assent, false⟩,                    -- questionNotFound
  ⟨"anna", .cast "q:permesso-olio" .assent, true⟩,               -- 1/2, open
  ⟨"bruno", .cast "q:permesso-olio" .dissent, true⟩,             -- 1a/1d, open
  ⟨"bruno", .cast "q:permesso-olio" .assent, true⟩,              -- switch → 2/2 POSITIVE
  ⟨"anna", .openQuestion "q:sconto" .collective, true⟩,
  ⟨"elena", .cast "q:sconto" .dissent, true⟩,                    -- 1 dissent
  ⟨"elena", .cast "q:sconto" .dissent, true⟩,                    -- idempotent re-cast
  ⟨"carlo", .cast "q:sconto" .dissent, true⟩,                    -- 2 dissents → NEGATIVE
  ⟨"anna", .openQuestion "q:incarico" (.permission "bruno"), true⟩,
  ⟨"elena", .cast "q:incarico" .assent, true⟩,                   -- non-designee: still open
  ⟨"bruno", .cast "q:incarico" .assent, true⟩,                   -- designee → POSITIVE
  ⟨"anna", .openQuestion "q:magazzino" .collective, true⟩,
  ⟨"carlo", .cast "q:magazzino" .assent, true⟩,                  -- 1/2, open
  ⟨"anna", .removeMember "carlo", true⟩,                         -- franchise 3, θ=2, open
  ⟨"anna", .removeMember "bruno", true⟩,                         -- franchise 2, θ=1 →
                                                                 -- POSITIVE, franchiseChange
  ⟨"anna", .openQuestion "q:aperta" .collective, true⟩,          -- stays OPEN (undecided)
  ⟨"anna", .renounce "q:aperta", true⟩                           -- slice-A no-op
]

def θ : Threshold := legacyThreshold

def stepJson (input : VoteState) (s : Seed) :
    Except String (Json × VoteState) :=
  match validateVoteEvent θ input s.signer s.event with
  | .ok () =>
    if !s.expectApplied then
      .error s!"seed inatteso: applicato dove era atteso un rifiuto ({s.signer})"
    else
      let next := applyVoteEvent θ input s.signer s.event
      .ok (Json.mkObj [
        ("input", toJson input), ("signer", Json.str s.signer),
        ("event", toJson s.event),
        ("result", Json.mkObj [("tag", "applied"), ("state", toJson next),
          ("closedCount", Json.num next.closed.length)])], next)
  | .error e =>
    if s.expectApplied then
      .error s!"seed inatteso: rifiutato dove era atteso applicato ({s.signer})"
    else
      .ok (Json.mkObj [
        ("input", toJson input), ("signer", Json.str s.signer),
        ("event", toJson s.event),
        ("result", Json.mkObj [("tag", "refused"), ("error", toJson e)])], input)

def stepsJson : VoteState → List Seed → Except String (List Json)
  | _, [] => .ok []
  | gs, s :: rest => do
    let (j, gs') ← stepJson gs s
    let tail ← stepsJson gs' rest
    .ok (j :: tail)

#eval do
  match stepsJson emptyVoteState seeds with
  | .ok steps =>
    IO.println (Json.mkObj [("V",
      Json.mkObj [
        ("schema", "kelgroups-vote.trace"),
        ("version", (1 : Nat)),
        ("threshold", "legacyThreshold"),
        ("initial", toJson emptyVoteState),
        ("steps", Json.arr steps.toArray)])]).compress
  | .error msg =>
    throw (IO.userError s!"SEED-VOTE-TRACE-INVALID: {msg}; nessun corpus emesso")
