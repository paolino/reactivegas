import KelGroups.Vote.State
import KelGroups.Vote.Event

/-!
# Required vote machine — admissibility

Distinct admissibility errors and the one validation entry point. The
authorization decision is total and explicit over the complete `VoteEvent`
surface (R57-02): every constructor is enumerated, there is no wildcard
fallback, and no constructor list or boolean side registry exists, so a newly
added constructor cannot acquire an authorization default — the match stops
compiling (R57-02, INV-57-EXHAUSTIVE).

The universal signer rule (R57-04, R-45): once a franchise exists, every
signed event — including `admitMember`, `removeMember`, and `setRoles` —
requires the signer to be a current responsabile. Before a franchise exists,
only the `admitMember` bootstrap capability is retained: it is what lets a
production trace from `emptyVoteState` seed the first responsabile. This
adds no Slice-B R-66/R-67 admission-shape semantics.

`notDesignee` and `notProposer` are declared here from Slice A so Slice B
(designee-only casting on permission questions, proposer-only renunciation)
extends rather than redesigns the vocabulary; nothing in Slice A produces
them yet. The `threshold` parameter is part of the contractual signature and
becomes operative in Slice B.
-/

namespace KelGroups.Vote

/-- Distinct admissibility errors. The identity of the first error is part of
the contract. -/
inductive VoteError where
  | notResponsabile
  | questionNotFound
  | notDesignee
  | notProposer
deriving DecidableEq, BEq, Repr

instance : BEq (Except VoteError Unit) where
  beq
    | .ok (), .ok () => true
    | .error left, .error right => left == right
    | _, _ => false

/-- Validate one signed event against the current state. The authorization
boundary is exhaustive: success on a nonempty franchise implies
`isResponsabile signer gs = true` for every constructor; the empty-franchise
`admitMember` branch is the retained bootstrap capability only. -/
def validateVoteEvent (threshold : Threshold) (gs : VoteState) (signer : Key)
    (event : VoteEvent) : Except VoteError Unit :=
  match event with
  | .openQuestion _ _ =>
      if isResponsabile signer gs then .ok () else .error VoteError.notResponsabile
  | .cast questionId _ =>
      if !(isResponsabile signer gs) then .error VoteError.notResponsabile
      else
        match lookupQuestion questionId gs with
        | some _ => .ok ()
        | none => .error VoteError.questionNotFound
  | .renounce questionId =>
      if !(isResponsabile signer gs) then .error VoteError.notResponsabile
      else
        match lookupQuestion questionId gs with
        | some _ => .ok ()
        | none => .error VoteError.questionNotFound
  | .admitMember _ _ _ =>
      if isResponsabile signer gs || franchiseSize gs == 0 then .ok ()
      else .error VoteError.notResponsabile
  | .removeMember _ =>
      if isResponsabile signer gs then .ok () else .error VoteError.notResponsabile
  | .setRoles _ _ =>
      if isResponsabile signer gs then .ok () else .error VoteError.notResponsabile

end KelGroups.Vote
