import KelGroups.Vote.State
import KelGroups.Vote.Event

/-!
# Required vote machine — admissibility

Distinct admissibility errors and the one validation entry point. Slice A
enforces the franchise rules (R-44, R-45): a cast — or a question opening —
by anyone who is not a current responsabile is rejected with a distinct
error, and the fold makes such casts no-ops besides.

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

/-- Validate one signed event against the current state. -/
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
      match lookupQuestion questionId gs with
      | some _ => .ok ()
      | none => .error VoteError.questionNotFound
  | .admitMember _ _ _ => .ok ()
  | .removeMember _ => .ok ()
  | .setRoles _ _ => .ok ()

end KelGroups.Vote
