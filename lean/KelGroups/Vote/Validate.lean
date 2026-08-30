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

The universal signer rule (R57-04, R-45): every signed event requires the signer
to be a current responsabile *in the canonical view* (R62-11). The franchise is
read from `KelGroups.GroupState.members` through that view; this machine has no
membership of its own to consult.

`notDesignee` and `notProposer` are declared here from Slice A so Slice B
(designee-only casting on permission questions, proposer-only renunciation)
extends rather than redesigns the vocabulary; nothing in Slice A produces
them yet.

## The three retired member events

`admitMember`, `removeMember` and `setRoles` no longer have anywhere to write:
membership has exactly one writable store and it is not this payload. They are
**refused** here with their own error identity, never accepted as a silent
no-op — a caller cannot mistake a refusal for a committed membership change.
The constructors themselves leave the sum in T6222 (S62-B), together with the
bootstrap admission capability they used to carry; the founding admin now
arrives through the guarded initial aggregate rather than through a vote event.
-/

namespace KelGroups.Vote

/-- Distinct admissibility errors. The identity of the first error is part of
the contract. -/
inductive VoteError where
  | notResponsabile
  | questionNotFound
  | notDesignee
  | notProposer
  | membershipNotVoteLocal
deriving DecidableEq, BEq, Repr

instance : BEq (Except VoteError Unit) where
  beq
    | .ok (), .ok () => true
    | .error left, .error right => left == right
    | _, _ => false

/-- Validate one signed event against the canonical view and the current vote
payload. The authorization boundary is exhaustive: success implies
`isResponsabile signer view = true` for every question event, and no membership
event is admissible at all. -/
def validateVoteEvent (threshold : Threshold) (view : GroupView) (gs : VoteState)
    (signer : Key) (event : VoteEvent) : Except VoteError Unit :=
  match event with
  | .openQuestion _ _ =>
      if isResponsabile signer view then .ok () else .error VoteError.notResponsabile
  | .cast questionId _ =>
      if !(isResponsabile signer view) then .error VoteError.notResponsabile
      else
        match lookupQuestion questionId gs with
        | some _ => .ok ()
        | none => .error VoteError.questionNotFound
  | .renounce questionId =>
      if !(isResponsabile signer view) then .error VoteError.notResponsabile
      else
        match lookupQuestion questionId gs with
        | some _ => .ok ()
        | none => .error VoteError.questionNotFound
  | .admitMember _ _ _ => .error VoteError.membershipNotVoteLocal
  | .removeMember _ => .error VoteError.membershipNotVoteLocal
  | .setRoles _ _ => .error VoteError.membershipNotVoteLocal

end KelGroups.Vote
