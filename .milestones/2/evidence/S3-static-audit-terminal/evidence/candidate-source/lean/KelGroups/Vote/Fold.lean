import KelGroups.Vote.State
import KelGroups.Vote.Event
import KelGroups.Vote.Validate

/-!
# Required vote machine — the production fold

One step function, `applyVoteEvent`, and the list fold `foldVote` over it.
Every theorem and executed witness of this run is stated over `foldVote`
(R-68, R-69).

Every function here takes the canonical `GroupView` explicitly (R62-11): the
franchise a sweep recomputes against is read from the one writable member store,
not from a payload-local copy, so a tally can never be evaluated against a
membership this machine believes but the group does not.

Three load-bearing shapes:

* `placeBallot` is the one-position-per-responsabile placement (R-56): casting
  assent inserts into the assent list *and erases from the dissent list*;
  dissent is symmetric. Legacy's bug — `ps' = r : filter (/= r) ps` with `ns`
  untouched — landed switchers in both lists and broke the "just vote no"
  escape; this placement cannot.
* `sweepClosures` re-evaluates every open question against the supplied
  canonical view and threshold and closes, in the same step, every question that
  has reached a verdict. `applyVoteEvent` sweeps after every *admitted* event's
  effect (R-51), because the verdict depends on the *current* franchise, not
  only on ballots. A rejected event reaches neither its effect nor the sweep.
* Closure is removal from the open set *plus* an appended closure record, as
  one operation. No branch ever erases a question without writing its record
  (R-61): a purchase-approval question holds members' money in escrow, and a
  question erased without a verdict strands it.

The retired member events are gone from `VoteEvent` entirely (T6222); this
payload never had anywhere to write them.
`renounce` is carried in the vocabulary and is a no-op in
this slice; its closing behaviour and the
`closeProposerQuestions`/`proposerDeparted` route arrive with Slice B, as does
the recomputation obligation that a *base* membership transition owes this
machine (R62-11, T6223).

No time-like field or transition exists here (R-54); the sweep closes nothing
by the passage of anything, because it reads no clock — there is none to read.
-/

namespace KelGroups.Vote

/-- The one-position-per-responsabile placement (R-56). Inserting into one
list erases from the other; re-casting the same position changes neither
tally (`setInsert` is guarded); switching positions moves the voter. -/
def placeBallot (voter : Key) (ballot : Ballot) (question : Question) : Question :=
  match ballot with
  | .assent =>
      { question with assents := setInsert voter question.assents,
                      dissents := question.dissents.erase voter }
  | .dissent =>
      { question with dissents := setInsert voter question.dissents,
                      assents := question.assents.erase voter }

/-- One open question's sweep outcome: `none` keeps the question open, and
`some record` closes it under the recorded verdict and its observable cause. -/
def sweepStep (threshold : Threshold) (view : GroupView)
    (entry : QuestionId × Question) : Option ClosureRecord :=
  match verdictOf threshold view entry.2 with
  | .open => none
  | verdict =>
      some { questionId := entry.1, question := entry.2, verdict,
             cause := closureCause view entry.2 verdict }

/-- Evaluate every open question and close, in this same step, each one whose
verdict under the supplied canonical franchise and threshold is positive or
negative (R-51). Closed questions move to the closure log with their verdict and
observable cause; nothing is ever dropped silently (R-61). -/
def sweepClosures (threshold : Threshold) (view : GroupView) (gs : VoteState) : VoteState :=
  { gs with
    openQuestions :=
      gs.openQuestions.filter (fun entry => verdictOf threshold view entry.2 = .open),
    closed := gs.closed ++ gs.openQuestions.filterMap (sweepStep threshold view) }

/-- The event's own effect on the payload, before the recompute-and-close
sweep. Effects are authorization-free by architecture (F-001 property class):
they assume an already-admitted event — all signer authorization happens only
in the total exhaustive `validateVoteEvent` boundary — and contain no
independent standing decision. An `openQuestion` never overwrites or revives
an existing id — decided questions stay decided. There is no membership
event in the sum for this payload to have to ignore. -/
def effectedState (gs : VoteState) (signer : Key) (event : VoteEvent) : VoteState :=
  match event with
  | .openQuestion questionId kind =>
      let fresh : Question := { kind, proposer := signer, assents := [], dissents := [] }
      if (lookupQuestion questionId gs).isNone
          && !(gs.closed.any (fun record => record.questionId == questionId)) then
        { gs with openQuestions := assocInsert questionId fresh gs.openQuestions }
      else gs
  | .cast questionId ballot =>
      match lookupQuestion questionId gs with
      | some question =>
          let placed := placeBallot signer ballot question
          { gs with openQuestions := assocInsert questionId placed gs.openQuestions }
      | none => gs
  | .renounce _ => gs

/-- Checked vote step: exactly one `validateVoteEvent` decision. On `.ok`
that decision dominates `effectedState` and `sweepClosures`; on `.error`
neither runs. The integrated production path (`Reactivegas.voteApply`)
uses this function and must not validate again. -/
def applyVoteEventChecked (threshold : Threshold) (view : GroupView)
    (gs : VoteState) (signer : Key) (event : VoteEvent) :
    Except VoteError VoteState :=
  match validateVoteEvent threshold view gs signer event with
  | .error err => .error err
  | .ok () =>
      .ok (sweepClosures threshold view (effectedState gs signer event))

/-- Historical state-returning fold step. It erases the checked result:
rejection is payload identity, which is *not* how the integrated path
reports refusal (admitted no-ops exist). -/
def applyVoteEvent (threshold : Threshold) (view : GroupView) (gs : VoteState)
    (signer : Key) (event : VoteEvent) : VoteState :=
  match validateVoteEvent threshold view gs signer event with
  | .ok () => sweepClosures threshold view (effectedState gs signer event)
  | .error _ => gs

/-- The production fold: every signed event, in order, from the empty payload
under one canonical view. -/
def foldVote (threshold : Threshold) (view : GroupView)
    (events : List (Key × VoteEvent)) : VoteState :=
  events.foldl
    (fun current signed => applyVoteEvent threshold view current signed.1 signed.2)
    emptyVoteState

/-- `foldVote` over a trace, continued from an arbitrary payload: the induction
surface for the invariant proofs. -/
def foldFrom (threshold : Threshold) (view : GroupView) (initial : VoteState)
    (events : List (Key × VoteEvent)) : VoteState :=
  events.foldl
    (fun current signed => applyVoteEvent threshold view current signed.1 signed.2)
    initial

end KelGroups.Vote
