import KelGroups.Vote.State
import KelGroups.Vote.Event

/-!
# Required vote machine — the production fold

One step function, `applyVoteEvent`, and the list fold `foldVote` over it.
Every theorem and executed witness of this run is stated over `foldVote`
(R-68, R-69).

Three load-bearing shapes:

* `placeBallot` is the one-position-per-responsabile placement (R-56): casting
  assent inserts into the assent list *and erases from the dissent list*;
  dissent is symmetric. Legacy's bug — `ps' = r : filter (/= r) ps` with `ns`
  untouched — landed switchers in both lists and broke the "just vote no"
  escape; this placement cannot.
* `sweepClosures` re-evaluates every open question against the current
  franchise and threshold and closes, in the same step, every question that
  has reached a verdict. `applyVoteEvent` calls it unconditionally on every
  branch (R-51): after a ballot, after a member event, after an opening —
  because the verdict depends on the *current* franchise, not only on
  ballots.
* Closure is removal from the open set *plus* an appended closure record, as
  one operation. No branch ever erases a question without writing its record
  (R-61): a purchase-approval question holds members' money in escrow, and a
  question erased without a verdict strands it.

Slice-A fold treatment of the member events: they are the franchise mechanics
the R-53 witness and the franchise theorems need (a responsabile leaves, the
threshold falls, tallies are read as recorded). The admission *requirements*
R-66/R-67 (immediacy theorem, no-question-payload theorem and their
witnesses) remain Slice B. `renounce` is carried in the vocabulary and is a
no-op in this slice; its closing behaviour and the
`closeProposerQuestions`/`proposerDeparted` route arrive with Slice B.

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
def sweepStep (threshold : Threshold) (gs : VoteState)
    (entry : QuestionId × Question) : Option ClosureRecord :=
  match verdictOf threshold gs entry.2 with
  | .open => none
  | verdict =>
      some { questionId := entry.1, question := entry.2, verdict,
             cause := closureCause gs entry.2 verdict }

/-- Evaluate every open question and close, in this same step, each one whose
verdict under the current franchise and threshold is positive or negative
(R-51). Closed questions move to the closure log with their verdict and
observable cause; nothing is ever dropped silently (R-61). -/
def sweepClosures (threshold : Threshold) (gs : VoteState) : VoteState :=
  { gs with
    openQuestions :=
      gs.openQuestions.filter (fun entry => verdictOf threshold gs entry.2 = .open),
    closed := gs.closed ++ gs.openQuestions.filterMap (sweepStep threshold gs) }

/-- The event's own effect on the state, before the recompute-and-close
sweep. A cast by a non-responsabile is a no-op effect besides its rejection in
validation (R-44, R-45): only a current responsabile can move a tally. An
`openQuestion` never overwrites or revives an existing id — decided questions
stay decided. -/
def effectedState (gs : VoteState) (signer : Key) (event : VoteEvent) : VoteState :=
  match event with
  | .openQuestion questionId kind =>
      let fresh : Question := { kind, proposer := signer, assents := [], dissents := [] }
      if (lookupQuestion questionId gs).isNone
          && !(gs.closed.any (fun record => record.questionId == questionId)) then
        { gs with openQuestions := assocInsert questionId fresh gs.openQuestions }
      else gs
  | .cast questionId ballot =>
      if isResponsabile signer gs then
        match lookupQuestion questionId gs with
        | some question =>
            let placed := placeBallot signer ballot question
            { gs with openQuestions := assocInsert questionId placed gs.openQuestions }
        | none => gs
      else gs
  | .renounce _ => gs
  | .admitMember key email roles =>
      { gs with members := assocInsert key (Member.mk key email roles) gs.members }
  | .removeMember key => { gs with members := assocErase key gs.members }
  | .setRoles key roles =>
      { gs with members := assocAdjust key (fun member => { member with roles }) gs.members }

/-- One fold step: the event's own effect, then the unconditional
recompute-and-close sweep. The sweep call is outside the match, so every
branch recomputes (R-51); a branch that skips it is exactly the mutation the
R-70 controls must redden. -/
def applyVoteEvent (threshold : Threshold) (gs : VoteState) (signer : Key)
    (event : VoteEvent) : VoteState :=
  sweepClosures threshold (effectedState gs signer event)

/-- The production fold: every signed event, in order, from the empty state.
This is the fold every theorem and witness of the run is stated over. -/
def foldVote (threshold : Threshold) (events : List (Key × VoteEvent)) : VoteState :=
  events.foldl
    (fun current signed => applyVoteEvent threshold current signed.1 signed.2)
    emptyVoteState

/-- `foldVote` over a trace, continued from an arbitrary state: the induction
surface for the invariant proofs. -/
def foldFrom (threshold : Threshold) (initial : VoteState)
    (events : List (Key × VoteEvent)) : VoteState :=
  events.foldl
    (fun current signed => applyVoteEvent threshold current signed.1 signed.2)
    initial

end KelGroups.Vote
