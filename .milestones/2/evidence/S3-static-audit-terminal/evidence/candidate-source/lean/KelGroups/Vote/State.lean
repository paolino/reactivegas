import KelGroups.Vote.Types

/-!
# Required vote machine — state and the single verdict site

The vote payload: the open-question set and the append-only closure log. It
holds **no membership** (R62-01, R62-11). Franchise, authorization, threshold
and verdict are read from the canonical `GroupView` projected from
`KelGroups.GroupState.members`, which is the one writable member/role store;
this machine has no private copy to disagree with it.

There is no clock and no time-like field of any kind here (R-54); that absence
is what makes the no-expiry theorem provable.

`verdictOf` is the only place a verdict is decided. It takes the threshold and
the canonical view explicitly (R-46, R62-11) and dispatches on the question
kind, so a permission verdict can never reach the tally comparison (R-64). The
comparison order is legacy `maggioranza` exactly: recorded assents against the
threshold, then recorded dissents against the same threshold, then open (R-50).

Deliberate divergence from legacy, recorded here: legacy seeds a collection
with the proposer (`Assensi ur [ur] []`); this machine opens every question
with empty tallies, which is what the data model specifies and what makes
R-48b's "no ballot cast at all" exact.
-/

namespace KelGroups.Vote

/-- A question as it stands while open: kind fixed at opening, proposer
recorded, and the two tallies. No time-like field exists (R-54). -/
structure Question where
  kind : QuestionKind
  proposer : Key
  assents : List Key
  dissents : List Key
deriving DecidableEq, BEq, Repr

/-- A closure record: the question as it stood when it left the open set, the
verdict it closed under (never `open`), and the observable cause (R-55). It
carries no member snapshot. -/
structure ClosureRecord where
  questionId : QuestionId
  question : Question
  verdict : Verdict
  cause : ClosureCause
deriving DecidableEq, BEq, Repr

/-- The vote payload. `closed` is append-only: closing a question is removing it
from the open set and appending a closure record, as one operation — never a
silent deletion (R-61). -/
structure VoteState where
  openQuestions : List (QuestionId × Question)
  closed : List ClosureRecord
deriving DecidableEq, BEq, Repr

/-- The empty payload: no open questions, no closures. -/
def emptyVoteState : VoteState :=
  { openQuestions := [], closed := [] }

/-! ## Franchise (V-1, R62-11)

Read from the canonical view, never stored. There is no franchise snapshot in
the payload, so a tally can never be evaluated against a stale membership.
-/

/-- The franchise: the keys of the current responsabili (members holding an
admin role, `hasAdmin` from the shared substrate). -/
def franchise (view : GroupView) : List Key := GroupView.admins view

/-- Number of current responsabili; the argument a threshold policy is read
at. -/
def franchiseSize (view : GroupView) : Nat := (franchise view).length

/-- A key is a responsabile iff it is a member holding an admin role. -/
def isResponsabile (key : Key) (view : GroupView) : Bool := GroupView.isAdmin key view

/-! ## The single verdict site (R-46, R-49, R-50, R-64) -/

/-- The only place a verdict is decided. The threshold and the canonical view
are explicit parameters, and the threshold is consulted only for collective
questions, at the current franchise size, on both sides (legacy `maggioranza`'s
exact order and symmetry). A permission question's verdict never consults the
threshold or any tally count: the designee's recorded assent makes it positive,
their recorded dissent makes it negative, and absence of both leaves it open. -/
def verdictOf (threshold : Threshold) (view : GroupView) (question : Question) : Verdict :=
  match question.kind with
  | .collective =>
      let required := threshold (franchiseSize view)
      if question.assents.length ≥ required then .positive
      else if question.dissents.length ≥ required then .negative
      else .open
  | .permission designee =>
      if question.assents.contains designee then .positive
      else if question.dissents.contains designee then .negative
      else .open

/-- The open question with the given id, if any. -/
def lookupQuestion (questionId : QuestionId) (gs : VoteState) : Option Question :=
  assocLookup questionId gs.openQuestions

/-! ## Closure cause (R-55) -/

/-- The observable closure cause, decided from the canonical view at closure
time. R-55's second exit is "a franchise change carrying a stale tally past the
threshold": a tally is stale exactly when the deciding side records a key the
current franchise no longer counts. A closure whose deciding side is entirely
inside the current franchise is the first exit, a plain verdict. The
departure/renunciation causes are written by Slice B. -/
def closureCause (view : GroupView) (question : Question) (verdict : Verdict) : ClosureCause :=
  match verdict with
  | .positive => if question.assents.all (isResponsabile · view) then .tally else .franchiseChange
  | .negative => if question.dissents.all (isResponsabile · view) then .tally else .franchiseChange
  | .open => .tally

end KelGroups.Vote
