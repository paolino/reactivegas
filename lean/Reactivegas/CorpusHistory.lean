import Lean
import Reactivegas.FiniteHistory
import Reactivegas.VoteCorpus
import Reactivegas.ReplayContext

/-!
# Validation of the vote corpus bytes (`reactivegas#66`, slice S5-C)

`REACTIVEGAS_VOTE_CORPUS=<vote.json> REACTIVEGAS_VOTE_CONTEXT=<vote.context.json> lake env lean Reactivegas/CorpusHistory.lean`
reads the committed bytes of the vote corpus and its replay context, decodes the
corpus into a supplied integrated history, and validates that history with the
integrated validator `historyRefusal` of `Reactivegas.FiniteHistory`:

* the threshold is the table of the sidecar, validated by `parseContext`
  (schema, version, corpus digest, authorization token, table rules) and applied
  through `tableThreshold`; no Lean threshold constant is consulted. A query
  outside the table's domain is a refusal to validate, never a verdict;
* the authorization is the always-refuse authorization the sidecar names;
* three kinds of defect are then planted in that same decoded history and each
  must be refused for its own reason at the step it was planted in: a planted
  post-state, a flipped `accepted`, and a non-genesis initial aggregate.

Each check prints one line `HISTORY-VERIFY <name> PASS|FAIL <detail>`. This is a
validation of one supplied history; it decides nothing about reachability.
Outside the import closure of the corpus exporter, so the source-input digest
does not cover it.
-/

namespace Reactivegas.CorpusHistory

/-- The integrated corpus step the validator reads. -/
def ofVoteStep (st : VoteTraceStep) : HistoryStep :=
  { signer := st.signer, event := st.event, accepted := st.accepted
    state := st.state, change := st.change }

/-- Validate under the sidecar's table. The first size queried outside the
table's domain is returned beside the verdict. -/
def validateUnderTable (table : Array Nat) (h : History) :
    IO (Option HistoryRefusal × Option Nat) := do
  let firstOut ← IO.mkRef (none : Option Nat)
  let θ := tableThreshold table firstOut
  let refusal ← IO.lazyPure (fun _ => historyRefusal θ probeAuth h)
  return (refusal, ← firstOut.get)

def report (name : String) (pass : Bool) (detail : String) : IO Bool := do
  IO.println s!"HISTORY-VERIFY {name} {if pass then "PASS" else "FAIL"} {detail}"
  return pass

/-- A control passes when the validator refuses the planted history with
exactly the expected reason and never queried outside the table. -/
def control (table : Array Nat) (name : String) (planted : History)
    (expected : HistoryRefusal) : IO Bool := do
  let (got, out) ← validateUnderTable table planted
  report name (got == some expected && out.isNone)
    s!"expected {repr expected}, got {repr got}, outside-table query {out}"

def setAt {α : Type} (i : Nat) (f : α → α) : List α → List α
  | [] => []
  | x :: xs => if i = 0 then f x :: xs else x :: setAt (i - 1) f xs

def run (corpusBytes sidecarBytes : String) : IO Bool := do
  match parseContext corpusBytes sidecarBytes with
  | .error r => report "validated-vote-corpus" false s!"sidecar refused: {r.1} {r.2}"
  | .ok table =>
    match decodeVoteFile corpusBytes with
    | .error e => report "validated-vote-corpus" false s!"corpus does not decode: {e}"
    | .ok (initial, voteSteps) =>
      let h : History := { initial, steps := voteSteps.map ofVoteStep }
      let accepted := h.steps.filter (·.accepted)
      let refused := h.steps.filter (fun st => !st.accepted)
      let (verdict, out) ← validateUnderTable table h
      let mut ok ← report "validated-vote-corpus"
        (verdict.isNone && out.isNone && !accepted.isEmpty && !refused.isEmpty)
        s!"steps={h.steps.length} accepted={accepted.length} refused={refused.length} domain=0..{table.size - 1} refusal={repr verdict} outside-table-query={out}"
      match h.steps.findIdx? (·.accepted), h.steps.findIdx? (fun st => !st.accepted) with
      | some a, some r =>
        ok := (← control table "control-planted-post-state"
          { h with steps := setAt a (fun st =>
              { st with state := { st.state with members := st.state.members.tail } }) h.steps }
          (.postStateDiffers a)) && ok
        ok := (← control table "control-flipped-accepted-true"
          { h with steps := setAt a (fun st => { st with accepted := false }) h.steps }
          (.appliedButStoredRefused a)) && ok
        ok := (← control table "control-flipped-accepted-false"
          { h with steps := setAt r (fun st => { st with accepted := true }) h.steps }
          (.refusedButStoredAccepted r)) && ok
      | _, _ =>
        ok ← report "control-planted-steps" false "the corpus has no accepted or no refused step to plant into"
      let pendingStore : List (KelGroups.ProposalId × KelGroups.PendingProposal) :=
        [("depart:forged", { proposal := .removeMember "forged", proposer := "forged", approvals := [] })]
      ok := (← control table "control-non-genesis-pending-proposal"
        { h with initial := { h.initial with pendingProposals := pendingStore } }
        .notBootOutput) && ok
      ok := (← control table "control-non-genesis-live"
        { h with initial := { h.initial with appFold := { h.initial.appFold with
            live := [{ questionId := "forged", target := .permission 1, verdict := .positive }] } } }
        .payloadNotClean) && ok
      ok := (← control table "control-non-genesis-comune-member"
        { h with initial := { h.initial with
            members := (comuneId, { key := comuneId, email := "comune@reserved.invalid", roles := [] }) :: h.initial.members } }
        .comuneReserved) && ok
      return ok

end Reactivegas.CorpusHistory

/-- The entry of the verification step: when both paths are given in the
environment, validate; any failed check is an error, so the elaboration of this
file fails. With no path given it does nothing, so a build that elaborates this
module reads no corpus. -/
def Reactivegas.CorpusHistory.runFromEnvironment : IO Unit := do
  match ← IO.getEnv "REACTIVEGAS_VOTE_CORPUS", ← IO.getEnv "REACTIVEGAS_VOTE_CONTEXT" with
  | some corpus, some context =>
    let ok ← Reactivegas.CorpusHistory.run (← IO.FS.readFile corpus) (← IO.FS.readFile context)
    unless ok do throw (IO.userError "vote corpus history validation failed")
  | _, _ => pure ()

#eval Reactivegas.CorpusHistory.runFromEnvironment
