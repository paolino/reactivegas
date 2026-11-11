import Reactivegas.Mirrors
import KelGroups.Mirrors

open KelGroups
open KelGroups.Vote

def adm : Member := ⟨"a", "a@x", [.adminRole .publicAdmin]⟩
def usr : Member := ⟨"u", "u@x", []⟩
def vw : GroupView := ⟨[("a", adm), ("u", usr)]⟩
def dup : GroupView := ⟨[("u", usr), ("u", adm)]⟩
def good : State := { State.empty with conti := [("u",7)], casse := [("a",7)] }
def bad : State := { good with conti := [("u",-3)] }
def col : Collection := ⟨1,"a",true,[],[]⟩
def pledged : Collection := { col with pending := [⟨"u",5⟩] }
def pending : PendingProposal := ⟨.removeMember "u","a",["a"]⟩
def group : GroupState Nat := ⟨[("a",adm),("u",usr)],[("p",pending)],[],7⟩
def question : Question := ⟨.collective,"a",["a"],[]⟩
def votes : VoteState := ⟨[("q",question)],[]⟩

-- Every pair has distinguishable values, including nonzero financial payloads.
example : (!GroupView.isMember comuneId vw) = true := by decide
example : (!GroupView.isMember comuneId ⟨[(comuneId,usr)]⟩) = false := by decide
example : conservationB good = true ∧ conservationB bad = false := by decide
example : solventB vw good = true ∧ solventB vw bad = false := by decide
example : insolventB vw good = false ∧ insolventB vw bad = true := by decide
example : uniquePledgesB pledged = true ∧ uniquePledgesB { pledged with accepted := [⟨"u",6⟩] } = false := by decide
example : allUniquePledgesB { good with collections := [pledged] } = true ∧
  allUniquePledgesB { good with collections := [{ pledged with accepted := [⟨"u",6⟩] }] } = false := by decide
example : (col.permitted && col.pending.isEmpty) = true ∧
  (pledged.permitted && pledged.pending.isEmpty) = false := by decide
example : escrowHeldB pledged "u" 5 = true ∧ escrowHeldB pledged "u" 6 = false := by decide
example : escrowHeldB pledged "absent" 5 = false := by decide
example : governanceEnactsB "u" { good with collections := [col] } = true ∧
  governanceEnactsB "a" { good with collections := [col] } = false := by decide
example : doubleEntryB State.empty good "a" "u" 7 = true ∧
  doubleEntryB State.empty good "a" "u" 6 = false := by decide
example : canCloseGroupB vw State.empty = true ∧ canCloseGroupB vw good = false := by decide
example : pendingWellFormedB pending = true ∧ pendingWellFormedB { pending with approvals := [] } = false := by decide
example : membersCoherentB group = true ∧ membersCoherentB { group with members := [("other",usr)] } = false := by decide
example : pendingCoherentB group = true ∧ pendingCoherentB { group with pendingProposals := [("p",{pending with approvals := []})] } = false := by decide
example : wellFormedB group = true ∧ wellFormedB { group with members := [("u",usr),("u",usr)] } = false := by decide
example : enactsB group "p" (tryEnactDetailed group "p").state = true := by decide
example : enactsB group "p" { (tryEnactDetailed group "p").state with appFold := 8 } = false := by decide
example : questionCleanB question = true ∧ questionCleanB { question with dissents := ["a"] } = false := by decide
example : sweepReadyB vw votes = true ∧ sweepReadyB vw { votes with openQuestions := [("q",question),("q",question)] } = false := by decide
example : voteWellFormedB (fun _ => 2) vw votes = true ∧ voteWellFormedB (fun _ => 1) vw votes = false := by decide
-- Duplicate-key first-match semantics and absent-key zero are exercised explicitly.
example : bal [("u",7),("u",-99)] "u" = 7 ∧ bal [("u",7)] "missing" = 0 := by decide
example : solventB dup {good with conti := [("u",7),("u",-99)]} = true := by decide
example : canCloseGroupB vw {State.empty with casse := [("a",0),("a",99)]} = true := by decide
example : conservationB {State.empty with casse := [("a",0),("a",99)]} = false := by decide
-- An actual reachable nonzero economic state; no literal is labelled reachable without a trace.
example : stepEvent vw State.empty (.deposit "a" "u" 7) (fun _ _ => false) = some good := by decide
example : Reach vw (fun _ _ => false) good := by
  apply Reach.trans (Reach.boot (by decide))
  decide
#eval "WITNESS-OK 19 distinct positive/negative correspondence pairs; duplicate/default and nonzero reachable deposit controls"
