import Reactivegas.Mirrors
import KelGroups.Mirrors

-- V4 exception evidence: definitional identity (EQ form, closes by rfl).
example (theta : KelGroups.Vote.Threshold) (view : KelGroups.GroupView)
    (gs : KelGroups.Vote.VoteState) (s : KelGroups.Key)
    (ev : KelGroups.Vote.VoteEvent) (qid : KelGroups.Vote.QuestionId) :
    KelGroups.Vote.PreservesQuestionSemantics theta view gs s ev qid =
      (KelGroups.Vote.preservesQuestionDecide theta view gs s ev qid = true) := rfl

-- P11 exception evidence: per-constructor definitional projection (14x rfl).
-- Each arm of `authorizedStep` IS the existing `isResponsabile` application.
example (view : KelGroups.GroupView) (s s' : State) (a : KelGroups.Key) (c : CollId) :
    authorizedStep view s (.openPurchase a c) s' = (isResponsabile view a = true) := rfl
example (view : KelGroups.GroupView) (s s' : State) (a : KelGroups.Key) (c : CollId) :
    authorizedStep view s (.grantPermission a c) s' = (isResponsabile view a = true) := rfl
example (view : KelGroups.GroupView) (s s' : State) (a : KelGroups.Key) (c : CollId) :
    authorizedStep view s (.denyPermission a c) s' = (isResponsabile view a = true) := rfl
example (view : KelGroups.GroupView) (s s' : State) (a u : KelGroups.Key) (v : Int) :
    authorizedStep view s (.deposit a u v) s' = (isResponsabile view a = true) := rfl
example (view : KelGroups.GroupView) (s s' : State) (a u : KelGroups.Key) (v : Int) :
    authorizedStep view s (.withdraw a u v) s' = (isResponsabile view a = true) := rfl
example (view : KelGroups.GroupView) (s s' : State) (a f : KelGroups.Key) (v : Int) :
    authorizedStep view s (.transferCassa a f v) s' = (isResponsabile view a = true) := rfl
example (view : KelGroups.GroupView) (s s' : State) (a : KelGroups.Key) (v : Int) :
    authorizedStep view s (.donate a v) s' = (isResponsabile view a = true) := rfl
example (view : KelGroups.GroupView) (s s' : State) (a : KelGroups.Key) (w : Int) :
    authorizedStep view s (.backdonate a w) s' = (isResponsabile view a = true) := rfl
example (view : KelGroups.GroupView) (s s' : State) (a u : KelGroups.Key) (c : CollId) (v : Int) :
    authorizedStep view s (.pledge a u c v) s' = (isResponsabile view a = true) := rfl
example (view : KelGroups.GroupView) (s s' : State) (a u : KelGroups.Key) (c : CollId) :
    authorizedStep view s (.acceptPledge a u c) s' = (isResponsabile view a = true) := rfl
example (view : KelGroups.GroupView) (s s' : State) (a u : KelGroups.Key) (c : CollId) :
    authorizedStep view s (.refusePledge a u c) s' = (isResponsabile view a = true) := rfl
example (view : KelGroups.GroupView) (s s' : State) (a u : KelGroups.Key) (c : CollId) (v : Int) :
    authorizedStep view s (.correctPledge a u c v) s' = (isResponsabile view a = true) := rfl
example (view : KelGroups.GroupView) (s s' : State) (a : KelGroups.Key) (c : CollId) :
    authorizedStep view s (.closePurchase a c) s' = (isResponsabile view a = true) := rfl
example (view : KelGroups.GroupView) (s s' : State) (a : KelGroups.Key) (c : CollId) :
    authorizedStep view s (.failPurchase a c) s' = (isResponsabile view a = true) := rfl

-- R0 exception evidence: executable (Decidable instance + evaluation).
example (s : State) : Decidable (stalled s) := inferInstance
example : decide (stalled State.empty) = false := by decide


namespace AuditExceptions
def adminView : KelGroups.GroupView := ⟨[("a", ⟨"a", "a@audit", [.adminRole .publicAdmin]⟩)]⟩
example : authorizedStep adminView State.empty (.openPurchase "a" 7) State.empty := by decide
example : ¬authorizedStep ⟨[]⟩ State.empty (.openPurchase "a" 7) State.empty := by decide
#eval IO.println "EXCEPTION-P11 openPurchase positive+negative"
example : authorizedStep adminView State.empty (.grantPermission "a" 7) State.empty := by decide
example : ¬authorizedStep ⟨[]⟩ State.empty (.grantPermission "a" 7) State.empty := by decide
#eval IO.println "EXCEPTION-P11 grantPermission positive+negative"
example : authorizedStep adminView State.empty (.denyPermission "a" 7) State.empty := by decide
example : ¬authorizedStep ⟨[]⟩ State.empty (.denyPermission "a" 7) State.empty := by decide
#eval IO.println "EXCEPTION-P11 denyPermission positive+negative"
example : authorizedStep adminView State.empty (.deposit "a" "u" 11) State.empty := by decide
example : ¬authorizedStep ⟨[]⟩ State.empty (.deposit "a" "u" 11) State.empty := by decide
#eval IO.println "EXCEPTION-P11 deposit positive+negative"
example : authorizedStep adminView State.empty (.withdraw "a" "u" 11) State.empty := by decide
example : ¬authorizedStep ⟨[]⟩ State.empty (.withdraw "a" "u" 11) State.empty := by decide
#eval IO.println "EXCEPTION-P11 withdraw positive+negative"
example : authorizedStep adminView State.empty (.transferCassa "a" "b" 11) State.empty := by decide
example : ¬authorizedStep ⟨[]⟩ State.empty (.transferCassa "a" "b" 11) State.empty := by decide
#eval IO.println "EXCEPTION-P11 transferCassa positive+negative"
example : authorizedStep adminView State.empty (.donate "a" 11) State.empty := by decide
example : ¬authorizedStep ⟨[]⟩ State.empty (.donate "a" 11) State.empty := by decide
#eval IO.println "EXCEPTION-P11 donate positive+negative"
example : authorizedStep adminView State.empty (.backdonate "a" 11) State.empty := by decide
example : ¬authorizedStep ⟨[]⟩ State.empty (.backdonate "a" 11) State.empty := by decide
#eval IO.println "EXCEPTION-P11 backdonate positive+negative"
example : authorizedStep adminView State.empty (.pledge "a" "u" 7 11) State.empty := by decide
example : ¬authorizedStep ⟨[]⟩ State.empty (.pledge "a" "u" 7 11) State.empty := by decide
#eval IO.println "EXCEPTION-P11 pledge positive+negative"
example : authorizedStep adminView State.empty (.acceptPledge "a" "u" 7) State.empty := by decide
example : ¬authorizedStep ⟨[]⟩ State.empty (.acceptPledge "a" "u" 7) State.empty := by decide
#eval IO.println "EXCEPTION-P11 acceptPledge positive+negative"
example : authorizedStep adminView State.empty (.refusePledge "a" "u" 7) State.empty := by decide
example : ¬authorizedStep ⟨[]⟩ State.empty (.refusePledge "a" "u" 7) State.empty := by decide
#eval IO.println "EXCEPTION-P11 refusePledge positive+negative"
example : authorizedStep adminView State.empty (.correctPledge "a" "u" 7 11) State.empty := by decide
example : ¬authorizedStep ⟨[]⟩ State.empty (.correctPledge "a" "u" 7 11) State.empty := by decide
#eval IO.println "EXCEPTION-P11 correctPledge positive+negative"
example : authorizedStep adminView State.empty (.closePurchase "a" 7) State.empty := by decide
example : ¬authorizedStep ⟨[]⟩ State.empty (.closePurchase "a" 7) State.empty := by decide
#eval IO.println "EXCEPTION-P11 closePurchase positive+negative"
example : authorizedStep adminView State.empty (.failPurchase "a" 7) State.empty := by decide
example : ¬authorizedStep ⟨[]⟩ State.empty (.failPurchase "a" 7) State.empty := by decide
#eval IO.println "EXCEPTION-P11 failPurchase positive+negative"
example : decide (stalled {State.empty with conti := [(comuneId, -7)]}) = true := by decide
example : decide (stalled {State.empty with conti := [(comuneId, 7)]}) = false := by decide
#eval IO.println "EXCEPTION-R0 positive+negative-nonzero"
open KelGroups.Vote in
example : preservesQuestionDecide (fun _ => 2) adminView ⟨[("q", ⟨.collective, "a", [], []⟩)], []⟩ "a" (.cast "q" .assent) "q" = false := by decide
open KelGroups.Vote in
example : preservesQuestionDecide (fun _ => 2) adminView ⟨[("q", ⟨.collective, "a", [], []⟩)], []⟩ "absent" (.cast "q" .assent) "q" = true := by decide
#eval IO.println "EXCEPTION-V4 real-question-change+preservation"
end AuditExceptions
#print Reach
#print Reach.below
