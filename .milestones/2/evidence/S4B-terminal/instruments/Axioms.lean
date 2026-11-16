import Reactivegas.Mirrors
import KelGroups.Mirrors
#check conservation_corr
#print axioms conservation_corr
#check solvent_corr
#print axioms solvent_corr
#check insolvent_corr
#print axioms insolvent_corr
#check uniquePledges_corr
#print axioms uniquePledges_corr
#check allUniquePledges_corr
#print axioms allUniquePledges_corr
#check escrowHeld_corr
#print axioms escrowHeld_corr
#check governanceEnacts_corr
#print axioms governanceEnacts_corr
#check doubleEntry_corr
#print axioms doubleEntry_corr
#check canCloseGroup_corr
#print axioms canCloseGroup_corr
#check KelGroups.pendingWellFormed_corr
#print axioms KelGroups.pendingWellFormed_corr
#check KelGroups.membersCoherent_corr
#print axioms KelGroups.membersCoherent_corr
#check KelGroups.pendingCoherent_corr
#print axioms KelGroups.pendingCoherent_corr
#check KelGroups.wellFormed_corr
#print axioms KelGroups.wellFormed_corr
#check KelGroups.enacts_corr
#print axioms KelGroups.enacts_corr
#check KelGroups.Vote.questionClean_corr
#print axioms KelGroups.Vote.questionClean_corr
#check KelGroups.Vote.sweepReady_corr
#print axioms KelGroups.Vote.sweepReady_corr
#check KelGroups.Vote.voteWellFormed_corr
#print axioms KelGroups.Vote.voteWellFormed_corr
#check comune_not_a_member_corr
#print axioms comune_not_a_member_corr
#check permissionToClose_corr
#print axioms permissionToClose_corr
#check productionWellFormed_proj
#print axioms productionWellFormed_proj
-- Restored original runtime positives for both executable-body challenges.
example : KelGroups.GroupView.isMember "u" {members := [("u", ⟨"u","",[]⟩)]} = true := by decide
example : (stepEvent {members := [("a", ⟨"a","",[.adminRole .publicAdmin]⟩)]}
  {State.empty with collections := [⟨1,"a",false,[],[]⟩]}
  (.closePurchase "a" 1) (fun _ _ => false)).isSome = false := by decide
#eval "RESTORATION-P01-TRUE-P07-FALSE"
