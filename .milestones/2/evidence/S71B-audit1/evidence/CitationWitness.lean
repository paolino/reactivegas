import Reactivegas
import KelGroups
import Reactivegas.TraceTests
-- lean:AppEvent = Reactivegas/Types.lean:75
#check _root_.AppEvent
-- lean:BackdonateAuth = Reactivegas/Step.lean:41
#check _root_.BackdonateAuth
-- lean:BaseChange = KelGroups/Event.lean:46
#check _root_.KelGroups.BaseChange
-- lean:ClosureCause = KelGroups/Vote/Types.lean:73
#check _root_.KelGroups.Vote.ClosureCause
-- lean:ClosureRecord = KelGroups/Vote/State.lean:41
#check _root_.KelGroups.Vote.ClosureRecord
-- lean:Collection = Reactivegas/State.lean:13
#check _root_.Collection
-- lean:Event = Reactivegas/Types.lean:41
#check _root_.Event
-- lean:GroupView = KelGroups/Types.lean:134
#check _root_.KelGroups.GroupView
-- lean:KelGroups.AppFold = KelGroups/Fold.lean:5
#check _root_.KelGroups.AppFold
-- lean:KelGroups.BaseEvent = KelGroups/Event.lean:11
#check _root_.KelGroups.BaseEvent
-- lean:KelGroups.Enacts = KelGroups/Invariants.lean:322
#check _root_.KelGroups.Enacts
-- lean:KelGroups.GroupState = KelGroups/State.lean:25
#check _root_.KelGroups.GroupState
-- lean:KelGroups.GroupView = KelGroups/Types.lean:134
#check _root_.KelGroups.GroupView
-- lean:KelGroups.RoleDef = KelGroups/Types.lean:37
#check _root_.KelGroups.RoleDef
-- lean:KelGroups.Tests.proposerAutoApproval = KelGroups/Tests.lean:46
#check _root_.KelGroups.Tests.proposerAutoApproval
-- lean:KelGroups.ValidationError = KelGroups/Validate.lean:7
#check _root_.KelGroups.ValidationError
-- lean:KelGroups.Vote.VoteState = KelGroups/Vote/State.lean:51
#check _root_.KelGroups.Vote.VoteState
-- lean:KelGroups.applyApprove = KelGroups/Fold.lean:71
#check _root_.KelGroups.applyApprove
-- lean:KelGroups.applyEvent = KelGroups/Fold.lean:84
#check _root_.KelGroups.applyEvent
-- lean:KelGroups.applyPropose = KelGroups/Fold.lean:54
#check _root_.KelGroups.applyPropose
-- lean:KelGroups.approvals_nodup = KelGroups/Invariants.lean:312
#check _root_.KelGroups.approvals_nodup
#print axioms _root_.KelGroups.approvals_nodup
-- lean:KelGroups.authMode = KelGroups/State.lean:65
#check _root_.KelGroups.authMode
-- lean:KelGroups.bootstrapNonMemberWitness = KelGroups/Invariants.lean:521
#check _root_.KelGroups.bootstrapNonMemberWitness
-- lean:KelGroups.bootstrapPendingWitness = KelGroups/Invariants.lean:550
#check _root_.KelGroups.bootstrapPendingWitness
-- lean:KelGroups.enact = KelGroups/Fold.lean:9
#check _root_.KelGroups.enact
-- lean:KelGroups.enact_implies_threshold_met = KelGroups/Invariants.lean:342
#check _root_.KelGroups.enact_implies_threshold_met
#print axioms _root_.KelGroups.enact_implies_threshold_met
-- lean:KelGroups.finishEnact = KelGroups/Fold.lean:18
#check _root_.KelGroups.finishEnact
-- lean:KelGroups.foldGroup = KelGroups/Fold.lean:88
#check _root_.KelGroups.foldGroup
-- lean:KelGroups.majority = KelGroups/State.lean:50
#check _root_.KelGroups.majority
-- lean:KelGroups.majority_not_strict_on_even = KelGroups/Invariants.lean:459
#check _root_.KelGroups.majority_not_strict_on_even
#print axioms _root_.KelGroups.majority_not_strict_on_even
-- lean:KelGroups.majority_table = KelGroups/Invariants.lean:450
#check _root_.KelGroups.majority_table
#print axioms _root_.KelGroups.majority_table
-- lean:KelGroups.member_key_coherent = KelGroups/Invariants.lean:374
#check _root_.KelGroups.member_key_coherent
#print axioms _root_.KelGroups.member_key_coherent
-- lean:KelGroups.members_change_implies_enacted = KelGroups/Invariants.lean:379
#check _root_.KelGroups.members_change_implies_enacted
#print axioms _root_.KelGroups.members_change_implies_enacted
-- lean:KelGroups.proposer_mem_approvals = KelGroups/Invariants.lean:317
#check _root_.KelGroups.proposer_mem_approvals
#print axioms _root_.KelGroups.proposer_mem_approvals
-- lean:KelGroups.setInsert = KelGroups/Types.lean:46
#check _root_.KelGroups.setInsert
-- lean:KelGroups.stalePendingWitness = KelGroups/Invariants.lean:504
#check _root_.KelGroups.stalePendingWitness
-- lean:KelGroups.tryEnact = KelGroups/Fold.lean:43
#check _root_.KelGroups.tryEnact
-- lean:KelGroups.validateApproval = KelGroups/Validate.lean:116
#check _root_.KelGroups.validateApproval
-- lean:KelGroups.validateEvent = KelGroups/Validate.lean:180
#check _root_.KelGroups.validateEvent
-- lean:KelGroups.validateProposal = KelGroups/Validate.lean:107
#check _root_.KelGroups.validateProposal
-- lean:Pledge = Reactivegas/Types.lean:26
#check _root_.Pledge
-- lean:Reach = Reactivegas/Predicates.lean:96
#check _root_.Reach
-- lean:Reactivegas/Invariants.lean:checkAdminDepartureCleanup = Reactivegas/Invariants.lean:1437
#check _root_.Reactivegas.checkAdminDepartureCleanup
-- lean:Reactivegas/Step.lean:isResponsabile = Reactivegas/Step.lean:31
#check _root_.isResponsabile
-- lean:Reactivegas/Types.lean:Proposal = Reactivegas/Types.lean:119
#check _root_.Proposal
-- lean:Route = Reactivegas/Composition.lean:38
#check _root_.Reactivegas.Composition.Route
-- lean:State = Reactivegas/State.lean:23
#check _root_.State
-- lean:VoteError = KelGroups/Vote/Validate.lean:38
#check _root_.KelGroups.Vote.VoteError
-- lean:absorbConto = Reactivegas/Step.lean:245
#check _root_.Reactivegas.absorbConto
-- lean:allUniquePledges = Reactivegas/Predicates.lean:45
#check _root_.allUniquePledges
-- lean:appDecided_verdict_exhaustive = Reactivegas/Composition.lean:139
#check _root_.Reactivegas.Composition.appDecided_verdict_exhaustive
#print axioms _root_.Reactivegas.Composition.appDecided_verdict_exhaustive
-- lean:appFold = Reactivegas/Step.lean:181
#check _root_.Reactivegas.appFold
-- lean:appVerdictAllows = Reactivegas/Composition.lean:130
#check _root_.Reactivegas.Composition.appVerdictAllows
-- lean:applyEvent = KelGroups/Fold.lean:84
#check _root_.KelGroups.applyEvent
-- lean:applyVoteEventChecked = KelGroups/Vote/Fold.lean:107
#check _root_.KelGroups.Vote.applyVoteEventChecked
-- lean:authorizedStep = Reactivegas/Predicates.lean:74
#check _root_.authorizedStep
-- lean:bal = Reactivegas/State.lean:39
#check _root_.bal
-- lean:baseHook = Reactivegas/Step.lean:298
#check _root_.Reactivegas.baseHook
-- lean:baseProposalFaithful = Reactivegas/Composition.lean:96
#check _root_.Reactivegas.Composition.baseProposalFaithful
-- lean:bump = Reactivegas/State.lean:62
#check _root_.bump
-- lean:canCloseGroup = Reactivegas/Predicates.lean:85
#check _root_.canCloseGroup
-- lean:close_permission_to_close = Reactivegas/Invariants.lean:647
#check _root_.close_permission_to_close
#print axioms _root_.close_permission_to_close
-- lean:close_spends_referente = Reactivegas/Invariants.lean:679
#check _root_.close_spends_referente
#print axioms _root_.close_spends_referente
-- lean:closureCause = KelGroups/Vote/State.lean:109
#check _root_.KelGroups.Vote.closureCause
-- lean:comuneBal = Reactivegas/State.lean:47
#check _root_.comuneBal
-- lean:comuneId = Reactivegas/Types.lean:22
#check _root_.comuneId
-- lean:conservation = Reactivegas/Predicates.lean:22
#check _root_.conservation
-- lean:conservation_preserved = Reactivegas/Invariants.lean:430
#check _root_.conservation_preserved
#print axioms _root_.conservation_preserved
-- lean:deposit_double_entry = Reactivegas/Invariants.lean:697
#check _root_.deposit_double_entry
#print axioms _root_.deposit_double_entry
-- lean:doubleEntry = Reactivegas/Predicates.lean:67
#check _root_.doubleEntry
-- lean:economicCleanup = Reactivegas/Step.lean:274
#check _root_.Reactivegas.economicCleanup
-- lean:effectedState = KelGroups/Vote/Fold.lean:87
#check _root_.KelGroups.Vote.effectedState
-- lean:escrowHeld = Reactivegas/Predicates.lean:55
#check _root_.escrowHeld
-- lean:escrowOf = Reactivegas/State.lean:76
#check _root_.escrowOf
-- lean:escrowSum = Reactivegas/State.lean:79
#check _root_.escrowSum
-- lean:governanceEnacts = Reactivegas/Predicates.lean:62
#check _root_.governanceEnacts
-- lean:governance_enacts_windUpAdmin = Reactivegas/Invariants.lean:637
#check _root_.governance_enacts_windUpAdmin
#print axioms _root_.governance_enacts_windUpAdmin
-- lean:insolvent = Reactivegas/Predicates.lean:35
#check _root_.insolvent
-- lean:legacyThreshold = KelGroups/Vote/Types.lean:44
#check _root_.KelGroups.Vote.legacyThreshold
-- lean:majority = KelGroups/State.lean:50
#check _root_.KelGroups.majority
-- lean:memberKeys = Reactivegas/Step.lean:35
#check _root_.memberKeys
-- lean:not_insolvent_of_reach = Reactivegas/Invariants.lean:1184
#check _root_.not_insolvent_of_reach
#print axioms _root_.not_insolvent_of_reach
-- lean:open_questions_are_open = KelGroups/Vote/Invariants.lean:810
#check _root_.KelGroups.Vote.open_questions_are_open
#print axioms _root_.KelGroups.Vote.open_questions_are_open
-- lean:permissionToClose = Reactivegas/Predicates.lean:50
#check _root_.permissionToClose
-- lean:pledge_escrow_debit = Reactivegas/Invariants.lean:659
#check _root_.pledge_escrow_debit
#print axioms _root_.pledge_escrow_debit
-- lean:pledge_guard_inv = Reactivegas/Invariants.lean:154
#check _root_.pledge_guard_inv
#print axioms _root_.pledge_guard_inv
-- lean:pledge_preserves_allUnique = Reactivegas/Invariants.lean:1269
#check _root_.pledge_preserves_allUnique
#print axioms _root_.pledge_preserves_allUnique
-- lean:pledge_rejected_when_member = Reactivegas/Invariants.lean:1241
#check _root_.pledge_rejected_when_member
#print axioms _root_.pledge_rejected_when_member
-- lean:proposalDigest = Reactivegas/Step.lean:309
#check _root_.Reactivegas.proposalDigest
-- lean:pullCollection = Reactivegas/State.lean:103
#check _root_.pullCollection
-- lean:reach_solvent = Reactivegas/Invariants.lean:1177
#check _root_.reach_solvent
#print axioms _root_.reach_solvent
-- lean:refundAll = Reactivegas/State.lean:94
#check _root_.refundAll
-- lean:route = Reactivegas/Composition.lean:47
#check _root_.Reactivegas.Composition.route
-- lean:s62bThreshold = Reactivegas/Invariants.lean:1302
#check _root_.Reactivegas.s62bThreshold
-- lean:solvent = Reactivegas/Predicates.lean:30
#check _root_.solvent
-- lean:solvent_init = Reactivegas/Invariants.lean:877
#check _root_.solvent_init
#print axioms _root_.solvent_init
-- lean:solvent_preserved = Reactivegas/Invariants.lean:1164
#check _root_.solvent_preserved
#print axioms _root_.solvent_preserved
-- lean:splitUser = Reactivegas/State.lean:85
#check _root_.splitUser
-- lean:stalled = Reactivegas/State.lean:55
#check _root_.stalled
-- lean:step = Reactivegas/Step.lean:44
#check _root_.step
-- lean:stepEvent = Reactivegas/Step.lean:147
#check _root_.stepEvent
-- lean:step_authorized = Reactivegas/Invariants.lean:561
#check _root_.step_authorized
#print axioms _root_.step_authorized
-- lean:step_correct_inv = Reactivegas/Invariants.lean:284
#check _root_.step_correct_inv
#print axioms _root_.step_correct_inv
-- lean:stripCollections = Reactivegas/State.lean:116
#check _root_.stripCollections
-- lean:sweepClosures = KelGroups/Vote/Fold.lean:74
#check _root_.KelGroups.Vote.sweepClosures
-- lean:tryEnact = KelGroups/Fold.lean:43
#check _root_.KelGroups.tryEnact
-- lean:uniquePledges = Reactivegas/Predicates.lean:40
#check _root_.uniquePledges
-- lean:uniquePledges_pend_cons = Reactivegas/Invariants.lean:1228
#check _root_.uniquePledges_pend_cons
#print axioms _root_.uniquePledges_pend_cons
-- lean:validateBootstrapProposal = KelGroups/Validate.lean:88
#check _root_._private.KelGroups.Validate.0.KelGroups.validateBootstrapProposal
-- lean:validateNormalProposal = KelGroups/Validate.lean:96
#check _root_._private.KelGroups.Validate.0.KelGroups.validateNormalProposal
-- lean:validateRoleChanges = KelGroups/Validate.lean:79
#check _root_._private.KelGroups.Validate.0.KelGroups.validateRoleChanges
-- lean:validateVoteEvent = KelGroups/Vote/Validate.lean:54
#check _root_.KelGroups.Vote.validateVoteEvent
-- lean:voteApply = Reactivegas/Step.lean:172
#check _root_.Reactivegas.voteApply
-- lean:voteDerived = Reactivegas/Composition.lean:66
#check _root_.Reactivegas.Composition.voteDerived
-- lean:windUpAdmin = Reactivegas/Step.lean:254
#check _root_.Reactivegas.windUpAdmin
-- lean:withdraw_double_entry = Reactivegas/Invariants.lean:707
#check _root_.withdraw_double_entry
#print axioms _root_.withdraw_double_entry
-- lean:zeroThreshold = KelGroups/Vote/Types.lean:48
#check _root_.KelGroups.Vote.zeroThreshold

-- Read-only finite evaluations of the real imported transition, no source mutation.
namespace AuditS2R

def view : KelGroups.GroupView :=
  { members := [
    ("a", {key := "a", email := "a", roles := [.adminRole .publicAdmin]}),
    ("u", {key := "u", email := "u", roles := []})] }
def auth : BackdonateAuth := fun _ _ => false

def journey : List Event := [
  .deposit "a" "u" 30,
  .openPurchase "a" 7,
  .pledge "a" "u" 7 30,
  .acceptPledge "a" "u" 7,
  .grantPermission "a" 7]

def reach : Option State :=
  journey.foldl (fun acc ev => acc.bind (fun s => stepEvent view s ev auth)) (some State.empty)

def facts : Option (Bool × Bool × Int × Int × Int × Int × Int) := do
  let before ← reach
  let after ← stepEvent view before (.closePurchase "a" 7) auth
  pure (after.conti == before.conti, after.collections.isEmpty,
    bal before.casse "a", bal after.casse "a", bal before.conti "a",
    bal after.conti "a", escrowSum before.collections)

-- Distinguishable 30-unit successful journey, not all-zero fixture equality.
#eval facts
-- First falsify the documentation's account-credit predicate on that reachable result.
#eval match reach with
  | none => false
  | some before => match stepEvent view before (.closePurchase "a" 7) auth with
    | none => false
    | some after => bal after.conti "a" == bal before.conti "a" + escrowSum before.collections
-- Then confirm the source theorem's cassa-debit relation on the same result.
#eval match reach with
  | none => false
  | some before => match stepEvent view before (.closePurchase "a" 7) auth with
    | none => false
    | some after => bal after.casse "a" == bal before.casse "a" - escrowSum before.collections
-- Boundary + positive + negative controls for the documented deposit refusal.
#eval ([-1, 0, 1] : List Int).map (fun v => (v, (stepEvent view State.empty (.deposit "a" "u" v) auth).isSome))
#eval (stepEvent view State.empty (.deposit "u" "u" 0) auth).isSome
-- Zero entries are stored by a reachable zero deposit.
#eval (stepEvent view State.empty (.deposit "a" "u" 0) auth).map (fun s => (s.conti, s.casse))
-- pullCollection is a rejection boundary before the signer guard.
#eval [0, 7].map (fun c => (c, (stepEvent view State.empty (.grantPermission "a" c) auth).isSome))
#eval (stepEvent view State.empty (.openPurchase "a" 7) auth).bind (fun s =>
  (stepEvent view s (.grantPermission "a" 7) auth).map (fun t => t.collections.map (·.permitted)))
-- S1 binding is read from the actual elaborated manifest.
#eval guardManifest.filter (fun row => row.1 == "withdraw" || row.1 == "closePurchase")
#print close_spends_referente
#print axioms close_spends_referente
#print authorizedStep
-- AUTH predicate accepts an admin-authored claim with a missing target member,
-- while the actual step refuses it. The predicate guards signer role only.
local instance : Decidable (authorizedStep view State.empty (.deposit "a" "stranger" 1) State.empty) := by
  unfold authorizedStep
  infer_instance
#eval decide (authorizedStep view State.empty (.deposit "a" "stranger" 1) State.empty)
#eval (stepEvent view State.empty (.deposit "a" "stranger" 1) auth).isSome
end AuditS2R

#print _root_.Proposal
#print KelGroups.Proposal
#check pledge_escrow_debit
#check conservation_preserved
