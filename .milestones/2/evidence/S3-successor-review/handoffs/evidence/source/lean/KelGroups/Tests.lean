import KelGroups.Invariants
import KelGroups.Validate

/-!
# Executed fidelity tests

These point tests pin the awkward behavior of the Haskell vote machine.  They
are deliberately elaborated by the root library: a false guard makes
`lake build` fail.
-/

namespace KelGroups
namespace Tests

def adminRole : Role := .adminRole .publicAdmin
def privateAdminRole : Role := .adminRole .privateAdmin
def plainRole : Role := .appRole "plain"

def member (key : Key) (roles : List Role := []) : Member :=
  { key, email := key ++ "@example.test", roles }

def stateWith (members : List (Key × Member)) (appFold : Nat := 0) : GroupState Nat :=
  { members, pendingProposals := [], pendingBase := [], appFold }

def admins (keys : List Key) : GroupState Nat :=
  stateWith (keys.map fun key => (key, member key [adminRole]))

def digest : Proposal → ProposalId
  | .introduceMember key _ _ => "introduce:" ++ key
  | .removeMember key => "remove:" ++ key
  | .changeRoles key _ => "roles:" ++ key

def validKey (key : Key) : Bool := key != "bad"

def emptyConfig : GroupConfig Nat := { roleDefs := [] }

def blockedConfig : GroupConfig Nat :=
  { roleDefs :=
      [ ("blockedAdd", { canAdd := fun _ => false, canRemove := fun _ => true })
      , ("blockedRemove", { canAdd := fun _ => true, canRemove := fun _ => false })
      ] }

def proposalIntro (key : Key) (roles : List Role := [plainRole]) : Proposal :=
  .introduceMember key (key ++ "@new.test") roles

def proposerAutoApproval : Bool :=
  let gs := applyPropose digest (admins ["a", "b", "c"]) "a" (proposalIntro "new")
  match lookupPending (digest (proposalIntro "new")) gs with
  | some pp => pp.proposer == "a" && pp.approvals == ["a"]
  | none => false

#guard proposerAutoApproval

def proposeReplacesPending : Bool :=
  let gs0 := admins ["a", "b", "c", "d", "e"]
  let p := proposalIntro "new"
  let gs1 := applyPropose digest gs0 "a" p
  let gs2 := applyApprove gs1 "b" (digest p)
  let gs3 := applyPropose digest gs2 "c" p
  match lookupPending (digest p) gs3 with
  | some pp => pp.proposer == "c" && pp.approvals == ["c"]
  | none => false

#guard proposeReplacesPending

def duplicateApprovalIsIdempotent : Bool :=
  let gs0 := admins ["a", "b", "c", "d", "e"]
  let p := proposalIntro "new"
  let gs1 := applyPropose digest gs0 "a" p
  let gs2 := applyApprove gs1 "b" (digest p)
  let gs3 := applyApprove gs2 "b" (digest p)
  match lookupPending (digest p) gs3 with
  | some pp => pp.approvals.length == 2 && pp.approvals == ["b", "a"]
  | none => false

#guard duplicateApprovalIsIdempotent

def approveUnknownIsNoOp : Bool :=
  let gs := admins ["a", "b", "c"]
  applyApprove gs "a" "missing" == gs

#guard approveUnknownIsNoOp

def majorityZero : Bool := majority (emptyState 0) == 0
def majorityOne : Bool := majority (admins ["a"]) == 1
def majorityTwo : Bool := majority (admins ["a", "b"]) == 1
def majorityThree : Bool := majority (admins ["a", "b", "c"]) == 2
def majorityFour : Bool := majority (admins ["a", "b", "c", "d"]) == 2
def majorityFive : Bool := majority (admins ["a", "b", "c", "d", "e"]) == 3

#guard majorityZero
#guard majorityOne
#guard majorityTwo
#guard majorityThree
#guard majorityFour
#guard majorityFive

def zeroAdminsEnactImmediately : Bool :=
  let p := proposalIntro "founder" [adminRole]
  let gs := applyPropose digest (emptyState 0) "stranger" p
  lookupMember "founder" gs ==
      some { key := "founder", email := "founder@new.test", roles := [adminRole] } &&
    lookupPending (digest p) gs == none

#guard zeroAdminsEnactImmediately

def oddBoundaryWaitsThenEnacts : Bool :=
  let p := proposalIntro "new"
  let gs1 := applyPropose digest (admins ["a", "b", "c"]) "a" p
  let gs2 := applyApprove gs1 "b" (digest p)
  lookupMember "new" gs1 == none && lookupPending (digest p) gs1 != none &&
    lookupMember "new" gs2 != none && lookupPending (digest p) gs2 == none

#guard oddBoundaryWaitsThenEnacts

def evenBoundaryIsNotStrict : Bool :=
  let p := proposalIntro "new"
  let gs1 := applyPropose digest (admins ["a", "b", "c", "d"]) "a" p
  let gs2 := applyApprove gs1 "b" (digest p)
  lookupMember "new" gs1 == none && lookupMember "new" gs2 != none

#guard evenBoundaryIsNotStrict

def preEnactmentMajorityControlsAdminIntroduction : Bool :=
  let p := proposalIntro "c" [adminRole]
  let gs := applyPropose digest (admins ["a", "b"]) "a" p
  lookupMember "c" gs != none && lookupPending (digest p) gs == none

#guard preEnactmentMajorityControlsAdminIntroduction

def enactDeletesOnlySelected : Bool :=
  let gs0 := admins ["a", "b", "c"]
  let p := proposalIntro "new"
  let sibling := proposalIntro "sibling"
  let siblingPending : PendingProposal :=
    { proposal := sibling, proposer := "c", approvals := ["c"] }
  let pending :=
    [ (digest p, { proposal := p, proposer := "a", approvals := ["b", "a"] })
    , (digest sibling, siblingPending)
    ]
  let gs := tryEnact { gs0 with pendingProposals := pending } (digest p)
  lookupPending (digest p) gs == none &&
    lookupPending (digest sibling) gs == some siblingPending

#guard enactDeletesOnlySelected

def removeAbsentIsNoOp : Bool :=
  let gs := admins ["a"]
  enact gs (.removeMember "missing") == gs

#guard removeAbsentIsNoOp

def changeRolesAbsentIsNoOp : Bool :=
  let gs := admins ["a"]
  enact gs (.changeRoles "missing" [plainRole]) == gs

#guard changeRolesAbsentIsNoOp

def removeAbsentStillRecordsEnactment : Bool :=
  let p := Proposal.removeMember "missing"
  let before := admins ["a"]
  let outcome := applyEventDetailed digest (fun n event => n + event)
    before "a" (.base (.propose p))
  outcome.state.members == before.members &&
    lookupPending (digest p) outcome.state == none &&
    match outcome.enactment with
    | some enacted =>
        enacted.proposalId == digest p && enacted.pending.proposal == p &&
          enacted.pending.approvals.length >= majority enacted.preState
    | none => false

#guard removeAbsentStillRecordsEnactment

def changeRolesAbsentStillRecordsEnactment : Bool :=
  let p := Proposal.changeRoles "missing" [plainRole]
  let before := admins ["a"]
  let outcome := applyEventDetailed digest (fun n event => n + event)
    before "a" (.base (.propose p))
  outcome.state.members == before.members &&
    lookupPending (digest p) outcome.state == none &&
    match outcome.enactment with
    | some enacted =>
        enacted.proposalId == digest p && enacted.pending.proposal == p &&
          enacted.pending.approvals.length >= majority enacted.preState
    | none => false

#guard changeRolesAbsentStillRecordsEnactment

def introduceOverwrites : Bool :=
  let old := member "a" [adminRole]
  let gs := stateWith [("a", old)]
  let changed := enact gs (.introduceMember "a" "replacement@example.test" [plainRole])
  lookupMember "a" changed ==
    some { key := "a", email := "replacement@example.test", roles := [plainRole] }

#guard introduceOverwrites

def applicationSignerDiscarded : Bool :=
  let gs := admins ["a", "b", "c"]
  let out := applyEvent digest (fun n event => n + event) gs "not-a-member" (.app 7)
  out.members == gs.members && out.pendingProposals == gs.pendingProposals &&
    out.appFold == 7

#guard applicationSignerDiscarded

def foldDoesNotValidate : Bool :=
  let p := proposalIntro "new"
  let gs := applyEvent digest (fun n event => n + event)
    (admins ["a", "b", "c"]) "outsider" (.base (.propose p))
  validateEvent validKey emptyConfig (admins ["a", "b", "c"])
      "outsider" (.base (.propose p)) == .error (.notAnAdmin "outsider") &&
    lookupPending (digest p) gs != none

#guard foldDoesNotValidate

def bootstrapAcceptsUninspectedSigner : Bool :=
  validateProposal validKey emptyConfig (emptyState 0) "stranger"
    (proposalIntro "founder" [privateAdminRole]) == .ok ()

#guard bootstrapAcceptsUninspectedSigner

def bootstrapNonMemberUsesProductionEnactment : Bool :=
  let p := proposalIntro "founder" [adminRole]
  let event : GroupEvent Nat := .base (.propose p)
  let outcome := applyEventDetailed digest (fun n appEvent => n + appEvent)
    (emptyState 0) "stranger" event
  let folded := foldGroup digest (fun n appEvent => n + appEvent) 0 [("stranger", event)]
  validateEvent validKey emptyConfig (emptyState 0) "stranger" event == .ok () &&
    outcome.state == folded && !isMember "stranger" folded &&
    lookupMember "founder" folded != none &&
    match outcome.enactment with
    | some enacted =>
        enacted.pending.proposer == "stranger" &&
          enacted.pending.approvals.contains "stranger"
    | none => false

#guard bootstrapNonMemberUsesProductionEnactment

def bootstrapRejectsNonAdminIntroduction : Bool :=
  validateProposal validKey emptyConfig (emptyState 0) "stranger"
    (proposalIntro "founder" [plainRole]) == .error .bootstrapRequiresAdmin

#guard bootstrapRejectsNonAdminIntroduction

def bootstrapRejectsOtherPayload : Bool :=
  validateProposal validKey emptyConfig (emptyState 0) "stranger"
    (.removeMember "nobody") == .error .bootstrapRequiresAdmin

#guard bootstrapRejectsOtherPayload

def currentAdminAccepted : Bool :=
  validateProposal validKey emptyConfig (admins ["a"]) "a" (proposalIntro "new") == .ok ()

#guard currentAdminAccepted

def currentNonAdminRejected : Bool :=
  let gs := stateWith
    [("a", member "a" [adminRole]), ("m", member "m" [plainRole])]
  validateProposal validKey emptyConfig gs "m" (proposalIntro "new") ==
    .error (.notAnAdmin "m")

#guard currentNonAdminRejected

def errorNotAMember : Bool :=
  validateEvent validKey emptyConfig (admins ["a"]) "missing" (.app 1) ==
    .error (.notAMember "missing")

#guard errorNotAMember

def errorNotAnAdmin : Bool := currentNonAdminRejected

#guard errorNotAnAdmin

def errorBootstrapRequiresAdmin : Bool := bootstrapRejectsNonAdminIntroduction

#guard errorBootstrapRequiresAdmin

def errorMemberAlreadyExists : Bool :=
  validateProposal validKey emptyConfig (admins ["a"]) "a" (proposalIntro "a") ==
    .error (.memberAlreadyExists "a")

#guard errorMemberAlreadyExists

def errorMemberNotFound : Bool :=
  validateProposal validKey emptyConfig (admins ["a"]) "a" (.removeMember "missing") ==
    .error (.memberNotFound "missing")

#guard errorMemberNotFound

def errorProposalNotFound : Bool :=
  validateApproval (admins ["a"]) "a" "missing" ==
    .error (.proposalNotFound "missing")

#guard errorProposalNotFound

def errorAlreadyApproved : Bool :=
  let p := proposalIntro "new"
  let gs := applyPropose digest (admins ["a", "b", "c"]) "a" p
  validateApproval gs "a" (digest p) == .error (.alreadyApproved "a" (digest p))

#guard errorAlreadyApproved

def errorRoleAddPrecondition : Bool :=
  validateProposal validKey blockedConfig (admins ["a"]) "a"
    (proposalIntro "new" [.appRole "blockedAdd"]) == .error (.roleAddPrecondition "blockedAdd")

#guard errorRoleAddPrecondition

def errorRoleRemovePrecondition : Bool :=
  let gs := stateWith
    [("a", member "a" [adminRole]), ("m", member "m" [.appRole "blockedRemove"])]
  validateProposal validKey blockedConfig gs "a" (.changeRoles "m" []) ==
    .error (.roleRemovePrecondition "blockedRemove")

#guard errorRoleRemovePrecondition

def errorInvalidKeyComesFirst : Bool :=
  validateProposal validKey emptyConfig (emptyState 0) "stranger"
    (proposalIntro "bad" []) == .error (.invalidKey "bad")

#guard errorInvalidKeyComesFirst

def adminRolesBypassPredicates : Bool :=
  let gs := stateWith
    [("a", member "a" [adminRole]), ("m", member "m" [privateAdminRole])]
  validateProposal validKey blockedConfig gs "a" (.changeRoles "m" [adminRole]) == .ok ()

#guard adminRolesBypassPredicates

def unknownApplicationRolePermitted : Bool :=
  validateProposal validKey blockedConfig (admins ["a"]) "a"
    (proposalIntro "new" [.appRole "unknown"]) == .ok ()

#guard unknownApplicationRolePermitted

#check @approvals_nodup
#check @proposer_mem_approvals
#check @enact_implies_threshold_met
#check @members_change_implies_enacted
#check @member_key_coherent
#check majority_table
#check @majority_not_strict_on_even

example {β : Type} (eventDigest : Proposal → ProposalId)
    (appFoldFn : AppFold β) (gs : GroupState β) (signer : Key)
    (event : GroupEvent β) (enacted : Enactment β)
    (h : (applyEventDetailed eventDigest appFoldFn gs signer event).enactment =
      some enacted) :
    enacted.pending.approvals.length ≥ majority enacted.preState :=
  enact_implies_threshold_met eventDigest appFoldFn gs signer event enacted h

end Tests
end KelGroups
