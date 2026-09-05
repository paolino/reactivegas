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

/-- V-2: the proposer-credit expectation is retired; empty-open is pinned
by `t68HistEmptyOpen` below. -/

def proposeReplacesPending : Bool :=
  let gs0 := admins ["a", "b", "c", "d", "e"]
  let p := proposalIntro "new"
  let gs1 := applyPropose digest gs0 "a" p
  let gs2 := applyApprove gs1 "b" (digest p)
  let gs3 := applyPropose digest gs2 "c" p
  match lookupPending (digest p) gs3 with
  | some pp => pp.proposer == "c" && pp.approvals == []
  | none => false

#guard proposeReplacesPending

def duplicateApprovalIsIdempotent : Bool :=
  let gs0 := admins ["a", "b", "c", "d", "e"]
  let p := proposalIntro "new"
  let gs1 := applyPropose digest gs0 "a" p
  let gs2 := applyApprove gs1 "b" (digest p)
  let gs3 := applyApprove gs2 "b" (digest p)
  match lookupPending (digest p) gs3 with
  | some pp => pp.approvals.length == 1 && pp.approvals == ["b"]
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
  let gs3 := applyApprove gs2 "c" (digest p)
  lookupMember "new" gs1 == none && lookupPending (digest p) gs1 != none &&
    lookupMember "new" gs2 == none && lookupPending (digest p) gs2 != none &&
    lookupMember "new" gs3 != none && lookupPending (digest p) gs3 == none

#guard oddBoundaryWaitsThenEnacts

def evenBoundaryIsNotStrict : Bool :=
  let p := proposalIntro "new"
  let gs1 := applyPropose digest (admins ["a", "b", "c", "d"]) "a" p
  let gs2 := applyApprove gs1 "b" (digest p)
  let gs3 := applyApprove gs2 "c" (digest p)
  lookupMember "new" gs1 == none && lookupMember "new" gs2 == none &&
    lookupMember "new" gs3 != none

#guard evenBoundaryIsNotStrict

def preEnactmentMajorityControlsAdminIntroduction : Bool :=
  let p := proposalIntro "c" [adminRole]
  let gs1 := applyPropose digest (admins ["a", "b"]) "a" p
  let gs2 := applyApprove gs1 "b" (digest p)
  lookupMember "c" gs1 == none && lookupPending (digest p) gs1 != none &&
    lookupMember "c" gs2 != none && lookupPending (digest p) gs2 == none

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
  let proposed := applyPropose digest before "a" p
  let outcome := applyEventDetailed digest (fun n event => n + event)
    proposed "a" (.base (.approve (digest p)))
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
  let proposed := applyPropose digest before "a" p
  let outcome := applyEventDetailed digest (fun n event => n + event)
    proposed "a" (.base (.approve (digest p)))
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
          enacted.pending.approvals == []
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
  let gs2 := applyApprove gs "b" (digest p)
  validateApproval gs2 "b" (digest p) == .error (.alreadyApproved "b" (digest p))

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
#check @proposer_absent_above_one
#check @sole_admin_self_approval_ok
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

/-! ## T68 worker guards (RED on the proposer-credit base)

These mirror the fenced ticket-owner oracle through reachable calls
(raw `applyPropose`/`applyApprove` chains and `applyIntegratedEvent` /
`foldIntegrated`, never bare helpers). Every refusal guard pins the
pending-shape precondition first: a vacuous pass is a defect. On the
pre-change base each guard below is FALSE for the intended semantic
reason (proposals open non-empty; proposer credit enacts or mislabels
the refusal); the V-2 implementation plus the old-regime rewrites make
them true. -/

def t68ig : Integration Unit Empty BaseMutation Empty where
  reserved := "zz-reserved"
  digest := fun m => match m with
    | .removeMember k => "rm:" ++ k
    | .changeRoles k _ => "ch:" ++ k
  proposalMutation := id
  appFold := fun _ _ _ _ e => nomatch e
  baseHook := fun _ _ _ _ => .ok ()

def t68adm (keys : List Key) : GroupState Unit :=
  { members := keys.map fun k =>
      (k, { key := k, email := k ++ "@example.test", roles := [adminRole] })
    pendingProposals := [], pendingBase := [], appFold := () }

def t68HistEmptyOpen : Bool :=
  let gs := applyPropose digest (admins ["a", "b"]) "a" (.removeMember "b")
  match lookupPending "remove:b" gs with
  | some pp => pp.approvals == [] && pp.proposer == "a"
  | none => false

#guard t68HistEmptyOpen

def t68HistN2UnilateralPends : Bool :=
  let gs := applyPropose digest (admins ["a", "b"]) "a" (.removeMember "b")
  lookupPending "remove:b" gs != none && lookupMember "b" gs != none

#guard t68HistN2UnilateralPends

def t68HistN3Killer : Bool :=
  let p : Proposal := .removeMember "b"
  let gs1 := applyPropose digest (admins ["a", "b", "c"]) "a" p
  let gs2 := applyApprove gs1 "c" (digest p)
  (lookupPending (digest p) gs2 != none)
  && lookupMember "b" gs2 != none

#guard t68HistN3Killer

def t68HistSelfBar : Bool :=
  let gs1 := applyPropose digest (admins ["a", "b"]) "a" (.removeMember "b")
  match lookupPending "remove:b" gs1 with
  | some pp => pp.approvals == [] && match validateApproval gs1 "a" "remove:b" with
    | .error (.alreadyApproved _ _) => false
    | .error _ => true
    | .ok _ => false
  | none => false

#guard t68HistSelfBar

def t68IntEmptyOpen : Bool :=
  match applyIntegratedEvent t68ig (t68adm ["a", "b"]) "a"
      (.propose (.removeMember "b")) with
  | .ok r => match lookupPendingBase "rm:b" r.state with
    | some pb => pb.approvals == [] && pb.proposer == "a"
    | none => false
  | .error _ => false

#guard t68IntEmptyOpen

def t68IntSelfRefused : Bool :=
  match applyIntegratedEvent t68ig (t68adm ["a", "b"]) "a"
      (.propose (.removeMember "b")) with
  | .ok r => match lookupPendingBase "rm:b" r.state with
    | some pb => pb.approvals == [] &&
      match applyIntegratedEvent t68ig r.state "a" (.approve "rm:b") with
      | .error (.validation (.alreadyApproved _ _)) => false
      | .error _ => true
      | .ok _ => false
    | none => false
  | .error _ => false

#guard t68IntSelfRefused

def t68IntN1TwoStep : Bool :=
  let pends := foldIntegrated t68ig (t68adm ["a"])
    [("a", .propose (.removeMember "a"))]
  let enacted := foldIntegrated t68ig (t68adm ["a"])
    [("a", .propose (.removeMember "a")), ("a", .approve "rm:a")]
  (match lookupPendingBase "rm:a" pends with
    | some pb => pb.approvals == []
    | none => false)
  && lookupPendingBase "rm:a" enacted == none
  && lookupMember "a" enacted == none

#guard t68IntN1TwoStep

def t68IntN5TwoPend : Bool :=
  let two := foldIntegrated t68ig (t68adm ["a", "b", "c", "d", "e"])
    [("a", .propose (.removeMember "e")),
     ("b", .approve "rm:e"),
     ("c", .approve "rm:e")]
  lookupPendingBase "rm:e" two != none && lookupMember "e" two != none

#guard t68IntN5TwoPend

def t68IntAdminChange : Bool :=
  let base := [("a", .propose (.removeMember "b")),
               ("a", .direct (.admitMember "c" "c@example.test" [adminRole]))]
  let one := foldIntegrated t68ig (t68adm ["a", "b"])
    (base ++ [("c", .approve "rm:b")])
  (lookupPendingBase "rm:b" one != none) && lookupMember "b" one != none

#guard t68IntAdminChange

def t68HistN1TwoStep : Bool :=
  let gs1 := applyPropose digest (admins ["a"]) "a" (.removeMember "a")
  let gs2 := applyApprove gs1 "a" "remove:a"
  (match lookupPending "remove:a" gs1 with
    | some pp => pp.approvals == []
    | none => false)
  && lookupPending "remove:a" gs2 == none
  && lookupMember "a" gs2 == none

#guard t68HistN1TwoStep

def t68HistN2OtherEnacts : Bool :=
  let gs1 := applyPropose digest (admins ["a", "b"]) "a" (.removeMember "b")
  let gs2 := applyApprove gs1 "b" "remove:b"
  lookupPending "remove:b" gs2 == none && lookupMember "b" gs2 == none

#guard t68HistN2OtherEnacts

def t68HistN3TwoOthers : Bool :=
  let p : Proposal := .removeMember "b"
  let gs1 := applyPropose digest (admins ["a", "b", "c"]) "a" p
  let gs2 := applyApprove gs1 "c" (digest p)
  let gs3 := applyApprove gs2 "b" (digest p)
  lookupPending (digest p) gs3 == none && lookupMember "b" gs3 == none

#guard t68HistN3TwoOthers

def t68IntN2OtherEnacts : Bool :=
  let gs := foldIntegrated t68ig (t68adm ["a", "b"])
    [("a", .propose (.removeMember "b")), ("b", .approve "rm:b")]
  lookupPendingBase "rm:b" gs == none && lookupMember "b" gs == none

#guard t68IntN2OtherEnacts

def t68IntN3TwoOthers : Bool :=
  let gs := foldIntegrated t68ig (t68adm ["a", "b", "c"])
    [("a", .propose (.removeMember "b")),
     ("c", .approve "rm:b"),
     ("b", .approve "rm:b")]
  lookupPendingBase "rm:b" gs == none && lookupMember "b" gs == none

#guard t68IntN3TwoOthers

/-! ## T68 retained raw counterexample (F-01 repair, T68-25 correction 5)

Worker-owned mirror of the auditor's 7-event ScopeWitness (instrument
source sha `3b4229fc` vs executed run sha `0a2799b7` — cited each for
what it is: the SOURCE is the archived instrument file, the RUN is the
executed evidence hash from the audit report; this family re-derives the
shape from Tests fixtures and never imports the auditor file as
authority). Scope, accurately stated: events 1-6 are
boundary-admissible (each `validateEvent`-ok, proved below — stronger
than final-decision-only); event 7 is boundary-REFUSED
(`proposerSelfApproval`) yet raw-executed by `foldGroup`; the final
state exhibits the indexed-violation shape (proposer-credit approvals
`["a"]` above n=1 with the member still present). This family must
stay GREEN: it pins the excluded raw domain (if the raw fold ever
validated, the violation guards fail and signal the change). -/

def t68RawTrace : List (Key × GroupEvent Nat) :=
  [ ("stranger", .base (.propose (proposalIntro "a" [adminRole])))
  , ("a", .base (.propose (proposalIntro "b" [adminRole])))
  , ("a", .base (.approve "introduce:b"))
  , ("a", .base (.propose (proposalIntro "c" [adminRole])))
  , ("b", .base (.approve "introduce:c"))
  , ("a", .base (.propose (.removeMember "c")))
  ]

def t68RawTraceValidFrom (gs : GroupState Nat) :
    List (Key × GroupEvent Nat) → Bool
  | [] => true
  | (signer, event) :: rest =>
      validateEvent validKey emptyConfig gs signer event == .ok () &&
        t68RawTraceValidFrom (applyEvent digest (fun n _ => n) gs signer event) rest

def t68RawBefore : GroupState Nat :=
  foldGroup digest (fun n _ => n) 0 t68RawTrace

def t68RawAfter : GroupState Nat :=
  foldGroup digest (fun n _ => n) 0
    (t68RawTrace ++ [("a", .base (.approve "remove:c"))])

#guard adminCount t68RawBefore == 3
#guard (lookupPending "remove:c" t68RawBefore).map (·.approvals) == some []
#guard t68RawTraceValidFrom (emptyState 0) t68RawTrace == true
#guard validateApproval t68RawBefore "a" "remove:c" ==
  .error (.proposerSelfApproval "a" "remove:c")
#guard adminCount t68RawAfter == 3
#guard (lookupPending "remove:c" t68RawAfter).map (·.approvals) == some ["a"]
#guard (lookupPending "remove:c" t68RawAfter).map (·.proposer) == some "a"
#guard lookupMember "c" t68RawAfter != none

end Tests
end KelGroups
