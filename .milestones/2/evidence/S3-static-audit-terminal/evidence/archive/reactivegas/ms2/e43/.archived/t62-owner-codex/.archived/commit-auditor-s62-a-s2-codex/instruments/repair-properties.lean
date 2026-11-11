import Reactivegas

namespace S62A2Audit

/- A non-degenerate app transition: two distinct members, one nonempty open
   vote question, and an economic donation that must move the payload. -/
def checkPayloadCarry : Bool :=
  match Reactivegas.preservationDonate with
  | .ok result =>
      result.state.members == Reactivegas.preservationGroup.members &&
        result.state.appFold.votes == Reactivegas.preservationGroup.appFold.votes &&
        result.state.appFold.votes.openQuestions.length == 1 &&
        !(result.state.appFold == Reactivegas.preservationGroup.appFold) &&
        result.change == none
  | .error _ => false

/- The production boundary rejects an arbitrary aggregate containing the
   reserved comune key, while accepting a well-formed non-comune aggregate. -/
def checkReservedBoundary : Bool :=
  Reactivegas.checkComuneCannotAuthorize &&
    (Reactivegas.boot Reactivegas.preservationGroup.members
      Reactivegas.preservationGroup.appFold).isSome &&
    (Reactivegas.boot [(comuneId, Reactivegas.comuneAdminMember)] State.empty).isNone

/- The shipped mutant is an actual member-writing transition result, not an
   alternate expected fixture. Its payload still moved, so rejection/no-op
   cannot make the detector pass. -/
def checkMemberWritingMutantApplied : Bool :=
  Reactivegas.checkAppMembersPreservationMutant &&
    match Reactivegas.memberWritingApply with
    | .ok result =>
        result.state.members == Reactivegas.preservationGroup.members.tail &&
          !(result.state.members == Reactivegas.preservationGroup.members) &&
          !(result.state.appFold == Reactivegas.preservationGroup.appFold)
    | .error _ => false

/- Supply the unresolved #47 decision explicitly in both directions. This
   checks that the production fold executes without evaluating the legacy
   sorry-backed policy and that the argument is genuinely observed. -/
def backdonateGroup : KelGroups.GroupState State :=
  { Reactivegas.preservationGroup with
    appFold :=
      { Reactivegas.preservationGroup.appFold with
        conti := [(comuneId, 10)] } }

def backdonateAllowed :=
  Reactivegas.apply KelGroups.Vote.legacyThreshold (fun _ _ => true)
    backdonateGroup "alice"
    (KelGroups.IntegratedEvent.app (AppEvent.backdonate 1))

def backdonateDenied :=
  Reactivegas.apply KelGroups.Vote.legacyThreshold (fun _ _ => false)
    backdonateGroup "alice"
    (KelGroups.IntegratedEvent.app (AppEvent.backdonate 1))

def checkBackdonateAuthIsExplicitAndExecutable : Bool :=
  match backdonateAllowed, backdonateDenied with
  | .ok allowed, .error (.integrated (.app .rejected)) =>
      bal allowed.state.appFold.conti comuneId == 8 &&
        bal allowed.state.appFold.conti "alice" == 1 &&
        bal allowed.state.appFold.conti "bob" == 1 &&
        allowed.state.appFold.votes == backdonateGroup.appFold.votes &&
        allowed.state.members == backdonateGroup.members
  | _, _ => false

#guard checkPayloadCarry
#guard checkReservedBoundary
#guard checkMemberWritingMutantApplied
#guard checkBackdonateAuthIsExplicitAndExecutable

#eval IO.println
  "AUDIT-PROPERTIES PASS payload=carried comune=refused mutant=applied backdonate-auth=explicit"

#print axioms Reactivegas.appFold
#print axioms Reactivegas.apply
#print axioms Reactivegas.app_members_preservation_holds
#print axioms Reactivegas.comune_cannot_authorize
#print axioms Reactivegas.memberWritingApply
#print axioms stepEvent
#print axioms backdonateAuthorized

end S62A2Audit
