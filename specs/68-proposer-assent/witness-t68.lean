import KelGroups.Tests
import KelGroups.Integration

/-!
# T68 witness driver — ticket-owner oracle for #68 (V-2 ruling, A-001 reading).

READ-ONLY FOR THE IMPLEMENTATION WORKER (fenced in the owner packet): the
worker may neither edit this file nor tailor proofs to its spelling. It is
the independent executable encoding of R68-09, cross-checking the
worker-owned `#guard` theorems in `Tests.lean`.

Execution: AFTER `lake build`, via
`cd lean && lake env lean ../specs/68-proposer-assent/witness-t68.lean`
(gate leg 5). The file lives OUTSIDE lean/ on purpose:
`scripts/check-reactivegas-inversion-coverage` quantifies over every lean/
file as a lake target, so an unimported oracle inside lean/ breaks leg 2
with `unknown target` (observed 2026-09-05); imports resolve through the
lake-built oleans, not through file placement. Every `#guard`
elaborate-fails RED on a wrong candidate. On the
pre-change base the whole file must RED for the intended semantic reasons
(proposals open non-empty; proposer credit enacts or mislabels) — that run is
the base falsification proof.

Every scenario goes through reachable calls (`applyPropose` / `applyApprove`
chains on inhabited states, `foldIntegrated`, stepwise
`applyIntegratedEvent` where the intermediate refusal itself is asserted) —
never bare helpers such as `approvePending` in isolation.
-/

namespace T68Witness

open KelGroups

/-! ## Historical path (`pendingProposals`) -/

/-- A proposal opens with zero approvals and records its proposer. -/
def hEmptyOpen : Bool :=
  let gs := applyPropose Tests.digest (Tests.admins ["a", "b"]) "a" (.removeMember "b")
  match lookupPending "remove:b" gs with
  | some pp => pp.approvals == [] && pp.proposer == "a"
  | none => false

#guard hEmptyOpen

/-- n=2: another admin's approval enacts. -/
def hN2OtherEnacts : Bool :=
  let gs1 := applyPropose Tests.digest (Tests.admins ["a", "b"]) "a" (.removeMember "b")
  let gs2 := applyApprove gs1 "b" "remove:b"
  lookupPending "remove:b" gs2 == none && lookupMember "b" gs2 == none

#guard hN2OtherEnacts

/-- n=1: propose pends, then the sole admin's separate approval enacts
(agency preserved; one-event enactment gone). -/
def hN1TwoStep : Bool :=
  let gs1 := applyPropose Tests.digest (Tests.admins ["a"]) "a" (.removeMember "a")
  let gs2 := applyApprove gs1 "a" "remove:a"
  (match lookupPending "remove:a" gs1 with
    | some pp => pp.approvals == []
    | none => false)
  && lookupPending "remove:a" gs2 == none
  && lookupMember "a" gs2 == none

#guard hN1TwoStep

/-- n=3: one other approval still pends; the second enacts. -/
def hN3 : Bool :=
  let p : Proposal := .removeMember "b"
  let gs1 := applyPropose Tests.digest (Tests.admins ["a", "b", "c"]) "a" p
  let gs2 := applyApprove gs1 "c" (Tests.digest p)
  let gs3 := applyApprove gs2 "b" (Tests.digest p)
  (lookupPending (Tests.digest p) gs2 != none)
  && lookupPending (Tests.digest p) gs3 == none
  && lookupMember "b" gs3 == none

#guard hN3

/-- Historical admissibility boundary: above n=1 the proposer's own approval
is refused, and the refusal is NOT `alreadyApproved` (different meaning). -/
def hValidBar : Bool :=
  let gs1 := applyPropose Tests.digest (Tests.admins ["a", "b"]) "a" (.removeMember "b")
  match validateApproval gs1 "a" "remove:b" with
  | .error (.alreadyApproved _ _) => false
  | .error _ => true
  | .ok _ => false

#guard hValidBar

/-! ## Integrated path (`pendingBase`, the production root) -/

/-- Minimal concrete contract bundle: identity reading of `BaseMutation`,
permissive sealed hook (the hook still RUNS on every commit). -/
def ig : Integration Unit Empty BaseMutation Empty where
  reserved := "zz-reserved"
  digest := fun m => match m with
    | .removeMember k => "rm:" ++ k
    | .changeRoles k _ => "ch:" ++ k
  proposalMutation := id
  appFold := fun _ _ _ _ e => nomatch e
  baseHook := fun _ _ _ _ => .ok ()

/-- Inhabited aggregate with the given admin keys. -/
def iadm (keys : List Key) : GroupState Unit :=
  { members := keys.map fun k =>
      (k, { key := k, email := k ++ "@example.test", roles := [Tests.adminRole] })
    pendingProposals := [], pendingBase := [], appFold := () }

/-- A proposal opens with zero approvals and records its proposer. -/
def iEmptyOpen : Bool :=
  match applyIntegratedEvent ig (iadm ["a", "b"]) "a" (.propose (.removeMember "b")) with
  | .ok r => match lookupPendingBase "rm:b" r.state with
    | some pb => pb.approvals == [] && pb.proposer == "a"
    | none => false
  | .error _ => false

#guard iEmptyOpen

/-- n=2: the proposer's own approval is refused at the boundary, and the
refusal is NOT `alreadyApproved`. -/
def iSelfRefused : Bool :=
  match applyIntegratedEvent ig (iadm ["a", "b"]) "a" (.propose (.removeMember "b")) with
  | .ok r => match applyIntegratedEvent ig r.state "a" (.approve "rm:b") with
    | .error (.validation (.alreadyApproved _ _)) => false
    | .error _ => true
    | .ok _ => false
  | .error _ => false

#guard iSelfRefused

/-- n=2: another admin's approval enacts. -/
def iN2OtherEnacts : Bool :=
  let gs := foldIntegrated ig (iadm ["a", "b"])
    [("a", .propose (.removeMember "b")), ("b", .approve "rm:b")]
  lookupPendingBase "rm:b" gs == none && lookupMember "b" gs == none

#guard iN2OtherEnacts

/-- n=1: propose pends, then the sole admin's separate approval enacts. -/
def iN1TwoStep : Bool :=
  let pends := foldIntegrated ig (iadm ["a"])
    [("a", .propose (.removeMember "a"))]
  let enacted := foldIntegrated ig (iadm ["a"])
    [("a", .propose (.removeMember "a")), ("a", .approve "rm:a")]
  (match lookupPendingBase "rm:a" pends with
    | some pb => pb.approvals == []
    | none => false)
  && lookupPendingBase "rm:a" enacted == none
  && lookupMember "a" enacted == none

#guard iN1TwoStep

/-- n=3 B-counterexample (kept as negative witness): proposer self-approval
is refused so only ONE other assent stands — the proposal stays PENDING. A
length-plus-exists-non-proposer guard would enact here (length 2); the ruling
table demands pending. -/
def iN3Killer : Bool :=
  let gs := foldIntegrated ig (iadm ["a", "b", "c"])
    [("a", .propose (.removeMember "b")),
     ("a", .approve "rm:b"),
     ("c", .approve "rm:b")]
  (match lookupPendingBase "rm:b" gs with
    | some pb => pb.approvals == ["c"]
    | none => false)
  && lookupMember "b" gs != none

#guard iN3Killer

/-- n=3: two other approvals enact. -/
def iN3TwoOthers : Bool :=
  let gs := foldIntegrated ig (iadm ["a", "b", "c"])
    [("a", .propose (.removeMember "b")),
     ("c", .approve "rm:b"),
     ("b", .approve "rm:b")]
  lookupPendingBase "rm:b" gs == none && lookupMember "b" gs == none

#guard iN3TwoOthers

/-- n=5: two other approvals still pend; the third enacts. -/
def iN5 : Bool :=
  let two := foldIntegrated ig (iadm ["a", "b", "c", "d", "e"])
    [("a", .propose (.removeMember "e")),
     ("b", .approve "rm:e"),
     ("c", .approve "rm:e")]
  let three := foldIntegrated ig (iadm ["a", "b", "c", "d", "e"])
    [("a", .propose (.removeMember "e")),
     ("b", .approve "rm:e"),
     ("c", .approve "rm:e"),
     ("d", .approve "rm:e")]
  (lookupPendingBase "rm:e" two != none)
  && lookupPendingBase "rm:e" three == none
  && lookupMember "e" three == none

#guard iN5

/-- Changing canonical admin count: proposed at n=2, a third admin is
admitted via the direct route, one other approval still pends (threshold now
2 from current membership), the second other approval enacts. -/
def iAdminChange : Bool :=
  let base := [("a", .propose (.removeMember "b")),
               ("a", .direct (.admitMember "c" "c@example.test" [Tests.adminRole]))]
  let one := foldIntegrated ig (iadm ["a", "b"]) (base ++ [("c", .approve "rm:b")])
  let two := foldIntegrated ig (iadm ["a", "b"])
    (base ++ [("c", .approve "rm:b"), ("b", .approve "rm:b")])
  (lookupPendingBase "rm:b" one != none)
  && lookupPendingBase "rm:b" two == none
  && lookupMember "b" two == none

#guard iAdminChange

end T68Witness
