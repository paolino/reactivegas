import KelGroups.Mirrors

namespace AuditGroups
open KelGroups
open KelGroups.Vote

def check (rows : Array (String × Bool)) : IO Unit := do
  let mut failed := false
  for (name, ok) in rows do
    IO.println s!"WITNESS {name} {ok}"
    if !ok then failed := true
  if failed then throw (IO.userError "WITNESS-FAILED")
def admin : Member := ⟨"a", "a@audit", [.adminRole .publicAdmin]⟩
def user : Member := ⟨"u", "u@audit", [.appRole "buyer"]⟩
def view : GroupView := ⟨[("a", admin), ("u", user)]⟩
def pending : PendingProposal := ⟨.removeMember "u", "a", ["a"]⟩
def gs : GroupState Nat := { members := view.members, pendingProposals := [("p", pending)], pendingBase := [], appFold := 73 }
def q : Question := ⟨.collective, "a", ["a"], ["u"]⟩
def qbad : Question := {q with assents := ["a", "a"]}
def closed : ClosureRecord := ⟨"closed", q, .positive, .tally⟩
def votes : VoteState := ⟨[("open", q)], [closed]⟩
def rows : Array (String × Bool) := #[
  ("K1-valid-proposer-assent", pendingWellFormedB pending),
  ("K1-duplicate-approval", !pendingWellFormedB {pending with approvals := ["a", "a"]}),
  ("K1-proposer-absent", !pendingWellFormedB {pending with approvals := ["u"]}),
  ("K2-nonempty-coherent", membersCoherentB gs),
  ("K2-key-mismatch", !membersCoherentB {gs with members := [("wrong", admin)]}),
  ("K3-nonempty-pending-coherent", pendingCoherentB gs),
  ("K3-invalid-pending", !pendingCoherentB {gs with pendingProposals := [("p", {pending with approvals := []})]}),
  ("K4-nonempty-wellformed", wellFormedB gs),
  ("K4-duplicate-member-key", !wellFormedB {gs with members := [("a", admin), ("a", admin)]}),
  ("K4-duplicate-proposal-key", !wellFormedB {gs with pendingProposals := [("p", pending), ("p", pending)]}),
  ("K4-incoherent-member", !wellFormedB {gs with members := [("wrong", admin)]}),
  ("K4-incoherent-pending", !wellFormedB {gs with pendingProposals := [("p", {pending with approvals := []})]}),
  ("K5-enacted-exact-state", enactsB gs "p" (tryEnactDetailed gs "p").state),
  ("K5-different-Nat-payload", !enactsB gs "p" {(tryEnactDetailed gs "p").state with appFold := 74}),
  ("K5-absent-proposal", !enactsB gs "absent" gs),
  ("V1-disjoint-nonempty", questionCleanB q),
  ("V1-duplicate-assent", !questionCleanB qbad),
  ("V1-duplicate-dissent", !questionCleanB {q with dissents := ["u", "u"]}),
  ("V1-overlapping-ballots", !questionCleanB {q with dissents := ["a"]}),
  ("V2-nonempty-carriers", sweepReadyB view votes),
  ("V2-open-duplicate-key", !sweepReadyB view {votes with openQuestions := [("open", q), ("open", qbad)]}),
  ("V2-closed-duplicate-key", !sweepReadyB view {votes with closed := [closed, closed]}),
  ("V2-overlapping-key", !sweepReadyB view {votes with openQuestions := [("closed", q)]}),
  ("V2-open-unclean", !sweepReadyB view {votes with openQuestions := [("open", qbad)]}),
  ("V2-closed-unclean", !sweepReadyB view {votes with closed := [{closed with question := qbad}]}),
  ("V2-closed-open-verdict", !sweepReadyB view {votes with closed := [{closed with verdict := .open}]}),
  ("V3-callable-policy-two", voteWellFormedB (fun n => n + 1) view votes),
  ("V3-callable-policy-one", !voteWellFormedB (fun _ => 1) view votes),
  ("V3-dependent-sweep-shape", !voteWellFormedB (fun n => n + 1) view {votes with closed := [closed, closed]})]
#eval check rows
end AuditGroups
