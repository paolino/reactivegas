set_option maxHeartbeats 8000000
open Lean Elab Command Meta in
run_cmd do
  let env ← getEnv
  let mods ← IO.FS.lines "/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final-r2/instruments/modules.txt"
  let mut count := 0
  let mut bad := false
  for (n, ci) in env.constants.toList do
    let .thmInfo _ := ci | continue
    let owned := match env.getModuleIdxFor? n with
      | some i => match env.header.moduleNames[i]? with
        | some m => mods.contains m.toString
        | none => false
      | none => n.toString == "auditForbiddenTheorem"
    if !owned then continue
    count := count + 1
    if count > 20000 then throwError "AXIOM extent resource bound exceeded"
    let axs ← liftTermElabM (collectAxioms n)
    elabCommand (← `(command| #print axioms $(mkIdent n)))
    for a in axs do
      if !([`propext, `Classical.choice, `Quot.sound].contains a) then
        logError m!"AUDIT-FORBIDDEN-AXIOM {n} {a}"
        bad := true
  if count == 0 then throwError "AUDIT-AXIOM-EMPTY"
  if bad then throwError "AUDIT-AXIOM-FAILED"
  logInfo m!"AUDIT-AXIOM-OK count={count}"
