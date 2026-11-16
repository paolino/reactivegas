import fs from 'node:fs';import {execFileSync} from 'node:child_process';
const root='/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-codex-r3';
const modules=execFileSync('git',['ls-files','lean/*.lean'],{encoding:'utf8'}).trim().split('\n').filter(x=>x!=='lean/lakefile.lean').map(x=>x.slice(5,-5).replaceAll('/','.'));
const src=modules.map(m=>`import ${m}`).join('\n')+`
import Lean

def auditPositive := True
def auditAlias := auditPositive
def auditNegative (_ : Prop) : Nat := 7

def ownedModules : Array String := #[${modules.map(JSON.stringify).join(',')}]
open Lean Elab Command Meta in
run_cmd do
  let env ← getEnv
  let mods := env.header.moduleNames
  let mut names : Array String := #[]
  for (n, ci) in env.constants.toList do
    let admitted := match ci with
      | .defnInfo _ => true
      | .inductInfo _ => true
      | .opaqueInfo _ => true
      | .axiomInfo _ => true
      | _ => false
    unless admitted do continue
    let home := match env.getModuleIdxFor? n with
      | some i => toString (mods[i]!)
      | none => "<driver>"
    unless ownedModules.contains home || ["auditPositive", "auditAlias", "auditNegative"].contains (toString n) do continue
    let prop ← liftTermElabM <| forallTelescopeReducing ci.type fun _ cod => do
      let c ← whnf cod
      pure (c == .sort .zero)
    if prop then
      names := names.push (toString n)
      logInfo m!"INVENTORY-PROP {home} :: {n} :: {ci.type}"
  unless names.contains "auditPositive" && names.contains "auditAlias" && !names.contains "auditNegative" do
    throwError "INVENTORY-CLASSIFIER-CONTROL-FAIL"
  names := names.filter fun n => n != "auditPositive" && n != "auditAlias"
  let control ← IO.getEnv "AUDIT_EMPTY_CONTROL"
  if control == some "1" then names := #[]
  if names.isEmpty then throwError "INVENTORY-EMPTY-REJECTED"
  logInfo m!"INVENTORY-COUNT {names.size}"
  for n in names.qsort (· < ·) do logInfo m!"INVENTORY-NAME {n}"
`;
fs.writeFileSync(`${root}/instruments/Inventory.lean`,src);fs.writeFileSync(`${root}/evidence/source-modules.json`,JSON.stringify(modules,null,2)+'\n');console.log(`${modules.length} Git-derived modules imported`);
