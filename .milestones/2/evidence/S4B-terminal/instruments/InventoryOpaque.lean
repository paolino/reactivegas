import KelGroups
import KelGroups.Event
import KelGroups.Fold
import KelGroups.Integration
import KelGroups.Invariants
import KelGroups.Mirrors
import KelGroups.State
import KelGroups.Tests
import KelGroups.Types
import KelGroups.Validate
import KelGroups.Vote.Event
import KelGroups.Vote.Fold
import KelGroups.Vote.Invariants
import KelGroups.Vote.State
import KelGroups.Vote.Tests
import KelGroups.Vote.Types
import KelGroups.Vote.Validate
import Reactivegas
import Reactivegas.Composition
import Reactivegas.CorpusExport
import Reactivegas.CorpusGate
import Reactivegas.Invariants
import Reactivegas.Mirrors
import Reactivegas.Predicates
import Reactivegas.State
import Reactivegas.Step
import Reactivegas.Trace
import Reactivegas.TraceTests
import Reactivegas.Types
import Lean

def auditPositive := True
def auditAlias := auditPositive
def auditNegative (_ : Prop) : Nat := 7

def ownedModules : Array String := #["KelGroups","KelGroups.Event","KelGroups.Fold","KelGroups.Integration","KelGroups.Invariants","KelGroups.Mirrors","KelGroups.State","KelGroups.Tests","KelGroups.Types","KelGroups.Validate","KelGroups.Vote.Event","KelGroups.Vote.Fold","KelGroups.Vote.Invariants","KelGroups.Vote.State","KelGroups.Vote.Tests","KelGroups.Vote.Types","KelGroups.Vote.Validate","Reactivegas","Reactivegas.Composition","Reactivegas.CorpusExport","Reactivegas.CorpusGate","Reactivegas.Invariants","Reactivegas.Mirrors","Reactivegas.Predicates","Reactivegas.State","Reactivegas.Step","Reactivegas.Trace","Reactivegas.TraceTests","Reactivegas.Types"]
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
open Lean Elab Command in
run_cmd do
  let env ← getEnv
  match env.find? `auditOpaque with
  | some (.opaqueInfo v) => logInfo m!"OPAQUE-COMPILED-TYPE {v.type}"
  | _ => throwError "OPAQUE-CONTROL-NOT-COMPILED"
