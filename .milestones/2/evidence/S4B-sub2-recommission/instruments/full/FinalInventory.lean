import Lean
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

set_option maxRecDepth 4096
set_option maxHeartbeats 8000000

open Lean Elab Command Meta in
run_cmd do
  let env ← getEnv
  let sp ← Lean.searchPathRef.get
  let root ← IO.FS.realPath "/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final-r2/worlds/S10/lean/.lake/build/lib/lean"
  let artifactRootPrefix := root.toString ++ "/"
  let out ← IO.FS.Handle.mk "/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final-r2/evidence/full/FinalInventory.jsonl" .write
  let bytes ← IO.mkRef (0 : Nat)
  let emit (j : Json) : CommandElabM Unit := do
    let line := j.compress ++ "\n"
    let size := (← bytes.get) + line.toUTF8.size
    if size > 268435456 then throwError "M1 output bound exceeded: 256 MiB; partial measurement only"
    bytes.set size
    out.putStr line
  let mut owned : Array String := #[]
  for m in env.header.moduleNames do
    let some p ← sp.findModuleWithExt "olean" m | throwError "M1 unresolved artifact: {m}"
    let path ← IO.FS.realPath p
    let localArtifact := artifactRootPrefix.isPrefixOf path.toString
    if localArtifact then owned := owned.push m.toString
    emit <| Json.mkObj [("record", toJson "module"), ("name", toJson m.toString),
      ("artifact", toJson path.toString), ("owned", toJson localArtifact)]
  if owned.isEmpty then throwError "M1 empty owned module inventory"
  let tracked ← IO.FS.lines "/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final-r2/instruments/modules.txt"
  for m in tracked do
    if !owned.contains m then throwError "M1 tracked module not loaded from owned output: {m}"
  for m in owned do
    if !tracked.contains m then throwError "M1 owned output has no tracked source: {m}"
  let mut count := 0
  let mut predicates := 0
  let mut unknown := 0
  for (n, ci) in env.constants.toList do
    let some idx := env.getModuleIdxFor? n | continue
    let some m := env.header.moduleNames[idx]? | throwError "M1 invalid module index: {n}"
    if !owned.contains m.toString then continue
    count := count + 1
    if count > 20000 then throwError "M1 owned declaration bound exceeded: 20000; no denominator inferred"
    let kind := match ci with
      | .axiomInfo _ => "axiom"
      | .defnInfo _ => "defn"
      | .thmInfo _ => "theorem"
      | .opaqueInfo _ => "opaque"
      | .quotInfo _ => "quot"
      | .inductInfo _ => "inductive"
      | .ctorInfo _ => "constructor"
      | .recInfo _ => "recursor"
    let result ← liftTermElabM (try
      forallTelescopeReducing ci.type fun _ cod => do
        let c ← whnf cod
        pure <| match c with
          | .sort .zero => "Prop"
          | .sort _ => "Type"
          | .const ``Bool _ => "Bool"
          | _ => "other"
      catch _ => pure "sort-undecided")
    if result == "sort-undecided" then unknown := unknown + 1
    if result == "Prop" && (kind == "defn" || kind == "opaque" || kind == "inductive") then
      predicates := predicates + 1
    emit <| Json.mkObj [("record", toJson "declaration"), ("name", toJson n.toString),
      ("module", toJson m.toString), ("kind", toJson kind), ("result", toJson result),
      ("levelParams", toJson (reprStr ci.levelParams)), ("typeExpr", toJson (reprStr ci.type)),
      ("valueExpr", toJson (reprStr (ci.value? true)))]
  emit <| Json.mkObj [("record", toJson "summary"), ("ownedModules", toJson owned.size),
    ("declarations", toJson count), ("predicateCandidates", toJson predicates),
    ("sortUndecided", toJson unknown), ("scope", toJson "planning measurement only; no acceptance classification")]
  out.flush
  if count == 0 || predicates == 0 then throwError "M1 empty declaration or predicate inventory"
  if unknown != 0 then throwError "M1 unresolved result sorts retained by identity; no classification closure"
  logInfo m!"M1-MEASUREMENT-COMPLETE declarations={count} predicateCandidates={predicates} modules={owned.size}"
