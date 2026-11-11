import Lean
import KelGroups.Vote.Types

/-!
# Policy re-derivation (`reactivegas#75`)

The source-side check of the replay-context contract: the threshold table in a
sidecar is compared, at every point of its declared domain, with the value the
named live declaration returns.

This module is deliberately independent of the code that writes the sidecar: it
imports only the vote vocabulary that holds the declaration and `Lean`'s JSON, it
reads the table straight from the sidecar *bytes*, and it never consults a
provenance field as evidence of agreement. The one provenance-looking field it
reads, `policies.threshold.source.declaration`, names which live declaration to
evaluate; naming a declaration this check does not hold is a refusal.
-/

namespace Reactivegas.PolicyCheck

open Lean

/-- The declarations the check can evaluate, each bound to the live definition
(not a copy), so a changed definition changes the answer at the next build. -/
def declarations : List (String × (Nat → Nat)) :=
  [("KelGroups.Vote.legacyThreshold", KelGroups.Vote.legacyThreshold)]

/-- Evaluate the named declaration at every domain point and compare with the
sidecar's table. `.ok n` is the number of points compared; the error names the
first point that differs (`POLICY-MISMATCH n=<point> ...`). -/
def checkPolicyAgainstDeclaration (sidecarBytes : String) : Except String Nat := do
  let j ← Json.parse sidecarBytes
  let threshold ← (← j.getObjVal? "policies").getObjVal? "threshold"
  let declName ← (← threshold.getObjVal? "source").getObjValAs? String "declaration"
  let some decl := declarations.lookup declName
    | throw s!"POLICY-REFUSED declaration {declName} is not one this check evaluates"
  let table ← (← threshold.getObjVal? "table").getArr?
  if table.isEmpty then throw "POLICY-REFUSED empty table"
  let mut points := 0
  for i in [0:table.size] do
    let entry ← table[i]!.getArr?
    unless entry.size == 2 do throw s!"POLICY-MISMATCH n={i} entry is not a pair"
    let n ← entry[0]!.getNat?
    let v ← entry[1]!.getNat?
    if n != i then throw s!"POLICY-MISMATCH n={i} table key is {n}, not the point's own index"
    if decl n != v then
      throw s!"POLICY-MISMATCH n={n} table={v} declaration {declName}={decl n}"
    points := points + 1
  return points

end Reactivegas.PolicyCheck
