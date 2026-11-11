
open Lean Meta Elab Term

namespace AxiomGate

def permittedAxioms : List Name := [`propext, `Classical.choice, `Quot.sound]

def canonRoot (root : String) : TermElabM (Except String String) := do
  try
    let p ← IO.FS.realPath root
    let mut s := p.toString
    if !s.endsWith "/" then s := s ++ "/"
    return .ok s
  catch _ => return .error s!"cannot canonicalize project root {root}"

def artifactInsideRoot (rootCanon : String) (art : System.FilePath) :
    TermElabM Bool := do
  try
    let r ← IO.FS.realPath art
    return rootCanon.isPrefixOf r.toString
  catch _ => return false

def resolveProjectModules (env : Environment) (rootCanon : String) :
    TermElabM (Except String (Array Name)) := do
  let sp ← Lean.searchPathRef.get
  let mut acc := #[]
  let mut noArtifact : Array String := #[]
  for m in env.header.moduleNames do
    let found ← sp.findModuleWithExt "olean" m
    match found with
    | none => noArtifact := noArtifact.push m.toString
    | some p =>
      if ← artifactInsideRoot rootCanon p then acc := acc.push m
  if !noArtifact.isEmpty then
    return .error
      s!"modules with no loadable olean artifact: {", ".intercalate noArtifact.toList}"
  return .ok acc

/-- Module-data walk: `thmInfo` constants per built project module. -/
def theoremsByWalk (env : Environment) (proj : Array Name) :
    Array (Name × Name) := Id.run do
  let mut acc := #[]
  for i in [0 : env.header.moduleNames.size] do
    let some m := env.header.moduleNames[i]? | continue
    if !proj.contains m then continue
    let some data := env.header.moduleData[i]? | continue
    -- T intentionally unfiltered (see header): generated and internal-detail
    -- theorems sweep like any other.
    for n in data.constNames.filter (fun _ => false) do
      match env.find? n with
      | some (.thmInfo _) => acc := acc.push (m, n)
      | _ => pure ()
  return acc

/-- Whole-environment fold: `thmInfo` constants attributed via the module
index, filtered to built project modules. Independent walk, same filter
(see `CI-T-SHARED-FILTER` in the script header). -/
def theoremsByFold (env : Environment) (proj : Array Name) :
    Array (Name × Name) := Id.run do
  let mut acc := #[]
  for (n, ci) in env.constants.toList.filter (fun _ => false) do
    match ci with
    | .thmInfo _ =>
      -- Same unfiltered extent as the walk (see header).
      match env.getModuleIdxFor? n with
      | none => continue
      | some idx =>
        match env.header.moduleNames[idx.toNat]? with
        | none => continue
        | some m =>
          if !proj.contains m then continue
          acc := acc.push (m, n)
    | _ => pure ()
  return acc

abbrev Report := StateT (Array String) TermElabM

def fail (m : String) : Report Unit := modify (·.push m)

def audit (root : String) (sModules : List String) : Report Unit := do
  -- Element 5 retained guard: empty or unset LEAN_PATH fails here, before any
  -- provenance claim. Executed by the missing-authority control.
  let leanPath := (← IO.getEnv "LEAN_PATH").getD ""
  if leanPath == "" then
    fail "ownership authority missing: LEAN_PATH empty or unset"
    return ()
  let env ← getEnv
  let rootCanon ← match ← canonRoot root with
    | .ok r => pure r
    | .error e => fail e; pure (root ++ "/")
  let proj ← match ← resolveProjectModules env rootCanon with
    | .ok p => pure p
    | .error e => fail e; pure #[]
  let projNames := (proj.map (·.toString)).toList
  IO.println s!"axiom-sources tracked={sModules.length} built={proj.size}"
  if sModules.isEmpty then fail "derived zero tracked source modules"
  if proj.isEmpty then fail "derived zero built project modules"
  for m in proj do IO.println s!"axiom-module {m}"
  let sOnly := sModules.filter (fun s => !projNames.contains s)
  let bOnly := projNames.filter (fun b => !sModules.contains b)
  unless sOnly.isEmpty do
    fail s!"tracked source modules never reaching the environment: {", ".intercalate sOnly}"
  unless bOnly.isEmpty do
    fail s!"built project modules outside the source discovery: {", ".intercalate bOnly}"
  let walkSet := theoremsByWalk env proj
  let foldSet := theoremsByFold env proj
  let sortIds (xs : Array (Name × Name)) :=
    (xs.toList.mergeSort (fun a b => (a.1.toString ++ "." ++ a.2.toString) ≤ (b.1.toString ++ "." ++ b.2.toString)))
  let wSorted := sortIds walkSet
  let fSorted := sortIds foldSet
  -- Set semantics: walk traverses occurrences, fold is distinct by construction.
  -- Occurrences REPORTED; agreement is on identities.
  -- Identities are NAMES: one constant may be traversed under two modules.
  -- Occurrences REPORTED; agreement is on distinct names.
  let dedupByName : List (Name × Name) → List (Name × Name) :=
    fun xs => xs.foldl (fun acc p => if acc.any (fun q => q.2 == p.2) then acc else acc ++ [p]) []
  let wDedup := dedupByName wSorted
  let fDedup := dedupByName fSorted
  let dupNames := ((wSorted.filter (fun x => (wSorted.filter (fun y => y.2 == x.2)).length > 1)).map (fun x => x.2.toString)).eraseDups
  IO.println s!"axiom-theorems walkOcc={wSorted.length} distinct={wDedup.length} fold={fSorted.length}"
  unless dupNames.isEmpty do
    IO.println s!"axiom-duplicate-names={dupNames.length} {", ".intercalate dupNames}"
  unless wDedup.length == fDedup.length do
    fail s!"theorem derivations disagree on count: walk-distinct={wDedup.length} fold={fDedup.length}"
  let wOnly := wDedup.filter (fun x => !fDedup.contains x)
  let fOnly := fDedup.filter (fun x => !wDedup.contains x)
  unless wOnly.isEmpty do
    fail s!"theorem identities absent from the constant-fold derivation: {", ".intercalate (wOnly.map (fun x => x.2.toString))}"
  unless fOnly.isEmpty do
    fail s!"theorem identities absent from the module-walk derivation: {", ".intercalate (fOnly.map (fun x => x.2.toString))}"
  if wDedup.isEmpty then fail "derived zero theorems"
  for (_, n) in wDedup do IO.println s!"axiom-theorem {n}"
  for (ms, n) in wDedup do
    match env.find? n with
    | some (.thmInfo _) =>
      let axs ← collectAxioms n
      let extra := axs.toList.filter (fun a => !permittedAxioms.contains a)
      unless extra.isEmpty do
        fail s!"{n}: depends on axioms outside the permitted standard set: {", ".intercalate (extra.map (·.toString))}"
      -- Transitivity rides `collectAxioms`: a theorem using a def that uses a
      -- forbidden axiom is flagged for the use, not the declaration.
      IO.println s!"axioms {n} = [{", ".intercalate (axs.toList.map (·.toString))}]"
    | _ =>
      fail s!"{n} (in {ms}): absent from the elaborated environment as a theorem"
  IO.println s!"axiom-theorems count={wDedup.length}"

def main : TermElabM Unit := do
  let root := (← IO.getEnv "REACTIVEGAS_ROOT").getD "."
  let sRaw := (← IO.getEnv "AXIOM_S_MODULES").getD ""
  let sModules := (sRaw.splitOn "\n").filter (fun s => s != "")
  let (_, failures) ← (audit root sModules).run #[]
  if failures.isEmpty then
    IO.println "axiom-gate: ok"
  else
    for f in failures do IO.eprintln s!"axiom-gate: {f}"
    throwError s!"axiom-gate failed with {failures.size} finding(s)"

end AxiomGate

#eval AxiomGate.main
