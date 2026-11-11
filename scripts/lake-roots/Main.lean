import Lake
import Lake.Load.Workspace

/-!
Prints the Lean roots that Lake evaluates for a workspace's root package: every
`lean_lib` root and every `lean_exe` root, one `LAKE-EVALUATED-ROOT <module>`
line each. `scripts/check-lean-mirrors` imports exactly these.

The workspace's `lean-toolchain` must name the Lean running this tool: Lake
elaborates the workspace configuration with the running Lean, so a mismatch
would evaluate roots under a different Lake than the workspace builds with.
-/

open Lake System

private def checkToolchain (wsDir : FilePath) : IO (Option String) := do
  let file := wsDir / "lean-toolchain"
  let running := s!"leanprover/lean4:v{Lean.versionString}"
  let declared := (← IO.FS.readFile file).trim
  if declared == running then return none
  return some s!"LAKE-ROOTS-TOOLCHAIN-MISMATCH workspace={declared} running={running}"

private def evaluateRoots (wsDir : FilePath) : IO UInt32 := do
  if let some msg ← checkToolchain wsDir then
    IO.eprintln msg
    return 1
  let (elan?, lean?, lake?) ← Lake.findInstall?
  let some lean := lean?
    | IO.eprintln "LAKE-ROOTS-INSTALL-MISSING lean"
      return 1
  let lake := lake?.getD (LakeInstall.ofLean lean)
  let lakeEnv ←
    match ← (Lake.Env.compute lake lean elan?).toBaseIO with
    | .ok env => pure env
    | .error msg =>
      IO.eprintln s!"LAKE-ROOTS-ENV-FAILED {msg}"
      return 1
  let some ws ← (Lake.loadWorkspace {
      lakeEnv
      wsDir
      reconfigure := true
      updateDeps := false
      updateToolchain := false
    }).toBaseIO
    | IO.eprintln s!"LAKE-ROOTS-WORKSPACE-FAILED {wsDir}"
      return 1
  for lib in ws.root.leanLibs do
    for root in lib.roots do
      IO.println s!"LAKE-EVALUATED-ROOT {root}"
  for exe in ws.root.leanExes do
    IO.println s!"LAKE-EVALUATED-ROOT {exe.root.name}"
  return 0

def main (args : List String) : IO UInt32 := do
  match args with
  | [target] =>
    let wsDir : FilePath := ⟨target⟩
    if wsDir.isAbsolute then
      evaluateRoots wsDir
    else
      IO.eprintln s!"LAKE-ROOTS-TARGET-NOT-ABSOLUTE {target}"
      return 2
  | _ =>
    IO.eprintln "usage: lakeRoots <absolute-workspace-directory>"
    return 2
