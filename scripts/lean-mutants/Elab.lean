/-
Lean mutant ledger — elaboration driver.

    elab <module> <source> <olean> <setup> [<module> <source> <olean> <setup> ...]

The runner compiles this file once per run (`lean -c`, `leanc`) and calls the
executable with `LEAN_PATH` from `lake env`, one module per process.

Elaborates each module in the order given, in one process. For each it prints
a JSON marker line `{"elab":"begin","module":…}`, every message the frontend
reports as one JSON object per line (the reporter `lean --json` uses, with the
error cap lifted), then `{"elab":"end","module":…,"errors":…}`.

Unlike `lean -o`, the `.olean` is written even when the module has errors: a
theorem whose proof fails is still in the environment (admitted by error
recovery), so the modules that import it can be elaborated and observed under
the same mutant.

The module's options come from `<setup>`, the `ModuleSetup` file Lake wrote
when it built that module (Lake's own evaluation of `lakefile.lean`); no option
is written here. Imports are resolved through `LEAN_PATH`, never through the
setup's pre-resolved artifacts, so a mutated upstream `.olean` is the one
imported. A setup carrying plugins or dynamic libraries is refused
(`LAKE-OPTION-UNREPRODUCED`): this driver does not load them.
-/
import Lean

open Lean Elab

def elabOne (modName src olean setupPath : String) : IO (Option Bool) := do
  let setup ← ModuleSetup.load setupPath
  if setup.name != modName.toName then
    IO.eprintln s!"LAKE-OPTION-UNREPRODUCED {modName}: setup file is for {setup.name}"
    return none
  unless setup.plugins.isEmpty && setup.dynlibs.isEmpty do
    IO.eprintln s!"LAKE-OPTION-UNREPRODUCED {modName}: Lake loads plugins or dynlibs for it"
    return none
  let input ← IO.FS.readFile src
  let inputCtx := Parser.mkInputContext input src
  let opts : Options := setup.options.toOptions.setNat `maxErrors 0
  let opts := Lean.internal.cmdlineSnapshots.setIfNotSet opts true
  let opts := Elab.async.setIfNotSet opts true
  let mainModuleName := modName.toName
  let isModule := setup.isModule
  let setupFn stx := do
    return .ok { imports := stx.imports, isModule := strictOr isModule stx.isModule,
                 mainModuleName, opts, trustLevel := 1024, plugins := #[] }
  IO.println (Json.compress (Json.mkObj [("elab", "begin"), ("module", modName)]))
  let snap ← Language.Lean.process setupFn none { inputCtx with }
  let snaps := Language.toSnapshotTree snap
  let hasErrors ← snaps.runAndReport opts (json := true)
  let some cmdState := Language.Lean.waitForFinalCmdState? snap
    | IO.eprintln s!"elab: {modName}: header processing failed"; return none
  writeModule cmdState.env olean
  IO.println (Json.compress (Json.mkObj
    [("elab", "end"), ("module", modName), ("errors", toJson hasErrors)]))
  return some hasErrors

def main (args : List String) : IO UInt32 := do
  initSearchPath (← findSysroot)
  let rec go : List String → IO UInt32
    | [] => return 0
    | m :: s :: o :: st :: rest => do
      match ← elabOne m s o st with
      | some _ => go rest
      | none => return 2
    | _ => do
      IO.eprintln "usage: Elab.lean <module> <source> <olean> <setup> [...]"
      return 64
  if args.isEmpty then
    IO.eprintln "usage: Elab.lean <module> <source> <olean> <setup> [...]"
    return 64
  go args
