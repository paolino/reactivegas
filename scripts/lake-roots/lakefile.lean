import Lake
open Lake DSL

package lakeRootsTool

@[default_target]
lean_exe lakeRoots where
  root := `Main
  supportInterpreter := true
