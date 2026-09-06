import Lake
open Lake DSL

package reactivegas where
  leanOptions := #[
    ⟨`autoImplicit, false⟩
  ]

@[default_target]
lean_lib Reactivegas where
  srcDir := "."

@[default_target]
lean_lib KelGroups where
  srcDir := "."

lean_exe corpusExport where
  root := `Reactivegas.CorpusExport
