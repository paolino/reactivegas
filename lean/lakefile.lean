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

/-- Vote-derived economic effects at the production root: the theorems and
the executable oracle. Outside the `Reactivegas` umbrella; `lake build`
elaborates both. -/
@[default_target]
lean_lib ReactivegasComposition where
  srcDir := "."
  roots := #[`Reactivegas.CompositionRoot, `Reactivegas.CompositionTests]
