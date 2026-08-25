import Lake
open Lake DSL

package reactivegas where
  leanOptions := #[
    ⟨`autoImplicit, false⟩
  ]

@[default_target]
lean_lib Reactivegas where
  srcDir := "."
