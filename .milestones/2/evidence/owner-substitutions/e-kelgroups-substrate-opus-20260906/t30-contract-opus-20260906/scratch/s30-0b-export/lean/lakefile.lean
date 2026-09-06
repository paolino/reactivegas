import Lake
open Lake DSL

require keri from git
  "https://github.com/paolino/keri-lean" @ "e37386c7e04cb39aafcb61b808e16d8222c6cc66"

package kelgroups where
  leanOptions := #[
    ⟨`autoImplicit, false⟩
  ]

@[default_target]
lean_lib KelGroups where
  srcDir := "."
