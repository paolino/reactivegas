# Functions model — #71

No production function changes. Named checker operations (docs-companion scope only):

- `discover_citations(docs_dir) -> markers[]`: parse marker blocks, fail on
  malformed marker (negative control).
- `discover_lean_extent(lean_dir, pin) -> symbols[]`: enumerate elaborated or
  declared symbols from source (no hand list); prerequisite failure → hard RED
  (fail-closed, never silent pass).
- `resolve_all(markers, extent) -> unresolved[]`: unknown symbol → RED.
- `freshness_pins(markers, pin) -> stale[]`: blob/line mismatch → RED.

Signatures are checker documentation; implementation lives in the gate/script
companion, not in shipped Lean/Haskell. No Lean `def` added or changed.
