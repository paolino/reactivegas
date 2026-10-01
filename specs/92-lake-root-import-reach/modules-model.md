# Modules — #92

- **M1 `scripts/lake-roots/`** (new, Lean exe against Lake API): sole
  responsibility = print the workspace root package's evaluated lib and exe
  roots. Consumes an absolute workspace path; emits `LAKE-EVALUATED-ROOT <name>`
  lines or a named failure.
- **M2 `scripts/check-lean-mirrors`** (changed): consumes M1's root set and
  imports it into the generated driver; reach reconciliation unchanged.
- **M3 negative control** (new, placement = commit owner's choice; runs from
  `just lean`): exercises M2 on a tree with an undeclared unimported tracked
  module and requires a named reach-gap failure.

Dependency direction: `justfile lean` → M2 → M1 → Lake; M3 → M2.
