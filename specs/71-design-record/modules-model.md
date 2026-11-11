# Modules model — #71, version 2

- **design record** (`docs/en/design/*.md`): published description of the model
  on master. Depends on the Lean library by citation only.
- **citation checker** (new, outside `lean/`): reads the record, asks the built
  Lean library which citations resolve and which code spans name declarations,
  exits non-zero on any failure. Depends on the Lean library build; nothing
  depends on it except the CI step and the justfile recipe.
- **checker fixtures** (new, next to the checker): copies of record fragments
  carrying one known defect each, used only by the hosted controls.
- **CI wiring** (`.github/workflows/ci.yaml`, `justfile`): one recipe for the
  check, one for its controls; invoked from the "Build and check" job or a job
  of its own.

Dependency direction: record → (cites) Lean library; checker → Lean library and
record; CI → justfile → checker. Nothing in `lean/` depends on the checker.
