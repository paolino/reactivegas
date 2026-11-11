# Plan — #92

Seed: candidate `8df63cf` (on `890a74f`), which replaces the checker's fixed
`import KelGroups`/`import Reactivegas` with roots printed by a small Lake-API
tool (`scripts/lake-roots`, `lakeRoots <abs-workspace>`), evaluated per run.
Reused as seed after review: the design meets R92-1; it lacks R92-3.

Slices (one OWNER slice, bisect-safe commits):

1. **S1** Land the seed (cherry-pick `8df63cf`), keeping or trimming its
   run-binding receipt lines on leanness grounds (commit owner's call; the
   `just lean` receipt guard must keep working).
2. **S2** Add the permanent negative control (R92-3) and the setup-failure
   diagnostics check (R92-4) to the `just lean` path.
3. **S3** Demonstrate on an isolated combined tree (candidate + PR94's two
   driver files and lakefile stanzas from `c037bf4`), not committed here.

Live boundary: Lake API at toolchain `leanprover/lean4:v4.25.0`, inside the
Nix dev shell used by CI.
