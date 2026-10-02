# Modules model — #86 exporter successor

## Changed responsibilities (all additive, on `9c8756a`)

- **M86-EXPORT (`lean/Reactivegas/CorpusExport.lean`).** Gains: live binding
  of `view` + both `auth` identities + integrated `initial` to their
  authoritative defs (`seedView`, `econAuthIdentity`/`intAuthIdentity` sources,
  `corpusInitial`) through the same call-site `check` path; malformed-arity
  guard so `["check", onePath]` no longer falls into the 2-arg write arm.
  Still calls `seedCorpus`/`emitIntegratedCorpus`; takes no list args; adds
  no wrapper field. One-way import direction unchanged.
- **M86-JUST (`justfile`).** Gains: extended `lean-corpus-verify` (context
  binding + key-set checks stay; exact post-repair body frozen in gate);
  no existing recipe weakened. `ci` keeps invoking verify.
- **M86-CI (`.github/workflows/ci.yaml`, additive).** Gains one step running
  corpus verification on the committed path (exact command frozen in gate).
  Loses nothing: `lean-toolchain-contract` + `just lean` (direction,
  inversion coverage ± negative control, trace agreement, `lake build`) stay.
- **M86-NIX (`nix/project.nix` or owned shell decl).** Gains `jq` in shell
  inputs (exact attr frozen in gate). Nothing else moves.
- **M86-CORPUS (`lean/corpus/*`).** Unchanged bytes by default (fence).
  Only a separately accepted upstream integration may re-emit, recorded
  separately with new hashes + dated handoff entry.
- **M86-RECORD (`handoffs/CORPUS-COVERAGE.md`, ticket root).** Corrected to
  current hashes + zero `UNPROVED`, dated pre-S1 evidence preserved, vote
  hole + provisional list (#68/#69/#76/#81/#75-context) + replayer table kept.

## Explicit non-goals

Model edits, `Trace`/schema changes, wrapper widening, `docs/`, kelgroups
implementation, vote-corpus (#75), composition (#76), V-5 lifecycle (#81),
replay sidecar (#75-context). Still provisional after acceptance.
