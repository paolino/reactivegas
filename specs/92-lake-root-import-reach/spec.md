# #92 — mirror checker import reach covers declared Lake roots

## Story

As a maintainer registering a new top-level Lean root in `lean/lakefile.lean`
(e.g. `TraceDriverV1`, `KelTraceDriverV1` from #70/PR94), the mandatory
mirror check (`just lean` → `scripts/check-lean-mirrors`) accepts the tree
without any edit to the checker, while a tracked module that no declared root
reaches still fails, naming that module.

## Requirements

- **R92-1** Every root declared by the `lean/` workspace's own Lake package
  (lib roots and exe roots, as Lake evaluates them) is in the generated
  driver's import closure. Source of truth is Lake's evaluated configuration:
  no name list, no name-shape predicate, no lakefile text parsing, no
  exclusion of tracked sources.
- **R92-2** A tracked `lean/**/*.lean` module outside the closure of declared
  roots and the checker's direct imports fails the check with
  `MIRROR-IMPORT-REACH-GAP <module>` naming that module. Importing every
  tracked module (making the reach check vacuous) is a bypass, not a fix.
- **R92-3** R92-2 is asserted permanently by the committed CI path: `just
  lean` executes a negative control in which an undeclared, unimported
  tracked module makes the checker fail naming it; a checker disabled or
  bypassed makes that control fail.
- **R92-4** Setup failures stay distinguishable from a reached reconciliation
  failure and name their subject: missing tool input (path), tool build
  failure, root evaluation failure, empty root set, invalid import in a
  tracked module.
- **R92-5** Existing mirror correspondence, classification and receipt
  contract unchanged: on the base tree the summary stays
  `rows=19 exceptions=4 discovered=24 promoted=2`, kind census
  `pred=24 excluded-thm=1285 unclassified=0`; `MIRROR-RECEIPT-ABSENT` guard in
  `just lean` unchanged. No count quota, no weakened upstream check.

## Non-goals

- No change to Lean model semantics, to `docs/en/design`, or to PR94's files.
- No new CI workflow job: the existing `Build lean specification` job
  (`nix --quiet develop --command just lean`) carries every row.
