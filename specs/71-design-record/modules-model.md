# Modules model — #71

- `docs/en/design/state-machine.md` (owned, rewritten): State/Event/Route/
  Authority mapping, law-vs-witness marking, composition/vote limits,
  Voci non-goal, dated authority, current-vs-ruled pending table, marker blocks.
- `docs/en/design/kelgroups-vote-machine.md` (owned, verify-and-carry):
  re-derive 30/30 citation resolution; rewrite only on measured drift.
- Citation checker companion (owned, documented alongside the record):
  discovers cited extent from markers, resolves against pinned Lean blobs,
  fails closed on missing prerequisite. Invoked by the untracked gate; not
  committed as production code beyond docs scope.
- `specs/71-design-record/` (owned, planning only): spec/plan/models/tasks.

No new upstream module promotion. No Lean/Haskell/simulator modules touched.
