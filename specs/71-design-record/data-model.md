# Data model — #71

- Citation marker: `{file, line, commit, symbol, kind}` where kind ∈
  {law (universally quantified theorem), witness (finite `check…=true` oracle
  or exhibit), definition, route-classification, gap (explicitly unimplemented)}.
  Every declaration-like prose claim carries one; missing marker is a gate failure.
- Operator authority entry: `{date, source-note, ruling, scope, supersedes}`.
  Later dates supersede earlier assertions on the same scope.
- Pending row: `{id (#66-S1/#68/#69), current-behavior pin, ruled-behavior,
  source-ruling, re-pin-condition}`. Implemented-behavior claims require merged pin.
- canCloseGroup classification: `{symbol, location, usages-count, conjuncts,
  product-intent refs, verdict: missing-guarantee|justified-non-goal}`.
  No new theorem, no Lean edit.

Validation: marker symbols must resolve in the discovered Lean extent at the
pinned commit; authority dates must be present and ordered; pending rows must
name their merge condition.
