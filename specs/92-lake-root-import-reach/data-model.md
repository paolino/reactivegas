# Data — #92

- **D1 root set**: non-empty, deduplicated set of module names from Lake's
  evaluated root package (`leanLibs[*].roots ∪ leanExes[*].root`). Empty set
  is a failure (`MIRROR-LAKE-ROOTS-EMPTY`), never a vacuous pass.
- **D2 tracked set**: unchanged — `git ls-files lean/**/*.lean`.
- **D3 closure**: imports reachable from D1 ∪ checker direct imports.
  Invariant: D2 ⊆ D3, else `MIRROR-IMPORT-REACH-GAP <m>` for each m ∈ D2 \ D3.
