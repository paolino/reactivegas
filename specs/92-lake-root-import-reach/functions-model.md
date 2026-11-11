# Functions — #92

- `lakeRoots (target : absolute FilePath) : IO UInt32` — exit 0 with D1
  printed; exit ≠ 0 with one of `LAKE-ROOTS-INSTALL-MISSING`,
  `LAKE-ROOTS-ENV-FAILED`, `LAKE-ROOTS-WORKSPACE-FAILED`,
  `LAKE-ROOTS-TARGET-NOT-ABSOLUTE`.
- `scripts/check-lean-mirrors` — no new arguments required; a negative-control
  mode or separate script is the commit owner's choice (signature recorded
  here on first commit).
