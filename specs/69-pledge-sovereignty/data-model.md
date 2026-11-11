# Data model — #69

Ceiling: 20 lines / 1200 bytes. No type changes (INV69-CONTRACT).

| Entity | State invariant after any accepted step |
|---|---|
| `Collection.pending` | at most one row per user (L8); every amount > 0 after a pending correction (a zero correction removes the row) |
| `Collection.accepted` | changed only by `acceptPledge` and the referente's correction (G-ACC); a pending correction never writes it |
| `State.conti` | member balances ≥ 0 (solvency); conservation `sumBal casse − sumBal conti − escrowSum = 0` |
