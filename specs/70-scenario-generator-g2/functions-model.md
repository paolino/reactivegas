# Functions — #70 G2

| ID | Signature | Constraint |
|---|---|---|
| F-SHRINK | `shrinkTrace(steps, violation, core) -> { steps, violation }` | deterministic; every kept candidate is reachable through the production core and violates the same law at its last step |
