# Functions — #70 S5-sim-a

| ID | Signature | Constraint |
|---|---|---|
| F-DERIVE-CORE-PIN | `deriveCorePin(repo: path, head: revision) -> { commit, tree }` | reads git history only; throws with a message naming the core pin and the cause when no commit is found or the revision does not resolve |
