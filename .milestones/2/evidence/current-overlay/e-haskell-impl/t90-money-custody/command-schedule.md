# #90 prospective execution schedule v1

Authority: answers/A-002-execution-unit-ruled.md; prior spend 0/10.
Shared hard ceiling 10; author at most four, including its final verification.
At most two submissions, one adjudicated repair batch, two blind initial
inspectors, one conditional delta inspector. No reset and no budget raise.

## Units frozen before first execution

- CI: exactly `nix develop --quiet -c just ci`, one invocation = one unit,
  including the declared committed recipe/dependency expansion. All stages run.
  The existing recipe is retained; the mandate adds the permanent economic
  test recipe and necessary build wiring. Record the candidate-specific recipe
  hash before each invocation. A repair does not permit dropping existing checks.
- Focused proof: exactly `nix develop --quiet -c just economic-test`, one
  invocation = one unit, with the Cabal test component build and execution as
  its declared dependency expansion. This is the ordinary permanent test suite,
  not a wrapper aggregating independent campaign runs.
- Any other compile, test execution, interpreter run, mutant experiment,
  readiness attempt or separately invoked CI stage costs a separate unit and
  must consume an explicitly available actor slot before execution. No free
  readiness convention applies. Setup failures consume an attempted slot and
  remain distinct from semantic failures; they never establish a mutant kill.
- Static reading, Git/file/hash operations, planning, formatting edits,
  process identity checks and receipt bookkeeping are not build/run evidence.
  They cannot close semantic rows.
- Permanent suite assertions may include guard-fault injection using the
  core's declared read-only query boundary, actual transitions, and shared
  comparison predicates. They are ordinary compiled test cases. Do not introduce
  scripts to bundle separate mutant compilations, retries or actor runs into
  one unit. If a required mutant needs a separate invocation, reserve and count
  it separately; return a concrete gap before exceeding an actor allocation.

## Allocation

| Slots | Actor / purpose | Limit |
|---|---|---|
| B1 | ticket owner: baseline full CI | 1 |
| A1 | author: focused executable RED | 1 |
| A2 | author: candidate full CI GREEN | 1 |
| A3 | author: one additional focused/CI attempt if needed | 1 |
| A4 | author: post-acceptance final-tree full CI | 1 |
| I1 | initial blind inspector 1: full CI | 1 |
| I2 | initial blind inspector 2: full CI | 1 |
| R1 | same author: conditional adjudicated repair verification | 1 |
| D1 | fresh delta inspector: conditional repaired-candidate CI | 1 |
| F1 | ticket owner: final exact-SHA full CI | 1 |

The accepted eight slots are A1–A4, I1–I2, R1, D1; B1/F1 use the remaining
two. Repair work cannot make the author exceed four total author executions:
R1 is reserved campaign capacity, but can be exercised only if A3 is unused
(or another unspent author slot is explicitly transferred in the ledger).
No worker may consume another actor's slot unilaterally. Unused conditional
capacity is not a new submission or a second repair.

Record actual argv, actor, slot, candidate/tree, recipe hash, start/end UTC,
elapsed cost, exit and raw evidence path/hash for every attempt in
`campaign-ledger.tsv`. Nested stages and their results remain in the evidence.
The recorder transports the result; it does not change the execution unit.

Gate falsification belongs to author A1's proof phase. The gate contract is
fixed before author launch; implementation admission follows the executable RED
receipt. No nonexistent prior semantic receipt is claimed.
