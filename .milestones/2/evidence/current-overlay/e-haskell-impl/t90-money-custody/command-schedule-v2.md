# #90 prospective execution schedule v2

Authority: answers/A-003-author-allocation.md and A-002-execution-unit-ruled.md.
Supersedes v1 prospectively; v1 remains immutable historical evidence.
Shared spent3/10: B1 baseline1 plus U1/U2 author2. Author allocation FIVE.
ZERO reserve. No refund, reset, semantic credit, acceptance reduction, or extra
readiness run. One further unplanned attempt ends this campaign.
Two submissions, one repair batch, all four arms unchanged.

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

## Authoritative remaining allocation

| Slot | Purpose | Units |
|---|---|---|
| A1 | fresh author focused semantic RED | 1 |
| A2 | fresh author candidate full CI | 1 |
| A4 | author final post-acceptance tree full CI | 1 |
| I1 | blind initial inspector 1 full CI | 1 |
| I2 | blind initial inspector 2 full CI | 1 |
| R1 | conditional single adjudicated repair | 1 |
| D1 | conditional fresh delta inspector CI | 1 |

Arithmetic exactly 3+3+2+1+1=10. A-003 expressly funds the final author CI A4;
its final exact tree receipt also supplies mandatory final CI evidence for the
ticket owner. No duplicate v1 F1 invocation is allocated under this ruling.
The conditional repair slot is retained as expressly ruled; initial author
execution authority now is only A1 and A2, with A4 held for final disposition.
Any repair commission must bind the conditional R1 allocation explicitly.

Append a reservation/START BEFORE each attempt, naming slot, actor, exact argv,
candidate/tree and recipe hashes. Append actual outcome, UTC times, elapsed,
raw output path/hash, and distinct setup versus semantic result afterward.
The owner maintains its own ledger; parent reconciles each durable receipt.
Gate hash and all product mandates remain unchanged. No tail pipes or partial
output receipts. No extra readiness, formatting-via-Nix, or standalone checks.
