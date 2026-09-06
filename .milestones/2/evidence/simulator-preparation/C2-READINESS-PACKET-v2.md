# C2 readiness packet v2 — workflow wiring corrected, fence disposed, domains restored

Supersedes v1 (`72d24761…`, **preserved**). Preparation only. C1 acceptance plus
an explicit C2 commission still precede implementation. Geometry remains pending
and is **not inferred**. No C1 change, no seat, build, audit, push, PR, merge or
threshold ruling.

## 1. Workflow ≠ local recipe — my §2 was the wrong graph

**Verified:** `.github/workflows/ci.yaml` at `3590c001` contains **zero**
occurrences of `just ci`. It runs explicit commands:

```
nix --quiet develop --command just lean-toolchain-contract
nix --quiet develop --command just lean
nix --quiet develop --command just lean-corpus-verify
```

v1's §2 graph was the **local `ci` recipe**, not the workflow. **Wiring a
generator gate only into that recipe would leave automated execution unwired** —
which is precisely the "#74 omission" shape I had just retired as stale, and I
came within one step of reintroducing it. #86 fixed the corpus *invocation*; it
did not make every later recipe addition run in CI.

**Corrected requirement — both, not either:**

1. a **direct committed workflow invocation** in `ci.yaml`, placed **after**
   corpus verification;
2. **and** the local `just` recipe integration.

All existing checks preserved, none reordered or weakened.

**Binding and controls at the final candidate:**

- bind and **exercise the actual workflow command**, not a local paraphrase;
- an **invocation omission / disable control**: remove or disable the workflow
  invocation and the gate must go **RED** — otherwise a green workflow that never
  runs the verifier passes again;
- keep **remote clean-SHA CI success distinct from local mutation execution**.
  **No remote mutant push is required**, and none will be proposed.

## 2. Fence — disposed, and my v1 question is withdrawn

The earlier C2 mandate **already listed** `justfile`, the new generator files and
`.github/workflows/ci.yaml`. These are **not newly discovered scope questions**
merely because they sit outside C1's different fence. **No separate C1-successor
ticket is required**, and v1 §7 is withdrawn as posed.

**Planning fence, accepted:**

- original C2 paths;
- **minimal** economic core export/adapter adjustment **only if** a concretely
  demonstrated required route cannot use the accepted surface;
- `nix/project.nix` `shell.buildInputs` for required test tools.

**Excluded:** semantic fold reimplementation, runtime semantics change, Lean
source edit. A larger structural or semantic change **returns before dispatch**.

## 3. The core is already importable — I should have inspected first

**Verified by import**, not by reading: `economics-simulator-core.mjs` exports
`attempt`, `applyIntegrated`, `bootAggregate`, `canonState`, `canonAggregate`,
`lawViolations`, `verifyTraceV1`, `verifyIntegratedV1`, `traceConformance`,
`integratedTraceConformance`, the vote functions (`vtApply`, `vtValidate`,
`vtSweep`, `vtTheta`), `EV`, `EVENT_ROUTES`, `CLAIMS` and the refusal inventory —
all resolving as functions/values at import.

**v1 asserted a new export might be necessary before inspecting the surface.
Withdrawn.** Prefer **reuse of the accepted surface** once it lands.

**But availability is not sufficiency:** an existing export does **not** prove the
generator can drive every required route. Any export need must be a
**concretely demonstrated** route failure, not an assumption.

## 4. Tools — declared vs available, stated without overreach

- **`jq` is declared** in `nix/project.nix` `shell.buildInputs`.
- **`nodejs` is not declared** there, nor in `flake.nix` or `nix/*.nix`.

**I do not claim Node is universally absent** — it plainly runs on this host, so
it is coming from the environment rather than a repo-declared input. The precise
statement: **its provision is not reproducible from the repository**, so if the
generator needs Node it must be **explicitly and reproducibly declared** in
`shell.buildInputs`. Final paths and precise necessity **bind at C2 intake on
accepted C1**.

## 5. Domains — restored, not shrunk to the convenient one

v1's table named only `AppEvent` for constructor discovery. **That shrank the
mandate to the surface with the tidiest schema.** Restored, each with its actual
adapter:

| domain | adapter |
|---|---|
| **legacy `Event`** (carries `author`) | the JS `attempt` machine |
| **`AppEvent`** (no actor; signer separate) | `applyIntegrated` / import path |
| **integrated / base proposal / mutation / change** | integrated stream + `bootAggregate` |
| **`VoteEvent`** | `vtApply` / `vtValidate` / `vtSweep` |

Every relevant domain and its adapter is required. The C1 actor-substitution
finding still binds: a generator must not be able to emit an actor inside
application-event arguments **by construction**.

## 6. Witness independence — the scheduler must not pick its own denominator

v1 said "reachable witnesses = generator + adapter replay". **Insufficient.**
Witness **selection and validation must be independent of the scheduler under
test**, and the **omission control must leave the bound denominator unchanged** —
otherwise a broken scheduler shrinks the denominator and scores itself.

**Preserved from the original mandate:** the explicit **inconclusive vs
unreachable** distinction, and any legitimately unresolved rows. An unproven row
stays unresolved; it is not promoted to "unreachable" because no witness was
found.

## 7. Operational bounds — withdrawing "wall-clock is not a contract"

**Withdrawn.** A finite runtime limit **is** a valid operational bound, even
across unequal hosts. My v1 framing was wrong.

What remains true is narrower: **a timeout cannot substitute for coverage.** The
three coexist:

- **counted invocations** at their actual layer — failed, setup and warm retries
  included;
- a **deterministic case ceiling per generator invocation**, **fixed before the
  run and reported afterwards** — never chosen after seeing output;
- a **fail-closed timeout**: expiry is a failure, never a silent pass.

**A full suite remains a substantive operation even when launched by a command
named `generator`.**

## 8. Allowances — still proposals, not taken

Owner **12 substantive / 60 targeted**; fresh auditor **6 / 40**. **Nothing is
spent or reserved.** These bind only at an explicit C2 commission.

## 9. Retained from v1

Observation base `3590c001`; implementation bound to the **future accepted C1
SHA**; the four retired stale mandate passages; constructor-telescope evidence
retention (script bytes, pinned toolchain/source inputs, raw output, hash-bound —
"discarded" means no tracked model/emitter change, never destroying the basis of
a claim); the named design unknowns (shrinking failure-identity predicate, guard
reachability, whether the generator can drive the browser adapter); and **no
instrument described as frozen when it is not built.**

## 10. Blockers

Geometry ruling pending; C1 not accepted; owner at 0 targeted / 5 substantive
with the F-02 control gap unresolved; C2 implementation requires accepted C1 plus
its own explicit commission.
