# C2 readiness packet — measured against today's tree. Preparation only.

Ticket owner `%313`. **No implementation, build, audit, seat, push, PR or merge
is granted or performed.** C1's repair state and the pending geometry ruling are
untouched, and **the geometry answer is not inferred.**
`C2-GENERATOR-MANDATE.md` is **preserved**; its current-model vs
ruled-undelivered distinction, externally bound refusal denominator,
replay-preserving shrinking and transient mutated-simulator witness requirements
are **kept**. This packet refreshes what has gone stale and adds what was missing.

## 1. Base — observation vs binding

- **Observation base:** accepted master **`3590c0015b84fd58004bf6fb44dd18b107304c48`**.
- **Implementation binds to the future accepted C1 SHA**, which does not yet
  exist. Nothing here binds `9717405e` or any repair commit above it.

**Retired as stale** (mandate text preserved, superseded here):

| mandate line | why it is stale |
|---|---|
| `:199` *"#74's omission: a green workflow that never runs its new verifier"* | **#86/PR87 exporter landed**; the verifier is now wired — see §2 |
| `:211` *"Do not edit the workflow now — it would collide with the pending #74 and #66"* | **#66/S2R landed** at `3590c001`; that collision no longer exists |
| `:176` *"not a grant to widen #74"* | #74 is historical/superseded |
| `:234`/`:236` `builds_budget = 20`, *"bounded by the gate's own wall-clock"* | not a complete campaign contract — replaced in §4 |

**Not anticipated:** #68/#69, threshold defaults, corpus-schema widening.

## 2. Actual current CI wiring — re-read today, not carried forward

```
ci: lean-toolchain-contract → build → format-check → hlint
    → lean → lean-corpus-gate → lean-corpus-verify
lean: nix/lean-dependency-direction.sh
    → scripts/check-reactivegas-inversion-coverage
    → scripts/check-reactivegas-inversion-coverage --negative-control
    → scripts/check-lean-axioms
    → scripts/check-trace-coverage-agreement
lean-corpus-gate:   lake env lean Reactivegas/CorpusGate.lean  ⇒ must print "true"
lean-corpus-verify: lake build corpusExport → run binary to a tmp dir
                    → jq schema assertions over corpus/integrated.json
```

**Two things changed since the mandate was written** and both matter:
`lean-corpus-verify` **exists** and runs the exporter binary; `check-lean-axioms`
**is wired** inside `lean`.

**Exact intended integration point:** a **new `just` recipe** appended to the
`ci` chain **after** `lean-corpus-verify`, so the generator observes a tree whose
corpus has already been re-derived and schema-checked. **No existing step is
removed, reordered or weakened.**

**Tool inputs the new step depends on:** `node` (generator + simulator adapter),
`jq` (already a corpus-verify dependency), `lake`/Lean toolchain at the pinned
version, and the nix devshell that provides them. **These are declared, not
assumed** — see the fence question in §7.

## 3. Constructor-telescope query — ephemeral, but its basis is retained

The compiled telescope query may remain **untracked**. **"Discarded" means no
tracked model or emitter change — it does not mean destroying the basis of a
discovery claim.** The evidence packet must retain and hash-bind:

- the **script bytes** actually executed;
- the **pinned toolchain and source inputs** it read (Lean toolchain version and
  the exact source SHAs/pins);
- its **raw output**, verbatim.

A discovery whose basis was deleted is an assertion, not evidence.

## 4. Numerical allowances — replacing `builds_budget=20` + wall-clock

Wall-clock is not a contract: it varies by host and cannot be reconciled after
the fact. **Replaced by counted invocations plus a declared deterministic case
ceiling.**

| counter | owner | fresh auditor |
|---|---|---|
| **substantive** (full `just ci`, full gate, full driven suite) | **12** | **6** |
| **targeted** (one focused command: a generator invocation, a Node probe, a browser probe, a single mutant, a sub-gate run) | **60** | **40** |
| **generator case ceiling per invocation** | **declared in the receipt**, deterministic, seed-bound | same |

**Counting rules, stated so they cannot be argued later:**

- **every** invocation counts at its actual layer — **including failed ones,
  setup failures, and warm retries**;
- a **focused mode that executes a full suite is still a full suite**;
- a **generator invocation is one targeted unit**; the number of *cases* inside
  it is bounded by the **declared case ceiling in its receipt**, never by
  wall-clock;
- an outer wrapper does not launder nested substantive calls.

**This is preparation, not a budget taken.** Nothing is spent; the numbers are a
proposal for the eventual commission.

## 5. Acceptance-to-instrument relation

| acceptance requirement | instrument | status |
|---|---|---|
| independent required **constructor** domain | derived from pinned Lean `AppEvent`, not a hand list | design known; **not built** |
| independent **guard/refusal** domain | derived from the machine's refusal set | design known; **not built** |
| **reachable witnesses** for each domain element | generator + adapter replay | **unknown**: reachability for some guards is unproven |
| **omitted-scheduler failure with unchanged denominator** | denominator bound **externally**, not by the scheduler under test | design known; **not built** |
| **per-step oracle vs final-only control** | both, with the final-only variant as the negative control | design known; **not built** |
| **deterministic seed replay** | seed in the receipt; replay must reproduce byte-identical | design known; **not built** |
| **failure-identity-preserving shrinking** | shrink must preserve the *same* failure identity, proven by a control that shrinks to a *different* failure and is rejected | **unknown**: identity predicate not yet specified |
| **import/export recomputation** | round-trip through the production adapter | design known; **not built** |
| **actual mandatory CI invocation** | the new `just` recipe in §2 | design known; **not built** |

**Named design unknowns — not to be presented as solved:** the failure-identity
predicate for shrinking; guard reachability for refusal classes the UI cannot
currently construct; and whether the generator can drive the *browser* adapter or
only the core. **No instrument here is frozen, and none should be described as
such.**

## 6. Schema intake — bound to the two actual C1 surfaces

The C1 actor-substitution finding makes this an **integration requirement**, not
a naming clarification. Verified in today's tree:

- `lean/Reactivegas/Types.lean` — **`AppEvent` has no `author`**; the signer
  arrives separately (`Step.lean` `appFold`);
- `lean/Reactivegas/Types.lean:43+` — **legacy `Event` declares `author`**;
- the JS `attempt` machine consumes `e.author`, implementing the **legacy**
  surface;
- and the committed corpus schema already keeps them apart:
  `lean-corpus-verify` asserts step keys
  `["accepted","change","event","signer","state"]` — **`signer` is its own key,
  outside `event`.**

**C2 intake must bind both surfaces explicitly**: generated application events
carry **no** actor field, the signer is supplied separately, and the adapter is
the only place the legacy `author` is constructed — **from the signer alone**.
A generator that emits an `author` inside event arguments would reproduce the
exact C1 defect and must be **impossible by construction**, not merely refused.

## 7. File fence — the exact bounded scope question, returned

C1's fence covers `economics-simulator.html`, `economics-simulator-core.mjs`,
`economics-simulator-ui-gate.mjs` and a scoped `lean/lakefile.lean`.

**C2 as designed does not fit inside it**, and I am returning that rather than
hiding an environment assumption or reimplementing the fold:

1. **`justfile`** — a new recipe and its insertion into `ci`. **Outside the C1
   fence.**
2. **A generator/adapter source file** — a new tracked file. **Outside.**
3. **A production *importable* adapter surface** — if the generator must import
   the fold from production rather than reimplement it, that requires production
   to **export** it. **Reimplementing the fold is explicitly refused**: a
   generator that models the machine instead of driving it proves nothing about
   the machine.
4. **A tool declaration** (`node`/`jq` availability in the devshell) if the
   current environment does not already guarantee it in CI.

**Question for the desk, to be answered before any C2 dispatch:** does C2 get a
bounded fence extension covering exactly (1), (2), (4) and a **minimal export**
for (3) — or should the export be a separately audited C1-successor change?
**I am not choosing, and nothing proceeds until it is answered.**

## 8. Remaining blockers

- **Geometry ruling pending** — C1 acceptance cannot complete without it, and C2
  implementation requires accepted C1.
- **Fence question in §7 unanswered.**
- **Owner has 0 targeted / 5 substantive**; the F-02 control gap is already
  returned and unresolved.
- C2 implementation requires **accepted C1 plus its own explicit commission**;
  neither exists.
