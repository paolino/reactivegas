# C2 — random scenario generator: mandate, acceptance, dependencies

Prepared under NOTE-064 while C1 repair runs. **Preparation only.** No seat is
dispatched, no source is edited, and nothing here enters `/code/reactivegas-sim-fable`
until an accepted C1 base exists.

Base: `origin/master = 4a6cd87fcbc3e4a536bbc9f240f5efe5704022af` (S1 landed).
Actual base for implementation is the **accepted C1 candidate**, not `af9c1e5`.

---

## 1. What the research handoff still supports, and what is now dead

`handoffs/random-scenario-generator-research-v1.md` (2026-08-30) is **input, not
current verification**.

| research claim | status |
|---|---|
| blocked: #62 not landed | **obsolete** — merged, transcribed, C1 in repair |
| blocked: `EXPECTED-TRACES` absent | **obsolete** — exists, and is corrected |
| library recommendations for a Haskell implementation | **unverified** — dated facts. Must be refreshed from primary sources before any recommendation. No Haskell implementation is delegated here |
| generator subsumes the machine-coverage gate | retained, and made executable in §3.1 |

## 2. `EXPECTED-TRACES-one-membership.md` is retired as AUTHORITY, not as behaviour

Binding rule: **every property is bound to the accepted Lean predicate, its
hypotheses and its representation, at the pinned base** — never to the prose,
and never to a ruling that has not landed.

**Correction, NOTE-065.** My first draft said the prose's pre-#68 proposer
examples must "not be encoded". That inverted this lane's own standing rule. The
generator follows the **then-accepted base**, including where it differs from a
future ruling. What is retired is the prose as *authority*; the behaviour the
pinned model implements stays authoritative until the model changes.

Each divergence below now cites **its own source**, corrected from my earlier
overbroad attribution to `Predicates.lean` — that file supports conservation and
solvency only.

| # | claim | source of truth | status |
|---|---|---|---|
| 1 | conservation | `Predicates.lean` — `sumBal s.casse - sumBal s.conti - escrowSum s.collections = 0` | prose adds a **separate comune term**; `comuneId` is a key *inside* `conti`, already counted by `sumBal s.conti`. Prose double-counts. **Implement the Lean form.** |
| 2 | solvency | `Predicates.lean` — `solvent (view) (s)` | **view-parameterised**; first conjunct restricted to `GroupView.isMember`. The reserved comune conto may legitimately go negative during a stall. An unconditional "no negative balance" check reports false violations |
| 3 | no orphan rows | **`Step.lean`** — `absorbConto` is `bump (bump conti key (-bal)) comuneId (+bal)` | a bump, not an erase: a departed member **keeps a zero row**. A property forbidding zero-valued rows fails on correct behaviour. Not a `Predicates.lean` claim at all |
| 4 | proposer counted | **`KelGroups/Fold.lean:49`** — `approvals := [signer]` | the **base channel does seed the proposer**, and `majority = (adminCount + 1) / 2`. **Generate against this.** Note the *app vote* channel does the opposite: `Vote/Fold.lean:90` opens with `assents := []`, proposer not seeded. The two channels differ and the generator must model each as written |
| 5 | #76 effects | — | present in prose, **absent from the machine**. §4 |

**D-68 recorded** (§5): when #68 lands, rows 4 and its dependents are refreshed
against the new base. Not anticipated here.

## 3. Executable acceptance

Each row is a gate that must be **proved able to fail** before it counts.

### 3.1 Discovered extent and handler agreement

**Three constructor domains, named because they do not share a coverage rule**
(NOTE-065.3):

| domain | source | today |
|---|---|---|
| economic `Event` | `Reactivegas/Types.lean` | 14 |
| integrated `AppEvent` / base `Proposal`, `BaseMutation`, `BaseChange` | `Types.lean`, `KelGroups/Event.lean` | separate extents |
| `VoteEvent` | `KelGroups/Vote/Event.lean` | separate extent |

Each extent is **discovered** from the pinned Lean, never a name list. The
generator's actual handler set is compared to its own domain: **missing**,
**extra** or **unschedulable** handlers fail.

**Field discovery — mechanism named, not assumed** (NOTE-066.2). C1's
cited-source derivation plus the `Types.lean` `case` parser yields cited paths
and constructor **names**. It does **not** establish field names, types or
arities, and my earlier "constructor and field extent" claimed more than that
mechanism delivers.

The mechanism for fields is an **ephemeral elaborated Lean query** — a
throwaway `lake env lean` script that reflects over the constructor telescopes
and prints name/type/arity — run at gate time and discarded. It requires **no
model edit** and adds no tracked emitter. Its output is consumed directly; it
is never written down as a second list.

**Hand-maintaining a field list is forbidden** — that is the manifest-of-members
defect this milestone keeps deleting. If the ephemeral query proves insufficient
and a tracked additive introspection emitter is genuinely needed, that is a
**scope question to the desk** naming exact paths and ownership, not a silent
addition.

**Coverage rule — the denominator is independent of the generator** (NOTE-066.1).

My previous wording derived the required refusal set from "refusals the
generator actually reaches". That makes the denominator **self-defined**: a
broken scheduler that never schedules a reachable refusal shrinks the required
set and scores full coverage. The measurement moved with the thing measured —
the same defect shape as my metadata-only dispatch extent and my fabricated
oracle controls.

Corrected:

1. **Derive the candidate refusal/guard domain from the accepted model**, by
   enumerating the guard arms of each constructor in the pinned Lean —
   independently of the generator under test, and never from its output;
2. **bind each reachable row with an executable witness**: a concrete sequence
   that reaches that refusal. The witness is the evidence the row belongs in
   the denominator;
3. **require the generator to reach every bound row within declared bounds**
   (seed count, step budget), stated in the receipt;
4. **unreachable arms are named exclusions carrying proof or explicit
   evidence** — a guard that no reachable state can falsify. **A failed search
   is inconclusive coverage, never proof of unreachability**, and must be
   reported as inconclusive rather than silently excluded;
5. **control:** an *omitted-refusal scheduler* mutant — a scheduler that drops
   one reachable refusal — must **fail** the coverage gate, and must do so
   **without the denominator changing**. That is the control proving the
   denominator is genuinely external.

### 3.2 Reachable sequences through the accepted production root
- sequences run through the **accepted production root**, not a parallel
  driver; reuse the repaired C1 aggregate stream where it genuinely applies;
- **explicit initial state**; no handmade mid-run states blessed as reachable;
- signer, auth and policy context carried explicitly per step;
- per-step predicate checks, not final-state-only;
- **deterministic receipts**: same seed and same inputs reproduce byte-identical
  output.

### 3.3 Shrinking
- **structural and argument** shrinking, both;
- a shrink must **preserve the specific failure**, not merely some failure, and
  must preserve command applicability;
- the shrunk witness is **replayed**, and the report names the **first bad
  step**.

### 3.4 Mutants — the generator's own controls

**callback-removal** and **final-state-only** mutants must be caught.

**The transient witness, specified precisely** (NOTE-065.4). My first draft was
ambiguous enough to read as demanding a reachable counterexample to a *proved*
accepted-Lean invariant, which does not exist and cannot be asked for. What is
required is a reachable execution **of a MUTATED simulator** that violates an
actual Lean property and then recovers. Pin all four parts:

1. the **valid prefix** — reachable under the unmutated machine;
2. the **mutated transition** — which mutant, at which step;
3. the **violated predicate** — named, from its own source file;
4. the **later recovery** — the state returns to satisfying it.

Then demonstrate that the **final-state-only checker misses it**, which is the
whole point of the control.

**Keep real checker faults distinct.** A synthetic checker exception counted as
a discovered state violation is not a control — the defect this lane has hit
three times (F5; my fabricated oracle constants; my statically-killed guard
control). A crash is a crash; only a state that genuinely violates the predicate
is a violation.

### 3.5 Export / import
- existing schemas **frozen**; reproducible round-trip;
- **stored values checked against recomputation**, so a stale stored value
  cannot pass as agreement.

## 4. Separation of current-model from undelivered product

Two disjoint property sets, and no oracle may report the second passing:

| set | contents |
|---|---|
| **current-model transcription** | everything in §3, bound to predicates that exist at the pinned Lean today |
| **ruled-but-undelivered** | #76 vote-to-economic producer effects. **It does not exist.** No property may assume a closure it does not consume |

## 5. Owned dependencies — retained, not blocking

| id | dependency | effect on C2 |
|---|---|---|
| D-76 | vote-to-economic producer (#76) | **composition generation** waits on it. Retained as an owned dependency and **kept in closure** — not erased, not used to hold the rest of C2 |
| D-81 / D-75 | #81, #75 | same: composition-generation scope only |
| D-75-R3.1 | the #75 R3.1 context | a **planning contract, coordinated through the desk**. Not a grant to widen #74, and this packet does not widen it |
| D-θ | product theta default | **still open**. An explicit accepted *test* policy is not the product default and must not be recorded as one |

Everything in §3 proceeds without any of these.

## 6. File fence — concrete paths

**May add** (after an accepted C1 base):

```
economics-simulator-generator.mjs         the generator
economics-simulator-generator-gate.mjs    its gate, with --selftest controls
economics-simulator-scenarios/*.json      shrunk witnesses promoted to fixtures
```

**May modify:** `justfile` — one recipe `generator-gate`, and
`.github/workflows/ci.yaml` — one step that invokes it.

**Why the workflow and not just the recipe** (NOTE-066.3). I verified the desk's
finding myself: `ci.yaml` invokes exactly `just lean-toolchain-contract` and
`just lean`. **Nothing invokes `just ci`.** So my earlier proposal — add
`generator-gate` to the `ci` chain — would have produced a recipe that never
runs in CI, and I would have described it as CI-integrated. That is precisely
#74's omission: a green workflow that never runs its new verifier.

Requirements for the wiring:

- an **actual workflow step** invoking `generator-gate`, in the same
  `nix --quiet develop --command` shape the existing steps use;
- **required tools declared by the shell** the step runs, not assumed present;
- a **wiring/omission control that can fail**: remove the workflow step, or
  break the recipe, and a check must go red. A recipe whose absence from CI
  changes nothing is not integrated, and this control is what makes the claim
  falsifiable.

**Do not edit the workflow now** — it would collide with the pending #74 and #66
changes. The edit happens at C2 dispatch, on the accepted C1 base.

**Constructor/field emitter ownership:** the generator **consumes** the extent;
it does **not** own an emitter. The pinned-Lean extent is derived by the same
mechanism the C1 gate already uses (`handoffs/derive-cited-sources.mjs` pattern
plus the `Types.lean` constructor parse). If a new emitter turns out to be
required, that is a **scope question back to the desk**, not a silent addition.

**May not touch:** `lean/` at all — including the two trace producers, which
belong to C1; `docs/en/design/` (frozen while #71 is open); the frozen C1
instruments; any sibling lane's source; master-side scripts; `flake.nix`,
`cabal.project` or any Haskell path.

## 7. Budget and seats

- **no seat dispatched now**; C1's owner is not to be interrupted and no second
  implementation seat enters its worktree;
- implementation seat at dispatch: `muse` (new implementation seats per the
  handover), never as auditor;
- audit seat: `codex` or `grok` only;
- **fresh campaign ledger** for C2; S62-SIM's counters belong to C1 and are not
  reused;
- **`builds_budget` = 20**, `builds_spent` = 0 at dispatch. Same declared unit as
  S62-SIM: one build = one `nix develop -c bash -c 'cd lean && lake build'`.
  Generator runs are not builds and are bounded by the gate's own wall-clock;
- **`ceiling_raises` = 0**, two-submission cap, as S62-SIM;
- no Haskell implementation is delegated by this packet.

## 8. Ready-when

C1 accepted → rebase C2 onto that exact SHA → verify the §2 divergences still
hold against the then-current Lean → dispatch.
