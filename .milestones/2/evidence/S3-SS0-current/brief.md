# BRIEF — SS-0 bounded real experiment (S3 measurement worker)

You execute **one frozen sequence, once**, and return **one packet**. You are a
measurement worker, not an author or an auditor. **This is a bounded prototype and
NOT S3 acceptance**: the five partly-blocking findings and every original
identity/atom/receipt/measurement obligation stand unreduced.

## Bindings

| field | value |
|---|---|
| commissioner | **`%503`** — `inbox/`, `questions/`; I answer in `answers/` |
| model / effort | `pi --provider opencode-go --model muse-spark-1.3-contributor --thinking xhigh`, the existing approved pin. **Verify your own live argv and state it in your first journal line.** |
| runtime root | this directory; `STATUS.md` append-only, live UTC via `/code/llm-settings/shared/skills/worker-protocol/scripts/status-event` |
| **only mutable source** | `/code/reactivegas-66-s3-ss0-scratch`, detached at **`3590c0015b84fd58004bf6fb44dd18b107304c48`**, clean, **genuinely cold — no `.lake`, zero oleans** (verified at creation) |
| **forbidden** | the candidate and repair worktrees and **their build artifacts** are neither writable nor executable inputs. No product commit, push, PR, semantic change or acceptance. |
| budget | **exactly 3 substantive + 1 targeted**, **no retry or exploration reserve**, **whole-experiment wall ceiling 15 minutes** |
| accounting | historical S3 **5 substantive / 3 targeted** remains; if all four execute, cumulative **8/4**. **No static submission count is reset.** Setup failures consume the actual operation. |

**Preparation, parsing and hashing are not permission to hide another compiler
invocation.** The only Lean/lake invocations are the four inside the frozen runner.

## What you run — once

```
bash instruments/run-ss0.sh
```

Fixed order and frozen timeouts inside the 15-minute ceiling:

| op | class | command target | timeout |
|---|---|---|---|
| 1 cold baseline | substantive | `lake build Reactivegas.Invariants` | 420s |
| 2 single-atom mutant | substantive | same named target, atom applied | 150s |
| 3 restore + matching build | substantive | same named target, restored | 150s |
| 4 U-CHECK | targeted | `lake env lean instruments/Check.lean` | 60s |

**Named chain targets, not bare `lake build`.** Measurements concern the
**C-STEP chain only** — not 207 rows and not the repository.

## The frozen subject, fixed before START

- **Atom** — `instruments/SS0-atom.diff` `8c0b9e5d3431238ed7431bf107ccdf69534de42105fbc9bdd681c2ddf143e003`,
  one line at `Reactivegas/Step.lean:55`:
  `pure { s with collections := { col with permitted := true } :: rest }`
  → `pure { s with collections := col :: rest }`.
  It applies and reverts cleanly; I verified both.
- **Source hashes at `3590c001`** — `Step.lean` `f498490c…`, `Invariants.lean` `0ffbbfc7…`.
- **Expected outcome — a SEMANTIC STATEMENT failure at a named obligation.**
  `step_grant_inv` (`Invariants.lean:197`) concludes
  `s' = { s with collections := { col with permitted := true } :: rest }`.
  The atom removes exactly that field, so the post-state equation becomes
  **false**. This is *not* proof-script sensitivity: the statement itself no
  longer holds.
- **Selected unchanged obligation that must be REACHED** — `step_deny_inv`
  (`Invariants.lean:211`), later in the same file, which does not consume
  `permitted`. Its being reached and proved while `:197` fails is **observed
  evidence** of same-module continuation. **No source-order-only claim that Lean
  stops at its first theorem error is permitted** — it is established by the log
  or not at all.
- **Required corroboration** — evidence that the **mutated `Reactivegas.Step`
  compiled successfully**. If Step itself fails to compile, the run measured a
  compile error, not a semantic kill.
- **U-CHECK** — one isolated **elaboration** of
  `theorem ss0_check_elaboration : Reactivegas.checkSweepIdempotent = true := by decide`,
  fully qualified, **after the clean restore**. It is **not** `#eval` relabelled
  and **not** a runtime replay. Runtime and shared/batch costs remain required
  later and are **not** measured here.

## Stop semantics — read this twice

- **The expected RED at OP2 is an OBSERVED RESULT, not a stop.** The runner
  continues to OP3 and OP4.
- **Stop on an UNEXPECTED outcome, setup failure or loading failure.** The runner
  does this for you: OP1 or OP3 non-zero, OP2 timing out, or **OP2 returning
  GREEN**.
- **A surprising GREEN is NOT success.** If the atom does not redden, that is a
  blocker to report, not a pass.
- On any stop: everything is preserved, the scratch source is restored **without
  another verification build**, and the exact blocker is returned. **No retry.**

## What is recorded

The runner binds toolchain paths and hashes, argv, cwd, `LEAN_PATH`, the resolved
olean set after OP1, timer boundaries, exit capture and restoration checks, and it
records that **no environment variable references a candidate or repair worktree**.
The deliberately mutated source inside the cycle is **recorded as dirty, never
called clean** — "before/after clean" refers to the mutation/restore cycle only.
The runner **refuses to overwrite a prior run directory**.

## Return

One packet: your journal, `evidence/run/`, and a short `handoffs/SS0-RETURN.md`
stating per operation the **exit, duration, named target, and whether the outcome
was semantic, proof-script, or setup**. Do not draw S3 conclusions — I read the
packet against raw exits and loaded artifacts at my altitude.

If the four-operation sheet cannot meet all of the above inside 3+1 and 15
minutes, **return the exact missing operation before spending anything.**
