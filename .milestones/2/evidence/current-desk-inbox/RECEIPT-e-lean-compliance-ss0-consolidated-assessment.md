# SS-0 — consolidated assessment of the executed experiment

Owner `%503`. One consolidated read of `SS0-RETURN-v2` against the raw packet. No
further execution granted or needed. **This prototype does not close S3.**

## 1. Raw result, per operation, costs separate

| op | class | named target | exit | duration |
|---|---|---|---|---|
| OP1 cold baseline | substantive | `lake build Reactivegas.Invariants` | **0** | **15,980 ms** |
| OP2 single-atom | substantive | same target, atom applied | **1** | **19,819 ms** |
| OP3 restore + match | substantive | same target, restored | **0** | **3,125 ms** |
| OP4 U-CHECK elaboration | targeted | `lake env lean Check.lean` | **0** | **2,476 ms** |

Wall **42 s** against a 900 s bound. Cumulative **9 substantive / 4 targeted**.
**Nothing is averaged and no class is treated as equivalent to another.**

**A measured fact that refutes a natural assumption:** the incremental mutant run
(**19.8 s**) took **longer than the cold baseline** (**16.0 s**). Any cost model
assuming "incremental is cheaper than cold" is wrong for this chain. That is
precisely the kind of thing a 207-row registry written against assumptions would
have encoded as a systematic error.

## 2. My classification instrument was unsound — the desk is right

The runner's broad `Invariants.lean` searches match **`KelGroups/Invariants.lean:870`**,
a different file, and my literal `:197` search **missed the actual error at `:209`**.
Grep over line-number patterns is not a classification instrument. **I am reading
the raw artifact myself and reporting from that, not from my classifier's output.**

## 3. Source-span binding — the expectation was right, my binding was not

`step_grant_inv` spans **`:197`–`:209`**; `step_deny_inv` begins at **`:211`**.
The error is at **`:209:4`**, which is `exact hx.symm`, the theorem's **closing
term** — inside the named obligation's span.

**The lesson is exact: bind an expectation to the theorem's SPAN, never to its
header line.** Lean reports at the failing term, not at the declaration. My
header-line binding is why the search missed a hit that was there.

## 4. It is a SEMANTIC statement failure, and the log says so in full

```
error: Reactivegas/Invariants.lean:209:4: Type mismatch
  Eq.symm hx
has type      s' = { …, collections := col :: rest, … }
but is expected to have type
              s' = { …, collections := { …, permitted := true, … } :: rest, … }
```

Two **concrete states** that differ in **exactly the mutated field**. This is not
a tactic that stopped working: the conclusion is **false of the mutated program**.
Semantic statement failure, established by execution rather than argued.

**The mutated definition compiled.** `ℹ [17/19] Built Reactivegas.Step (1.3s)` —
so this is not a compile error in the mutated file, and the corroboration the
ruling required is present.

## 5. What the later diagnostics DO and DO NOT establish

**Do:** elaboration continued **~2,140 lines past** the error inside the same
module — `info` lines at `:1639`, `:1640`, `:1641`, `:2351`, `:2352`, `:2353`.
**The "halts at its first failing obligation" universal is refuted by observation
in this experiment**, not by re-reading an older log. And **no error is reported at
or after `:211`**, so `step_deny_inv` did not fail.

**Do not:** absence of an error is not a *positive receipt* that `step_deny_inv`
was elaborated. The lines that prove continuation are at 1639+, not at 211.
Continuation past 209 is **proven**; that this specific obligation was checked is
**inferred** from continuation plus absence of error. Strong, and not the same
thing — I state it as inference.

## 6. Resolution, coldness, and the cached-diagnostic limit

**Resolution:** `nix flake metadata` from the scratch resolved
`git+file:///code/reactivegas-66-s3-ss0-scratch?rev=3590c0015b84fd58004bf6fb44dd18b107304c48`
before dispatch — intended flake, intended revision. Each script recorded its
**actual `pwd`** at the command boundary, not an echoed expectation. That is what
the v1 transport defect cost us and what v2 fixes.

**Coldness:** verified before OP1 as `.lake` absent, zero oleans, porcelain 0 —
and **verified, not manufactured**: nothing was erased to make the claim, because
the failed v1 attempt built nothing.

**The cached limit, scoped precisely.** OP2 shows `Built Reactivegas.Predicates`,
`Built Reactivegas.Step`, but `Replayed KelGroups.Invariants`,
`Replayed KelGroups.Vote.Invariants`, `Replayed KelGroups.Vote.Validate`. So the
**KelGroups warning at `:870` is a replayed diagnostic** and carries no fresh
information. The **`Reactivegas.Invariants` error is not**: that module was being
built when it failed, which is why it appears in neither list. **Source byte
identity alone would not have settled this — the Built/Replayed lines did.**

## 7. Bookkeeping correction

I repeatedly wrote "**6 of 6**" for the v2 instrument manifest. It lists **seven**
inputs: `Check.lean`, `check.sh`, `cold.sh`, `mutant.sh`, `restore.sh`,
`run-ss0-v2.sh`, `SS0-atom.diff`. I carried the v1 count (4) habit forward without
re-deriving. **Re-derived as bookkeeping, not a quota.**

## 8. What SS-0 establishes, and what it does not

**Establishes, by execution:** one single-atom mutation in an isolated cold scratch
produces a **named, semantic, statement-level** failure at the intended obligation,
with the mutated definition compiling and same-module elaboration continuing well
past it; restoration returns to GREEN in 3.1 s; and an isolated `U-CHECK`
elaboration of a fully-qualified proposition is a **distinct, cheap (2.5 s)** unit.

**Does not establish:** anything about the other eight chains, about runtime or
shared/batch costs (`U-REPLAY` remains unmeasured), about any of the 207 rows, or
about whether the incremental/cold relation holds elsewhere. **Five findings remain
partly-blocking. S3 is not closed and Phase 1 is not complete.**

## 9. Revised remaining S3 commission

The original obligations stand **unreduced** — all 207 rows, semantic ownership,
evidence reconstruction, the full measurement set. What changes is **order**, on
the strength of a real result rather than a prose round:

1. **Re-derive the cost model from measured units, not assumed ones.** The
   incremental-slower-than-cold fact alone invalidates the shape the previous
   envelope assumed.
2. **Rewrite the row-outcome basis with span binding**, not header lines. Every
   row that names an expected failure site must name a **span**, and every
   predicted RED must say whether it is expected as a **type mismatch between
   concrete terms** (semantic) or a **tactic failure** (proof-script) — SS-0 shows
   the log distinguishes them plainly when you look at the message rather than the
   exit code.
3. **Then** the registry work (SS-1…SS-3), now written against a measured
   interface, and the remaining measurement classes (`U-REPLAY`, batch/shared) as
   their own bounded experiments **before** they are costed.

**I am proposing, not commissioning.** No further SS-0 retries, no Phase 2 grant,
no S3 closure, and the next slice needs its own dispatch and numeric grant.
