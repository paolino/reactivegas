# #92 Q-004 — A2S segfaulted. Three replacement margins spent on the same unit.

Owner `%503`. `%615` stopped at the first required failure and did not diagnose
past it, as ruled. **Spend 4/16 author, 4/20 total.** Unexecuted: A3–A8, I1–I2,
R1–R6, D1, F1. No commit, push or PR.

## What A2S actually did

The wrapper fix was applied **exactly** as prescribed — derived prefix, readable
check, digest, `--load-dynlib` before `--run`, driver and
`import Lake.Load.Workspace` untouched:

```
MIRROR-LAKE-DYNLIB-BINDING prefix=/nix/store/4gr3n4nrp0xxgykyyzdxi3xjj2ikn5x1-lean4-4.25.0
                           path=…/lib/lean/libLake_shared.so
```

`dynlib_sha=bb1c9162` — matching the digest I verified independently before
dispatch. **The product build passed.** Then:

```
error: Recipe `lean` failed with exit code 139
error: Recipe `ci` failed with exit code 139
```

**Exit 139 is SIGSEGV** (128 + 11). The library resolved, was readable, loaded, and
the interpreter **crashed**.

**What I am not claiming:** I have not diagnosed *why* it segfaulted, and neither
has the seat — it was told not to diagnose past the first failure, and no budget
exists for it. Exit 139 says the process died on a signal; it does not by itself
identify the cause.

## The pattern, which is now the real finding

Three consecutive replacement margins have been spent on the **same named unit**,
each for a **distinct execution-host reason**, none of them a model defect or a bad
root:

| unit | cause | charged |
|---|---|---|
| **A2** | `Lake.loadWorkspace` not exported by `import Lake` | yes |
| **A2R** | execution-host failure before evaluated-root reconciliation | yes |
| **A2S** | **SIGSEGV** after the Lake shared library loaded and the product built | yes |

Each ruling was correct on its evidence, and each was narrower than the last. But
the accumulating fact is that **the in-process interpreter route to Lake's
evaluated workspace is not working on this host**, and A2 GREEN — the gate that
unlocks A3–A8 — has never been reached.

## A cheap observation that may matter

`lake` exposes `query <targets>... build targets and output results`, with `--json`
/ `--text` output formats documented specifically for it. That is a **non-interpreter,
first-party** surface. I have **not** established that it can yield declared library
roots, and I have spent nothing to find out — checking would be a product execution
this campaign cannot afford, and guessing is exactly what has cost three margins.

I raise it only so the next ruling has the option in view. **It is a lead, not a
finding.**

## The decision, which is not mine

Options, none chosen:

1. **A fourth replacement margin** for another in-process attempt — the same shape
   that has now failed three times.
2. **A bounded diagnostic unit** to establish *why* the interpreter dies, before
   spending another attempt on a fix aimed at a guess.
3. **A different mechanism** — e.g. whether `lake query` or another first-party CLI
   surface can supply evaluated roots — which would need one unit to test.
4. **Reconsider the requirement's feasibility on this host** and rule what the
   honest contract is if evaluated-data-in-process is not attainable. The original
   bar exists because a text parser is unacceptable; if neither is available, that
   is a fact the contract has to absorb, not something the seat can resolve.

**No unit is spent, no seat is dispatched, and the contract is not weakened.**
`%615` remains blocked and intact. #92 and #66 remain open; no merge is granted.

**S3 is unaffected**: its Grok auditor is working on the single reserved unit of
its own ceiling of 22.
