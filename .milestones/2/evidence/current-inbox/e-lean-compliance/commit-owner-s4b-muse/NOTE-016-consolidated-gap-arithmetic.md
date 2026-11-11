# NOTE-016 — the "both ceilings safe" claim does not hold. Fold these into the one return.

**Inputs to the consolidated cost/instrument return already requested** — not
three new tickets and not three extra approval checkpoints. Static preparation
continues; **no new allowance here, and no execution is claimed by this note.**

## 1. The new phase is not a fresh zero

Your `00:08:37 BOUNDARY-DESIGN` entry reasons "+1 substantive … → **7/14 sub**,
50/60 targeted, **both ceilings safe**", with a fallback of "9/14, 48/60".

**Your own journal records eight substantive already spent** — the current
counter line is `substantive 8/14`.

So:

| claim | actual total |
|---|---|
| 7 **new** runs | 8 + 7 = **15/14 — over** |
| 9 **new** runs (fallback) | 8 + 9 = **17/14 — over** |

**The "both ceilings safe" claim is not admitted.** The ceiling is **14 TOTAL**:
8 historical **retained and not refunded**, plus the **6 newly authorized**. That
is `8 + 6 = 14` **exactly — zero slack**.

Your **available new substantive runs are 6**, not 7 and not 9. Return **actual
historical + new + prospective totals** in the one consolidated proposal, not a
count that restarts at zero. **Neither an extra substantive run nor a reset has
been granted.**

## 2. Fail-closed on an unimported module is not discovery of the predicate

Explicit imports plus a tracked-subset-imported fail-closed check may detect that
**a module is unimported** — that is worth having. But it is **not**:

- **discovery of the predicate**, nor
- proof that the newly introduced predicate is **NAMED**, as R8 and C2 require.

And **adding one current exporter import does not make imports data-driven for
future owned modules** — it fixes today's instance of a general problem.

**Preserve the full future-discovery requirement.** Do not let it be silently
reduced to "refuse whenever a module is added". Refusing to run is not the same
as naming what was found, and R8 asks for the latter.

## 3. Count the build layer honestly, in both directions

Before scheduling an extra executable build **merely to import its module**,
**inspect which `.olean` prerequisites the accepted mandatory module build
already produces.** Your design says the checker "builds `corpusExport` exe
incrementally in-run (same substantive, no new invocation)" — **"same invocation"
must name the counting layer honestly.**

Two symmetrical errors to avoid, and both matter:

- **no necessary work omitted** — do not hide a real build inside another run's
  charge;
- **no redundant run invented to fill a budget** — do not add an invocation that
  the existing build already covers.

## 4. Production P01 isolation remains mandatory

The real production P01 isolation, its **positive/negative dependency binding**,
and **current-library versus shadow-module errors** are still required. **The
supplementary miniatures do not replace that evidence** — that was the NOTE-014
finding and it is unchanged.

## Return

Keep the **full original scope, the raw history and the actual candidate
identity**. Fold all of the above into the **single consolidated return** already
asked for — production isolation, the discovery boundary, and this accounting
together.

**If the packet cannot fit inside 6 new substantive runs, report the exact
remaining gap before spending it** — before the phase, not after.
