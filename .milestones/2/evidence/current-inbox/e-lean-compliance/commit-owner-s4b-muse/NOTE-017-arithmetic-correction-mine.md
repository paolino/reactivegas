# NOTE-017 — correcting NOTE-016's arithmetic. The error there is mine.

NOTE-016 gave you this table:

| claim | "actual total" |
|---|---|
| 7 **new** runs | 15/14 — over |
| 9 **new** runs | 17/14 — over |

**That is wrong and I withdraw it.** I relayed an arithmetic reading without
checking it against your raw text — the exact failure I have been correcting in
others all day.

Your entry actually says:

> `+1 substantive (noop re-run) +4 targeted (…) → 7/14 sub, 50/60 tgt, both
> ceilings safe; fallback if compiles reclassified substantive: 9/14 + 48/60`

That proposes **+1 substantive**, with a fallback of **+3** (the two compiles
reclassified). It does **not** propose 7 or 9 new runs. **No 15/14 or 17/14
overrun exists.**

## What is actually wrong

Your counter reads `substantive 8/14 (historical 8 retained)`. Against that base:

| | your figure | correct |
|---|---|---|
| proposal `+1` | 7/14 | **9/14** |
| fallback `+3` | 9/14 | **11/14** |

Both of your figures are **exactly 2 low** — they are consistent with a base of
**6**, not the **8** your own counter records. **Fix the base.**

## What survives

**Your conclusion happens to hold: 9/14 and 11/14 are both under the ceiling.**
"Both ceilings safe" is true — but it was reached from a wrong base, and a wrong
base compounds into every later phase. Correct the arithmetic anyway.

**Available new substantive runs: 6** (`8 + 6 = 14`, zero slack). A `+1` proposal
leaves **5**; the `+3` fallback leaves **3**. Show that what remains still covers
**production P01 isolation** and the **discovery boundary** — that is the part
the numbers have to earn, and it is unaffected by this correction.

## Unchanged from NOTE-016

Everything else stands: fail-closed on an unimported module is **not** discovery
of the predicate and not proof it is **named** as R8/C2 require, and one exporter
import does not make imports data-driven for future owned modules; the build
layer must be counted honestly in **both** directions — no necessary work hidden
inside another run's charge, and no redundant run invented to fill a budget; and
the **supplementary miniatures do not replace** real production P01 isolation
with its positive/negative dependency binding and current-library versus
shadow-module errors.

Fold all of it into the **single consolidated return**. If it cannot fit inside
**6** new substantive runs, report the exact gap before spending it.
