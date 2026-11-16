# NOTE-030 — diagnosis confirmed and extended; +1 escalated, not granted. Hold.

## Your diagnosis is right, and I found a second independent cause

I ran the mechanism in bash rather than reason about it:

```
old pattern -> def x := #[__TRACKED_MODULES__]      unchanged — never fired
new pattern -> def x := #[LIT]                      correct; #[ and ] survive
control     -> "#__TRACKED_MODULES__ at start" => "LIT at start"
```

The control is the part that makes this evidence: a `#`-leading pattern *does*
substitute when the match sits at position 0, so the anchoring rule is real and
the check can return a different answer.

**You named one cause; there are two.** (1) Bash anchors a `#`-leading pattern to
the start of the value. (2) The literal text is `#[__TRACKED_MODULES__`, so
`#__TRACKED_MODULES__` is not even a substring of it. Either alone is fatal. Say
both in the submission — a repair that fixed only the anchoring would still fail,
and the record should show the defect was understood, not merely worked around.

Your fix is correct and minimal: `git diff` is exactly that one line, tree
otherwise clean, and `#[` / `]` are Lean's array-literal delimiters that must and
do survive. I verified `4d0a324` introduced the placeholder, so no earlier run
could have caught it.

**One adjacent trap, since you are near this code.** Do not "harden" the pattern
by putting `#[` into it. In `${var//pattern/repl}` the pattern is a glob, and `[`
opens a bracket expression — it would not match a literal `[`. The current
unadorned `__TRACKED_MODULES__` is the right pattern precisely because it carries
no glob metacharacters.

Your validation discipline here was the right one and cost nothing: you proved
the mechanism by bash-only simulation and stated that execution proof awaits the
retry rather than claiming it. Keep doing exactly that before any spend.

## The +1 is escalated, not granted

Substantive **16 → 17** is a ceiling raise and that is the desk's, not mine. I
have escalated it with the evidence above, stating plainly that a +1 restores
exact fit with **zero slack** — seven runs against seven — and that this is the
**second consecutive substantive operation lost to a harness-script defect rather
than to the artifact under test**. That framing is mine to carry upward; you were
right not to assume the raise.

I found no relief inside the existing cap and did not manufacture any. The failed
`O1retry` was a real invocation and stays spent — failed and warm calls count. The
O-phase is not compressible without weakening the clean-build baseline, and I will
not reclassify to make room.

## Hold

Stay at `BLOCKED-SUBSTANTIVE-GAP-1R`. Nothing further to run. Keep the failed log
and the counters as they are (`1/2, 10/16, 52/60`); do not recount, do not
re-baseline, do not restart. If free work remains — record tidying, the two-cause
write-up above, the retained-receipt discipline from NOTE-029 — do that; it costs
nothing. Otherwise wait for the grant.
