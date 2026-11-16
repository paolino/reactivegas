# #92 Q-005 — A1-recut went RED for the wrong reason. One staging correction needed.

Owner `%503`. `%615` stopped without correcting and without retrying, as ruled.
**Spend 6/19 author, 6/23 total.** Unexecuted: fresh A2, A3–A8, I1–I2, R1–R6, D1,
F1. No commit, push or PR.

## What happened, and why it is not a checker result

A1-recut intended a RED where a **mutated native tool omits one evaluated root**
(`NextLine.Probe`) — the control that proves the checker detects an evaluator
omission.

The run did exit 1. **But not for that reason.** From its own receipt:

```
intended = NextLine.Probe-evaluator-omission
actual   = tracked-walked-disagreement
           subject-not-reached
```

And from line 17 of the log, the cause is visible in the porcelain:

```
?? lean/NextLine/
```

**The probe fixture is untracked.** An earlier mandatory step — the source census
that reconciles *tracked* against *walked* — failed on that disagreement, and the
mirror subject **was never reached**.

**The seat classified this correctly and refused to bank it.** A RED for the wrong
reason proves nothing; taking it as the intended RED is exactly the defect this
campaign has been rejecting in others. It charged the unit and stopped rather than
staging a correction it had no margin for.

## The evidence that the fix is staging, not design

The **rejected** campaign's X6 run recorded `PORCELAIN_AT_START=A lean/Extra/Probe.lean`
— **`A`, staged**. This run recorded `?? lean/NextLine/` — **untracked**. Same
class of fixture, different staging, and that difference alone accounts for the
census disagreement.

This is consistent with the design rather than against it: a probe that is meant to
be a **legitimate registered project root** has to be tracked to *be* project
source. The tool package's own `.lake/` is separately ignored, as the amendment
requires, and that is unaffected.

## What I am not claiming

I have not established that staging the fixture makes A1 produce the intended RED —
only that the observed failure is a tracked-vs-walked census disagreement caused by
an untracked fixture, before the subject was reached. Whether the evaluator-omission
control then fires correctly is what a re-run would establish, and nothing here
substitutes for it.

## The ruling needed

There is **no setup or retry margin** in the frozen schedule, so `%615` cannot stage
and re-run on its own authority. The options, none chosen:

1. **One replacement A1** with the fixture staged — a single unit, and the narrowest
   correction available.
2. **A general staging rule** added to the fixture contract for A1–A8, so the same
   defect cannot recur across the six remaining controls, with whatever unit
   consequence that carries.
3. **Re-cut the A1 control** if the desk judges that an evaluator-omission control
   cannot be staged without disturbing the census.

Option 2 is worth weighing beyond A1: **five further controls (A3–A8) use fixtures
under `lean/`**, and if staging is the general requirement, ruling it once now is
cheaper than discovering it five more times.

**No unit is spent beyond the charged A1-recut, no seat is dispatched, and the
contract is not weakened.** `%615` remains blocked and intact. #92 and #66 remain
open; no merge is granted.

**The native tool qualification stands**: exit 0, `native_build=yes
evaluated_config=yes exact_roots=yes`, three roots on the exact channel including
the namespaced default. That result is unaffected by this staging failure.
