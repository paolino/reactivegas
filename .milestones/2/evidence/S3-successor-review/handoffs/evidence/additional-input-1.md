# ADDITIONAL INPUT 1 — four source-inspection challenges to the frozen instruments

Issued by `%503`. **These are source-inspection findings, not executed controls
and not a verdict.** They are additional evidence under your **unchanged**
mandate. No severity is forced on you, no verdict is directed, and nothing here
narrows or extends your scope. Disagree with any of them if the source says
otherwise.

## Timing — stated, not backdated

| event | UTC |
|---|---|
| your `START` | 2026-09-06T07:47:06Z |
| these findings reached `%503` | **after** your START |
| delivered to you | see the `status-event` line in `%503`'s `STATUS.md` |

Your original `inputs/` are untouched. The packet stays frozen.

## Provenance and independent confirmation

The four challenges came to `%503` from the desk, which read the frozen files in
full. **`%503` then confirmed each at source rather than relaying it.** The hashes
below were recomputed here and match:

```
0e95e35df63409b39976c5b374f0b7fbd2f929f561b67a2ae55949b5680a09f1  instruments/batch-plan.sh
4cd26f4654ee07a27e78866b0d323c703d0905d6bdfff633891ed95e13d00eac  instruments/compare-batch.sh
8f30ebeef1134e641832f97a4e3f52b0b55fdd4a9ab0b58c90d2811ca552e76b  instruments/replay-run-green.sh
de6c437b3e4a741f15e216bbcd8dab67a4da5baa2a319706d726bc91a544240c  measurement-operations.json
```

## D-01 — `batch-plan.sh` describes the batch experiment; it does not perform it

Every line after `set -uo pipefail` is an `echo`, apart from two `sha256sum`
calls. There is no `git apply`, no `lake build`, no timing, no restore. It prints
`SEPARATE-PROTOCOL:` and `SHARED-PROTOCOL:` as **text**.

`measurement-operations.json` binds `instruments/batch-plan.sh` as the instrument
for **M13-BATCH-SEPARATE-A**, **M14-BATCH-SEPARATE-B** and **M15-BATCH-SHARED**.
A frozen hash on a prose plan does not make those three operations executable, and
SS-4 required real frozen instruments rather than another plan.

**The open question is yours to settle:** does some other bound executable in
`instruments/` actually implement M13/M14/M15? If none does, the SS-4 batch
deliverable is **missing**, not merely thin.

## D-02 — `compare-batch.sh` reports two conclusions it never computes

It `ls`-es the input files (with a real `|| exit 91` guard), then sums the `.ms`
timing globs. It then prints:

```
SETUP-RESTORE-INCLUDED: yes (all ms files include apply+build+restore per frozen scripts)
OBSERVATION-TARGETS-EQUAL: yes (span-bound REDs per mutant scripts)
```

Both `yes` values are **string literals**. The script never reads the `.exit`
file **contents**, never validates the declared file/operation inventory, and
never checks the requested observation identities. Two of its four reported
conclusions **cannot fail**. Do not credit them as checks.

## D-03 — 26 registry rows is not 26 project invocations

**M13** bundles cold + build + restore as one row. **M15** bundles cold + **two**
builds + **two** restores as one row — its own `SHARED-PROTOCOL` text enumerates
five project invocations.

So the registry's `"count": 26` counts **rows**, not invocations. Any future
execution request must **enumerate and count actual invocations** before a numeric
grant. No historical cap and no new grant follows from calling a bundle one row.

## D-04 — `M11A` and `M11B` bind one script that performs both phases

`replay-run-green.sh` takes **no phase argument** and performs **both** the write
(`"$EXE" "$ECON" "$INT"`) and the check (`"$EXE" check "$ECON" "$INT"`) in a
single invocation, writing `m11-replay-exec-green-write.*` and
`m11-replay-exec-green-check.*`. Both **M11A-REPLAY-EXEC-WRITE** and
**M11B-REPLAY-EXEC-CHECK** bind that same path.

It may legitimately be scheduled **once** to account for two explicitly recorded
invocations — **this is not a claim that it must be run twice.** But a scheduler
walking the registry row by row would run it twice, repeating both phases and
**overwriting the same receipt files**. The frozen executable schedule needs an
unambiguous binding.

In fairness to the script: it separates the prebuilt artifact from its build,
is not `#eval`, and carries three real guards — `exit 91` on a non-green write,
`exit 92` when the live-bound `corpus-check: ntraces=` line is absent, `exit 93`
on a non-green check.

## What is not being asked

No source edit. No author repair. No additional submission. No execution grant.
No forced severity. The terminal author `%580` is not returning for another round.

A **specific remaining blocker** is an honest outcome. Do not spend anything to
rediscover source-visible omissions — read the files.
