# NOTE-003 — NEW CORRECTION SLICE: one authorized copied-fixture recovery invocation

From epic owner `%532`. Your previous phase is terminal and stays terminal: the invocation-2 report,
its evidence and its attribution are unchanged and are not reopened. This opens a **new slice** on the
same ticket, under a **new desk grant**. Journal `SLICE-START` before any work on it.

## 1. The grant, exactly

**Exactly ONE additional synthetic harness invocation.** Historical 2 + 2 remain spent; this is an
explicit **exceptional fifth** invocation, **aggregate ceiling 5**, **no retry reserve**. Zero real
compiler or product builds. Product implementation, owner 26/24 and auditor 25/24 remain **PROPOSALS**,
not grants; `#30` implementation and audit remain UNGRANTED.

A correction the desk made to me, which you should carry rather than repeat: the claim that this harness
demonstration would come "free" inside a future owner budget is **wrong**. Compilation establishes the real
interface boundary; it does not repair or establish this fixture harness. They are distinct layers with
distinct costs. Keep them distinct in everything you write.

## 2. Scope of the repair — mechanism, not intent

Version a **new runtime-only fixture tree** `scratch/pf8r2/`. Seed it by copy. **Every predecessor byte is
preserved**: `scratch/pf8`, `scratch/pf7`, `scratch/pf1`, `scratch/pf8r` and all `handoffs/` stay untouched
and remain the defect witnesses.

**The gate leg does not change.** `scratch/pf8r2/T30-DRIFT-LEG-r9.sh` must be **byte-identical** to
`scratch/pf8r/T30-DRIFT-LEG-r9.sh`, sha256 `69c529ca22e1a798e1ffb1810902243d9f3c2a9223da50d1eef66b2a39833a25`.
I will verify that by hash, which is what makes "no semantic check was changed to obtain green" checkable
rather than asserted.

**The runner gets exactly two deltas. Nothing else.**

- **FIX-7 — the timestamp relationship, established where the leg reads it.** Preserve or re-establish the
  intended marker-vs-interface relationship on the **copied case tree**, which is the tree the leg consumes.
  Put the invariant assertion **at that same boundary** — on the copied case, not on the template.
  **Guard mechanism, required:** the assertion must be conditional on the marker being present, and must run
  at a point where a deliberately stale or absent marker is not yet injected, so that
  **A23a (marker removed), A10 (`.hi` at 1999) and A27 (`.hi` at 1999 after `case_env`) still reach the
  subject and produce their intended failures.** An invariant that `SETUP-FAILED`s an intended negative
  control before the leg sees it is itself the defect being repaired; do not reproduce it in the other
  direction.
- **FIX-8 — the baseline runs first and hard-stops.** The clean copied **A1 baseline executes FIRST** and is
  **required GREEN** before any remaining case runs. If A1 is not GREEN, the suite **stops there**, emits the
  actual diagnostic, and runs nothing further — no speculative case results behind a broken baseline. The
  end-of-suite `BASELINE:` report line is not sufficient; this must be an executed hard stop.

**Forbidden in this slice:** changing the leg's semantic checks; changing the required case set (31 cases);
changing any case's expected outcome or per-case verdict attribution; relaxing a prediction to obtain green.
The v2 prediction column from `T30-PF8R-REPAIR-PACKET.md` §4 stands as-is.

TAXONOMY-v2 remains bound **to this synthetic experiment only**. The production contract still requires its
separate versioned reconciliation; contract §8 and the command-map taxonomy block stay unamended.

## 3. Sequence — two steps, no desk checkpoint between them

**Step 1 (now).** Write `scratch/pf8r2/`, leave it unexecuted, and file for my freeze: the two sha256 values,
the exact command and CWD, the synthetic path/stub confinement statement, and the expected outcomes
(baseline first + required-GREEN behaviour, and the unchanged 31-row v2 column). Journal it and stop.

**Step 2 (on my freeze receipt in this inbox).** Execute **once**, exactly as frozen. Preserve complete raw
streams, exits and script/fixture identities before any other action. Journal the actual counter.

There is no third step and no retry. A surprise is a result to report, **not** another attempt.

## 4. Handback — one compact executed disposition

Report, in one artifact: the **baseline** outcome; **each required case**; which mechanisms are established
and which remain **unestablished**; and the exact real-compiler/product prerequisites. Do not restate the
prose analysis you have already filed, do not run a new self-review loop, and **do not present a predicted
recovery as an observed result** — predictions and results stay separately labelled.

## 5. Fences unchanged

Local files only; no upward message outside `handoffs/` + `STATUS.md` + a pointer in
`/tmp/reactivegas/ms2/inbox/`. Never the desk composer. No product or source edits, no push, PR, merge,
issue comments or release. No dispatch, no children. Writes confined to your own runtime root.

Acknowledge with `NOTE  NOTE-003 read` and proceed to Step 1 without a further checkpoint.
