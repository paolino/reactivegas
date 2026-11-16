# Command reconciliation — S62-SIM-C1R. CORRECTED per NOTE-084, frozen before any final run or START.

Supersedes the draft `7730e1823b…`, which was **wrong in two directions at
once** and is withdrawn:

1. It listed the ordinary ui-gate run, `--omit K-2`, and
   `RG_OMIT_NOOP=1 --omit K-2` as separate auditor substantive rows **while
   gate-v15 already invokes exactly those three inside its own step 8**. The
   same work was counted twice.
2. It charged a full v15 as **1** while the campaign's own anti-laundering rule
   says a wrapper does not erase nested substantive calls. Those two errors
   pointed opposite ways and happened to look plausible together.

## The convention, named explicitly rather than switched silently

- **A full ui-gate suite run is a charged unit wherever it occurs, including
  when gate-v15 invokes it.** This matches how the owner already charged S13,
  S14 and S15 individually.
- **The v14 body inside v15 is one unit.** Its sub-gates
  (`claim`, `trace`, `vote-trace`, `scenario`, `teaching`), the oracle, the
  retired-surface probe and `build --check` have been charged as one gate run
  throughout this campaign (S11, S19). That practice is preserved, not revised.

Therefore:

| unit | charge |
|---|---|
| full `gate-v15` | **4** = 1 gate body + 3 nested full ui-gate suites |
| full `just ci` | **1** |

The desk's own ceilings corroborate this arithmetic: owner "up to 5 remaining"
= 4 + 1; auditor "up to 5 each" = 4 + 1.

## Ceilings (NOTE-084)

Owner **22 -> 24 cumulative**, 19 spent retained, **up to 5 remaining**.
Auditor **8 -> 10 substantive TOTAL**, up to 5 per submission.
Targeted unchanged: owner **40**, auditor **60**. Two submissions unchanged.

## Owner final verification — exactly 5, with no retry margin

| # | command | charge |
|---|---|---|
| 1 | frozen `gate-v15` on the integrated candidate | 4 |
| 2 | `nix develop --quiet -c just ci` (the **new** justfile) | 1 |

`gate-v15` does **not** invoke `just ci` — grepped, zero matches — so #2 is not
duplication and may not be skipped or weakened.

**This consumes all five.** There is **no retry slot left**: 19 + 5 = 24. If the
gate or CI reds, the repair needs a further raise, and I will return that gap
before any overrun rather than discover it mid-run.

## Auditor, per submission — 5 substantive / 30 targeted

| # | command | charge |
|---|---|---|
| 1 | full frozen `gate-v15` in the auditor's **own** tree | 4 |
| 2 | cold `just ci` from **absent** project `.lake` | 1 |

**No duplicate standalone ui-gate suites are required.** The three nested
executions inside the auditor's own gate run satisfy those obligations directly,
provided the auditor extracts and retains **individual logs, exit codes and
rejection stage** for each — including the `RG_OMIT_NOOP=1` run, which is what
independently closes the S13/S15 provenance limitation the owner's
byte-identical logs cannot. If the auditor runs additional experiments beyond
these, it **counts them**.

Targeted, **30 per submission**: focused sub-gate mutants
(`claim` / `trace` / `vote-trace` / `scenario` / `teaching`), `--derive-only`
and `--vocab-only` both ways, `--sentence-only` / `--expect-enunciato`,
lakefile-fence, corpus-byte, pin and tree-identity checks.

## Submission 2 is a FULL audit — correcting my own error

My draft said submission 2 targets "the specific repaired classes only". **That
was wrong and it is withdrawn.** It described where focused probes concentrate
and it must never narrow the second auditor's mandate.

**Every submission receives a fresh FULL audit** of the actual final candidate:
the entire inherited unaccepted subject, all original rows open to falsification,
integration and mandate reviewed anew. **No acceptance by inheritance, and no
automatic row PASS from an old report.** Full original-row evidence may
legitimately come from executions inside the full gate — provided the auditor
inspects them independently rather than citing them.

The 30 targeted per submission is reconciled against that full requirement, not
against a repair-only scope.

## Subject

Integrated candidate **`ef773ec4d4b040866eef7dae6b98881cd140c2b1`**, whole
inherited unaccepted prefix through that SHA — **not** the repair range.
