# S2R independent audit returned AUDIT-FINDINGS. PR #88 stays DRAFT.

Local file. Not an acceptance. **No merge request is returned by this handback**,
because the NOTE-049 closeout is conditioned on an *actual FULL PASS* and this is
not one.

## The verdict

| | |
|---|---|
| Terminal verdict | **AUDIT-FINDINGS** |
| Report | `handoffs/AUDIT-REPORT.md`, 401 lines, sha256 `d634df52c51d4351699d36927b5b0c662357a4ac08a7d689a6708db2d34def90` |
| Rows | **24 CLOSED, 1 PARTLY, 0 OPEN** — but one **ADVISORY finding remains OPEN** inside the PARTLY row |
| Blocking functional defect | **None demonstrated** in the commissioned current-configuration behaviour |
| Auditor's own words | "The report is complete, not an acceptance or merge decision." |

The auditor is the same seat throughout: codex `gpt-6-astra`, effort high, PID/PGID
1664317, original START `2026-09-05T14:01:11.657Z`, no fresh START claimed.

## The finding, verified by me at source rather than accepted

**Element 1 — ADVISORY, OPEN: the shipped gate's documentation exceeds its
behaviour.**

`scripts/check-lean-axioms` line 40 states:

```
# Fail-closed: missing/unresolvable `REACTIVEGAS_ROOT`; empty or unset
```

Line 277 is:

```lean
let root := (← IO.getEnv "REACTIVEGAS_ROOT").getD "."
```

A **missing** variable silently defaults to `"."`. It does not fail closed. P22
runs the production driver with `REACTIVEGAS_ROOT` unset from `cwd=lean` and gets
**exit 0** with `axiom-gate: ok` and the baseline identities.

The *unresolvable* half of the claim does hold — `canonRoot` catches and returns
an error. It is specifically the **missing-variable** half that is false.

**This is not a mandatory-path bypass.** Line 293 of the same script exports
`REACTIVEGAS_ROOT` before the driver runs, and S1/S2 pass. The defect is a claim
in a shipped artifact that the artifact does not honour.

Disposition owner is this seat, to route to the implementation owner if a
correction is commissioned. The auditor authored no remedy, which is correct.

## Why I am not treating this as acceptable by residual

I note the shape of it plainly, because it is this epic's own subject: **a
statement whose supporting artifact says something narrower than the statement
does** — found in the gate built to remove exactly that defect from this
codebase. It is one comment line, and it is false in a security-adjacent header
describing fail-closed behaviour.

**My recommendation, which is not my decision:** commission the one-line header
correction rather than ship the claim. Against that, a candidate edit means a new
submission against a frozen one-submission grant and a re-established final
packet, which is not free and is the desk's call, not mine. The alternatives are
to accept with the limitation recorded, or to correct the comment in the S3/S4
lane that next touches the file.

I am not choosing, not editing the candidate, and not accepting by residual.

## What the audit did settle, for weighing

- **The owner's retained contamination limit is resolved independently.** P02/P03
  each reject only their intended omission; P04/P05 pass when only the matching
  guard is disabled. The owner's contaminated P26/P27 remain invalid as necessity
  evidence and are explicitly not retroactively repaired.
- **The comparator limit I raised is closed by execution, not qualification.**
  P27 drove a changed value through the real comparison loop and got the named
  mismatch; P28 got `consumers=1` and the assertion failure; P29 repeated all
  nine comparisons and the full 27-module / 3385-constant scan with opaque bodies
  included. The separate type-mismatch branch remains listed as untested.
- **NOTE-004 is recorded as not a present implementation finding, with reason** —
  no `require`, `packages=[]`, no populated `.lake/packages`, all observed
  dependencies resolving outside the root — while keeping the physical-layout
  assumption explicit.
- **Rebuilt-base provenance is real.** S8 verified all 28 source hashes against
  base Git, removed affected compiled outputs and visibly rebuilt Predicates,
  Invariants and TraceTests; unaffected dependencies are declared as justified
  reuse rather than represented as recompiled.
- Scope limits are named rather than silently closed: `CI-T-SHARED-FILTER` and
  shadow-name independence stay bounded advisories; unexecuted exception, zero
  and type branches are named OPEN in the guard ledger; no claim of exhaustive
  mutation coverage or semantic adequacy of all 1213 statements.

## Spend, final

| | |
|---|---|
| Owner | 17/17 substantive · 35/35 targeted |
| Auditor | **9/9 substantive · 29/30 targeted**, 38 receipts, one targeted reserve unused |
| Reporting release | consumed no execution allowance |

S3 remains charged as a setup failure with no kill credit; P11/P12 remain charged
as ineffective mutations corrected by P15/P16; P19 is an instrument preflight.
All failed attempts, instruments and caches preserved.

## Manifests, verified by me

`FINAL.sha256` (14 entries, self-excluded), `EVIDENCE.sha256` and
`AUTHORITY.sha256` all verify clean **from the runtime root** — their paths are
runtime-root relative, so a check run from inside `handoffs/` reports a false
failure. The admitted owner packet matched at admission, `32299d25…`.

## Not yet terminal

The auditor's own terminal STATUS event carrying the report and final-manifest
hashes **has not yet been appended** — its journal still ends at the
`16:47:37Z RESUMED` entry, and the seat is alive (`Sl+`). The handoff documents
exist and verify; the journal's closing event does not exist yet.

## State, unchanged by this handback

PR #88 **DRAFT**, head `714cb2a`, base `master`, `closingIssuesReferences []`,
milestone 2, assignee paolino, label `chore`; remote CI green from the earlier
run and **not refreshed for this closeout**. Accepted master still `d670323`.
Issue #66 OPEN. PR #85 untouched. Candidate unedited, worktrees clean.

No candidate edit, no extra submission, no ready-metadata transition, no merge
request, no acceptance. S3 Phase 1 and S4-B remain held on accepted **landed**
S2R.
