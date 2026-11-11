# S3 successor Q-001-SCHEDULE — returned upward, with my own error corrected

Owner `%503`. The successor owner `%611` did exactly what it was told to do:
preflighted the schedule, found it does not fit, and **returned before spending**.
**Successor spend 0.** Historical spend 5. Ceiling 18. No product command has run.

Its handback: `s3-successor-owner/handoffs/PREFLIGHT-BLOCKED.md`, `d45c5277…`.

## The arithmetic, enumerated from the bound records

The comparison stage is **9 product units, not 5**. Its own extraction, from the
bound `measurement-operations.json`:

| record | product commands | units |
|---|---|---:|
| `M13-BATCH-SEPARATE-A` | mutant `lake build KelGroups.Invariants`; restored rebuild | 2 |
| `M14-BATCH-SEPARATE-B` | mutant `lake build KelGroups.Vote.Invariants`; restored rebuild | 2 |
| `M15C0-BATCH-SHARED-COLD` | clean shared `lake build` | 1 |
| `M15A-BATCH-SHARED-A` | mutant + restored rebuild | 2 |
| `M15B-BATCH-SHARED-B` | mutant + restored rebuild | 2 |
| | | **9** |

`comparison-stage-accounting.json` retains all **17 nested records** — 9
product-charged, 8 setup-uncharged.

**5 historical + 7 owner (S1–S7) + 9 comparison + 1 audit = 22, against a ceiling
of 18. Shortfall 4.**

It also tested the strongest reuse available — S1 serving as shared cold, S2 as
shared-A mutant, S3 as shared-A restore — **explicitly not as an authorized
schedule change**, and even then six additional calls are required against the cap
of five. It names the **precise sixth unexecuted command** with full argv, cwd,
base, precondition and expected observation: `M15B-BATCH-SHARED-B`,
`invocations[n=4]`, `U-RESTORE`,
`["timeout","--signal=TERM","--kill-after=15s","300s","lake","build","KelGroups.Vote.Invariants"]`.
The cwd is a **planned location only** — no scratch or build tree was created.

**S1–S7 are all unexecuted.** The fit failure was returned before spending, as the
inherited adjudication requires.

## Preservation is complete

Both prior manifests independently verified against their **entire recursive file
populations**: **632/632** and **636/636**, no self-entries, duplicates, missing
files or extras. Every prior path, including each old manifest, now has a
byte-identical historical copy under `retained-history/submission-1/` and
`retained-history/submission-2/`. The replacement-by-omission failure is closed.

## My error, which the seat caught

My successor packet's requirement 3 said `compare-batch.sh` *"prints
`SETUP-RESTORE-INCLUDED: yes` and `OBSERVATION-TARGETS-EQUAL: yes` as string
literals, reads no `.exit` contents, and reconciles no declared file population."*

**Verified at source, that is false for the current comparator.** Both packets ship
`8b4dfd78d5748cfb45b28ebe95ebfa0316391b5781c392a907a30ad4e2995a17` — genuinely
byte-identical, so the delta inspector's *identity* claim was right — but that file
**does** read `.exit` contents (`cat "$OUT/$f"` at `:26`, `:32`, `:42-45`) and
**does** require an exact declared population (`REQUIRED_EXITS` at `:21-22`, "each
`.exit` must contain 0"). Its conclusion lines are emitted **after** those checks:
`:51` `OBSERVATION-TARGETS-EQUAL (read from .exit contents above)`, `:57`
`SETUP-RESTORE-INCLUDED (apply+build+restore .ms files all summed above)`.

The constant-`yes` defect belonged to the **original** S3 packet's comparator. The
recovery campaign had already repaired it in **submission 1**, and submission 2
carried the repaired file forward unchanged. I read "byte-identical to submission
1" and re-attached the *old* defect description to it without opening the current
bytes. **Sixth instance this session of accepting a characterization instead of the
file.**

**Requirement 3 is therefore largely already satisfied and is amended.** What may
remain is narrower and I assert it no further than the bytes support: `:42-45`
compare **exit classes**, which is not the same as comparing **observation
identities**, and stale-extra-file reconciliation is not evident. That residual —
if it is one — is for the successor and its auditor to settle.

## What is needed

A consistent, fully enumerated funding or schedule ruling. The options I can see,
none picked as settled:

1. **Raise 18 → 22**, funding the complete subject as enumerated.
2. **Authorize the reuse schedule explicitly** — S1 as shared cold, S2/S3 as
   shared-A mutant and restore — which still leaves **one** unit short of the
   comparison cap and would need that one unit granted, and which weakens the
   independence of the cold anchor from the comparison.
3. **Re-cut the comparison subject** to a smaller admitted population, naming
   exactly which records are dropped and why — an explicit ruling, not my silent
   narrowing.

**Nothing is dispatched and nothing is executed pending that ruling.** S3 is not
accepted; F-01, F-02, F-03, F-06, F-07 remain PARTLY — blocking; `#66` is open.
**#92's ledger is untouched by this.**
