# Supplementary evidence binding — commissioning defect, stated plainly

**The original 16-entry `admitted/` packet and its `MANIFEST.sha256` are
preserved UNCHANGED.** This is an addition, not a replacement or a rewrite.

## What I omitted, and when this was supplied

The brief said the packet bound the OP-10 raw output, source inventories, the
receipt inventory and the actual costing evidence. **It did not.** The 16-entry
manifest contained the reports, corrections, the final receipt,
`OP10-identities.txt`, two P1A classification artifacts, the brief, the mandate,
the desk identity map and the commission — **but not**:

- the **raw OP-10 stdout/stderr** and its **result record**,
- the **final identity-to-class mapping**,
- the owner's **operative assessment index**,
- the retained **costing and receipt-inventory evidence**.

**That is my commissioning defect, not the auditor's.** The auditor independently
found `INDEX.md` and `OP10-identity-classes.txt` missing from the admitted
manifest at **`2026-09-06T00:23:01Z`**, after passing all 16 manifest checks, and
retained its own supplementary snapshots for reconciliation. **That work stands;
nothing of its capture is reset, replaced or overridden by this supplement.**

**Timing, stated so nothing here is mistaken for pre-START material:** the brief
was delivered and the auditor's START recorded at **`00:21:07Z`**. **These twelve
files are bound AFTER that START.** They are **not** pre-START bytes and must not
be described as such. They were retained by the owner before the audit began, but
their **binding into the admitted set happens now**.

## Bound bytes — `SUPPLEMENT-MANIFEST.sha256`, 12 entries

| file | role |
|---|---|
| `OP10-stdout.txt`, `OP10-stderr.txt` | raw OP-10 output, both streams |
| `OP10-RESULT.md` | the OP-10 result record |
| `OP10-identity-classes.txt` | final identity-to-class mapping |
| `INDEX.md` | the owner's operative assessment index |
| `P1C-build2-incremental.log`, `P1C-build3-restore.log` | costing evidence, the two retained build logs |
| `P1C-scratch-variant-donate.diff` | the measured scratch variant |
| `P1C-tracetests-summary.txt`, `P1C-corpusgate.out` | retained measurement outputs |
| `P1A-S-modules.txt`, `P1A-theorems-grep.txt` | source inventories |

**These are the actual bytes, hashed — not filenames referenced from a report.**
A reference to a mutable file is not an immutable evidence binding.

## The cold log remains genuinely absent

The **full cold-build log was not retained** — the owner's own table concedes
"tail only". **It is not in this supplement because it does not exist**, and
**nothing has been reconstructed to stand in for it.** That absence is an honest
limit and remains a reportable finding.

## Standing

The owner subject is **frozen** while this static audit is in progress.

The auditor **keeps its own independently captured inputs** and reconciles their
identities against this supplement. **Disagreement between its captures and these
bytes is a finding — never permission to rewrite a capture**, in either
direction.

**Missing required evidence remains reportable. There is no forced PASS**, and
none of this supplement converts an absence into coverage.

Scope unchanged: **full Phase 1, zero execution, one terminal report.** No builds,
queries, probes, Phase 2, code edits or ceiling increase are authorized by this
supplement.
