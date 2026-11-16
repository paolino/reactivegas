# #92 — single adjudication, one repair batch, and a concrete execution gap

Owner `%503`. **This is the one adjudication of the union.** No second
adjudication, no re-adjudication at any altitude. Submission 2 is the last.

| inspector | verdict | manifest |
|---|---|---|
| C — does the repair hold | **AUDIT-FINDINGS** | `1a7970eb…`, verified, no self-entry, **I7 unspent** |
| D2 — fences, quota, accounting | **AUDIT-FINDINGS** | 43 entries, verified, no self-entry, **I8 unspent (0/1)** |

D (the first attempt on D2's rows) returned `SCOPE-FAIL` with no candidate verdict
after crossing the packet fence — **partly my dispatcher error**, a symlink where a
read-only copy was owed. Its rows were re-inspected, not skipped.

## What passed — established, not conceded

D2 read every log end to end, with spans stated (I1 1–18032, I2 1–3139, I3
1–18033, I4 1–2853, I5 1–2980, I6 1–18122). No `head`, no tail-only.

- **Row 1 ownership — PASS.** All three retained trees carry **exactly one**
  tracked modification: `scripts/check-lean-mirrors`, **+24/−10**, mode unchanged.
  No simulator-lane edit, no driver deregistration.
- **Row 2 semantics — PASS.** Correspondence, exception and promotion tables and
  the harvester are **byte-equal** between base `:66-178` and repair `:76-188`.
- **Row 3 quota — PASS**, by independent source derivation of each commit's
  tracked Lean files. `tracked=31` and `tracked=29` reconcile from discovery.
- **Row 6 regression — PASS** for the preserved path: the pre-existing named
  reach-gap/refusal behaviour is unchanged, asserted rather than rebuilt.

## The batch

**P-01 (C/F001) — BLOCKING. The selector uses name shape, not Lake's declared
roots.** `grep -v '\.'` keeps dot-free names, but Lake 4.25.0 permits a library
target with ``roots := #[`Extra.Probe]`` — confirmed against the pinned toolchain's
own `LeanLibConfig.lean:29-45`, which defines roots as an array of Lean names. A
properly registered namespaced root is therefore **tracked, dotted, and filtered
out**, reproducing the reach gap the repair exists to close and violating the
acceptance bar's "no exclusion of legitimate project sources".

**Derive the import set from Lake's declared roots**, not from a name predicate.
C's limit is preserved exactly: this is a **static selector/declared-root
mismatch** with runtime failure **predicted, not observed** — `Extra.Probe` was
never built in that inspection, and no such claim is made.

**P-02 (C/F002)** — the submitted source-fallback explanation is unsupported.
Correct the explanation or supply what supports it.

**P-03 (D2/F-D2-01) — rows 4–5.** None of the six logs carries a full quality SHA,
a C1 SHA, a quality tree hash, the submitted checker SHA-256, or a `nix develop`
command record. I2 and I6 **are** distinct artifacts with different paths, nonces
and tracked counts — but distinctness is not the requirement; **binding to both
exact candidates is**. Fix the receipt harness so every run records its
contemporaneous command and tree/control binding. D2 is explicit that this is
"blocking for the requested assurance, **not evidence of defective product
behavior**".

## The execution gap — returned before spending, not narrowed

**Six author units are spent; four remain of ten.** I7 and I8 went unspent.

Changing the selector invalidates **every demonstration that exercises the repaired
checker**: I2, I3, I4, I5, I6. Add the namespaced-root control that P-01 now
requires, and the delta inspection of submission 2:

| needed | units |
|---|---|
| re-run I2, I3, I4, I5, I6 against the repaired checker | 5 |
| namespaced-root control — a registered `Extra.Probe`-style root **built and covered** | 1 |
| delta inspection of submission 2 | 1 |
| **total needed** | **7** |
| **available** | **4** |

**It does not fit, and I am not narrowing it to make it fit.** I1 is the only
demonstration that survives unchanged, because it records the **pre-repair** RED.

**I have therefore dispatched only the zero-execution work** — the selector rewrite
against Lake's declared roots, the F002 correction, and the receipt-harness binding
— with an explicit instruction to **spend nothing** and to freeze a static
submission. The three-unit shortfall is the desk's to rule on. Options I can see,
without recommending one as settled:

1. Grant three additional units for this campaign (a raise, which NOTE-078's
   sibling terms forbid without an explicit decision).
2. Re-cut the demonstration set: accept that some controls are re-established by
   argument from an unchanged checker path rather than re-execution, naming exactly
   which and why — this weakens the evidence and should be an explicit ruling, not
   my silent choice.
3. Accept submission 2 as **static-only** and defer all re-execution to a
   separately granted campaign, leaving #92 open with its execution branch named.

## Fences unchanged

No production-semantics change, no `docs/en/design/` writes, no simulator-lane
edit, no driver deregistration, no ad hoc name-list exception. Submission 2 writes
to its own directory; submission 1 is never overwritten. No push, PR, merge or
issue comment. S3's budget is separate and untouched.

**#92 is not accepted and #66 is not closed.**
