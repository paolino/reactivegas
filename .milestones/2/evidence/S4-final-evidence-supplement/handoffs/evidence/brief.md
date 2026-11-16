# BRIEF — S4 STATIC FINAL EVIDENCE SUPPLEMENT (one terminal verdict)

You are a fresh independent auditor commissioned by quality epic owner `%503`
(issue #66, milestone 2). You have **no** inherited audit context. You have never
seen this candidate. Read every input yourself.

## Identity you must verify before START

| | |
|---|---|
| model | `gpt-6-astra`, effort `high` — verify the **live** session, not a launch flag |
| root | your own fresh runtime directory; do **not** reuse any prior audit root |
| candidate | `04eb6c7d9aeb2a3602fca5ece14cbc033221cb43`, tree `caaa0488f39a6afb2553680a11fd6bfd86d1c90b` |
| accepted base | `3590c0015b84fd58004bf6fb44dd18b107304c48` |
| source worktree | `/code/reactivegas-66-s4b-final-audit` — **read-only** |

Own a post-cursor START in your `STATUS.md`, then work.

## Scope — what this is, and what it is not

This is a **supplement**. It reviews **FS-01**, **FS-02** and an explicit
**documentation-boundary disposition** against the existing **FULL eight-commit
final audit** (`inputs/PRIOR-FULL-AUDIT-REPORT.md`, sha256
`43db90494fbad83282092d388382651d9f2d56e7aefe30da7b2c66e08443fe9c`) and its
**74-entry evidence manifest**, both **required inputs**.

**It is not** a second mutation campaign, **not** a re-run of unchanged semantic
checks, and **not** a newly executed full audit — do not describe it as one in
any wording. The prior full report's conclusions retain their original scope and
provenance; they remain **challengeable if your reading contradicts them**, but
they are not re-derived here for their own sake.

## Hard limits

- **ZERO project execution.** No `lake`, no `just`, no `nix develop -c`, no Lean
  elaboration, no probe, no mutation, no build. Static reading, parsing and
  hashing only.
- No author contact, no sibling-lane contact.
- **Local delivery only** — your own `STATUS.md` and your own `handoffs/`.
  Nothing to any human composer.
- **No candidate edit.** If you conclude a source edit is required, **return that
  as a finding to `%503`**; do not make it and do not ask anyone else to.
- No loop, no auto-repair, no second submission. **One terminal verdict.**
- **No instruction here requires a PASS.** A specific remaining blocker is a
  complete and acceptable outcome.

## Inputs — frozen, `MANIFEST.sha256` in the parent directory (11 entries)

Verify the manifest from the `inputs/` directory before reading. `MANIFEST.sha256`
has no self-entry.

| file | what it is |
|---|---|
| `PRIOR-FULL-AUDIT-REPORT.md` | the terminal FULL static audit, verdict AUDIT-FINDINGS, 07:06:01Z |
| `PRIOR-FULL-AUDIT-MANIFEST.sha256`, `PRIOR-FULL-AUDIT-MANIFEST-CHECK.txt` | its 74-entry evidence manifest |
| `PRIOR-FULL-AUDIT-ONWARD.md` | its onward discoveries |
| `S2-CI-final-clean.receipt.txt`, `S2-CI-final-clean.log` | the new clean-committed CI evidence (completed 07:07:14Z, **after** the terminal verdict) |
| `SUPERSEDED-S2-CI-comment-only.log` | the earlier log, preserved unchanged; opens with two dirty-tree warnings |
| `S4-FS01-FS02-CONSOLIDATED-DISPOSITION.md` | `%503`'s consolidated disposition — **the thing you are auditing**, not a source of truth |
| `CLOSURE-MAP.after-fs02.md`, `CLOSURE-MAP.pre-fs02-preserved.md` | the FS-02 repair and the exact pre-repair state |
| `NOTE-072-…md` | the commissioning note, including the desk's own withdrawal |

## The question you must settle

**Is the new evidence actually compatible with the full final candidate and with
the source-sensitive mandatory consumers — or is there a specific remaining
blocker?**

### FS-01 — the clean-final receipt

Establish, at source, each of: actual clean-final receipt integrity; candidate
identity before and after; captured exit code; complete stream digest against the
preserved log; and **execution of the source-sensitive census / checker path**.

`%503`'s assessment states four limits. **Challenge each independently; do not
inherit them.**

- (a) it calls the run **cache-assisted, not cold**, on the ground that all 17
  `[n/m]` progress lines read `Replayed` and none reads `Built`, while noting
  Lake prints a line only for a job emitting diagnostics — so silent fresh
  compilation is not excluded. Is that reasoning right, and is the conclusion
  correctly bounded in both directions?
- (b) it concludes the `#print axioms` lines in this log are **replayed cached
  diagnostics** and therefore supply **no new axiom evidence**. Verify.
- (c) it states the repaired module `Reactivegas.Mirrors` is an **import-graph
  leaf** that emits no diagnostics, so this log shows **neither** a `Built` nor a
  `Replayed` line for it, and its build/replay status is therefore not readable
  from the log. **This is the residual `%503` hands you.** Determine whether some
  other artifact in the frozen inputs settles it, or whether it stands as a
  blocker.
- (d) it argues a `Replayed` line is Lake asserting input-hash equality, so the
  cached oleans correspond to the committed bytes regardless of which run
  produced them. Test that argument.

Also test the positive claims: that the mirror driver **imports
`Reactivegas.Mirrors`** and attributes declarations by module home; that
`MIRROR-RECEIPT-WROTE nonce=1788678416820927632` shows the driver ran **in this
run**; that the census `rows=19 exceptions=4 discovered=24 promoted=2 tracked=29`
is unchanged from the pre-repair run; and that the negative controls at log lines
3, 8 and 36 are controls that **can** fail rather than checks that cannot.

Owner budget is **20 substantive / 52 targeted, spent**. No project operation is
available to you or to anyone; if the evidence is insufficient, **say so** rather
than requesting a run.

### FS-02 — the closure-map record

The dated ruling `RG-S4-REACH-20260906` required recording in the **#66 issue
body and the closure map**. Verify at source that the current map now carries the
ruling, the **OPEN finite-history correspondence**, **S5 ownership with #75 and
#71**, the precise **genesis / fixed view-auth / refusal** distinctions, the
existing **retention and inversion** obligations, and **H-01 / H-02 / H-03**.

Verify also that the **prior omission is preserved** in the historical section.
**AMENDMENT 1 supersedes the additive-only test that stood here** — read
`inbox/AMENDMENT-1-CURRENT-STATE-AND-ADDITIVITY.md` and apply its four-point
criterion instead. For reference, the withdrawn text was: diff
`CLOSURE-MAP.pre-fs02-preserved.md` against `CLOSURE-MAP.after-fs02.md` and
confirm the change is additive apart from the single amended bullet line
`%503` declares. Confirm `%503`'s **correction of its own earlier false claim** is
recorded rather than the earlier text being silently rewritten, and that **no
`docs/en/design/` file** was touched.

### The documentation boundary

The desk has **explicitly withdrawn** the unqualified claim that a
documentation-only repair has **no** source-sensitive effect. It is withdrawn, not
proved. Assess `%503`'s disposition against that.

**Allowed observed effects** of this exact two-site repair: module and declaration
doc metadata; declaration source ranges; raw generated driver documentation bytes;
source/build hashes induced by the text.

**Required unchanged:** non-comment program bytes; proof statements and terms;
exception membership; imports; runtime and checker logic.

No claim of literal byte-identical generated source and no zero-metadata claim is
permitted. Old compiled artifacts, ranges and source hashes are **never**
interchangeable with new ones; prior mutation executions keep `94bb7bb` identity
and their own timestamps. **No semantic failure is waived.**

## Standing evidence rules

- A control that exits non-zero for a reason other than the guard under test
  proves nothing; so does a check that cannot fail. Say which you found.
- Never rename a prediction as an observation.
- Unfolding is necessary, not sufficient, for a kill.
- If you cannot establish something, record the **exact limit**, with the control
  that would have settled it. An honest gap outranks a confident inference.
- Every claim you make carries its file, line or hash. Bind expectations to a
  **span**, not to a header line.

## Deliverables

In your own `handoffs/`: `AUDIT-REPORT.md`, an evidence directory, a
`MANIFEST.sha256` with **no self-entry**, and a terminal
`COMPLETE`/`BLOCKED` event in your `STATUS.md` written with
`/code/llm-settings/shared/skills/worker-protocol/scripts/status-event`.

State plainly in your report whether the new evidence **is** compatible with the
full final candidate and its source-sensitive mandatory consumers, or **name the
specific remaining blocker**. Do not close #66, do not accept the candidate, do
not grant a merge — none of that is yours or `%503`'s to give here.
