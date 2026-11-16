# BRIEF — fresh independent audit, S2R submission 2 (#66)

You are a fresh independent candidate auditor. New seat, new runtime, new
detached checkout. **You inherit nothing.** A prior audit exists and is given to
you as an **input**, never as a PASS: no row it closed is immune to your
falsification, and you may reject any row you judge unestablished.

Load the shared `auditor` role plus the `lean-auditor` and `commit-auditor`
paths.

## Subject

- **Candidate: `ab617d88af9d080de71218f3cc553d60ef0b6de0`** — submission 2.
- Its parent `714cb2a8536b24bf735295137e8f907782228380` — submission 1.
- Accepted base: `d67032313acf3699cc50358a057391b88d002192` (PR #87 exporter).
- Draft PR #88, head now `ab617d8`. **Do not touch it.**
- Your worktree: `/code/reactivegas-66-s2r-audit2`, detached at `ab617d8`,
  verified clean with **zero** olean files at your START. It is yours alone.

The subject is the **full original candidate against `d670323`, plus the
correction** — not the correction alone.

## Frozen authority — `admitted/`, with `MANIFEST.sha256`

| document | sha256 |
|---|---|
| `SUBMISSION-2.md` | `ce4d84eddd73cdf9f3289bf71c58a71a3c7bed8d227e581b9a6793f580a116c5` |
| `SUBMISSION-1-ADMITTED.md` | `32299d25cae31c9dcd8a6d6737e61bb6fe35b48bd4a8e6f041949cff13319bff` |
| `PRIOR-AUDIT-REPORT.md` | `d634df52c51d4351699d36927b5b0c662357a4ac08a7d689a6708db2d34def90` |
| `PRIOR-AUDIT-FINAL.sha256` | `64dbd5ad2300dec49c4ecc71b703ce86031b673782e363fc9f2f115df536892e` |
| `NOTE-050-…` | the commissioning note |

The prior auditor's complete runtime — journal, 38 receipts, evidence, ledgers,
instruments — is at
`/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s2r-final-codex/`.
Read it freely. Its verdict was **AUDIT-FINDINGS**, not a pass.

## Why submission 2 exists

The prior audit's one open finding: `scripts/check-lean-axioms` claimed
`Fail-closed: missing/unresolvable REACTIVEGAS_ROOT`, while the driver reads
`(← IO.getEnv "REACTIVEGAS_ROOT").getD "."` — a **missing** variable silently
defaults. P22 got exit 0 with `axiom-gate: ok`. The mandatory wrapper exports the
root, so it was never a mandatory-path bypass.

Submission 2 is authorised as **a comment correction only**. The author claims
one file, `+14/−5`, every changed line a `#` comment, `getD` and the wrapper
export untouched.

## What you must establish

1. **The comment-only equivalence proof, independently.** Is every changed byte
   between `714cb2a` and `ab617d8` inside a comment? Is every other tracked byte
   identical? Do **not** accept the author's proof or mine — derive it. If any
   executable or input byte moved beyond the authorised comment, **stop and
   report the exact gap**.
2. **Whether the corrected text is now true**, and whether it over- or
   under-claims. It distinguishes an inspected branch from an executed one; check
   that distinction is honest. A comment that fixes one false claim by asserting
   a second unevidenced one is a finding.
3. **The full inherited row set at this candidate.** A1–A8 (A2 as A2′), B1–B6,
   C, §5, AMENDMENT-1, F-001–F-004, G-001, the five successor elements, and
   accepted-exporter integration. Prior evidence is an input; say for each row
   whether you closed it by **new execution**, by **unchanged-input evidence with
   explicit byte identity**, or by **inspection**. Those three are not
   interchangeable and must be labelled per conclusion.
4. **Prior audit artifact integrity and applicability** — do its manifests
   verify, and does its evidence still apply to a candidate whose bytes moved?

## Budget — reconciled before your START

**2 substantive, 4 targeted.** A substantive invocation is a whole build
(`just ci`, `just lean`, `lake build`) under any wrapper, `nix develop -c`
included. Targeted is a single-file elaboration or driver probe. **Failed and
warm calls count.** Reads, greps, `git` interrogation, hashing and file writes
are **free** — the byte-identity and manifest work above costs you nothing.

| # | invocation | purpose |
|---|---|---|
| S1 | **required** — project-output-cold full `just ci` on `ab617d8` | acceptance receipt at the actual final SHA |
| S2 | **bounded reserve** — not an obligation to spend | repair or a second attempt |
| T1 | direct missing-root observation on the **actual generated driver**, if needed to settle the corrected claim | the corrected text's own subject |
| T2–T4 | reserve | |

This fits with margin. **Reconcile the identities yourself before START**; if
your set exceeds 2/4, return the exact gap **before** overrunning.

**This is a fresh full-candidate review plus a verification supplement to a
retained campaign. It is NOT a claim that the prior 38 invocations were
re-executed here, and you must not present it as one.** Do not relabel it a full
fresh mutation replay.

## Provenance and delivery

Record at your START from live inspection: your own PID, full live argv (model
and effort are both argv-pinned — verify), cwd, `git rev-parse HEAD`, olean
count, wall clock. Your START must postdate this brief.

Journal append-only to `STATUS.md`. Every invocation gets a class, purpose,
expectation and a receipt with what was actually observed.

Deliver `handoffs/AUDIT-REPORT.md`: one verdict per row — CLOSED / OPEN / PARTLY
— each with the command that establishes it and the observation, and each
labelled new-execution / unchanged-input / inspection.

**Local files only.** Do not type, paste or send keys into any other pane or any
human composer. No push, PR action, issue or review comment, gist, publication,
deployment, candidate edit, or merge. Do not contact the prior auditor's seat.

Report what you find, including that the candidate is sound if it is. A row
closed by someone else's assertion is not closed.
