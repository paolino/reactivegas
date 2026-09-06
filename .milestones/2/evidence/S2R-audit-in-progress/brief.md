# BRIEF — fresh FULL independent candidate audit, S2R (#66)

You are an independent candidate auditor. You did not write this code and you
inherit nothing. No prior candidate result and no prior auditor PASS carries
into this audit. Three earlier candidates on this row (`5745a2c`, `561347d`,
`b0c2cdb`) were REJECTED; a fourth (`fa01779`-era S2) was superseded. Their
reports are not evidence for this one.

Load the shared `auditor` role and the `lean-auditor` and `commit-auditor`
paths. This is an issue-backed completed implementation submission, so commit
provenance, diff scope, submission caps and build budgets are in scope
alongside the Lean and gate semantics.

## Subject

- Candidate: `714cb2a8536b24bf735295137e8f907782228380`
- Parent / accepted base: `d67032313acf3699cc50358a057391b88d002192` (PR #87)
- Chain below that: accepted `4a6cd87` (PR #79, S1)
- Draft PR: https://github.com/paolino/reactivegas/pull/88 (DRAFT, not accepted)
- Your worktree: `/code/reactivegas-66-s2r-audit`, detached at `714cb2a`,
  verified clean with **zero** `.olean` files at your START. It is yours; the
  implementation owner works in a different worktree and must not be disturbed.
- Owner packet: `/tmp/reactivegas/ms2/e-lean-compliance/commit-owner-s2r-muse/handoffs/SUBMISSION.md`
  (sha256 at admission `601e52abaff025160c2027786d6bae6da81170929213aef8f7c9cef364b8f96d`;
  the owner is amending §8 and §9 under NOTE-008 — read whatever is on disk and
  record its actual hash yourself, both versions are citable).
- Owner evidence: `.../commit-owner-s2r-muse/handoffs/evidence/`.
  Two named bindings you should recompute: `17-final-ci.log`
  `e467941ff3d06095b7aff90d1e6e62692d3af7ed094dbd88e11b2883048a6f5a`,
  `probe-35-tzero-isolated.log`
  `60281fbb68123fba10abba0d357435a0dc9d5fbb64b9ffe1e22ff7422aa9a063`.

## Scope

The FULL original S2R mandate — every A, B and C row — **plus** the integration
onto the accepted exporter base. Not just the five new elements.

The accepted exporter (`d670323`) is an **integration input**. You are NOT asked
to repeat PR #86/#87's separate mutation campaign. But the ownership policy, the
compiled extent and the source/build inventory at THIS candidate must account
for it: the exporter added `Reactivegas.CorpusExport` (S 26→27), a `lean_exe`
registration, a `justfile` verify recipe and a CI step. If the gate's extent,
its provenance filter or its CI reachability is wrong about those, that is a
finding here.

### Rows to re-derive independently

`A1` S/B/T derivation and reconciliation · `A2′` quota absent plus its eight
constraints · `A3` cold provenance · `A4` non-zero S, B and T · `A5` added
module, truncated inventory, removed module, `sorry` · `A6` axiom policy and
using-shape, including the transitivity argument · `A7` mandatory path · `A8`
totality and PANIC handling · `Row B` nine renames · `Row B4` three dead
re-exports · `Row C` doc path · `§5` driver rule · `AMENDMENT-1`
quota-blindness · `F-001` prefix hole · `F-002` T truncation · `F-003`
import-Lean closure · `F-004` lexical vs resolved · `G-001` empty/unset guard.

## The two things this audit exists to settle

**1. The omission controls are NOT closed.** The owner's `S \ B` (P26) and
`B \ S` (P27) logs each carry, beside the intended omission finding, an extra
axiom-policy finding from a stale poisoned `CorpusGate` olean. The branches
demonstrably executed and named the right identities — but the aggregate
non-zero exit is not an unmasked necessity test, because that extra failure
would persist with the omission check disabled. The later rebuild does not
retroactively clean the earlier inputs or outputs, and the owner has explicitly
disclaimed clean single-failure provenance.

You must execute BOTH controls on YOUR OWN clean artifacts and require the
intended identity with **no unrelated axiom or policy finding**. A failure
there is a finding. Running out of budget before establishing it is also a
finding — report the gap. Neither is closed by owner acceptance or by the
owner's word.

**2. Checks that cannot fail, and checks that fail for the wrong reason.** This
row exists because three declarations named `i57TrustNoSorry`,
`kelGroupsHasNoReactivegasImport` and `leanToolchainMatchesPin` policed nothing
— each was a conjunction of unrelated true facts. Apply the same test to the new
gate: for every guard that is claimed to protect something, ask whether an input
exists that fires that guard **and nothing else**. Where the owner's control is
non-zero for a reason other than the guard under test, say so and do not accept
it. Where a guard cannot be reached at all, say whether that is a construction
proof or an untested branch.

Do not accept an exit status as evidence of attribution.

## Command budget — reconciled by the parent BEFORE your START

Cap: **8 substantive, 30 targeted.** A substantive invocation is a whole-build:
`just lean`, `just ci`, `lake build`, in any wrapper (`nix develop -c ...`
counts as the command it wraps). A targeted invocation is a single-file
elaboration or a driver probe. **Failed and warm-cache calls count.** Reads,
greps, `git` interrogation, hashing, version queries and file writes are free.

The parent reconciled the required set against the cap before dispatching you.
It fits, with margin; you are not being sent in short.

Substantive, 7 named + 1 reserve = 8:

| # | run | closes |
|---|---|---|
| S1 | cold `just lean` on the empty `.lake` you start with | A1, A3, F-004 baseline, identities |
| S2 | full `just ci` | A7, exporter integration, remote-parity |
| S3 | `sorry` injected into an existing theorem | A5 sorry, AMENDMENT-1 |
| S4 | forbidden axiom declared and used | A6 |
| S5 | new module registered outside the root, built into the project dir | A5 added-module, F-001, F-003 |
| S6 | axiom gate removed or neutered from the mandatory path | necessity: does the gate do the killing |
| S7 | `just lean` through a symlinked/aliased invocation | A7 + F-004 equivalent path |
| S8 | RESERVE — repair, or a second attempt at any of the above |

Targeted, 17 named + 13 reserve = 30:

`S \ B` clean · `B \ S` clean · S-zero · B-zero · T-zero isolated ·
T walk-skip · T fold-skip · `LEAN_PATH` empty · `LEAN_PATH` unset ·
vendor-first loader form · relative-entry loader form · extent census with
identity readback · Row B presence/absence · `Std.*` exclusion · three driver
preflight/setup slots.

If your reconciled set exceeds 8/30, return the **exact** gap before you
overrun. Do not overrun and report afterwards.

## Provenance and delivery

Record at your START, in your own words and from live inspection, not from this
brief: your own PID, your full live argv (model and reasoning effort are both
pinned there — verify it), your cwd, your `git rev-parse HEAD`, your olean count,
and the wall-clock time. That START must be after this brief was written.

Journal append-only to `STATUS.md` in your runtime root. Every invocation gets a
class, a purpose, an expectation and a receipt with what was actually observed.

Deliver `handoffs/AUDIT-REPORT.md` with one integrated verdict per row —
CLOSED / OPEN / PARTLY — each carrying the command that establishes it and the
observation, not a summary. A row you closed on inspection alone says so.

**Delivery is LOCAL FILES ONLY.** Write into this runtime. Do not type, paste,
send keys or otherwise write into any other pane. Do not push, open or edit a
PR, comment on GitHub, publish, or merge. Do not repair the candidate; you audit
it. If you believe a repair is needed, that is a finding.

Report what you found, including that the candidate is sound if it is. A finding
restated without new evidence is not a finding, and a row closed by the owner's
assertion is not closed.
