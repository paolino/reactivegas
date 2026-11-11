# S4-B submission 2 — FROZEN command sheet

**Frozen by the parent before the execution phase starts.** Instrument paths,
selected chain names, executable argv and charge classifications are fixed here
so no auditor reconstructs them from prose.

Base `3590c0015b84fd58004bf6fb44dd18b107304c48`. Prior `189e1ed…` is **not
accepted**; the audit subject is the **complete owned diff from that base to the
actual final candidate**. The denominator is **discovered, not frozen at 89**.

## Charge rules (unchanged layer rules)

- **substantive** = a whole build: `just lean`, `just ci`, `lake build`, under any
  wrapper (`nix develop -c …` counts as what it wraps).
- **targeted** = a single-file elaboration or driver probe (`lake env lean …`).
- **free** = reads, greps, `git` interrogation, hashing, file writes.
- **Every failed, warm and setup invocation counts.** Reserves are *within* the
  figures below, never on top.

## Instrument paths — actual, under the owner runtime

```
commit-owner-s4b-muse/instruments/S2-mut-opaque.diff          F01 specimen
commit-owner-s4b-muse/instruments/S2-mut-classifier-omit.diff R3′ omission mutant
commit-owner-s4b-muse/instruments/S2-mut-isMember-false.diff  P01 body mutant
commit-owner-s4b-muse/instruments/S2-mut-close-perm.diff      P07 body mutant
commit-owner-s4b-muse/instruments/S2-chain-P07.lean           isolated P07 chain driver
commit-owner-s4b-muse/instruments/S2-chain-P01.lean           isolated P01 chain driver
commit-owner-s4b-muse/instruments/S2-witness-close.lean       close behaviour witnesses
commit-owner-s4b-muse/handoffs/evidence/S2-*.log              receipts, one per row
```

Every mutant keeps its **raw diff and its restoration receipt**. Exact selected
statement and proof bytes are preserved before any mutation.

## Selected chains — named before execution

| row | chain |
|---|---|
| **P07** | `close_permission_to_close` `Invariants.lean:647` ← `step_close_inv` `:305` ← `close_guard_inv` `:178`. Adapter: `stepEvent` `Step.lean:147` → `step`. Intended failure: the permission-atom mutant breaks the `col.permitted` conjunct that `step_close_inv` derives and `close_guard_inv` splits |
| **P01** | `isMember_of_view_mem` `Mirrors.lean:81` (negative: unprovable under constant-false) and `view_mem_of_isMember` `:71` (positive, via `assocLookup`) |

**A first failure in the named relevant chain may establish its sensitivity; an
unrelated earlier failure, or a synthetic replacement statement, cannot.**

## Owner commands — argv and charge

| # | charge | argv | establishes |
|---|---|---|---|
| O1 | substantive | `nix develop --quiet -c just lean` (fresh `.lake`) | repaired classifier admits the real census; mandatory path green |
| O2 | substantive | `nix develop --quiet -c just lean` with `S2-mut-opaque.diff` applied | F01 specimen **discovered and rejected**; receipt-nonce path exercised |
| O3 | substantive | `nix develop --quiet -c just lean` with `S2-mut-classifier-omit.diff` | **fail-closed fires with the classifier's own unclassified-kind diagnostic**, not a Lean type error |
| O4 | substantive | `nix develop --quiet -c just lean` with `S2-mut-close-perm.diff` | P07 chain fails at its **named** target; plus the unpermitted **behaviour witness** |
| O5 | substantive | `nix develop --quiet -c just lean` with `S2-mut-isMember-false.diff` | P01 promoted rows fail at their named target |
| O6 | substantive | `nix develop --quiet -c just ci` at the final committed SHA | acceptance receipt, exit + raw log hash |
| OT1 | targeted | `lake env lean … S2-chain-P07.lean` | isolated P07 chain, no masking by earlier gates |
| OT2 | targeted | `lake env lean … S2-chain-P01.lean` | isolated P01 chain |
| OT3 | targeted | `lake env lean … S2-witness-close.lean` | authorized-close witness **and** unpermitted-close witness |
| OT4 | targeted | compiled-identity probe, census before/after | classification evidence |

**Isolation note, carried from the correction:** O4/O5 being separate whole
builds removes one masking mode but **not** the other — either can stop at an
earlier gate. **OT1/OT2 are the isolating instruments**; the whole-build runs
establish mandatory-path enforcement, the drivers establish the named chain's
own failure. Neither alone is sufficient, and neither is presented as the other.

**Owner: 6 substantive + 4 targeted.**

## Checker-dependent controls — re-run, not inherited

F01 **changes the discovery checker**, so these cannot be called unchanged-input
merely because Lean sources are unchanged:

| control | disposition |
|---|---|
| new-predicate rejection (S02 class) | **re-run** |
| missing-theorem / orphan-mirror (S03 class) | **re-run** |
| present-but-disabled checker | **re-run** |
| invocation / receipt-nonce path | **re-run** |

**Process sharing is allowed only where each claimed diagnostic is actually
reached. Early exit is not evidence of later checks.** Where a shared run cannot
reach a later diagnostic, it gets its own invocation or the row is reported OPEN.

## Retained evidence

Retained evidence requires **identity of the actual relevant inputs**, including
**checker and dependency footprints**. **Unchanged text of one theorem does not
prove unchanged inputs.** If table or promotion edits change relevant
dependencies, affected evidence is **re-established** or the **exact cost gap is
returned before proceeding**.

## Budget — additional operations, not a reset

| party | historical retained | newly authorized | resulting ceiling |
|---|---|---|---|
| **owner** | 8 substantive / 42 targeted | +6 substantive, +4 targeted | **14 substantive / 60 targeted** (targeted **not** raised to 64) |
| **auditor campaign** | 6 substantive / 59 targeted | +9 substantive, +10 targeted | **15 substantive / 69 targeted** |
| **parent** | unchanged | **zero** | no parent execution grant |

Owner substantive **8→14 is the second owner raise**; **6→8 remains in history**.
Auditor invalid/preflight and setup history is retained exactly as recorded.
**Submission 2 of 2 — no third.** **Zero automatic further raises.** If the exact
sheet cannot fit, **return the specific gap before executing that phase; do not
reduce coverage to fit.**

## Fence

`scripts/check-lean-mirrors` and the already-owned mirror files
(`lean/Reactivegas/Mirrors.lean`, `lean/KelGroups/Mirrors.lean`) for **necessary
bindings and classification only**.

**Forbidden:** any existing model/guard/theorem statement change; a new duplicate
P07 theorem; a `justfile` recipe change; **any `Invariants.lean` edit**; S2R/S3
expansion; `docs/en/design/` writes.

The **full original owned diff remains the audit subject** even where repair
edits are restricted. If a required binding genuinely cannot be implemented
inside this fence, **return its exact dependency rather than silently dropping
the obligation**.

## Base movement

If the accepted base moves before final evidence, **bind the actual base**,
account for incoming changes, and report any revalidation gap. **Prior evidence
does not silently transfer across an integration.**

---

## CORRECTION 2026-09-06 — the instrument paths were PLANNED, not frozen inputs

**A correction to this sheet, not a new operator gate.** The S4 repair grant
**remains in force**, and static/code preparation within the fence continues.

What this sheet got wrong: it listed
`instruments/S2-chain-P07.lean`, `S2-chain-P01.lean`, `S2-witness-close.lean`
and the mutant diffs as *actual instrument paths*, and wrote
`lake env lean … S2-chain-P07.lean` as though it were executable argv. At the
desk's inspection the `instruments/` directory **did not exist**; at mine it
exists but is **empty**. Either way **these are planned paths, not existing
frozen inputs**, and OT4 was described only as a "compiled-identity probe" —
a description, not a command.

**This sheet does not yet bind executable controls, and must not be cited as
doing so.**

### The binding sequence, corrected

1. **Parent freezes the command *contract*** — rows, chains, charge classes,
   isolation rules, fence. That part of this sheet stands.
2. **The authorized implementer prepares the required instrument bytes.**
3. **The parent then verifies and binds them** — actual driver bytes and
   **hashes**, exact commands, **current-directory and search-path setup**, and
   **each imported mutated dependency** together with its selected chain — before
   any affected run.

**Before the FIRST affected execution phase**, that concretization must be
complete. **No imaginary file is frozen, and no auditor is asked to reconstruct
one later.**

### Counting

**Base/positive and mutant/negative invocations are counted separately when they
really are separate.** A single driver may exercise several cases **only when all
of them are reached and observed** — the same rule already stated for process
sharing.

### Allowances

Unchanged: owner **+6 substantive / +4 targeted**, auditor **+9 / +10**. An
**exact fit or gap must be stated before the phase**. Owner code preparation is
not stopped while instruments are being prepared, but **no unbound expensive run
is authorized by calling its path frozen**.

### Receipt discipline — a correction to my own reporting

**No source-level inspection and no pane behaviour substitutes for the owner's
own post-grant `RESUMED`/`NOTE` receipt carrying the admitted hashes and
counters.** I previously reported NOTE-011 delivery as "confirmed by application
behaviour" from a pane capture. That was not a receipt.

The receipt has since landed and **is** the evidence: the owner recorded
`RESUMED (NOTE-011 submission-2 AUTHORIZED)` with seat identity verified
unchanged, the admitted packet **hash-verified file-by-file (MANIFEST all-OK)**
naming v2 `2214ff8a…`, v3.1, v3-superseded, the frozen sheet, the grant and the
S4 contract, the audit report read in full, and v3.1 operative for P01/P07 with
the v3 new-theorem proposal withdrawn as false-absence-based.
