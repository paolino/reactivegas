# BRIEF — `s2-delta`, the delta inspector for S30-2 submission 2

**Role:** commit auditor (delta scope). **Worker id:** `s2-delta`.
**Runtime root:** `/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/s2-delta`. **Report:** `/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/s2-delta/handoffs/REPORT.md`.
**Evidence:** `/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/s2-delta/evidence/`. **Instruments:** `/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/s2-delta/instruments/`.

**Load:** `auditor`, then `commit-auditor`, then `worker-protocol`,
`tmux-orchestrator`, `verification`, `invariants`. Shared skills are at
`4981cd80f4571c94d0f695e5670fd034250c700f`.

## Seat identities, independently evidenced

| | |
|---|---|
| commissioning owner (your only parent) | ticket owner `t30-contract-opus-20260906`, pane `%572`, Claude Opus 5 |
| subject author (a sibling under audit — **never contact it**) | commit owner `commit-owner-s30-2`, pane `%607`, `harness=pi provider=opencode-go model=muse-spark-1.3-contributor effort=xhigh` — verified live by `tmux display-message`, `cmd=pi` |
| your seat | Codex `gpt-6-astra`, effort `high`, pane recorded at dispatch |

Your family alternates from the author's (muse/pi). All six submission-1 seats
were Codex; that is alternation against the **author**, which is the constraint.

## Subject — frozen, and do not re-derive submission 1

| | |
|---|---|
| candidate (audit this) | `bdeba37a093c59a579c27735a41c0c560ec70f1c` |
| rejected predecessor | `b7cca4e109c213f7c344ee23ea7f654461654ea8` |
| RED | `f5259b5d30ebe49f6caecb29a4bb6191de5702fa` |
| base | `9762ad4db50f370348ea71abd44f7e969349d4b4` |
| repair delta | `b7cca4e1..bdeba37a` — **one file**, `test/VerdictSpec.hs` |
| your worktree | `/code/kelgroups-s2delta-bdeba37a` (detached, clean, yours alone) |

**You must NOT re-derive submission 1.** Six seats inspected it. Its adjudicated
result is fixed and is an input to you, not a question for you.

## Your scope — the delta plus the batch's open rows. Nothing else.

**Read the batch, not the inspectors' reports:**
`/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/handoffs/REPAIR-BATCH-S30-2.md` sha256
`649b75a622cf333d54f9fd552e4935dbfd8c2d2a8954b271225e317fe5e7e058`

### (a) B-1 / `CTRL-1` — did the repair close the CLASS, or only the instance?

The survivor was a mutant inserting `if threshold (franchiseSize view) > 1000
then Open` into the **permission** branch. It survived because
`VerdictSpec.hs:83,106` bounded generated thresholds to `0..8` or `100` and
views to `<=6`, so evaluated requirements were `<=606` and the branch lay
outside the fixtures **for every seed**.

**The property class:** *a property claiming to quantify over a domain must have
generators that actually reach it.* Decide, with executed evidence:

1. Do the repaired generators **actually reach** the domain
   `INV-S30-2-PERM-NEVER-TALLIES` claims — *every* threshold function and
   *every* view? Show the reached distribution, do not infer it from the source.
2. Does the seed mutant now **die**? Seed artifacts, **read-only, do not edit**:
   `/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/s2-insp-controls/instruments/PermissionThreshold.patch`,
   `/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/s2-insp-controls/instruments/Probe.hs`,
   `/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/s2-insp-controls/evidence/mutations.log`.
3. **Is the kill general or tuned?** A repair that widened the range just far
   enough to catch `1000` has not closed the class. Construct at least one
   **fresh** threshold-value mutant at a different magnitude and require it to
   die too. `1000` was a witness, never a policy.
4. **The pre-registered trap:** any repair leaving the permission property
   satisfiable **without** the threshold-value quantification **is** the
   degradation. Check the property still fails when quantification is removed.
5. **Coverage-gating check:** if the generators were narrowed, capped, or split
   so the hard combination stops arising, that is coverage-gating, not a passing
   property — `tabulate` still reports each axis individually hit.

### (b) rows 2–6 — the negative controls submission 1 never executed

`s2-insp-semrec3` reported these **green in the positive direction and refused
to promote them without executed negative controls**. That restraint was correct
and I am not laundering it into coverage. Close each with the **specific** missing
control named in the batch:

| row | positive evidence held | the control that is missing |
|---|---|---|
| `INV-S30-2-PARAM` | exact-set gate rows | a complete all-module negative control or permanent-test mutant |
| `INV-S30-2-ORDER` | 6300 cases green | its shipped-test **value** mutant was never executed |
| `INV-S30-2-PERM-NEVER-TALLIES` | 56700 cases green | `CTRL-1`, inherited **OPEN** — (a) above closes or fails it |
| `INV-S30-2-CURRENT-FRANCHISE` | gate rows | a stored/cached-franchise mutant was never executed |
| `INV-S30-2-SHARED-ADMIN` | 108 cases green | an independently executed same-checker negative control |

### (c) `INV-S30-2-SINGLE-SITE` v5 and gate row **G-7** — audit MY repair

I re-worded this row after `SEMREC3-1` and added G-7. **The row and the gate are
mine, and they are in your scope**: verify that G-7 can still fail, that it reads
GHC's `RecSel` classification rather than a name allowlist, and that v5 has not
weakened what the invariant was for. My own falsification is at
`/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/handoffs/evidence/G7-FALSIFICATION.txt` — treat it as a claim to check, not
as evidence you may inherit.

## What the mandate says now

`/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/handoffs/T30-S30-2-MANDATE.md` sha256
`70b48cd7121c40925fa30ac012ac0828c0c5904df0dff2f197cd1d1519ebacd7` — six invariant rows,
**all BLOCKING**, plus `REQ-VERDICT-COLL` and `REQ-VERDICT-PERM` exact-success
records. That row set is your **finite coverage denominator**. Meet it or record a
precise `BLOCKED` reason per row; budget exhaustion never turns a blocking row
advisory.

## Verification commands, at YOUR paths

```sh
cd /code/kelgroups-s2delta-bdeba37a
./gate.sh                     # frozen, read-only, mode 555, sha256 c38cbb75a50ad9dd29ebe1bdcc1f33cd18af192086336f68c3264a0f3c516abb
nix develop .#ci -c just ci   # ONE whole-project operation
```

The gate is **untracked and gitignored by design**, so no checkout provides it —
it has been placed for you and verified present, hash-matched, executable and
**not writable**. Do not substitute, repair or edit it. If it is wrong, that is a
contract block, not something you fix.

## Budget, and the authority that bounds it

- **2 reserved executions.** An execution is one whole-project operation
  (`./gate.sh` or `just ci`). Probes that run no build are free.
- **You are launch attempt 1 of 2** for submission 2. A second block stops the
  chain; there is no third seat.
- **Stopping rule:** when the denominator above is met or precisely BLOCKED, you
  stop and report. Never "until no more findings".

## Carried disclosure

Submission 1's combined semantic+REQ scenario **reduced one axis of
independence**; that is on the record and is not yours to re-litigate.

## Boundaries

You may not edit, stage, commit, push or repair the candidate; may not weaken any
row; may not author the shipped property that closes your own finding; may not
decide acceptance; may not contact the commit owner. Route everything through me.

Terminal verdict is exactly one of `AUDIT-PASS`, `AUDIT-FINDINGS`,
`AUDIT-CONTRACT-BLOCKED`, `SCOPE-FAIL`, appended to `/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/s2-delta/STATUS.md` with your
report hash. Journal `START mode=COMMIT-AUDITOR pane=<%id> cli=codex alternate=true`
first.

You are not alone in the codebase; do not revert edits made by others.

## Coverage denominator — the finite row set, with severities

| row | severity | disposition entering this audit |
|---|---|---|
| `INV-S30-2-SINGLE-SITE` | BLOCKING | v5 + G-7, mine, scope (c) |
| `INV-S30-2-PARAM` | BLOCKING | positive only; negative control owed |
| `INV-S30-2-ORDER` | BLOCKING | positive only; value mutant owed |
| `INV-S30-2-PERM-NEVER-TALLIES` | BLOCKING | `CTRL-1` inherited OPEN |
| `INV-S30-2-CURRENT-FRANCHISE` | BLOCKING | positive only; stored-franchise mutant owed |
| `INV-S30-2-SHARED-ADMIN` | BLOCKING | positive only; same-checker control owed |
| `REQ-VERDICT-COLL` | BLOCKING | exact-success record required |
| `REQ-VERDICT-PERM` | BLOCKING | exact-success record required |

## Frozen instrument and oracle hashes

| artifact | sha256 |
|---|---|
| `instruments/s30-2-oracle` | `f885af5667f9df5a2b95ed05a1d7af1c17bdf15ec51e7eef8e213eb9018beb57` |
| `instruments/verdict-site.sh` (G-7) | `dc24d3b52c33a149a265088c9e7bc10a0e7805530d06a61414955fa7908a9879` |
| `instruments/s30-1-rows.sh` | `be0975b861a406aada6ccd28ede40809016c0d3417d3f0fd76267aedc5f4652f` |
| `instruments/req-records.sh` (G-6) | `297a72593cca7247a2af89970612a2f46b4f9c0ba09e1dcc2fb551bda521ba8f` |
| `instruments/d4-exports.sh` | `3e67dec2d066dd84933e2d9d29bdcd6ae9276f69ce6d9f4e489c7e2a0d5473f0` |
| `instruments/d4-decls.sh` | `fc77097f9a82793d8258fe08425ad8f12dfbc660fb03e163bc66de73986c1d17` |

These are the gate's own instruments, read-only. You may read them; you may not
edit them. Build your own probes under `/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/s2-delta/instruments/`.

## Campaign ledger, reconciled

Authoritative ledger: `T30-S30-2-CAMPAIGN-LEDGER.md` at
`/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/handoffs/T30-S30-2-CAMPAIGN-LEDGER.md`, sha256
`0d5b8bad6b6536fe7c730e8d1e7181b7e48ffc63e8959a0f1b51ff3a697707a7`.

Submission-1 audit spend: **6 spent**, 2 remaining — and **those remaining 2 ARE
your reservation**, so nothing is uncommitted. Your allocation is 2 executions.
A dispatch snapshot and a live counter are different facts; no seat may consume
another's reservation, and there is no other live seat.

Your stopping rule is stated above: meet the denominator or record a precise
`BLOCKED` per row, then stop and report. Exhausting the budget never turns a
blocking row advisory — return the exact evidence gap instead.
