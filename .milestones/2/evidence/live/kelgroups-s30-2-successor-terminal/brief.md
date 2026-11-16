# BRIEF — `s2-succ`, the single seat of the S30-2B evidence-successor campaign

**Role:** commit auditor (evidence campaign). **Worker id:** `s2-succ`. **Runtime root:** `/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/s2-succ`.
**Report:** `/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/s2-succ/handoffs/REPORT.md`. **Findings:** `/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/s2-succ/handoffs/findings.jsonl`.
**Evidence:** `/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/s2-succ/evidence/`. **Your instruments:** `/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/s2-succ/instruments/`.

**Load:** `auditor`, then `commit-auditor`, then `worker-protocol`, `tmux-orchestrator`,
`verification`, `invariants`. Shared skills at `4981cd80f4571c94d0f695e5670fd034250c700f`.

**You are the only seat of this campaign, launch attempt 1 of 1. There is no second launch and no
retry.** A contract block or a command failure is **terminal**: return the exact terminal state
with preserved evidence.

## Canonical counters — identical to the frozen snapshot, and the only figures that bind

```
CURRENT-AUDIT-COUNTER: spent=0 ceiling=4 committed=4 uncommitted=0
CURRENT-LAUNCH-ATTEMPTS: used=0 ceiling=1
```

**Take spend, reservation and attempt authority from this block and from nowhere else.** No prose
figure anywhere in this packet is authoritative — a first textual match is exactly what produced
`reservation=2` against a brief allocating 4 in the predecessor campaign, and that seat was right
to block on it.

**These counters are prospective.** At launch you append the actual `START` to your **live
journal** and verify it separately: post-cursor `START`, actual pane, model and argv, and actual
attempt **1/1**, all **before** any candidate work. The frozen snapshot is **not** to be mutated to
make its prospective counter look current, and neither is this brief.

## Why this campaign exists

The predecessor seat `s2-delta2` blocked at **0 of 4 executions** on an inconsistent authority
packet — **mine**. It performed **zero** candidate and zero semantic inspection, which is why your
Codex family reuse is explicitly allowed: nothing about the candidate was seen. The old campaign
stays closed at audit **8/12**, attempts **2/2**, four executions returned, all six rows BLOCKED.
**Nothing was refunded, reset, waived or retroactively accepted, and none of it is candidate
evidence.**

## Authorization snapshot

`/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/handoffs/T30-S30-2B-SNAPSHOT.md` sha256 `09dcbf79f3cc22a152a364fb989c32bddc9dec3da5170f8dcfdfc74479311c5f` — frozen, immutable, carrying the row ledger,
historical spend, the four-command schedule and every instrument hash.

## Seat identities, independently evidenced

| | |
|---|---|
| commissioning owner (your only parent) | ticket owner `t30-contract-opus-20260906`, pane `%572`, Claude Opus 5 |
| subject author (sibling under audit — **never contact it**) | `commit-owner-s30-2`, pane `%607`, `harness=pi provider=opencode-go model=muse-spark-1.3-contributor effort=xhigh`, verified live by `tmux display-message`, `cmd=pi` |
| your seat | Codex `gpt-6-astra`, effort `high`, fresh conversation and context, pane recorded at dispatch |

## Subject — frozen, byte-identical, write-idle, zero submissions

| | |
|---|---|
| candidate | `bdeba37a093c59a579c27735a41c0c560ec70f1c` |
| tree | `2988c76009832c1a71b4cda47ce2a14a51d0c53b` — verify at start **and** at the end |
| your worktree | `/code/kelgroups-s2s-bdeba37a` (fresh, detached, clean, yours alone) |
| mandate | `/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/handoffs/T30-S30-2-MANDATE.md` sha256 `70b48cd7121c40925fa30ac012ac0828c0c5904df0dff2f197cd1d1519ebacd7` |
| gate at your path | `/code/kelgroups-s2s-bdeba37a/gate.sh` sha256 `c38cbb75a50ad9dd29ebe1bdcc1f33cd18af192086336f68c3264a0f3c516abb`, mode 555, verified not writable |

**No candidate byte may change.** Mutants are applied, observed and **reverted** inside your own
worktree; the tree hash must still hold when you finish. **No candidate edit, no repair, no
submission** — implementation submission authority is exhausted.

## THE EXACT EXECUTABLE SCOPE — four executions, in this order, no fifth

| # | execution | atom(s) |
|---|---|---|
| 1 | your own `./gate.sh` | gate rows, G-7 |
| 2 | one **`ORDER`** mutant | `INV-S30-2-ORDER` |
| 3 | one **stored/cached-snapshot** mutant **inside `verdictOf`** | `INV-S30-2-CURRENT-FRANCHISE` |
| 4 | one **combined** round: `PERM-NEVER-TALLIES`/`CTRL-1` **+** `SHARED-ADMIN` | disjoint sites, distinct named checks |

`./gate.sh` is the **only** whole-project command in execution 1. Its **G-2 already runs
`just ci`**, so running `just ci` separately duplicates it. **Never both** — that duplication
consumed an entire earlier budget.

```sh
cd /code/kelgroups-s2s-bdeba37a
./gate.sh
```

**No hidden invocations, no wrapper concealing a fifth command, no merged rounds.** If a fifth
execution becomes necessary, **stop and report the exact terminal state**.

## Two technical rulings you must not lose

**1. A direct `franchiseSize` mutant is INADMISSIBLE for `CURRENT-FRANCHISE`.** The property at
`test/VerdictSpec.hs:276` derives its own oracle **through `franchiseSize`**:

```haskell
let required = runThreshold shape (franchiseSize view)
```

so mutating it moves **implementation and oracle together** and the test **cannot fail**. That is a
self-cancelling mutant, not evidence. **The required mutant plants a stored/cached franchise
snapshot inside `verdictOf`**: production reads stale, the test reads fresh.

**2. `ORDER` stays separate from the stored-snapshot mutant.** Both alter `verdictOf`; a combined
result would attribute the kill to neither. Executions 2 and 3 are **not** to be merged.

## `PARAM` — zero executions, and name the claim correctly

Re-established by a **negative control on the gate's export rows** against retained or synthetic
interface data — the method used for G-7 v5.2. Retained real dumps:
`/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/s2-insp-semrec3/evidence/State.iface`, `/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/handoffs/evidence/Types-bdeba37a.iface`.

**Report it ONLY as instrument evidence, never as a shipped-property mutation kill.** Those are
different claims and a reader without that sentence will take it for product-level coverage.

## `CTRL-1` remains blocking until its quantified-domain controls execute

The survivor inserted `if threshold (franchiseSize view) > 1000 then Open` into the **permission**
branch and lived because `VerdictSpec.hs:83,106` bounded thresholds to `0..8` or `100` and views
to `<=6`, putting the branch outside the fixtures **for every seed**. A fixture added at the
surviving value reddens the instance and **does not close the row**. In execution 4 establish:

1. the repaired generators **actually reach** the quantified domain — every threshold function,
   every view. Show the reached distribution; do not infer it from source;
2. the seed mutant **dies**. Read-only seeds: `/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/s2-insp-controls/instruments/PermissionThreshold.patch`,
   `/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/s2-insp-controls/instruments/Probe.hs`, `/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/s2-insp-controls/evidence/mutations.log`;
3. the kill is **general, not tuned** — use a **fresh magnitude** different from `1000`, which was
   a witness and never a policy;
4. **the pre-registered trap:** any repair leaving the permission property satisfiable **without**
   the threshold-value quantification **is** the degradation;
5. **coverage-gating:** generators narrowed or split so the hard combination stops arising is not a
   passing property — `tabulate` still reports each axis individually hit.

## Reporting — every active row individually

For **each** active row report **meaning, reach, discrimination, coverage and provenance**
separately, with the executed evidence, which execution produced it, and what it establishes
versus what it cannot.

`REQ-VERDICT-COLL` and `REQ-VERDICT-PERM` may be **carried from the exact inherited killed
evidence, labelled inherited** — exact records in both fresh builds, 12/12 malformed-record
variants rejected by the frozen G-6 checker, produced by `s2-delta`. **They are not new
executions and you must not re-execute them.**

A row with no executed evidence is `BLOCKED` with a precise reason. **Budget exhaustion never
turns a blocking row advisory**, and no row may be weakened or narrowed to fit.

## G-7 must retain four independently established properties — verify each

1. the primed identifier `verdictOf'` is **seen and rejected** as a second decision site;
2. a permissive binding census versus classified count **disagreement REFUSES**;
3. the refusal branch has a **can-fail control**;
4. the unchanged candidate passes **only if every binding-shaped line is accounted for**.

My falsification is `/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/handoffs/evidence/G7-v5.2-FALSIFICATION.txt`
(`ec1b37cf555215ccd6c72b17c2c2158a61f4644e3de6ea4166f276497f6287c9`) — **a claim to check, not
evidence to inherit.** v5.1 produced a false positive on `KelGroups.Vote.Types`; v5.2 fixed it.
That history is in the file and you should confirm the fix rather than assume it.

## Boundaries and terminal conditions

You may not edit, stage, commit, push or repair the candidate; may not weaken or narrow a row; may
not author the shipped property that closes your own finding; may not decide acceptance; may not
contact the commit owner. Route everything through me.

**A candidate finding returns to me for the desk** — implementation submission authority is
exhausted, so it is recorded and returned, never repaired. A complete PASS returns for **one**
ticket-owner adjudication over the full historical union and **authorizes no push, PR, merge,
closure or release**.

Terminal verdict is exactly one of `AUDIT-PASS`, `AUDIT-FINDINGS`, `AUDIT-CONTRACT-BLOCKED`,
`SCOPE-FAIL`, appended to `/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/s2-succ/STATUS.md` with your report hash and per-row attribution. Journal
`START mode=COMMIT-AUDITOR pane=<%id> cli=codex alternate=true` first.

You are not alone in the codebase; do not revert edits made by others.

## Base, row set and instrument hashes — bound here, not only by reference

**base** `9762ad4db50f370348ea71abd44f7e969349d4b4`; RED `f5259b5d30ebe49f6caecb29a4bb6191de5702fa`;
rejected predecessor `b7cca4e109c213f7c344ee23ea7f654461654ea8`.

### Coverage denominator — the finite row set with severities

| row | severity | entering state |
|---|---|---|
| `INV-S30-2-SINGLE-SITE` | BLOCKING | BLOCKED — execution 1 |
| `INV-S30-2-PARAM` | BLOCKING | BLOCKED — zero-execution instrument control |
| `INV-S30-2-ORDER` | BLOCKING | BLOCKED — execution 2 |
| `INV-S30-2-PERM-NEVER-TALLIES` | BLOCKING | BLOCKED — execution 4, carries `CTRL-1` OPEN |
| `INV-S30-2-CURRENT-FRANCHISE` | BLOCKING | BLOCKED — execution 3 |
| `INV-S30-2-SHARED-ADMIN` | BLOCKING | BLOCKED — execution 4 |
| `REQ-VERDICT-COLL` | BLOCKING | INHERITED PASS/KILLED — carried, not re-executed |
| `REQ-VERDICT-PERM` | BLOCKING | INHERITED PASS/KILLED — carried, not re-executed |

### Frozen instrument and oracle hashes

| artifact | sha256 |
|---|---|
| `instruments/s30-2-oracle` | `f885af5667f9df5a2b95ed05a1d7af1c17bdf15ec51e7eef8e213eb9018beb57` |
| `instruments/verdict-site.sh` (G-7 v5.2) | `4de71457548383f7046bea61a5f107144e619af1aca3978f20f8716d14847a6e` |
| `instruments/s30-1-rows.sh` | `be0975b861a406aada6ccd28ede40809016c0d3417d3f0fd76267aedc5f4652f` |
| `instruments/req-records.sh` | `297a72593cca7247a2af89970612a2f46b4f9c0ba09e1dcc2fb551bda521ba8f` |
| `instruments/d4-exports.sh` | `3e67dec2d066dd84933e2d9d29bdcd6ae9276f69ce6d9f4e489c7e2a0d5473f0` |
| `instruments/d4-decls.sh` | `fc77097f9a82793d8258fe08425ad8f12dfbc660fb03e163bc66de73986c1d17` |

Read them; do not edit them. Build your own probes under `/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/s2-succ/instruments/`.

## Stopping rule, re-derivation, and the disclosed independence cost

**Stopping rule:** meet the denominator above or record a precise `BLOCKED` per row, then **you
stop and report**. Never "until no more findings". If the four executions cannot decide the rows,
return the exact gap in the same terminal report — do not overrun.

**Do not re-derive** submission 1, the predecessor campaign, or the two inherited REQ rows. Six
seats and two campaigns preceded you; their adjudicated results are **inputs**, not questions. Your
executions are for the five unverified rows and `CTRL-1`.

**Disclosed independence cost, carried and not yours to re-litigate:** submission 1's combined
semantic+REQ scenario **reduced one axis of independence**, and the two REQ rows you inherit come
from that scenario. Record it as a stated limit on the inherited evidence.
