# Commit-auditor brief — S62-SIM-C1R, submission 1 of 2, FULL independent audit

**Role:** commit auditor. **Parent:** ticket owner `%313` (claude,
`claude-opus-5[1m]`, effort high). **You report only to me.**
**Family fence:** codex. Grok's allowance is consumed. No substitute family.
**Your worktree:** `/code/reactivegas-sim-fable-audit-c1r-s1` — detached at the
candidate, tree clean, **project `.lake` ABSENT**. I did not warm it; that was
my error in a previous round and it cost a note to undo. Your build must be
your own.

**You ship no code. You make no acceptance decision. You recommend; I decide.**

## Subject — the WHOLE unaccepted prefix, not the repair

```
6879970fdb1a797263843387e14704eaa1e3a2e7 .. 9717405e52664c9a520fcd0c65edb4e90612110a
```

That is the **original base** through the final candidate — inherited
implementation, source/corpus/CI integration, everything. `3590c001..9717405e`
is only the latest rebase span and is **not** your subject.

**Every former PASS row is open to falsification**, including R-GEO, R-CIT,
R-ITA, R-LAY and the nine previously unaccepted commits. **No acceptance by
inheritance.** Owner evidence and every prior audit report — including my own
verifications — are **inputs, never acceptance**. If you can falsify something I
verified, that is the most valuable thing you can return.

## Budget — reconciled before START, and it binds

| counter | submission 1 | campaign total (both submissions) |
|---|---|---|
| substantive | **5** | 10 |
| targeted | **30** | 60 |

**Counting rule.** Substantive = a full `just ci`, a full gate run, or a full
driven ui-gate suite — warm or cold, pass or fail. **A wrapper does not launder
nested calls**: a full `gate-v16` run charges **4** (its v14 body as one unit +
three nested full ui-gate suites). Full `just ci` charges **1**. Targeted = a
single probe, a single mutant, one focused command.

Your five are therefore: **one full frozen `gate-v16` (4) + one cold `just ci`
(1)**. That is the whole substantive allocation.

**You do NOT need standalone ui-gate suites.** `gate-v16` runs ordinary,
`--omit K-2` and `env RG_OMIT_NOOP=1 --omit K-2` internally and now **retains a
named log per suite** with its own `SUITE-INVOCATION` header, exit code and
sha256. Extract and inspect those. Running them again standalone is duplicate
spend. **If you run additional experiments, count them.**

The mutation campaign belongs in **targeted**: a mutant driven through a single
sub-gate (`claim`, `trace`, `vote-trace`, `scenario`, `teaching`) is one
focused command. Calibration: the previous audit killed 9 of 11 rows using 5 of
30 builds.

**If this does not fit once you have read the subject, return the concrete
over-budget command list BEFORE overrunning.** Do not deliver a weaker audit.

## The instrument

```
/tmp/reactivegas/ms2/t-simulator-fable/handoffs/gate-v16-one-membership.sh
sha256 705231918134a9a9194e22b2f8378f6b0b1476798432914a04ed48a386793556
```

Immutable. Verify the hash. Falsified by me on this exact tree in both
directions — D2 defect restored reddens the neutered-discard row; discard
disabled reddens the omission row; unmutated is green. **If you think the gate
is wrong, file a question — a previous seat did exactly that and it corrected my
gate twice.**

Predecessors retained and immutable: `gate-v15` `1b6a0011…`, `gate-v14`
`1164ae9d…`, `oracle-one-membership-v2.mjs`, `retired-surface-probe.mjs`.

## What this campaign is actually about — read this before you plan

Every expensive failure here has been **a control that reported success while
its own mechanism was broken**. Five costumes so far, in three different seats,
including mine:

- an omission control that reddened in **both** branches, so it passed against a
  harness whose discard did nothing;
- a banned-word regex double-escaped (`/\\bpledge\\b/i`) so it could never
  match the word it banned;
- a "derived" extent that was a hand-written table checked against source;
- a negative control whose **precondition** failed first, so the property it was
  named for was never reached;
- a mutation that never landed in the input the checker actually reads.

So: **for every control you rely on, show it can fail for its own stated
reason, with prerequisites still satisfied.** A control that cannot be made to
go the other way is not evidence.

## Named open limitations — verify or falsify, do not take on trust

1. **S13/S15 historical rows** (on the superseded `0c3c1e9`) are byte-identical
   by design and **no artifact datum distinguishes them**; their distinctness
   rests on owner testimony. On the **final** candidate v16's per-suite headers
   remove this. Confirm that yourself.
2. **SELF-2** — a composition-pin control may be masked by a reachability
   precondition firing before the pin-identity predicate. **Source-only, never
   executed.** No claim is made. Reaching its intended predicate would be a real
   finding.
3. **Fixture-shape exemption** in `retired-surface-probe.mjs` matches shape
   only (`/^const [A-Z][A-Z0-9_]*\s*=\s*\{"/`); it does **not** bind
   byte-verified constants. Stated, not closed.
4. **A withdrawn finding, so you do not chase it:** I claimed the claim gate
   never validates a cited line. **False.** I had mutated the core; `runGate`
   reads the HTML. Re-measured correctly it rejects both a past-EOF and a
   wrong-but-present line.

## Inputs

- packet `/tmp/reactivegas/ms2/t-simulator-fable/commit-owner-c1r-muse/PACKET-SUBMISSION-1.md` sha256 `cd8a40f81bb174f20aaf9e18f6e2219dba5893fc1c68490197e81b74821177ca`
- owner journal `/tmp/reactivegas/ms2/t-simulator-fable/commit-owner-c1r-muse/STATUS.md`, receipts `/tmp/reactivegas/ms2/t-simulator-fable/commit-owner-c1r-muse/evidence/RECEIPTS.md`,
  per-suite logs `/tmp/reactivegas/ms2/t-simulator-fable/commit-owner-c1r-muse/evidence/v16-suites/`
- my lane journal `/tmp/reactivegas/ms2/t-simulator-fable/STATUS.md`
- prior audit `../commit-auditor-s62sim-grok-s2/report.md` — **findings, not acceptance**

## Verdict and delivery

Terminal event: `AUDIT-RESULT submission=1 verdict=<pass|findings|blocked> report=<sha256>`.
Compact report: one verdict per invariant, blocking findings with exact evidence
pointers, command receipts with exit and duration. **Each finding names the
property CLASS it generalises to** — a repair that only reddens the reported
instance has not closed it; that is the single most expensive lesson of this
campaign.

**LOCAL FILES ONLY.** Write to your own `STATUS.md` and this root. **Never** send
keys, notices or prompt text into `%510` or any human composer. No issue or PR
comments, no gists, no publication.

## Write fence — a mutation audit MUST be able to write

Do not read this as "no writes whatsoever"; that would contradict the audit you
are commissioned to do.

- **Read-only reference:** `/code/reactivegas-sim-fable` — the owner's worktree.
  Never write there, never edit the candidate, never commit into it.
- **Your writable envelope:** `/code/reactivegas-sim-fable-audit-c1r-s1` — your
  own detached worktree at the candidate. **Mutate freely there.** Build, break
  things, create mutants, keep evidence.
- `gate-v16` asserts a **clean worktree**, so a mutant must be **committed** in
  your envelope before you run the gate over it — otherwise the gate reddens on
  your own dirt instead of on the property you are testing. That is precondition
  masking and it would waste a charged run.
- Your evidence root `RG_GATE_EVIDENCE` is yours to write. Point it somewhere
  under this runtime root so your per-suite logs are citable.

**No push, no PR, no merge, no publication, no remote mutation of any kind.**

Journal at every substantive phase. Declare `CAPACITY` at ~80% by the pane meter
with a resume note, or `BLOCKED` with a concrete question.

## Complete budget history, recorded before your START

Frozen reconciliation **v2**: `/tmp/reactivegas/ms2/t-simulator-fable/handoffs/AUDITOR-BUDGET-RECONCILIATION-v2-9717405e.md`
sha256 `4ad03cae1e8463ba975ec3eef6b6a8f921de45b5e2fce12c882b6c1bd651f63a`.
Its v1 predecessor is preserved and superseded, not deleted.

Owner ceiling rose **five** times — 12→18 (NOTE-078), 18→20 (NOTE-079), 20→22
(NOTE-083), 22→24 (NOTE-084), 24→28 (cost disposition) — ending at **28/28
substantive, 37/40 targeted**. One accidental full run was **counted spend, not
a raise**. Prior-campaign spend and lost receipts are separate history, never
refunded. Your own ceiling rose 8→10 substantive (NOTE-084); targeted 60 is
unchanged.

The desk authorised this audit under that complete history as a **task-specific
exception** to the generic two-raise/third-increase termination rule. It grants
no extra budget to anyone. **Do not terminate on the generic rule** — this
explicit task grant governs.

## Desk-verified identity, which I re-measured myself

| artifact | sha256 |
|---|---|
| frozen `gate-v16` | `705231918134a9a9194e22b2f8378f6b0b1476798432914a04ed48a386793556` |
| owner final gate log | `b3ec49192a092521dd99776f1969f9d7d74a3cb811aad3a82e9196a4f2086575` |
| owner final CI log | `cde34a1f620b69c367bf1b971b90395ec951d14939f2d0d0d98080435304c25f` |

All three match the desk's independent measurement and mine. They are **inputs
for you to falsify**, not acceptance.
