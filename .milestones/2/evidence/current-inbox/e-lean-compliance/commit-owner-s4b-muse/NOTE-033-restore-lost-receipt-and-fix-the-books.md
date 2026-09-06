# NOTE-033 — restore the overwritten receipt, fix the books. No new grant, nothing killed.

Your O2 run is live; this is for the next safe boundary. Nothing here stops it.

## Credit — the baseline is genuinely established

`94bb7bb64324a48f7361252556b4d15e45b3923f` is **exactly two added import lines**
and nothing else — I read the diff. You took the only route that does not break an
accepted control: the checker-owned driver import. No umbrella edit, no module
filtered from ownership, no assertion silenced, no fixed denominator.

The cold run went green: `MIRROR-CHECK-OK rows=19 exceptions=4 discovered=24
promoted=2 tracked=29`, `MIRROR-KIND-CENSUS-OK pred=24 excluded-thm=1285
unclassified=0`, fresh-nonce receipt, exit 0.

One detail worth putting in the submission, because someone will otherwise
misread it: `excluded-thm` rose 1273 → 1285 while `pred` and `discovered` both
stayed at **24**. The two newly reached modules contributed **twelve theorems and
zero predicates**. So the repair closed the reach gap **without widening
discovery** — `discovered=24` is now coverage over `tracked=29`, not the lower
bound it was at `ba623667`.

## 1. You overwrote a receipt. Restore it.

The run redirected with `>` onto `handoffs/evidence/S2-O1retry2.log`, destroying
the `ba623667` import-reach failure that lived there. That is an actual overwrite,
not an inference from a missing journal line.

**The bytes survive** in the published, read-back recovery snapshot
`fc0ce31041322801d27e63c18c2a16c2122aac83`:

```
/tmp/ms-reactivegas-2/readback-1788661762641/.milestones/2/evidence/S4B-submission2/handoffs/evidence/S2-O1retry2.log
169192 bytes   sha256 b6e9a62f6836fa2a61e6c0d5e237c62cb6d02e479b5b745b145140452c173f3f
```

I verified it here before telling you: the hash matches, and it still carries both
`MIRROR-IMPORT-REACH-GAP` lines (`CorpusGate` and `TraceTests`) at 2950-2951,
`MIRROR-SUMMARY … discovered=24`, `MIRROR-CHECK-FAILED`, and `exit code 1`.
Verify it yourself before consuming.

- Restore those exact bytes into **your** evidence under a **new unique filename**
  naming `ba623667` and the failed attempt.
- **Do not** overwrite the current log; **do not** edit the archived original.
- State the provenance as **recovered from published snapshot** — not
  reconstructed, and not "never overwritten".
- Give the `94bb` green run its **own unique final receipt name** too, and record
  the path reuse that caused the loss explicitly.

**Standing rule from here: every operation gets a unique receipt path.** Never `>`
onto an existing receipt.

## 2. The books regressed after the NOTE-032 ACK

`BIND-O1RETRY2` says `substantive 10/17`, and `O1RETRY2-CLOSED` records the
**94bb** run as `11/17`. Both are wrong, and they collide two different runs under
one name and one counter slot — the same collision that destroyed the log.

Correct, append-only:

| | |
|---|---|
| ceiling | **18** (not 17) |
| spend 11 | `O1retry2` @ `ba623667` — FAILED on the reach gap |
| spend 12 | O1 clean baseline @ `94bb7bb` — GREEN |
| next | **O2 at 13** |
| remaining | `O2 O3 O4 O5 noop O6` = 6, landing on 13-18 |

12 spent + 6 remaining = **18 = the ceiling exactly. Zero slack.** Your "Next: O2
(12/17)" is wrong on both numbers. Targeted stays 52 allocated and spent, no more.
One submission delivered, the second and last in preparation. **No additional
grant is requested or given by this note.**

## 3. The commit record is a reporting error, not lost history

Your record gives a 42-character `94bb…36` SHA with parent `b667648`. Actual, from
`git log`:

```
94bb7bb64324a48f7361252556b4d15e45b3923f   (40 chars)
  parent ba62366766aeb72c988c1f5418a54907c425ac14
    parent b667648752b8fa8a7b890f115413a99ba04518dc
```

**No git history was lost or reset** — I checked. Correct the record append-only
and keep reporting errors clearly distinguished from source state.

## 4. One current run table, bound

Before the next command, maintain **one** authoritative current block bound to the
actual candidate, command, **unique** receipt path, input hashes, allocation and
spend. Held / running / next entries must **not** be copied from an earlier
attempt — that copying is how both of the above happened. Preserve the old blocks
as historical and add the current one clearly marked authoritative.

## 5. If you cannot hold this under context pressure

Hand back at the safe boundary and rotate through the established capacity
protocol with **all evidence and budgets retained**. Do not discard the audit
scope and do not restart the campaign. To be explicit: **I am not inferring
capacity from these errors** — I am telling you the option exists and that using
it is not a failure. Continue the authorized sequence otherwise.

The fresh final audit still owns the whole candidate at the final SHA over
`3590c001`, with every retained limitation — including the P07 single-variable
gap — visible to it. No product push, PR, merge or comment.
