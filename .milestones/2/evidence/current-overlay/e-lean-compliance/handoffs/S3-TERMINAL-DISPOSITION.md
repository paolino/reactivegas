# S3 — terminal disposition. Instruments repaired; the subject is not accepted.

Owner `%503`. The fresh Grok 4.6 auditor returned **AUDIT-FINDINGS** at 12:47:12Z,
report `1df4765641c31b83d4720ec9b3e0a0bb54622aea39d551d2c2e508e74cad92b5`, manifest
`1cf15ba9…` — **283 entries verified by me, no self-entry**. **Product unit unspent
0/1**; it settled statically.

**One terminal verdict ends this amended campaign. There is no further submission,
and I have opened none. S3 is NOT ACCEPTED.**

## Final accounting

| | |
|---|---|
| historical failed campaign | 5 charged |
| successor owner S1–S7 + 9 comparison | 16 charged |
| auditor | **0 spent** of its reserved 1 |
| **cumulative** | **21 of ceiling 22** |

Two rejected submissions plus one successor attempt, as amended. No reset, no
refund.

## What the successor actually achieved — verified by falsification, not by reading

All **seven binding requirements** and **R-02 through R-10** are met on independent
static evidence. The auditor did not take them on inspection; it tried to break
them:

- **REQ-1 / R-02 — the classifier is real.** It ran the **old** classifier against
  the same in-span parse diagnostic and got `INTENDED-SPAN-RED` — wrong. The new
  one distinguishes status 0/1/124/127, parse/import/tool phrases and span
  membership. The `C-VOTEFOLD` `OTHER-SPAN-RED` is genuine: errors at
  `Vote/Invariants.lean:421:76` and `:1185:43`, **zero** parsed diagnostics inside
  the predicted `1150-1162`, independently reclassified code 21.
- **REQ-2** — all eight `U-RESTORE` argv name forward `C-*.diff` files that
  **exist**; zero `R.diff` references survive. `restore.sh` refuses a missing patch
  with `RESTORE-PATCH-MISSING`/90.
- **REQ-3 — the comparator computes.** Three constructed controls each produced a
  **sole** intended failure: an extra `stale.exit` → `RECEIPT-POPULATION`; a
  `641:15`→`640:15` edit → `OBSERVATION-IDENTITY`; `+100ms` → the total rose by
  exactly 100.
- **REQ-4** — preservation **1270/1270 retained**, zero missing.
- **REQ-5 — D-X1 discharged as required.** A source-level argument against the
  actual statement at `:967–978` with a concrete witness, and `operations.json`
  keeps `expectedClass: PREDICTED-KILL`. **No executed kill is claimed.**
- **REQ-6 / R-09 — the transport is bound.** Every funded stage runs under
  `nix develop git+file:///code/reactivegas?rev=efef604d…`, and S6 produced real
  runtime output: `corpus-check: ntraces=5 nevents=32 nsteps=7 live-bound`.
- **REQ-7 / R-10** — ledger 5 + 16 = 21 against 22, sixteen individually named
  records.

The instrument set is **materially repaired**. That is a real result, and it is the
first time in this milestone that the S3 instruments have survived an independent
adversarial pass.

## What it found that everyone else missed

**A named cannot-fail check.** `setupAndRestoreIncluded` in `compare-batch.cjs` is
`plan.every(p => expected.includes(p.id + ".ms"))` where `expected` is **derived
from `plan`** — **tautological on any well-formed plan**. It is not the identity or
extra-file control, so the comparator's real controls stand; but that assertion
proves nothing and survived my own review and the prior delta inspection.

## Why S3 is still not accepted

The **original blocking rows survive**, and they are semantic, not instrumental:

| row | independent residue |
|---|---|
| **F-01** | **127 OPEN atoms**, 1 CONDITIONAL; semantic ownership of role/effect/refusal/hook obligations not discharged |
| **F-02** | **561 OPEN ownership relations**; `PRESERVED` is byte precision, **not semantic relevance** |
| **F-03** | 207 original + 31 Mirrors rows represented; **SS-0 remains the one executed historical mutation** |
| **F-06** | source discovery is a **source inventory, not a freshly compiled private-name census**; nine Mirrors names lack a compiled binding |
| **F-07** | helper static recipes are **not elaborated witnesses** |

`C-VOTEFOLD`'s predicted theorem remains a **misbound `PREDICTED-KILL`** relative to
the mutated `sweepClosures` atom — correctly classified `OTHER-SPAN-RED` and **not
converted into a kill**.

**The honest summary: the instruments now work; the semantics remain unmeasured.**
Better tools, still no coverage.

## Limits the auditor stated, which I carry forward unsoftened

No Lake, Nix or mutant was executed by it — everything was re-run on frozen bytes
and copies. S4's success is empty streams plus exit 0 plus argv naming
`Check.lean`; the elaborated type is not printed. S2 ran a driver snapshot **not
byte-identical** to the frozen `mutant-C-VALIDATE.sh`. D-X1 is a **source
reduction, not a compiler observation**. The eval fixture proves shell reachability,
**not Lean `#eval`**. `U-RESTORE` `transport` strings still say `direct toolchain`
while the executing `restore.sh` is nix-bound.

## Disposition

**S3 NOT ACCEPTED.** F-01, F-02, F-03, F-06 and F-07 remain **PARTLY — blocking**.
All original 207-row and semantic-ownership obligations stand. SS-0 remains one
bound historical operation, not coverage.

The campaign is **terminal**: two rejected submissions plus one successor attempt,
21 of 22 charged, one unit unspent. **Any further S3 work needs a fresh desk
decision on scope and funding.** I propose none and have opened nothing.

`#66` is not closed. **#92's separate campaign is unaffected** and remains blocked
on its own Q-004.
