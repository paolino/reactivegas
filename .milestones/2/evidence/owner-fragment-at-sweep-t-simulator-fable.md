# RESUME — simulator lane (C1 / #70), ticket owner

Written for the desk to aggregate, not to reconstruct. Everything here is a
fact I verified, not a summary of intent.

Supersedes `resume.md` (2026-09-01, pre-handover). That file is kept as the
park-era record; **this file is current**.

## Identity and launch

| field | value |
|---|---|
| role | ticket owner, simulator lane |
| pane | `%313` |
| window | `reactivegas-ms2-t-simulator-fable` (session `reactivegas`, index 3) |
| parent desk | `%510`, gpt-6-astra |
| runtime root | `/tmp/reactivegas/ms2/t-simulator-fable` |

Exact argv to replay this seat — quotes included, the `[1m]` suffix is part of
the identifier and is not optional:

```sh
claude --dangerously-skip-permissions --model 'claude-opus-5[1m]' --effort high
```

## Repository

| field | value |
|---|---|
| worktree | `/code/reactivegas-sim-fable` |
| branch | `feat/economics-simulator-fable` |
| HEAD | `af9c1e5091014702c88df89e4b591819aad57979` |
| tree | clean |
| upstream | **none yet.** 30 commits in `origin/master..af9c1e5`; push + draft PR AUTHORIZED by NOTE-055 conditional on full local CI green |
| `origin/master` | `e6c59242ccf9b388053626c24446faaa2d7417fd` |
| Lean pin | `934de7a8df136d86a8ad2caadbda99af60e58b59` (#62 merge, PR #64) |

## Candidate

`af9c1e5091014702c88df89e4b591819aad57979` — **one** commit,
`feat(simulator): re-bind onto one membership`, mandated message verbatim,
parented on the rebase tip `dbd1ed8`. Delta vs `origin/master` is 13 files: eight
simulator surfaces, three scenario JSONs, two Lean trace producers. Nothing
outside the mandate.

Supersedes `035355bc`, which is **orphaned** — not an ancestor of HEAD. The
NOTE-010 teardown fix was amended in rather than added, so the slice stays one
commit.

## Instruments — all `chmod 444`

| artifact | sha256 |
|---|---|
| `handoffs/gate-v12-one-membership.sh` | `c9663ab92bde1e08776ec0c996051b45973b1b76bc28ae54d500547cc726bd50` |
| `handoffs/derive-cited-sources.mjs` | `5efd430530f310040c2dd31a2f7537947b34abc69e8e0a810324f39d5288c752` |
| `handoffs/oracle-one-membership-v1.mjs` | `d3979aacfcd5b8d85adedb9e3a8cdeaec0a2ed080b37904243524a94aadcc372` |

v9/v10/v11 are on disk as the record of what ran before each ruling. **Do not
run them.** v12 is the instrument.

Gate lineage, because two of the four versions exist due to defects in my own
instrument, both caught by someone else:

- v9 → asserted tip equality (predecessor's; REDs on any unrelated merge)
- v10 → reachability + blob equality, hardcoded pair (mine)
- v11 → step 3 narrowed so a true claim citing `KelGroups.Proposal.removeMember`
  is not forced to be reworded (**A-003**; my gate deforming the product)
- v12 → cited extent derived from `CHECK_RECEIPT.sourcePins`, all 22 checked
  (**A-002**; my gate reproducing the defect it was built to catch)

## Stage

**Fresh independent audit in flight.** Not accepted.

Full-gate receipt, deterministic — three consecutive v12 runs on `af9c1e5`:

```
run 1 exit=0  ENOTEMPTY=0   evidence/af9c1e5-gate-run1.log
run 2 exit=0  ENOTEMPTY=0   evidence/af9c1e5-gate-run2.log
run 3 exit=0  ENOTEMPTY=0   evidence/af9c1e5-gate-run3.log
                            evidence/af9c1e5-determinism.log
```

Identical GREEN banner each run: pin reachable, **all 22** cited sources fresh at
their own pins, 14 constructors derived from the pinned Lean, retired absent with
`removeMember` only in the faithful-vocabulary citation, no second membership
store, oracle + 7 negative controls, build/claim/trace/vote/scenario/teaching
each with selftests.

## Descendants

| pane | role | seat | state |
|---|---|---|---|
| `%315` | commit owner | `glm`, harness pi, provider zai, `glm-5.3-flash`, effort max | **parked, write-idle**, `PROOF-COMPLETE submission=1` on `af9c1e5` |
| `%517` | commit auditor, submission 1 | `codex` 0.153.2, `-m gpt-6-astra`, effort high | **running**, `START` verified 07:45:53Z, scope `dbd1ed8..af9c1e5` |

Auditor worktree `/code/reactivegas-sim-fable-audit-s62sim-b`, clean detached at
exactly the candidate, `.lake` warm. The earlier seat `%515` returned
`AUDIT-CONTRACT-BLOCKED` (CB-001, my packet omitted the campaign ledger and
build budget); its pane is killed, its root archived, its worktree removed.

Auditor launch argv, replayable — note **`codex-raw` no longer exists on this
host**; the shared skill's documented shape is stale and a seat launched from it
dies silently while `split-window -P -F` still returns a pane id:

```sh
codex --dangerously-bypass-approvals-and-sandbox \
  -m gpt-6-astra -c model_reasoning_effort=high
```

## Next action

1. full local CI (`nix develop --command just ci`) on `af9c1e5`, running in
   `scratchpad/verify-cc1500d` so the auditor's checkout is untouched;
2. let `%517` finish under its **frozen** C1 mandate; consume its terminal
   report; require it to confirm independently that semantic failure exit codes
   survive `rmQuiet` (NOTE-054's condition);
3. on CI green — push and open a **clearly scoped draft PR** distinguishing
   completed C1 work, actual audit state, and remaining #70 / #68 / #69 work.
   No auto-close of #70;
4. **then commission submission 2** — a fresh **grok-4.6** audit of the complete
   resulting candidate over extent `125409b53..candidate`, covering the nine
   unaccepted prefix commits plus C1 and any repair (NOTE-058). Packet already
   written: `handoffs/EXPANDED-REVIEW-PACKET-s2.md`. Campaign ledger must be
   versioned for the wider extent before dispatch;
5. then the remaining #70 scope: scenario generator. #68/#69 are separate
   semantic slices and are never anticipated.

**Submission cap: 2, and submission 2 is the wider audit.** The `%515`
`CONTRACT-BLOCKED` dispatch is recorded as rejected before substantive review
and does not consume one. No automatic ceiling raise; no third submission.

## Carried in the acceptance packet, not as closed

- **Residual 1 — silent cleanup.** `rmQuiet` retains semantic exit codes but its
  `catch` is silent, so a genuine teardown failure vanishes. Accepted by
  NOTE-054 as an explicit non-blocking limitation **for C1 only**, conditional on
  the audit confirming semantic codes are preserved. Not to be amended in now.
- **S1 (#66) dependency: none today, conditional tomorrow.** Verified at
  `af9c1e5`: `TraceDriverV1.lean` imports `Reactivegas.Step`, not
  `Reactivegas.Trace`; `Trace.lean` absent from the 22 cited sources; zero
  `declaration`/`guard` JSON keys in the corpora. The moment any driver here
  imports `Reactivegas.Trace`, S1 becomes a hard prerequisite.
  Report: `handoffs/S1-integration-dependency-report.md`.
- **V-2 threshold — the operator's, unlanded.** `bgMajority` stays
  `(adminCount + 1) / 2` verbatim; the frozen oracle pins n = 0..5 including the
  n=2 → 1 case, so anticipation REDs.
- **Design-record content held**, routed not written:
  `handoffs/design-record-content-for-71.md`. No lane commit has ever touched
  `docs/en/design/`; verified three ways.

## Authority

**Push and a clearly scoped DRAFT PR are AUTHORIZED** by NOTE-055/NOTE-058,
conditional on full local CI green on the candidate. **No merge and no
publication** remain unauthorized.

Canonical publication destination, when authorized:
`https://preview.dev.plutimus.com/lambdasistemi/reactivegas/sim-fable/`
— the destination this lane published to on 2026-08-30 under operator order,
bytes-identical local/published, verified by sha256 both sides.

## The prefix acceptance gap — carried, not closed

A PR on this branch lands **30 commits**, not one. The last acceptance receipt
maps to `125409b53`; **nine commits sit between it and C1 with no acceptance** —
`4a90e36` (journal records `published=yes accepted=no`) plus the eight
operator-directed changes of 2026-08-30 that NOTE-048 was written about.

Submission 1's scope (`dbd1ed8..af9c1e5`) does **not** reach them. NOTE-058 ruled
that an honest PR limitation does not accept a formerly rejected or unaudited
change, and that v12 partly exercising their surviving effects is not complete
behavioural coverage. Closing that gap is submission 2's job.

Inventory: `handoffs/prefix-acceptance-inventory.md`.
