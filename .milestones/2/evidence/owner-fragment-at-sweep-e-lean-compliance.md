# Resume fragment — e-lean-compliance (epic owner, #66)

**This file is the single current resume instruction.** It replaces the previous
fragment, preserved verbatim and explicitly historical at
`RESUME-v1-superseded-20260905.md`. That older file describes S1 seats `%507`/
`%508`, base `e6c5924`, and an S2 successor that is "merely proposed" — **all of
that is dead.** A successor following it would restart the wrong work. The
append-only journal `STATUS.md` remains the history; this file is the projection.

Rewritten 2026-09-05 under the desk's recovery sweep. No build, mutation,
candidate change, audit coaching or dispatch grant is implied by it.

## Exact launch

| | |
|---|---|
| Seat | `claude-opus-5[1m]`, Claude Code, pane **`%503`**, window `reactivegas:4 lean-quality` |
| Runtime root | `/tmp/reactivegas/ms2/e-lean-compliance/` |
| Own worktree | `/code/reactivegas-lean-compliance`, branch `chore/lean-quality-compliance` |
| Parent desk | `%510` `ms2-reactivegas-kelgroups` (gpt-6-astra) |
| Issue | **#66**, parent epic #72, milestone 2 "Reactivegas on kelgroups" |
| Upward delivery | **local files only** — own `STATUS.md`, own `handoffs/`, a notice in `/tmp/reactivegas/ms2/inbox`. Never type into `%510` or any human composer. |

## SHAs that matter — read these, not the old ones

| | |
|---|---|
| **Accepted `master`** | **`3590c0015b84fd58004bf6fb44dd18b107304c48`** — S2R **LANDED**, squash, single parent `d670323`, tree `44a1f0bce4796c63203070e23b96172a7774956e` |
| previous accepted bases | `d67032313acf3699cc50358a057391b88d002192` (PR #87 exporter); `4a6cd87fcbc3e4a536bbc9f240f5efe5704022af` (PR #79, S1) |
| landed S2R candidate | `ab617d88af9d080de71218f3cc553d60ef0b6de0` — its tree is identical to `3590c001` |
| its parent | `714cb2a8536b24bf735295137e8f907782228380` — S2R submission 1, audited AUDIT-FINDINGS |
| PR #88 | **MERGED** 2026-09-05T21:12:18Z, `closingIssuesReferences []`. **#66 deliberately not closed** |
| Superseded PR | **#85** — old S2 branch `chore/66-s2-axiom-gate`. OPEN, untouched, **not mine to close unasked** |
| Rejected S2 candidates | `5745a2c`, `561347d`, `b0c2cdb` — preserved with their audits, never reused |

`fa01779` is the **accepted S1 repaired candidate**, tree-identical to landed
`4a6cd87`. It is *not* a rejected S2 candidate.

## Seats — current, with live state

| pane | seat | role | state |
|---|---|---|---|
| `%544` | pi muse-spark-1.3, PID 1312702 | S2R commit owner, `/code/reactivegas-66-s2r` | **LIVE, thawed.** 18/18 substantive, 35/37 probes. Submission 2 delivered, parked |
| `%548` | codex gpt-6-astra high, PID 1664317 | S2R submission-1 auditor | **TERMINAL.** AUDIT-FINDINGS. Preserve; **do not reactivate** |
| `%555` | codex gpt-6-astra high, PID 2331552 | S2R submission-2 auditor | **TERMINAL.** AUDIT-PASS 32/32, report `c71dda1e…`. Preserve; **do not reactivate** |
| `%541` | pi, PID 1246512 | S4 Phase A, tree `4a6cd87` | **SIGSTOP (`Tl`), finished.** 3/3 builds, 15/20 queries. **Do not restart** |
| `%547` | pi, PID 1493708 | S4-B mirrors, `/code/reactivegas-66-s4b` | **LIVE.** Candidate `189e1ed3`, **4/6 substantive**, 41/60 targeted, 0/2 submissions. C2/C3 in cap; **C4/C26 held pending a +2 grant** |
| `%558` | pi muse-spark-1.3, PID 2401092 | S3 Phase 1, `/code/reactivegas-66-s3-phase1` at `3590c001` | **LIVE.** Cost assessment only, 3 builds max, **no coverage claim** |

**Thawing a stopped seat is not one signal.** Established the hard way today:
`SIGCONT` alone leaves the seat unreachable, and `fg` **re-cooks the tty**. The
order is **SIGCONT → `fg` (restore foreground process group) → restore termios
last**, applying a live sibling seat's exact `stty -g` rather than
flag-approximating. Verify `tpgid` equals the agent's pgid and the mode matches a
running reference before sending anything. Rollback records:
`handoffs/owner-544-tty-resume-before.json`.

## Authority — paths and hashes

| what | where | sha256 |
|---|---|---|
| S2R submission 2 | `commit-owner-s2r-muse/handoffs/SUBMISSION-2.md` | `ce4d84ed…a116c5` |
| S2R submission 1 | same dir, `SUBMISSION.md` | `32299d25…319bff` |
| Submission-1 audit report | `candidate-auditor-s2r-final-codex/handoffs/AUDIT-REPORT.md` | `d634df52…4def90` |
| its final manifest | same dir, `FINAL.sha256` | `64dbd5ad…36892e` |
| Submission-2 auditor brief | `candidate-auditor-s2r-sub2-codex/brief.md` | `f62132c6…fdc9977` |
| A2′ eight constraints | `handoffs/S2-FROZEN-PACKET.md` | `99fe06ab…a38a6e` |
| Five successor elements | `handoffs/S2-SUCCESSOR-CAMPAIGN-PROPOSAL.md` | `5ef67ef4…0dcb3` |
| Frozen invocation allocation | `handoffs/S2R-CONTROL-RECONCILIATION.md` | `4ba46889…1eae81` |

**`handoffs/S2-MANDATE.md` is HISTORY, not authority.** Its §4 "named residuals"
proposal is superseded by the implemented renames.

## Exact next action

**S2R is landed. Supervise the two running phases to their terminal
deliverables** — do not stop at the release or landing receipt.

- **`%547` S4-B** — it runs C2 then C3 and **stops at 6/6**. C4 (the
  effective-checker-disable control) and C26 (final CI) are held on an escalated
  **+2 substantive** request I do not have authority to grant. If the grant
  arrives, release exactly those two rows. **If it is refused, C4 and C26 are
  reported OPEN and unestablished — never folded into another run, never
  narrowed away.** Then its candidate goes to a fresh independent auditor of
  another family.
- **`%558` S3 Phase 1** — cost assessment only. Watch for `PHASE1-REPORT.md`
  (P1-A…P1-D) or a concrete gap. **Phases 2..n are unauthorized** and are costed
  by its P1-D proposal; there is no implicit ceiling waiting for them.

Neither may overrun its cap. A concrete shortfall reported early is correct; an
overrun is not.

**Merge discipline, for whatever lands next.** Re-read the exact head/base tuple
immediately before acting and stop if either moved. Merge only through
`mcp__merge-guard__guard-merge` — proven callable in this executor — never a CLI
substitute, and only under an explicit exact-SHA grant naming the method. Remote
CI must be green at the exact final SHA, read fresh, never projected from a
superseded head. **C1 holds the next landing reservation**; no sibling quality
merge goes ahead of it without desk sequencing.

## Next-slice dependency map — finishing S2R is NOT finishing #66

| slice | state | wakes on |
|---|---|---|
| **S1** | **CLOSED**, landed `4a6cd87` | — |
| **S2** | **DEAD.** Campaign exhausted, three candidates rejected, no fourth | superseded by S2R |
| **S2R** | **LANDED** at `3590c001` | — |
| **S3 Phase 1** | **RUNNING** — `%558`. Cost assessment only, no coverage claim | dispatched; phases 2..n unauthorized |
| **S4 Phase A** | finished, report v3 delivered | desk disposition |
| **S4-B** | **RUNNING** — `%547`, 4/6. C2/C3 in cap, then STOP at 6/6 | **+2 grant for C4/C26, escalated** |
| **S5** | contract prepared, **no execution grant** | an explicit grant that does not yet exist |

S3 and S4-B wake on an **accepted, landed** base — not on an S2R commit, not on
green CI, and not on the owner's own green. S5 additionally owns
`ONWARD-68-INV-01` — inversion **binding** is not inversion **exactness**. The
recorded distinction, stated exactly: **14 constructor bindings**, **six
machine-checked converses**, and a per-consumer assessment finding **eleven exact
and three inexact**. Eleven *assessed* exact is **not** eleven compiled converse
proofs, and no proof count may be inferred from the assessment. What S5 owes is
the **per-consumer assessment** — `step_pledge_inv`, `step_accept_inv` and
`step_close_inv` omit the live non-stalled guard — not a derived proof tally.

Rows from the original mandate still wholly untouched — stated as **content
owed**, because the desk has explicitly accepted the filenames `Goals.lean` and
`decisions.md` as **optional**. Do not re-impose them as mandatory:

- **Theorem-keyed mutation ledger** — one mutant per guard, each keyed to the
  theorem that owns it, with the campaign's result. Owed unchanged.
- **Clarity measurement** — the record of what the Lean did *not* let a fresh
  reader decide, what it decided anyway, and every place a doc comment, a story
  and a definition disagreed. Owed unchanged.
- **Statement separation** — the theorem statements separated from the model,
  numbered to the design record, with the axiom listing at the end. It is the
  *separation and numbering* that is owed; a file named `Goals.lean` is one way
  to carry it, not the requirement.
- **Dated authority content** — every ruling recorded verbatim with its date and
  the precision added in conversation, cited by the doc comments that rely on it.
  It is the *dated, cited authority* that is owed; a file named `decisions.md` is
  one way to carry it, not the requirement.

#66 stays open until those and S3–S5 are done.

## S3 Phase 1 — reconciled, and one discrepancy named rather than rewritten

| | |
|---|---|
| **Operative frozen input** | `commit-owner-s3-phase1/brief.md`, sha256 **`77c9d6bf8425afdd`** |
| **Contract it binds** | `handoffs/S3-MANDATE.md` |
| **Operative version** | **revision 3** |
| **Grant** | **Phase 1 only, AUTHORIZED by NOTE-021.** 3-build cap, coverage claim forbidden. Phases 2..n unauthorized |
| **Dispatch condition** | accepted + **LANDED** S2R base, announced by the desk |

The claimed hash `77c9d6bf8425afdd` is **correct**, but it is the hash of the
**Phase 1 brief**, not of the mandate — the two were being conflated.

**Named discrepancy, deliberately not repaired here.** `handoffs/S3-MANDATE.md`
is revision 3 in content — it carries a "Changes from revision 2 (NOTE-021)"
table and records "Phase 1 AUTHORIZED by NOTE-021" — while its **opening line
still reads "S3 contract — revision 2"** and its preservation note mentions only
v1, never `S3-MANDATE-v2-superseded.md`, which does exist. So the file's header
understates its own version. The Phase 1 brief is unambiguous and points at
"revision 3 — read it". I am **not** silently rewriting a frozen artifact to hide
this; whoever dispatches S3 reads the brief, treats the mandate as revision 3,
and knows the header lags.

## Standing boundaries

No push, PR action, merge, publication, deployment, issue or review comment, or
gist without an exact desk grant. No `docs/en/design/` writes while #71 is open.
Never weaken or remove a check to make something pass. Never enable `#eval!` to
bypass a generated `sorry`. Artifacts the operator asked for carry **no** AI
attribution; only comments would be signed, and I never post one unasked.
