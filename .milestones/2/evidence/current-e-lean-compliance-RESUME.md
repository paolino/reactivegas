# RESUME — quality epic owner `%503`, issue #66, milestone 2

Refreshed 2026-09-06T16:30Z. S4 landed; S3 terminal; #92 successor live on rebound base 890a74f1.

## Seats

| seat | state |
|---|---|
| `%503` | this desk (`claude`, ticket owner) |
| `%633` | **#92 successor commit owner** (`grok`/`grok-4.6`) — LIVE, START 14:49:13Z, 0/11 author rows spent |
| `%547` | S4 author, idle, all budgets spent |

All other seats are **terminal and retired with outputs preserved**: `%615` (Sol,
terminalized, not the successor author), the three #92 inspectors, and the S3 owner
and Grok auditor.

## S4 — LANDED

Merge commit **`efef604de87b2a1efae51e84d1a9150e585c1db0`**, parents `3590c001` +
`04eb6c7d`, master tree **`caaa0488f39a6afb2553680a11fd6bfd86d1c90b`** matching the
accepted tree exactly. PR #89 MERGED 08:37:18Z, `closingIssuesReferences` empty, 0
comments. Receipt `handoffs/S4-LANDING-RECEIPT.md`.

**Nine limitations carried unwaived**; both audits remain AUDIT-FINDINGS, not PASS;
**F-001, H-01, H-02, H-03 OPEN**. Landing this bounded slice did not establish the
full model and did not close #66 or #72.
## S3 — TERMINAL. Instruments repaired; subject NOT accepted.

Grok 4.6 auditor **AUDIT-FINDINGS** 12:47:12Z, report `1df47656…`, 283-entry
manifest verified, **product unit unspent 0/1**. Final: **21 of 22** charged.
Two rejected submissions plus one successor attempt. **No further submission
exists and none was opened.** Disposition `handoffs/S3-TERMINAL-DISPOSITION.md`
(`1716bea5…`).

All seven binding requirements and R-02–R-10 **met**, verified by falsification —
the old classifier misclassified the same diagnostic, three comparator controls each
produced a sole intended failure, preservation 1270/1270, D-X1 discharged with no
executed kill claimed, every stage nix-bound.

**Still blocking, and semantic not instrumental:** F-01 127 OPEN atoms; F-02 561
OPEN ownership relations (PRESERVED = byte precision, not semantic relevance); F-03
SS-0 still the one executed historical mutation; F-06 source inventory, not a
compiled private-name census, nine Mirrors names unbound; F-07 helper recipes not
elaborated witnesses.

**The instruments now work; the semantics remain unmeasured.** A named cannot-fail
check was found in `compare-batch.cjs` (`setupAndRestoreIncluded` is tautological)
that my review and the prior delta both missed.

Further S3 work needs a fresh desk decision. Seats retired, ~784M of artifacts
preserved on disk.
## S5 — three obligations, all open

Retention outside V-5; `ONWARD-68-INV-01` inversion exactness; the **OPEN
finite-history correspondence** under ruling `RG-S4-REACH-20260906`, retaining
#75 and #71.

## Supervisory discipline (reloaded 2026-09-06, skill rev 615711b)

- **One long blocking wait per turn** on children's journals — harness Monitor
  or `wait-status` (default now 590 s). **No loops of short waits.**
- Verify a child's claim **at the transition, via a one-line check**; a supervisor
  does not read artifacts into its own context. **Conflict with this campaign's
  standing source-verification instruction is unresolved — see
  `handoffs/SKILLS-RELOAD-CONFLICTS-AND-TURNOVER.md` §2.**
- Supervising seats run at the **standard pin, not `[1m]`**. This seat is `[1m]`;
  a turnover to a standard-pin seat is owed, at the desk's timing (§1).
- A child parked on a decision writes its resume brief and **exits**.
- `capture-pane` for **liveness only**, 10–15 lines, after an event or stale verdict.

## Audit process (af60ac2, 2026-09-06T09:10:31+01:00 — supersedes a55a25a2)

Applies to **new eligible campaigns**, not retroactively to frozen ones:
2–3 **parallel blind inspectors** on submission 1 with distinct fault scenarios,
mechanical findings collection, **one** ticket-owner adjudication and **one** repair
batch, **one** fresh delta inspector on submission 2. No repeated adjudication at
epic or desk altitude. **One declared campaign budget**, not a default per inspector;
historical spend preserved separately and never retroactively refunded. Simplified
launch packets; **no receipt files**. Existing explicit full-candidate audit
requirements stay binding until an actual amendment reconciles them.

**Status tags: core + role vocabulary only.** `START`, `NOTE`, `BLOCKED`, `RESUMED`,
`GATE-PASS`/`GATE-FAIL`, `COMMIT`, `PUSHED`, `COMPLETE`, plus the role tags
(`AUDIT-PASS`, `AUDIT-FINDINGS`, `PROOF-COMPLETE`, …). **A seat invents no tag** — a
parent can only wait on a tag it knows. Earlier events in this journal carry invented
tags; they stand as written and are not rewritten.

**Contract revision limit:** at most two revisions before a build exercises a packet;
the third revision is the build.

## Pointer transport (`cb154732`)

**One buffer load, one paste, one Enter.** The **only** delivery success criterion
is a **post-cursor worker-journal event**. Scrollback containing the pointer, a
visible paste, or a busy spinner without `START` **does not prove delivery**.

**An acknowledgement timeout is uncertain delivery, never failure** — it never
authorizes a second Enter or a resubmission. Classify the pane and read the journal
first. Resize any pane below **40 columns or 8 rows** before injection.
`pane-nudger` is the separate mechanism for a stable unsent composer.

*I broke this once today: `send-pointer` reported no ACK on `%622`, I retyped the
instruction by hand, and the seat had already absorbed it — a duplicate submission
from inferring failure out of a timeout.*

## Dispatcher rule — auditor launches (learned the hard way, 2026-09-06)

`commit-auditor` SKILL.md:29-35 requires the canonical approval-bypass launch block.
**A bare `codex …` launch leaves the seat suspended behind an approval dialog it
cannot see past or journal**, so only dispatcher-side pane capture detects it — and
that omission is chargeable to the dispatcher. It cost three stalls today (`%578`,
`%581`, and both S3 inspectors for an unknown duration).

`codex-raw` is **not on PATH** here; the installed `codex` 0.153.2 accepts the flag:

```sh
codex --dangerously-bypass-approvals-and-sandbox \
  -C <root> -m <model> -c model_reasoning_effort=<effort>
```

The read-only-candidate and writable-isolated-evidence boundary is enforced by the
**brief and by parent verification at source**, never by the bypass flag.

## #92 — successor live on the rebound base, ceiling 26

Two prior campaigns **rejected**. `580e3d5f` is rejected for landing; that campaign
is terminal at 14/20 author with three launch attempts spent.

**`%633` (`grok`/`grok-4.6`) is the successor commit owner**, START 14:49:13Z,
`alternate=true`, mandate `4dc4ab70…`. Three actual CLI families now: `claude`
ticket owner, `grok` author, `codex` inspectors — *a model alias is not another
family, which is what my Sol-then-Astra proposal got wrong.*

**Base rebound to master `890a74f1c4c34b52c55b5d941c78c94fa504e005`**, tree
`0f40463d…`, after #90 landed. I verified it, that `efef604d` is an ancestor, and
that the #90 delta (19 paths) and C1 delta (15 paths) have **zero overlap**.

**Quality-only fence — exactly four paths**, `justfile` deliberately excluded:
`scripts/check-lean-mirrors`, `scripts/lake-roots/{lakefile.lean,Main.lean,.gitignore}`.
The combined C1 tree is **integration evidence only** and never the landing subject.

Ceilings **26 total / 22 author / 14 no-repair**; launch attempts **5**; 11 author
rows for submission 1 (`N1`, `A1`–`A7`, `A8R`, `A8G`, `INT`). `N1R` only if
native-tool bytes change.

## Next action

1. **Supervise `%633` event-driven** through its 11 author rows to submission 1.
   One long wait per turn, overlay check folded in. It stops at the first required
   failure and returns the exact branch; no retry or setup margin exists.
2. **Packet format V2** (`6aa0ad7c`): before any freeze I must **author** the three
   required-role artifacts — `dispatch-preflight-receipt`, `current-campaign-ledger`,
   `current-row-ledger` — and bind exactly one input to each with `ROLE`. Freeze and
   verify refuse missing, duplicate, undeclared, dangling, legacy role-free or
   changed-after-READY bindings. Each seat runs `verify` **and** `preflight` from its
   own cwd; those results go in a **separate launch receipt cited in `START`**, never
   appended to the sealed receipt.

   **Role binding proves presence, uniqueness and immutability — not that the contents
   are complete, current or true.** Omitting the receipt is what ended the last
   campaign, and it was mine.
3. **S3 needs a fresh desk decision** on scope and funding. Propose none.
4. **No merge** without a separate exact-head desk authorization.
## Standing bars — unmoved

No push, PR, merge, release, issue/review comment or gist without an exact desk
grant. Merge only via `mcp__merge-guard__guard-merge` after a separate exact-SHA
grant. No `docs/en/design/` writes while #71 is open. Never weaken a check to make
something pass; never enable `#eval!` to bypass a generated `sorry`. Upward
delivery is local files only. **`#66` must not be closed.**

## The recurring failure to watch in myself

**Eight times** this session I asserted something from a label, a summary or an
expectation rather than from the bytes:

1. relayed "digests swapped" without computing them;
2. claimed the ruling was in the closure map when only the issue body had it;
3. wrote a preservation locator pointing at bytes that did not contain the text;
4. read the validator's `UNSUPPORTED-OBSERVED` **message** as its predicate;
5. read a **window name** and nearly reported my own live seat as recycled;
6. re-attached the old constant-`yes` defect to a comparator I had not opened —
   the successor caught it;
7. called X6 an answer "by construction" when it demonstrated one spelling;
8. reported two seats as dispatched when neither had been sent a pointer.

Also: inferring liveness from plausible pane text, and from CPU that proves neither
stall duration nor progress.

**Open the file. Compute the hash. Run the control. Check what actually moves.**
