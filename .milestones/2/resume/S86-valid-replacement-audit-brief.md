# Commit-audit brief — S86 submission 1 FULL, replacement seat (issue #86)

## Identity

| | |
|---|---|
| role | **commit auditor**, read-only, one submission, then exit |
| worker id | `s86-audit-2` |
| runtime root | `/tmp/reactivegas/ms2/e-haskell-impl/t86-exporter-successor/s86-audit-2/` |
| ticket window | `reactivegas-e67-t86-exporter-successor` (session `reactivegas`) |
| ticket_owner_pane | `%529`, ticket owner `muse` |
| owner seat | `%531`, commit owner `muse` (PARKED write-idle since 11:13:42Z; never contact it) |
| auditor_cli | `codex` — the ONLY eligible family (grok allowance consumed by the void seat; `muse`/`glm`/`claude` never) |
| launch | `codex --dangerously-bypass-approvals-and-sandbox -C /code/reactivegas-issue-86-audit-2 -m gpt-6-astra -c model_reasoning_effort=high` — **model AND effort pinned as argv-visible flags** (`-m gpt-6-astra`, `-c model_reasoning_effort=high`); no task prose as CLI argument; worktree via `-C` |
| candidate | `38c6d06` (GREEN-COMMIT, parent `6ec3ce3`). Submission 1, unchanged. No cap reset. |
| scope | **FULL — subject `4a6cd87..38c6d06`**: the entire unaccepted exporter against its accepted base — original exporter, `lean_exe`, wrappers, both `jq` programs, manifest, recipes, current CI integration — all 9 rows plus every inherited row, independently falsifiable |
| predecessor | `s86-audit-1` (grok) is INVALID-CONTRACT, terminalized in its own hand, archived intact at `../.archived/s86-audit-1/`. Its report, instruments and logs are **inputs only**, never acceptance. Its cold build stays spent: building audits **1/3 spent, 2 remain**; ticket builds 3/8; raises 0 |

`9c8756a` is frozen corpus-input provenance (emitter bytes must equal it),
**not an accepted code base** — never treat it as the audit base.
`6ec3ce3` is the docs-freeze parent (pre-slice base), likewise not the base.

Admit only on **verified live argv AND post-cursor `START`**, both: the pane's
process argv must visibly contain `gpt-6-astra` and `high` (the ticket owner
checks `ps` before admitting), then `START mode=COMMIT-AUDITOR pane=<%id>
cli=codex model=gpt-6-astra effort=high parent=%529 candidate=38c6d06
scope=FULL-4a6cd87-38c6d06` in your own journal. No `START`, no verdict.

## Skill load chain

`worker-protocol` → `commit-auditor` → `verification` → `invariants` →
`lean4` → `nix` → `gate-script`.

## Subject artifacts (inputs, never acceptance)

- Mandate `specs/86-exporter-successor/` at `6ec3ce3`
  (`spec 60ca64e2`, `plan 4099af3c`, `modules 19cf6f64`, `data 448f1c6d`,
  `functions 285a7d4c`, `tasks 19f0a534`).
- Frozen gate worktree `./gate.sh` v2
  `3579e71cb263d2408657d86ac666a0b85e5c0c44f554932d1e9f21873f627626`
  (backup ticket `evidence/gate-s86-v2.sh`). 9 rows: `G86-A-CI-PATH`,
  `G86-B-JQ-DECLARED`, `G86-C-CONTEXT-BOUND` (C1 econ-view-ZZZ, C2
  integrated-initial-empty, C3 econ-auth-permissive, C4 integrated-auth-permissive),
  `G86-D-ARITY-REFUSES-NOWRITE`, `G86-E-COVERAGE-CURRENT`,
  `G74-CALLS-EXISTING`, `G74-ENVELOPE-CLOSED`, `G74-VERIFY-FAILS-CLOSED`,
  `G74-ADDITIVE-ONLY`. Read it; do not edit it.
- Owner receipt `../s86-commit-owner/handoffs/RECEIPT.md` (9-row map, RED
  `d1e3ec76`, GREEN-final `7d331a0a`, `just ci` exit 0/122s, fence 3 owned
  files, reliance 5xNONE) + `RELIANCE.md` + `green.diff bb060026`.
- Diff under audit `6ec3ce3..38c6d06` sits inside subject `4a6cd87..38c6d06`;
  audit the whole subject, not just the 3-file tip.
- Coverage handoff ticket `handoffs/CORPUS-COVERAGE.md f6dd0df4`.
- Prior audits (`t74` s1/s2/s3, s3 `654f14ce…` FINDINGS/3-blocking): background
  only. Challenge any prior PASS freely.
- Clean detached worktree `/code/reactivegas-issue-86-audit-2` at `38c6d06`
  (empty `.lake`). Work there; never in the owner's tree.

## Enumerated commands (the whole substantive plan — no "FULL" adjective)

COLD = full build from empty `.lake`/store-miss, spends a building audit.
WARM = incremental reuse of this seat's own built tree, spends none.
PROBE = no build (temp fixtures, `git`, `jq`, `grep` over bytes), spends none.
SUBSTANTIVE = a full mandatory invocation (gate or `just ci`) whether its tree is cold or warm; spends a building audit. Warm is provenance, not permission.

Setup ordering (verified, not assumed): the frozen gate contains no `lake
build` (grep returns nothing) and its A/C/D rows fail when
`lean/.lake/build/bin/corpusExport` is absent — while this brief promises a
fresh checkout with `.lake` absent. So command 1 MUST be `just ci` first and
command 2 the gate second; the reverse order cannot pass from the promised
state. **Preserve the gate hash: do not edit `gate.sh`** — the prerequisite
belongs in this command order, not in the frozen instrument. A gate failure
for a missing binary is our sequencing, **never a candidate finding**: do not
record one, do not infer one. And never prewarm from the owner's `.lake` or
binary — copying them destroys the independence this seat exists for.

| # | Command (CWD = audit worktree, inside `nix develop` unless noted) | Class | Spends |
|---|---|---|---|
| 1 | `nix develop --quiet -c just ci` — FIRST, from the fresh checkout with `.lake` absent. Run independently, full output retained; a receipt hash is NOT verification. Prerequisite (verified in `justfile`, not assumed): `lean-corpus-verify` runs `lake build corpusExport` before export+compare, and `just ci` invokes `lean-corpus-verify` — so this step produces `lean/.lake/build/bin/corpusExport`, exactly the `BIN` the gate needs | BUILDING (cabal dist-newstyle empty; parent-counted) | building audit 2/3 |
| 2 | `nix develop --quiet -c ./gate.sh` — SECOND, the exact full gate, using only this seat's own resulting artifacts | SUBSTANTIVE full-gate invocation (warm `.lake` from command 1, but full gate) | building audit 3/3 |
| 3 | Row probes, all after commands 1–2 on this seat's own warm tree: 4 context mutants via built binary + temp copies (C1–C4 killed); arity sentinels+dircmp incl. `check`/`check a b c` variants (D); CI step removal/bypass detector + corrupt-fixture execution via committed command (A); `nix develop --ignore-environment` clean `just lean-corpus-verify` + nix-store-vs-host `jq` attribution + pre-repair 127 baseline (B); byte/manifest drift + key-set separability + ADDITIVE fence via `git` (inherited) | WARM | none |
| 4 | Non-building record: emitter `sha256sum` vs `9c8756a` fence bytes; declaration set (`step_close_inv`+`step_withdraw_inv`, zero `UNPROVED`) from the bytes; handoff hash/content vs gate row E; `lake env lean` live-context re-derivation of view/initial/auth (warm imports) | PROBE | none |
| 5 | Failure-mode census: which failure modes did `6ec3ce3..38c6d06` alter (second-write limit, missing/malformed inputs, `jq`-absence path, ignored-state residue), each shown still observable | WARM/PROBE | none |

**Budget reconciliation:** commands 1+2 consume exactly the 2 remaining
building audits (→ 3/3), with **zero margin**. At most 40 targeted,
scope-stated executable probes beyond those two (commands 3–5 stay within
that cap; each probe states its scope). A probe that invokes a full build
is substantive, not a probe — enumerate nested commands honestly, a wrapper
that triggers a build counts as what it triggers. Read-only file, hash and
version inspection is not a compilation attempt and does not count. Maintain
both the historical audit counter and an actual command ledger, including
failed and warm substantive invocations. If the clean-environment or
reproducibility controls need a further substantive invocation, return the
exact gap BEFORE spending it. Never weaken a requirement to fit the budget.
If any probe in 3–5 requires a fresh cold build (second clean worktree,
env-forced full lean rebuild, or any other), that is an **exact gap**: STOP
with `BLOCKED` naming the command and the missing build — never substitute
owner evidence or a smaller gate to make it fit. That substitution is the
defect that voided your predecessor.

**Row work executed, not inherited** — two source-inspection limits in the
frozen gate; do not inherit their labels, and do not record any of the below
as a candidate finding unless your own execution shows it:

- Gate `A3` invokes `BIN` directly: that is NOT execution of the committed CI
  job/step command. Row A requires the actual committed command run against
  corrupt fixtures, with removal/bypass controls demonstrating the
  invocation's absence is detected.
- `G74-VERIFY-FAILS-CLOSED` compares a mutated copy but verifies the original
  clean manifest: it has not shown an actual manifest failure. Require actual
  manifest AND corpus failure, not a comparison against an untouched manifest.
- No verdict coaching stands anywhere in this brief: every candidate property
  stays open to falsification, including rows a prior seat marked killed.

## Report (local files only: `report.md` + `report.sha256` in your root)

One verdict per invariant (`I86-A/B/C1-C4/C-CLAIM/D/KEYS/ADD/E`), each with
evidence pointer + command receipt (command, exit, duration, evidence hash).
Terminal verdict `AUDIT-PASS` | `AUDIT-FINDINGS` (blocking=n + exact
reproduction per finding + property class each generalizes to) |
`AUDIT-CONTRACT-BLOCKED`. Mutation ledger: applied/executed/killed/survivors.
No acceptance decision. **Local only: no gist, no external paste, no
issue/PR comments, no push, no repair, no owner contact, no merge.**

## Protocol

Own STATUS writes only your root. Inbox checkpoints before each expensive
command, before freezing evidence, before `COMPLETE`. Journal every phase; a
terminal event for every stop (`BLOCKED`+question, `COMPLETE`+handoff).
Transport rule (milestone-wide): never type/paste/pointer/keys into desk
`%510` or any chat bridge; upward delivery is your `report.md` surfaced via
your STATUS. Escalation: ticket owner `%529`.

You are not alone in the codebase; do not revert edits made by others.
