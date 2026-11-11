# Q-002 — gate v4 header mismatch (leg-2 fails by eye, zero builds spent)

From: commit owner `t28-app-api/commit-owner` (pane `%545`). To: ticket owner `%534`.
Date: 2026-09-05. Worktree `/code/kelgroups-issue-28`, HEAD `570fe4a` (RED, clean).
Re: answer A-001 + r5 + gate v4 + BINDING-570fe4a (all read in full).
RESUMED Q-001 recorded. GREEN NOT started (0/9 spent). This is a freeze-defect
challenge (gate's own leg-2 rule), not a requirement question. Parked after
BLOCKED Q-002. No builds, no gate run, no GREEN code written this turn.

## Verification by eye (leg-2 procedure, charge-0 recon only)

- Worktree `gate.sh` vs `handoffs/gate-v4.sh.backup`: `diff -q` identical.
- Full hashes: worktree `1c19f172…` == backup `1c19f172…` == freeze note `full=1c19f172…` ✓.
- Normalized (`sed 's/^GATE_SHA256=".*"/GATE_SHA256=""/' | sha256sum`): worktree `ad7826a5…` == backup `ad7826a5…` == freeze note `gate=ad7826a5…` ✓.
- Header inside file: `GATE_SHA256="e358cc38…"` (v3) ≠ normalized `ad7826a5…` (v4). `GATE_VERSION="G28-1 v4 (r5)"`, `FROZEN_BASE="570fe4a…"` are updated; ONLY the header hash is stale.
- Leg-2 (`[ "$GATE_SHA256" = "$norm_hash" ]`) therefore FAILS by eye, triggering stop-before-spend (`IDENTITY-FAIL`, exit before any build/mutation). Any `./gate.sh` run now fails legs 1/2/2b with 0 builds spent and no mutant evidence.
- Evidence: commands above (read-only: `grep`, `sha256sum`, `diff -q`, `git rev-parse`, `git status`). No compilations, no tests, no gate run. Spend this turn: 0 builds, 0 probes.

## Context (accepted, standing)

- RED `570fe4a` accepted as GREEN parent (HEAD verified clean). D1–D5 rulings, H4'/H7 mandates, two-layout format rule, R-a–R-f refinements, spend FINAL (RED 4B sunk + GREEN 9B preserved + SLIM 3B; dev ≤14 probes, trigger 12; formatters charge-0) all absorbed from A-001/r5/BINDING.
- D1 (suffixed `*Voted` arms), D2 (M6 arity `-> s -> STM ()`), D3 (flattened M1), D5 (founding `GroupState s` arg) require a PASSING leg-2 before any of them can execute. The header defect blocks all six kills independently of their correctness.

## Options

- (a) You issue a header-corrected gate v4.1 (or v5): single-line header update to `GATE_SHA256="ad7826a5bfd1d6afce9418a223d329135967f18d3842be5706c2ca21e9f8292c` (the normalized bytes you already froze), recompute full hash, record new freeze note (gate stays `ad7826a5…`, full changes), preserve all backups, re-bind (new binding version + hashes) before execution. Same-requirement mechanical under A-001 §3 (gate's own self-hash rule) — no desk round, but re-bind required since the frozen bytes change. Then wake me to GREEN under the corrected freeze.
- (b) If you rule the header intentionally stale (not recommended; contradicts leg-2 + r5 Gate v4 summary + freeze note), state the exact leg-2 bypass in writing as a new binding version — I will not route around a frozen check without it.

## Recommendation

Adopt (a). One-line header fix, new full-hash freeze note, re-bind, then GREEN. No RED rework (RED `570fe4a` untouched, logs/hashes stand). No budget impact beyond your freeze (my GREEN 9 + SLIM 3 preserved; this turn spent 0/0).

## Decision needed (durable, in my `answers/` + wake)

1. Corrected gate freeze (header + full hash + backup + binding version) with leg-2 passing by eye.
2. Confirm GREEN still on RED `570fe4a` as parent under the corrected freeze (my recommendation) with identical D1–D5/H-mandates/R-a–R-f.
3. Confirm spend preserved (GREEN 9, SLIM 3, dev ≤14/trigger 12, formatters charge-0; this Q + verification cost 0/0).

Frozen diff at challenge: none (worktree clean at RED `570fe4a`; gate.sh ignored, untouched by me). Candidate: none. Evidence: this Q, STATUS journal, gate header/normalized/full quotes above.
