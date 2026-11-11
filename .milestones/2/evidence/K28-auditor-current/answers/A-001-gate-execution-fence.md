# A-001-gate-execution-fence — execution envelope BOUND (pre-execution, binding)

To: commit auditor `t28-app-api/commit-auditor-s28` (pane `%554`). From:
ticket owner `%534`. Date: 2026-09-05. Re: your
`questions/Q-001-gate-execution-fence.md` (BLOCKED acknowledged; conflict
CONFIRMED exactly as stated — the named-path gate cds into the author tree
per its lines ~34–37 and mutates/restores tracked sources in place; adopted
your proposed envelope verbatim; the fence-amendment alternative is NOT
taken). Authority: NOTE-015 (desk Q001-close-commissioning via epic —
complete scope, no drips). This file + pointer is your wake; journal
`RESUMED Q-001-gate-execution-fence` on delivery (verified against your
artifacts), then START, then execute.

## Ruling: your proposed envelope ADOPTED (no new candidate/gate-rewrite/cap-reset)

- Execution worktree (disposable, exact candidate, DETACHED):
  `/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28/exec-84a2dae/`
  — HEAD `84a2dae…` (verified at creation), clean. Created by ticket owner
  pre-execution (setup, no builds); owned by you from RESUMED on.
- Gate copy (byte-identical frozen v8, named `gate.sh` at that root):
  `…/exec-84a2dae/gate.sh` — full sha256
  `7a7a99e351df0a34a8147804f68bc85a3f182556cbfafccaf62e46105743c30a`,
  blank-normalized self-hash
  `f5796d1e5524f57c98b7d92168ad4fc10b22b6d318c57e0c989a1bf02f1b3650`
  (both recomputed ON THE COPY pre-execution — no abbreviated substitutes).
  Copied from the hash-bound backup (provenance chain intact); executable
  bit set; worktree gate file never executed from the author path.
- Evidence dir (overrides gate default): `G28_EVIDENCE_DIR` MUST equal
  `…/commit-auditor-s28/handoffs/evidence/` (created empty pre-execution).
  The gate default (author `handoffs/evidence`) would mix log ownership —
  never use it.
- Reference checkout `/code/kelgroups-audit-84a2dae` stays IMMUTABLE
  (read/compare only — your independent read-through surface). Author
  checkout `/code/kelgroups-issue-28` UNTOUCHED, never executed (no builds,
  no gate runs, no reads required — leave it alone entirely).

## NOTE-014 mechanics (subsumed here — no second answer)

Author tree NEVER writable by you (no builds, no edits, no gate runs
there). Rationale recorded so the rule survives contact with temptation:
the frozen gate resolves its own script directory and `cd`s there, so ANY
invocation of the author-tree path — regardless of your shell's initial
CWD — runs AND mutates the author tree. Hence the disposable copy above is
the ONLY executable gate. Pre-first-invocation proof procedure (journal ALL
of it BEFORE leg-3, i.e. before the first build): quoted `pwd` (must be the
exec root), `ls` of the exec root (must show YOUR gate.sh copy), `echo
$G28_EVIDENCE_DIR` (must be YOUR evidence dir), `git -C <author> rev-parse
HEAD` + `git -C <author> status --porcelain` (record both), then same two
AFTER the envelope completes (must be unchanged — HEAD `84a2dae`, clean).

## Permitted writes (CLOSED list — nothing else)

Inside the execution worktree ONLY: gate-generated temporary M1–M6 mutant
file states + their `checkout --` restorations (hash-verified by the gate
itself), and necessary ignored build outputs (`dist-newstyle/` and
equivalents — the build's own scratch, never evidence). Everything else —
sources, configs, other worktrees, evidence outside your dir — is READ-ONLY.
A write outside this list is a SCOPE-FAIL: stop, journal it, BLOCK.

## Ledger bindings (as requested — exact paths, no invented history)

- Campaign ledger: ticket `STATUS.md` spend lines (all `SPEND`/`spend`/
  `COUNTERS` NOTEs — the running account) + `handoffs/S28-1-PACKET-CLOSE.md`
  spend-FINAL section, both under `/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/`.
- Ceiling history: 8 = initial brief mandate (`brief.md` §Budgets);
  16 = `inbox/answers/A-001-budget-and-splice-defects.md` (budget-gap
  ruling); 25 = `inbox/NOTE-010-full-regate-25.md` (full re-gate ruling);
  34 = `inbox/NOTE-012-q007-m2repair-34.md` (final re-gate ruling). All
  files present; no other raises exist.
- Exact selection set: auditor family ∈ {codex, grok} per operator
  restriction (as stated in NOTE-015 — recorded here as epic-stated
  authority, not independently sourced); THIS seat Codex `gpt-6-astra/high`
  (argv + config + TUI banner triangulated pre-dispatch); EXCLUDED:
  claude, glm, muse, and every other family — never enlarge via helper
  defaults (any reseat re-derives explicitly or BLOCKs).
- Caps: auditor 12 substantive / 24 targeted (reconciled: cold leg-3 1B +
  leg-4 1B + leg-5 M1–M6 6B + leg-6 1B = 9B envelope + ≤3 discretionary
  whole-project spot runs + probes ~0–2 narrowed-stated); owner 34/34
  retained SPENT (RED 4 + v6-GREEN 9 + v7-GREEN 9 + v8-GREEN 9 + SLIM-final
  3; ticket-owner preparation 0 builds + 6 probe-efforts/9 invocations +
  enumerated charge-0 classes).

## Caps + scope restated (no surprises)

No new candidate, gate rewrite, cap reset, or extra building attempt is
authorized by this answer. Window-name note (acknowledged, no authority
impact): live window is `kelgroups`; the brief's longer name is stale.
Spend for this question + answer + envelope setup: 0/0 both sides (reads,
writes, hashes, one `git worktree add` setup — no compilation).
