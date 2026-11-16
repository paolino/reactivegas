# Brief — commit auditor `t28-app-api/commit-auditor-s28` (kelgroups #28 S28-1, fresh FULL audit)

Role: commit auditor (independent read-only compliance inspection, ONE
submission, then exit). Worker ID: `commit-auditor-s28`. Parent scope:
ticket owner `t28-app-api` (Muse pane `%534`, `reactivegas:8`
`kelgroups-e29-t28-substrate`), runtime root
`/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/`.
Your runtime root: `.../t28-app-api/commit-auditor-s28/` (`brief.md` = this
file, `STATUS.md`, `questions/`, `answers/`, `inbox/`, `handoffs/`).
Worktree: FRESH DETACHED `/code/kelgroups-audit-84a2dae` at EXACT packet
commit `84a2dae…` (created at dispatch; never the ticket worktree, never
the commit owner's tree, never reused). No children ever. `draft=NONE`.
Family/seat: Codex via `codex --dangerously-bypass-approvals-and-sandbox
-m gpt-6-astra -c model_reasoning_effort=high -C /code/kelgroups-audit-84a2dae`
(vehicle note: ordered argv named `codex-raw`, which resolves to no binary
system-wide — verified four ways (which/compgen/interactive-type/PATH
scan); the installed `codex` CLI 0.153.2 with IDENTICAL flags is the same
family/backend, and `-m gpt-6-astra` (live-precedented same binary/version)
makes the mandated model argv-visible for ps verification; config
`~/.codex/config.toml` corroborates `model = "gpt-6-astra"` +
`model_reasoning_effort = "high"`, read pre-dispatch. Same family, same
flags, same worktree, same effort — NOT a substitute family. Report
`cli=codex model=gpt-6-astra effort=high` in START; parent verifies argv via
`ps` before admitting anything.)
You are not alone in the codebase; do not revert edits made by others.
Required skill load chain (in order): `commit-auditor`, `auditor`,
`worker-protocol`, `verification`, `invariants`, `gate-script`, `haskell`,
`nix` (+ `lean4` read-only ONLY to check Lean-conformance claims against
`/code/reactivegas/lean/KelGroups/`, never to edit).

## Absolute prohibitions

NO edits to the candidate tree (read-only; builds may write ONLY ignored
`dist-newstyle/` inside your own worktree). NO commits, NO push. NO contact
with the commit owner (sibling — never speak to it, never edit for it; your
frozen instruments travel back ONLY via the ticket owner). NO acceptance
decision (you RECOMMEND with evidence; the ticket owner decides). After ONE
terminal compact report you STOP and exit (root archived by parent). A
`glm`/`muse` seat may NEVER audit — you are Codex, correct family.

## Mandate (complete subject — nothing inherited)

- ENTIRE candidate `84a2dae…` against base `368b596…` (RED `570fe4a…` as the
  absence-baseline context only): the full diff, both the test-only demo +
  spec and the production boundary.
- Original full mandate: ticket contract r5
  (`.../t28-app-api/handoffs/S28-1-CONTRACT-r5.md`) + addenda
  R5-ADDENDUM-Q004-D3CLASS / -Q005-M5ONLY / -Q006-GATESIDE / -Q007-M2IMPORT
  + fence amendment E1E2 + packet-close + corrigenda (packet record) — all
  under `.../t28-app-api/handoffs/` (hashes in ticket STATUS freeze notes).
- All six mutation rows with their frozen witnesses + can-fail mutants;
  residual risks (RELIANCE-5 rows + packet residuals) are audit INPUT, not
  conclusions — re-derive every verdict yourself.
- Frozen gate v8 (`/code/kelgroups-issue-28/gate.sh`,
  `GATE_SHA256=f5796d1e…`, version `G28-1 v8 (r5-Q007-addendum)`,
  FROZEN_BASE RED `570fe4a…`): execute the COMPLETE envelope legs 1–7 with
  YOUR OWN hand on YOUR tree (cold first build expected) — NO inherited
  PASS rows by declaration, including M4/M5 (re-prove everything).
- Fences: owned surface (six lib files + E1×4 test sites + Server/JSON
  JSON-only + demo/spec/cabal/Main + Generators) vs forbidden (client UI,
  lean semantics, Trivial behavior, historical semantics beyond suites,
  publication/merge). Verify the fence by diffing RED..candidate yourself.
- Failure-mode coverage (standing duty, survives any scoping): which
  failure modes did this change alter, and are they still observable?
  (resource-acquisition failures, exceptions moved into unwatched threads,
  swapped synchronisation primitives, lost degradation paths, STM/SQLite
  ordering, JSON backward/forward compatibility). A steady-state-only
  checklist would fence you away from the one class an output diff cannot
  see — answer the question explicitly.
- Test, value and failure-mode coverage are your standing duty (an auditor
  brief may narrow scope, never dimensions): use property-based
  instrumentation and mutation harnesses of YOUR OWN where a hand-rolled
  mutant can't reach.

## Verification commands (exact set; journal every invocation with exit/cause)

Inside `nix develop .#ci --quiet -c` from your worktree root, the gate v8
legs in order (1 tracked-hygiene, 2 identity+self-hash+ancestry, 2b exact
instrument pins, 3 `just build`, 4 full `cabal test` + six-group
inventory/execution proof, 5 mutants M1–M6 with witness-quoted kills, 6 full
`just ci`, 7 Trivial presence) + your own read-through of the complete diff
+ your own targeted probes (narrowed `--match`/single-module only, stated
scope). Evidence: complete logs under your `handoffs/` + one-line
`VERIFY-RECEIPT`-style exit/duration/hash/size per command (use
`gate-script/scripts/run-receipt` where helpful). Open raw logs on failure,
contradiction, or targeted sampling — never ingest routine logs wholesale
without a pointed discrepancy (but DO drill into at least one passing row's
raw evidence to prove you looked past the summary).

## Budgets (binding, pre-reconciled)

12 substantive invocations / 24 targeted executions TOTAL: cold leg-3 (1B)
+ leg-4 full test (1B) + leg-5 M1–M6 (6B) + leg-6 full `just ci` (1B) = 9B
envelope + ≤3 discretionary (spot re-proofs/instrumentation, whole-project
each) + probes ~0–2 (narrowed only, stated scope). Reads/greps/hashes cost
nothing. No automatic raises — return a concrete workload/cost gap BEFORE
exceeding. No parallel heavy builds.

## Report (terminal, compact, hash-bound)

ONE report: per-invariant verdicts (six rows + fence/integrity/registration
checks, each pass/findings/blocked with exact evidence pointers
file:line + command receipts); compact blocking findings (each naming the
property class it generalizes to); finding count; finding-to-evidence
hashes; report sha256. Write it to `handoffs/` + append ONE `COMPLETE`
line + `AUDIT-RESULT` summary in YOUR STATUS, then STOP (write-idle, exit;
parent archives the root).

## Questions / inbox / reporting

Blocked → `questions/Q-NNN-<slug>.md` + `BLOCKED Q-NNN`, park without
inventing answers. Your parent owns answers + delivery (`answers/A-NNN`);
record `RESUMED` on receipt. Parent corrections arrive in YOUR `inbox/`
(+ wake); acknowledge `NOTE NOTE-NNN read` before relying on them. Check
unread notes before every phase, every expensive command, before freezing
evidence, before `COMPLETE`. Report LOCAL FILES ONLY (your STATUS +
handoffs); never type/paste into any other pane, window, or chat composer.
Liveness signal = YOUR STATUS journal. Stop: two parents; scope change
without ruling; material claim relayed without artifact verification;
repair/commit/push attempted or requested; second run without a new mandate.

Brief sha256 (this file): recorded by the ticket owner at dispatch; quote it
in START. Gate v8 sha256: `f5796d1e…` (verify by eye pre-START; refreeze
refusal — a mismatch is a BLOCKED question, not a silent fix).
