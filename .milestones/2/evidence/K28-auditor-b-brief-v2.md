# Brief — commit auditor `t28-app-api/commit-auditor-s28b` (kelgroups #28 S28-1, fresh FULL audit)

Role: commit auditor (independent read-only compliance inspection, ONE
submission, then exit). Worker ID: `commit-auditor-s28b`. Parent scope:
ticket owner `t28-app-api` (Muse pane `%534`, `reactivegas:8`), runtime root
`/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/`.
Your runtime root: `.../t28-app-api/commit-auditor-s28b/` (`brief.md` = this
file, `STATUS.md`, `questions/`, `answers/`, `inbox/`, `handoffs/`).
Execution worktree (disposable, exact candidate, DETACHED — the ONLY tree
you execute in): `.../commit-auditor-s28b/exec-84a2dae/` at `84a2daea1db81b1baf73c73a7874dcd68ce9f4b2`
(verified clean at binding). Reference checkout (IMMUTABLE, read/compare
only): `/code/kelgroups-audit-84a2dae-b` at `84a2daea1db81b1baf73c73a7874dcd68ce9f4b2`. Author checkout
`/code/kelgroups-issue-28`: UNTOUCHED, never executed (no builds, no gate
runs, no reads required — leave it alone entirely). No children ever.
`draft=NONE`. This is a FRESH context: a prior seat
(`commit-auditor-s28`, archived) terminalized CONTRACT-BLOCKED pre-execution
(report `93a16836d1d60e78d4a3c693fde14247a9a98ba88c8941c0e5109a10c5cdb6fe`, 0/0 spend, candidate unexamined — preserved, never
reopened); you inherit NOTHING except this brief and the bindings below.
Family/seat: Codex via `codex --dangerously-bypass-approvals-and-sandbox
-m gpt-6-astra -c model_reasoning_effort=high -C
/code/kelgroups-audit-84a2dae-b` (vehicle note: ordered argv named
`codex-raw`, which resolves to no binary system-wide — verified four ways;
the installed `codex` CLI 0.153.2 with IDENTICAL flags is the same family/
backend, and `-m gpt-6-astra` (live-precedented same binary/version) makes
the mandated model argv-visible for ps verification; config
`~/.codex/config.toml` corroborates `model = "gpt-6-astra"` +
`model_reasoning_effort = "high"`, read pre-dispatch. Same family, same
flags, same effort — NOT a substitute family. Allowed families for auditor
seats on this lane: {codex, grok} only (operator restriction, per NOTE-015);
EXCLUDED: claude, glm, muse, all others — never enlarge via helper
defaults.) Report `cli=codex model=gpt-6-astra effort=high` in START;
parent verifies argv via `ps` before admitting anything.
Trust note: repo root `/code/kelgroups` is trust-listed in codex config
(pre-existing + lane entry verified pre-dispatch), so no trust prompt is
expected. If ANY trust/approval prompt appears anyway: STOP, touch nothing,
BLOCKED with its exact text (fail-closed; never click through).
You are not alone in the codebase; do not revert edits made by others.
Required skill load chain (in order): `commit-auditor`, `auditor`,
`worker-protocol`, `verification`, `invariants`, `gate-script`, `haskell`,
`nix` (+ `lean4` read-only ONLY to check Lean-conformance claims against
`/code/reactivegas/lean/KelGroups/`, never to edit).

## Absolute prohibitions

NO edits to any candidate tree (read-only; builds may write ONLY what the Permitted writes section allows — ignored build outputs inside your execution worktree). NO commits, NO push. NO
contact with the commit owner (sibling — never speak to it, never edit for
it). NO acceptance decision (you RECOMMEND with evidence; the ticket owner
decides). After ONE terminal compact report you STOP and exit (root archived
by parent). A `glm`/`muse` seat may NEVER audit — you are Codex, correct
family.

## Precedence binding (desk-granted, part of this mandate — read twice)

The generic commit-auditor two-raise termination rule is OVERRIDDEN for this
seat by specific desk grants under inherited milestone authority. The count
is kept OPENLY — three owner-funding decisions, nothing renamed or reset:
8 (initial brief mandate `brief.md` §Budgets) → 16 (ticket
`answers/A-001-budget-and-splice-defects.md` — the `inbox/answers/` prefix
some records carry is a typo; the file lives at ticket `answers/`) → 25
(`inbox/NOTE-010-full-regate-25.md`) → 34
(`inbox/NOTE-012-q007-m2repair-34.md`), with the auditor envelope itself
raised 5→12 substantive / 20→24 targeted attached to the first decision
(initial brief §Budgets vs the A-001 budget answer — both verified
pre-dispatch). This exception covers the ENTIRE authorized history —
including both auditor-limit increases — limited to completing this ONE
full independent audit at existing 12/24. It is NOT a fourth raise, new
submission, refund, scope reduction, or result. All original rows remain
binding. Owner 34/34 retained (spent: RED 4 + v6/v7/v8 GREEN 9×3 + SLIM 3;
no extra owner execution authorized). NO later raise is authorized. The
prior seat's diagnosis (missing precedence binding, CB-001) is preserved quoted in answers/A-002-commissioning-addendum.md (addendum hash in ticket STATUS) — answered, not erased. The auditor envelope itself was raised 5→12 substantive / 20→24 targeted attached to the first funding decision (initial brief §Budgets vs the A-001 budget answer — both verified pre-dispatch; desk artifacts/NOTE-014-kelgroups-q007-final-instrument-pass.md read in full this turn). You independently verify scope, authority, and adequacy, including whether this complete mandate is establishable as specified; you may return a NEW concrete contradiction with exact evidence, and you own your verdict. (You need not invent a new operator ruling for the express exception above, but nothing here forbids you from questioning it.) Then EXECUTE what is established.

## Mandate (complete subject — nothing inherited as PASS)

- ENTIRE candidate `84a2daea1db81b1baf73c73a7874dcd68ce9f4b2` against base `368b596fef0b6d393c2ac7afc631d236c55d86d1`, INCLUDING the RED-baseline changes (RED `570fe4a68f510fad3c9912ea59c1e492f3e11740` is contextual evidence for the absence baseline, not an accepted base hiding earlier changes — verify the fence over the full BASE→candidate diff): the full diff, test-only demo + spec and
  production boundary.
- Ticket contract r5 (`.../t28-app-api/handoffs/S28-1-CONTRACT-r5.md`) +
  addenda R5-ADDENDUM-Q004-D3CLASS / -Q005-M5ONLY / -Q006-GATESIDE /
  -Q007-M2IMPORT + fence amendment E1E2 + packet-close + corrigenda (all
  under `.../t28-app-api/handoffs/`, hashes in ticket STATUS freeze notes).
- All six mutation rows with frozen witnesses + can-fail mutants; residual
  risks (RELIANCE-5 rows + packet residuals) are audit INPUT, not
  conclusions — re-derive every verdict yourself.
- Frozen gate v8 (YOUR copy at `.../exec-84a2dae/gate.sh`, full sha256
  `7a7a99e351df0a34a8147804f68bc85a3f182556cbfafccaf62e46105743c30a`,
  blank-normalized `f5796d1e5524f57c98b7d92168ad4fc10b22b6d318c57e0c989a1bf02f1b3650`,
  version `G28-1 v8 (r5-Q007-addendum)`, FROZEN_BASE RED `570fe4a68f510fad3c9912ea59c1e492f3e11740`):
  execute the COMPLETE envelope legs 1–7 with YOUR OWN hand on YOUR
  execution tree (cold first build expected), evidence to YOUR
  `handoffs/evidence/` via `G28_EVIDENCE_DIR` (set it before leg-3; the
  gate default points at the author root — never use it). NO inherited
  PASS rows by declaration.
- Fences: owned surface (six lib files + E1×4 test sites + Server/JSON
  JSON-only + demo/spec/cabal/Main + Generators) vs forbidden (client UI,
  lean semantics, Trivial behavior, historical semantics beyond suites,
  publication/merge). Verify the fence by diffing RED..candidate yourself.
- Failure-mode coverage (standing duty, survives any scoping): which
  failure modes did this change alter, and are they still observable?
  (resource-acquisition failures, exceptions moved into unwatched threads,
  swapped synchronisation primitives, lost degradation paths, STM/SQLite
  ordering, JSON backward/forward compatibility). Answer explicitly.
- Test, value and failure-mode coverage are your standing duty: use
  property-based instrumentation and mutation harnesses of YOUR OWN where a
  hand-rolled mutant can't reach.

## Pre-first-invocation proof procedure (journal ALL of it BEFORE leg-3)

Quoted `pwd` (must be the exec root), `ls` of the exec root (must show YOUR
gate.sh copy), `echo $G28_EVIDENCE_DIR` (must be YOUR evidence dir),
`git -C <author> rev-parse HEAD` + `git -C <author> status --porcelain`
(record both — author tree must stay `84a2dae`/clean throughout; re-check
after the envelope too). Preconditions + splice cardinalities per the gate
(fail closed, never skip).

## Permitted writes (ONE coherent fence: general rule + expressly-scoped exception)

IMMUTABLE (never written by you — verify cleanliness instead): the author
checkout `/code/kelgroups-issue-28` and the reference checkout
`/code/kelgroups-audit-84a2dae-b` (record `rev-parse` + `status --porcelain`
for both before the first build and after the envelope completes).
PERMITTED inside the execution worktree ONLY: gate-generated temporary
M1–M6 mutant file states + their `checkout --` restorations (hash-verified
by the gate), and necessary ignored build outputs (`dist-newstyle/` and
equivalents).
PERMITTED, exactly these paths outside it, for THIS SINGLE envelope only
(see Temp-path envelope below for the complete conditions): `/tmp/g28m3.hs`,
`/tmp/g28m4.hs`, `/tmp/g28m5.hs` (auditor-reserved regular files you create
exclusively pre-build; the gate overwrites them and moves them into the
execution tree as frozen) and bounded `mktemp` stderr outputs under your
`$TMPDIR`-override directory (created fresh inside your runtime; propagation
verified pre-build).
PERMITTED: runtime evidence under your `handoffs/evidence/` via
`G28_EVIDENCE_DIR` (the gate default pointing at the author root is never
used).
A write outside this section is a SCOPE-FAIL: stop, journal it, BLOCK.

## Verification commands (exact set; journal every invocation with exit/cause)

Inside `nix develop .#ci --quiet -c` from your EXECUTION root, the gate v8
legs in order + your own read-through of the complete diff + your own
targeted probes (narrowed `--match`/single-module only, stated scope).
Evidence: complete logs under your `handoffs/` + one-line exit/duration/
hash/size receipt per command. Open raw logs on failure, contradiction, or
targeted sampling — but drill into at least one passing row's raw evidence
to prove you looked past the summary.

## Budgets (binding, reconciled pre-dispatch)

12 substantive invocations / 24 targeted executions TOTAL: cold leg-3 (1B)
+ leg-4 full test (1B) + leg-5 M1–M6 (6B) + leg-6 full `just ci` (1B) = 9B
envelope + ≤3 discretionary (spot re-proofs/instrumentation, whole-project
each) + probes ~0–2 (narrowed only, stated scope). Reads/greps/hashes cost
nothing. No automatic raises — return a concrete workload/cost gap BEFORE
exceeding. No parallel heavy builds.

## Report (terminal, compact, hash-bound)

ONE report: per-invariant verdicts (six rows + fence/integrity/registration
checks, each pass/findings/blocked with exact evidence pointers file:line +
command receipts); compact blocking findings (each naming the property class
it generalizes to); finding count; finding-to-evidence hashes; report
sha256. Write it to `handoffs/` + append ONE `COMPLETE` line + `AUDIT-RESULT`
summary in YOUR STATUS, then STOP (write-idle, exit; parent archives root).

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

Brief sha256 (THIS file, version 2 — v1 bytes preserved at ticket handoffs/auditor-brief-v1-superseded.md, sha cbcb191ec8dcc33ed05b885cee3a08c2af1268df71025288ea32900f750c3bec): re-read IN FULL (do not rely on v1 memory); quote THIS version's sha in START (re-hash at START time). Gate v8: full raw sha256 `7a7a99e351df0a34a8147804f68bc85a3f182556cbfafccaf62e46105743c30a` and blank-normalized self-hash `f5796d1e5524f57c98b7d92168ad4fc10b22b6d318c57e0c989a1bf02f1b3650` — verify BOTH by byte-hash equality (recompute each and compare character-for-character; no abbreviated substitutes, no eye-matching of prefixes); mismatch is a BLOCKED question, never a silent fix.

## Version record (v2 — read this on every re-read)

- VERSION 2. Brief v1 bytes (sha cbcb191ec8dcc33ed05b885cee3a08c2af1268df71025288ea32900f750c3bec, acknowledged at your ACK) are preserved byte-identical at ticket `handoffs/auditor-brief-v1-superseded.md` and are EVIDENCE ONLY, never instruction.
- Changes v1→v2 (ONLY these hunks differ — verify with `diff` against the preserved copy): (1) build-writes absolute now points at the unified fence; (2) precedence section: auditor independence restored + CB-001 quoted below + auditor-limit history added; (3) fence-verification scope BASE→candidate including RED baseline + every abbreviated SHA expanded to full; (4) footer requires byte-hash verification; (5) this record + Temp-path envelope section added. Authority: NOTE-018 + NOTE-019 + NOTE-020 (desk/epic, binding).
- CB-001 preserved (quoted verbatim from archived `commit-auditor-s28/handoffs/AUDIT-REPORT.md`): the supplied execution answer resolves the write fence but binds the same campaign's 34 ceiling; three ceiling grants 8→16→25→34 with no binding authorizing reliance; generic two-raise rule requires `AUDIT-CONTRACT-BLOCKED reason=third-ceiling-grant`, complete, exit without auditing. Property class: commissioning must carry the full ceiling history. Answered by the Precedence binding above + answers/A-002-commissioning-addendum.md — not erased.
- Path `...` abbreviations in this file are readability shorthands for the fixed runtime root defined above, never measurements; every SHA constraining execution is full-length above.

## Temp-path envelope (authorized for THIS SINGLE audit envelope — NOTE-019)

The three frozen staging paths the gate writes outside any checkout — `/tmp/g28m3.hs`, `/tmp/g28m4.hs`, `/tmp/g28m5.hs` — plus bounded `mktemp` stderr outputs, are AUTHORIZED under exactly these conditions (same D3-class mechanical bounds as the M-splices; no gate rewrite, no budget or candidate change):
- `lstat` BEFORE first use: `ls -la` + `test -L` each path, journal type/uid/inode-or-absent. A PRE-EXISTING file or symlink at any of the three is a CONCRETE BLOCKER: stop, journal it, BLOCKED — never overwrite, delete, or follow it silently.
- If absent: reserve each with EXCLUSIVE creation owned by this execution (`set -o noclobber` + `: >` the path fails closed if present); record type/uid/inode. Any collision fails BEFORE compilation.
- TMPDIR: create a NEW directory inside YOUR runtime (plain `mkdir`, never `-p` over an existing dir — pre-existence fails closed); `export TMPDIR` to it for the gate invocation; VERIFY propagation pre-build (`TMPDIR=<dir> mktemp -u` must print a path under it — dry-run, creates nothing); after leg-2b `ls` the dir (e1/e2 must be there, not /tmp). Record the override as envelope configuration. Gate bytes, cwd, and evidence dir are unchanged by this.
- Process table + owner park re-checked AT execution time: no concurrent k28 gate may use those names (bounded now-observation, not a universal proof); ticket owner runs no gates concurrently (stated in the commissioning answers; verify ticket STATUS shows no active gate).
- The gate may overwrite ONLY your reserved regular files (identity-checked against reservation records) and move them into the isolated execution tree exactly as frozen. Preserve generated mutants, intended failure receipts, and byte-exact restoration.
- On exit: retain all evidence; verify source restoration + author/reference cleanliness; remove ONLY this audit's residual reserved temp files whose identity still matches reservation records — never an unexplained replacement; any leftover or collision is journaled honestly, never cleaned broadly (`/tmp` at large is not yours; no unrelated-file reads).
- These controls are discretionary bounds, not an OS sandbox — do not claim one.
