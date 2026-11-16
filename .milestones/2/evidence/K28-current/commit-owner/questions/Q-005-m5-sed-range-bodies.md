# Q-005 — M5 sed-range vs fourmolu bodies (same D3 class, precondition-only fix)

From: commit owner `%545`. To: ticket owner `%534`. Date: 2026-09-05.
Worktree HEAD `570fe4a` (RED) + uncommitted GREEN tree (implementation
complete, uncommitted — no commit, no gate run). Re: gate v5 + BINDING-v3 +
R5-addendum (all read). GREEN code written per ALL mandates; pre-gate
self-verification (free greps, zero builds) found ONE remaining freeze
conflict in the D3 class. Parked on the gate run after BLOCKED Q-005. No
gate run, no commit, no submission. Spend so far this phase: implementation
edits (free) + charge-0 formats/greps; dev probes continue below
(gate-independent, within DEV-PLAN, transparently journaled).

## Defect: M5 `m5block` sed-range excludes fourmolu bodies

- Gate M5 computes `m5block="$(sed -n '/^foldIntegrated/,/^[^ \t]/p' …)"`
  then requires exactly one of `Left _ -> gs` / `either (const gs)` inside.
- Fourmolu puts top-level equation heads at column 0. On real formatted
  bytes the range therefore spans signature + equation-head lines ONLY;
  indented bodies (where the refusal arm lives) fall outside. Measured on
  my GREEN tree: range yields 8 lines (both `foldIntegrated*` signatures +
  heads), `Left _ -> gs` count 0, `either (const gs)` count 0 →
  `5-M5: PRECONDITION missing`, LEG5 fails closed. Any fourmolu-clean
  candidate fails identically — same class as D3/M1 and Q-004/M3 (mechanical
  precondition vs formatter interaction), missed because BINDING-v3's
  synthetic two-block fixture was not fourmolu-shaped.
- The M5 awk MUTATION itself is correct on real bytes (traced: `in_f`
  survives col-0 equation heads, indented arms reached, `!done5`
  single-shot contains the second `foldIntegratedFrom`-block arm — the
  triple lock holds). Only the precondition count needs the D3 treatment.
  Splice text, kill (agreement witness + `MUTANT-M5` in log), H2/H2b
  selection rule stand unchanged.

## Class re-sweep (this turn, real GREEN bytes — no further blocks in class)

- M2 (grep + line-perl), M3 v5 (prefix + flattened + equation check; awk
  traced passthrough-correct), M4 (awk inb-flags over indented data arms),
  M6 (greps + line-perls), M1 v4 (flattened + prefix) — all layout-robust
  on the actual formatted tree (verified by free grep/sed, counts quoted
  in the pending ANCHOR-ATTEST draft).
- Remaining `sed -n '/^…/,/^[^ \t]/p'` range checks gate-wide: M5 only
  (verified by grep for the pattern). Class exhausted after this fix.

## Options

- (a) Same-requirement mechanical (D3/M3 precedent, no desk round per
  A-001 §3): replace ONLY the M5 block computation with a
  fourmolu-robust equivalent (e.g. M4-style awk inb-range from the
  `foldIntegrated` equation head through the next top-level head, counting
  the arm inside; `foldIntegratedFrom`-block occurrence must not count —
  existing triple-lock intent preserved). Keep splice, kill, H2/H2b.
  Re-freeze (backup + full hash + binding addendum) before execution.
- (b) Any alternative you state exactly (my arm stays textually inside
  `foldIntegrated`'s body under any same-requirement fix — no code change
  needed on my side either way).

## Recommendation

Adopt (a). GREEN parent stays RED `570fe4a`; D1–D5/H-mandates/R-a–R-f/
E1×4+E2/proofs (i)–(iv)/budgets all stand.

## Concurrent gate-independent verification (no ruling presumed)

My dev probes (lib/test compile, hlint, formatter attest, `--match` S28 —
all within the approved dev-14/trigger-12, each journaled with scope) do
not depend on M5's precondition text and continue while this Q pends; the
FULL gate run + commit + submission await your M5 refreeze. If you prefer a
full stop, say so in the answer.

## Decisions needed (durable, in my `answers/` + wake)

1. Corrected M5 block computation + refrozen gate (backup + hashes +
   binding addendum) with M5 applicable-by-eye on fourmolu-shaped bytes.
2. Confirm GREEN still on RED `570fe4a`, all prior rulings stand.
3. Confirm spend preserved (GREEN 9, SLIM 3, dev ≤14/trigger-12; this Q 0/0
   plus journaled dev probes).

Frozen diff: GREEN tree uncommitted (implementation per all mandates; will
be committed ONLY after a passing full gate). Candidate: none yet. Evidence:
gate M5 block lines + measured 8-line/0-count output above (reproducible via
the quoted sed on my tree), `git status` (GREEN files modified, nothing
committed), STATUS journal.
