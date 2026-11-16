# SUBMISSION-2 BINDING HANDOFF — prepared instrument bytes for owner verification

**From:** S4-B commit owner (`muse`, pid/pgid 1493708).
**For:** ticket owner `%503` — verify and BIND before any OT/O run (NOTE-012).
**Candidate:** `59309d668966206df6b01a7e9027614f79e52e5f` (clean tree; prior
`189e1ed` preserved as the audited submission-1 baseline, not amended).
Base `3590c001` (verified `origin/master`, unchanged since resume).
**Spend at handoff:** submissions 1/2 used (this submission 2 of 2 open),
substantive 8/14, targeted 42/60. This handoff spends nothing (reads/writes).

## 1. Repair state (in `59309d6`, reviewed, unexecuted)

- `scripts/check-lean-mirrors`: F01 total kind classification (home → explicit
  kind arms defn/induct/opaque + thm/axiom/ctor/rec exclusions with counted
  named reasons + fail-closed catch-all at both match layers), P01 promotion
  table (`view_mem_of_isMember`, `isMember_of_view_mem`: existence as thmInfo
  mentioning `KelGroups.GroupView.isMember`), extended summary/receipt
  (promoted count, kind census).
- `lean/Reactivegas/Mirrors.lean`: the two helpers promoted `private`→`public`;
  statement/proof bytes otherwise identical (pre-edit bytes snapshotted at
  `instruments/S2-selected-P01-helpers.txt`).
- No justfile/model/guard/statement/Invariants/docs changes; no new theorems.

## 2. Instrument bytes + hashes (all under `instruments/`)

| path | sha256 | lines | content |
|---|---|---|---|
| `S2-mut-opaque.diff` | `159262df…46f8b8` | 20 | O2 world: opaque pred append (Predicates) + orphan Bool mirror append (Mirrors). Two hunks, one application. Covers the F01-specimen row AND the S03-class orphan re-run in one run (see §4 for the reach argument). |
| `S2-mut-classifier-omit.diff` | `e16b035b…1ecd28` | 11 | O3 world: deletes the `\| .inductInfo _` alternative from the repaired classifier's or-pattern (single line). Owned inducts fall to fail-closed. |
| `S2-mut-isMember-false.diff` | `85e250ac…61ecb6` | 13 | O5 world: `(lookupMember key view).isSome` → `false` (exact audited shape). |
| `S2-mut-close-perm.diff` | `95edbfa3…0616dc` | 19 | O4 world: `&& col.permitted &&` → `&& true &&` (exact audited atom) + appended print-only `#eval` of the unpermitted-close `isSome` (masking-free: prints, never fails; visibility precedent: 50 `info:` lines in submission-1 build logs; actual presence reported honestly at execution). |
| `S2-chain-P07.lean` | `757bd4e6…f917f8` | 231 | OT1: hermetic P07 chain (miniature env documented in-file; target statements/proofs byte-identical; P07-orig included as proving contrast). Expects exit 1 at `step_close_inv`. |
| `S2-chain-P01.lean` | `f69a9003…fcb8a8` | 120 | OT2: hermetic P01 chain (miniature env; byte-identical helper statements/proofs; P01-orig proving contrast). Expects exit 1 at both helpers. |
| `S2-witness-close.lean` | `531eb3e9…503de6` | 33 | OT3: clean-tree witnesses (exact pullCollection binding, authorized-succeeds, result-empties, unpermitted-refused, contrast pair), all `by decide`. Expects exit 0. |
| `S2-census.lean` | `a04745d8…43106b` | 99 | OT4 (path chosen here): old-rule vs new-rule census + assertions (both views found==24, unclassified==0, opaque-pred==0 clean, thm-excluded nonzero). Mechanically mirrors the repaired classifier; O1 cross-checks agreement. Expects exit 0. |

Selected-bytes snapshots: `S2-selected-{isMember-body,closePurchase-arm,P01-helpers,P07-chain,P01P07-orig}.txt` (hashed at journal time).
All four `.diff`s validate with `git apply --check` against the current tree
(verified this handoff); each restores via `git checkout --` (all target files
tracked post-commit).

## 3. Exact commands, cwd, search-path (for binding)

- OT1–OT4: cwd=`/code/reactivegas-66-s4b/lean`,
  argv=`nix develop --quiet -c lake env lean <ABS-DRIVER-PATH>`
  (lake env supplies LEAN_PATH from local `.lake`; hermetic OT1/OT2 need only
  the toolchain sysroot; OT3 needs Step/State/Types oleans, valid — sources
  unchanged since the last full build; OT4 needs roots+Mirrors oleans —
  kind-identical pre/post promotion, sound for census).
- O1: `rm -rf lean/.lake` (free, file op) then cwd=`/code/reactivegas-66-s4b`,
  argv=`nix develop --quiet -c just lean`. Mutated deps: none (clean tree).
- O2: `git apply instruments/S2-mut-opaque.diff` (cwd=root), then O1 argv.
  Mutated deps: Predicates (+opaque pred), Mirrors (+orphan mirror). Restore:
  `git checkout -- lean/Reactivegas/Predicates.lean lean/Reactivegas/Mirrors.lean` + verify.
- O3: `git apply instruments/S2-mut-classifier-omit.diff`, then O1 argv.
  Mutated deps: checker script only (no lean impact; lean env = O1 build).
  Restore via checkout.
- O4: `git apply instruments/S2-mut-close-perm.diff`, then O1 argv. Mutated
  deps: Step. Restore via checkout.
- O5: `git apply instruments/S2-mut-isMember-false.diff`, then O1 argv.
  Mutated deps: Types. Restore via checkout. Expected honest collaterals
  (solvent/insolvent/canClose via the promoted helpers) reported alongside the
  named targets, never presented as the targets.
- O6: `rm -rf lean/.lake`, then `nix develop --quiet -c just ci` at the final
  SHA; raw log hash recorded.
- Receipts: `handoffs/evidence/S2-O{1..6}.log`, `S2-OT{1..4}.log` (one per row).

## 4. Coverage mapping for shared runs (show, don't assert)

- O2 carries three diagnostics (uncovered-opaque, orphan-mirror, CHECK-FAILED +
  receipt absent). Reach argument: the checker collects every diagnostic before
  failing (no early exit; verified in code paths §1/§5/§6 of the driver); lake
  build succeeds in the O2 world (both staged defs compile), so the checker
  runs to completion. The O2 receipt will either show all three lines or the
  row is reported OPEN. Fallback if rejected: separate orphan run = +1
  substantive gap (stated now, not later).
- O4 carries chain failure + print witness. Reach argument: Step elaborates
  before Invariants (dependency order); print is elaboration-side-effect-only.
  Actual print presence reported honestly at execution; the chain failure does
  not depend on it.

## 5. Fit statement

6 substantive (O1–O6) + 4 targeted (OT1–OT4) = the grant exactly; zero slack.
Any unexpected red, extra invocation, or binding-required change returns as an
exact gap BEFORE the affected phase (no compression, no reclassification).
Targeted ceiling 60 respected (42 + 4 = 46 planned). No base movement since
resume (`origin/master` still `3590c001`, verified).
