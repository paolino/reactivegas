# SUBMISSION-3 — S2 AMENDMENT-2 (F-003 discriminator only)

Seat `muse`. Owner `claude-opus-5[1m]` pane `%503`. Record: submissions 2/2
exhausted, then explicit desk extension 2 → 3 (NOTE-007); **no fourth**.
Prior candidate `561347d` (F-001/F-002 closed, not reopened).
**New candidate `b0c2cdb`** (local commit; no push, no PR, no issue edit, no
external artifact). Diff vs `561347d`: exactly `scripts/check-lean-axioms`
(inversion, Row B/C, justfile byte-identical — verified by diff).
Gate versioning: new gate script hashes
`c83ae5647485018e72eef85bd217dfe6ad5202fba224d4e6bb9680dd0f25feb5`
(sha256 of `scripts/check-lean-axioms` at this SHA); supersedes
`cd67ade9bc137f87` at `561347d`, which remains evidence and is not overwritten.
Ceiling raise 2/2 (FINAL) consumed: 11 → 14. Spend: builds **12/14**
(10 + att1 + att2; att3 compiled nothing; att4 no-op — verified zero `Built`
lines in both logs), probes **15/16** Lean elaborations (incl. failed setups;
shell-only checks uncounted). Nothing left to grant; nothing further needed.

Companion: `AXIOM-IDENTITIES.txt` (26 swept modules, 1213 distinct swept
theorem identities, verbatim from the final `just ci`).

The defect (receipted, not re-spent): `B = env ∖ closure(import Lean)` is a
guess about toolchain contents, not provenance — 1707-module closure holds
261 `Std.*` internals yet excludes public `Std.Data.DHashMap`, so a clean
Std-importing root failed `just lean` at `B ∖ S` (audit `03-...`, RED
accepted as receipted).

## Ownership authority (required row 6 — the heart)

**Authority: per-module compiled-artifact resolution through the loader's own
`LEAN_PATH` search order.** For each elaborated module the driver maps the
name to its olean relative path, takes the first `LEAN_PATH` entry whose file
exists, and classifies: hit under the repository root (outside Lake's
`packages/` dependency footprint) = project-built, swept then reconciled;
hit anywhere else (toolchain lib, dependency checkouts) = dependency,
excluded; no hit = named finding, exit 1. Shadowing follows loader order
deterministically — first hit wins because that artifact is what elaborated —
with S-reconciliation as backstop (shadowed project module fails via `S ∖ B`,
shadowed name inside the project fails via `B ∖ S` when untracked).
Verified footprint: exactly 2 `LEAN_PATH` entries (project build lib first,
toolchain second); zero dependency packages (`lake-manifest.json` carries
none, `lean/.lake/packages/` absent); the `packages/` carve-out excludes
future dependency checkouts by construction. A directory or module-name
assertion is used nowhere: no prefix list, no name list, no guessed closure.
Fail-closed, each probed live: `REACTIVEGAS_ROOT` unset → named
`ownership authority missing` finding, exit 1 (main requires the variable;
no `.`-default guess remains); shadow-olean ambiguity → first-hit
classification with loud downstream finding (P-h); no-hit → dedicated
finding branch (defense in depth; unreachable for loaded modules since the
loader itself resolved them). `LEAN_PATH`-empty is likewise a named branch;
it is unreachable via `lake env` (always populated — observed 2 entries) and
is documented as defensive.

## Required evidence (commands; temp controls fully reverted, lakefile.lean byte-identical)

1. Registered root importing `Std.Data.DHashMap`, import retained
   (`lean/AuditRootStd.lean: import Std.Data.DHashMap` + clean theorem),
   through `just lean` (attempt 1): exit 0 — direction OK (16), root census
   lines, 165/165, both audits ok, axiom gate tracked=29 walked=29 built=29
   with all three roots swept, 1215 distinct, `Std.Data.DHashMap` absent
   from B, no module outside Reactivegas/KelGroups/AuditRoot* swept.
2. Poisoned equivalent (`axiom auditRootStdForbidden` + using theorem,
   import retained) through `just lean` (attempt 2): exit 1 with everything
   before the axiom gate green and the single dependency finding
   `auditRootStdUsesForbidden: depends on axioms outside the permitted
   standard set: auditRootStdForbidden`.
3. Omission fires both directions: `S ∖ B` via removed-TraceTests driver
   (named, exit 1); truncation via truncated env (walk+built mismatch, 2
   findings); zero-S discovery failure; panic predicate on fixtures.
4. `B ∖ S` on a genuinely project-owned module (attempt-3 tree + probe):
   `AuditRootGhost` (project-built olean) with source withheld, loaded
   transitively via tracked `AuditRootT` under bare elaboration → exit 1,
   SOLE finding `built project modules outside the source discovery:
   AuditRootGhost`, built=28, its clean theorem swept with no axiom finding.
   Through `just lean` the same tree exits 1 EARLIER at `lake build`
   (`Running AuditRootGhost: no such file …`, `bad import`) with zero
   compiles — this is the mandatory-path half of the evidence: Lake
   freshness makes a sourceless module unbuildable, so it can never slip
   into the sweep silently; `B ∖ S`-through-`just-lean` is unstageable by
   construction, and the log proves why.
5. Dependencies not misclassified: att1 receipt greps (0 `Std.*`/toolchain
   identities in B); olean-hit probe resolving project modules to the
   project build entry and Init/Lean/`Std.Data.DHashMap` to the toolchain
   entry. Lake-package-dependency limit, stated honestly: none exist today
   (verified); a dep olean under `packages/` is excluded by the carve-out; a
   dep olean hypothetically sharing the main build dir would sweep-then-fail
   LOUD via `B ∖ S`, never silently.
6. Authority + fail-closed: above, each behavior probed live except the
   unreachable-by-construction no-hit/`LEAN_PATH`-empty branches, which are
   named code paths documented as defensive.
7. Truncation and zero/discovery controls alive under the new discriminator
   (all re-probed this round; T-skip mutant still names its identity).

## Advisory carried, not closed

**CI-T-SHARED-FILTER**: both T derivations share `thmInfo` and B membership —
two views of one inventory, not two independent theorem sources. Skip-both
and T-side B-shrink are demonstrated survivors. Stated in these words as
required; not fixed; the gate is nowhere described as having independent
theorem sources or resisting common-filter omissions.

## Honest remainder

Row-6 `LEAN_PATH`-empty/no-hit branches are named-but-unfired (unreachable
via `lake env`; demonstrated for the reachable ROOT-unset twin). The F-001
round's inside-prefix control language is corrected per NOTE-006 (useful,
fails for other defects; missed the outside boundary — it did not). Stale
`.lake` artifacts from removed controls linger unreferenced (discovery is
git+walk; final run recompiled nothing). Upward transport complied with the
local-only standing rule throughout (packets here, journal in STATUS.md).

## DATED CORRECTION — 2026-09-05 (NOTE-008, desk NOTE-026)

The spend lines above stand as written and are corrected here, not edited
away. The packet claimed builds **12/14**, discounting attempt 3 ("compiled
nothing") and attempt 4 ("no-op"). That discount was wrong: the cap was on
**build/gate attempts including failures**, never on newly compiled objects.
Attempt 3 was the planned mandatory-path invocation (log
`s3-att3-justlean.log`: loud `lake build` failure on the withheld source,
zero compiles) and attempt 4 was the final full `just ci`
(`s3-att4-justlean.log`, exit 0, zero recompiles). Both were invoked; both
count. **Correct cumulative spend: 14/14.** Both raises consumed, nothing
left to grant, no fourth submission. No further build or gate invocation was
begun after this correction.

Exact probe enumeration for the AMENDMENT-2 round (cap 16, Lean
elaborations only; shell-only env/file inspection uncounted): **15/16**.
(1) OleanProbe olean-hit validation; (2) gate-direct run, driver
use-before-def failure; (3) gate-direct rerun, green; (4) shadow-ambiguity
probe, namespace-dir import error (failed setup); (5) shadow-ambiguity
retry with top-level olean, S-minus-B finding; (6) removed-TraceTests
driver v1, construction bug (full copy, exit 0); (7) removed-TraceTests
retry, S-minus-B finding; (8) truncated-env driver v1, untruncated env
(exit 0); (9) truncated-env retry, walk+built mismatch; (10) empty-S
zero-discovery; (11) T-skip mutant, T-mismatch; (12) ROOT-unset run,
unnamed IO exception (failed setup, drove the main restructure);
(13) ROOT-unset retry, named authority-missing finding; (14) gate-direct
sanity after restructure, green; (15) base-closure probe (`Base.lean`,
1707 modules — design input, superseded by olean authority). The S2-era
`Api*`/`TProbe2` probes belong to the prior round's accounting, not this 16.

Digest bindings (three instruments, one nonexistent): `c83ae5647485018e72
eef85bd217dfe6ad5202fba224d4e6bb9680dd0f25feb5` is the sha256 of the
tracked script `scripts/check-lean-axioms` at `b0c2cdb` (reverified from
the frozen object for this correction); `8e1c73fef0539c0c95db…` is the
combined tracked executable contract at `b0c2cdb (`justfile`, the four
scripts `check-lean-axioms`, `check-reactivegas-inversion-coverage`,
`check-trace-coverage-agreement`, `check-lean-toolchain`,
`.github/workflows/ci.yaml`), superseding `cd67ade9bc137f87` at `561347d`,
which remains evidence and is not overwritten. The ignored frozen
acceptance gate `./gate.sh` does not exist in this lane (`.gitignore:9`
reserves the path; no file was ever created), has no digest, and nothing in
this packet implies otherwise. No hash or mandate reset through terminology.
