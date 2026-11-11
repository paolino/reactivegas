# NUMBERED MEASUREMENT REQUEST — frozen executable costing campaign (plan only, no execution)

To commissioner `%503`. Supersedes submission-3 r3 request (7-vs-8 chains, 18-vs-12 arithmetic, deferred diffs/argv/target, `#eval` vs `by decide`, missing replay/batch, cold-after-cycles all repaired here). Dependent costing stays STOPPED until numeric grant. No Lean/build/probe is run by the static author under current authority. No allowance is granted by this file.

Base `3590c0015b84fd58004bf6fb44dd18b107304c48` (detached scratch, read-only source); toolchain `leanprover/lean4:v4.25.0`; check proposition `Reactivegas.checkSweepIdempotent = true := by decide` (fully-qualified, no `#eval`). Timer setup, executable production, compiler elaboration, runtime replay, restore each counted under actual type; none hidden as preparation. Every script below frozen+hashed in `instruments/` (hashes in `measurement-operations.json` + `MANIFEST.sha256`). Historical `18`/`143+1` never granted; no replacement total manufactured beyond the enumerated list below (which itself needs a separate numeric grant).

Prerequisite (exact, verified before each cycle): scratch checkout `<scratch>` of `3590c001…`, detached; before AND after each cycle `git -C <scratch> rev-parse HEAD` prints SHA and `git -C <scratch> status --porcelain` prints empty (except deliberately dirty single path during mutant build, recorded). Candidate worktree `/code/reactivegas-66-s3-repair` never touched. `lake --version` prints 4.25.0; `which lake/lean` + `sha256sum readlink -f` + `lean-toolchain` + `LEAN_PATH` recorded per script (version print alone never pins identity). Unexpected exits/timeouts charged+retained, never retried. `Built` vs `Replayed` loading evidence per log; source byte identity alone never proves absence of cached diagnostics. First-failure isolation: single-mutant build halts at first failing obligation; downstream flips after earlier break need separately authorized isolation (not smuggled).

## 1. M00-COLD — U-COLD — cold baseline (all chains)

- scratch/source/output: `/tmp/reactivegas-ms2-measure-scratch` / `/tmp/reactivegas-ms2-measure-scratch/lean` / `/tmp/reactivegas-ms2-measure-output/m00-cold.stdout`
- base/toolchain: `3590c0015b84fd58004bf6fb44dd18b107304c48` / `leanprover/lean4:v4.25.0`
- executable hashes: Executable identity pinned by preflight in each script: lake --version (4.25.0), lean --version, which lake/lean + sha256sum readlink -f, lean-toolchain file, LEAN_PATH recording. Historical R-BUILD2 lean path /nix/store/4gr3n4nrp0xxgykyyzdxi3xjj2ikn5x1-lean4-4.25.0/bin/lean retained as anchor class, not as current pin.
- input hashes: `{}`; mutation hash: `NONE (clean tree; porcelain 0, .lake absent, zero oleans before; verified, not manufactured)`
- argv: `["lake","build"]`; cwd: `/tmp/reactivegas-ms2-measure-scratch/lean`
- env/LEAN_PATH: `{"LEAN_PATH":"<recorded at runtime; must not name candidate worktree /code/reactivegas-66-s3-repair>"}`; initial cache: .lake absent, zero oleans, porcelain 0 (verified before OP1 per SS0 coldness pattern)
- targets/closure: `["lake build (default libs Reactivegas+KelGroups; per-module times retained)"]` / Full default-target closure (Reactivegas.* + KelGroups.*); TraceTests/CorpusGate/CorpusExport NOT reached by defaults (see separate replay targets)
- timer/exit: date +%s%N before/after in cold.sh -> m00-cold.ms; exit -> m00-cold.exit; oleans list -> m00-cold-oleans.txt; Build completed successfully line counted; timeout: `600s + 15s kill-after (timeout --signal=TERM --kill-after=15s 600s)`
- expected semantic evidence: GREEN with Build completed successfully + per-module times + olean count; cures missing cold log (F-07).
- wrong-reason rejection: A GREEN without per-module times/olean list is not a cold baseline; version print alone does not pin executable identity.
- loading evidence: Per-module Built/Replayed lines in retained log; coldness verified, not manufactured.
- restoration: N/A (baseline; porcelain empty before/after; HEAD 3590c001)
- budget charge (including failure): `1 U-COLD (including failure; unexpected exit charged+retained, never retried)`
- frozen scripts: instruments/cold.sh sha256:3da3aeec767b313171e452bd66568e9d224bf0282f104ae9d59cfc80b7d049bf

## 2. M01-C-VOTEFOLD — U-CHAIN — C-VOTEFOLD single-atom mutant build

- scratch/source/output: `/tmp/reactivegas-ms2-measure-scratch` / `/tmp/reactivegas-ms2-measure-scratch/lean` / `/tmp/reactivegas-ms2-measure-output/m-C-VOTEFOLD.stdout`
- base/toolchain: `3590c0015b84fd58004bf6fb44dd18b107304c48` / `leanprover/lean4:v4.25.0`
- executable hashes: Executable identity pinned by preflight in each script: lake --version (4.25.0), lean --version, which lake/lean + sha256sum readlink -f, lean-toolchain file, LEAN_PATH recording. Historical R-BUILD2 lean path /nix/store/4gr3n4nrp0xxgykyyzdxi3xjj2ikn5x1-lean4-4.25.0/bin/lean retained as anchor class, not as current pin.
- input hashes: `{}`; mutation hash: `79a6757ddb780ee2ba347fbdffdf078bdfb063f95b1accaf49c0e0aff07d8f4c (diffs/C-VOTEFOLD.diff)`
- argv: `["lake","build","KelGroups.Vote.Invariants"]`; cwd: `/tmp/reactivegas-ms2-measure-scratch/lean`
- env/LEAN_PATH: `{"LEAN_PATH":"<recorded; must be scratch, not candidate>"}`; initial cache: Warm (post-M00 cold .lake present); incremental classification established only because cold precedes cycle1 (S-07 defect1 cured; cold after cycles would not establish incremental).
- targets/closure: `["lake build KelGroups.Vote.Invariants"]` / KelGroups.Vote.Fold -> KelGroups.Vote.Invariants (exact, no ...)
- timer/exit: date +%s%N apply (m-C-VOTEFOLD-apply.ms) + build (m-C-VOTEFOLD.ms); exits captured; dirty-status (must be 1 path); applied.diff retained; timeout: `300s + 15s kill-after per build; 600s cold separate`
- expected semantic evidence: Span-bound RED at first failing obligation in KelGroups.Vote.Invariants (see mutant-*.sh EXPECTED-OBSERVABLE); continuation past error does not prove every later check executed (S-05).
- wrong-reason rejection: KelGroups/Invariants.lean:870 replayed warning (different file) must not count; literal :197 header grep misses :209:4 (SS0 L1).
- loading evidence: Built vs Replayed lines in log distinguish fresh vs cached diagnostics; source byte identity alone insufficient (SS0 L6). Changed-definition loading via Built <mutated module> line (e.g., Built Reactivegas.Step).
- restoration: Via restore.sh KelGroups.Vote.Invariants (U-RESTORE, next op); porcelain empty; HEAD 3590c001; restore cleanliness does not prove historical separation (F-07 stands retrospectively, cured prospectively).
- budget charge (including failure): `1 U-CHAIN (including failure; timeouts/failures charged+retained, never silently retried)`
- frozen scripts: instruments/mutant-C-VOTEFOLD.sh sha256:e3e96af4d2a57e907506b8d40582b757ad67ebf9d26a7f4e644e006064a1b205; instruments/diffs/C-VOTEFOLD.diff sha256:79a6757ddb780ee2ba347fbdffdf078bdfb063f95b1accaf49c0e0aff07d8f4c

## 3. M01-C-VOTEFOLDR — U-RESTORE — C-VOTEFOLD restore + matching rebuild

- scratch/source/output: `/tmp/reactivegas-ms2-measure-scratch` / `/tmp/reactivegas-ms2-measure-scratch/lean` / `/tmp/reactivegas-ms2-measure-output/m-C-VOTEFOLD-restore.stdout`
- base/toolchain: `3590c0015b84fd58004bf6fb44dd18b107304c48` / `leanprover/lean4:v4.25.0`
- executable hashes: Executable identity pinned by preflight in each script: lake --version (4.25.0), lean --version, which lake/lean + sha256sum readlink -f, lean-toolchain file, LEAN_PATH recording. Historical R-BUILD2 lean path /nix/store/4gr3n4nrp0xxgykyyzdxi3xjj2ikn5x1-lean4-4.25.0/bin/lean retained as anchor class, not as current pin.
- input hashes: `{}`; mutation hash: `NONE (restore: git checkout -- .)`
- argv: `["git","checkout","--",".","&&","lake","build","KelGroups.Vote.Invariants"]`; cwd: `/tmp/reactivegas-ms2-measure-scratch/lean`
- env/LEAN_PATH: `{"LEAN_PATH":"<recorded>"}`; initial cache: Dirty (1 path) before checkout; clean + warm after
- targets/closure: `["lake build KelGroups.Vote.Invariants to GREEN (R-BUILD3 pattern)"]` / KelGroups.Vote.Fold -> KelGroups.Vote.Invariants
- timer/exit: checkout ms + build ms separately (m-C-VOTEFOLD-restore-checkout.ms + m-C-VOTEFOLD-restore.ms); porcelain retained; timeout: `300s + 15s kill-after`
- expected semantic evidence: GREEN Build completed successfully; porcelain empty; R-BUILD3 pattern (no timings in history, timed here).
- wrong-reason rejection: Restore cleanliness does not cure historical candidate-source fence event or recover lost cold log (S-07 defect7 stands retrospectively).
- loading evidence: N/A (GREEN rebuild)
- restoration: Porcelain empty; HEAD 3590c001; single-cause attribution distinct from filesystem isolation.
- budget charge (including failure): `1 U-RESTORE (including failure)`
- frozen scripts: instruments/restore.sh sha256:9d58c3d984e6866318b06be2abbb1dd800b73a6391e151a44b8747f1a1714785

## 4. M02-C-VOTEVAL — U-CHAIN — C-VOTEVAL single-atom mutant build

- scratch/source/output: `/tmp/reactivegas-ms2-measure-scratch` / `/tmp/reactivegas-ms2-measure-scratch/lean` / `/tmp/reactivegas-ms2-measure-output/m-C-VOTEVAL.stdout`
- base/toolchain: `3590c0015b84fd58004bf6fb44dd18b107304c48` / `leanprover/lean4:v4.25.0`
- executable hashes: Executable identity pinned by preflight in each script: lake --version (4.25.0), lean --version, which lake/lean + sha256sum readlink -f, lean-toolchain file, LEAN_PATH recording. Historical R-BUILD2 lean path /nix/store/4gr3n4nrp0xxgykyyzdxi3xjj2ikn5x1-lean4-4.25.0/bin/lean retained as anchor class, not as current pin.
- input hashes: `{}`; mutation hash: `5b50db65100ec22b9668d04bf574fc7989f679002535d172605e809bd6fc00d5 (diffs/C-VOTEVAL.diff)`
- argv: `["lake","build","KelGroups.Vote.Invariants"]`; cwd: `/tmp/reactivegas-ms2-measure-scratch/lean`
- env/LEAN_PATH: `{"LEAN_PATH":"<recorded; must be scratch, not candidate>"}`; initial cache: Warm (post-M00 cold .lake present); incremental classification established only because cold precedes cycle1 (S-07 defect1 cured; cold after cycles would not establish incremental).
- targets/closure: `["lake build KelGroups.Vote.Invariants"]` / KelGroups.Vote.Validate -> KelGroups.Vote.Fold -> KelGroups.Vote.Invariants (exact, no ...)
- timer/exit: date +%s%N apply (m-C-VOTEVAL-apply.ms) + build (m-C-VOTEVAL.ms); exits captured; dirty-status (must be 1 path); applied.diff retained; timeout: `300s + 15s kill-after per build; 600s cold separate`
- expected semantic evidence: Span-bound RED at first failing obligation in KelGroups.Vote.Invariants (see mutant-*.sh EXPECTED-OBSERVABLE); continuation past error does not prove every later check executed (S-05).
- wrong-reason rejection: Nonzero exit alone is not named RED; broad file-wide search matching unrelated lines is unsound (SS0 L1-2).
- loading evidence: Built vs Replayed lines in log distinguish fresh vs cached diagnostics; source byte identity alone insufficient (SS0 L6). Changed-definition loading via Built <mutated module> line (e.g., Built Reactivegas.Step).
- restoration: Via restore.sh KelGroups.Vote.Invariants (U-RESTORE, next op); porcelain empty; HEAD 3590c001; restore cleanliness does not prove historical separation (F-07 stands retrospectively, cured prospectively).
- budget charge (including failure): `1 U-CHAIN (including failure; timeouts/failures charged+retained, never silently retried)`
- frozen scripts: instruments/mutant-C-VOTEVAL.sh sha256:d158eb7e4b37707ce323ce7384f3650c01b6b6e83d75ab93151f2a32659ea5a6; instruments/diffs/C-VOTEVAL.diff sha256:5b50db65100ec22b9668d04bf574fc7989f679002535d172605e809bd6fc00d5

## 5. M02-C-VOTEVALR — U-RESTORE — C-VOTEVAL restore + matching rebuild

- scratch/source/output: `/tmp/reactivegas-ms2-measure-scratch` / `/tmp/reactivegas-ms2-measure-scratch/lean` / `/tmp/reactivegas-ms2-measure-output/m-C-VOTEVAL-restore.stdout`
- base/toolchain: `3590c0015b84fd58004bf6fb44dd18b107304c48` / `leanprover/lean4:v4.25.0`
- executable hashes: Executable identity pinned by preflight in each script: lake --version (4.25.0), lean --version, which lake/lean + sha256sum readlink -f, lean-toolchain file, LEAN_PATH recording. Historical R-BUILD2 lean path /nix/store/4gr3n4nrp0xxgykyyzdxi3xjj2ikn5x1-lean4-4.25.0/bin/lean retained as anchor class, not as current pin.
- input hashes: `{}`; mutation hash: `NONE (restore: git checkout -- .)`
- argv: `["git","checkout","--",".","&&","lake","build","KelGroups.Vote.Invariants"]`; cwd: `/tmp/reactivegas-ms2-measure-scratch/lean`
- env/LEAN_PATH: `{"LEAN_PATH":"<recorded>"}`; initial cache: Dirty (1 path) before checkout; clean + warm after
- targets/closure: `["lake build KelGroups.Vote.Invariants to GREEN (R-BUILD3 pattern)"]` / KelGroups.Vote.Validate -> KelGroups.Vote.Fold -> KelGroups.Vote.Invariants
- timer/exit: checkout ms + build ms separately (m-C-VOTEVAL-restore-checkout.ms + m-C-VOTEVAL-restore.ms); porcelain retained; timeout: `300s + 15s kill-after`
- expected semantic evidence: GREEN Build completed successfully; porcelain empty; R-BUILD3 pattern (no timings in history, timed here).
- wrong-reason rejection: Restore cleanliness does not cure historical candidate-source fence event or recover lost cold log (S-07 defect7 stands retrospectively).
- loading evidence: N/A (GREEN rebuild)
- restoration: Porcelain empty; HEAD 3590c001; single-cause attribution distinct from filesystem isolation.
- budget charge (including failure): `1 U-RESTORE (including failure)`
- frozen scripts: instruments/restore.sh sha256:9d58c3d984e6866318b06be2abbb1dd800b73a6391e151a44b8747f1a1714785

## 6. M03-C-VOTESTATE — U-CHAIN — C-VOTESTATE single-atom mutant build

- scratch/source/output: `/tmp/reactivegas-ms2-measure-scratch` / `/tmp/reactivegas-ms2-measure-scratch/lean` / `/tmp/reactivegas-ms2-measure-output/m-C-VOTESTATE.stdout`
- base/toolchain: `3590c0015b84fd58004bf6fb44dd18b107304c48` / `leanprover/lean4:v4.25.0`
- executable hashes: Executable identity pinned by preflight in each script: lake --version (4.25.0), lean --version, which lake/lean + sha256sum readlink -f, lean-toolchain file, LEAN_PATH recording. Historical R-BUILD2 lean path /nix/store/4gr3n4nrp0xxgykyyzdxi3xjj2ikn5x1-lean4-4.25.0/bin/lean retained as anchor class, not as current pin.
- input hashes: `{}`; mutation hash: `118396924ab2c733e17ea3ae04d2d15d11df6728e70e76f60ca1de12483953d8 (diffs/C-VOTESTATE.diff)`
- argv: `["lake","build","KelGroups.Vote.Invariants"]`; cwd: `/tmp/reactivegas-ms2-measure-scratch/lean`
- env/LEAN_PATH: `{"LEAN_PATH":"<recorded; must be scratch, not candidate>"}`; initial cache: Warm (post-M00 cold .lake present); incremental classification established only because cold precedes cycle1 (S-07 defect1 cured; cold after cycles would not establish incremental).
- targets/closure: `["lake build KelGroups.Vote.Invariants"]` / KelGroups.Vote.State -> KelGroups.Vote.Validate/Fold -> KelGroups.Vote.Invariants (exact, no ...)
- timer/exit: date +%s%N apply (m-C-VOTESTATE-apply.ms) + build (m-C-VOTESTATE.ms); exits captured; dirty-status (must be 1 path); applied.diff retained; timeout: `300s + 15s kill-after per build; 600s cold separate`
- expected semantic evidence: Boundary witness (assents==required flips positive->open; sweepStep some->none; open_mem iff :418 fails); congruence lemma alone insufficient.
- wrong-reason rejection: Nonzero exit alone is not named RED; broad file-wide search matching unrelated lines is unsound (SS0 L1-2).
- loading evidence: Built vs Replayed lines in log distinguish fresh vs cached diagnostics; source byte identity alone insufficient (SS0 L6). Changed-definition loading via Built <mutated module> line (e.g., Built Reactivegas.Step).
- restoration: Via restore.sh KelGroups.Vote.Invariants (U-RESTORE, next op); porcelain empty; HEAD 3590c001; restore cleanliness does not prove historical separation (F-07 stands retrospectively, cured prospectively).
- budget charge (including failure): `1 U-CHAIN (including failure; timeouts/failures charged+retained, never silently retried)`
- frozen scripts: instruments/mutant-C-VOTESTATE.sh sha256:218eabed4e44f3a10f392f0e8fa13797917026520eee082a714bc33baf3b8bf2; instruments/diffs/C-VOTESTATE.diff sha256:118396924ab2c733e17ea3ae04d2d15d11df6728e70e76f60ca1de12483953d8

## 7. M03-C-VOTESTATER — U-RESTORE — C-VOTESTATE restore + matching rebuild

- scratch/source/output: `/tmp/reactivegas-ms2-measure-scratch` / `/tmp/reactivegas-ms2-measure-scratch/lean` / `/tmp/reactivegas-ms2-measure-output/m-C-VOTESTATE-restore.stdout`
- base/toolchain: `3590c0015b84fd58004bf6fb44dd18b107304c48` / `leanprover/lean4:v4.25.0`
- executable hashes: Executable identity pinned by preflight in each script: lake --version (4.25.0), lean --version, which lake/lean + sha256sum readlink -f, lean-toolchain file, LEAN_PATH recording. Historical R-BUILD2 lean path /nix/store/4gr3n4nrp0xxgykyyzdxi3xjj2ikn5x1-lean4-4.25.0/bin/lean retained as anchor class, not as current pin.
- input hashes: `{}`; mutation hash: `NONE (restore: git checkout -- .)`
- argv: `["git","checkout","--",".","&&","lake","build","KelGroups.Vote.Invariants"]`; cwd: `/tmp/reactivegas-ms2-measure-scratch/lean`
- env/LEAN_PATH: `{"LEAN_PATH":"<recorded>"}`; initial cache: Dirty (1 path) before checkout; clean + warm after
- targets/closure: `["lake build KelGroups.Vote.Invariants to GREEN (R-BUILD3 pattern)"]` / KelGroups.Vote.State -> KelGroups.Vote.Validate/Fold -> KelGroups.Vote.Invariants
- timer/exit: checkout ms + build ms separately (m-C-VOTESTATE-restore-checkout.ms + m-C-VOTESTATE-restore.ms); porcelain retained; timeout: `300s + 15s kill-after`
- expected semantic evidence: GREEN Build completed successfully; porcelain empty; R-BUILD3 pattern (no timings in history, timed here).
- wrong-reason rejection: Restore cleanliness does not cure historical candidate-source fence event or recover lost cold log (S-07 defect7 stands retrospectively).
- loading evidence: N/A (GREEN rebuild)
- restoration: Porcelain empty; HEAD 3590c001; single-cause attribution distinct from filesystem isolation.
- budget charge (including failure): `1 U-RESTORE (including failure)`
- frozen scripts: instruments/restore.sh sha256:9d58c3d984e6866318b06be2abbb1dd800b73a6391e151a44b8747f1a1714785

## 8. M04-C-VALIDATE — U-CHAIN — C-VALIDATE single-atom mutant build

- scratch/source/output: `/tmp/reactivegas-ms2-measure-scratch` / `/tmp/reactivegas-ms2-measure-scratch/lean` / `/tmp/reactivegas-ms2-measure-output/m-C-VALIDATE.stdout`
- base/toolchain: `3590c0015b84fd58004bf6fb44dd18b107304c48` / `leanprover/lean4:v4.25.0`
- executable hashes: Executable identity pinned by preflight in each script: lake --version (4.25.0), lean --version, which lake/lean + sha256sum readlink -f, lean-toolchain file, LEAN_PATH recording. Historical R-BUILD2 lean path /nix/store/4gr3n4nrp0xxgykyyzdxi3xjj2ikn5x1-lean4-4.25.0/bin/lean retained as anchor class, not as current pin.
- input hashes: `{}`; mutation hash: `37f26b8b70e3e7a431d1273cfb585e89677055f71883fed007af34edbdbb5ace (diffs/C-VALIDATE.diff)`
- argv: `["lake","build","KelGroups.Invariants"]`; cwd: `/tmp/reactivegas-ms2-measure-scratch/lean`
- env/LEAN_PATH: `{"LEAN_PATH":"<recorded; must be scratch, not candidate>"}`; initial cache: Warm (post-M00 cold .lake present); incremental classification established only because cold precedes cycle1 (S-07 defect1 cured; cold after cycles would not establish incremental).
- targets/closure: `["lake build KelGroups.Invariants"]` / KelGroups.Validate -> KelGroups.Integration -> KelGroups.Invariants (exact, no ...)
- timer/exit: date +%s%N apply (m-C-VALIDATE-apply.ms) + build (m-C-VALIDATE.ms); exits captured; dirty-status (must be 1 path); applied.diff retained; timeout: `300s + 15s kill-after per build; 600s cold separate`
- expected semantic evidence: Span-bound RED at first failing obligation in KelGroups.Invariants (see mutant-*.sh EXPECTED-OBSERVABLE); continuation past error does not prove every later check executed (S-05).
- wrong-reason rejection: Nonzero exit alone is not named RED; broad file-wide search matching unrelated lines is unsound (SS0 L1-2).
- loading evidence: Built vs Replayed lines in log distinguish fresh vs cached diagnostics; source byte identity alone insufficient (SS0 L6). Changed-definition loading via Built <mutated module> line (e.g., Built Reactivegas.Step).
- restoration: Via restore.sh KelGroups.Invariants (U-RESTORE, next op); porcelain empty; HEAD 3590c001; restore cleanliness does not prove historical separation (F-07 stands retrospectively, cured prospectively).
- budget charge (including failure): `1 U-CHAIN (including failure; timeouts/failures charged+retained, never silently retried)`
- frozen scripts: instruments/mutant-C-VALIDATE.sh sha256:e3a6323601fe0302a8d4582e3045626c2dc216417c4b94856e65eb93d544eaa0; instruments/diffs/C-VALIDATE.diff sha256:37f26b8b70e3e7a431d1273cfb585e89677055f71883fed007af34edbdbb5ace

## 9. M04-C-VALIDATER — U-RESTORE — C-VALIDATE restore + matching rebuild

- scratch/source/output: `/tmp/reactivegas-ms2-measure-scratch` / `/tmp/reactivegas-ms2-measure-scratch/lean` / `/tmp/reactivegas-ms2-measure-output/m-C-VALIDATE-restore.stdout`
- base/toolchain: `3590c0015b84fd58004bf6fb44dd18b107304c48` / `leanprover/lean4:v4.25.0`
- executable hashes: Executable identity pinned by preflight in each script: lake --version (4.25.0), lean --version, which lake/lean + sha256sum readlink -f, lean-toolchain file, LEAN_PATH recording. Historical R-BUILD2 lean path /nix/store/4gr3n4nrp0xxgykyyzdxi3xjj2ikn5x1-lean4-4.25.0/bin/lean retained as anchor class, not as current pin.
- input hashes: `{}`; mutation hash: `NONE (restore: git checkout -- .)`
- argv: `["git","checkout","--",".","&&","lake","build","KelGroups.Invariants"]`; cwd: `/tmp/reactivegas-ms2-measure-scratch/lean`
- env/LEAN_PATH: `{"LEAN_PATH":"<recorded>"}`; initial cache: Dirty (1 path) before checkout; clean + warm after
- targets/closure: `["lake build KelGroups.Invariants to GREEN (R-BUILD3 pattern)"]` / KelGroups.Validate -> KelGroups.Integration -> KelGroups.Invariants
- timer/exit: checkout ms + build ms separately (m-C-VALIDATE-restore-checkout.ms + m-C-VALIDATE-restore.ms); porcelain retained; timeout: `300s + 15s kill-after`
- expected semantic evidence: GREEN Build completed successfully; porcelain empty; R-BUILD3 pattern (no timings in history, timed here).
- wrong-reason rejection: Restore cleanliness does not cure historical candidate-source fence event or recover lost cold log (S-07 defect7 stands retrospectively).
- loading evidence: N/A (GREEN rebuild)
- restoration: Porcelain empty; HEAD 3590c001; single-cause attribution distinct from filesystem isolation.
- budget charge (including failure): `1 U-RESTORE (including failure)`
- frozen scripts: instruments/restore.sh sha256:9d58c3d984e6866318b06be2abbb1dd800b73a6391e151a44b8747f1a1714785

## 10. M05-C-INTEGRATION — U-CHAIN — C-INTEGRATION single-atom mutant build

- scratch/source/output: `/tmp/reactivegas-ms2-measure-scratch` / `/tmp/reactivegas-ms2-measure-scratch/lean` / `/tmp/reactivegas-ms2-measure-output/m-C-INTEGRATION.stdout`
- base/toolchain: `3590c0015b84fd58004bf6fb44dd18b107304c48` / `leanprover/lean4:v4.25.0`
- executable hashes: Executable identity pinned by preflight in each script: lake --version (4.25.0), lean --version, which lake/lean + sha256sum readlink -f, lean-toolchain file, LEAN_PATH recording. Historical R-BUILD2 lean path /nix/store/4gr3n4nrp0xxgykyyzdxi3xjj2ikn5x1-lean4-4.25.0/bin/lean retained as anchor class, not as current pin.
- input hashes: `{}`; mutation hash: `8c8f7fe8192006ec083d7c4f5210404477f70cf3ae723a01be853d54848bed8a (diffs/C-INTEGRATION.diff)`
- argv: `["lake","build","KelGroups.Invariants"]`; cwd: `/tmp/reactivegas-ms2-measure-scratch/lean`
- env/LEAN_PATH: `{"LEAN_PATH":"<recorded; must be scratch, not candidate>"}`; initial cache: Warm (post-M00 cold .lake present); incremental classification established only because cold precedes cycle1 (S-07 defect1 cured; cold after cycles would not establish incremental).
- targets/closure: `["lake build KelGroups.Invariants"]` / KelGroups.Integration -> KelGroups.Invariants (exact, no ...)
- timer/exit: date +%s%N apply (m-C-INTEGRATION-apply.ms) + build (m-C-INTEGRATION.ms); exits captured; dirty-status (must be 1 path); applied.diff retained; timeout: `300s + 15s kill-after per build; 600s cold separate`
- expected semantic evidence: Span-bound RED at first failing obligation in KelGroups.Invariants (see mutant-*.sh EXPECTED-OBSERVABLE); continuation past error does not prove every later check executed (S-05).
- wrong-reason rejection: Nonzero exit alone is not named RED; broad file-wide search matching unrelated lines is unsound (SS0 L1-2).
- loading evidence: Built vs Replayed lines in log distinguish fresh vs cached diagnostics; source byte identity alone insufficient (SS0 L6). Changed-definition loading via Built <mutated module> line (e.g., Built Reactivegas.Step).
- restoration: Via restore.sh KelGroups.Invariants (U-RESTORE, next op); porcelain empty; HEAD 3590c001; restore cleanliness does not prove historical separation (F-07 stands retrospectively, cured prospectively).
- budget charge (including failure): `1 U-CHAIN (including failure; timeouts/failures charged+retained, never silently retried)`
- frozen scripts: instruments/mutant-C-INTEGRATION.sh sha256:c88aacabda031465e793758a0c7fb7e107f3115b39d635195b58ecffb4de7aba; instruments/diffs/C-INTEGRATION.diff sha256:8c8f7fe8192006ec083d7c4f5210404477f70cf3ae723a01be853d54848bed8a

## 11. M05-C-INTEGRATIONR — U-RESTORE — C-INTEGRATION restore + matching rebuild

- scratch/source/output: `/tmp/reactivegas-ms2-measure-scratch` / `/tmp/reactivegas-ms2-measure-scratch/lean` / `/tmp/reactivegas-ms2-measure-output/m-C-INTEGRATION-restore.stdout`
- base/toolchain: `3590c0015b84fd58004bf6fb44dd18b107304c48` / `leanprover/lean4:v4.25.0`
- executable hashes: Executable identity pinned by preflight in each script: lake --version (4.25.0), lean --version, which lake/lean + sha256sum readlink -f, lean-toolchain file, LEAN_PATH recording. Historical R-BUILD2 lean path /nix/store/4gr3n4nrp0xxgykyyzdxi3xjj2ikn5x1-lean4-4.25.0/bin/lean retained as anchor class, not as current pin.
- input hashes: `{}`; mutation hash: `NONE (restore: git checkout -- .)`
- argv: `["git","checkout","--",".","&&","lake","build","KelGroups.Invariants"]`; cwd: `/tmp/reactivegas-ms2-measure-scratch/lean`
- env/LEAN_PATH: `{"LEAN_PATH":"<recorded>"}`; initial cache: Dirty (1 path) before checkout; clean + warm after
- targets/closure: `["lake build KelGroups.Invariants to GREEN (R-BUILD3 pattern)"]` / KelGroups.Integration -> KelGroups.Invariants
- timer/exit: checkout ms + build ms separately (m-C-INTEGRATION-restore-checkout.ms + m-C-INTEGRATION-restore.ms); porcelain retained; timeout: `300s + 15s kill-after`
- expected semantic evidence: GREEN Build completed successfully; porcelain empty; R-BUILD3 pattern (no timings in history, timed here).
- wrong-reason rejection: Restore cleanliness does not cure historical candidate-source fence event or recover lost cold log (S-07 defect7 stands retrospectively).
- loading evidence: N/A (GREEN rebuild)
- restoration: Porcelain empty; HEAD 3590c001; single-cause attribution distinct from filesystem isolation.
- budget charge (including failure): `1 U-RESTORE (including failure)`
- frozen scripts: instruments/restore.sh sha256:9d58c3d984e6866318b06be2abbb1dd800b73a6391e151a44b8747f1a1714785

## 12. M06-C-FOLD — U-CHAIN — C-FOLD single-atom mutant build

- scratch/source/output: `/tmp/reactivegas-ms2-measure-scratch` / `/tmp/reactivegas-ms2-measure-scratch/lean` / `/tmp/reactivegas-ms2-measure-output/m-C-FOLD.stdout`
- base/toolchain: `3590c0015b84fd58004bf6fb44dd18b107304c48` / `leanprover/lean4:v4.25.0`
- executable hashes: Executable identity pinned by preflight in each script: lake --version (4.25.0), lean --version, which lake/lean + sha256sum readlink -f, lean-toolchain file, LEAN_PATH recording. Historical R-BUILD2 lean path /nix/store/4gr3n4nrp0xxgykyyzdxi3xjj2ikn5x1-lean4-4.25.0/bin/lean retained as anchor class, not as current pin.
- input hashes: `{}`; mutation hash: `a1d5a821b120aa5ab96cbc182d03c83b6ae591f5d6afdbd223da171026b6440c (diffs/C-FOLD.diff)`
- argv: `["lake","build","KelGroups.Invariants"]`; cwd: `/tmp/reactivegas-ms2-measure-scratch/lean`
- env/LEAN_PATH: `{"LEAN_PATH":"<recorded; must be scratch, not candidate>"}`; initial cache: Warm (post-M00 cold .lake present); incremental classification established only because cold precedes cycle1 (S-07 defect1 cured; cold after cycles would not establish incremental).
- targets/closure: `["lake build KelGroups.Invariants"]` / KelGroups.Fold -> KelGroups.Invariants (exact, no ...)
- timer/exit: date +%s%N apply (m-C-FOLD-apply.ms) + build (m-C-FOLD.ms); exits captured; dirty-status (must be 1 path); applied.diff retained; timeout: `300s + 15s kill-after per build; 600s cold separate`
- expected semantic evidence: Span-bound RED at first failing obligation in KelGroups.Invariants (see mutant-*.sh EXPECTED-OBSERVABLE); continuation past error does not prove every later check executed (S-05).
- wrong-reason rejection: Nonzero exit alone is not named RED; broad file-wide search matching unrelated lines is unsound (SS0 L1-2).
- loading evidence: Built vs Replayed lines in log distinguish fresh vs cached diagnostics; source byte identity alone insufficient (SS0 L6). Changed-definition loading via Built <mutated module> line (e.g., Built Reactivegas.Step).
- restoration: Via restore.sh KelGroups.Invariants (U-RESTORE, next op); porcelain empty; HEAD 3590c001; restore cleanliness does not prove historical separation (F-07 stands retrospectively, cured prospectively).
- budget charge (including failure): `1 U-CHAIN (including failure; timeouts/failures charged+retained, never silently retried)`
- frozen scripts: instruments/mutant-C-FOLD.sh sha256:7d96d7f0ee7aee66c26253ee653ab6593baff25017e517c3986aac76e092c015; instruments/diffs/C-FOLD.diff sha256:a1d5a821b120aa5ab96cbc182d03c83b6ae591f5d6afdbd223da171026b6440c

## 13. M06-C-FOLDR — U-RESTORE — C-FOLD restore + matching rebuild

- scratch/source/output: `/tmp/reactivegas-ms2-measure-scratch` / `/tmp/reactivegas-ms2-measure-scratch/lean` / `/tmp/reactivegas-ms2-measure-output/m-C-FOLD-restore.stdout`
- base/toolchain: `3590c0015b84fd58004bf6fb44dd18b107304c48` / `leanprover/lean4:v4.25.0`
- executable hashes: Executable identity pinned by preflight in each script: lake --version (4.25.0), lean --version, which lake/lean + sha256sum readlink -f, lean-toolchain file, LEAN_PATH recording. Historical R-BUILD2 lean path /nix/store/4gr3n4nrp0xxgykyyzdxi3xjj2ikn5x1-lean4-4.25.0/bin/lean retained as anchor class, not as current pin.
- input hashes: `{}`; mutation hash: `NONE (restore: git checkout -- .)`
- argv: `["git","checkout","--",".","&&","lake","build","KelGroups.Invariants"]`; cwd: `/tmp/reactivegas-ms2-measure-scratch/lean`
- env/LEAN_PATH: `{"LEAN_PATH":"<recorded>"}`; initial cache: Dirty (1 path) before checkout; clean + warm after
- targets/closure: `["lake build KelGroups.Invariants to GREEN (R-BUILD3 pattern)"]` / KelGroups.Fold -> KelGroups.Invariants
- timer/exit: checkout ms + build ms separately (m-C-FOLD-restore-checkout.ms + m-C-FOLD-restore.ms); porcelain retained; timeout: `300s + 15s kill-after`
- expected semantic evidence: GREEN Build completed successfully; porcelain empty; R-BUILD3 pattern (no timings in history, timed here).
- wrong-reason rejection: Restore cleanliness does not cure historical candidate-source fence event or recover lost cold log (S-07 defect7 stands retrospectively).
- loading evidence: N/A (GREEN rebuild)
- restoration: Porcelain empty; HEAD 3590c001; single-cause attribution distinct from filesystem isolation.
- budget charge (including failure): `1 U-RESTORE (including failure)`
- frozen scripts: instruments/restore.sh sha256:9d58c3d984e6866318b06be2abbb1dd800b73a6391e151a44b8747f1a1714785

## 14. M07-C-KSTATE — U-CHAIN — C-KSTATE single-atom mutant build

- scratch/source/output: `/tmp/reactivegas-ms2-measure-scratch` / `/tmp/reactivegas-ms2-measure-scratch/lean` / `/tmp/reactivegas-ms2-measure-output/m-C-KSTATE.stdout`
- base/toolchain: `3590c0015b84fd58004bf6fb44dd18b107304c48` / `leanprover/lean4:v4.25.0`
- executable hashes: Executable identity pinned by preflight in each script: lake --version (4.25.0), lean --version, which lake/lean + sha256sum readlink -f, lean-toolchain file, LEAN_PATH recording. Historical R-BUILD2 lean path /nix/store/4gr3n4nrp0xxgykyyzdxi3xjj2ikn5x1-lean4-4.25.0/bin/lean retained as anchor class, not as current pin.
- input hashes: `{}`; mutation hash: `0314824bbd3001caae4297b7e1919910b1f4c61974d07733e7cbc7d15a57caa7 (diffs/C-KSTATE.diff)`
- argv: `["lake","build","KelGroups.Invariants"]`; cwd: `/tmp/reactivegas-ms2-measure-scratch/lean`
- env/LEAN_PATH: `{"LEAN_PATH":"<recorded; must be scratch, not candidate>"}`; initial cache: Warm (post-M00 cold .lake present); incremental classification established only because cold precedes cycle1 (S-07 defect1 cured; cold after cycles would not establish incremental).
- targets/closure: `["lake build KelGroups.Invariants"]` / KelGroups.State -> KelGroups.Fold/Validate/Integration -> KelGroups.Invariants (exact, no ...)
- timer/exit: date +%s%N apply (m-C-KSTATE-apply.ms) + build (m-C-KSTATE.ms); exits captured; dirty-status (must be 1 path); applied.diff retained; timeout: `300s + 15s kill-after per build; 600s cold separate`
- expected semantic evidence: Span-bound RED at first failing obligation in KelGroups.Invariants (see mutant-*.sh EXPECTED-OBSERVABLE); continuation past error does not prove every later check executed (S-05).
- wrong-reason rejection: Nonzero exit alone is not named RED; broad file-wide search matching unrelated lines is unsound (SS0 L1-2).
- loading evidence: Built vs Replayed lines in log distinguish fresh vs cached diagnostics; source byte identity alone insufficient (SS0 L6). Changed-definition loading via Built <mutated module> line (e.g., Built Reactivegas.Step).
- restoration: Via restore.sh KelGroups.Invariants (U-RESTORE, next op); porcelain empty; HEAD 3590c001; restore cleanliness does not prove historical separation (F-07 stands retrospectively, cured prospectively).
- budget charge (including failure): `1 U-CHAIN (including failure; timeouts/failures charged+retained, never silently retried)`
- frozen scripts: instruments/mutant-C-KSTATE.sh sha256:76767a5019b44145505a64e1ca04d769acc7095e469815ff4788de1442b2c1fe; instruments/diffs/C-KSTATE.diff sha256:0314824bbd3001caae4297b7e1919910b1f4c61974d07733e7cbc7d15a57caa7

## 15. M07-C-KSTATER — U-RESTORE — C-KSTATE restore + matching rebuild

- scratch/source/output: `/tmp/reactivegas-ms2-measure-scratch` / `/tmp/reactivegas-ms2-measure-scratch/lean` / `/tmp/reactivegas-ms2-measure-output/m-C-KSTATE-restore.stdout`
- base/toolchain: `3590c0015b84fd58004bf6fb44dd18b107304c48` / `leanprover/lean4:v4.25.0`
- executable hashes: Executable identity pinned by preflight in each script: lake --version (4.25.0), lean --version, which lake/lean + sha256sum readlink -f, lean-toolchain file, LEAN_PATH recording. Historical R-BUILD2 lean path /nix/store/4gr3n4nrp0xxgykyyzdxi3xjj2ikn5x1-lean4-4.25.0/bin/lean retained as anchor class, not as current pin.
- input hashes: `{}`; mutation hash: `NONE (restore: git checkout -- .)`
- argv: `["git","checkout","--",".","&&","lake","build","KelGroups.Invariants"]`; cwd: `/tmp/reactivegas-ms2-measure-scratch/lean`
- env/LEAN_PATH: `{"LEAN_PATH":"<recorded>"}`; initial cache: Dirty (1 path) before checkout; clean + warm after
- targets/closure: `["lake build KelGroups.Invariants to GREEN (R-BUILD3 pattern)"]` / KelGroups.State -> KelGroups.Fold/Validate/Integration -> KelGroups.Invariants
- timer/exit: checkout ms + build ms separately (m-C-KSTATE-restore-checkout.ms + m-C-KSTATE-restore.ms); porcelain retained; timeout: `300s + 15s kill-after`
- expected semantic evidence: GREEN Build completed successfully; porcelain empty; R-BUILD3 pattern (no timings in history, timed here).
- wrong-reason rejection: Restore cleanliness does not cure historical candidate-source fence event or recover lost cold log (S-07 defect7 stands retrospectively).
- loading evidence: N/A (GREEN rebuild)
- restoration: Porcelain empty; HEAD 3590c001; single-cause attribution distinct from filesystem isolation.
- budget charge (including failure): `1 U-RESTORE (including failure)`
- frozen scripts: instruments/restore.sh sha256:9d58c3d984e6866318b06be2abbb1dd800b73a6391e151a44b8747f1a1714785

## 16. M08-C-RSTATE — U-CHAIN — C-RSTATE single-atom mutant build

- scratch/source/output: `/tmp/reactivegas-ms2-measure-scratch` / `/tmp/reactivegas-ms2-measure-scratch/lean` / `/tmp/reactivegas-ms2-measure-output/m-C-RSTATE.stdout`
- base/toolchain: `3590c0015b84fd58004bf6fb44dd18b107304c48` / `leanprover/lean4:v4.25.0`
- executable hashes: Executable identity pinned by preflight in each script: lake --version (4.25.0), lean --version, which lake/lean + sha256sum readlink -f, lean-toolchain file, LEAN_PATH recording. Historical R-BUILD2 lean path /nix/store/4gr3n4nrp0xxgykyyzdxi3xjj2ikn5x1-lean4-4.25.0/bin/lean retained as anchor class, not as current pin.
- input hashes: `{}`; mutation hash: `702d6935ebb4e3ec651044f04730169d58d5ebeeccadf6439dfd9d0d8fb4576a (diffs/C-RSTATE.diff)`
- argv: `["lake","build","Reactivegas.State"]`; cwd: `/tmp/reactivegas-ms2-measure-scratch/lean`
- env/LEAN_PATH: `{"LEAN_PATH":"<recorded; must be scratch, not candidate>"}`; initial cache: Warm (post-M00 cold .lake present); incremental classification established only because cold precedes cycle1 (S-07 defect1 cured; cold after cycles would not establish incremental).
- targets/closure: `["lake build Reactivegas.State"]` / Reactivegas.State (primary); conditional higher Reactivegas.Invariants via isolation (separately authorized) (exact, no ...)
- timer/exit: date +%s%N apply (m-C-RSTATE-apply.ms) + build (m-C-RSTATE.ms); exits captured; dirty-status (must be 1 path); applied.diff retained; timeout: `300s + 15s kill-after per build; 600s cold separate`
- expected semantic evidence: PRIMARY State.refundAll_sum (Reactivegas/State.lean:159) fails FIRST in imported producer; higher deny/fail fund-equation CONDITIONAL separately bound (not conflated); halts at first failing obligation per Addendum r3.
- wrong-reason rejection: Nonzero exit alone is not named RED; broad file-wide search matching unrelated lines is unsound (SS0 L1-2).
- loading evidence: Built vs Replayed lines in log distinguish fresh vs cached diagnostics; source byte identity alone insufficient (SS0 L6). Changed-definition loading via Built <mutated module> line (e.g., Built Reactivegas.Step).
- restoration: Via restore.sh Reactivegas.State (U-RESTORE, next op); porcelain empty; HEAD 3590c001; restore cleanliness does not prove historical separation (F-07 stands retrospectively, cured prospectively).
- budget charge (including failure): `1 U-CHAIN (including failure; timeouts/failures charged+retained, never silently retried)`
- frozen scripts: instruments/mutant-C-RSTATE.sh sha256:c6e3327f6dbd651783fabfec1f6fb583bf5e57db0e73402a1bae73456090360e; instruments/diffs/C-RSTATE.diff sha256:702d6935ebb4e3ec651044f04730169d58d5ebeeccadf6439dfd9d0d8fb4576a

## 17. M08-C-RSTATER — U-RESTORE — C-RSTATE restore + matching rebuild

- scratch/source/output: `/tmp/reactivegas-ms2-measure-scratch` / `/tmp/reactivegas-ms2-measure-scratch/lean` / `/tmp/reactivegas-ms2-measure-output/m-C-RSTATE-restore.stdout`
- base/toolchain: `3590c0015b84fd58004bf6fb44dd18b107304c48` / `leanprover/lean4:v4.25.0`
- executable hashes: Executable identity pinned by preflight in each script: lake --version (4.25.0), lean --version, which lake/lean + sha256sum readlink -f, lean-toolchain file, LEAN_PATH recording. Historical R-BUILD2 lean path /nix/store/4gr3n4nrp0xxgykyyzdxi3xjj2ikn5x1-lean4-4.25.0/bin/lean retained as anchor class, not as current pin.
- input hashes: `{}`; mutation hash: `NONE (restore: git checkout -- .)`
- argv: `["git","checkout","--",".","&&","lake","build","Reactivegas.State"]`; cwd: `/tmp/reactivegas-ms2-measure-scratch/lean`
- env/LEAN_PATH: `{"LEAN_PATH":"<recorded>"}`; initial cache: Dirty (1 path) before checkout; clean + warm after
- targets/closure: `["lake build Reactivegas.State to GREEN (R-BUILD3 pattern)"]` / Reactivegas.State (primary); conditional higher Reactivegas.Invariants via isolation (separately authorized)
- timer/exit: checkout ms + build ms separately (m-C-RSTATE-restore-checkout.ms + m-C-RSTATE-restore.ms); porcelain retained; timeout: `300s + 15s kill-after`
- expected semantic evidence: GREEN Build completed successfully; porcelain empty; R-BUILD3 pattern (no timings in history, timed here).
- wrong-reason rejection: Restore cleanliness does not cure historical candidate-source fence event or recover lost cold log (S-07 defect7 stands retrospectively).
- loading evidence: N/A (GREEN rebuild)
- restoration: Porcelain empty; HEAD 3590c001; single-cause attribution distinct from filesystem isolation.
- budget charge (including failure): `1 U-RESTORE (including failure)`
- frozen scripts: instruments/restore.sh sha256:9d58c3d984e6866318b06be2abbb1dd800b73a6391e151a44b8747f1a1714785

## 18. M09-CHECK — U-CHECK — isolated proof/check elaboration

- scratch/source/output: `/tmp/reactivegas-ms2-measure-scratch` / `/tmp/reactivegas-ms2-measure-scratch/lean` / `/tmp/reactivegas-ms2-measure-output/m09-check.stdout`
- base/toolchain: `3590c0015b84fd58004bf6fb44dd18b107304c48` / `leanprover/lean4:v4.25.0`
- executable hashes: Executable identity pinned by preflight in each script: lake --version (4.25.0), lean --version, which lake/lean + sha256sum readlink -f, lean-toolchain file, LEAN_PATH recording. Historical R-BUILD2 lean path /nix/store/4gr3n4nrp0xxgykyyzdxi3xjj2ikn5x1-lean4-4.25.0/bin/lean retained as anchor class, not as current pin.
- input hashes: `{"checkLean":"f9d3d3ac8c66ce46ce7fe4846c91b5992d281e81cc0e5c9c6331d042616bcdd1"}`; mutation hash: `NONE (no mutant; clean tree)`
- argv: `["lake","env","lean","/tmp/reactivegas/ms2/e-lean-compliance/s3-successor-spec-1/handoffs/instruments/Check.lean"]`; cwd: `/tmp/reactivegas-ms2-measure-scratch/lean`
- env/LEAN_PATH: `{"LEAN_PATH":"<recorded>"}`; initial cache: Warm (post-cold); isolated elaboration, not incremental rebuild
- targets/closure: `["elaborate theorem ss_check_elaboration : Reactivegas.checkSweepIdempotent = true := by decide (fully-qualified, by decide, no #eval)"]` / Reactivegas.Invariants (imported) + Check.lean (isolated file)
- timer/exit: date +%s%N -> m09-check.ms; exit -> m09-check.exit; stdout/stderr byte counts (clean elaboration silent beyond ACTUAL-CWD lines); timeout: `120s + 15s kill-after`
- expected semantic evidence: GREEN silent elaboration (exit 0, empty stdout/stderr beyond ACTUAL lines); distinct cheap unit (SS0 OP4 2476ms pattern); not #eval, not runtime replay.
- wrong-reason rejection: Unqualified checkSweepIdempotent (actually Reactivegas.checkSweepIdempotent) with #eval is mixed elaboration/evaluation, not isolated by-decide elaboration (S-07 defect2). Even qualified, kind remains mixed module elaboration/evaluation if #eval present.
- loading evidence: N/A (elaboration, not Built/Replayed build)
- restoration: N/A (no mutant; no restore needed)
- budget charge (including failure): `1 U-CHECK (targeted; including failure)`
- frozen scripts: instruments/Check.lean sha256:f9d3d3ac8c66ce46ce7fe4846c91b5992d281e81cc0e5c9c6331d042616bcdd1; instruments/check.sh sha256:4bcefdb45d12fa719209daf10b863da86fe26b90c54452ba1ecf0b272cc3ae26

## 19. M10-REPLAY-PROD — U-REPLAY-PROD — runtime artifact production (separate from execution)

- scratch/source/output: `/tmp/reactivegas-ms2-measure-scratch` / `/tmp/reactivegas-ms2-measure-scratch/lean` / `/tmp/reactivegas-ms2-measure-output/m10-replay-prod.stdout`
- base/toolchain: `3590c0015b84fd58004bf6fb44dd18b107304c48` / `leanprover/lean4:v4.25.0`
- executable hashes: Executable identity pinned by preflight in each script: lake --version (4.25.0), lean --version, which lake/lean + sha256sum readlink -f, lean-toolchain file, LEAN_PATH recording. Historical R-BUILD2 lean path /nix/store/4gr3n4nrp0xxgykyyzdxi3xjj2ikn5x1-lean4-4.25.0/bin/lean retained as anchor class, not as current pin.
- input hashes: `{}`; mutation hash: `NONE (clean)`
- argv: `["lake","build","corpusExport"]`; cwd: `/tmp/reactivegas-ms2-measure-scratch/lean`
- env/LEAN_PATH: `{"LEAN_PATH":"<recorded>"}`; initial cache: Warm; executable production counted separately from execution (S-07: none hidden as preparation)
- targets/closure: `["lake build corpusExport (lean_exe root Reactivegas.CorpusExport; closure CorpusExport->Trace->Invariants)"]` / Reactivegas.CorpusExport (exe) + Reactivegas.Trace + Reactivegas.Invariants
- timer/exit: date +%s%N -> m10-replay-prod.ms; exit -> m10-replay-prod.exit; exe sha retained (.lake/build/bin/corpusExport); timeout: `300s + 15s kill-after`
- expected semantic evidence: GREEN + exe sha + per-module Built lines; cures U-REPLAY absent (S-07 defect3) at production half.
- wrong-reason rejection: Plain lake build default roots do not reach CorpusExport/TraceTests/CorpusGate; exact target corpusExport required (no ...).
- loading evidence: Built Reactivegas.CorpusExport line; exe sha pins executable identity (version print alone insufficient per S-07 defect5).
- restoration: N/A (no mutant)
- budget charge (including failure): `1 U-REPLAY-PROD (including failure)`
- frozen scripts: instruments/replay-build.sh sha256:e00121b4d00f90d22ddb6e2e109677358b6389dd54df64b41e315cb83f8efe77

## 20. M11A-REPLAY-EXEC-WRITE — U-REPLAY-EXEC — runtime replay write (prebuilt exe, fixed corpus)

- scratch/source/output: `/tmp/reactivegas-ms2-measure-scratch` / `/tmp/reactivegas-ms2-measure-scratch/lean/.lake/build/bin/corpusExport` / `/tmp/reactivegas-ms2-measure-output/replay-econ.json`
- base/toolchain: `3590c0015b84fd58004bf6fb44dd18b107304c48` / `leanprover/lean4:v4.25.0`
- executable hashes: Exe sha from M10 (pinned; recorded before run)
- input hashes: `{"exe":"<from M10 exe sha>"}`; mutation hash: `NONE (fixed corpus seedCorpus/emitIntegratedCorpus via exe; nonzero extents)`
- argv: `["/tmp/reactivegas-ms2-measure-scratch/lean/.lake/build/bin/corpusExport","/tmp/reactivegas-ms2-measure-output/replay-econ.json","/tmp/reactivegas-ms2-measure-output/replay-int.json"]`; cwd: `/tmp/reactivegas-ms2-measure-scratch/lean`
- env/LEAN_PATH: `{"LEAN_PATH":"<recorded; exe run, not lean elaboration>"}`; initial cache: Exe prebuilt (M10); cache irrelevant for runtime; fixed corpus, no #eval
- targets/closure: `["write econ/int wrappers (exact bytes compared later)"]` / Runtime only (no rebuild)
- timer/exit: date +%s%N -> m11-replay-exec-green-write.ms; exit -> .exit; econ/int sha + byte counts retained; timeout: `120s + 15s kill-after`
- expected semantic evidence: GREEN exit 0 + econ/int shas; nonzero trace/event extents (check phase verifies).
- wrong-reason rejection: #eval during elaboration is not this runtime unit and must never be relabelled as one.
- loading evidence: N/A (runtime, not Built/Replayed)
- restoration: N/A (writes to OUT, not scratch; no restore needed)
- budget charge (including failure): `1 U-REPLAY-EXEC (including failure)`
- frozen scripts: instruments/replay-run-green.sh sha256:8f30ebeef1134e641832f97a4e3f52b0b55fdd4a9ab0b58c90d2811ca552e76b

## 21. M11B-REPLAY-EXEC-CHECK — U-REPLAY-EXEC — runtime replay check-green (live-bound verification)

- scratch/source/output: `/tmp/reactivegas-ms2-measure-scratch` / `/tmp/reactivegas-ms2-measure-scratch/lean/.lake/build/bin/corpusExport` / `/tmp/reactivegas-ms2-measure-output/m11-replay-exec-green-check.stdout`
- base/toolchain: `3590c0015b84fd58004bf6fb44dd18b107304c48` / `leanprover/lean4:v4.25.0`
- executable hashes: Exe sha from M10
- input hashes: `{"econ":"<from M11A econ sha>","int":"<from M11A int sha>"}`; mutation hash: `NONE (fixed corpus)`
- argv: `["/tmp/reactivegas-ms2-measure-scratch/lean/.lake/build/bin/corpusExport","check","/tmp/reactivegas-ms2-measure-output/replay-econ.json","/tmp/reactivegas-ms2-measure-output/replay-int.json"]`; cwd: `/tmp/reactivegas-ms2-measure-scratch/lean`
- env/LEAN_PATH: `{"LEAN_PATH":"<recorded>"}`; initial cache: Wrappers from M11A; fixed corpus
- targets/closure: `["check wrappers element-for-element vs live seedCorpus/emitIntegratedCorpus + view/auth binding + nonzero extents"]` / Runtime only
- timer/exit: date +%s%N -> m11-replay-exec-green-check.ms; exit -> .exit; stdout must contain corpus-check: ntraces=... live-bound; timeout: `120s + 15s kill-after`
- expected semantic evidence: GREEN exit 0 + corpus-check: ntraces/nevents/nsteps live-bound line; nonzero extents from bytes (not counts alone; same-size swap must fail — see RED control).
- wrong-reason rejection: Counts alone insufficient; live-call/derived-ToJson method does not establish serializer independence (none required).
- loading evidence: N/A
- restoration: N/A
- budget charge (including failure): `1 U-REPLAY-EXEC (including failure)`
- frozen scripts: instruments/replay-run-green.sh sha256:8f30ebeef1134e641832f97a4e3f52b0b55fdd4a9ab0b58c90d2811ca552e76b

## 22. M12-REPLAY-EXEC-RED — U-REPLAY-EXEC — runtime replay red negative control (ZZZ view)

- scratch/source/output: `/tmp/reactivegas-ms2-measure-scratch` / `/tmp/reactivegas-ms2-measure-scratch/lean/.lake/build/bin/corpusExport` / `/tmp/reactivegas-ms2-measure-output/m12-replay-exec-red.stdout`
- base/toolchain: `3590c0015b84fd58004bf6fb44dd18b107304c48` / `leanprover/lean4:v4.25.0`
- executable hashes: Exe sha from M10
- input hashes: `{"econGreen":"<from M11A>","intGreen":"<from M11A>"}`; mutation hash: `sed s/"key":"[^"]*"/"key":"ZZZ"/ on econ copy (deterministic; diff nonzero verified)`
- argv: `["/tmp/reactivegas-ms2-measure-scratch/lean/.lake/build/bin/corpusExport","check","/tmp/reactivegas-ms2-measure-output/replay-econ-red.json","/tmp/reactivegas-ms2-measure-output/replay-int.json"]`; cwd: `/tmp/reactivegas-ms2-measure-scratch/lean`
- env/LEAN_PATH: `{"LEAN_PATH":"<recorded>"}`; initial cache: Mutated wrapper (ZZZ) + fixed int wrapper
- targets/closure: `["check must FAIL economic: view differs from live seedView"]` / Runtime only
- timer/exit: date +%s%N -> m12 ms; exit nonzero required; stdout/stderr retained; timeout: `120s + 15s kill-after`
- expected semantic evidence: RED exit 1 + FAIL economic line (view differs); proves replay can fail (non-vacuous).
- wrong-reason rejection: A RED for another reason (parse error, missing file) proves nothing; must be FAIL economic.
- loading evidence: N/A
- restoration: N/A (OUT files; scratch untouched)
- budget charge (including failure): `1 U-REPLAY-EXEC (including failure; GREEN here is failure of control, charged)`
- frozen scripts: instruments/replay-run-red.sh sha256:bf8d9ced166d34cef6a4f4223fc89a67d82e1728b6c1b6432245463407ac7672

## 23. M13-BATCH-SEPARATE-A — U-SHARED-SEPARATE — shared/batch separate infra variant A (C-VALIDATE)

- scratch/source/output: `/tmp/reactivegas-ms2-batch-separate-A` / `/tmp/reactivegas-ms2-batch-separate-A/lean` / `/tmp/reactivegas-ms2-measure-output/m-batch-separate-A.stdout`
- base/toolchain: `3590c0015b84fd58004bf6fb44dd18b107304c48` / `leanprover/lean4:v4.25.0`
- executable hashes: Executable identity pinned by preflight in each script: lake --version (4.25.0), lean --version, which lake/lean + sha256sum readlink -f, lean-toolchain file, LEAN_PATH recording. Historical R-BUILD2 lean path /nix/store/4gr3n4nrp0xxgykyyzdxi3xjj2ikn5x1-lean4-4.25.0/bin/lean retained as anchor class, not as current pin.
- input hashes: `{}`; mutation hash: `37f26b8b70e3e7a431d1273cfb585e89677055f71883fed007af34edbdbb5ace`
- argv: `["lake","build","KelGroups.Invariants"]`; cwd: `/tmp/reactivegas-ms2-batch-separate-A/lean`
- env/LEAN_PATH: `{"LEAN_PATH":"<recorded; separate-A>"}`; initial cache: Cold in A (separate .lake); independent source/output roots preserved
- targets/closure: `["lake build KelGroups.Invariants in A"]` / C-VALIDATE closure in A
- timer/exit: Cold + build + restore ms separately (m-batch-separate-A-*.ms); exits retained; timeout: `600s cold + 300s build + 15s kill-after`
- expected semantic evidence: Span-bound RED per C-VALIDATE (see mutant script); setup/restore included; equal observation targets.
- wrong-reason rejection: Sampling within class does not establish every row cost/batching/upper bound (S-07 defect4).
- loading evidence: Built/Replayed in A log
- restoration: restore.sh in A (U-RESTORE); porcelain empty
- budget charge (including failure): `1 U-SHARED-SEPARATE cold+build+restore (including failure)`
- frozen scripts: instruments/batch-plan.sh sha256:0e95e35df63409b39976c5b374f0b7fbd2f929f561b67a2ae55949b5680a09f1; instruments/diffs/C-VALIDATE.diff sha256:37f26b8b70e3e7a431d1273cfb585e89677055f71883fed007af34edbdbb5ace

## 24. M14-BATCH-SEPARATE-B — U-SHARED-SEPARATE — shared/batch separate infra variant B (C-VOTEFOLD)

- scratch/source/output: `/tmp/reactivegas-ms2-batch-separate-B` / `/tmp/reactivegas-ms2-batch-separate-B/lean` / `/tmp/reactivegas-ms2-measure-output/m-batch-separate-B.stdout`
- base/toolchain: `3590c0015b84fd58004bf6fb44dd18b107304c48` / `leanprover/lean4:v4.25.0`
- executable hashes: Executable identity pinned by preflight in each script: lake --version (4.25.0), lean --version, which lake/lean + sha256sum readlink -f, lean-toolchain file, LEAN_PATH recording. Historical R-BUILD2 lean path /nix/store/4gr3n4nrp0xxgykyyzdxi3xjj2ikn5x1-lean4-4.25.0/bin/lean retained as anchor class, not as current pin.
- input hashes: `{}`; mutation hash: `79a6757ddb780ee2ba347fbdffdf078bdfb063f95b1accaf49c0e0aff07d8f4c`
- argv: `["lake","build","KelGroups.Vote.Invariants"]`; cwd: `/tmp/reactivegas-ms2-batch-separate-B/lean`
- env/LEAN_PATH: `{"LEAN_PATH":"<recorded; separate-B>"}`; initial cache: Cold in B (separate .lake)
- targets/closure: `["lake build KelGroups.Vote.Invariants in B"]` / C-VOTEFOLD closure in B
- timer/exit: Cold+build+restore ms separately; timeout: `600s + 300s + 15s`
- expected semantic evidence: Span-bound RED per C-VOTEFOLD; setup/restore included.
- wrong-reason rejection: One mutation per import root is sampling, not batching proof.
- loading evidence: Built/Replayed in B log
- restoration: restore.sh in B
- budget charge (including failure): `1 U-SHARED-SEPARATE (including failure)`
- frozen scripts: instruments/batch-plan.sh sha256:0e95e35df63409b39976c5b374f0b7fbd2f929f561b67a2ae55949b5680a09f1; instruments/diffs/C-VOTEFOLD.diff sha256:79a6757ddb780ee2ba347fbdffdf078bdfb063f95b1accaf49c0e0aff07d8f4c

## 25. M15-BATCH-SHARED — U-SHARED-BATCH — shared/batch shared infra (A+B sequential in one scratch)

- scratch/source/output: `/tmp/reactivegas-ms2-batch-shared` / `/tmp/reactivegas-ms2-batch-shared/lean` / `/tmp/reactivegas-ms2-measure-output/m-batch-shared.stdout`
- base/toolchain: `3590c0015b84fd58004bf6fb44dd18b107304c48` / `leanprover/lean4:v4.25.0`
- executable hashes: Executable identity pinned by preflight in each script: lake --version (4.25.0), lean --version, which lake/lean + sha256sum readlink -f, lean-toolchain file, LEAN_PATH recording. Historical R-BUILD2 lean path /nix/store/4gr3n4nrp0xxgykyyzdxi3xjj2ikn5x1-lean4-4.25.0/bin/lean retained as anchor class, not as current pin.
- input hashes: `{}`; mutation hash: `37f26b8b70e3e7a431d1273cfb585e89677055f71883fed007af34edbdbb5ace+79a6757ddb780ee2ba347fbdffdf078bdfb063f95b1accaf49c0e0aff07d8f4c (sequential, never combined in one variant)`
- argv: `["lake","build","KelGroups.Invariants","&&","lake","build","KelGroups.Vote.Invariants"]`; cwd: `/tmp/reactivegas-ms2-batch-shared/lean`
- env/LEAN_PATH: `{"LEAN_PATH":"<recorded; shared>"}`; initial cache: One cold in shared, then sequential apply/build/restore/apply/build/restore sharing .lake cache; variants never combined (one atom per variant; combined mutant would be ambiguous cause per S3 D5).
- targets/closure: `["sequential builds in shared scratch"]` / C-VALIDATE + C-VOTEFOLD closures sharing cache
- timer/exit: Shared cold ms + per-variant build/restore ms (m-batch-shared-*.ms); comparator parses statically; timeout: `600s + 300s per build + 15s`
- expected semantic evidence: Both span-bound REDs observed sequentially; shared-cache timing vs separate sum (with setup/restore) compared by comparator.
- wrong-reason rejection: Mutating several atoms in one subject is one mutant with ambiguous cause, not independent kills (S3 D5). Shared scheduling fine, combined mutation not.
- loading evidence: Built/Replayed in shared log (per-variant)
- restoration: restore.sh after each variant in shared; porcelain empty between variants
- budget charge (including failure): `1 U-SHARED-BATCH cold + 2 builds + 2 restores (including failure)`
- frozen scripts: instruments/batch-plan.sh sha256:0e95e35df63409b39976c5b374f0b7fbd2f929f561b67a2ae55949b5680a09f1

## 26. M16-BATCH-COMPARATOR — U-COMPARATOR-STATIC — batch comparator (static, no project execution)

- scratch/source/output: `N/A (static)` / `N/A` / `/tmp/reactivegas-ms2-measure-output/m-batch-compare.stdout`
- base/toolchain: `3590c0015b84fd58004bf6fb44dd18b107304c48` / `N/A (static)`
- executable hashes: N/A (parses/hashes files only; never launches project code)
- input hashes: `{"separateA":"<from M13 ms/exit>","separateB":"<from M14>","shared":"<from M15>"}`; mutation hash: `NONE`
- argv: `["bash","/tmp/reactivegas/ms2/e-lean-compliance/s3-successor-spec-1/handoffs/instruments/compare-batch.sh"]`; cwd: `/tmp/reactivegas-ms2-measure-output`
- env/LEAN_PATH: `{}`; initial cache: N/A
- targets/closure: `["parse m-batch-*.ms/.exit/.stdout; report separate sum vs shared sum with setup/restore included"]` / Static only
- timer/exit: N/A (static; own wall not charged as project execution); timeout: `60s`
- expected semantic evidence: Separate-vs-shared wall sums with setup/restore; samples/extrapolations labelled; no unsupported all-row upper bound.
- wrong-reason rejection: Measuring one mutation per import root is sampling; it does not establish every row cost/batching/upper bound.
- loading evidence: N/A
- restoration: N/A
- budget charge (including failure): `0 project execution (static analysis; not charged as build/elaboration/replay)`
- frozen scripts: instruments/compare-batch.sh sha256:4cd26f4654ee07a27e78866b0d323c703d0905d6bdfff633891ed95e13d00eac

## History preserved

R-BUILD2 (Step 1.2s / Predicates 445ms / RI fail 7.6s at `:407` guard-conjunct; exact 1-line `(0<v)->(0<v+1)` diff), R-BUILD3 (restore 27 jobs GREEN, no timings), R-CORPUS (`true`, no timing), R-TRACE (43 checks + 7 negative controls, no timing), SS0 `OP1 15980ms / OP2 19819ms / OP3 3125ms / OP4 2476ms` retained as anchors; this request extends, never rewrites, them. 4-against-3 overrun stays on record. F-07 historical isolation gap stands retrospectively, cured prospectively by scratches above. Measured (SS0/R-BUILD2 samples) vs extrapolated kept distinct; incremental-slower-than-cold (19.8s vs 16.0s) refutes universal cheaper-incremental, not stable comparative cost.

## Grant needed

Enumerated list above is finite and reviewable (cold first, 8 chains × build+restore, isolated check, replay prod + 2 exec + red control, separate/shared batch + comparator). Commissioner `%503` must grant explicit numeric ceiling equal to enumerated invocation count split by substantive/elaboration/replay/other + wall clock, zero retry/exploration unless separately granted, before any execution seat runs it in order, cold first, restoring after each separately admitted variant, stopping at ceiling or instrument/loading defect (RED recorded + restore; unexpected failure charged+returned, not repaired).