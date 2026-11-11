# Submission receipt — S86 commit owner, candidate 38c6d06 (submission 1 pending E-route)

- base: `6ec3ce3` (docs-only mandate freeze; production == `9c8756a`)
- candidate: `38c6d06` GREEN-COMMIT parent=`6ec3ce3`, verify-commit-handoff OK
  vs frozen `handoffs/green.diff` (`bb060026…`), commit-gate OK
- gate: `./gate.sh` v2 `3579e71c…9626` (hash-bound, unchanged)
- RED evidence: `handoffs/gate-red-confirm.log` `d1e3ec76…` (exit=1,
  byte-identical to ticket `evidence/gate-red-v2.log`)
- GREEN evidence (final, 9/9): `handoffs/gate-green-final.log` `7d331a0a…`
  (exit=0, every row PASS; partial 8/9 run `cbefc1e5…` was NOT green and
  is retained only as provenance)
- full ticket gate: `nix develop --quiet -c just ci` exit=0 elapsed=122s
  (toolchain, cabal build, format-check, hlint, lean, corpus-gate, verify)
- emitter bytes: identical to `9c8756a` (`73a077fc…` 14494B, `1f173aec…`
  7673B; re-emit cmp + manifest clean in normal and `--ignore-environment`)
- size: prod+config diff 51 insertions / 2 deletions (budget ≤150)
- fence: changed set == owned 3 files; ADDITIVE-ONLY PASS; lakefile untouched;
  no theorem/guard/step/state/`Trace`/schema/`docs`/kelgroups touch
- reliance: `handoffs/RELIANCE.md` `5c3732a8…` (5 rows, all enforced:NONE)
- builds: owner-build-1 lake-build-corpusExport exit=0 17s;
  clean-verify(ignore-env) exit=0 2s; just-ci exit=0 122s (ticket total 3/8)
- submissions: 0/2 consumed (PROOF-COMPLETE withheld pending E routing)

## Row map (invariant → proof → RED → GREEN → result)

- I86-A (BLOCKING): ci.yaml gains additive `just lean-corpus-verify` step,
  `just lean` legs intact. RED: A-PATH/A-REMOVAL FAIL (wiring absent).
  GREEN: A-PATH PASS, A-REMOVAL PASS (1->0), A-EXEC PASS
  (corrupt-rejected exit=1, clean-accepted exit=0) + clean-env
  `just lean-corpus-verify` exit=0. Remote-CI leg owned by ticket owner.
- I86-B (BLOCKING): `jq` added to `nix/project.nix` shell.buildInputs.
  RED: B FAIL + ticket clean-jq-probe exit=127 (omission baseline).
  GREEN: B-DECLARED PASS; shell `command -v jq` → nix-store jq-1.8.1 in
  normal and `--ignore-environment` shells (attribution=declared, not host
  `/run/current-system/sw/bin/jq`).
- I86-C1..C4 (BLOCKING): `checkEconContext`/`checkIntContext` wired into
  `checkEconFile`/`checkIntFile` (value equality + nonzero member extents).
  RED: all 4 mutants applied-in-shell and accepted exit=0 (true survivors).
  GREEN: all 4 killed exit=1 (C-CONTEXT-BOUND PASS); clean files still exit=0.
- I86-C-CLAIM (BLOCKING, wording): module comment carries exactly
  "this live-call/derived-ToJson method does not establish
  serializer-instance independence".
- I86-D (BLOCKING): 2-arg write arm refuses when `args[0]=="check"`;
  all other check-headed arities already fell into usage-exit-1.
  RED: exit=0 + wrote file `check` + overwrote target (proven).
  GREEN: `check <one-path>` exit=1 dir-identical (sentinels+dircmp);
  `check`/`check a b c` variants exit=1, no writes.
- I86-KEYS (BLOCKING): key-set programs untouched; ENVELOPE-CLOSED PASS.
- I86-ADD (BLOCKING): 3 owned files only; model/corpus bytes identical.
- I86-E (ADVISORY): content authored `handoffs/CORPUS-COVERAGE.md`
  `f6dd0df4…` (current hashes, zero UNPROVED, dated entry + dated pre-S1
  history, vote hole + provisional list + replayer table kept; stale
  `Trace.lean` comment recorded, no model edit). ROUTED by ticket owner
  (hash unchanged); E-row PASS on final 9/9 run.

## Salvage (no draft authorized)

draft=NONE → draft_changed_lines=0, retained=0, salvage_ratio=0 (vacuous),
owner_delta_lines=53. Owner wall ≈ RED-proof + impl + 3 verification
commands; tokens unavailable (no native telemetry).
