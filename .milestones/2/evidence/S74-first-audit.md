# Commit Audit

- Submission: 1/2
- Base: `507bc793f39bdd6092a09cefc607e117d9761743`
- Candidate: `fed19b338a1a6329ce56a2b1830702ad5579f5a1`
- Mandate: spec/plan/modules/data/functions/tasks blobs `f15026a7 b6063d8d f6ca56d1 1952b6fd 0f5ab83a d58735b2`
- Scope: FULL `507bc79..fed19b3` (6 files, +113/−0)
- Verdict: FINDINGS
- Audit loop: submission `1/2`; next submission `ALLOWED`
- Ceiling raises: `0/2`; ledger this brief (no raise)
- Campaign: OPEN — ended by none (two BLOCKING rows still OPEN)
- Builds: `1/3` this ticket; this audit `1`, `cache=cold` then warm reruns
- Gate: v3 sha256 `66ea7cb62676d3405b503b6bb1204cedff5f0d01f557e411159353f3b343816a`
- Issue: https://github.com/paolino/reactivegas/issues/74

## Invariant matrix

| Invariant | Severity | Verdict | Row state | Proof / evidence |
|---|---|---|---|---|
| G74-CALLS-EXISTING | BLOCKING | FAIL | OPEN | THIS candidate: dump of live `seedCorpus`/`emitIntegratedCorpus`/`corpusInitial` equals frozen wrapper fields (5 traces / 32 events / 7 steps), `lean_exe` present. Shipped check is `grep -q seedCorpus` (`gate.sh:54`). Restatement keeping the names in comments emits empty traces and still export+verify 0. |
| G74-ENVELOPE-CLOSED | BLOCKING | FAIL | OPEN | THIS candidate: keys `{auth,traces,view}` / `{auth,initial,steps}`; `structure Trace` only in `Trace.lean:254`; `ToJson GroupView` unsynthesizable from a `Trace` import. Shipped check does not read emitted keys. Third-field mutant export+verify 0 with `extra`. |
| G74-VERIFY-FAILS-CLOSED | BLOCKING | PASS | KILLED | Clean export/verify 0. One-byte json → verify 1 (`cmp` byte 1) → restore sha `91526dc6…f5e86` → 0. Manifest corruption → verify 1 → restore sha `8b055ba6…c5b7bbb` → 0. Stub `lean-corpus-verify: exit 0` → `GATE-FAIL G74-VERIFY-FAILS-CLOSED verify exit 0 on mutated corpus file`. In-gate controls ran inside cold `./gate.sh`. |
| G74-RECORD-HONEST | ADVISORY | PASS | RESIDUAL | `docs/` byte-identical to `e6c5924` and `507bc79`. `commit-owner/handoffs/coverage.md` present and content-honest (counts, vote hole `Step.lean:140-142`, assenso, provisional #68+#69, replayer table). Gate does not check coverage presence. |
| G74-ADDITIVE-ONLY | BLOCKING | PASS | KILLED | Diff is the 6 owned paths only. Docs-touch untracked file → `GATE-FAIL G74-ADDITIVE-ONLY docs/ touched`. No change under `Eventi/` `Core/` `Lib/` `Voci/` `Server/` `docs/` `paolino/kelgroups` `lean/KelGroups/` or existing model modules. |

## Failure modes altered

- File write (`IO.FS.writeFile` in `CorpusExport.lean:77-78`): new acquisition. Wrong arity → `main` returns 1. Write error raises `IO.Error`; `just` `set -euo pipefail` fails closed. Observable.
- `lake build corpusExport` / missing `lake`: export/verify non-zero (owner recorded bare `./gate.sh` 127; this audit ran under `nix develop`). Observable.
- none altered -- checked: no ports/sockets/locks; no `async`/`fork`/background tasks; no mutex/MVar/TVar swap; no prior exporter degradation path to drop. Diff is additive leaf `lean_exe` + just recipes + frozen JSON.

## Test / value / failure-mode coverage (per row)

| Row | Test | Value | Failure-mode |
|---|---|---|---|
| G74-CALLS-EXISTING | grep is a lead. Independent `#eval Lean.toJson seedCorpus` (compiled oleans) equals frozen `.traces`. Restatement-to-empty still passes export+verify. | Dump values are the real 5/32/7, not a shared empty default. Verify compares emitter to frozen files; both sides can collapse together (restatement survivor). | none altered -- checked: restatement does not change process failure signalling; it changes oracle bytes. |
| G74-ENVELOPE-CLOSED | Gate checks `ToJson` spelling and `structure Trace` in the new module, not emitted keys. | THIS candidate key sets are the bound set. Third-field mutant is a distinct extra key, not a degenerate default. | none altered -- checked: extra JSON field does not change IO failure signalling. |
| G74-VERIFY-FAILS-CLOSED | In-gate + auditor manual controls execute `just lean-corpus-verify` on mutated bytes. Stub-exit-0 is killed by the in-gate control. | First-byte mutation and first-byte manifest corruption are distinct from the clean files (sha-proven restore). | Verify-on-drift is the failure mode this row adds; stubbing it silent is the mutant the gate now kills. |
| G74-RECORD-HONEST | `git diff --quiet e6c5924 -- docs/` (tracked only). Coverage presence is not gated. | n/a (prose). Counts in coverage match frozen JSON (5/32/7). | none altered -- checked: no runtime path. |
| G74-ADDITIVE-ONLY | Gate quantifies `e6c5924...HEAD` plus untracked vs forbidden path regexes. | n/a (path set). | none altered -- checked: fence is static. |

## Residuals

- G74-RECORD-HONEST: coverage presence and prose honesty are ADVISORY. Gate checks only tracked `docs/` identity. Named owner: ticket owner. Follow-up: T7403 route to #71. Honest limit: a missing or false coverage file would not fail `./gate.sh`.

## Candidate invariants

None (unratified). The two blocking findings already name the unguarded properties (live-def binding; emitted key set).

## Onward discoveries — outside this ticket

See `onward-discoveries.md`. One item, `RECORDED, NOT-OPENED`: owner receipt last-nibble mismatch on `neg-verify-restored.log` (`…bfb8` quoted vs `…bfb2` on disk). Recipient: epic owner pane `%504`.

## Blocking findings

1. G74-CALLS-EXISTING `lean/Reactivegas/CorpusExport.lean:59` and `gate.sh:54` — shipped proof is identifier presence. Mutant `instruments/mutant-restate-comment-only.lean` (sha256 `a6febf97daff46f69e5ef81a9f76d1aeec41ba29cfd9f85f6e31c20751771ac3`) replaces `Lean.toJson seedCorpus` / `Lean.toJson Reactivegas.emitIntegratedCorpus` with empty JSON arrays, keeps the names in comments, then `just lean-corpus-export` and `just lean-corpus-verify` exit 0 with `ntraces=0` `nsteps=0` (`evidence/warm-restate-econ.jq`, `evidence/warm-restate-verify.log` sha256 `373edce40d067a96fa8f0da0cd09be021c8490de7da1be288cd164fcc3680dda`). **Property class:** wrapper `traces`/`steps` must be bound to the live values of `seedCorpus`/`emitIntegratedCorpus` by an executable check that fails when those calls are replaced by a distinct value while the identifiers remain as source text. Independent dump of this candidate equals frozen fields (`evidence/dump-compare.txt` sha256 `ff7d7ff47b1ce3f8f06b6c19ace106db0a6e5f4d6cebea0a2fd6a4c1b07059e9`); that snapshot is not a shipped property. Drop-name (identifiers removed) does fail grep (`evidence/preflight-drop-name.log` sha256 `9672bcdbf1fe9fc4fed360b3b75620ccbb28da75695307a11e11bc5d07479d62`).

2. G74-ENVELOPE-CLOSED `lean/Reactivegas/CorpusExport.lean:55-68` and `gate.sh:61-69` — shipped proof does not inspect emitted wrapper keys. Mutant `instruments/mutant-third-field.lean` (sha256 `f07f17881970ead5322a41512825eee11caffd56a9092dd25fb869468b58fd69`) adds `"extra"` to both wrappers; export+verify exit 0 with keys `{auth,extra,traces,view}` and `{auth,extra,initial,steps}` (`evidence/warm-third-econ.jq`). **Property class:** emitted wrapper key set must be exactly `{view,auth,traces}` and `{initial,auth,steps}` by an executable check on the bytes `corpusExport` writes. THIS candidate currently has those key sets; `ToJson GroupView` is absent from `Trace` (dump failed to synthesize the instance).

## Verification receipts

| Command | Exit | Duration | Evidence |
|---|---:|---:|---|
| `nix develop --quiet -c ./gate.sh` | 0 | 137753 ms, cache=cold | `evidence/cold-gate.log` sha256 `9069c8f9754f45b7c838438e09025f47d58710d21d6cca85994a94fcc90caa8d` (`GATE-PASS S74 all rows green`; includes `just ci`) |
| `nix develop --quiet -c just lean-corpus-export` | 0 | 1438 ms, cache=warm | `evidence/warm-export.log` sha256 `87425c57c52891b54fcdc57a428155f4656875daac84836d12d585eb9263056e` |
| `nix develop --quiet -c just lean-corpus-verify` | 0 | 1448 ms, cache=warm | `evidence/warm-verify.log` sha256 `3f84247fa97b14daf51cc9ea32ef8feb8c295d7e02adda39ed49e9092e0034ff` |
| verify after one-byte json mutation | 1 | 10022 ms, cache=warm | `evidence/warm-neg-mutated.log` sha256 `44df778facd711209f1c145cfa6ed1d8ad5a18959c6902bde5b527022804ed3e` (`cmp` byte 1) |
| verify after json restore | 0 | 1432 ms, cache=warm | restore sha `91526dc6bf821979fabf516c2e5831a1594de1e171936b235ec36a08154f5e86`; `evidence/warm-neg-restored.log` sha256 `3f84247fa97b14daf51cc9ea32ef8feb8c295d7e02adda39ed49e9092e0034ff` |
| verify after manifest corruption | 1 | 10069 ms, cache=warm | `evidence/warm-neg-manifest.log` sha256 `91cc9cf8a54aed7501644d208fb0b5aaecab3a38daff0be172878a77f0f18ba4` |
| verify after manifest restore | 0 | 1456 ms, cache=warm | restore sha `8b055ba6a28af1bdafa8234778fd693f3b355d01caa172cb6985262e0c5b7bbb`; `evidence/warm-neg-manifest-restored.log` sha256 `3f84247fa97b14daf51cc9ea32ef8feb8c295d7e02adda39ed49e9092e0034ff` |

Worktree `git status --porcelain` empty and `HEAD=fed19b3` after every run.

## Lean surface (domain)

No new theorems, `sorry`, or axioms. Exporter is a `lean_exe`. `#eval Lean.toJson seedView` from a `Trace`-only import fails to synthesize `ToJson KelGroups.GroupView` (instance lives only in `CorpusExport.lean:29`). `#eval` of `seedCorpus` / `corpusInitial` / `emitIntegratedCorpus` used compiled oleans; that printer is a lead; compiled `corpusExport` plus JSON equality is the evidence rung. Lean affordance used: `lake env lean` `#eval` dumps; full rebuild per mutant of `CorpusExport` only (warm, ~1s compile + 3s link).

## Advisories

- Frozen instruments (runtime root `instruments/`): restatement-comment-only, third-field, drop-name, stub-verify, docs-touch, dump-binding. Hashes in `evidence/instruments.sha256`. Seed evidence for the two blocking property classes above; not shipped tests.
- Owner `reliance.md` (7 ADVISORY/NONE rows) already names unpinned 5/32/7 counts. That reliance is consistent with finding 1; it does not lower G74-CALLS-EXISTING severity.
- `just lean-corpus-verify`'s `sha256sum -c` checks frozen files against the manifest, not the temp re-emit. `cmp` is what binds re-emit to frozen bytes. The in-gate controls still fail without `cmp` on file/manifest mutation; emitter drift without `cmp` would not. Covered by finding 1's property class.
