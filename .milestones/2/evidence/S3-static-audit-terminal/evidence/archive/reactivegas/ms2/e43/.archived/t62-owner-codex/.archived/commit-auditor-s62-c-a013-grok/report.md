# Commit Audit

- Submission: `1/2`
- Issue: reactivegas `#62` slice `S62-C` campaign A013
- Base: `6a56f90115e7624830da769da55a1ce1a3c5f5e3` tree `911467a2829dfe311a45b3c35d116c8577a85142`
- RED: `61d01c5db5326a8f7216c6bd15d9eca3ada8c2de`
- Candidate: `76f952dbc859115bc14838ff91d1a93ef107d7d5`
- Candidate tree: `f0f795906e4e26aa68830622055f70be0e42527c`
- Mandate: `9d67265d33122d6240c884ebb5d7dfc94968f3ea3c0d0903d7f4d432a2128ac1`
- Frozen A013 gate: `0b85e667ac54313145123ae721d37376b16ccfcf8fc901e8ce727661d0bd8a39`
- Scope: FULL `base..candidate` limited to A013 wiring plus carried killed rows; active property `G62-C-TRACE`
- Verdict: PASS
- Audit loop: submission `1/2`; next submission `FORBIDDEN`
- Ceiling raises: `0/2`; none authorized
- Campaign: CLOSED — ended by SET-POINT
- Builds: `34/40` this ticket; this audit `3`, mutant `cache=cold`, A013 gate `cache=cold-worktree`, ticket gate `cache=warm`
- Remote: NONE (candidate absent from local `refs/remotes/origin/*`; no fetch)
- Recommendation: ticket owner may accept A013 at this SHA. Auditor does not accept.

## Invariant matrix

| Invariant | Severity | Verdict | Row state | Proof / evidence |
|---|---|---|---|---|
| G62-C-THEOREMS | BLOCKING | PASS | KILLED carried | Production blobs identical to base; `GATE-ROW G62-C-THEOREMS PASS` in `evidence/a013-gate.log` `716afe5dd56b90772a556aa1f2d299a6f9279b95ec85d84ffee4bcab0ded415d` |
| G62-C-ECONOMY | BLOCKING | PASS | KILLED carried | Same blobs; `GATE-ROW G62-C-ECONOMY PASS` |
| G62-C-EXHAUSTIVE | BLOCKING | PASS | KILLED carried | Same blobs; `FALSIFY-OK constructor-seeds count=6`; `GATE-ROW G62-C-EXHAUSTIVE PASS` |
| G62-C-INHERITED57 | BLOCKING | PASS | KILLED carried | `KelGroups/**` empty delta; includes DISJOINT; `GATE-ROW G62-C-INHERITED57 PASS` |
| I57-01-BOUNDARY | BLOCKING | PASS | KILLED carried | `Step.lean` blob `06b2d12eb3dc09a060f99f88297290ac776c13dc` = base; one `applyVoteEventChecked` in `voteApply`; `checkI57Boundary` still in unchanged `Invariants.lean`; inherited row PASS |
| G62-C-TRUST-CI | BLOCKING | PASS | KILLED carried | Zero escape-hatch drift; `GATE-ROW G62-C-TRUST-CI PASS`; shipped `just ci` re-executed in A013 and ticket gates |
| G62-C-TRACE | BLOCKING | PASS | KILLED | Value-level carried (`Invariants.lean` blob `f14bbd7614fe29d1680c7f97c6f84a3df7e8eaa7`). Wiring: tracked `lean/Reactivegas/CorpusGate.lean` byte-equal to frozen fixture `62bd769fe5ba2e02b3826d78ca73f15090a111c96805d7783dae8a6812bc8c4f`; tracked `lean-corpus-gate` runs `lake env lean Reactivegas/CorpusGate.lean` and requires exact `true`; tracked `ci` calls it after `just lean`. Mutant below makes that same `just ci` RED at `lean-corpus-gate` with no Lean elaboration error. Restored candidate GREEN. |

## Active mutation matrix

| Mutant | Edit | Applied | Command | Exit | Kills |
|---|---|---|---|---:|---|
| TRACE-FALSE-AND | insert `  false &&` after `def checkIntegratedCorpus : Bool :=` in throwaway only | sha256 `3e6349021426a31ebdf402ab145d9f202b1906c056a889ae5971a856ce5095b8`; git blob `e894471beb3e386982365cadeda3e30562f6d999`; `evidence/mutant-applied.txt` / `mutant-diff.txt` | `nix develop --quiet -c just ci` | 1 | `Build completed successfully (27 jobs)` then `error: Recipe \`lean-corpus-gate\` failed with exit code 1`; `error(lean` count=0; `.lean:N` error count=0. `evidence/mutant-ci.log` `a0318b2098112f32d4e3d51ea2ea3105ecb7797c26886f20f2fececb98eb0f85` duration 95451 ms `cache=cold` |

Throwaway `/tmp/reactivegas-a013-audit-mutant.XPABaV` created detached at the candidate, mutated, run, then `git worktree remove --force` + prune. Audit worktree HEAD/tree/Invariants blob unchanged throughout.

## Ancestry and fence

Two-commit stack from base, verify-commit-handoff PASS on both frozen diffs:

| Commit | Parent | Paths | Lines |
|---|---|---|---|
| RED `61d01c5db5326a8f7216c6bd15d9eca3ada8c2de` | base | `lean/Reactivegas/CorpusGate.lean` (+3) | 3 |
| GREEN `76f952dbc859115bc14838ff91d1a93ef107d7d5` | RED | `justfile` (+8) | 8 |

`base..candidate`: 2 files, 11 insertions, 0 deletions, modes `100644`. Full-index diff sha256 `feec1200f65f06aa5a7a1e019034d2b1aacf9f929f3bc15adfb85f345196236d`. Fence ≤2 paths / 40 lines.

Forbidden production blobs identical to base:

| Path | Blob |
|---|---|
| `lean/Reactivegas/Step.lean` | `06b2d12eb3dc09a060f99f88297290ac776c13dc` |
| `lean/Reactivegas/Types.lean` | `3d55e72e9cb38c2f47558d217c7391e03bd64ed8` |
| `lean/Reactivegas/Invariants.lean` | `f14bbd7614fe29d1680c7f97c6f84a3df7e8eaa7` |
| `lean/KelGroups/**` | empty `name-status` |

No Nix, Haskell, workflow, planning, or dependency delta. Handoffs `red2.diff` `0489516e6d1b3e439c921751b51c6b8a3a5d35a1e9f0cf7cb106a54ee5407fd9` and `green2.diff` `2641c938842821f9fd768fabeda16a948756a361fa7189cd3094319c2a8847fb`. Receipt `808151b0479a02ab4752ecef342963ec2ba48775c89b91012e9f347e614a1891`. Both commits pass `commit-gate`. Detached audit worktree clean at candidate before and after every run.

Frozen contracts hashed before first inspection and after last gate; all eight match the brief.

## Reliance and candidate invariants

Owner reliance (`handoffs/reliance-declaration.md`) checked, not re-ratified:

- `INV-62-CORPUS-TRUE-AT-BASE` — enforced: shipped recipe `#eval`s `Reactivegas.checkIntegratedCorpus`; `false &&` mutant makes tracked `just ci` RED.
- `INV-62-EVAL-STDOUT-EXACT` — partial: GREEN stdout satisfies `[[ "$result" == "true" ]]`; format drift would fail closed. Not proved against future Lean printers.
- `INV-62-CI-ORDERING` — partial: tracked `ci` calls `just lean` then `just lean-corpus-gate`; a reversal would fail loudly on missing oleans.
- `INV-62-LAKE-ENV-PINNED` — relied on existing `lean-toolchain-contract`, not authored here.

Auditor candidate invariants: None.

## Failure modes altered

none altered in the silent-loss classes — checked: no new sockets, files, locks, or connections; no work moved into a thread; no synchronisation primitive swap; no previously graceful degradation path removed.

The change adds one observable failure signal: tracked `just ci` now exits nonzero when `checkIntegratedCorpus` is not the exact stdout `true`. That path was demonstrated RED by TRACE-FALSE-AND and GREEN on the restored candidate.

## Residuals

None. Residual acceptance is forbidden; no ADVISORY remainder.

## Onward discoveries — outside this ticket

See `onward-discoveries.md`. One item (`OD-A013-BANNER`) `RECORDED, NOT-OPENED` for epic #43. Not a finding in #62.

## Blocking findings

None. Finding count: 0.

## Verification receipts

| Command | Exit | Duration | Evidence |
|---|---:|---:|---|
| throwaway `nix develop --quiet -c just ci` | 1 | 95451 ms | `evidence/mutant-ci.log` `a0318b2098112f32d4e3d51ea2ea3105ecb7797c26886f20f2fececb98eb0f85` command `1f96c71604d37c665a69211b9ccd9656b76fd324d6278ff3fa17c289c371a5b2` free_space before 51816044 KiB `/` after 46834560 KiB `/` (throwaway since removed) `cache=cold` aggregate 32/40 |
| `nix develop --quiet -c gates/a013/gate-s62-c-a013.sh <audit-wt>` | 0 | 96876 ms | `evidence/a013-gate.log` `716afe5dd56b90772a556aa1f2d299a6f9279b95ec85d84ffee4bcab0ded415d` command `51e14fffce6c95bf9f659fdee3d9f78a2d8f4eeaeb269d36704f0cad299ef435` `cache=cold-worktree` aggregate 33/40 |
| `nix develop --quiet -c gates/gate.sh.frozen ticket` | 0 | 77492 ms | `evidence/ticket-gate.log` `7fd7f5be508b7320fb277d3e99136c6e77973a18f8085edf80c0b8ae480984e8` command `bdf63bb6744ea04bcfcffbc2692d6c1de64d180b09420e6853fd422aa9a1f13d` `cache=warm` aggregate 34/40 |

A013 gate: all six static `GATE-ROW` PASS + `SLICE-GATE S62-C-A013 PASS scope=wiring-only shipped-ci=corpus-evaluator carried-killed=6`. Ticket gate: `S62-A PASS rows=4`, `S62-B PASS rows=6`, `S62-C PASS rows=6`, all `full-ci=pass`.

## Build accounting

Ticket aggregate begins 31/40. This audit spent 32–34 (3 of 3 reserved). No ceiling raise. No further substantive start authorized on this seat.

## Advisories

None mandate-grounded. Point-mutant evidence is `evidence/mutant-diff.txt` sha256 `fdd69684768e54e94a68bbd6af1e19cd3ac8bec43362c0e6b428a09e481f5c91`; not a shipped instrument.

## Recommendation

Recommend A013 acceptance of candidate `76f952dbc859115bc14838ff91d1a93ef107d7d5`. Do not treat this report as acceptance.
