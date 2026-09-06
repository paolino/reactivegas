# Commit Audit

- Submission: `1`
- Base: `97f9684c0db344dd449e7a0a405ebc06c2e5cb74`
- Candidate: `757dac98aecce705e44eda6c9283a5da01b02827`
- Mandate: `spec.md` `bbdaf1a344be2695c4808d38ad24af627a5fa5bda55781421e0c1d5d881e5bc6`; all five supplied artifact hashes verified before reading
- Scope: FULL `97f9684..757dac98`
- Verdict: FINDINGS
- Audit loop: submission `1/2`; next submission `ALLOWED`
- Ceiling raises: `0/2`
- Campaign: OPEN — `stopped=none`; ledger `handoffs/campaign-ledger.md` sha256 `98c26657f347f53066c5ef1c2e3a7bb631e0f8944eb9dee6026a150bc5765a0f`
- Builds: `9/20` this ticket; this audit `6`, mixed cold/warm
- Provenance/fence: clean detached candidate; exact four-commit base ancestry; eight changed paths, all `lean/KelGroups.lean` or `lean/KelGroups/Vote/**`; modes `100644`; no links; the seven Slice-1 modules are blob-identical to `ccdda83`

## Invariant matrix

| Invariant | Severity | Verdict | Row state | Proof / evidence |
|---|---|---|---|---|
| INV-54-PARTITION | BLOCKING | FAIL | OPEN | `Invariants.lean:706-713` proves only final-container nodup/disjointness and non-open closed verdicts; it never relates an opened ID in the event history to either final container. C3 red: `42beba4a09c86eb3d94188a858873b260d15ffca310040c960c3cbf464a55b8c`. |
| INV-54-DISJOINT | BLOCKING | PASS | KILLED | `ballots_nodup_disjoint` quantifies every open and closed question from `foldVote`; C2 removed the opposite-tally erase and made both the witness and carrier proof red: `203c8a3e3cfbe8952afece5166405296e21ac33edab16cf19f64550e73e9c462`. |
| INV-54-NOSTALE | BLOCKING | PASS | KILLED | `open_questions_are_open` quantifies every production-fold trace under the supplied threshold; C4 skipped non-ballot sweeps and made witnesses/carrier proof red: `736207103c130c59f36d8dfe2b82a94b712526e0671ecba0bf7ff8e77a27d446`. |
| INV-54-FRANCHISE | BLOCKING | FAIL | OPEN | `franchise_of_tallies` (`Invariants.lean:962-970`) exposes only some prefix where the key was responsible, not the cast event; `unfranchised_cast_noop` observes only global key membership, so a prior voter switching tallies after losing standing is outside both conclusions. |
| INV-54-NOEXPIRY | ADVISORY | FAIL | OPEN | `no_expiry` (`Invariants.lean:717-724`) covers only one cast on a distinct question from an arbitrary well-formed state, not every production-fold event that preserves ballots/franchise/proposer. C5 field grep: `1c9a019b840e1a2e5ccb8256d428860c8b66922160cb1d01952405b7c4f11e40`. |
| INV-54-POLICYFREE | BLOCKING | PASS | KILLED | Current `verdictOf` reads the supplied threshold explicitly. A hard-coded `legacyThreshold` mutant made the zero-policy witness red; frozen source `9e738cfb0d4e7794e0c11b6a9bdab88e33c4469f59a406d1595a17317fc28cc5`, log `e0b3703a2462d0116945a809485fc18a72356f5d07247084262116331c6248a6`. |

## Residuals

None. The advisory no-expiry row remains OPEN for repair rather than being accepted as a residual.

## Candidate invariants

None.

## Onward discoveries — outside this ticket

None.

## Blocking findings

1. [R-45 / franchise boundary] `lean/KelGroups/Vote/Fold.lean:80-109` — `openQuestion` is applied without the current-responsabile check and `foldVote` does not invoke `validateVoteEvent`. The production trace containing only `("stranger", openQuestion "q" collective)` under `zeroThreshold` closes `q` positive with empty tallies. Property class: every inadmissible signer/event pair is a production-fold no-op, including opening, or the production fold is intrinsically validation-coupled. Frozen witness `instruments/nonresponsabile-open.lean` sha256 `1f7aa80ace8dfee1fd6240832d5cc04b04241715383e43a0877e7dae5264bc2b`; receipt `dd17626dfdcdea8bfdcb42dab5e835c2875d0e027a742569d20ff1bc99b5e0b6`.
2. [INV-54-PARTITION / R-61, R-68] `lean/KelGroups/Vote/Invariants.lean:706` — the named theorem does not state that every ID ever opened remains in exactly one of open/closed. An empty final pair of containers satisfies its conclusion after silent deletion. Property class: for every production-fold prefix that opens an ID, every extension contains that ID in exactly one final container and every closure verdict is non-open.
3. [INV-54-FRANCHISE / R-44] `lean/KelGroups/Vote/Invariants.lean:834,962` — the public cast-time theorem drops the cast event from its existential witness, while the no-op theorem compares only membership in the flattened global key set. Both admit a former responsabile changing position after losing standing. Property class: identify the actual cast event and its immediately preceding state, and prove any cast with `isResponsabile=false` preserves the complete vote state/tallies.
4. [Toolchain/gate boundary] `specs/54-lean-vote-machine/plan.md:115`, `lean/lean-toolchain:1`, `nix/project.nix:16` — the mandate pins Lean 4.27, but the frozen Nix gate resolves Lean 4.25. The exact 4.27 `lake build` exits 1 on twelve pre-existing `Reactivegas.State` errors (the new Vote modules do elaborate); that required full-build claim is therefore not reproducible inside the owner fence. Property class: the compiler version exercised by the frozen gate equals the hash-bound toolchain version and the complete repository elaborates under it. Evidence `focused-lake-4.27-v2.log` sha256 `12ae178604607641ef24e45c1a0cb01f7c06b913d1292eb74882d4434fc029e8`.

## Verification receipts

| Command | Exit | Duration | Evidence |
|---|---:|---:|---|
| Nix Lean 4.25 focused `lake build` | 0 | 6.187s | `focused-lake-4.25.log` sha256 `d7692497ad21dce30a3ea7ce04f072e84508d469ca9f2ef228e097b5ba7a66f7` |
| Exact Lean 4.27 `lake build` | 1 | 5.167s | `focused-lake-4.27-v2.log` sha256 `12ae178604607641ef24e45c1a0cb01f7c06b913d1292eb74882d4434fc029e8` |
| `./gate-slice-a.sh` | 0 | 84s | `frozen-slice-gate.log` sha256 `09d0bef734e761e7bcef26defeac6119fe1e1e5424cf94e652a05ae497a3830e` |
| `./gate.sh` | 0 | 24.690s | `ticket-gate.log` sha256 `054139b1786edc8c583f95a6c4d3114a8d0d0732b19c31e9a1e2f5f459ce8a82` |
| R-45 production-fold witness | 0 | 1.027s | `nonresponsabile-open-v3.log` sha256 `dd17626dfdcdea8bfdcb42dab5e835c2875d0e027a742569d20ff1bc99b5e0b6` |

## Advisories

- INV-54-NOEXPIRY remains under-specified: quantify production-fold events whose effect preserves the target question's ballots, franchise, and proposer standing. This is advisory by the declared severity, not included in `blocking=4`.
