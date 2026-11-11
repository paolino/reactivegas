# Commit Audit

- Submission: `1/2`
- Base: `6a57a836a25d85ce10c923309ea3f4adf1a7c4db`
- Candidate: `2f2a327f4e1dafa27216aeabe1d36095ea25bcd8` (tree `58809294bde06a11cbd55808458cc558d75d1556`)
- Mandate: `3be23d02e5aab37cef399d435c5e639d48825107808d344e7e38eb0ac04e67b0`
- Scope: FULL `6a57a836..2f2a327f`
- Verdict: `FINDINGS` (`4` blocking findings)
- Audit loop: submission `1/2`; next submission `ALLOWED` for this report's named findings only
- Ceiling raises: `1/2`; ledger `/tmp/reactivegas/ms2/e43/t62-owner-codex/ceiling-ledger.md` sha256 `1bbbbee58e296de238792e4cafac88610d8f41c7639e0f95eb3144f091b5acdb`
- Campaign: `CLOSED` — ended by `SET-POINT`; `killed=3 residual=0 blocked=3 open=0`
- Campaign ledger: `campaign-ledger.md` sha256 `f033dd3b1e78c237798d49f6e273d403f3a50d9cf1dcc7c3373fd62b4c2f3799`
- Builds: `31/40` this ticket; this audit `3/3`, cache=`cold,warm,warm`

Provenance and scope passed: detached clean candidate, exact tree, base ancestry, no remote containing branch, eight regular `100644` Lean paths only, and exact GREEN handoff. Evidence: `evidence/provenance.log` sha256 `2ec4633cbe1e1641479949395b7ca879612f0eb4f93a11788c1990d01c7bd980`; `evidence/handoff-verify.log` sha256 `f0198a6c42b6132004075c3dc7c17b57fdb1a8e17829909521f0cff608b44ddb`.

## Invariant matrix

| Invariant | Severity | Verdict | Row state | Proof / evidence |
|---|---|---|---|---|
| `G62-C-THEOREMS` | BLOCKING | PASS | KILLED | Historical block independently hashes to accepted `ab9b4aadb52fbbcdb62bb8de39f62acbc76f0ffbfa4c8eeb5d1d79f6fff334f4`; concrete cleanup/recompute witnesses are non-degenerate and focused elaboration prints only allowed axioms. `evidence/static-trust-and-history-v2.log` sha256 `968fc50fbeb0c3f47759e3cdf2de2ff324c740b2c3ea199ee3342435c2b21a71`; focused proof below. |
| `G62-C-ECONOMY` | BLOCKING | PASS | KILLED | Production receives explicit `BackdonateAuth`; the canonical two-member fixture distinguishes payload-local ghost/comune keys and verifies per-member credits plus comune debit. Focused proof sha256 `76c32a87d1099fe0d3fb3cbae84fa9e408afaa6e9d454a433e4e6c02999b5b21`. |
| `G62-C-TRACE` | BLOCKING | FAIL | BLOCKED | The claimed corpus has no integrated JSON type/emitter/replayer; its rows are independent `apply` calls from one `mixedGroup`, and `replayIntegratedCorpus` checks length only. `checkIntegratedCorpus` has no caller/theorem/gate consumer. F-TRACE; `evidence/semantic-findings.log` sha256 `9a4ae25dc225ff89ecee13b8f310c5116cf7f12a778b6712352d48cd7afac979`. |
| `G62-C-EXHAUSTIVE` | BLOCKING | PASS | KILLED | App/direct/proposal/base-change/vote/verdict eliminations are closed and wildcard-free; fresh seeded constructor selftests and the full ticket gate pass. Full-gate evidence below. |
| `G62-C-INHERITED57` | BLOCKING | FAIL | BLOCKED | I57-01 makes two validation decisions; DISJOINT/FRANCHISE/POLICYFREE checks do not exercise the integrated path; I57-10's real pin/runtime identities mismatch. F-I57-ONE-DECISION, F-I57-INTEGRATED-LEGS, F-I57-TOOLCHAIN. Frozen instrument `instruments/audit-boundaries.sh` sha256 `6dee1ad1ec83c69b5ddef2e0ac19f2200961586e10ea1ffcab4a98729235b586`. |
| `G62-C-TRUST-CI` | BLOCKING | FAIL | BLOCKED | Escape-hatch, axiom, history, dependency-direction, source, and CI checks pass, but `lean/lean-toolchain` pins `4.27.0` while the fresh commands execute Lean `4.25.0`; the shipped toolchain Bool tests unrelated domain values. `evidence/toolchain-pin-compare.log` sha256 `b018190fde8b05b602995e0e122f3046dd52bbade8d2ad21d571d7adebc3b5d7`. |

## Inherited #57 coverage

| Leg | Verdict | State | Independent coverage |
|---|---|---|---|
| `I57-01-BOUNDARY` | FAIL | BLOCKED | `voteApply` validates, then admitted events call `applyVoteEvent`, which validates again; `checkI57Boundary` stops on a non-member economic event and cannot count the admitted vote boundary. |
| `I57-02-EXHAUSTIVE` | PASS | KILLED | The integrated admin `openQuestion` reaches the live exhaustive vote/app eliminations; constructor seeding makes the classifiers fail to elaborate. |
| `I57-03-NOOP` | PASS | KILLED | Rejected integrated events preserve the full aggregate through `foldIntegrated`; non-member, admission, and rejecting-hook values are distinguishable. |
| `I57-04-AUTH` | PASS | KILLED | A canonical member without admin role is rejected for each of the three closed vote constructors and the integrated fold remains identical. |
| `I57-05-R45` | PASS | KILLED | Stranger influence is inert on a reachable open-question aggregate; the non-admin-member cases in I57-04 cover the vote validator rather than only the outer membership guard. |
| `I57-06-PARTITION` | PASS | KILLED | The real V-3 base enactment moves `q` from open to exactly one closed record. |
| `I57-06-DISJOINT` | FAIL | BLOCKED | The new check inspects the prebuilt literal `v3Question`; it never casts through `apply`/`foldIntegrated` and is insensitive to the production placement mutation. |
| `I57-06-NOSTALE` | PASS | KILLED | A real base enactment changes franchise, sweeps, and closes the formerly open question; the payload change is non-vacuous. |
| `I57-06-FRANCHISE` | FAIL | BLOCKED | The new check inspects literal tallies against one pre-view; no ballot is cast through the integrated path, so it does not establish cast-time franchise. |
| `I57-06-POLICYFREE` | FAIL | BLOCKED | The new check calls `verdictOf` directly with two thresholds; it cannot detect a hard-coded/default threshold injected in `appFold`/`voteApply`. |
| `I57-07-NOEXPIRY` | PASS | KILLED | An admitted integrated vote event preserves an existing open question under unchanged semantics. |
| `I57-08-TRUST` | PASS | KILLED | Independent source scan finds zero escape hatches and focused elaboration prints only `{propext, Classical.choice, Quot.sound}`. |
| `I57-09-DIRECTION` | PASS | KILLED | Dependency-direction script passes with positive control `imports=15`; no `KelGroups` module imports `Reactivegas`. |
| `I57-10-TOOLCHAIN` | FAIL | BLOCKED | Executing Lean is `4.25.0`; tracked pin is `leanprover/lean4:v4.27.0`. The exact comparator exits `1`. |

Test/value coverage was checked explicitly. Passing economic and base-hook fixtures use distinct member/non-member keys, non-zero balances, actual membership/role changes, and a verdict-changing franchise. The failing trace accepts any same-length result list, and the three failing inherited checks use literals/direct calls that cannot distinguish the named integrated-path mutations.

## Residuals

None. All rows are BLOCKING; none was residualized.

## Candidate invariants

None.

## Onward discoveries — outside this ticket

None.

## Blocking findings

1. **F-I57-ONE-DECISION / `I57-01-BOUNDARY`** — `lean/Reactivegas/Step.lean:169` validates with `validateVoteEvent` and, on success, calls `applyVoteEvent`; `lean/KelGroups/Vote/Fold.lean:110` validates the same vote again. The shipped `checkI57Boundary` at `lean/Reactivegas/Invariants.lean:1684` exercises a rejected economic event, not an admitted vote. **Property class:** every admitted integrated vote transition makes exactly one validation decision that dominates `effectedState` and `sweepClosures`; the permanent check must fail on both duplicate-validation and bypass mutants. Evidence: `evidence/semantic-findings.log` and the frozen instrument above.
2. **F-TRACE / `G62-C-TRACE`** — `lean/Reactivegas/Invariants.lean:1578` maps each event independently against `mixedGroup` into typed `Except` rows; `replayIntegratedCorpus` at line 1592 is only length equality. There is no integrated JSON serialization or sequential replay, and `checkIntegratedCorpus` at line 1676 is unconsumed. **Property class:** one serialized signed `IntegratedEvent`/`GroupState State` trace must replay sequentially through `applyIntegratedEvent`/`Reactivegas.apply`, compare every stored value, and cover all required transitions/effects in that same corpus; all-error, reordered, altered-state, and same-length mutants must fail.
3. **F-I57-INTEGRATED-LEGS / `I57-06-{DISJOINT,FRANCHISE,POLICYFREE}`** — checks at `lean/Reactivegas/Invariants.lean:1748`, `:1759`, and `:1766` inspect a fixture literal or call `verdictOf` directly. They do not run the integrated transition and remain green if vote placement, cast admission, or threshold threading is broken at the production boundary. **Property class:** each inherited leg needs a production-rooted, non-degenerate witness/general theorem whose own targeted definition/path mutant is shown red.
4. **F-I57-TOOLCHAIN / `I57-10-TOOLCHAIN`, `G62-C-TRUST-CI`** — `lean/lean-toolchain` pins `4.27.0`, but the fresh focused and gate commands execute Lean `4.25.0`; the exact comparison exits `1`. `checkI57Toolchain` at `lean/Reactivegas/Invariants.lean:1794` instead checks `comuneId` and `s62bThreshold`. **Property class:** a live gate must parse and compare the executing Lean version/commit with the tracked toolchain pin and fail closed on mismatch; a domain-value proxy is not toolchain evidence.

## Verification receipts

| Command | Exit | Duration | Evidence |
|---|---:|---:|---|
| `nix develop --quiet -c /tmp/reactivegas/ms2/e43/t62-owner-codex/gates/gate-s62-c.sh` | 0 | 108847 ms | `evidence/fresh-slice-gate.log` sha256 `f9d8410ac42b133ed99886bd238ac11f41dd2fd01d5004dff5a7e5e4c99ba298` |
| `nix develop --quiet -c bash -lc 'cd lean && lake build Reactivegas.Invariants'` | 0 | 1391 ms | `evidence/fresh-focused-proof.log` sha256 `76c32a87d1099fe0d3fb3cbae84fa9e408afaa6e9d454a433e4e6c02999b5b21` |
| `nix develop --quiet -c /tmp/reactivegas/ms2/e43/t62-owner-codex/gates/gate.sh.frozen ticket` | 0 | 70703 ms | `evidence/fresh-full-ticket-gate.log` sha256 `28d6a8fbaa8b1c59e2cd5f7e249ace8ea3022317af8d00d9b910a05bfc8e1ef8` |
| Frozen boundary instrument, candidate | 1 (finding reproduced) | 76 ms | `evidence/instrument-candidate.log` sha256 `ded8ea2722c2fc9af3d27d467ce02d92f1ef78cf9cc6a3e695f256d0ca9a8aef`; bad fixture also exits 1, good fixture exits 0 |
| Toolchain pin comparator | 1 (mismatch reproduced) | 834 ms | `evidence/toolchain-pin-compare.log` sha256 `b018190fde8b05b602995e0e122f3046dd52bbade8d2ad21d571d7adebc3b5d7` |

Build accounting: run 1 cold, free space `203870765056 → 203796557824` bytes; runs 2 and 3 warm, `203796557824 → 203796557824` bytes. Candidate tracked status was clean before and after every run. Frozen gate hashes matched before and after.

## Advisories

- The #47 `BackdonateAuth` truth value remains unresolved exactly as ratified. The candidate correctly requires explicit caller-supplied authorization and adds no default, inference, or threshold policy.

Report hash is bound by the terminal `AUDIT-FINDINGS` event and companion `report.md.sha256`.
