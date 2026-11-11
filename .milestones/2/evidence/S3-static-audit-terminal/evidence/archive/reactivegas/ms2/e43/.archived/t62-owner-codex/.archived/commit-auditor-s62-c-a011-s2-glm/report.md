# Commit Audit

- Submission: `2/2` (FINAL)
- Base: `bf027aabb764a006604ad5e88b4780c42d264011`
- RED: `ab0fcc151f5489cdeb57dbaedf7f9cc0fe8bbf31`
- Rejected: `b90161cffb478db0855e81e0bc3ab23818bba161`
- Candidate: `6a56f90115e7624830da769da55a1ce1a3c5f5e3` (tree `911467a2829dfe311a45b3c35d116c8577a85142`, parent = rejected, detached clean)
- Base..candidate diff SHA-256 (`--full-index`): `af00cb54aaf8716d3cf2f9ce540d659bb3229e05cfef3ee297985679c0bfbf0a`; verified byte-equal to the owner receipt's binary diff hash
- Repair delta `rejected..candidate`: exactly one tracked file `lean/Reactivegas/Invariants.lean`, `93/64` lines (`157` total), handoff diff SHA-256 `2111da67752dda6389182471b88e61b7be1805c9b164921441d5929f9773c8b4`, manifest `fdb58064c8da1828c8bdc7b65e0691583363654a0601dec3fcf61d3dd5228c36` — both match the brief; every repair-delta file carries a finding-specific reason in the receipt, and that file serves only the two forwarded findings
- Full fence: exactly nine allowed Lean files, `930/130` lines (`1060` total), within `11 paths / 1400 lines`; no spec/task/gate/Nix/workflow/Haskell/dependency/remote delta; zero forbidden proof escape hatches
- Mandate: `9a226235b117b77858ca8f99f8fe442cd6f21d8d86d64898a8ad0cb5c6c1c078` (verified); all six planning artifacts verified at planning commit `fd5c8e0` (tree `dc95547b…`), each blob SHA-256 equal to the brief
- Scope: REPAIR `rejected..repaired` plus the two submission-1 findings (`F-I57-REACHED-DUPLICATE`/`I57-01-BOUNDARY`, `F-TRACE-SERIALIZED-VALUE-COVERAGE`/`G62-C-TRACE`); five carried rows checked on exact blobs/boundaries, not reopened
- Verdict: `FINDINGS` (`1` blocking)
- Audit loop: submission `2/2`; next submission `FORBIDDEN` (campaign ends; re-cut required)
- Ceiling raises: `0/2`; ledger `/tmp/reactivegas/ms2/e43/t62-owner-codex/ceiling-ledger-s62-c-a011.md` SHA-256 `1e03836cd41c1442b71648b34b38f017e4100f82e593e208e80b38675a7c0155`; no raise used or authorized
- Campaign: `CLOSED` — all seven rows terminal (`6` KILLED, `1` BLOCKED, `0` RESIDUAL, `0` OPEN); `campaign=ENDED-RECUT-REQUIRED`
- Campaign ledger: `campaign-ledger.md` (this runtime root), carried from submission-1 ledger `53e5bee359d31919a4c9f3ee343548bdfb82cf2b57935d12c63db3f3fa292896`
- Builds: `23/40` this ticket; this audit `3/3` (`cold,warm,warm`)
- Remote/draft: `NONE`/`NONE` — candidate absent from every known local remote ref (no fetch performed); frozen gate hashes verified identical before and after all runs
- Report SHA-256: bound in the terminal `AUDIT-FINDINGS` event and `report.md.sha256`

## Provenance summary

Pane `%265` in `reactivegas:5 reactivegas-e43-t62-one-membership`; ticket owner `%195` (codex), parked owner `%247` (grok), auditor family `glm` via operator amendment `a0e3e556…` (+ NOTE-024/025), alternation Grok→GLM honored, no-secrets attestation `c09cd8da…` verified and no credential step encountered. Candidate/tree/parent verified from Git; handoff receipt, repair diff, manifest, submission-1 report `29aadedd…` and ledger `53e5bee3…` all hash-verified. Tracked tree clean before and after every run.

## Invariant matrix

| Invariant | Severity | Verdict | Row state | Proof / evidence |
| --- | --- | --- | --- | --- |
| `G62-C-THEOREMS` | BLOCKING | PASS | KILLED (carried) | Historical declaration blob `ab9b4aad…` byte-identical base→candidate; witness theorems print `[propext]`; both fresh gates green. Not reopened. |
| `G62-C-ECONOMY` | BLOCKING | PASS | KILLED (carried) | `canonical_economy_holds` prints `[propext]`; all economy surfaces byte-identical rejected→candidate. Not reopened. |
| `G62-C-EXHAUSTIVE` | BLOCKING | PASS | KILLED (carried) | `exhaustive_inventories_hold` prints `[propext]`; inventory defs untouched by the one-file repair delta; ticket gate executes constructor seeds fresh. Not reopened. |
| `G62-C-TRUST-CI` | BLOCKING | PASS | KILLED (carried) | Fresh escape-hatch scan: zero hits; all printed axiom sets `{propext}`; gate hashes stable; full CI green twice fresh. Not reopened. |
| `I57-01-BOUNDARY` | BLOCKING | PASS | KILLED | Production is exactly one checked decision (slice-gate static closure `production_decisions=1, checked_calls=1, raw_wrapper_effects=0`). Fresh probe: production admits; shipped `voteApplyDuplicate` reaches a second `validateVoteEvent` on the same signer/event and fails; bypass really admits and is caught; the shipped check pattern goes red against a duplicate-production and against the submission-1 pure-duplicate (same-output) shape; `checkI57Boundary` is `decide`-bound (`i57_boundary_holds`, `[propext]`), so the control can fail permanently by breaking the build. |
| `G62-C-INHERITED57` | BLOCKING | PASS | KILLED (carried) | `i57_disjoint_holds`, `i57_disjoint_mutant_caught`, `i57_franchise_mutant_caught`, `i57_policyfree_mutant_caught` all `[propext]`; I57-06 sections outside every repair hunk; `KelGroups/Vote/Invariants.lean` untouched; DISJOINT carried. Not reopened. |
| `G62-C-TRACE` | BLOCKING | FAIL | BLOCKED | The repaired machinery is evaluator-correct (fresh probe: `checkIntegratedCorpus=true`, real `fromJson?` decode length 7, omitted-state emitter dies, corrupted stored coordinate dies, all 7 typed mutants die through real `toJson`, decoded coordinates non-degenerate) but it is shipped unbound: nothing in the candidate ever evaluates `checkIntegratedCorpus`. Evidence below. |

## Exact two-row mutation matrix

| Open row | Mutants / controls | Result | Terminal state |
| --- | --- | --- | --- |
| `I57-01-BOUNDARY` | shipped reached-duplicate (second `validateVoteEvent` on same signer/event, post-apply); bypass (effect+sweep, no validation); duplicate-production pattern; pure-duplicate (submission-1 same-output shape) | duplicate `KILLED` (production ok / duplicate error); bypass `KILLED` (refusal + real admission caught); duplicate-production `RED` (`false`); pure-duplicate `RED` (`false`) | `KILLED` |
| `G62-C-TRACE` | shipped omitted-state emitter; corrupted stored coordinate (departing conto 999); all-error, reordered, altered-state, same-length, omit-event, corrupt-change, omit-signer through real `Lean.toJson` | all 9 `KILLED` **at value level in the fresh instrument**; none observable in the shipped tree because `checkIntegratedCorpus` is referenced by nothing | `BLOCKED` |

## Property-class counterexamples

1. `PC-TRACE-UNBOUND-SHIPPED-CONTROL` (new, decisive): a control `def` with zero referents cannot make any future defect red. `git grep checkIntegratedCorpus` at the candidate returns exactly one hit — the definition (`lean/Reactivegas/Invariants.lean:1908`). `just ci` is compile-only (`lake build` elaborates but never evaluates an unbound def); both frozen gates are textual presence checks that never name it, plus `just ci`. The repair removed the only two prior bindings — `theorem integrated_corpus_holds : checkIntegratedCorpus = true := by decide` and the `checkIntegratedTheoremWitness` conjunct — with no replacement. The candidate's own Json decode path is not kernel-decidable: fresh elaboration probe of `example : checkIntegratedCorpus = true := by decide` fails with "reduction got stuck at the `Decidable` instance" (`evidence/probe-decide.log`), and `native_decide` is forbidden by the repository's own escape-hatch scan. A mutant that guts, deletes, or inverts `checkIntegratedCorpus` (or its decode instances) therefore compiles, passes the slice gate, and passes the full ticket gate. The submission-1 required property class — real serialize→decode→replay with per-coordinate kills — now exists as value but not as shipped proof. Honest limit: the neutron demonstration (rebuild of a mutated copy) was not spent because the three-build budget was exhausted by the instrument and the two mandatory gates; the zero-referent grep plus build/gate wiring plus the `decide` probe settle the claim statically.
2. `PC-TRACE-TRANSPORT` and `PC-TRACE-VALUE` (carried from submission 1) are closed at value level: the fresh instrument shows the omitted-state emitter dies at decode (`fromJson?` on `omittedStateCorpusJson` yields no steps), the corrupted cleanup coordinate dies at full-state replay comparison and at the coverage oracle, `pendingBase`/`pendingProposals` are non-empty at s2, and the departing member's conto is 0 with the comune credited non-zero at s3 (`evidence/probe-main2.log`). Typed-value reuse and comment-only decode are gone from the machinery itself.
3. `PC-I57-PURE-DUPLICATE` (submission 1) is closed: the shipped check pattern requires `.ok production, .error duplicate`; the pure-duplicate shape evaluates red in the fresh instrument, and the shipped reached-duplicate fails on the same signer/event as mandated.

## Test, value, and failure-mode coverage

- Test coverage: the production boundary executes (`voteApply → KelGroups.Vote.applyVoteEventChecked`, exactly one decision, statically closed by the frozen instrument inside the slice gate); the duplicate, bypass, pure-duplicate, and duplicate-production shapes all execute in the fresh instrument; the corpus emitter/decoder/replayer executes in the fresh instrument with all 9 mutants.
- Value coverage: boundary fixtures keep signer identity constant (`"alice"`) across both decisions; the corpus now carries non-degenerate stored coordinates (non-empty pending stores, absorbed conto 0, comune credited non-zero) verified on decoded values, not typed originals.
- Failure-mode coverage: refusal still flows as `Except.error` and is observed; no resource acquisition, threading, synchronization, or external degradation path exists in the Lean delta. What changed about breaking: the trace row's failure signal moved from "wrong but bound" (rejected candidate: decide-bound, vacuous) to "right but unwired" (candidate: correct, dead). A steady-state green build cannot see this — the kill evidence exists only in runtime receipts.

## Failure modes altered

- Trace decode failure is still not a shipped observable: decode errors (`fromJson?` `.error`) are collapsed to `false` inside `checkIntegratedCorpus`, which nothing evaluates. No shipped surface can observe a serialization regression. (Checked: gate rows `row_C_TRACE`, `row_C_INHERITED57`, `row_C_TRUST_CI`; `just ci` recipe; `lake build` semantics.)
- No other failure mode is altered by the one-file delta: vote refusal path, backdonation authorization, cleanup, and recomputation surfaces are byte-identical outside `Invariants.lean`, and the boundary row's refusal observability is unchanged and bound.

## Reliance

No new load-bearing assumption is introduced by the repair delta: no import changes, no new axioms (fresh scans: zero escape hatches; all printed axiom sets `[propext]`), no policy inference, no membership path, no dependency reversal, no historical theorem edit (blob `ab9b4aad…` unchanged). Submission-1's reliance registry verification is carried unchanged.

## Residuals

None. Every row is BLOCKING; no residual disposition is lawful.

## Candidate invariants

None. The single failure is a violation of the already-ratified `G62-C-TRACE` kill requirement, not a new truth.

## Onward discoveries — outside this ticket

None. See `onward-discoveries.md` (`RECORDED, NOT-OPENED` unused; designated owner `reactivegas epic #43 invariant/census backlog`).

## Blocking findings

1. **`G62-C-TRACE` — shipped corpus control is unbound (dead code); no permanent red exists** — `lean/Reactivegas/Invariants.lean:1908` (`checkIntegratedCorpus`), with the whole island (`emitIntegratedCorpusJson` 1749, `omittedStateCorpusJson` 1759, `corpusCorruptCleanup` 1830, `integratedCorpusCoversRequired` 1772, `replayIntegratedCorpus` 1743) referenced by nothing outside the island and `checkIntegratedCorpus` referenced by nothing at all. Observed violation: the repair removed `theorem integrated_corpus_holds` and the `checkIntegratedTheoremWitness` corpus conjunct and shipped no replacement binding; `decide` cannot reduce the check (`evidence/probe-decide.log`), `native_decide` is forbidden, and the frozen gates contain no execution hook for it — so the row cannot terminate `KILLED`. Note also: the slice gate's textual mutant patterns (`!replayIntegratedCorpus (corpusAllError emitIntegratedCorpus)` etc.) are now satisfied only by commented-out lines in the candidate while the executable equivalents are `Lean.toJson`-wrapped — the comments pass the gate, the executable code passes nothing. **Property class for the re-cut:** a shipped runtime control must be bound to a permanent evaluation surface the build/CI/gate actually executes; where a `Lean.fromJson?` path is not kernel-decidable, either restructure the decode to a kernel-decidable equivalent or give the frozen gate an execution hook at re-cut time — evidence-only runtime discrimination does not close a BLOCKING row. Evidence: `evidence/probe-main2.log` (value-correct, all kills red), `evidence/probe-decide.log` (decide stuck), `evidence/build2-slice-gate.log`, `evidence/build3-ticket-gate.log` (green with the defect present), frozen instrument `instruments/probe-main.lean` SHA-256 `1c412668ff7e960e7c79765ddf34385fd99d54b8e2f6c4c1d5fc319e0262a244` and `instruments/probe-decide.lean` SHA-256 `822bc4bb85b3b08a2ef9c22bb7674b525f7a2dec12f1f6131abc196510ec78c5`.

Finding count: `1` blocking, `0` advisory.

## Verification receipts

| Command | Exit | Duration | Evidence |
| --- | ---: | ---: | --- |
| `nix develop -c bash -c 'cd lean && lake build'` (instrument build 1/3, cold) | 0 | 34516 ms | `evidence/build1-lake-build.log` SHA-256 `2c321e3ba65102f90b18c57695af70396490aecb90488455cda9028a57770f64` |
| probe-main first iteration (probe-side type errors; partial results already recorded) | 1 | 23894 ms | `evidence/probe-main.log` SHA-256 `347994d06dd47b91379bc511a4d3b51fed6c2c42f5d8822ca92e9f9fbf09c706` |
| probe-main final (instrument, warm; 21/21 expected values) | 0 | 7710 ms | `evidence/probe-main2.log` SHA-256 `a5f7256daebe941771296036341fc9373d758c167738f7d2478a579242424d0c` |
| probe-decide (expected-fail elaboration evidence) | 1 | 2779 ms | `evidence/probe-decide.log` SHA-256 `c20e460f330b5584e2d2d6114e8b2fc8cf035870bc464b01ee6af3d943e2383f` |
| `/tmp/reactivegas/ms2/e43/t62-owner-codex/gates/a011/gate-s62-c-a011.sh <audit worktree>` (build 2/3, warm) | 0 | 82129 ms | `evidence/build2-slice-gate.log` SHA-256 `cae174d6144b59dccd12031e505c161eb04c8d6c2d2cbd556c53e3f0252ddf07` |
| `nix develop --quiet -c …/gate.sh.frozen ticket` (build 3/3, warm) | 0 | 73631 ms | `evidence/build3-ticket-gate.log` SHA-256 `35f0a9419e8d7532dec487fbc026f3ff042af2139be0d1bb1aa56173103fa270` |

## Build accounting

| Audit build | Cache | Free space before → after (bytes) | Exit / duration |
| ---: | --- | --- | --- |
| 1/3 focused instrument (`lake build` + probe iterations) | cold | `202195709952 → 202172739584` | `0` / 34516 ms build; probes 23894+7710+2779 ms |
| 2/3 A011 slice gate | warm | `202172739584 → 202118156288` | `0` / 82129 ms |
| 3/3 full ticket gate | warm | `202118156288 → 202118156288` | `0` / 73631 ms |

Aggregate exactly `23/40`; ceiling raises `0/2`. Candidate tracked status clean before and after every run (`git status --porcelain` empty; verified after each receipt).

## Campaign stop reason

Submission `2/2` FINAL. Six of seven rows terminal `KILLED` (five carried on exact blobs, one freshly killed); `G62-C-TRACE` terminal `BLOCKED` with the exact blocking fact recorded in the ledger. Any finding at submission 2 ends the campaign: `campaign=ENDED-RECUT-REQUIRED`. No repair, residual, open row, ceiling raise, or third submission exists.

## Recommendation

Do not accept candidate `6a56f90115e7624830da769da55a1ce1a3c5f5e3`. Re-cut the ticket carrying: (a) the `G62-C-TRACE` blocking finding and its binding property class above; (b) this campaign ledger and remaining build budget (`17` of `40` substantive builds at `23/40`); (c) the carried killed rows with their blob pins. `I57-01-BOUNDARY` is settled and its repair should be carried as-is into the re-cut base.
