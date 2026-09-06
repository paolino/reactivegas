# Commit Audit

- Submission: 1
- Base: `e6c59242ccf9b388053626c24446faaa2d7417fd`
- Candidate: `09f82301fbf38033c0f9e388f7cfacc593b64835`
- Mandate: `846f0b4c57d1cdac` (owner brief + NOTE-001 + NOTE-002 concatenated, sha256 first 16)
- Scope: FULL `e6c5924..09f8230` (3 files, +142/−16)
- Verdict: PASS
- Audit loop: submission `1/2`; next submission `ALLOWED`
- Ceiling raises: `0/2`; ledger none prior, this audit `ledger/campaign.md`
- Campaign: CLOSED — ended by SET-POINT
- Builds: `4/5` this audit; `cache=cold` for `just lean`, `cache=warm` thereafter except parent/mutant/ctor rebuilds
- Seat: grok 4.6, pane `%513`, author muse `%507`, ticket/epic owner `%503`
- Snapshot: `/code/reactivegas-lean-compliance-audit-s1-09f8230` detached at the candidate, started with no `lean/.lake`

Combined gate hash (file-bytes concat, sha256 first 16) independently `ad0a4311ccf2ab46`. Per-file first16 match NOTE-002.

## Invariant matrix

| Invariant | Severity | Verdict | Row state | Proof / evidence |
|---|---|---|---|---|
| S1-RESOLVE | BLOCKING | PASS | KILLED | `evidence/probe-manifest.out` sha256 `c0fda205…`: all 14 Event ctors bind; `missing=[]`. Unqualified `env.find? step_open_inv` is still `none`; `Reactivegas.step_open_inv` is a theorem. Six namespaced + eight atomic, zero last-component collisions among the 14. |
| S1-AGREE | BLOCKING | PASS | KILLED | Mutant of the *new* last-component sweep (atomic names only, not a replay of `env.find? mkSimple`) makes the shipped script RED: A `14/14/0` vs B `14/8/6`, exit 1. `evidence/mutant-agree.err` sha256 `f9bace29…`. PATH wraps on the green tree also RED: covered-minus-one (14 vs 13) and missing TRACE-INVENTORY line. |
| S1-NO-UNPROVED | BLOCKING | PASS | KILLED | Fresh parent rebuild (stale candidate oleans discarded): inventory `14/8/6`, decls `step_close_inv`×1 + `UNPROVED`×1. Candidate: `14/14/0`, `step_close_inv`×1 + `step_withdraw_inv`×1, `UNPROVED`=0. Declaration values masked to `"X"`: md5 `4309a735ac6448904abf41cd5e94f197` on **both** sides. Line-5 swap-back identical. `evidence/swap-back-fresh.txt`, `evidence/masked-equal.txt`. |
| S1-FROZEN | BLOCKING | PASS | KILLED | Candidate TraceTests: `checks=43 failures=0`, `TRACE-INVENTORY ctors=14 covered=14 missing=0`, exit 0 — `frozen_*_faithful` are `by decide` over `#freeze_reduced`. Live `traceInventory` from the env probe matches those frozen counts. Parent TraceTests after rebuild prints `14/8/6` and still decides, so the freeze tracks live values; no silent disagree. |
| S1-GREEN | BLOCKING | PASS | KILLED | Cold snapshot `just lean` exit 0 in 69s (`trace-coverage-agreement: ok (constructors=14 covered=14 missing=0)`, inversion 14/14/0, theorems 163/163). `lake env lean Reactivegas/CorpusGate.lean` prints `true`, exit 0. Agreement ran *before* the recipe's `cd lean && lake build` on a tree that had no `.lake`. |
| S1-CONTRACT | BLOCKING | PASS | KILLED | Dummy inductive, Trace.lean untouched: one correctly named theorem binds `alpha` and leaves `beta` missing; adding `step_beta_inv` binds both (`evidence/probe-contract-v2.out`, `probe-contract-v2-both.out`). 15th `Event` constructor cannot be added cheaply: `lake build Reactivegas.Trace` dies at `Step.lean:151` missing `Event.extraCtor` before the manifest runs. |

An empty prior mutant ledger is a reportable fact: NOTE-002 recorded zero definition mutants against the new resolver. This audit generated them.

## Failure modes altered

none altered -- checked: no threads, sockets, locks, or background tasks in the diff. The new script uses `mktemp` + `trap rm`; acquisition failure of the Lean sources dir exits 1. Distinct exits for instrument-A-fail, instrument-B-fail, missing report line, unparseable numbers, and count disagreement. Demonstrated: disagreement (mutant + minus-one wrap), missing B line (drop-line wrap). TraceTests kernel checks still exit 0 on a wrong 8/6 split (mutant TraceTests `checks=43 failures=0`) — that is the pre-existing blind spot M2 exists to catch, not a new swallow.

## Residuals

- **M2 compares counts, not covered-sets by name.** Confirmed: appending `PERMUTED-NAMES` to a 14/14/0 TRACE-INVENTORY line leaves the shipped script green (`evidence/wrap-perm.out`). At `missing=0` a same-count constructor permutation is vacuous. A swapped declaration name that both A and B still accept is the remaining hole; A’s name↔hypothesis bind and B’s `checkCoveredDeclarationBound` reject that class when they run. Honest limit: M2 itself will not see it. ADVISORY, named here, no follow-up ID (ticket owner to file if wanted).
- **Last-component hijack.** `namespace Other theorem step_alpha_inv` mentioning `stepEvent` and `ProbeEvent.alpha` binds `alpha` (`evidence/probe-hijack-v2.out`). The resolver is existential `Array.any` then stores the unqualified candidate — deterministic rendering, no declared precedence. Current Event env: 14 `step_*_inv` theorems, 0 last-component collisions. ADVISORY consequence of the desk’s unqualified rule, not a regression of the six.

## Candidate invariants

- `INV-S1-LAST-COMPONENT-UNIQUE` — at most one `step_*_inv` last component in the elaboration environment should mention a given Event constructor. Proposed severity ADVISORY. Evidence: hijack-v2 binds a namespaced impostor; nothing in the candidate gates last-component uniqueness.
- `INV-S1-GETSTRING-TOTAL` — `elabInversionManifest` calls `Name.getString!` on every `.thmInfo` in `env.constants` (whole environment, not `projectConstants`). A TraceTests-sized env has 70 theorems whose last component is numeric (`THM-NAMES total=59466 str=59396 num=70`). Trace.lean’s own compile env currently does not trip this (cold `just lean` green). Proposed severity ADVISORY. A later import that pulls a numeric-named theorem into Trace’s environment would panic the elaborator.

## Onward discoveries — outside this ticket

None.

## Blocking findings

None.

## Verification receipts

| Command | Exit | Duration | Evidence |
|---|---:|---:|---|
| `just lean` (cold snapshot) | 0 | 69s | `evidence/just-lean-cold.out` sha256 `05f9e5da…` cache=cold |
| `lake env lean Reactivegas/CorpusGate.lean` | 0 | 2s | `evidence/corpus-gate-direct.out` (`true`) |
| `lake env lean TraceTests.lean` (candidate) | 0 | 11s | `evidence/tracetests.out` sha256 `02d80c69…` |
| `lake build Reactivegas.Trace` (parent e6c5924, after dropping stale oleans) | 0 | 3s | `evidence/parent-lake-trace.out` cache=incremental **build 2** |
| `lake env lean TraceTests.lean` (parent, post-rebuild) | 0 | ~12s | `evidence/parent-tracetests-fresh.out` sha256 `1acfe878…` inventory 14/8/6 |
| `scripts/check-trace-coverage-agreement` (atomic-only mutant) | 1 | 42s | `evidence/mutant-agree.err` sha256 `f9bace29…` **build 3** |
| PATH wrap `covered-minus-one` | 1 | ~15s | `evidence/wrap-minus-one.err` A 14/14/0 vs B 14/13/1 |
| PATH wrap `drop-line` | 1 | ~15s | `evidence/wrap-drop.err` no TRACE-INVENTORY line |
| PATH wrap `same-count-perm` | 0 | ~15s | `evidence/wrap-perm.out` still ok 14/14/0 |
| `lake build Reactivegas.Trace` (Event.extraCtor) | 1 | 14s | `evidence/ctor15.out` missing cases `Step.lean:151` **build 4** |

## Lean inversion coverage

Denominator: 14 constructors of elaborated `Event` (`probe-manifest.out` EVENT-CTOR list). Bound theorem last-components: `step_open_inv`, `step_grant_inv`, `step_deny_inv`, `step_deposit_inv`, `step_withdraw_inv`, `step_transferCassa_inv`, `step_donate_inv`, `step_backdonate_inv`, `step_pledge_inv`, `step_accept_inv`, `step_refuse_inv`, `step_correct_inv`, `step_close_inv`, `step_fail_inv`. All 14 `inPermitted=true`, `viaUNPROVED=false` (`probe-permitted.out`). Self-falsification: atomic-only mutant unbinds the six namespaced required inversions and M2 goes red.

## Suspicious items from the brief

- **Collision rule.** Last-component `any`; render unqualified candidate. Deterministic string; hijack possible (see residuals). No current collision in the Event inventory.
- **Rendered spelling.** All 14 rendered names sit in `permittedNames`. The `d == "UNPROVED"` arm of `checkCoveredDeclarationBound` is unused on this candidate (`viaUNPROVED=false` on every row). The six do not pass for the weaker reason.
- **M2 permutation.** See residuals. A’s name↔hypothesis and B’s permittedNames catch identity swaps when those instruments themselves pass.
- **justfile ordering.** New check is before `cd lean && lake build`. Cold `just lean` succeeded; instrument A rebuilds the library, so a genuinely cold tree works.
- **Deleted `declText`.** No remaining references. `bound := some cand` is the unqualified candidate; for the old `mkSimple` hits this is the same string. Behaviour change is the six newly bound rows, not the deletion.
- **No new hardcoded extent.** Diff does not touch `expectedDeclarations := 163`. New script contains no `163` and no second extent constant; it parses executed stdout.

## Advisories

- Frozen instruments (runtime root, sha256): `instruments/lake` / `disagree-wrap.sh` `f98d4d38…`; `instruments/mutant-Trace-atomic-only.lean` `c8be763e…`; `instruments/probe-contract-v2.lean` `f93c4d9d…`; `instruments/probe-hijack-v2.lean` `d04771eb…`. Property shape: mutate the new last-component sweep (or B’s printed counts) and require the shipped agreement script to exit non-zero.
- 15th Event constructor: the author’s “pinned 14-ctor / 163-theorem counts” limit is real but incomplete. The first failure is exhaustive match in `stepEvent` (`Step.lean:151`), then GuardId/guardOf in Trace.lean and the other Event matches. The elaborator would discover a 15th ctor from `iv.ctors` if the machine compiled; that was not reached. The theorem-side contract holds without editing Trace.lean.

## Honest limits

- Parent envelopes required deleting candidate oleans copied onto the e6c5924 worktree; a first TraceTests run on that copy was invalid (printed 14/14/0). The receipts above are the rebuild.
- Dummy-inductive v1 (`def step`) was invalid (`step` already declared). v2 mentions the real `stepEvent` plus the dummy constructor in one type.
- `getString!` totality was measured in a TraceTests-sized environment, not proven for every future import set.
- Wraps intercept B’s stdout; the mutant edits the new resolver. The mutant is the one that closes S1-AGREE against NOTE-002’s “future regression of the new code” bar.

Worktrees for the ticket owner to retire: snapshot `/code/reactivegas-lean-compliance-audit-s1-09f8230` (~28 MiB), parent `/code/reactivegas-lean-compliance-audit-s1-e6c5924`, mutant `/code/reactivegas-lean-compliance-audit-s1-mutant`, ctor `/code/reactivegas-lean-compliance-audit-s1-ctor15`. Shared tree `/code/reactivegas-lean-compliance` and snapshot porcelain empty of tracked paths after every run.
