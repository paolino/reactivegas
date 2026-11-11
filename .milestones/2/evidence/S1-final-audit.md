# Commit Audit

- Submission: 2
- Base: `e6c59242ccf9b388053626c24446faaa2d7417fd`
- Previous candidate: `09f82301fbf38033c0f9e388f7cfacc593b64835` (PASS scoped to that commit only; closes nothing here)
- Candidate: `fa01779a5b60f40c8bc3a2903b5102b1f16bb5aa`
- Mandate: `0ea5d6de64554221` (owner brief + NOTE-001..NOTE-004 concatenated, sha256 first 16; independently recomputed)
- Scope: FULL `e6c5924..fa01779` (3 files, +146/−16) plus repair delta `09f8230..fa01779` (`lean/Reactivegas/Trace.lean` only, +8/−4)
- Verdict: PASS
- Audit loop: submission `2/2`; next submission `FORBIDDEN`
- Ceiling raises: `0/2`; ledger `ledger/campaign.md`
- Campaign: CLOSED — ended by SET-POINT
- Builds: `2/5` this audit; `cache=cold` for `just lean`, `cache=warm` (copied oleans, then rebuilt Trace) for the atomic-only mutant
- Seat: Grok 4.6, pane `%520`, author muse `%507`, ticket/epic owner `%503`
- Snapshot: `/code/reactivegas-lean-compliance-audit-s2-fa01779` detached at the candidate, started with no `lean/.lake`

Combined gate hash (file-bytes concat, sha256 first 16) independently `ad0a4311ccf2ab46`. Per-file first16 match NOTE-002. All five gate files byte-identical `09f8230..fa01779`. `expectedDeclarations := 163` blob identical across those commits; agreement script contains no extent constant.

Repair delta is the claimed match (`n.getString!` → `| .str _ s =>` / `| _ => none`) plus a totality comment. No other tracked path moved.

## Invariant matrix

| Invariant | Severity | Verdict | Row state | Proof / evidence |
|---|---|---|---|---|
| S1-RESOLVE | BLOCKING | PASS | KILLED | `evidence/probe-manifest.out` sha256 `c0fda205…` (byte-identical to the 09f8230 probe): all 14 Event ctors bind; `missing=[]`. Unqualified `env.find? step_open_inv` is still `none`; `Reactivegas.step_open_inv` is a theorem. Six namespaced + eight atomic. `COLLISION-SCAN-DONE` with zero last-component collisions among the 14. |
| S1-AGREE | BLOCKING | PASS | KILLED | Mutant of the *current* resolver (`\| .str _ s` → `\| .str .anonymous s`, patch applied at Trace.lean:174) makes the shipped script RED: A `14/14/0` vs B `14/8/6`, exit 1. `evidence/mutant-agree.err` sha256 `54fac39c…`. Not a replay of RED-2 or of the 09f8230 atomic-only file. PATH wraps inside nix (outer PATH is overwritten by `nix develop`): minus-one RED 14 vs 13; drop-line RED (no TRACE-INVENTORY line). |
| S1-NO-UNPROVED | BLOCKING | PASS | KILLED | Parent e6c5924 TraceTests: inventory `14/8/6`, decls `step_close_inv`×1 + `UNPROVED`×1. fa01779 and 09f8230 TraceTests logs are sha256-identical `02d80c69…`: `14/14/0`, `step_close_inv`×1 + `step_withdraw_inv`×1, `UNPROVED`=0. Raw jsonl fa01779==09f8230. Declaration values masked to `"X"` on TRACE-JSON-prefixed lines: md5 `4309a735ac6448904abf41cd5e94f197` on parent, 09f8230, **and** fa01779. Line-5 swap-back identical. |
| S1-FROZEN | BLOCKING | PASS | KILLED | Candidate TraceTests: `checks=43 failures=0`, `TRACE-INVENTORY ctors=14 covered=14 missing=0`, exit 0 — `frozen_*_faithful` are `by decide` over `#freeze_reduced`. Live `traceInventory` from the env probe matches those counts. Parent TraceTests still decides at `14/8/6`. |
| S1-GREEN | BLOCKING | PASS | KILLED | Cold snapshot `just lean` exit 0 in 54s (`trace-coverage-agreement: ok (constructors=14 covered=14 missing=0)`, inversion 14/14/0, theorems 163/163, negative-control detects withheld `backdonate`). Agreement ran *before* the recipe's `cd lean && lake build` on a tree that had no `.lake`. `lake env lean Reactivegas/CorpusGate.lean` prints `true`, exit 0. 0 panic strings in the cold log. |
| S1-CONTRACT | BLOCKING | PASS | KILLED | Dummy inductive, Trace.lean untouched: one correctly named theorem binds `alpha` and leaves `beta` missing; adding `step_beta_inv` binds both (`evidence/probe-contract-v2.out`, `probe-contract-v2-both.out`). |
| S1-TOTAL | BLOCKING | PASS | KILLED | Control is **absence of the panic string**, not exit status. Positive control: retained `../candidate-auditor-s1-grok/evidence/probe-hijack-v2.out` at 09f8230 — independently counted **70** lines of `PANIC at Lean.Name.getString`, first line `PANIC at Lean.Name.getString! Lean.Data.Name:28:15: unreachable code has been reached`, first panic at line 4, `HIJACK-V2-BEGIN` at 7144, 7147 lines / 1142334 bytes, **exit 0**. Same frozen instrument (sha256 `d04771eb…`, byte-identical, unmodified) at fa01779: **0** panics, 7 lines, `HIJACK-V2-BEGIN` at line 4, `BIND alpha` / `MISS beta`, exit 0 (`evidence/probe-hijack-v2.out` sha256 `504c36a2…`). Production Event manifest, TraceTests, and cold `just lean` also 0 panics while 70 numeric-last theorems remain in env (`THM-NAMES total=59466 str=59396 num=70`). Detector still works at this toolchain: direct `Name.getString!` on a `.num` emits the same first panic line, count=1 (`evidence/getstring-direct-summary.txt`). |

## Failure modes altered

none swallowed -- checked: no threads, sockets, locks, or background tasks in the repair delta. The repair replaces a partial `Name.getString!` over every `.thmInfo` with a total match that skips non-string last components. At 09f8230 the partial already *fell back* (70 panics, correct rows, exit 0); the observable change is that the panic string is gone while the 14 bindings stay byte-identical. `ctor.getString!` on `iv.ctors` remains (see residuals). The agreement script is untouched: `mktemp` + `trap rm`; demonstrated distinct exits for count disagreement (mutant + minus-one wrap) and missing B line (drop-line wrap).

## Residuals

- **M2 compares counts, not covered-sets by name.** Confirmed at this commit: appending `PERMUTED-NAMES` to a 14/14/0 TRACE-INVENTORY line leaves the shipped script green (`evidence/wrap2-same-count-perm.out`, exit 0, wrapper was on PATH). At `missing=0` a same-count constructor permutation is vacuous. Instrument A's name↔hypothesis bind and B's `checkCoveredDeclarationBound` reject that class when they run (`viaUNPROVED=false` on all 14 rows; every rendered name is in `permittedNames`). Honest limit: M2 itself will not see it. ADVISORY. Owner packet states this residual; the wrap re-derives it.
- **Last-component hijack, scoped as the documented syntactic existence check.** `namespace Other theorem step_alpha_inv` mentioning `stepEvent` and `ProbeEvent.alpha` still binds `alpha` (`evidence/probe-hijack-v2.out`). The resolver is existential `Array.any` over last components with no declared precedence — a property of this slice's resolver, not a consequence of the unqualified *rendering* rule. The probe binds a **dummy** inductive; it does not show a production `Event` inversion can be hijacked (no matching production witness; `Trace.lean` already declares the existence-check limit). Measured: 14 `step_*_inv` theorems, **zero** last-component collisions among the current Event inventory. Not a permanent blanket waiver and not proof of future safety. Owner packet wording matches this evidence.

## Candidate invariants

- `INV-S1-CTOR-GETSTRING` — `elabInversionManifest` still calls `ctor.getString!` on `iv.ctors`. Event's 14 constructors are all `.str` (production path does not panic). The same env contains 24 inductive constructors whose last component is numeric (Lean.Widget RPC packets). Those names are not arguments of `inversion_manifest%` in this tree. Proposed severity ADVISORY. Not a declaration-name panic; S1-TOTAL as specified is about declaration names in the theorem sweep.

## Onward discoveries — outside this ticket

None.

## Blocking findings

None.

## Verification receipts

| Command | Exit | Duration | Evidence |
|---|---:|---:|---|
| `just lean` (cold snapshot) | 0 | 54s | `evidence/just-lean-cold.log` sha256 `b20be4c9…` cache=cold **build 1** |
| `lake env lean Reactivegas/CorpusGate.lean` | 0 | 2s | `evidence/corpus-gate-direct.out` (`true`) |
| frozen `probe-hijack-v2.lean` at fa01779 | 0 | 2s | `evidence/probe-hijack-v2.out` sha256 `504c36a2…` panics=0 lines=7 |
| same instrument at 09f8230 (retained) | 0 | 18s | `../candidate-auditor-s1-grok/evidence/probe-hijack-v2.out` panics=70 lines=7147 (positive control; first panic line cited above, backtraces not pasted) |
| `Name.getString!` on `.num` (detector) | 0 | 2s | `evidence/getstring-direct-summary.txt` panics=1 first line same as 09f8230 |
| `lake env lean TraceTests.lean` (fa01779) | 0 | 12s | `evidence/tracetests-fa01779.out` sha256 `02d80c69…` identical to 09f8230 |
| `lake env lean TraceTests.lean` (e6c5924) | 0 | ~12s | `evidence/tracetests-e6c5924.out` sha256 `1acfe878…` inventory 14/8/6 |
| shipped agreement vs atomic-only mutant of `.str _ s` | 1 | 39s | `evidence/mutant-agree.err` sha256 `54fac39c…` **build 2** |
| PATH wrap `covered-minus-one` (inside nix) | 1 | ~15s | `evidence/wrap2-covered-minus-one.err` A 14/14/0 vs B 14/13/1 |
| PATH wrap `drop-line` | 1 | ~15s | `evidence/wrap2-drop-line.err` no TRACE-INVENTORY line |
| PATH wrap `same-count-perm` | 0 | ~15s | `evidence/wrap2-same-count-perm.out` still ok 14/14/0 |

## Lean inversion coverage

Denominator: 14 constructors of elaborated `Event`. Bound last-components: `step_open_inv`, `step_grant_inv`, `step_deny_inv`, `step_deposit_inv`, `step_withdraw_inv`, `step_transferCassa_inv`, `step_donate_inv`, `step_backdonate_inv`, `step_pledge_inv`, `step_accept_inv`, `step_refuse_inv`, `step_correct_inv`, `step_close_inv`, `step_fail_inv`. All 14 `inPermitted=true`, `viaUNPROVED=false`. Self-falsification: atomic-only mutant of the current match unbinds the six namespaced required inversions and M2 goes red.

## Repair envelope invariant (14 bindings + envelope unchanged vs 09f8230)

Verified, not accepted from the packet: TraceTests log sha256 identical; jsonl byte-identical; MASK-norm identical; X-prefix md5 `4309a735ac6448904abf41cd5e94f197` on both; probe-manifest sha256 identical to the 09f8230 probe. The match change does not alter str-name semantics for the current inventory.

## Suspicious items from the brief

- **Match totality.** `.str _ s` handled; `.num` / `.anonymous` skipped. Equivalent extraction to `getString!` on string last components; the difference is the skip vs panic. 70 numeric-last theorems still present; Event manifest and hijack-v2 elaborate without the panic string.
- **Remaining partial.** `ctor.getString!` on `iv.ctors` — see candidate invariant. Not reached by `inversion_manifest% Event`.
- **Agreement still falsifiable.** Mutant of the resolver *as it now stands* turns the shipped script red. First wrap attempt with PATH outside `nix develop` was a false green (nix overwrites PATH); wrap2 inside nix is the receipt.
- **Residual R1 wording.** Owner packet states resolver-owned existential last-component check, dummy inductive, zero current Event collisions, not imposed by the rendering rule, not a production Event hijack, not a permanent waiver. Hijack-v2 and the collision scan agree with that scoping.
- **Residual R2.** Perm wrap stays green; minus-one and drop-line go red. Packet matches.
- **No new hardcoded extent.** Repair does not touch `expectedDeclarations`; agreement script parses executed stdout only.
- **inject-num probe.** `addDecl` in `CommandElabM` did not apply (type mismatch). Not used as evidence. The 70 numeric theorems already in the imported environment are the declaration-name experiment.

## Advisories

- Frozen instruments (runtime root, sha256): `instruments/probe-hijack-v2.lean` `d04771eb…` (unmodified vs s1); `instruments/atomic-only.patch` `18cdf656…`; `instruments/lake` / `disagree-wrap.sh` `f98d4d38…`; `instruments/probe-getstring-direct.lean` `cfe166d3…`; `instruments/probe-ctor-shapes.lean` `1ea1dbaa…`. Property shape: mutate the current last-component match (or B's printed counts) and require the shipped agreement script to exit non-zero; assert absence of `PANIC at Lean.Name.getString` against a log that contains 70 of them.
- 15th Event constructor was not re-run (build saved). S1-CONTRACT is closed by the dummy inductive. The first failure of a 15th Event ctor remains exhaustive match in `stepEvent` (`Step.lean:151`) as measured at 09f8230.

## Honest limits

- S1-TOTAL is absence of the panic *string* while the 70 numeric-last theorems remain in env, plus a detector that still prints that string when `getString!` is invoked directly. It is not a proof over every future import set, and it does not close `ctor.getString!` on arbitrary inductives.
- `probe-inject-num.lean` failed to inject an extra numeric theorem; the existing 70 are the experiment.
- `just ci` was not re-run here. S1-GREEN is `just lean` + corpus gate. Remote CI is the desk's.
- Outer-PATH lake wraps are inert under `nix develop`; only the inside-nix wraps are evidence.

## Rows not independently closed

None of S1-RESOLVE, S1-AGREE, S1-NO-UNPROVED, S1-FROZEN, S1-GREEN, S1-CONTRACT, S1-TOTAL.

## Worktrees for the ticket owner to retire

- snapshot `/code/reactivegas-lean-compliance-audit-s2-fa01779` (~26.8 MiB) detached at `fa01779`
- mutant `/code/reactivegas-lean-compliance-audit-s2-mutant` (~26.8 MiB) dirty `Trace.lean` (atomic-only patch, uncommitted)

Used read-only, not created by this seat: `/code/reactivegas-lean-compliance-audit-s1-09f8230`, `/code/reactivegas-lean-compliance-audit-s1-e6c5924`. Shared tree `/code/reactivegas-lean-compliance` and snapshot porcelain empty of tracked paths after every run. No stray `.lean` under `lean/`.
