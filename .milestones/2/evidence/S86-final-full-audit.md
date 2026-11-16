# Commit audit — issue 86, submission 1, replacement seat s86-audit-2

**Verdict: AUDIT-PASS for the enumerated local audit.** No candidate findings. This is not acceptance: real remote CI at the clean final SHA remains the ticket owner's separate obligation under R86-A and the owner receipt. It was neither executed nor inferred here.

- Candidate: `38c6d0629c14dfc2209c56450475a77c445fba1f`; parent `6ec3ce315158c4615fcaf0ac9986efc6d3c5f8b3`.
- Subject: FULL `4a6cd87..38c6d06`, including all inherited exporter artifacts. `9c8756a` is corpus-input provenance, not the accepted code base.
- Seat: fresh codex context, live PID 1301444, pane `%543`, window `reactivegas-e67-t86-exporter-successor`; model `gpt-6-astra`, effort `high`, argv independently inspected. Parent `%529`, author `%531`; no author contact.
- Mandate: all six `specs/86-exporter-successor/` hashes match the brief. Diff and provenance retained in `evidence/full.diff`, `evidence/tip.diff`, `evidence/git-provenance.txt`.
- Frozen gate: `3579e71cb263d2408657d86ac666a0b85e5c0c44f554932d1e9f21873f627626`. Missing worktree copy restored byte-for-byte from the explicitly named backup; never edited.
- Submission remains 1/2; invalid predecessor neither resets cap nor supplies acceptance. Ceiling raises: 0. Historical building audits: 1/3 on entry, 3/3 on exit; this seat used two substantive invocations. Separate ticket counter at dispatch was 3/8; no parent ledger was edited.
- All 40 allowed targeted probe invocations used, including two non-candidate instrument/setup failures. No further full gate, cold tree, or backend build was launched. Campaign ended at the fixed probe cap after the enumerated behavioral classes were settled.

## Invariant ledger

PASS here means the named local observation is supported. Documentary and Git-provenance rows are identified separately and are not relabeled as killed behavioral mutants. Row IDs below link to exact command receipts in `command-ledger.json` and the receipt index in this report.

| Invariant | Severity | Verdict | Receipts | Evidence and limit |
|---|---|---|---|---|
| I86-A | BLOCKING | PASS (local scope) | P27 P28 P29 P30 P34 P35 P36 | Actual committed CI command rejects each corpus drift and real manifest drift; clean path succeeds; deleting step and executing no-op bypass both fail the auditor detector. Remote CI is the ticket owner's separate leg, unverified here. |
| I86-B | BLOCKING | PASS | P33 P32 | Exact pre-repair project.nix omission control: clean lookup fails, recipe reaches compiled check then fails at jq with exit 127. Candidate clean shell resolves /nix/store/qvbwz06cqra3cmlra40v0adw75j6j7wm-jq-1.8.1-bin/bin/jq and recipe exits 0. |
| I86-C1 | BLOCKING | PASS | P01 P02 P06 P37 | ZZZ mutation rejected for economic view mismatch; same bytes pass shipped key-set program, separating value from shape. Live view independently re-derived from imports. |
| I86-C2 | BLOCKING | PASS | P03 P06 P37 | Empty integrated initial members rejected for live corpusInitial mismatch; live initial equals stored initial. |
| I86-C3 | BLOCKING | PASS | P04 P06 P37 | Economic permissive auth identity rejected by the compiled context checker; current string equals live identity. |
| I86-C4 | BLOCKING | PASS | P05 P06 P37 | Integrated permissive auth identity rejected by the compiled context checker; current string equals live identity. |
| I86-C-CLAIM | BLOCKING | PASS (claim limit) | P37 | Module explicitly disclaims serializer-instance independence. Inspection confirms shared ToJson instances; P37 re-derives the live values, not an independent encoder. This is a documentary bound, not a killed behavioral mutant. |
| I86-D | BLOCKING | PASS | P07 P08 P09 P38 | All three malformed forms exit 1 with identical full directory byte inventories. Exact pre-repair source exits 0 and overwrites both check and a; the no-write detector rejects those effects. |
| I86-KEYS | BLOCKING | PASS | P10 P11 P12 P13 P14 P15 P39 P40 | Both shipped jq programs reject extra keys at the top and one nested level, and reject empty arrays. Positive controls pass. P02 demonstrates context/key-set separability. |
| I86-ADD | BLOCKING | PASS (Git provenance) | S1 S2 | 245 protected accepted-base file blobs unchanged. Every prior line preserved in the four modified existing files; new paths confined to exporter, corpus and mandates. All corpus/manifest bytes equal frozen 9c8756a input. No code-mutation claim is made for this Git-metadata row. |
| I86-E | ADVISORY | PASS (artifact truth) | P37 | Handoff f6dd0df4 matches current hashes, 5 traces/32 events/7 steps and zero UNPROVED; historical fed19b3 bytes independently hash to 91526dc6 and contain one UNPROVED. Dated history, vote hole, provisional list, replayer table, and stale-comment route preserved. |
| G74-CALLS-EXISTING | BLOCKING | PASS | S1 P06 P16 P17 P18 P19 P37 | Full leaf exporter reviewed: calls seedCorpus/emitIntegratedCorpus directly, no second event list; empty arrays and same-size permutations rejected against live definitions. Current extents independently re-derived. |
| G74-VERIFY-FAILS-CLOSED | BLOCKING | PASS | P27 P28 P29 P30 | Executed the committed recipe against actual corrupt files and actual corrupt manifest, not the gate's untouched-manifest comparison. All fail at the intended checker; clean version passes. |

## Executed verification and accounting

1. S1: `nix develop --quiet -c just ci` from absent `.lake` and `dist-newstyle`, exit 0, 153474 ms. The Cabal library/server compiled; formatting and hlint ran (`No hints`); Lean toolchain 4.25.0 matched; inversion coverage was 14/14, its withheld-backdonate negative control was detected; trace agreement was 14/14; Lean library and exporter built; corpus output matched, both manifest entries passed, compiled check reported 5/32/7.
2. S2: exact `nix develop --quiet -c ./gate.sh`, exit 0, 1571 ms, all 11 printed subrow lines passed (the nine named rows include subchecks). Independent execution produced deterministic output equal to the owner's log; equality was not used instead of running it.
3. P27–P30, P32–P33, P36 each reach the recipe's nested `lake build corpusExport`: seven targeted warm/cache-only invocations, each with this seat's `.lake` mounted read-only. All report replayed jobs and none report a newly built job. These are explicitly enumerated warm recipe probes under command 3 of the brief, not hidden cold builds. P26 fails before any Lake invocation. P31/P37 use `lake env lean`; P38 uses `lake env bash` then interpreted `lean --run` with existing imports and no backend output.
4. Corpus/Git/hash inspection and ledger/report generation are non-compiling records, separately accounted from the 40 executable probes. Source-text inspection closes no behavioral row.

## Failure modes and controls

- Malformed check arity: pre-repair two-argument fallthrough returned success and wrote both destinations; candidate returns 1 before either write. Full directory inventories establish effects (P07–P09, P38).
- Second write failure: deliberately targeting a directory returns 1 while the first file contains the emitted economic bytes. The documented non-atomic limit remains observable (P24); this audit asserts no broader atomicity.
- Missing and malformed inputs: economic/integrated missing files propagate IO errors; malformed JSON reports parse failure, all exit 1 (P20–P23). Context mismatches now return explicit errors (P01/P03–P05). No asynchronous, synchronization, or retry path was introduced by the repair.
- jq absence: the exact pre-repair declaration fails lookup in the clean shell, passes earlier emission/comparison/check stages, then exits 127 at jq; restored declaration resolves the Nix-store tool and succeeds (P33/P32). The earlier local Python-not-in-PATH setup failure is unrelated and is retained in STATUS.
- Ignored-state residue: S1 began with both build trees absent; subsequent corruption probes use only S1 artifacts mounted read-only. Corrupt fixtures remain rejected with the warm artifacts; tracked tree is clean afterward. No owner cache/binary or predecessor instrument supplied an audit result.

## Mutation ledger and limits

`mutation-ledger.json` enumerates 19 distinct applied data/config/source mutations: 19 executed, 19 killed by the relevant checker/detector, 0 observed survivors within this named set. Empty-array fixtures are reused against both live-value and key-set checks and are counted once per distinct mutant, not twice. Five additional missing/malformed/write-failure injections are reported separately. These totals are finite sampled evidence, not a claim about arbitrary mutants.
P26 is a mount-environment setup failure: just attempted its temporary script in a read-only runtime directory, so the recipe did not execute. P27 corrected that private runtime path; its intended cmp failure was observed, although the harness initially expected a different path prefix in the diagnostic. No rerun or extra mutation was charged for correcting that postcheck. P31 evaluated all requested live context values but its supplemental withdrawal axiom query used the wrong namespace; P37 corrected the instrument and passed. Neither setup/import error is a candidate finding or a killed mutant.
The arity measurements were collected on the candidate before the known-defect P38 run. Verdicts were deferred until that control completed. Final read-only inventory reconciliation rejects the captured P38 write effects first, then accepts the captured P07–P09 unchanged inventories with the same equality comparator (`evidence/arity-inventory-reconciliation.json`). This ordering is disclosed; no reversed execution chronology, timing, or concurrency claim is made.
Key-set mutant probes invoke the host jq; P32 separately executes the exact shipped programs with declared Nix jq. No jq implementation proof is claimed. The view/auth/initial live comparison shares the exporter's ToJson instances and identity definitions. Auth execution samples are both false; the source defines the refusing closures. Serializer-instance independence and backdonation coverage are not established.
The Lean model/proofs are protected inherited context, not a newly commissioned whole-model proof audit. The supplemental current declaration checks report `step_close_inv` and `Reactivegas.step_withdraw_inv` using only `propext`. No votes are covered by these corpora; the handoff explicitly retains that hole and provisional semantics.

## Hash-bound artifacts

- `command-ledger.json` SHA-256 `0db7f1e0afb3fa62df621d8f8559c0a3cfcbae22b70b44d1329c68f27d38b33a`.
- `mutation-ledger.json` SHA-256 `914f3a6b3b7ec62d7a5f825d06ec981f51f7bc9e68de1ff0f05e3c94d63641e0`.
- `instruments.sha256` SHA-256 `390d384e4365787636bf00a2a7b15b662abc4b9cc601d001222d8b8e2d47fd4e`.
- `evidence/reconciliation.json` SHA-256 `94dad799b016eaa4510e6cdac11258ea9811e35252e53062dd5e5e92f9920542`.
- `evidence/content-census.json` SHA-256 `cf9a91e89c8a7c9f07c6c6736951a79d25a3f9b1d97a2dc4ee6b738831d5e36a`.
- `evidence/full.diff` SHA-256 `d79db3f526cf10908b741303da04d617ef502402ddff7edfc0002cc8c2dac785`.
- `evidence/tip.diff` SHA-256 `bb0600269ab7e48fba6e590c50a10a8c4d19fe8a5d3d050fd0432d1b7867d3de`.
- `evidence/final-state.txt` SHA-256 `d79822bd26ce0c66c5a2547080331df233637b1fbc746828bdee93f3da705504`.
- `evidence/log-review.json` SHA-256 `d5e3e0bb5af4ccd966592f42a4fa0ff2faaa7c3efb5747d6e75c481454fe63ab`.

- `evidence/arity-inventory-reconciliation.json` SHA-256 `6dadaa6272cea74c0f76028039c38f21109bad43923ad9df2170635659784666`.

- `evidence/P07-inventory.json` SHA-256 `2a19b12389cf59a1ed07f1014658503a166a0bcb50ef3601816d0eae6f6d9024`.

- `evidence/P08-inventory.json` SHA-256 `2a19b12389cf59a1ed07f1014658503a166a0bcb50ef3601816d0eae6f6d9024`.

- `evidence/P09-inventory.json` SHA-256 `2a19b12389cf59a1ed07f1014658503a166a0bcb50ef3601816d0eae6f6d9024`.

- `evidence/P38-effects.json` SHA-256 `f117b0d4d2215fe5c7e80c122150f01a9111a61b196f001d064cde16394972cc`.

## Command receipt index

Exact argv, CWD, scope, nested commands, cache class, exit, duration and full evidence paths are in the hash-bound `command-ledger.json`. The table below provides every command's output receipt. Every log is retained in full. The 650911-byte full-CI log was also scanned end-to-end into `evidence/log-review.json`; compiler warnings are not represented as a zero-warning result.

| ID | Exit | ms | Evidence | SHA-256 |
|---|---:|---:|---|---|
| S1 | 0 | 153474 | evidence/S1-ci.log | 387ad840e00c17fce3c095d85e6a701e118ec5c2e12832b9e84e4f8d0f27836a |
| S2 | 0 | 1571 | evidence/S2-gate.log | 7d331a0a0b900c519d4830a3be5f8e0298c9a8ee40e013997a03758cbc5eab1c |
| P01-C1 | 1 | 63 | evidence/P01-C1.log | 58b2d693d0de820fb7e12c98b4a6f181ff31c6aa20617defb2c283c30f8600c4 |
| P02-C1-shape | 0 | 4 | evidence/P02-C1-shape.log | a17fcf0a2f50e2d495e4f90ce263410edc183add6c62699a2facbccf60410f74 |
| P03-C2 | 1 | 61 | evidence/P03-C2.log | f40e7d1568623b21449581dd3fa9178c877dbe3481aa7250aa7f2239b0074de9 |
| P04-C3 | 1 | 63 | evidence/P04-C3.log | fe546abc9bdac3f0e0c2068284c1751bf28d76981358444c94ac9fd0984db326 |
| P05-C4 | 1 | 64 | evidence/P05-C4.log | abf739c6841cf53ae8bcd5557624a41555519b41a1f5b98d84d8d0ffa12f8567 |
| P06-clean | 0 | 62 | evidence/P06-clean.log | c5078526f3700864e5bd02d41862003ebb6d190bbb7ae5865831ba04c6a2f61e |
| P07-arity | 1 | 61 | evidence/P07-arity.log | aee0c71cfb4741dfafaeaac007b7871c73bbeb42957fb54b6b3aa8f0b85ca3cc |
| P08-arity | 1 | 61 | evidence/P08-arity.log | aee0c71cfb4741dfafaeaac007b7871c73bbeb42957fb54b6b3aa8f0b85ca3cc |
| P09-arity | 1 | 63 | evidence/P09-arity.log | aee0c71cfb4741dfafaeaac007b7871c73bbeb42957fb54b6b3aa8f0b85ca3cc |
| P10-keys | 1 | 5 | evidence/P10-keys.log | 2ed27c1421e6928dbe13dbfdb5c59e1045b30341fe7ebe05700006bc5ac572c0 |
| P11-keys | 1 | 5 | evidence/P11-keys.log | 2ed27c1421e6928dbe13dbfdb5c59e1045b30341fe7ebe05700006bc5ac572c0 |
| P12-keys | 1 | 5 | evidence/P12-keys.log | 2ed27c1421e6928dbe13dbfdb5c59e1045b30341fe7ebe05700006bc5ac572c0 |
| P13-keys | 1 | 5 | evidence/P13-keys.log | 2ed27c1421e6928dbe13dbfdb5c59e1045b30341fe7ebe05700006bc5ac572c0 |
| P14-clean-econ-keys | 0 | 4 | evidence/P14-clean-econ-keys.log | a17fcf0a2f50e2d495e4f90ce263410edc183add6c62699a2facbccf60410f74 |
| P15-clean-int-keys | 0 | 4 | evidence/P15-clean-int-keys.log | a17fcf0a2f50e2d495e4f90ce263410edc183add6c62699a2facbccf60410f74 |
| P16-empty-traces | 1 | 62 | evidence/P16-empty-traces.log | 1abbbad162a255551f23cb687febdb8e78f0be1be1c655561a592e8753c84d73 |
| P17-empty-steps | 1 | 62 | evidence/P17-empty-steps.log | 2f3366181c10df572720e7165419e0d0b93183432257f576d3e9172fc6c51f36 |
| P18-trace-swap | 1 | 69 | evidence/P18-trace-swap.log | 26168b72ca3897561b76940d2aae15a692bdbff4ae51c53550babe5c4bda86dc |
| P19-step-swap | 1 | 62 | evidence/P19-step-swap.log | a607e85c23f969d38600a1cd56a105ad159665661fa6ba4d247c6667c6605161 |
| P20-missing | 1 | 62 | evidence/P20-missing.log | 4a24ff8a34e7b2ea2b215cf0e7cf5413bb5f288b6659875251e65af9a963d6df |
| P21-missing | 1 | 75 | evidence/P21-missing.log | 95ebcd1a88bb743e40fd972bde19cb44e1d91b9799a4dbc236fdc3396958fd40 |
| P22-malformed | 1 | 63 | evidence/P22-malformed.log | 078c944e635c8ff112db66229ebb81ab6ab815e5c02d9c5b38634ab4e691c641 |
| P23-malformed | 1 | 61 | evidence/P23-malformed.log | e7e895269d9dabcfc2a647db36ed772fad679ba0d59c3144bf1d3608536c3e82 |
| P24-second-write | 1 | 62 | evidence/P24-second-write.log | 8c3f3d6d5d790f3d967a5ba2d0ae6820268f40ad453720cafd868add32e9aae0 |
| P25 | 0 | 22 | evidence/P25-bwrap-readiness.log | e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855 |
| P26-ci-econ | 1 | 46367 | evidence/P26-ci-econ.log | 8aed6a287df8d41607999c869fb58369fc70177e1401ad71fab0e5fe94547de6 |
| P27-ci-econ | 1 | 10416 | evidence/P27-ci-econ.log | 2d5db125cc4b4ffe47b40a0d9ca85bb405d5644fa9b874d6e391c92b129e2d16 |
| P28-ci-int | 1 | 10320 | evidence/P28-ci-int.log | 0c53d6d456e4eb21039b24967afbc4895bf1ddbb509ea4621c3e0faf1ecff973 |
| P29-ci-manifest | 1 | 10248 | evidence/P29-ci-manifest.log | 660fc8d92f53b2f81d22669147a705c55792201c349d56fe8b4d22040a727c2c |
| P30-ci-clean | 0 | 9739 | evidence/P30-ci-clean.log | ac0b801d49621127e146a31f01b8c1cd4d267960019ad24da901e9beb1aac9fe |
| P31 | 1 | 4482 | evidence/P31-live-context.log | 28db96e66045f7a79de4e56f15fa4068d3b9c55c34d4e61e2ce2d8fbb58fdb14 |
| P32-clean-env | 0 | 10182 | evidence/P32-clean-env.log | d97e65150cd017af73692ec7f30ed4470e6f59a62bb167dc0f8276f86d723d5d |
| P33-jq-omission | 0 | 10201 | evidence/P33-jq-omission.log | e5e295e3fbb1d58e7794135c22d27b2cdb538376b8d6081ed5b3462928ca532e |
| P34-wiring-removed | 1 | 39 | evidence/P34-wiring-removed.log | 0b0f04ecd565cbe27e21a7e3c9972fbf7961bad9679877af860a2fb12615075d |
| P35-wiring-bypass | 1 | 62 | evidence/P35-wiring-bypass.log | 6fbd9da32607f96ebc88d1e60a5be9f063b8952b1691e5f2218f63f9e50e588c |
| P36-wiring-candidate | 0 | 1538 | evidence/P36-wiring-candidate.log | b635e98a4ba3bbebea2914b48abc7c311a1ab05e961b4faaf6ec32102ff5b570 |
| P37 | 0 | 4288 | evidence/P37-live-context.log | b9cfd3261a4d20429eed80f0bfe2c42b63e0ebae1af071b4c5cdda221b69bebf |
| P38-arity-control | 0 | 2469 | evidence/P38-arity-control.log | e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855 |
| P39-empty-traces-shape | 1 | 5 | evidence/P39-empty-traces-shape.log | 2ed27c1421e6928dbe13dbfdb5c59e1045b30341fe7ebe05700006bc5ac572c0 |
| P40-empty-steps-shape | 1 | 3 | evidence/P40-empty-steps-shape.log | 2ed27c1421e6928dbe13dbfdb5c59e1045b30341fe7ebe05700006bc5ac572c0 |

## Return boundary

Local report only. No source repair, commit, push, issue/PR comment, external paste, owner contact, merge, or acceptance decision. Final HEAD and gate hash unchanged; tracked worktree clean. The audit worktree and S1 build artifacts remain intact. No rebuilt project exists under the runtime root to retire. Upward delivery is this report and own STATUS only.
