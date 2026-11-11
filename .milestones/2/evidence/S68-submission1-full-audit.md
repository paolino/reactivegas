# Submission 1 audit — #68 proposer assent

AUDIT-VERDICT submission=1 verdict=FINDINGS

Independent advisory audit of `e9db9a181f636e1d50862ea3990145e5d11c95e8`. The frozen gate passes, but the historical fold guarantee has been withdrawn over an executable part of its originally quantified domain. The retained mutation evidence also cannot independently establish the exact scratch changes and frozen-test identity for the entire two-path campaign. Acceptance remains the ticket owner's decision.

## Frozen inputs and provenance

- Original pre-slice base: `214f9c085a840d916d879e49c55b66ff2f2d0c37`.
- Integrated pre-slice equivalent: `0cc8fb5b4f9a78e9abdac787026c233a3ed12ec7`; RED `3c3901424f6d2bd0973aa815cb05bb4fc001cfa4`; candidate `e9db9a181f636e1d50862ea3990145e5d11c95e8`.
- Accepted integration base: `4a6cd87fcbc3e4a536bbc9f240f5efe5704022af`. Git independently shows six planning commits, RED, GREEN above it.
- Brief SHA-256: `cc43324a4682e6d543a27bd693b3d09736824fa10898ef79ecdc520b85778f97`.
- Spec SHA-256: `32bc95ebc8b184ede1348934d06d9a69ef68bc75d8d287ba1d62038485a2cdcd`.
- Gate v3 SHA-256: `29e49c9dbaf9d20205ad09967dfe3235d85926be69a32a5aba74a2184ed482c8`.
- Oracle SHA-256: `9448e889e1b8149f356c23c3706323f7c6a96d4d57541c77f6c9d6e69bf96428`.
- GREEN handoff SHA-256: `ea787c92520ce0300626c129736535063957bad5c11fb6188b57471c077f2b64`; original and integrated handoffs are byte-identical. Independent `verify-commit-handoff` exits 0 against the integrated candidate and manifest.
- Consolidated RED+GREEN production patches before/after rebase are byte-identical: SHA-256 `a47e2439c7a01f5467934ee8c7dce6e2df49b7a2ce6d521eea4fd6c2782bee79`.

The literal `214f9c0..e9db9a1` diff has nine files, not six: the other three are exactly the disclosed upstream #66 changes (`justfile`, `Reactivegas/Trace.lean`, and `check-trace-coverage-agreement`). The six-path implementation delta is unchanged by that integration. The GREEN handoff describes GREEN's parent-to-commit delta, not the consolidated RED+GREEN patch; these were checked separately. No unauthorized implementation path was found. Mandate/oracle, arithmetic module, vote subtree, and economic Step are unchanged relative to the original base.

Full mandate/model/input hashes: [frozen-inputs.json](../evidence/frozen-inputs.json). Git reconciliation: [provenance.txt](../evidence/provenance.txt); per-source hashes: [source-hashes.json](../evidence/source-hashes.json).

## Invariant verdicts

Undeclared severity defaults to BLOCKING. PASS below certifies the stated behavior/proof at the documented boundary; it does not claim every arbitrary semantic mutation was tested.

| Row | Verdict | Executed evidence and scope |
|---|---|---|
| I68-01 zero-open | PASS | Both frozen oracle guards and worker guards elaborate; independent integrated check prints `true`. Exact-one-edit integrated restore prints `false` and fails its guard. |
| I68-02 self-assent bar | PASS at validation boundaries | n>1 historical and integrated validators return the exact `proposerSelfApproval(key,id)` payload; independent guards distinguish it from duplication, unknown proposal, and non-admin errors. Raw-fold preservation is separately FAIL below. |
| I68-03 sole-admin agency | PASS | Historical and integrated two-step n=1 guards execute; propose pends, explicit self-approval enacts. `sole_admin_self_approval_ok` is axiom-clean. |
| I68-04 enactment sets/current count | PASS | All 13 oracle guards execute, including n=2/3/5, proposer+one-other killer, and admission-driven 2→3 threshold change. Pending-state preconditions exclude empty-store vacuity. |
| I68-05 arithmetic | PASS | Two copies each of `majority_table` and `majority_not_strict_on_even`, and `majorityZero/One/Two`, are byte-identical; full build and guards pass. [arithmetic.json](../evidence/arithmetic.json). |
| I68-06 WellFormed/every transition | FAIL | F-01: a seven-event raw fold from empty violates the restated predicate. Validated traces and integrated folds with the strong initial invariants have proved preservation, which is a restricted guarantee. |
| I68-07 dependent guarantees/proofs | FAIL on scope; PASS on proof trust | All 486 theorem constants in the six changed modules have only accepted axioms. The unconditional historical fold guarantee is lost; no proof trust issue was found. |
| I68-08 witnesses and two-path mutations | BLOCKED on complete campaign provenance | 13 oracle and 14 worker `t68` guards execute; both archived campaigns have semantic Lean failures. Fresh B spot-check is exact and killed. Frozen scratch/test identity for the original complete campaign is not independently reconstructible from the retained artifacts; F-02. |
| STATEMENT-SCOPE | FAIL | Independently extracted before/after signatures confirm new strong/validation/admissibility hypotheses. F-01 supplies the excluded executable case. |
| FAILURE-MODES | PASS for changed boundaries | 12 independent guards check exact validation errors, whole-state identity after rejected self/duplicate approvals, hook-error atomicity, and corpus behavior. Full frozen oracle tests threshold-at-enactment after admin growth. |
| INVERSIONS | Structural PASS 14/14; exact-premise 11/14 | Five coverage-instrument controls detect defects. Three inherited statements omit stall; recorded separately for onward disposition, without reopening the accepted six-theorem #66 repair. |

Evidence: [full-lean.log](../evidence/full-lean.log), [oracle.log](../evidence/oracle.log), [failure-modes2.log](../evidence/failure-modes2.log), [axioms-final.log](../evidence/axioms-final.log), [inversions4.log](../evidence/inversions4.log).

## Blocking findings

### F-01 — historical reachable-fold guarantee withdrawn

**Rows:** I68-06, I68-07, STATEMENT-SCOPE. **Property class:** antecedent strengthening conceals a reachable violation of an originally unconditional fold invariant.

At base, `KelGroups.foldGroup_wellFormed` quantifies `digest`, `appFoldFn`, `initial`, and every event list, without admissibility. At candidate, it adds `validKey`, `config`, and a caller-supplied `TraceAdmissible ... events` proof. `foldGroup` itself still executes all raw events without validation.

The independent witness starts with `emptyState 0`, introduces admin a at threshold zero, introduces b with a's separate approval, introduces c with b's approval, proposes removing c by a, then applies a's own approval. All seven calls execute in the actual historical fold. The result has three admins and pending `remove:c` approvals `["a"]`. Lean proves `¬ WellFormed after`; `#print axioms Audit68.raw_reachable_violation` reports only `propext`.

The validator explicitly returns `proposerSelfApproval "a" "remove:c"` before the last raw step. Thus the excluded case is **raw-fold reachable from empty, but not admissible through the validating boundary**. This is not an integrated approval bypass. The distinction is exactly why the owner's assertion that no reachable guarantee was lost is too broad: the original historical theorem covered this raw executable domain. R68-07 and I68-06 still require preservation by every transition on both paths; the packet provides no withdrawal of that promised domain. The validation-free architecture and the stronger invariant now conflict at that domain boundary. Compiler GREEN does not resolve that mandate conflict.

Evidence: [ScopeWitness.lean](../evidence/ScopeWitness.lean), [scope-witness3.log](../evidence/scope-witness3.log), SHA-256 `0a2799b7a129f62416f74cf6b1e32bb2f31e05c707ee710f59aff8278fc1ddcb`; [public-signatures.md](../evidence/public-signatures.md), `KelGroups.foldGroup_wellFormed` and `applyApprove_preserves_wellFormed` rows. Instrument is a refutation receipt, not a proposed shipped repair.

### F-02 — complete two-path mutation provenance remains unverified

**Row:** I68-08 / mutation adequacy. **Property class:** semantic RED evidence without enough frozen input identity to prove exact-one-edit execution with unchanged tests.

The archived A/B logs show compilation of the changed production module followed by domain-relevant proof/witness failure; they are not mere spelling-tripwire failures. A fails historical empty-open equalities and executed stale/bootstrap guards. B fails the integrated empty-insertion equality. However, the retained handoffs contain no A/B scratch diff, scratch blob manifest, or before/after test hashes. The owner journal states “1-line revert verified,” and the scratch tree has been removed; that statement cannot be independently checked against a retained scratch candidate. The ordinary GREEN handoff identifies the unmutated candidate only.

The one authorized spot-check selected B. It preserves an exact one-line diff and hashes, compiles that module successfully, and runs a frozen independent check through real `applyIntegratedEvent`: candidate `true`/exit 0, mutant `false`/exit 1 at the guard. Witness, Tests, oracle, and candidate source hashes remain unchanged. This closes the spot-check's own provenance and semantic sensitivity; it is **not** a rerun of the entire frozen gate, nor retroactive proof of A's scratch identity. No second mutation was run beyond the brief's one-spot-check boundary.

Evidence: frozen A/B logs hashed in [frozen-inputs.json](../evidence/frozen-inputs.json); [mutant-manifest.json](../evidence/mutant-manifest.json), [mutant.diff](../evidence/mutant.diff), [mutant-compile.log](../evidence/mutant-compile.log), [mutant-negative2.log](../evidence/mutant-negative2.log), [final-integrity.json](../evidence/final-integrity.json). Fresh B semantic RED hash: `596af5b77b1572f1d2c85018cf827e421f7a986d99342efe3c4fcd7b2cf5d5fa`.

## Independent statement-scope dispositions

The [public signature inventory](../evidence/public-signatures.md) was derived from the base and candidate sources with namespace-aware extraction, independently of the owner's inventory. Complete frozen base/candidate copies accompany it. The wider [signature inventory](../evidence/signature-inventory.txt) includes private helpers; root aliases are disambiguated by the public inventory.

| Guarantee / type | Change and ruling | Reachability and completeness verdict |
|---|---|---|
| `PendingWellFormed`, `PendingCoherent` | Proposer membership becomes count-indexed exclusion; Nodup remains. R68-07 requires that regime change. | Sole-admin exception is in the predicate. It does not itself guarantee persistence across later count changes. |
| `WellFormed`, `BasePendingCoherent` | Adds the integrated-store coherence field; old structure had four fields, new has five. Both-path ruling supports checking the integrated store. | Empty stores establish this field; arbitrary supplied initial stores did not previously owe it. General initial-state compatibility is therefore conditional. |
| `enact_preserves_wellFormed` | Adds `StrongCoherent gs` and `StrongBaseCoherent gs`. | These prevent count changes from exposing stale self-credit. The ruling motivates an inductive invariant but does not explicitly withdraw the old h-only domain. Validated-trace/strong-initial integrated inductions establish the strong facts; all raw reachability does not. |
| `finishEnact_preserves_wellFormed` | Same two added strong hypotheses. | Erasure preserves strong facts, including across member-count changes. Same domain qualification as enact; no new validation premise here. |
| `tryEnact_preserves_wellFormed` | Same two added strong hypotheses. | Both threshold arms checked; no independent proof that every h-only input meets the new premises. |
| `applyPropose_preserves_wellFormed` | Same two added strong hypotheses. | New entry is empty; old siblings need strong facts through possible zero-threshold enactment. Strong facts hold throughout admitted historical traces / integrated traces from strong initial states, not for every raw fold state. |
| `applyApprove_preserves_wellFormed` | Adds strong facts and `validateApproval ... = .ok ()`. | Admission implies admin, nonduplicate, and proposer-bar conditions. Sole-admin self-approval enacts and erases its own entry. The new validation premise fails at the concrete reachable raw call in F-01. |
| `applyEvent_preserves_wellFormed` | Adds strong facts plus an approval-arm validation premise. | Propose/app arms do not need this validation premise; approve threads it. F-01 reaches exactly the excluded approve arm. |
| `foldGroup_wellFormed` | Adds validator inputs plus trace admissibility for **every** event. | Historical fold does not produce that proof or enforce those checks. It remains a caller obligation. Completeness over the old raw event-list domain fails, even though the admissible-trace theorem is proved. |
| proposer-membership family | Two old membership declarations removed; namespaced exclusion theorem and sole-admin validation theorem added. Root alias not replaced. | Membership reversal is ruled; valid positive n=1 and n>1 witnesses execute. Root-name compatibility changed; the semantic pair is available namespaced. |
| `requireAdmin` | Private→public; same type/body. | Visibility change, no new runtime premise or behavior. |

`foldEvents_preserves_all` proves the three invariants by induction assuming historical `TraceAdmissible`; `foldIntegrated_all` proves them by induction assuming the three initial invariants. The integrated induction covers all four constructors (direct/propose/approve/app) and retains the pre-state on error. Its successful branches obtain validation internally. Strong pending invariants start vacuously for empty stores, and a sole-admin self-approval cannot remain pending after a successful enactment. These are valid proofs for that initialized/validated domain. They do not discharge historical `hadm` for arbitrary raw lists or justify the owner's unrestricted completeness claim. Private integrated proofs are included in the 486-constant axiom audit.

## Inversions and inherited limits

Compiled `Event` supplies 14 constructors. Each has exactly one public successful-step inversion with the matching constructor hypothesis. Checks are over the elaborated environment; the source parser independently reconciles 163 declared public theorems with 163 elaborated declarations.

| Constructor | Public inversion | Exact-premise verdict |
|---|---|---|
| openPurchase | Reactivegas.step_open_inv | PASS |
| grantPermission | step_grant_inv | PASS |
| denyPermission | step_deny_inv | PASS |
| deposit | Reactivegas.step_deposit_inv | PASS |
| withdraw | Reactivegas.step_withdraw_inv | PASS |
| transferCassa | Reactivegas.step_transferCassa_inv | PASS |
| donate | Reactivegas.step_donate_inv | PASS |
| backdonate | Reactivegas.step_backdonate_inv | PASS |
| pledge | step_pledge_inv | FAIL: omitted non-stalled guard, inherited |
| acceptPledge | step_accept_inv | FAIL: omitted non-stalled guard, inherited |
| refusePledge | step_refuse_inv | PASS |
| correctPledge | step_correct_inv | PASS |
| closePurchase | step_close_inv | FAIL: omitted non-stalled guard, inherited |
| failPurchase | step_fail_inv | PASS |

The audit extends the frozen helper's converse check from its six designated inversions to every derived inversion. Eleven converse proofs close. The remaining three fail at the omitted stall premise. A separate compiled close example exhibits the theorem's complete exposed conjunction as true while `stepEvent` returns `none` on a stalled state. This is a converse counterexample, not a proof that the forward theorem is false, and no claim is made that this app state is reachable from a valid economic genesis. These statements and Step are unchanged by #68; they are recorded in [onward-discoveries.md](onward-discoveries.md), not assigned as a repair obligation for this slice.

Self-falsification: a new unrepresented constructor, removal of a live inversion, duplicate inversion, misbinding donate's observed premise to backdonate's name, and duplicated successful-step hypothesis are all detected. The final output lists each control. The temporary failed converse proof attempts mention `sorry`; the instrument rejects such generated attempts. **No candidate theorem depends on `sorryAx`.**

## Proof trust, failure modes, and resource receipt

The full frozen gate was run independently by its exact manual legs. Full Lean, toolchain contract, corpus exact-true command, and all 13 oracle guards pass; the required tripwire returns 1 with empty output (expected absence). The six changed modules' 486 theorem constants, including private/generated declarations, receive actual theorem-qualified `#print axioms` commands via Lean syntax identifiers; only `propext`, `Classical.choice`, and `Quot.sound` occur. A 27-file lexical sweep finds zero code `sorry`/`admit`; controls find both inserted admissions and exclude comments/string lookalikes. Axiom checks provide proof evidence beyond that textual sweep.

Changed failure behavior remains observable: first barred proposer approval has its own keyed error; true duplicate, non-admin and unknown-proposal variants retain distinct results. Rejected approvals leave the full integrated aggregate unchanged. The sealed-hook rejection now happens at the explicit approval step; the revised corpus and independent guard compare against the post-propose state. Threshold is still computed from current canonical membership; the frozen 2→3 admin-change scenario executes both pending and enactment boundaries. The corpus's 7→10 event expansion retains departure cleanup, role-change closure, rejection, and corruption controls with nonzero balances and distinguishable identities.

[Command receipts](../evidence/command-receipts.md) contain every verification command, exit, duration, output path and full SHA-256; [machine-readable ledger](../evidence/command-receipts.json) has 24 entries. Setup errors are retained and excluded from domain results. Budget: **1/2 full Lean invocations, 16 targeted Lean invocations including instrument/setup failures, one independently rerun mutant only**. Targeted runs use warm candidate modules; initial cache state of the full invocation was not recorded. The candidate was never mutated, so the final axiom recheck uses freshly verified candidate artifacts without mutant contamination; no second full rebuild was justified.

Audit model: Codex `gpt-6-astra`, reasoning effort `high`, observed in process 901396 argv. Pane `%528`, `reactivegas:6.3`, differs from ticket owner `%512` and author `%519` (muse). No subagents, owner contact, candidate edits/staging/commits, pushes or publication. START acknowledges POINTER-1788602143-902483. The explicit codex pin is compatible with the standing auditor family set and muse exclusion. The owner's one recorded build-ceiling raise is NOTE-005, 6→10; this audit uses its separate explicit 2-build budget.

Final candidate identity and clean tracked state are recorded in [final-integrity.json](../evidence/final-integrity.json). Own shadow build output is retired after hashing; the mutated source, manifest, instruments, raw logs and candidate build artifacts are preserved. Wall-time receipt and final evidence manifest are appended at freeze. One submission, one report, then stop.

Freeze UTC: 2026-09-05T10:17:47.969Z; journaled audit wall time 1263 seconds from START (pre-START packet/skill preflight additional). Observed process launch/elapsed: Sat Sep  5 10:55:20 2026    1347. Own shadow object retirement: 723800 bytes; only symlinks and the owned Integration.olean removed, retained mutant source unchanged. Final HEAD e9db9a181f636e1d50862ea3990145e5d11c95e8; git status empty.

Final [evidence manifest](../evidence/evidence-manifest.json) SHA-256: e575039570f456814aa6d2f6481bd0045e3a86895d54c8ef59bcb3b7114bcdd9.
