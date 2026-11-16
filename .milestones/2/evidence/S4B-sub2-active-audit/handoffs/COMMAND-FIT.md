# Pre-admission command fit — NOT EXECUTED

Candidate `94bb7bb64324a48f7361252556b4d15e45b3923f`; base `3590c0015b84fd58004bf6fb44dd18b107304c48`; full seven-commit range.

The concrete mutation subplan below has 44 separate single-file Lean invocations against current-candidate scratch inputs. Its targeted shortfall alone is 34. No invocation is authorized or executed by this document.

Even the diagnostic one-per-theorem subset (M01, M04, M08, M09, M11, M12, M15, M17, M19, M22, M24, M25, M26, M30, M32, M35, M42) contains 17 calls, exceeding 10 by 7. This comparison is NOT a proposed narrowing of the full atom mandate.

All commands have cwd `/code/reactivegas-66-s4b-audit4/lean`. `LEAN_PATH` names only the future fresh clean candidate library, which does not yet exist. Current source inputs, exact single edits, preserved theorem/proof suffixes, expected diagnostics, output receipt paths, and full argv arrays are bound in `COMMANDS.json`. Each mutant source is retained. These are preparation artifacts, not executable evidence.

| ID | Definition / atom | Original theorem | State |
|---|---|---|---|
| M01 | conservationB / cassa | conservation_corr | NOT-RUN |
| M02 | conservationB / conti | conservation_corr | NOT-RUN |
| M03 | conservationB / escrow | conservation_corr | NOT-RUN |
| M04 | solventB / member-domain | solvent_corr | NOT-RUN |
| M05 | solventB / balance | solvent_corr | NOT-RUN |
| M06 | solventB / pledge-domain | solvent_corr | NOT-RUN |
| M07 | solventB / pledge-amount | solvent_corr | NOT-RUN |
| M08 | insolventB / existential | insolvent_corr | NOT-RUN |
| M09 | uniquePledgesB / user-binding | uniquePledges_corr | NOT-RUN |
| M10 | uniquePledgesB / pledge-equality | uniquePledges_corr | NOT-RUN |
| M11 | allUniquePledgesB / composition | allUniquePledges_corr | NOT-RUN |
| M12 | escrowHeldB / actor | escrowHeld_corr | NOT-RUN |
| M13 | escrowHeldB / amount | escrowHeld_corr | NOT-RUN |
| M14 | escrowHeldB / absence | escrowHeld_corr | NOT-RUN |
| M15 | governanceEnactsB / actor | governanceEnacts_corr | NOT-RUN |
| M16 | governanceEnactsB / all | governanceEnacts_corr | NOT-RUN |
| M17 | doubleEntryB / conto | doubleEntry_corr | NOT-RUN |
| M18 | doubleEntryB / cassa | doubleEntry_corr | NOT-RUN |
| M19 | canCloseGroupB / member-balance | canCloseGroup_corr | NOT-RUN |
| M20 | canCloseGroupB / collections | canCloseGroup_corr | NOT-RUN |
| M21 | canCloseGroupB / cassa-balance | canCloseGroup_corr | NOT-RUN |
| M22 | pendingWellFormedB / nodup | pendingWellFormed_corr | NOT-RUN |
| M23 | pendingWellFormedB / proposer | pendingWellFormed_corr | NOT-RUN |
| M24 | membersCoherentB / key | membersCoherent_corr | NOT-RUN |
| M25 | pendingCoherentB / composition | pendingCoherent_corr | NOT-RUN |
| M26 | wellFormedB / members-nodup | wellFormed_corr | NOT-RUN |
| M27 | wellFormedB / pending-nodup | wellFormed_corr | NOT-RUN |
| M28 | wellFormedB / member-coherence | wellFormed_corr | NOT-RUN |
| M29 | wellFormedB / pending-coherence | wellFormed_corr | NOT-RUN |
| M30 | enactsB / enactment | enacts_corr | NOT-RUN |
| M31 | enactsB / state | enacts_corr | NOT-RUN |
| M32 | questionCleanB / assents | questionClean_corr | NOT-RUN |
| M33 | questionCleanB / dissents | questionClean_corr | NOT-RUN |
| M34 | questionCleanB / disjoint | questionClean_corr | NOT-RUN |
| M35 | sweepReadyB / open-nodup | sweepReady_corr | NOT-RUN |
| M36 | sweepReadyB / closed-nodup | sweepReady_corr | NOT-RUN |
| M37 | sweepReadyB / disjoint | sweepReady_corr | NOT-RUN |
| M38 | sweepReadyB / open-clean | sweepReady_corr | NOT-RUN |
| M39 | sweepReadyB / closed-clean | sweepReady_corr | NOT-RUN |
| M40 | sweepReadyB / closed-verdict | sweepReady_corr | NOT-RUN |
| M41 | sweepReadyB / lookup | sweepReady_corr | NOT-RUN |
| M42 | voteWellFormedB / sweep | voteWellFormed_corr | NOT-RUN |
| M43 | voteWellFormedB / open-verdict | voteWellFormed_corr | NOT-RUN |
| M44 | voteWellFormedB / threshold | voteWellFormed_corr | NOT-RUN |

Exact argv, one separately charged invocation per paragraph:

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M01.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M02.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M03.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M04.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M05.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M06.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M07.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M08.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M09.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M10.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M11.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M12.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M13.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M14.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M15.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M16.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M17.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M18.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M19.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M20.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M21.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M22.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M23.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M24.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M25.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M26.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M27.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M28.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M29.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M30.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M31.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M32.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M33.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M34.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M35.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M36.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M37.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M38.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M39.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M40.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M41.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M42.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M43.lean'
```

```sh
'nix' 'develop' '--quiet' '-c' 'env' 'LEAN_PATH=/code/reactivegas-66-s4b-audit4/lean/.lake/build/lib/lean' 'lean' '-DautoImplicit=false' '/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-sub2-final/mutants/M44.lean'
```

## Fit limits and remaining full mandate

The 44 operators were read as descriptions from the prior auditor and rebound to current source bytes, one exact production-body edit each. No previous KILLED state, raw diagnostic, compiled dependency, or acceptance was transferred. All 17 current source-level new-mirror table entries reconcile with these definitions; this is not the required compiled denominator and makes no completeness claim about undiscovered declarations.

The blocker does not depend on treating 44 as a global minimum. It is the cost of these concrete independently isolated commands. The operator one-per-theorem subset already exceeds the targeted allowance. Process sharing is permitted only when separate intended outcomes are actually reached and retained. Putting 44 Lean invocations under one shell does not make them one operation; combining edits into one mutant forfeits single-variable isolation. No compiled, unmasked multi-world batch instrument is bound in this packet. Designing and validating a different instrument is not an established cost saving.

Still required beyond this subset: clean mandatory baseline; newly introduced counterpart-absent and theorem-absent controls; repaired opaque/module discovery and classifier-own-diagnostic control; present-but-disabled checker/nonce enforcement; P01 and P07 real production-path controls; clean final full CI and final axioms/totality; both classification axes with nonempty positive and can-fail negative controls; exact statement/proof bindings and nondegenerate witnesses; P01/P07 accurate relatum scope plus independent compile/positive/negative body-chain probes and clean/defective close witnesses. No part is waived, downgraded or inherited.

A conventional substantive schedule would use nine whole-path invocations (baseline, counterpart absent, theorem absent, opaque/module discovery, classifier omission, P01 body, P07 body, checker-noop, final CI). Its mutations and integration probes are not frozen here: the already concrete targeted subplan fails admission before they are needed. P01/P07 shadow compile/negative/positive calls would add six targeted operations under the governing layer rule. Further census/witness/axiom and instrument controls have not been declared free or bundled into an imaginary exact total. Thus 34 is a lower bound on the shortfall of this 44-call subplan, not the asserted final full-audit deficit.

Return AUDIT-CONTRACT-BLOCKED before START. Actual new spend: 0 substantive / 0 targeted. Historical 6/59 is preserved separately; cumulative ceilings 15/69 confer only the new 9/10. No 54-operation allowance exists.
