# Q-001 — retained whole-build layers versus executable driver controls

NOTE-002 received after S1/S2/P01/P02 completed: spent **2/8 substantive, 2/30 targeted**. No command was wasted; no ceiling overrun. Its 0/0 statement was stale on receipt and is corrected in STATUS.

Eight substantive slots now have these concrete uses:
1. S1 cold `nix develop --quiet -c just lean` — executed GREEN.
2. S2 `nix develop --quiet -c just ci` — executed GREEN.
3. S3 `just lean` with the existing non-inversion theorem proof replaced by `sorry` through a private mount; expect only dependency rejection.
4. S4 `just lean` with forbidden axiom, intermediary def and consuming theorem, through a private mount; direct and transitive uses expected to be named.
5. S5 `just lean` with registered new outside-namespace module importing Std, plus vendor-first loader input; one real build serves extension and alias/dependency rows.
6. S6 `just lean` with the same S3 poison and axiom-gate call removed; expect GREEN (necessity), not a baseline-green decoy.
7. S7 `just lean` through symlinked cwd with relative project loader entry supplied at actual `lake env lean` calls; one real build serves both equivalent-path forms.
8. S8 `lake build` at a private mounted view of accepted d670323 using separate copied warm build artifacts; both compiled Expr comparison and whole-base consumer scan follow on those artifacts.

S5/S7 loader inputs can be injected by a runtime-owned `lake` interceptor which forwards genuine whole builds unchanged and alters only the environment of the real `lean` driver elaboration. This is an input mutation, not a candidate repair; tracked tree stays immutable. These are still substantive calls and each will have one complete receipt. Direct vendor-first and relative-entry probes independently confirm classification against nominal S/B/T.

**The exact remaining layer conflict is B-minus-S and missing-authority.** Initial brief explicitly allocates these as direct-driver targeted calls and demands clean inputs. NOTE-002 §4 now says not to reclassify frozen substantive rows as targeted. The production shell imports S itself and Lake supplies LEAN_PATH; changing an input to reach these guards requires driver input injection, or the direct Lean probes already stipulated by the original five-element contract. Running an ordinary `just lean` with absent outer LEAN_PATH cannot reach G-001 (Lake reinstates it); an early build/import/setup failure cannot be credited.

If the retained mandate requires **separate whole-build-prefixed** B-minus-S and G-001 executions, add:
9. S9 genuine `just lean` with AXIOM_S_MODULES missing KelGroups.Tests only at the final gate driver call; expect sole B-minus-S identity.
10. S10 genuine `just lean` with the final gate's import list replaced by import-Lean-only and its LEAN_PATH unset; expect sole missing-authority finding. Empty versus unset remains two distinct direct elaborations unless both are required as whole-build calls, in which case total is **11**, not 10.

Thus the concrete gap is **2 substantive slots (10 required versus cap 8)** for one full-path invocation per retained row, or **3 (11 versus 8)** if both empty and unset must each use a full path. No proposed number re-labels a build as a probe. These extra builds only establish mandatory-wrapper reachability under instrumentation; the clean direct driver controls remain separately counted.

Please bind which layer is required for B-minus-S and missing-authority. If the full-path rows remain mandatory, disposition the exact gap; I will not spend beyond 8 or silently omit them. Pending the answer I continue the already-authorized independent direct-driver controls and free source/provenance review. No additional substantive invocation until this reconciliation is resolved. This is not a request for candidate repair, a new submission, or automatic budget increase.
