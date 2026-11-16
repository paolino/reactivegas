# COST-MODEL — measured units, receipt bindings, per-identity reconciliation
Base 3590c001. Static only. No new measured cost is claimed by this file;
a statically repaired formula is not a measurement.

## 1. Counted units (defined; multiplier prose withdrawn)
- U-COLD: full cold `lake build` wall time with retained log. UNMEASURED (cold log missing, F-07).
- U-CHAIN(C): incremental rebuild wall time per module after a single-atom touch in chain-root module C, with retained per-module lines. MEASURED once (C=Step-chain, R-BUILD2). UNMEASURED for all other chains.
- U-CHECK(F): elaboration of file F's witness/check layer (decide theorems, #eval) isolated with timer receipt. UNMEASURED (the retained 2s command mixed elaboration+#eval, F-07).
- U-REPLAY: isolated runtime replay of corpus/trace checks with timer receipt. UNMEASURED.
- U-RESTORE: replay-after-restore verification (retained pattern R-BUILD3, no timings).
- U-OP-EXEC: one admitted single-atom mutant elaboration-to-RED-or-GREEN inside its chain (= U-CHAIN of its module + U-CHECK of owning files). Cost varies by chain; only Step-chain anchored.

## 2. Measured anchors (all read from retained receipts, nothing manufactured)
- R-BUILD2 (evidence/supplemental/P1C-build2-incremental.log + P1C-scratch-variant-donate.diff, exact 1-line guard diff Step.lean:80 `(0<v)`->`(0<v+1)`): Built Reactivegas.Step 1.2s [22/27]; Built Reactivegas.Predicates 445ms [23/27]; Building Reactivegas.Invariants FAILED 7.6s [24/27] with exact-term RED at Invariants.lean:407 (guard conjunct `(isResp && decide(0<v+1))=true` vs expected `(…(0<v))`); 5 Replayed (no timing). LEAN_PATH names the candidate worktree (isolation gap F-07 stands).
- R-BUILD3 (P1C-build3-restore.log): restore replay 27 jobs success, no timings.
- R-CORPUS (P1C-corpusgate.out): `true`, no timing. R-TRACE (P1C-tracetests-summary.txt): 43 checks 0 failures + 7 negative controls pass, no timing.
-(kind note: R-BUILD2's RED is a guard-shape exact-term mismatch at a guard conjunct — mechanism corroboration for OP-22-class rows, NOT a receipt for any OPMAP mutant atom.)

## 3. Chains from source-import-graph.json (rebuild closure of a mutant in root)
- C-STEP (Reactivegas.Step -> Predicates -> Reactivegas.Invariants -> Trace -> TraceTests; 3+89+1+12 theorems downstream): MEASURED (R-BUILD2).
- C-VOTEFOLD (Vote/Fold.lean -> Vote.Invariants, Step, Composition, ...): UNMEASURED.
- C-VOTEVAL (Vote/Validate.lean -> ...): UNMEASURED. C-VOTESTATE (Vote/State.lean -> ...): UNMEASURED.
- C-VALIDATE (KelGroups/Validate.lean -> Integration -> Step -> ...): UNMEASURED.
- C-INTEGRATION (KelGroups/Integration.lean -> Step -> ...): UNMEASURED.
- C-FOLD (KelGroups/Fold.lean -> KelGroups.Invariants -> ...): UNMEASURED.
- C-KSTATE (KelGroups/State.lean -> ...): UNMEASURED.
- C-RSTATE (Reactivegas/State.lean -> Step -> ...): UNMEASURED.

## 4. Per-identity reconciliation (every map OP -> unit + status)
- C-STEP: OP-11..24 (14 arm mutants), OP-25, OP-25B?? NO — OP-25B root is C-RSTATE. C-STEP gets OP-11..24, OP-25, OP-67G, OP-63 pair (baseHook Step.lean:298-303; OP-59 withdrawn so no double count — Addendum-1 Q answered: one invocation covers both L97 L98). Unit U-OP-EXEC(C-STEP). MEASURED-CLASS (anchor R-BUILD2; per-mutant times still vary, anchor is one sample).
- C-RSTATE: OP-25B. UNMEASURED.
- C-VOTEFOLD: OP-39, OP-40 x4 (L40 predicted-survive still needs its execution to confirm), OP-42K, OP-46a/b, OP-50/51/52 (P), OP-52B. UNMEASURED.
- C-VOTEVAL: OP-49 x3 (incl 2 predicted-survives to confirm). UNMEASURED.
- C-VOTESTATE: OP-53 x2. UNMEASURED.
- C-VALIDATE: OP-54, OP-55, OP-56. UNMEASURED.
- C-INTEGRATION: OP-57, OP-57B, OP-58/58B/58C/58D. UNMEASURED.
- C-FOLD: OP-60, OP-61, OP-62 x3, OP-63B, OP-64 x2. UNMEASURED.
- C-KSTATE: OP-67, OP-67b. UNMEASURED.
- ELAB-STATIC 60 + STATIC 2 + ACCEPT 1 + RECOVERED 9 (historical, no new run) + OBSERVED 31 (no new run; cascade argument static): unit U-CHECK(file) or NO-EXECUTION. UNMEASURED where timing needed; elaboration standing on accepted S2 base, not re-timed here.
- OPEN-KILL 31: NO-EXECUTION (OPEN stands; extents in OPEN-EXTENTS-31).

## 5. Old envelope disposition
CORRECTIONS-019 143+1: WITHDRAWN as fitted arithmetic (multipliers undefined, no per-row unit, OPMAP-v7 no cost column). Addendum-1 consistency check noted and preserved as history. NO replacement total is offered here: totals require MEASUREMENT-REQUEST results. The 20/14/4/6/2/1/1 op membership lists are superseded by section 4 (notably OP-59 removal and OP-49/OP-40 survive reclassifications change kill-counts, not run-counts).
