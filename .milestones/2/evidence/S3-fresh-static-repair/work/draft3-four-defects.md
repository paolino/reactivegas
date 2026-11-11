# DRAFT — NOT A SUBMISSION. Nothing here is frozen, nothing consumes the cap.
# Purpose: record completed static analysis for the four defects in d05d249d,
# so a desk-authorized third bounded submission can be assembled without rework.
# Base 3590c001, read-only worktree. No execution.

## Defect A — canonical_economy_holds killable by OP-23 atom (parent verified, repair owner concurs)
- checkCanonicalEconomy (Invariants.lean:1959) calls stepEvent backdonate-1 directly; OP-23 mutant
  (Step.lean:90-92, drop members.foldl distribution, retain comune debit) leaves alice==0, bob==0
  → member equalities false → decide fails. Static semantic argument: PREDICTED-KILL(c).
- Isolation caveat (execution-order): step_backdonate_inv proof (:410) also breaks under OP-23,
  so a naive full-file build halts before :1959 elaborates. The check VALUE is false AND the
  check is unreachable in a single-mutant full build — both stated, never blurred.
- Reachability classes for check evaluation (applies to all rows):
  REACHABLE (Step.lean decide-checks :415-470 elaborate before any Invariants proof can break them;
    OP-63 victims in Invariants.lean before :1600 elaborate before baseHook_votes breaks),
  PROOF-BLOCKED (same-file earlier proof breaks first, e.g. Invariants inversions :197-430 for Step mutants),
  IMPORT-BLOCKED (a dependency file fails first, e.g. KelGroups-mutant breaks in Invariants.lean
    need their KelGroups imports to elaborate — Vote/Fold mutants break Vote.Invariants).

## Defect B — NO-MUTANT re-audit: check-VALUE sensitivity vs build-REACHABILITY (all 31 OPEN rows)
Standing NO-MUTANT (value insensitive AND correctly so): #1 admissionPreservation, #2 app_members_mutant_caught
  (Step.lean, reachable, memberWritingApply-local), #7 comuneSanity (no production call), #12 disjoint_mutant
  (OP-39 touches assent arm only; trace needs dissent-arm erase which is intact; verified by reading
  checkI57Disjoint body), #14 franchise_mutant (cast arm intact, erase no-op on empty dissents),
  #15 noexpiry, #21 r45, #23 sweep_mutant (sweepDuplicating-local), + mirrors #25, #30.
  → 10 rows stay OPEN.
Value-FLIP → PREDICTED-KILL(c) (21 rows): #3 V3Base (OP-63 post→pre, OP-40 filter), #4 departure
  (OP-67G windUpAdmin-collections, OP-57/57B appFold-swap), #5 recompute (OP-63), #6 canonical (OP-23),
  #8 direct_only (OP-54 nonAdmin-refused, OP-55 comune-refused, OP-56 dup-refused — one atom each),
  #9 exhaustive (inherits #8 via conjunct), #10 auth (OP-49 bob-openQuestion), #11 boundary (OP-49),
  #13 i57_exhaustive (OP-58B members-wipe via Reactivegas.apply→applyIntegratedEvent app branch),
  #16 noop (inherits #11 OP-49 + nonAdmin OP-54), #17 nostale (inherits #3/#5),
  #18 partition (OP-63, OP-40), #19 policyfree (OP-40: closed-qp retained open, isNone fails),
  #20 policyfree_mutant (inherits #19 via conjunct), #22 integrated_witness (inherits #5 via conjunct),
  #24 sweep_witness (OP-40: retained q re-closes → duplicate), + mirrors #26, #27, #28, #29, #31.
  Each flip row carries its upstream-block site (first failing proof before the check) as isolation caveat.
Resulting distribution if frozen: KILL 89 (71 THEOREM-FAIL + 17 PROOF-FAIL + 1 MIXED),
  OPEN 10, OBSERVED 31, ELAB 60, others unchanged, 207 lines.

## Defect C — conservation 14-arm split (escrowOf = accepted+pending sums, State.lean:76-79)
THEOREM-FAIL (mutant moves money, conservation proposition false at witness — 10 arms):
  deny, deposit, withdraw, transferCassa, backdonate, pledge, refuse, correct, close, fail.
PROOF-FAIL (mutant moves no money; statement stays true; RED only via broken inversion equation — 4 arms):
  openPurchase (empty-collection escrow 0), grantPermission (permitted flag only; Step.lean:55 pure
  collections update — the OP-11 case), donate under named OP-22 guard mutant (success-path money
  unchanged), acceptPledge (pending→accepted within one escrowOf sum).
  Gap named: no donate-EFFECT mutant exists in the map; arm classification is relative to NAMED mutants.
COLL-conservation row kind: MIXED with this arm table (A-ASSESSMENT L2 to be rewritten accordingly).

## Defect D — measurement request rewrite (reviewable form)
- Chain count: 8 unmeasured (C-VOTEFOLD, C-VOTEVAL, C-VOTESTATE, C-VALIDATE, C-INTEGRATION, C-FOLD,
  C-KSTATE, C-RSTATE) + C-STEP measured. The "7" was wrong.
- Invocation count: 8 mutant builds + 8 restore reruns + 1 cold + 1 isolated check-elab = 18 timed
  invocations. Ceiling proposed: 18 (not 12). States its excess over Phase-1's 3-build ceiling plainly;
  no sampling (8 chains are 8 distinct closure classes; representative subset would drop a class).
- Exact argv/cwd: `lake build`, cwd `<scratch>/lean`, toolchain pin 4.25.0 (retained R-BUILD2 lean path
  /nix/store/4gr3n4nrp0xxgykyyzdxi3xjj2ikn5x1-lean4-4.25.0/bin/lean); first receipt line `lake --version`
  == 4.25.0. Prerequisite: scratch checkout detached at 3590c001, porcelain-empty before/after each cycle.
- Eight one-atom diffs (exact, frozen in draft): C-VOTEFOLD Vote/Fold.lean:76 pred→fun _ => True;
  C-VOTEVAL Vote/Validate.lean:58 if-gate→.ok (); C-VALIDATE Validate.lean:145 `if isAdmin signer gs`
  →`if True`; C-INTEGRATION Integration.lean:210 `change := none`→`change := some
  (BaseChange.memberAdmitted "measure")`; C-FOLD Fold.lean:37 `≥`→`>`; C-KSTATE State.lean:50
  `(adminCount gs + 1) / 2`→`adminCount gs + 1`; C-RSTATE State.lean refundAll fold→`m`;
  C-VOTESTATE: TO-FREEZE (needs one source read of Vote/State.lean simple-literal site; candidate:
  a guard/length literal touch — not yet read, stated gap).
- U-CHECK target (named): scratch file importing Reactivegas.Invariants evaluating
  `#eval checkSweepIdempotent`, argv `time lake env lean <scratch>/check-eval.lean`, cwd `<scratch>/lean`.
- Distinguishing observables per invocation: per-module `[n/27] Built/Replayed` lines with times (retained
  log), plus first-failure obligation (file:line + error text) or GREEN-with-changed-semantics note.
- Isolation design point (for Phase-2, not solved here): single-mutant full builds halt at the FIRST
  failing obligation in elaboration order, so downstream predicted kills (Defects A/B reachability notes)
  need a separately authorized isolation design; this request measures chains, it does not solve that.

## EVID-KIND 7th-column mapping (for OPMAP-v9 if authorized)
KILL→THEOREM-FAIL default; PROOF-FAIL for v7-(P) lines {64,65,66,67,68,69,70,84,85,87,91,92,93,95,96}
  + L25 + L34; MIXED for L2 (10/4 arm table); RECEIPT kind: none (no Phase-2 execution; t54-PARTITION
  and S4-B-O5 retained as CORROBORATION text at actual identity only). OBSERVED→CASCADE (all name
  upstreams). OPEN→NONE. Others→N-A.
