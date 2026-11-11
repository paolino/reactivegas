# Submission receipt — commit-owner-s1, slice S1 (#68 V-2/A-001)

## Hashes
- base=214f9c085a840d916d879e49c55b66ff2f2d0c37 (pre-slice, pushed PR #80 draft)
- red=2aabe20b98f78891e1008f68a5c8de89f157ae3d (Tests-only, +114)
- candidate=0e79796b1830d00364ffedff33506f6c5e400ef7 (6 files, +1532/-169)
- red.diff sha256=27955a97cc4a5f7b46f2d0b02e95b33bfc838ba51ccee5f07969e90640292da0
- green.diff sha256=ea787c92520ce0300626c129736535063957bad5c11fb6188b57471c077f2b64
  (verify-commit-handoff 0e79796 vs green.diff: OK exact)
- gate v3 sha256=29e49c9dbaf9d20205ad09967dfe3235d85926be69a32a5aba74a2184ed482c8
- oracle sha256=9448e889 (load-bearing=2), UNTOUCHED, never shaped to
- fence: exactly the 6 writable files; gate.sh/oracle/specs/vote/Step/Haskell/docs
  untouched (git status clean at candidate; gate.sh hash matches frozen)

## Attempt ledger (ceiling 10 per NOTE-005)
1. baseline gate (tripwire+driver RED as bound) 2. RED-gate exit=1 sha 5f658a70
3. green-gate.log leg-3 RED 4. green-gate2.log leg-3 RED
5. GREEN PASS sha 328363eb (legs 0-5, tripwire-absent, oracle-13, inversion-163)
6. mutant-A scratch sha e8753d7c RED 7. mutant-B scratch sha 690e57d9 RED
(mutantA-gate.log exit-127 missing-gate.sh incident counts 0; gate.sh copied
hash-verified for mutants; oracle resolves from specs/ in scratch.)

## Invariant → proof / RED / GREEN
- I68-01 zero-open: t68HistEmptyOpen + t68IntEmptyOpen (Tests) | RED false on
  base (5f658a70) | GREEN + oracle hEmptyOpen/iEmptyOpen (328363eb)
- I68-02 no self-assent: proposerSelfApproval variant (key+proposalId) in
  validateApproval/validateBaseApproval, bar before alreadyApproved |
  t68HistSelfBar + t68IntSelfRefused (pending-shape preconditions pinned) |
  GREEN + oracle hValidBar/iSelfRefused
- I68-03 sole-admin: t68HistN1TwoStep + t68IntN1TwoStep |
  GREEN + oracle hN1TwoStep/iN1TwoStep; sole_admin_self_approval_ok proved
- I68-04 enactment sets: t68HistN2UnilateralPends + t68HistN3Killer +
  t68IntN5TwoPend + t68IntAdminChange (pends) + t68HistN2OtherEnacts +
  t68HistN3TwoOthers + t68IntN2OtherEnacts + t68IntN3TwoOthers (enacts) |
  GREEN + oracle hN2OtherEnacts/hN3/iN2OtherEnacts/iN3Killer/iN3TwoOthers/
  iN5/iAdminChange (killer pins approvals==["c"], B-counterexample held)
- I68-05 arithmetic: majority_table, majority_not_strict_on_even,
  majorityZero/One/Two — byte-meaning identical, still proved (leg 2)
- I68-06 WellFormed restated (PendingWellFormed/BasePendingCoherent
  count-indexed, Nodup kept, sole exception in predicate) + preserved by
  EVERY transition both paths (hist publics + integrated privates +
  fold inductions, all elaborate GREEN)
- I68-07 dependents: proposer_absent_above_one (axiom-free) +
  sole_admin_self_approval_ok (propext/Classical.choice/Quot.sound) =
  ruled pair; threshold-met evidence, fold theorems, wrappers re-proved;
  stale/bootstrap witnesses re-traced to validated two-step shapes;
  zero sorry/admit (grep sweep: only English "admit*" + admitMember names);
  inversion audit pin 163 held (net public-theorem delta zero)
- I68-08 witnesses executable (all #guards elaborate-true; refusal guards
  pin pending-shape first) + mutant control below; scratch removed, trees clean

## Mutant control (T68-15, scratch at candidate, full gate.sh each)
- Mutant A (Fold.lean:49 approvals:=[signer], 1-line revert): leg-2 RED —
  applyPropose hstep/change trio (empty-open-indexed proof rejects credit)
  + stalePendingWitness/bootstrapNonMember/bootstrapPending guards false
  (traces re-exhibit credit). Intended reason: historical path opens non-empty.
- Mutant B (Integration.lean:191 approvals:=[signer], 1-line revert):
  leg-2 RED — integratedPropose_both hroute2 simpa mismatch (inserted-[]
  proof rejects credit). Intended reason: production path opens non-empty.
- Both RED through leg-2 semantic executable path; tripwire (leg 4)
  unreached in both runs (proves non-spelling detection).

## Method notes (auditor rows pre-answered)
- StrongCoherent/StrongBaseCoherent auxiliaries: count-free inductive
  invariant, proved for reachable operation (empty-start fold inductions;
  sole-approve erases via threshold); no exposed antecedent weakened —
  single-step theorems carry it as an ADDITIONAL hypothesis (narrower, not
  broader). Predicate exception per data model. (NOTE-004 §3)
- requireAdmin private→public (Validate.lean): visibility only, behavior
  unchanged; needed by validation inversions. No new public theorems
  (163 pin held; all new proofs private/def).
- Corpus 7→10 steps + cover rewrite + length pin: same stories + same
  mutants (cleanup-corruption retargeted to the enact step s4); no story
  weakened (R68-06 forces two others at n=3).
- Reliance: handoffs/reliance-s1.md (7 rows, ratified or discarded by owner).
- Owed post-integration (NOTE-004): 163 re-verify + full re-verify on
  accepted master 4a6cd87 (owner-coordinated; never self-rebased).

## Integrated (NOTE-006, onto origin/master 4a6cd87)
- Rebase: 8 local commits replayed with zero conflicts; old SHAs
  (2aabe20 RED, 0e79796 GREEN) → e9db9a1 (GREEN rebased, parent 3c39014).
- Integrated gate: green-gate-rebased.log sha256 =
  26511e100e7feab4cee014b325b2d0b6ae2fbedc29a75d0f1ff0733d,
  exit 0, legs 0-5 GREEN, tripwire-absent, oracle-13 GREEN.
- 163 re-verify: lean-theorems declared=163 elaborated-backed=163
  expected=163; inversion-coverage 14/14 exact (negative control detects).
  Quota held; no S2 import; no public-theorem delta from the rebase.
- Spend: attempt 8/10. No push. No new PROOF-COMPLETE (owner freezes
  the integrated candidate for audit).
- Integrated handoff: handoffs/green-integrated.diff (+ manifest),
  verify-commit-handoff e9db9a1 vs green-integrated.diff: OK exact
  (receipt of the command output retained in STATUS journal).

## Submission 2 (repair, T68-25, NOTE-009)
- Repair commit 3ee5c12d4bb5893d25f6ff9d95f741e3eb76d1f3 (parent 9a549f2),
  2 files +295/-1, fence Invariants/Tests/comments only, message trailer
  `Tasks: T68-25`. No signature change anywhere in the repair.
- F01 corrections mapped: (1) unconditional raw structural core —
  `RawStructural` bundle (private def) + step theorems
  (`applyPropose/applyApprove/applyEvent_structural`) + fold induction
  (`foldEvents_structural`, `foldGroup_structural`, both private) +
  `approvePending_idempotent` + `applyEvent_app_lists` (all private,
  leg-2 GREEN); threshold evidence cited unchanged; (2) prefix
  admissibility PROVED executably (`t68RawTraceValidFrom` with explicit
  `validKey`/`emptyConfig`, per-step `validateEvent`-ok, guard GREEN —
  stronger than final-decision-only); (3) antecedent/domain enumeration
  in `TraceAdmissible`/`foldGroup_wellFormed`/`applyApprove` doc comments
  (needed strong vs convenient admissibility; hadm stays caller-obligated;
  integrated-internal admission distinguished); (4) prohibitions honored —
  no promotions, no hiding, no visibility change (Validate/Fold/
  Integration/Reactivegas untouched — fence-forced), no new public
  theorems (163 pin held: file-local `^theorem` count 35 = base 35),
  S2 untouched; (5) worker-owned 7-event regression family in Tests.lean
  (`t68RawTrace` prefix + `t68RawBefore`/`t68RawAfter` + 8 guards:
  count/shape, prefix-admissible, bar-refused with exact
  `proposerSelfApproval` identity, violation shape `approvals==["a"]`
  at n=3 with member present) citing auditor instrument source
  `3b4229fc` vs run `0a2799b7` each for what it is; no auditor import.
- F02 reproof (bound to repaired bytes, scratch `/tmp/reproof-68` at
  3ee5c12 RETAINED pending T.O. confirm — not removed):
  - Identities: candidate 3ee5c12, Tests blob
    40cf2635573f9e092ef9be33051bf93f37392726, oracle 9448e889 (before =
    after), gate 29e49c9d, witnesses frozen pre-execution A=db9ee87292c9
    B=ed53a6da0c9a.
  - Path A: T6 clean-module build ✓ + T7 witness GREEN control ✓ +
    exact-one-edit mutant (Fold.lean:49, file sha c6cbb818705d) + T8
    mutated-module rebuild ✓ + T9 witness RED (`wEmptyOpen`, `wN2Pends`
    FALSE intended; `wBarIntact` FALSE via pending-shape precondition by
    design — bar survival proved by module confinement: 1-line Fold-only
    diff, Validate untouched, no Fold→Validate dependency) + gate-10
    full-gate RED (95b879cd: hstep trio + stale/bootstrap witnesses,
    tripwire unreached) + restore (blob 0b05fcae match) + T10 rebuild +
    T11 restored-GREEN witness.
  - Path B: T12 clean-module build ✓ + T13 witness GREEN control ✓ +
    exact-one-edit mutant (Integration.lean:191, file sha a23b27e82650
    — byte-identical prefix to auditor spot-check mutantSha a23b27e8…,
    independent cross-confirmation) + T14 mutated-module rebuild ✓ +
    T15 witness RED (`wEmptyOpen`, `wKiller` FALSE intended) + gate-11
    full-gate RED (5463d8a7: hroute2-simpa mismatch, tripwire unreached)
    + restore (blob be41debd match) + T16 rebuild + T17 restored-GREEN.
- Full gate GREEN on repaired bytes: gate-9 sha 105a03b0 (legs 0-5,
  tripwire-absent, oracle-13 + 8 raw guards, theorems 163/163/163).
- just-ci GREEN on final bytes: gate-12 sha b1ee60d2 (toolchain, cabal
  build, format, hlint, lean).
- Repair handoff: handoffs/green-r2.diff (sha f1a6f5960011 + manifest),
  verify-commit-handoff 3ee5c12 OK exact (via 9a549f2-temp-freeze).
- Spend: full gates 9,10,11,12 used (ceiling 14; 13 submission-2 gate
  below, 14 reserve); targeted T1-T17 individually receipted
  (T1-T5 repair iterations incl. 4 reds, T6-T17 F02 campaign).
  Pre-existing logs keep stated limits (see F-02 plan §1).
