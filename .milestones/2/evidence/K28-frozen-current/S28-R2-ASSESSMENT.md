# S28-R2 TICKET-OWNER ASSESSMENT (independent; for epic assessment)

Ticket owner `t28-app-api`. Authority: NOTE-045 (desk final-pass via epic).
Method: every load-bearing hash recomputed here; gate log, all eight kills,
row exemplar logs, ledger, terminal lines, lane state read on primary
sources; auditor's limit-honesty sampled (limits appear inside the evidence
logs themselves); nothing inherited as verdict. Candidate
`ab25cd11b554bcd5ba64ca56a050c2eb21432d3c`, tree `e52114c1…`, branch
`fix/28-r1-conservation-effect`→`fix/28-r2-refusal-order`, HEAD clean.
Gate v10.2 (`c00b88a2…`/`12f392b6…`, both files verified). Terminal
`d1d19060…` + inventory `3f352562…` verified; campaign 6-killed/0-open/
11/12+22/24.

## Row verdicts (mine)

- R1 PASS/KILLED: leg-4 row-1 (3) + M1/M2 kills + TYP-Event isolation +
  R1-run exact views/payloads (founding, admission, proposal, approval,
  hook views, nonmember state/bytes — read). Limit: finite values/trace.
- R2 PASS/KILLED: leg-4 row-2 (8: sequential + F1-regression + overlap +
  3 faulting + refusal) + M2/M8 kills (M8 digest `37be8bccc9…` triple-
  confirmed across isolated/owner/auditor executions) + P2-codec (accepted-
  throw + exact-Left + zero tuples, read) + P2-lock (trigger abort +
  domain refusal + next-success) + P2-conservation/SC (below). Limit: no
  crash-atomicity, no serializer-totality.
- R3 PASS/KILLED: leg-4 row-3 (3) + M3 kill + R3-run exact hook
  success/error outputs + restoration + reopen (read). Limit: arbitrary
  hooks/cleanup beyond executions unjudged.
- R4 PASS/KILLED: leg-4 row-4 (9) + M4/M7 kills (M7 quotes subset +
  absent-check) + P4/P5 (insert observed)/P6 (holds)/P7 (9 examples,
  2 failures — read). Limit: no exhaustive-operator claim.
- R5 PASS/KILLED: leg-4 row-5 (3) + M5 kill + R5-run integrated validators
  + exact per-prefix results/states + founding replay + 4 rows + reopen
  (read). No historical-validateEvent, no same-wrapper-twice. Limit:
  finite lifecycle.
- R6 PASS/KILLED: leg-4 row-6 (5) + M6 kill + P2-conservation (160
  schedules conserve + reopen; seed-side 160 fail) + SC pair. Limit: no
  multi-handle/cancel-crash/sandbox claims.

## Reliances (evidence + limits as ledgered)

HIST-FOLD (suites + preserved bodies; beyond-suites UNJUDGED), CESR (key
tests incl. JWK; decoder-domain UNJUDGED; direct admission never invokes
historical validator), APPFOLD-SHAPE (compile + TYP-Historical rejection;
semantics UNJUDGED), MAJORITY (3→5 denominator change + enactment exact,
read; pending/reopen + historical suites; no invented rules), STORE-STM
(P2''/M6/SC + abort/refusal/replay; finite one-store -O0; crash-between-
commit-and-TVar/handles/external-mutation UNJUDGED).

## Witnesses + preservations (explicit)

- P2/SC distinction: P2 = committed-suite probes (codec/conservation/lock
  classes); SC = shipped-checker falsification (identical seed body in
  Store shadow: RED 958/558 then GREEN 1/0 on the exact final checker).
  Different instruments, different obligations, both executed.
- Finite scheduling bounds: shown = 8 pairs × 20 seeds fail + 160
  candidate schedules conserve + reopen + shipped checker RED/GREEN;
  NOT shown = every interleaving, optimized code, crash recovery,
  separately opened handles, cancellation windows.
- Refusal-versus-exception: exact-Left preserved (nonmember, zero tuple)
  vs post-acceptance throw (member, zero tuple) vs mutant error-replacing-
  Left (kill quotes the refused control; tuple assertions unreached, never
  observed-unequal). Three distinct outcomes, never conflated.
- Historical/cleanup/unjudged: STM-thread-blocked message preserved-
  unattributed (present in accepted-base log too); shipped-test cleanup
  completes on observed positive + semantic-negative; timeout/thrown/
  async/kill-live/closeKEL-throw/double-failure/setup-exit paths NOT
  established (done-MVar/kill-receipt ≠ death-ack); decoder/historical/
  acquisition-leak/resource domains UNJUDGED or source-reviewed only.
- Separate costs (never mixed): S28-1 owner 34/34 + audit 9/12+7/24 (one
  spent submission); S28-R1 owner 13/16 + audit 10/12+16/24 (one spent)
  + invalid admission 0/0; S28-R2 owner 26/26 + 4/24 + 2/4 probes (one
  spent: RED0 + GREEN11 + isolated1 + gate11 + SLIM3 = 26 EXACT) + audit
  11/12+22/24 (floor-19 before remainder-3).

## Claim NOTHING beyond

No arbitrary crash safety; no crypto-authenticated integrated placeholder
events; no #29/#73 completion; no release readiness; no remote CI yet;
no merge (not granted). Finite-scope local evidence only.

## Verdict

Zero blockers remaining from this review. Every original row KILLED with
bounded executed evidence; every reliance evidenced-or-explicitly-
UNJUDGED (never residual-accepted); all preconditions met; books coherent
to exact fits; lane frozen clean at the assessed bytes. RECOMMEND
ACCEPTANCE of EXACT `ab25cd11b554bcd5ba64ca56a050c2eb21432d3c` to the epic
for step-1 assessment (local acceptance decision + conditional push/PR/CI
per NOTE-045 step 3 belong to the joint ruling, not this handoff).
