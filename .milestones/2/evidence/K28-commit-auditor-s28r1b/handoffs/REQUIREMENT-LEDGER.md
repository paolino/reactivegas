# Requirement ledger — S28-R1 / A-01

Base `368b596fef0b6d393c2ac7afc631d236c55d86d1` → candidate
`3af3d065b7d0c54f03d89b8c05d8b8acd4a53db4`.
Six requirement rows; default severity BLOCKING. This run spent 10/12 builds
and 16/24 targeted calls (the complete ≤16 probe allocation). Every result
below is based on fresh execution against this candidate, not owner acceptance.

`L4`, `L6`, and `M1`–`M7` name the corresponding logs with prefix
`evidence/20260906T001909Z-3af3d06-`. Probe IDs name `evidence/<ID>.log`.
Full hashes and exact commands are in VERIFICATION-RECEIPTS.md and
evidence/verification-receipts.json. Scope limits below remain part of each
judgment; a KILLED row is not exhaustive mutation or universal verification.

| Row | Severity | Carried state | Current verdict / state | Fresh evidence and limit |
|---|---|---|---|---|
| R1 distinct state/event, signer, sole canonical GroupView | BLOCKING | OPEN | PASS / KILLED | L4's three examples; M2 rejects bypassed signer guard; TypeNegative isolates DemoState-as-DemoEvent with correct proposal type (both valid types compile in P1/R1); R1-run checks hand-built founding, direct admission, pending proposal, role enactment, exact hook view/payload tuples and nonmember state/bytes. M1 remains coupled and is not used alone for event separation. Finite trace, not a proof over every value. |
| R2 refusal before append; accepted events durable | BLOCKING | BLOCKED, F1 | FINDINGS / BLOCKED, F3 | L4's four examples and M2 kill; P2 closes the observed eight-pair lost-update class, plus sequential, real SQL-error and domain-refusal controls. F3 observes a serializer exception replacing the exact nonmember refusal. No state/log change in that failure, but refusal semantics are not preserved. |
| R3 sealed atomic base hook | BLOCKING | OPEN | PASS / KILLED | M3 fails both hook-refusal examples. R3-run records exact success-side pre/post views and changed counter/log output, reopens it, records exact refusal-side hook arguments in the error, and checks unchanged aggregate/zero rows/zero count/reopen. R1 also covers role-change hook arguments. No assertion of total correctness for arbitrary application hooks or process crashes. |
| R4 direct-only admission; voted effects cannot insert | BLOCKING | BLOCKED, F2 | PASS / KILLED | M4 fails production exhaustiveness. M7 and P7 now fail the real subset property and deterministic absent-target effect check; P4 proves the exact shadow Fold loaded; P5 catches its insertion, P6 passes the original effect. Present and absent keys occur in the strengthened permanent property. This settles F2's observed vacuity, not complete mutation of all effects. |
| R5 integrated validation/fold/lifecycle agreement | BLOCKING | OPEN | PASS / KILLED | M5 kills the existing empty-start refusal regression check; its limit is explicit. R5-run separately compares each real integrated-validator decision, returned result, stored state and founding replay against hand-built expected states for admission/proposal/duplicate refusal/approval-enactment/app addition. Four exact accepted rows persist and real reopen equals the expected final aggregate. This is independent finite-case lifecycle assurance, not the shipped same-wrapper comparison relabeled as independent. |
| R6 one integrated authority and complete-log replay | BLOCKING | BLOCKED, F1 | PASS / KILLED | M6's stale-state rewire fails the named live/replay check and other witnesses. P2 checks complete ordered decoded events, sequence numbers, event count and entire live/replay state for eight overlapping pairs. R5 adds pending/voted/app persisted replay; L4 pending-entry reopen passes. TypeNegative separately rejects DemoEvent at historical applyEvent's DemoState boundary. No claim of an OS/capability sandbox or protection against arbitrary direct SQL writes through the pre-existing exported connection. |

Current requirement totals: KILLED 5, BLOCKED 1, OPEN 0, RESIDUAL 0.
Assigned gate mutants: seven intended failures, seven byte-exact restorations.
The bounded audit campaign terminates at its row set-point with R2 blocked by
F3; this is a findings result, not ticket closure or acceptance. Prior OPEN
rows were investigated and given bounded evidence, never converted to residuals.

## Reliances and explicit limits

| Reliance | Severity | Current evidence | Remaining limit |
|---|---|---|---|
| HISTORICAL-FOLD | ADVISORY | Full accepted-base diff reviewed; historical Fold/Validate bodies preserved, new state-field completions and JSON changes within E1/E2; L4 and L6 historical suites pass. | Semantic equivalence beyond those suites is UNJUDGED. No residual acceptance inferred. |
| CESR-KEY-VALIDITY | BLOCKING, corrected declaration | L4/L6 execute key-code/type/roundtrip/refusal suites. Integrated direct validation keeps the declared admin/reserved/existing-member guards; no claim it calls the historical CESR validator. | Decoder domain beyond the suites is UNJUDGED; arbitrary integrated member-key text is outside a new cryptographic assurance claim. |
| STORE-STM-DISCIPLINE | BLOCKING | P2's eight controlled overlaps conserve full state, committed rows and lengths; real SQL abort leaves values unchanged and a subsequent append demonstrates lock release. M6 remains a state-staleness witness only. | F3 separately blocks refusal preservation. Crash/interruption between committed SQL insert and TVar update, multiple independently opened handles, and exhaustive schedules are UNJUDGED. |
| MAJORITY-FRANCHISE | BLOCKING | MAJ-run changes current admin count 3→5 mid-vote; two votes stay pending, third commits removal and exact resulting state. L4 pending-entry persistence/reopen passes; existing count/majority suites execute. | The ratified claim concerns current majority denominator and pending map. No added rule discarding former-admin approvals is inferred; all vote-history/payload possibilities are not proved. |
| HISTORICAL-APPFOLD-SHAPE | ADVISORY | Alias/role-predicate definitions unchanged in diff; actual historical caller graph compiles in L3/L4/L6. TypeNegative additionally establishes the specific demo event/state incompatibility. | Historical semantics beyond compile shape and executed suites remain UNJUDGED. |

Instrument controls: P2 begins with a seeded inconsistent tuple and sequential
positive control; codec failure is caught and is deliberately activated with
an accepted member before comparing a nonmember to the pure boundary. P5/P6
are executable negative/positive effect controls. R1/R3/R5/MAJ include
data-level wrong-output/threshold controls before evaluating the candidate;
these are not claimed as additional production mutations. Mandatory gate
mutants remain the named production/type-level can-fail evidence.

No unrelated discovery or new product invariant is ratified. F1/F2 are
resolved for their observed classes; F3 remains BLOCKING. There is no
second implementation submission or repair automatically authorized here.
