# Requirement ledger — S28-R2 FINAL

FULL accepted base `368b596fef0b6d393c2ac7afc631d236c55d86d1` → final
`ab25cd11b554bcd5ba64ca56a050c2eb21432d3c`. All six rows were reopened for
fresh judgment. Severity defaults to BLOCKING. Spend: 11/12 builds, 22/24
targeted calls; mandatory 19 completed before three remainder calls.

L3/L4/L6 and M1–M8 identify the gate's corresponding raw logs in evidence/;
other IDs identify evidence/<ID>.log. VERIFICATION-RECEIPTS.md and its JSON
bind exact commands, exits, duration, log hashes and observed resource samples.

| Row | Severity | Fresh verdict / state | Evidence and limits |
|---|---|---|---|
| R1 distinct state/event, signer, sole canonical GroupView | BLOCKING | PASS / KILLED | L4 three examples; M2 makes nonmember route accept. M1 compiler failure is coupled proposal/event; TYP-Event separately rejects DemoState where DemoEvent is required with correct proposal type. R1-run checks exact founding view, direct admission, pending role proposal, approval/enactment, hook views/payload and nonmember state/bytes against hand-built expectations. Finite values/trace, not universal type/value correctness. |
| R2 refusal before append; accepted events durable | BLOCKING | PASS / KILLED | M2 signer bypass and M8 encode-before-decision fail registered refusal controls. P2-codec independently observes accepted codec exception, exact nonmember refusal and zero tuple/whole-state conservation. P2-lock tests actual SQLite abort, unchanged tuple, exact domain refusal and successful next append. P2-conservation plus SC-negative/positive establish the tested lost-update repair and permanent checker sensitivity. No crash-atomicity or arbitrary serializer-totality claim. |
| R3 sealed atomic base hook | BLOCKING | PASS / KILLED | M3 ignores hook refusal and fails both registered witnesses. R3-run checks exact recording-hook success views/payload, refusal arguments, full aggregate/count/row restoration and real reopen; R1 additionally checks role-change hook outputs. Arbitrary hooks and failure cleanup outside these executions remain unjudged. |
| R4 direct-only admission; voted effects cannot insert | BLOCKING | PASS / KILLED | M4 adds voted admission and fails production exhaustiveness. M7 kills effect subset/deterministic absence checks. P4 proves freshly generated one-expression Fold shadow loaded; P5 detects insertion, P6 unchanged effect holds, P7 fails two of nine shipped examples. Nondegenerate present/absent targets and both mutation arms appear in permanent coverage; no exhaustive operator claim. |
| R5 integrated validation/fold/lifecycle agreement | BLOCKING | PASS / KILLED | M5 kills empty-start refusal divergence; it does not prove founding lifecycle. R5-run separately calls integrated validators, checks exact expected decisions/results/stored states at five steps, compares foldIntegratedFrom over founding to hand-built prefix states, decodes four exact accepted rows and closes/reopens the database. No historical validateEvent or same-wrapper-twice oracle substitutes for these checks. Finite lifecycle assurance. |
| R6 one integrated authority and full-log replay | BLOCKING | PASS / KILLED | M6 stale-state rewire fails live/replay and other registered witnesses. P2-conservation checks two accepted events, exact serial-order alternatives, seq_no 1/2, complete live/replay states, row/count/decoded conservation and actual close/reopen in 160 candidate schedules. SC-negative/positive establishes shipped checker sensitivity to pre-lock snapshot/decision skew. R5/L4 cover pending/voted reopen. No capability sandbox, multiple-handle concurrency or arbitrary direct SQL-write protection claimed. |

Totals: six KILLED, zero OPEN, zero BLOCKED, zero RESIDUAL. Campaign closes
at SET-POINT for this finite mandate. These are bounded evidence judgments,
not a claim that every possible mutant is killed or a ticket acceptance.

## Five reliance obligations

| Reliance | Severity | Fresh evidence | Explicit limit |
|---|---|---|---|
| HIST-FOLD | ADVISORY | L4 historical suites; complete base→FINAL diff confirms preserved historical Fold/Validate bodies, E1 field completions and E2 JSON changes. | Semantics beyond suites UNJUDGED; no residual waiver inferred. |
| CESR | BLOCKING | L4 actual key/primitive size, code, roundtrip, refusal, signature and JWK tests execute; L6 repeats actual CI. | Decoder-domain beyond suites UNJUDGED. Integrated direct admission does not invoke the historical CESR validator; arbitrary textual integrated keys gain no cryptographic assurance from these tests. |
| APPFOLD-SHAPE | ADVISORY | L3/L4 compile historical aliases/callers; unchanged alias/body review; TYP-Historical rejects the concrete incompatible event at applyEvent. | Semantics beyond compile shape and suites UNJUDGED. |
| MAJORITY | BLOCKING | MAJ-run changes current admin denominator from three to five during a pending vote: two approvals stay pending; three enact removal with exact complete state. L4 pending-entry persistence/reopen and historical majority suites execute. | Ratified denominator/pending-map claim only. No invented rule discarding former-admin approvals and no all-vote-histories theorem. |
| STORE-STM | BLOCKING | P2'' control/candidate runs, M6 and SC sensitivity; actual SQL abort/refusal/next-success; full-state persisted replay. | Finite one-store schedules at -O0. Crash/interruption between SQL commit and TVar update, concurrent separately opened handles and arbitrary external mutation UNJUDGED. |

## Control strength

P2 uses a nonwaiting codec observer and a caller-start gate entirely outside
both append implementations. The new seed body moves state/count reads and
decision before the lock while preserving the SQL/TVar effects. All 160 seed
pairs returned two successes but violated conservation/reopen; all 160 final
pairs conserved and reopened. The same eight-pair domain and 20 repetitions
are used on both sides. No codec barrier forces production evaluation order.
SC uses this identical seed body in a separately compiled Store shadow and
the exact final shipped Hspec checker, proving a semantic RED then GREEN.

R1/R3/R5/MAJ wrong-value controls are data-level oracle checks, not extra
production mutants. The named gate mutations supply the corresponding
production/type-level can-fail evidence. No setup failure was counted as a
kill. No prior report, owner gate or reconstructed old mutant was inherited
as a verdict.
