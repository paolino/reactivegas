# Commit audit — AUDIT-PASS, S28-R2 FINAL

- Worker: commit-auditor-s28r2; marker AUDIT-S28R2.
- Scope: FULL accepted base `368b596fef0b6d393c2ac7afc631d236c55d86d1` →
  candidate `ab25cd11b554bcd5ba64ca56a050c2eb21432d3c`.
- Tree: `e52114c1f7a676073303ff76caa8f22821e0b2a3`.
- Mandate: S28-R2-COMMAND-PLAN,
  `f97e0c55bb0462cbf1698486341e142dfd5fe5e3b94bab7020b76032e9952e74`,
  NOTE-033/040/043 and the full final brief.
- Gate: G28-1 v10.2 (S28R2-M8faithful), full
  `c00b88a29989b11d09696d7afa164f7d9f93b59aee661a1b88a120c7a4934b75`,
  normalized `12f392b6fe691230269a70bf9588fa4c25f71330639d0b6a464ceb8c532d67b0`.
- Verdict: **AUDIT-PASS for the declared finite scope.** No blocking finding.
  This is independent local audit evidence; the ticket owner owns acceptance.
- Campaign: SET-POINT, six KILLED rows, no OPEN/BLOCKED/RESIDUAL rows.
- Spend: **11/12 builds and 22/24 targeted calls**. Mandatory 19 completed
  before three relevant remainder calls. No setup failure, retry or overrun.

## Findings and requirement judgment

All R1–R6 and all five reliances were reassessed against FINAL bytes.
[REQUIREMENT-LEDGER.md](REQUIREMENT-LEDGER.md) gives each requirement's
observable meaning, severity, controls, fresh judgment and limits. No prior
kill was carried forward as acceptance; no uncovered row was made residual.

F3 is resolved for the faulting-codec refusal class. P2-codec confirms the
accepted-member codec exception is observable, while the same codec from
an outsider returns exactly `Left (IEValidation (NotAMember "outsider"))`.
Both leave the measured `(hot, rows, length, replay, decoded)` tuple at
`(0,0,0,0,0)` and the full aggregate unchanged. M8's faithful reorder
replaces the refusal with the seeded exception and fails both registered
refusal checks. The tuple assertions are not reached under that mutant;
this kill proves exception-replaces-required-refusal, not observed unequal
post-failure tuples. M8 retained diff:
`37be8bccc9f50e48275cca01be21d9aec686d404a205092b711acc279a8274c1`,
identical to the bound owner control.

F1 is resolved for the independently tested concurrency class. A new
compiled instrument uses a codec that only records observations and returns
ordinary DemoEvent JSON. A shared start gate precedes the public calls;
there is no codec wait/barrier/delay or production-order workaround.
For each of eight specified value pairs, twenty executions on the explicit
pre-lock snapshot/decision seed first demonstrate a real lost update; the
same 160 schedules on the final candidate conserve both successful events,
full live/replay state, exact SQL row/count/decoded counts, sequence numbers
1/2 and one of the two submitted orders. Each database is closed and reopened;
all final states agree. Every seed execution reports two accepted calls but
fails conservation/reopening; every candidate execution passes. This is
finite schedule evidence at -O0, not a claim about every interleaving.

The final shipped concurrency checker was also freshly falsified, rather
than relying on the owner's reconstructed S1 mutant provenance. SC-compile
loads a retained Store shadow with the same seed body. SC-negative runs
exactly the final `concurrent appends conserve every committed transition`
example and fails on counter **958 expected / 558 observed**, one example,
one failure, without SETUP/timeout. SC-positive runs the same selected
checker on the unchanged candidate: one example, zero failures. Both
terminate promptly. These three calls use the remainder only after the floor.

F2 is resolved for the effect-vacuity class. P4 compiles the fresh exact
one-expression Fold shadow (`Map.adjust` → `Map.insert`); its log names the
shadow path. P5 observes the absent key inserted, P6 observes no insertion
on the candidate, and P7 makes two of the nine final shipped direct-only
examples fail: the real subset property and deterministic absent-target
check. M7 independently kills those final checks in the full gate.

R1 and R3 use exact hand-built member relations and distinguishable payload
values (initial counter 17, recording-hook increment 23), including direct
admission and role-change views, hook success/error outputs, restoration
and persisted reopening. R5 independently compares integrated validators,
returned results and stored/founding-replayed states with hand-built states
through admission, propose, duplicate refusal, approve/enact and app add;
four exact accepted rows persist and real reopening matches. MAJ checks
current franchise growth from three to five: two votes remain pending,
the third enacts removal and the expected complete state. TYP-Event keeps
the proposal type correct and rejects DemoState as DemoEvent; the separate
TYP-Historical command rejects DemoEvent at the historical DemoState boundary.

## Gate, provenance and attribution

The unchanged `./gate.sh` independently returns **OVERALL_FAIL=0**. L4
executes all 31 S28 examples (3/8/3/9/3/5), none missing or pending; the
suites report 135 kelgroups and 91 keri-hs examples, zero failures. L6 runs
the actual `.github/workflows/ci.yml` command,
`nix develop .#ci --quiet -c just ci`, including formatting, cabal formatting,
HLint, Haskell build/tests, Lean build (17 jobs), client build/tests.
The eight inventory string hits are not eight requirement groups.

| Control | Fresh observed failure |
|---|---|
| M1 | Compiler: coupled DemoProposal/DemoEvent vs DemoState/DemoState mismatch at applyIntegratedEvent. Isolated event evidence is TYP-Event. |
| M2 | Registered nonmember-refusal checks accept after signer gate bypass. |
| M3 | Both registered hook-refusal checks accept after hook suppression. |
| M4 | Production validateBaseMutation is non-exhaustive for AdmitMemberVoted. |
| M5 | Registered empty-start prefix check raises MUTANT-M5 on refusal. Independent founding/lifecycle evidence is R5-run. |
| M6 | Registered live/replay check and five other examples fail after stale-state write. |
| M7 | Real voted-effect property and deterministic absent-target check detect insertion. |
| M8 | Registered faulting-codec refusal checks throw after encode-before-decision reorder. |

The owner COMPLETE full-hash prefix a7ca9dc4… belongs to the older v10
freeze, as its preserved correction records; it is not v10.2 identity.
Actual v10.2 full and normalized hashes are bound separately above.

Every gate mutant has retained exact diff/source bytes and a byte-exact
restoration receipt. Final HEAD/tree/gate remain unchanged and porcelain is
empty. The complete base→FINAL diff is 15 files, +1845/−13 across nine
linear commits; the S28-R2 repair delta is exactly Store.hs and
S28AppApiSpec.hs. Historical Fold/Validate bodies, Trivial, client/Lean
source and workflow wiring are preserved; E1 record-field completions and
E2 JSON compatibility changes were reviewed explicitly. Publication and
commit-signature verification are not claimed.

## Failure modes and assurance limits

- Serialization now follows authoritative in-lock validation. P2-codec
  independently re-proves the preserved refusal and accepted exception.
- Synchronization spans fresh state/decision, encoding, SQL insert and TVar
  commit. P2 and SC prove the specified finite loss-detection/conservation
  cases. No conclusion covers separately opened concurrent handles or
  cancellation/crash between committed SQL insertion and TVar update.
- Real SQLite trigger abort reaches the caller without state/count advance;
  exact domain refusal also conserves state. A subsequent successful append
  demonstrates lock release (P2-lock).
- Hook refusal and persisted replay are executed with exact independent
  values. Corrupt/mismatched founding and filtered undecodable-row behavior
  are source-reviewed, not exhaustively fault-tested; acquisition-failure
  cleanup and resource-leak assurance remain UNJUDGED.
- Production adds no background worker. The shipped test's threaded cleanup
  completes on the observed positive and semantic-negative exits. Timeout,
  thrown-worker-exception, async cancellation, kill-live-worker receipt,
  and closeKEL-throw/double-failure paths are not established by these runs.
  A done-MVar or killThread receipt is not a worker-death acknowledgment.
- P2's observation hook uses test-only unsafePerformIO; its claims are bound
  to the compiled -O0 instrument and measured schedules. No optimized-code
  or universal scheduling assertion follows. Setup-exit cleanup was not
  exercised in this audit's harness either.
- HIST-FOLD semantics beyond suites, CESR decoder domain beyond executed
  tests, and historical APPFOLD semantics beyond compile shape remain
  UNJUDGED as ledgered. The MAJORITY claim concerns the current denominator
  and pending map, not an invented former-voter-disqualification rule.
- Raw historical suites again print `thread blocked indefinitely in an STM
  transaction` alongside passing tests. The same message is present in the
  retained accepted-base log; it is preserved, not attributed to this repair
  or erased by the passing exit code.

These named limits are not accepted residual requirements. No arbitrary
application-hook/codec totality, cryptographic authenticity of unsigned
integrated placeholder rows, remote CI, release readiness, epic #29/#73
completion or ticket acceptance is claimed.

## Authority, accounting and retained evidence

Fresh live Codex gpt-6-astra/high PID 3019780, pane %569, was independently
bound to reactivegas:12 (kelgroups, @157), distinct from ticket owner %534
and Muse author %545. The unqualified tmux query returned ambient client
focus :4; the process-bound pane query establishes the actual :12 seat.
The brief, mandate, gate, owner evidence and all three terminal records
were read; prior report hashes match b7b793a3…, 24252ef1… and 22c79c04….
No predecessor pane, context, process, binary or ledger was reused.

Retained Trace/Row4/runner sources informed new local instruments; their
provenance and amended source hashes are explicit. StoreProbe-v2 adds
actual close/reopen before its first compile, preserving the predecessor
source and pre-amendment command list. The complete nineteen-command fit
was acknowledged before the gate; exact executed argv are in the receipts.
M1/M4 compiler failures and semantic test failures remain distinct.

Historical ledgers remain separate: S28-1 owner 34/34, audit 9/12+7/24;
S28-R1 owner 13/16, audit 10/12+16/24, failed admission 0/0. S28-R2 owner
26/26+4/24 with diagnostic 2/4 is retained; NOTE-040 explicitly grants its
14→26 ceiling change for the gate-scope repair. This fresh audit uses
11/12+22/24, with no auditor ceiling increase. No automatic next submission,
raise, repair or merge is authorized here.

[VERIFICATION-RECEIPTS.md](VERIFICATION-RECEIPTS.md) binds all executions;
[EVIDENCE-INVENTORY.md](EVIDENCE-INVENTORY.md) and JSON index the retained
source, exact mutant diffs, command logs, authority snapshots and receipts.
Phase durations are observer intervals, not CPU time; filesystem free-space
samples are host observations, not attributable per-process allocations.
Only reproducible build outputs under this audit's handoffs/build are retired
after report freeze; measured retirement receipts are retained. The detached
worktree remains under ticket-owner control. No candidate repair, author
contact, remote write, push, PR, merge or memory update was performed.

Blocking findings: none. Advisories: none newly issued. Candidate invariants:
none. Onward discoveries: none opened. The ticket owner owns acceptance;
this auditor returns one terminal report and stops.
