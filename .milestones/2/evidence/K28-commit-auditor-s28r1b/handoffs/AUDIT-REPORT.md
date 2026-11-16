# Commit audit — AUDIT-FINDINGS

- Submission: S28-R1, one commissioned submission; worker commit-auditor-s28r1b.
- Scope: FULL accepted base → candidate, including inherited S28-1 changes.
- Base: `368b596fef0b6d393c2ac7afc631d236c55d86d1`.
- Candidate: `3af3d065b7d0c54f03d89b8c05d8b8acd4a53db4`.
- Tree: `b4eb37f2187c39db4950072e309c0d125d5c27af`.
- Mandate: plan v2 `eff38e3bca5022a0bcacdbc37deec44766917c755d2ac68fc96c7fdcfdc3c9e0`,
  with NOTE-024/025/026/028/029 and A-01/NOTE-030. Amended brief:
  `6f4fcd08370b81a57e4856cda6b6775c7b133608e05efc6ad1cdc0dbe6806c07`.
- Gate: G28-1 v9 (S28R1-plan), full
  `dcbc8c2b8eefa111b5b71873be8d87fa95de2369642e6224417f9544e5a8e815`, normalized
  `3c433effb967052aa91aef2302268c05ab27b3d0f3e54c979504d6978611d340`.
- Verdict: **AUDIT-FINDINGS — one BLOCKING finding, F3.** The observed F1
  concurrency and F2 vacuous-effect-test defects are resolved for the tested
  classes. No acceptance or merge authority is conferred.
- Execution: **10/12 builds and 16/24 targeted calls**; every mandatory
  command executed, plus one isolated type-negative compilation. No failed
  setup call, discretionary build, extra stress run or overrun.
- Campaign: row set-point reached with five KILLED rows and one BLOCKED row;
  zero OPEN requirement rows and zero residual waivers. R2 remains blocked by F3.

## F3 — eager serialization replaces a required nonmember refusal with an exception

**BLOCKING: R2 / preservation of refusal and failure behavior.**
Candidate `lib/KelGroups/Store.hs:618–627`; specifically the forced payload
at line 623 before the shared validator at line 626.

The repair moves encoding and strict Text evaluation ahead of the member
decision. In the previous implementation, a refused event returned directly
without demanding its JSON encoding. The current pure integrated boundary
still returns `Left (IEValidation (NotAMember "outsider"))` independently of
the application payload's codec; the durable wrapper can instead throw from
that codec before reaching the refusal. This contradicts preserved refusal
behavior and the wrapper's own documented validate-first ordering.

The compiled StoreProbe injects a deliberately faulting ToJSON instance.
An accepted-member call confirms an injected exception is observable. The
same event from a nonmember has the expected pure-boundary refusal, but
`appendIntegratedEvent` produces a **caught** `AUDIT-SEED-SERIALIZATION`
exception. The probe prints `preserves-refusal=False` and exits 1 from its
final assertion. It does not crash during setup. The measured state/count
tuple is `(0,0,0,0,0)`: this finding is changed refusal behavior, not an
observed unauthorized append or lost state.

**Property class:** a validation refusal independent of the application
payload must remain the same caller-visible refusal when processing that
payload would fail. Moving work ahead of validation must not let its
exception replace a decision that previously short-circuited that work.

**Limits:** this is controlled fault injection into the generic application's
serializer. It does not claim that the demo's ordinary JSON codec fails,
that every application exposes this condition, or that an external exploit
was demonstrated. No production codec or candidate file was altered by P2.
The generic boundary and the explicit failure-preservation duty supply the
scope; no new product invariant is being proposed as an acceptance rule.

Evidence: `evidence/P2.log`, SHA256
`7e9bdb49092e3d9147765ce16d7e35ea50f2ffef0e520ead2943c7d39dfc73a3`;
frozen `StoreProbe.hs`, SHA256
`6775a8cc0caf0ac26cf5a16e39a303ea1961928d4da0953b22de6bfe046de3be`.
The full repair diff is retained as `evidence/repair.diff`. No repair is
prescribed or authored by this audit.

## Repaired findings and requirement coverage

F1: all eight controlled concurrent pairs passed: inherited (1,2), (3,7),
(11,19), (101,307), plus (5,11), (42,43), (1000,7), (0,999). Each pair
returned two successes, persisted exactly the two submitted events in one
observed serial order with sequence numbers 1/2, maintained length=2, and
gave identical complete live/replay states. The probe also passed its
seeded lost-update control, sequential control, real SQLite trigger abort,
domain refusal and subsequent successful append proving lock release.
P2's overall RED comes solely from F3 after those checks, not a recurrence
of F1. This is an eight-pair single-store schedule result, not exhaustive
concurrency, optimized-code or crash-recovery assurance.

F2: the exact one-expression shadow `Map.adjust`→`Map.insert` compiled
against the repaired source. P5 observed absent-target insertion by the
shadow; P6 observed non-insertion by the candidate. P7 now fails the real
subset property and deterministic absent-target check: **9 examples,
2 failures**, whereas the prior six-example group survived this class.
The full gate's new M7 also kills both checks. Present/absent target values
and both mutation arms occur in the permanent strengthened property.

R1: explicit founding aggregate, direct admission, pending role proposal,
approval/enactment and nonmember refusal match independently specified
states/views/payloads. The extra TypeNegative command isolates the event
parameter (correct proposal type) and rejects a DemoState where DemoEvent
is required; the same invocation separately rejects DemoEvent at the
historical `applyEvent`/DemoState boundary. This closes the concrete type
coverage gap left by M1's coupled proposal/event mismatch.

R3: a recording hook exposes exact pre/post views and a distinguishable
counter/log payload. Success survives reopening; refusal returns its exact
hook-argument record and preserves the initial state, zero count, zero
rows and reopened state. Data-level wrong-output controls precede the
candidate checks; they are not counted as extra production mutants.

R5: the hand-computed lifecycle checks the integrated validators directly,
then exact returned results and stored states at every prefix: admission,
proposal, duplicate approval refusal, approval/enactment and application
addition. The founding replay matches each expected prefix; exactly four
accepted events persist and reopening reproduces the expected final state.
The oracle's states do not come from another call to the shared wrapper.

MAJORITY: increasing the admin population from three to five during a
pending vote raises the required count; two votes remain pending and the
third enacts the expected removal and exact resulting state. Existing
pending-entry reopen and historical majority suites also execute.

The per-row PASS/FINDINGS states, mutant bindings, reliance judgments and
honest limits are in [REQUIREMENT-LEDGER.md](REQUIREMENT-LEDGER.md). Named
limits are not acceptance of OPEN rows as residuals. In particular,
historical semantics beyond suites, CESR decoder-domain completeness,
arbitrary vote histories and all possible schedules remain unproved.

## Gate and verification

The independent gate returned **OVERALL_FAIL=0**. Leg 4 executed all
27 registered S28 examples (3/4/3/9/3/5), with no missing or pending
examples; kelgroups reports **131 examples, 0 failures**, keri-hs **91,
0 failures**. The gate's eight source-string inventory hits are not eight
requirement groups. Full local CI passed formatting, cabal formatting,
HLint, build/tests, Lean build and client build/tests. The actual
`.github/workflows/ci.yml` invokes exactly
`nix develop .#ci --quiet -c just ci`, matching leg 6. No remote CI result
or release-readiness claim is made.

Every gate kill was independently observed and its exact diff captured:

| Mutant | Actual failure attribution |
|---|---|
| M1 | Compiler: expected `IntegratedEvent DemoProposal DemoEvent`, actual `IntegratedEvent DemoState DemoState`, at `applyIntegratedEvent`. Coupled; isolated event proof is TypeNegative. |
| M2 | `nonmember append persists nothing byte-identical` returns Right after membership bypass. |
| M3 | `hook refusal rejects the whole transition` and `failing hook restores prestate plus prelog` return Right with the hook ignored. |
| M4 | Production `validateBaseMutation`: non-exhaustive match, missing `AdmitMemberVoted _ _ _`. |
| M5 | `prefix folds match steps over mixed traces` raises the intended `MUTANT-M5` from the refusal arm. This is the empty-start check, not independent founding-lifecycle evidence. |
| M6 | `replayed log reproduces live state exactly` mismatches live counter 0 against replay 3; four other witnesses also fail. This proves state staleness detection, not every authority property. |
| M7 | `voted mutations never insert members` is falsified by an absent target; deterministic absent-target check also fails with an added key. |

All seven mutants were restored byte-exactly, and the final candidate was
tracked-clean with unchanged HEAD/tree/gate. Build receipts measure observed
phase intervals rather than exact child CPU time. Targeted receipts record
exact argv, real exit, duration, hashes and before/after free-space samples.
P5/P7 and TypeNegative are intended negative results; P2 is the new finding.
No import, parse, linker or unrelated failure is counted as a kill.

See [VERIFICATION-RECEIPTS.md](VERIFICATION-RECEIPTS.md) and
`evidence/verification-receipts.json` for every execution. The package
configuration snapshot binds probe imports/library directories to this
audit worktree's built candidate; P4's log names the shadow Fold source.

## Provenance, failure modes and limits

The complete base→candidate diff is 15 files, +1589/−13; the repair delta
from 84a2dae is exactly Store.hs plus S28AppApiSpec.hs, +111/−29. All three
local commits in the range were inspected. E1 is limited to the declared
empty pendingBase field completions; E2 is JSON-only. Historical Fold and
Validate bodies, Trivial behavior, client/Lean source and automation wiring
are unchanged. This establishes the local provenance/fence, not commit
publication or signature verification.

- Synchronization: a per-store MVar now spans fresh state/length reads,
  SQL insertion and the TVar writes. P2 demonstrates the repaired overlap
  and post-SQL-failure lock release. Historical append logic is unchanged.
- Serialization: evaluation moves ahead of validation; F3 demonstrates
  the resulting caller-visible exception/refusal change.
- SQL failure: an actual aborting trigger reaches the caller, does not
  advance state/count and does not prevent a subsequent successful append.
- Hook refusal/replay: actual persisted and reopened checks pass, including
  the hand-computed pending/enacted lifecycle. The store's existing choice
  to filter undecodable rows was reviewed, not exhaustively fault-tested.
- Resource acquisition: opening/initializing SQLite and generating the
  filler key remain synchronous; explicit founding-corruption/mismatch
  branches are visible in source. Failure cleanup/resource-leak coverage
  was not independently established. No new background worker was added.
- Crash/interruption after a committed SQL insert but before TVar update,
  concurrent independently opened handles and arbitrary faulty application
  hooks are not adjudicated by these probes.
- JSON compatibility: the suite executes nonempty pendingBase roundtrip,
  missing-field compatibility and malformed-field refusal; R5 additionally
  checks exact integrated accepted-row decoding and real reopening. Full
  backward/forward codec-domain equivalence is not claimed.

Raw historical test output includes `thread blocked indefinitely in an STM
transaction` alongside passing suites. This was also recorded in previous
base evidence; this audit does not assign it to the repair or silently
interpret passing exit status as proof no such background exception occurred.
No unrelated issue is opened and no new product invariant is ratified.

## Authority, accounting and evidence disposition

Live verification admitted fresh Codex gpt-6-astra/high PID 2708047 in pane
%567, reactivegas:11, distinct from ticket owner %534 and Muse author %545.
The untargeted tmux query follows client focus and reported :15; the
process-bound `$TMUX_PANE` query and pane list establish co-location in :11.
The old %566 context/root was not reused. Both prior terminal reports were
read as historical evidence, never inherited as verdicts.

A-01 arrived post-launch/pre-START and explicitly corrected founding and
replay witness wording. It resolved the locally drafted Q-001 before any
terminal event. The pre-amendment draft report/ledger are retained with
`DRAFT-PRE-A01` names and are **not terminal reports**. The current report
is the single terminal verdict. START acknowledges A-01 and the exact
command-fit plan before gate execution. No provider issue occurred.

History remains separate: S28-1 owner 34/34, audit 9/12+7/24; S28-R1
owner 13/16 (including its separately recorded premature SLIM timing),
prior failed-admission auditor 0/0, replacement audit **10/12+16/24**.
Cumulative actually spent independent-audit calls across these records:
19 substantive and 23 targeted. No ledger was zeroed/refunded; no ceiling
raise or further submission is authorized by this report.

Runtime source, receipts, raw logs, mutant diffs, snapshots, ledger and report
are retained and hash-bound. Only this audit's reproducible build outputs
under handoffs/build are retired after report freeze; measured receipts
record the reclamation. The detached worktree and its build outputs remain
for ticket-owner disposition. No candidate repair, author contact, external
message, push, PR, merge, remote write or memory update was performed.
The ticket owner owns disposition of F3 and acceptance; this auditor stops.
