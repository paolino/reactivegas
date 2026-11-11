# Commit Audit

- Submission: `1/2`
- Base: `bf027aabb764a006604ad5e88b4780c42d264011`
- RED: `ab0fcc151f5489cdeb57dbaedf7f9cc0fe8bbf31`
- Candidate: `b90161cffb478db0855e81e0bc3ab23818bba161` (tree `1353a3b031e65421169f59ddd4f4094c55e4ef45`)
- Diff SHA-256: `151906f49fcc4447fa91dbb3be98ae5ff9c328307b664821c56adb1b50c7e423` (`--full-index --binary`)
- Mandate: `9a226235b117b77858ca8f99f8fe442cd6f21d8d86d64898a8ad0cb5c6c1c078`
- Scope: FULL `base..candidate` plus the three A011 open rows
- Verdict: `FINDINGS` (`2` blocking findings)
- Audit loop: submission `1/2`; next submission `ALLOWED` by the outer cap, but this auditor requests none
- Ceiling raises: `0/2`; source ledger `/tmp/reactivegas/ms2/e43/t62-owner-codex/ceiling-ledger-s62-c-a011.md` SHA-256 `1e03836cd41c1442b71648b34b38f017e4100f82e593e208e80b38675a7c0155`
- Campaign: `CLOSED` — ended by `SET-POINT`; killed `5`, residual `0`, blocked `2`, open `0`
- Campaign ledger: `campaign-ledger.md` SHA-256 `53e5bee359d31919a4c9f3ee343548bdfb82cf2b57935d12c63db3f3fa292896`
- Builds: `15/40` this ticket; this audit `3/3`, cache=`cold,warm,warm`
- Remote/draft: `NONE` / `NONE`
- Report SHA-256: bound externally in `report.md.sha256` and the terminal `AUDIT-FINDINGS` event

Provenance and scope pass. The worktree is clean and detached at the exact
candidate/tree; the chain is exactly base → RED → GREEN; both frozen handoffs
revalidate; no known remote ref contains the candidate. The delta is exactly
the nine allowed regular files, `901/130` lines (1,031 total), with no forbidden
spec, task, gate, Nix, workflow, Haskell, dependency, or remote delta. Fresh
postflight evidence: `evidence/postflight.log` SHA-256
`0405de7c000dc0fc19f98602a91c9759103fe18e88f9ac4af6857b3309453dd3`.

## Invariant matrix

| Invariant | Severity | Verdict | Row state | Proof / evidence |
| --- | --- | --- | --- | --- |
| `G62-C-THEOREMS` | BLOCKING | PASS | KILLED | Inherited without reopening. Historical declaration remains byte-identical at `ab9b4aadb52fbbcdb62bb8de39f62acbc76f0ffbfa4c8eeb5d1d79f6fff334f4`; fresh full gate prints only allowed axioms. |
| `G62-C-ECONOMY` | BLOCKING | PASS | KILLED | Inherited non-degenerate canonical economy proof `76c32a87d1099fe0d3fb3cbae84fa9e408afaa6e9d454a433e4e6c02999b5b21`; exact gates pass. |
| `G62-C-EXHAUSTIVE` | BLOCKING | PASS | KILLED | Inherited constructor controls remain wired; fresh full gate executes all app/base/proposal/change/verdict seeds. |
| `G62-C-TRUST-CI` | BLOCKING | PASS | KILLED | Released four blobs match; pin/runtime mismatch control, dependency direction, zero escape hatches, allowed axioms, and full CI pass. |
| `I57-01-BOUNDARY` | BLOCKING | FAIL | BLOCKED | Production presently has one checked decision and kills bypass, but the reached duplicate produces the same state while its shipped control remains true behind a disconnected constant. Focused evidence `d53064ca7972a0a747e1da6c85671573d90becfb206fe079345b639751ba0f04`. |
| `G62-C-INHERITED57` | BLOCKING | PASS | KILLED | The unfranchised `.cast` and hard-threshold mutants execute non-degenerate production-rooted sequences and are distinguished; DISJOINT remains inherited killed. Same focused evidence. |
| `G62-C-TRACE` | BLOCKING | FAIL | BLOCKED | The typed replay has no executable JSON decode path, its acceptance ignores the full emitter, an omitted-state emitter survives, and expected-value coverage accepts corrupted departure cleanup. Same focused evidence. |

## Exact three-row mutation matrix

| Open row | Mutants / controls | Result | Terminal state |
| --- | --- | --- | --- |
| `I57-01-BOUNDARY` | bypass; reached duplicate with constant signer and both decisions reached | bypass `KILLED`; duplicate `SURVIVED` (`production == duplicate`, shipped control still `true`) | `BLOCKED` |
| `G62-C-INHERITED57` | unfranchised cast admission; hard-coded threshold; inherited DISJOINT placement | franchise `KILLED`; policy-free `KILLED`; DISJOINT carried `KILLED` | `KILLED` |
| `G62-C-TRACE` | all-error, reordered, altered typed state, same-length, event/change/signer; omitted-state serialized emitter; corrupted member-cleanup value | typed mutants `KILLED`; serialized omission and cleanup-value mutants `SURVIVED`; historical pending coordinate is value-degenerate | `BLOCKED` |

## Property-class counterexamples

1. `PC-I57-PURE-DUPLICATE`: on signer `alice` and the same reachable
   `.openQuestion`, `voteApply` and `voteApplyDuplicate` return BEq-equal states.
   `checkVoteApplyDuplicateCaught` is also `true` because
   `applyVoteEventChecked_count` aliases the unrelated literal
   `checked_decisions := 1`. Any pure duplicate validation with identical
   output survives this value/count shape.
2. `PC-TRACE-TRANSPORT`: the complete `IntegratedTraceStep` codec independently
   round-trips, but replacing `emitIntegratedCorpusJson` with a distinct emitter
   that omits `state` leaves `checkIntegratedCorpus = true`. Any serialized
   coordinate omitted or corrupted outside the typed input list is invisible.
3. `PC-TRACE-VALUE`: changing Bob's post-departure conto to `999` leaves
   `integratedCorpusCoversRequired = true`; `pendingProposals` is empty in every
   corpus state. A producer and replayer sharing the same broken cleanup, or an
   omitted default-valued coordinate, therefore remains green.

## Test, value, and failure-mode coverage

- Test coverage: the real `voteApply → applyVoteEventChecked` boundary executes;
  the bypass, franchise, policy-free, typed trace, and constructor controls all
  execute. The reached duplicate also executes, but no shipped observable kills
  it.
- Value coverage: franchise and threshold fixtures use distinct admins,
  non-admin Bob, a real cast, and thresholds that yield open versus positive.
  The trace has non-zero conti/cassa/collection/vote data, but its cleanup oracle
  omits Bob's absorbed conto and its historical pending store is always empty.
- Static trust: no `sorry`, `admit`, `sorryAx`, custom axiom, `native_decide`, or
  `Lean.ofReduceBool`; printed dependencies are within `{propext,
  Classical.choice, Quot.sound}`.

## Failure modes altered

- Vote refusal now flows through `applyVoteEventChecked` as `Except.error`, is
  visible from `Reactivegas.apply`, and is intentionally degraded to aggregate
  identity only by `foldIntegrated`. The bypass control observes this; the pure
  duplicate-decision failure remains unobservable.
- Backdonation authorization changed from an unevaluable `sorry` boundary to a
  required caller-supplied `BackdonateAuth`; no default policy is inferred, and
  rejection remains visible as `Option.none` / integrated `StepError.rejected`.
- Trace decode failure is not represented: despite the comment claiming
  `FromJson.parse`, replay accepts typed steps, creates partial JSON, pattern
  matches its array shape, and reuses the original values. Malformed or omitted
  serialized coordinates therefore never reach a decoder failure path.
- No resource acquisition, background thread, synchronization primitive, or
  external degradation path is introduced by the scoped Lean delta.

## Reliance

Owner registry `/tmp/reactivegas/ms2/e43/t62-owner-codex/commit-owner-s62-c-a011-grok-a012/handoffs/RELIANCE-s62-c.md`
matches SHA-256 `9cd163eef1047e1c319f4116b695af830f9fcb4d871ffc41f9296a89c8f8ca6f`.
The 16 imported assumptions remain `enforced=9 partial=5 none=1`; the sole
`NONE` is the advisory #47 true/false policy, while the candidate requires an
explicit caller value. No new load-bearing `NONE`, policy inference, membership
path, identity bridge, or dependency reversal was found.

## Residuals

None. Every row is BLOCKING, so no residual disposition is lawful.

## Candidate invariants

None. Both failures are violations of already-ratified rows, not new truths
proposed by this auditor.

## Onward discoveries — outside this ticket

None. `onward-discoveries.md` SHA-256
`5ad53f1cc9d37e989447a54f03af007e631d58c554eaf0d78947cd27158ecada`;
designated owner `reactivegas epic #43 invariant/census backlog`.

## Blocking findings

1. **F-I57-REACHED-DUPLICATE / `I57-01-BOUNDARY`** — production is currently
   one decision at `lean/Reactivegas/Step.lean:172` and
   `lean/KelGroups/Vote/Fold.lean:107`, but the permanent control at
   `lean/Reactivegas/Invariants.lean:1919-1947` does not kill its reached
   duplicate. The same signer/event reaches both validations, yet the duplicate
   result equals production and the control stays true because its count is a
   literal unrelated to either call graph. **Property class:** a permanent
   production-bound proof must make a reached second validation decision fail,
   while separately killing bypass; names, source counts, constants, and
   same-output fixtures do not establish decision cardinality. Evidence:
   `evidence/fresh-focused-instrument.log` SHA-256
   `d53064ca7972a0a747e1da6c85671573d90becfb206fe079345b639751ba0f04`.
2. **F-TRACE-SERIALIZED-VALUE-COVERAGE / `G62-C-TRACE`** —
   `lean/Reactivegas/Invariants.lean:1734-1747` mentions decoding only in
   comments, emits just signer/accepted locally, pattern matches the array, and
   replays the original typed `steps`. The full emitter at lines `1749-1757` is
   never consumed by `checkIntegratedCorpus` at lines `1882-1892`. The coverage
   oracle at lines `1762-1777` also accepts a non-zero corrupted post-departure
   conto and gives `pendingProposals` no distinguishing value. **Property
   class:** serialize and decode signed events plus complete integrated state,
   replay decoded values through `Reactivegas.apply`, independently assert every
   cleanup/stored coordinate with non-degenerate values, and make every
   per-coordinate omission/corruption red. Evidence: same frozen instrument and
   focused receipt.

Finding count: `2` blocking, `0` advisory.

## Frozen instrument

- Shell: `instruments/audit-open-properties.sh` SHA-256
  `257349a4e94307a8973f26cf47ea3f50dd51c447c990e07bfc6e81d8210fe4f5`
- Lean probe: `instruments/AuditOpenProperties.lean` SHA-256
  `461b6f70ecef3e011cde90ff3eba1ba2d676478e3e94b967e1bfc422a7e30b43`
- Bundle SHA-256: `4509f87bdfd24ed2ec31b9ad996da2d3d48d467e9a89259342a4bc9887aef69d`
- Preflight: comment-only decoder, opening-event franchise mutant, and
  disconnected duplicate count were killed before candidate evaluation;
  positive controls were detected.

## Verification receipts

| Command | Exit | Duration | Evidence |
| --- | ---: | ---: | --- |
| Fresh focused instrument + Lean probe | 1 (7 scoped findings; Lean/setup 0) | 560224 ms | `evidence/fresh-focused-instrument.log` SHA-256 `d53064ca7972a0a747e1da6c85671573d90becfb206fe079345b639751ba0f04` |
| `/tmp/reactivegas/ms2/e43/t62-owner-codex/gates/a011/gate-s62-c-a011.sh <audit-worktree>` | 0 | 89453 ms | `evidence/fresh-slice-gate.log` SHA-256 `2a8fd3b02a12487fe2a02de6be86ec0f17672c8eb8aab8437139d21e0fc3409b` |
| `nix develop --quiet -c .../gate.sh.frozen ticket` | 0 | 73185 ms | `evidence/fresh-full-ticket-gate.log` SHA-256 `d369103c357b5a3f4f3d49107bccf8e676ccde832ea893dbd5aaccf790b92b1b` |
| Static postflight provenance/fence/handoff verification | 0 | 644 ms | `evidence/postflight.log` SHA-256 `0405de7c000dc0fc19f98602a91c9759103fe18e88f9ac4af6857b3309453dd3` |

## Build accounting

| Audit build | Cache | Free space before → after (bytes) | Exit / duration |
| ---: | --- | --- | --- |
| 1/3 focused instrument | cold | `202643402752 → 202618986496` | `1` diagnostic / `560224 ms` |
| 2/3 A011 slice gate | warm | `202618941440 → 202563203072` | `0` / `89453 ms` |
| 3/3 full ticket gate | warm | `202563203072 → 202563203072` | `0` / `73185 ms` |

The small between-command free-space drift is reported as measured and not
attributed to this worktree. Candidate tracked status was clean before and
after every run. Aggregate is exactly `15/40`; no ceiling raise was used.

## Advisories

None.

## Recommendation

Do not accept candidate `b90161c`. Return this terminal `FINDINGS` report to
the ticket owner. Per the auditor brief, no repair, ceiling raise, or
submission-2 request is made by this auditor.
