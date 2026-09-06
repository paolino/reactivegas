# Commit Audit

- Submission: `2/2` (FINAL)
- Base: `0369c222ce1e77f6ccde1b5f902ed9d1c72b3208`
- Rejected: `855219762c623dc75d1d0bd6f4a73310ed813b82`
- Candidate: `7c2379d52798aeccf64149264d912a33e8978431` (tree `3f58fc04cacc4421d8a3e0271a9bbbd516bdd6f4`)
- Released dependency: `d7a3e05116f40920f3d78daf3e1818ad17c74a74`
- Mandate: `3be23d02e5aab37cef399d435c5e639d48825107808d344e7e38eb0ac04e67b0`
- Scope: REPAIR `8552197..7c2379d` plus `F-I57-ONE-DECISION`, `F-TRACE`, `F-I57-INTEGRATED-LEGS`, and `F-I57-TOOLCHAIN`
- Verdict: `FINDINGS` (`3` blocking findings)
- Audit loop: submission `2/2`; next submission `FORBIDDEN`; blocking rows force ticket re-cut
- Ceiling raises: `1/2`; ledger `/tmp/reactivegas/ms2/e43/t62-owner-codex/ceiling-ledger.md` sha256 `48c416c03d256067476139a5ef5686cc3d2f40393257a9b94f11b11fd74c8349`
- Campaign: `CLOSED` — ended by `SET-POINT`; killed `4`, residual `0`, blocked `2`, open `0`
- Campaign ledger: `campaign-ledger.md` sha256 `286c4f04618b5eae564771ce55cea0698ecd1967e3d31d5580ae40eb766ba006`
- Builds: `37/40` this ticket; this audit `3/3`, cache=`cold,warm,warm`
- Remote/draft: `NONE` / `NONE`
- Report hash: computed from final bytes and bound in `report.md.sha256` and the terminal `AUDIT-FINDINGS` event

Provenance and scope pass. The worktree is clean and detached at the exact
candidate/tree; the release and rewritten base are ancestors; the candidate is
absent from remotes. The repair delta contains exactly
`lean/KelGroups/Vote/Fold.lean` and `lean/Reactivegas/Step.lean` for
F-I57-ONE-DECISION, plus `lean/Reactivegas/Invariants.lean` for that finding,
F-TRACE, and the three inherited legs. The authorized release delta contains
exactly `.github/workflows/ci.yaml`, `justfile`, `lean/lean-toolchain`, and
`scripts/check-lean-toolchain` for F-I57-TOOLCHAIN; all four candidate blobs
equal the release. Old/new repair diffs are byte-identical, and old/new complete
#62 patches are byte-identical. Evidence: `evidence/provenance-v2.log` sha256
`443c1ef39ce5195acd70d3c29954af302dad5cc97525736a3fb46978615a9218`.

## Invariant matrix

| Invariant | Severity | Verdict | Row state | Proof / evidence |
|---|---|---|---|---|
| `G62-C-THEOREMS` | BLOCKING | PASS | KILLED | Carried terminal without reopening. Historical declaration still hashes to accepted `ab9b4aadb52fbbcdb62bb8de39f62acbc76f0ffbfa4c8eeb5d1d79f6fff334f4`; exact full ticket gate passes. |
| `G62-C-ECONOMY` | BLOCKING | PASS | KILLED | Carried terminal without reopening; no repair file changes its boundary and the exact gates pass. |
| `G62-C-TRACE` | BLOCKING | FAIL | BLOCKED | Sequential typed replay exists, but it is not serialized replay and does not store/compare the complete integrated state or assert the cleanup fields. Instrument/evidence below. |
| `G62-C-EXHAUSTIVE` | BLOCKING | PASS | KILLED | Carried terminal without reopening; constructor seeds and exact gates remain green. |
| `G62-C-INHERITED57` | BLOCKING | FAIL | BLOCKED | Repaired witnesses are production-rooted, but I57-01 duplicate, I57-06-FRANCHISE cast-admission, and I57-06-POLICYFREE threshold-threading mutants are not killed by their shipped controls. |
| `G62-C-TRUST-CI` | BLOCKING | PASS | KILLED | Pin and executing Lean both resolve to `4.25.0`; the same comparator rejects a live mutated pin and is wired into `just ci` and CI. `evidence/toolchain-preflight.log` sha256 `b6117b60dfe019f8dd528dbcfd77de5b2c0fa23135c0c3af37bb1ee4096d5d15`. |

Test and value coverage were checked explicitly. The current production vote
edge has one decision and uses distinct admin/non-admin values. The integrated
corpus has non-zero accounts, a cassa, a collection, pending approvals, real
membership changes, and a verdict-changing franchise, but its recorder drops
those distinguishing coordinates. DISJOINT switches one real ballot between
sides and is non-degenerate. FRANCHISE and POLICYFREE production witnesses are
non-degenerate, but their named mutant controls use an opening event where the
claimed cast/threshold defect is unobservable. The toolchain control mutates the
parsed patch component and requires the real comparator to exit nonzero.

## Touched inherited #57 legs

| Leg | Verdict | State | Independent coverage |
|---|---|---|---|
| `I57-01-BOUNDARY` | FAIL | BLOCKED | `voteApply -> applyVoteEventChecked` currently makes one decision, but the duplicate control compares different signers and its duplicate arm stops at the first rejection; it never executes or detects the second decision. |
| `I57-06-DISJOINT` | PASS | KILLED | A three-admin integrated open/assent/dissent sequence leaves one side only; `placeBallotMutant` retains both and the built negative control distinguishes it. |
| `I57-06-FRANCHISE` | FAIL | BLOCKED | The real integrated witness refuses Bob's cast, but the named “cast-admission” mutant invokes Bob's `.openQuestion`, not `.cast`. |
| `I57-06-POLICYFREE` | FAIL | BLOCKED | The real integrated witness distinguishes thresholds `2` and `1`; the named hard-policy mutant is exercised only on `.openQuestion` and the check requires mutant/real equality on that threshold-insensitive event. |
| `I57-10-TOOLCHAIN` | PASS | KILLED | Live pin/runtime equality and mutated-pin rejection reproduced independently. The old Lean domain-value Bool is not cited as toolchain evidence. |

All other #57 subleg states are preserved from submission 1:
`I57-02-EXHAUSTIVE`, `I57-03-NOOP`, `I57-04-AUTH`, `I57-05-R45`,
`I57-06-PARTITION`, `I57-06-NOSTALE`, `I57-07-NOEXPIRY`, `I57-08-TRUST`,
and `I57-09-DIRECTION` remain `KILLED`.

## Residuals

None. Every campaign row is BLOCKING; no residual is lawful.

## Candidate invariants

None.

## Onward discoveries — outside this ticket

None. `onward-discoveries.md` sha256
`ec3fce0e9c5942c36b053af25c642cd5f094eeba521f8178ff2752bb13819ec2`.

## Blocking findings

1. **F-I57-ONE-DECISION / `I57-01-BOUNDARY`** — the production edge at
   `lean/Reactivegas/Step.lean:172` and
   `lean/KelGroups/Vote/Fold.lean:107` is presently one checked decision, and
   the bypass control is discriminating. The duplicate control at
   `lean/Reactivegas/Invariants.lean:1812` is not: it runs production with
   admin `alice`, then runs the duplicate wrapper with non-admin `bob` and
   accepts `.error`; the duplicate wrapper exits on its first validation, so
   the second validation never executes. No shipped structural property counts
   the actual production closure, while the carried auditor instrument's green
   `validation_decisions=1` counts historical `applyVoteEvent`, not the new
   checked callee. The repair also places callable bypass/duplicate helpers in
   the production `Reactivegas.Step` module at lines `235` and `244` rather than
   a frozen mutation-only surface. **Property class:** the permanent production
   call-graph property must establish exactly one validation decision dominating
   effects/sweep and demonstrably fail for both a reached duplicate-decision
   mutant and a bypass mutant.

2. **F-TRACE / `G62-C-TRACE`** — `IntegratedTraceStep` at
   `lean/Reactivegas/Invariants.lean:1582` stores only member keys, `conti`, and
   open/closed IDs rather than the full `GroupState State`: member details,
   both pending stores, `casse`, `collections`, and full vote values are absent.
   The JSON emitter at line `1672` additionally omits its own `event`, `conti`,
   and `change` fields. `replayIntegratedCorpus` at line `1668` accepts an
   in-memory `List IntegratedTraceStep`; nothing parses or replays the emitted
   JSON. Coverage at line `1685` asserts no `conti`, `casse`, or `collections`
   cleanup value. Thus same-producer typed equality and the four typed mutants
   cannot observe defects in omitted serialized/state coordinates.
   **Property class:** serialize the signed integrated event and complete
   integrated state once in their owning components, parse that representation,
   replay sequentially through `Reactivegas.apply`, compare every stored value,
   and make all-error, reordered, altered-state, same-length, and per-coordinate
   mutants fail.

3. **F-I57-INTEGRATED-LEGS / `I57-06-{FRANCHISE,POLICYFREE}`** — the repaired
   real witnesses at `lean/Reactivegas/Invariants.lean:1954` and `:1992` now use
   non-degenerate integrated folds, but their mutation floor remains open.
   `checkI57FranchiseMutant` at line `1975` bypasses authorization for
   `.openQuestion`, not the contracted unfranchised `.cast` placement.
   `checkI57PolicyFreeMutant` at line `2009` applies real and hard-coded
   thresholds only to `.openQuestion` and explicitly accepts equal results,
   although opening is threshold-insensitive. DISJOINT is repaired and killed.
   **Property class:** apply each targeted mutant to the same production-rooted
   cast/threshold-distinguishing sequence, prove the edit executed, and require
   the permanent FRANCHISE/POLICYFREE property to fail.

The fresh pre-falsified repair instrument is
`instruments/audit-repair-properties.sh` sha256
`e586bc2cc11749c0653cbd4c3c2dadab893cfbe5a1341cab634af27bfdfec451`.
It first exits `1` on the rejected candidate (`evidence/repair-instrument-known-defective-v2.log`,
sha256 `e98792f1a46b8bf14b1b555b1790a0eaaf9e4efa9391dfd1a0bb26e21006c8aa`),
then exits `1` with seven scoped failures on the final candidate
(`evidence/repair-instrument-candidate-v2.log`, sha256
`0853cf233302166be3e7133c0097f4a26a4c09f2a172617351d8c23f4e340f88`).

## Verification receipts

| Command | Exit | Duration | Evidence |
|---|---:|---:|---|
| `scripts/check-lean-toolchain` | 0 | 837 ms | `evidence/toolchain-preflight.log` sha256 `b6117b60dfe019f8dd528dbcfd77de5b2c0fa23135c0c3af37bb1ee4096d5d15` |
| `nix develop --quiet -c .../gate-s62-c.sh` | 0 | 94399 ms | `evidence/fresh-slice-gate.log` sha256 `66301143d43c9b7aedd3b18370d6051d48246b89eca91560d4f33dc982664f99` |
| `nix develop --quiet -c bash -lc 'cd lean && lake build Reactivegas.Invariants'` | 0 | 1390 ms | `evidence/fresh-focused-proof.log` sha256 `2c5ad1546e2e456cf0af787aefa0a13815502fd96b71e535c9f2158cfe873633` |
| `nix develop --quiet -c .../gate.sh.frozen ticket` | 0 | 73169 ms | `evidence/fresh-full-ticket-gate.log` sha256 `90c28b3780d17c2970231b2f880dd1a6d558491a81d5064c854b394ac4947521` |
| Repair instrument, rejected seed | 1 (pre-falsification) | 260 ms | `evidence/repair-instrument-known-defective-v2.log` sha256 `e98792f1a46b8bf14b1b555b1790a0eaaf9e4efa9391dfd1a0bb26e21006c8aa` |
| Repair instrument, candidate | 1 (findings reproduced) | 296 ms | `evidence/repair-instrument-candidate-v2.log` sha256 `0853cf233302166be3e7133c0097f4a26a4c09f2a172617351d8c23f4e340f88` |

Build accounting: run 1 cold, free space `203797073920 -> 203722022912`
bytes; runs 2 and 3 warm, `203722022912 -> 203722022912` bytes. The
candidate tracked status was clean before and after every run. Manifest and all
three frozen gate hashes matched before and after. Campaign stopped by
`SET-POINT`, not by budget or tail.

## Advisories

- The #47 `BackdonateAuth` product truth value remains unresolved exactly as
  ratified. The candidate continues to require explicit caller-supplied
  authorization and introduces no default, inference, or threshold policy.
