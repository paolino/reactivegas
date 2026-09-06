# S4-B SUBMISSION — FINAL (all rows executed; audit-ready)

**Candidate:** `189e1ed306f8f8e8bcdd11eeab4fc5657a518fc8` (committed, clean tree verified post-run).

**Seat:** `muse` · **Branch:** `chore/66-s4b-mirrors` · **Base:** `3590c001`
(S2R landed; rebased clean from `d670323`, owned diff byte-preserved).
**Instruments:** brief `b4a79201…`, reconciliation v2, acceptance instrument v2
(`2214ff8a…`, self-verified). v1 statements cited only as superseded.
**Status of this file:** FINAL — packet complete, audit-ready (see §7).

## 1. Candidate content (owned diff, fence R16)

| path | kind |
|---|---|
| `lean/Reactivegas/Mirrors.lean` (new) | 11 P mirrors + 11 theorems + 1 supporting projection + 8 private helper theorems |
| `lean/KelGroups/Mirrors.lean` (new) | 5 K + 3 V mirrors/theorems + 3 private helper theorems |
| `scripts/check-lean-mirrors` (new, +x) | mandatory checker: discovery + 19-row table + 4 exceptions + orphan check + 17 eval assertions + nonce receipt |
| `justfile` (additive only) | 1 wiring line + nonce/assertion lines in `lean` recipe + `lean-mirrors` recipe block; zero pre-existing lines altered (diff: additions only) |

No lakefile/aggregator/existing-source edits. No new runtime monitor, no
coordinator behaviour (R2). No original theorem touched or weakened (R5);
generic `[DecidableEq α]` only on the new K5 pair. Threshold an explicit
callable parameter in V3 only, never compared, no default (R6). P01/P07 relate
existing expressions, no new runtime (R4).

## 2. Correspondence rows (19) — statement, evidence, falsification control

Exact statements per `/tmp/s4b-t32.log` (T32, exit 0). Shaping proofs T1–T5.
Per-row: production statement preserved in all controls except the two
superseded v1 statement-mutations (C05/C11, kept as superseded logs).

| row | statement (abridged) | control C-id | mutation (single, ASCII-anchored) | observed failure |
|---|---|---|---|---|
| P01 comune | `comune_not_a_member view ↔ ((!isMember comuneId view) = true)` | C5R (v2; C5 superseded) | scratch: Prop relatum minus negation, well-typed; original-named theorem + proof byte-identical | exit 1, sole error at `comune_not_a_member_corr`, both branches `False` |
| P02 conservation | `conservation s ↔ conservationB s = true` | C6 | drop `- escrowSum` term | exit 1 at `conservation_corr`, goal echoes both equations |
| P03 solvent | `solvent view s ↔ solventB view s = true` | C7 | second-conjunct `.all`→`.any` | exit 1 at `solvent_corr`, names echoed |
| P04 insolvent | `insolvent view s ↔ insolventB view s = true` | C8 | `.any`→`.all` | exit 1 at `insolvent_corr` (+orphaned-lemma warning) |
| P05 uniquePledges | `uniquePledges col ↔ uniquePledgesB col = true` | C9 | `\|\|`→`&&` (def indent-anchored) | exit 1 at `uniquePledges_corr` |
| P06 allUniquePledges | `allUniquePledges s ↔ allUniquePledgesB s = true` | C10 | negate per-collection check | exit 1 at `allUniquePledges_corr` |
| P07 permissionToClose | `permissionToClose col ↔ ((permitted && isEmpty) = true)` | C11R (v2; C11 superseded) | scratch: Prop relatum `∧`→`∨`, well-typed; original-named theorem + proof byte-identical | exit 1, sole error at `permissionToClose_corr`, both shapes `False` |
| P08 escrowHeld | `escrowHeld … ↔ escrowHeldB … = true` | C12 | `amt = v` → `amt = v + 1` | exit 1 at `escrowHeld_corr`, both-direction type mismatches echoing the equations |
| P09 governanceEnacts | `governanceEnacts … ↔ governanceEnactsB … = true` | C13 | negate per-collection decide | exit 1 at `governanceEnacts_corr` |
| P10 doubleEntry | `doubleEntry … ↔ doubleEntryB … = true` | C14 | `+ v` → `+ (v + 1)` (cassa) | exit 1 at `doubleEntry_corr`, both equations echoed |
| P12 canCloseGroup | `canCloseGroup … ↔ canCloseGroupB … = true` | C15 | negate `collections = []` check | exit 1 at `canCloseGroup_corr` |
| K1 PendingWellFormed | `… ↔ pendingWellFormedB … = true` | C16 | `&&`→`\|\|` | exit 1 at `pendingWellFormed_corr` |
| K2 MembersCoherent | `… ↔ membersCoherentB … = true` | C17 | negate per-entry decide | exit 1 at `membersCoherent_corr`, both directions |
| K3 PendingCoherent | `… ↔ pendingCoherentB … = true` | C18 | negate per-entry mirror | exit 1 at `pendingCoherent_corr`, both directions |
| K4 WellFormed | `WellFormed gs ↔ wellFormedB gs = true` | C19 | negate last conjunct | exit 1 at `wellFormed_corr`, both directions |
| K5 Enacts | `Enacts … ↔ enactsB … = true` (`[DecidableEq α]`) | C20 | `.isSome`→`.isNone` | exit 1 at `enacts_corr`, both directions |
| V1 QuestionClean | `… ↔ questionCleanB … = true` | C21 | third-check `.all`→`.any` | exit 1 at `questionClean_corr` |
| V2 SweepReady | `SweepReady … ↔ sweepReadyB … = true` | C22 | negate closed-verdict decide | exit 1 at `sweepReady_corr`, both directions |
| V3 VoteWellFormed | `VoteWellFormed θ … ↔ voteWellFormedB θ … = true` | C23 | negate sweep conjunct (`\|\|` attempt superseded: weak `simp-made-no-progress` naming) | exit 1 at `voteWellFormed_corr`, name-echoing mismatches both directions |

Every run: exactly one temp mutation, reverted and sha256/restore-verified
(Mirrors shas `3172103b…` / `1be677dc…` hold throughout); error lines confined
to the row's own declaration (per-log audit, no cross-identity contamination);
no first-failure masking (19 separate commands + 1 superseded attempt + 2
v2 re-establishments). Accurate label (v2 Amd.1): P01/P07 production proofs are
truth tables parametric in the reused value — vacuous to expression-body
mutation — so evidenced sensitivity is to relatum defects, not claimed as
expression-body strength. Full logs `/tmp/s4b-ctrl-c{05..23}.log`,
`/tmp/s4b-ctrl-c05r.log`, `/tmp/s4b-ctrl-c11r.log`.

Finite-reduction exactness (R14/R15): member quantifications reduce over
occurring member-list keys with first-match-consistent membership lemmas
(duplicates give redundant identical checks); absent keys read default zero
(`bal_absent`); cassa-wide quantification via `canClose_third`; open-question
carrier read through `assocLookup` at occurring keys. No `Nodup` or
well-formedness premise in any statement.

## 3. Inventory reconciliation (R7)

Discovery (compiled env, result-sort, module-attributed; T11 staging,
T40 post-rebase): **24 = 19 tabled rows + 4 named exceptions + 1 structural
below-exclusion** (`Reach.below`, eliminator machinery of discovered induct
`Reach`, logged by rule). Exceptions: V4 `PreservesQuestionSemantics`
(definitional EQ proved by `rfl` in-driver), P11 `authorizedStep` (14
per-constructor `rfl` projections onto existing `isResponsabile`), R0
`stalled` (`inferInstance` Decidable + `decide … = false` evaluation),
P13 `Reach` (NOT-EXECUTABLE, bounded). Stale rows / missing consts / shape
mismatches / orphans / empty discovery all fail loud (none observed).
17 executable-evaluation assertions (`by decide` on tiny witnesses) elaborate.
The nineteen was not used as an allowlist: post-rebase discovery re-derived
(exporter adds zero Props).

## 4. Mandatory-path rows — all executed (A-001 wake received as NOTE-004)

- **C1 clean baseline**: CLOSED — cold `just lean` exit 0 at `189e1ed` (all base gates incl. S2R axiom gate tracked=29/walked=29, build 27 jobs, checker 19 rows + fresh-nonce receipt + assertion). Log `/tmp/s4b-c1g.log`. (Prior reds preserved as spent: S1 quota, C1 tracked-untracked, C1r CWD-wiring — all genuine.)
- **C2 counterpart-absent**: CLOSED — probe `s4b_probe_nocounterpart` through `just lean` → exit 1, checker names it (`MIRROR-UNCOVERED Reactivegas.Predicates :: s4b_probe_nocounterpart`), discovery 25, 19 rows intact. Log `/tmp/s4b-c2.log`. Restored byte-identical.
- **C3 theorem-absent**: CLOSED — probe pred + orphan mirror through `just lean` → exit 1 with the distinct two-line signature (`MIRROR-UNCOVERED … s4b_probe_notheorem` + `MIRROR-ORPHAN s4b_probe_notheoremB …claimed by no row`), separate receipt from C2. Log `/tmp/s4b-c3.log`. Both restored byte-identical.
- **C4 ineffective-while-present**: CLOSED — checker neutered to unconditional `exit 0` while present through `just lean` → build green, checker noop, assertion fired `MIRROR-RECEIPT-ABSENT: checker did not operate` → exit 1. Establishes execution-enforcement (sensitivity weight stays on C2/C3/C5–C23). Log `/tmp/s4b-c4.log`. Checker restored byte-identical. 127 never claimed.
- **C26 final CI**: CLOSED — cold `nix develop --quiet -c just ci` exit 0 at exact clean `189e1ed` (toolchain, build, format, hlint, lean, corpus gate + exporter verify, corpusExport built). Raw log `/tmp/s4b-c26.log` (17,987 lines, sha256 `699792e4…`). Tree verified clean post-run.

## 5. Proof-axiom and totality at the final tree (R13, tree-conditional)

T33 (`lake env lean`, single-file, S1-built oleans): all 20 theorems depend
ONLY on permitted axioms (`propext`, `Quot.sound`; no `sorryAx`, no
`Classical.choice`; private helpers covered transitively through use);
`PANIC at` count 0 in both streams. Zero-warning elaborations throughout
(T2/T5/T38/T39). FINAL-TREE RERUN (T42, post-C26, `189e1ed` oleans): 20/20
axiom lines identical (propext/Quot.sound only), 0 panics — C24/C25 CLOSED at
the final tree.

## 6. Exact spend (failed and warm included; file ops/reads spend nothing)

- Substantive 8/8 EXACT (S1, C1, C1r, C1g, C2, C3, C4, C26 — all genuine; Q3 gap closed by the A-004 +2 grant with zero overrun).
- Targeted 42/60: T1–T11 (11) · C5–C23 killing runs (19) + 1 superseded attempt ·
  T32 statements · T33 axioms/totality · T36 counted setup failure · T37
  checker+receipt green · C05R C11R v2 re-establishments (2) · T38–T41
  re-verifications (4) · T42 final-tree axioms (1). Every invocation listed;
  no warm repeats.
- Submissions 0/2 (packet delivered; audit commissioning is the owner's). Remaining caps: 0 substantive (8/8 at cap — no further whole builds without a new grant), 18 targeted.
- Measured gap: CLOSED — Q3 gap of 2 covered exactly by the A-004 +2 grant
  (8/8, no overrun, no compression, no reclassification). Any further whole
  build returns as a new exact gap before overrun.

## 7. Unclosed, honestly, with owners

- **S2R quota dependency (QUESTION-001 / A-001): RESOLVED by the landed base**
  (`3590c001`; quota verified absent) — C1r/C2/C3/C4/C26 executed above.
  Q3 gap CLOSED by the A-004 +2 grant (8/8 exact, no overrun, no compression).
- **Rebase done, integration verified** (d670323, then `3590c001`; justfile regions coexist;
  dep closure byte-identical; exporter adds zero Props). S2R-base lean deltas
  triaged (comment-only in cone; Bool-decoy swaps elsewhere).
- **Candidate SHA: `189e1ed306f8f8e8bcdd11eeab4fc5657a518fc8`** — committed,
  clean tree verified after every control and after C26. No push/PR/merge (none authorized).
- **Awaiting owner commissioning of the fresh independent audit** (Codex
  gpt-6-astra/high or eligible Grok per A-004; 8/60 TOTAL both submissions).
  Packet `handoffs/SUBMISSION.md` FINAL. All original S4 requirements remain
  open to falsification by the auditor; no row closed by implication.
