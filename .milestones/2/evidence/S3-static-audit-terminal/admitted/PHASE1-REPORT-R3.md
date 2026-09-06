# S3 Phase 1 report — REVISION R3 (one coherent updated packet)

Local only. No push, PR action, comment, gist, publication, deployment or merge.
No `docs/en/design/` writes. No coverage claim. No Phase-2 grant exists; no
campaign is proposed as funded. Spend: explicit overrun **4 against ceiling 3**.

- Original `handoffs/PHASE1-REPORT.md` (`dbc2cb68…`) and R2
  (`handoffs/PHASE1-REPORT-R2.md`, `32c06530…`) **preserved as history**.
- Base/seat/contract: HEAD `3590c0015b84fd58004bf6fb44dd18b107304c48`, parent
  `d670323…`, tree `44a1f0b…`, PR #88 squash, #66 OPEN, seat PID/PGID `2401092`,
  contract rev 3. Worktree clean at R3 (`git status --porcelain=v1` empty).
- **Mandate not narrowed:** 239 fully-qualified source identities are the
  source-derived floor; the environment (compiler-generated) extent is
  unenumerated (gap G-B1). If the required extent exceeds what is measured
  here, that is a finding about the measurement, not a smaller target.
- Static only for this revision: reads, greps, `git` object-DB retrieval,
  writing. No build, probe, or elaboration run.

## §1. Receipt inventory — exact, reconciled, no double count

**1A. File roster: exactly 43 distinct `*campaign-ledger*` files** (numbered by
`find … | sort`; the R2 "row 43" note is removed — the t48-owner-codex main TSV
is file 3, listed once):

| # | file (under `reactivegas/ms2/`) | format | rows |
|---|---|---|---|
| 1 | `.archived/t-simulator/.archived/auditor-claude-round2-s1/handoffs/campaign-ledger.md` | md table | 3 (R2-CORE/SUBJECTS/CLICK-PARAMS) |
| 2 | `.archived/t-simulator/.archived/commit-owner-glm-vote-gap/handoffs/campaign-ledger.md` | prose (0 table rows) | 0 |
| 3 | `e43/.archived/t48-owner-codex/campaign-ledger.tsv` | tsv | 18 (10 E-* + 8 INV-48-*) |
| 4 | `e43/.archived/t48-owner-codex/docs-auditor-1/handoffs/campaign-ledger.tsv` | tsv | 8 (M1–M8) |
| 5 | `e43/.archived/t48-owner-codex/docs-auditor-2/handoffs/campaign-ledger.tsv` | tsv | 8 (M1–M8) |
| 6 | `e43/.archived/t48-owner-codex/emitter-auditor-1/handoffs/campaign-ledger.tsv` | tsv | 18 (same shape as 3) |
| 7 | `e43/.archived/t48-owner-codex/emitter-auditor-2/handoffs/campaign-ledger.tsv` | tsv | 18 (same shape as 3) |
| 8 | `e43/.archived/t48-owner-codex/inversions-auditor-1/handoffs/campaign-ledger.md` | md | 6 (I10-*) |
| 9 | `e43/.archived/t48-owner-codex/pledge-auditor-1/handoffs/campaign-ledger.md` | md | 4 (A1–A4) |
| 10 | `e43/.archived/t54-composition-owner-codex/campaign-ledger.md` | md | 6 |
| 11 | `e43/.archived/t54-composition-owner-codex/campaign-ledger-v3.md` | md | 6 |
| 12 | `e43/.archived/t57-owner-codex/campaign-ledger.md` | md | 10 |
| 13 | `e43/.archived/t59-owner-claude/docs-auditor-1/evidence/campaign-ledger.tsv` | tsv | 13+ (I-FENCE-*/I-R1-*/M-*/FENCE-*) |
| 14 | `e43/.archived/t59-owner-claude/mandate/campaign-ledger.tsv` | tsv | 8+ (E-*/FENCE-*/M-*) |
| 15–19 | `e43/.archived/t62-owner-codex/.archived/commit-auditor-s62-c-a011-s1-codex-r1`, `-a011-s2-glm`, `-a013-grok`, `-s1-codex`, `-s2-codex` `campaign-ledger.md` | md | decision/SHA refs, no mutant rows |
| 20 | `e43/.archived/t62-owner-codex/.archived/commit-owner-s62-c-a011-grok-a012/handoffs/campaign-ledger.md` | md | A-011 refs |
| 21 | `e43/.archived/t62-owner-codex/campaign-a013/campaign-ledger.md` | md | SHA refs |
| 22 | `e43/.archived/t62-owner-codex/campaign-ledger.md` | md | 9 (INV-62-*) |
| 23 | `e43/.archived/t62-owner-codex/campaign-ledger-s62-c-a011.md` | md | A-010/A-011 |
| 24 | `e43/t54/.archived/commit-auditor-s1/handoffs/campaign-ledger.md` | md | 31 (R-1..R-29 + R-2b + E-PRED) |
| 25 | `e43/t54/.archived/commit-auditor-s2/handoffs/campaign-ledger.md` | md | 31 (same shape) |
| 26 | `e43/t54-vote-coverage/auditor-slice-a-s1/handoffs/campaign-ledger.md` | md | 6 (INV-54-*) |
| 27 | `e43/t54-vote-coverage/auditor-slice-a-s2/handoffs/campaign-ledger.md` | md | 7 (6 INV-54-* + R-45) |
| 28 | `e-haskell-impl/t74-corpus-exporter/evidence/campaign-ledger.md` | md | 2 (E-CLOSED/E-ONLY) |
| 29 | `e-lean-compliance/auditor-choices-codex/evidence/inputs/t48-campaign-ledger.md` | md | 10 (copy of file 34 shape) |
| 30 | `e-lean-compliance/auditor-choices-codex/evidence/inputs/t54-campaign-ledger.md` | md | 7 (copy of file 27 shape) |
| 31 | `t48-inversions/.archived/commit-auditor-s48-i-recut1-s1/handoffs/mutation-campaign-ledger.md` | md | 6 (SURFACE/NAME-BINDING/GUARDS/CANFAIL/AXIOMS/REGRESSION) |
| 32 | `t48-inversions/.archived/commit-auditor-s48-i-s1/handoffs/campaign-ledger.md` | md | 10 (candidate `4898e55e`) |
| 33 | `t48-inversions/.archived/commit-auditor-s48-i-s2/handoffs/campaign-ledger.md` | md | 10 (candidate `a408e09…`) |
| 34 | `t48-inversions/campaign-ledger.md` | md | 10 (candidate `a408e09…`) |
| 35 | `t-release-pipeline/.archived/recut-s001-auditor/handoffs/campaign-ledger.md` | md | 10 (I001 + …; I001 KILLED-S001, C002 DEFERRED-S002) |
| 36 | `t-simulator-fable/.archived/commit-auditor-grok-batch1-s1/handoffs/campaign-ledger.md` | md | 12 (INV-* all KILLED) |
| 37–41 | `t-simulator-fable/handoffs/batch2`, `fidelity`, `fidelity-recut`, `geometry-permalinks`, `strips` `*-campaign-ledger.md` | md | GEO/IT/LINK/E-/NOTE- rows |
| 42–43 | `t-toolchain-contract/.archived/auditor-s1-corrected`, `t-toolchain-contract/handoffs/campaign-ledger.tsv` | tsv | 4 each (INV-TC-01..04, OPEN/NONE) |

**1B. Lean-mutant receipt rows (S3-relevant set), one per row.** Markings:
STALE-DEMONSTRATED (named context change shown in §2) / UNESTABLISHED-REUSE
(footprint unbound; nothing disproved; fresh run needed) / UNRECOVERABLE
(provenance genuinely irretrievable, reason given). Kinds per rev-3
(FIXTURE/CHECKER/PRODUCTION-DEFINITION; NON-MUTANT where no kill is recorded).
Re-key target = current qualified identity or explicit none.

Files 32/33/34 (+29, copy of 34): t48 inversion rows. Recorded candidates
`4898e55e` (32) / `a408e09…` (33, 34). Evidence hashes truncated to 8 hex in
the ledger (full values live in the audit reports, also archived — retrieval
not pursued: the checker diff below already settles status).

| row | file 32 state | file 33/34 state | kind | marking + evidence | re-key target |
|---|---|---|---|---|---|
| INV-48-I-SURFACE | OPEN | OPEN | CHECKER (+fixture decoys) | STALE-DEMONSTRATED (checker D1: script rewritten 467+/247+ lines) | 14 `step_*_inv` binding rows |
| INV-48-I-GUARDS | OPEN | OPEN | CHECKER | STALE-DEMONSTRATED (D1) | 14 `step_*_inv` guard conjuncts |
| INV-48-I-CANFAIL | OPEN | KILLED (33) / OPEN (34) | PRODUCTION (removal/unwiring kills) + CHECKER (bypass) | STALE-DEMONSTRATED (D1; checker changed; unwiring subjects predate #79) | inversion rows + `stepDetailed_erases` |
| INV-48-I-AXIOMS | OPEN | KILLED (33) / OPEN (34) | CHECKER (sorry/custom-axiom) | STALE-DEMONSTRATED (D1; six-report instrument superseded by axiom gate) | axiom-gate scope (≈239 env identities, G-B1) |
| INV-48-I-REGRESSION | OPEN | OPEN | CHECKER (163/163 lexical count) | STALE-DEMONSTRATED (D2: quota instrument gone at base — verified no `163`/`expectedDeclarations` in `scripts/`) | none (instrument retired) |
| INV-48-I-FENCE | KILLED | KILLED | CHECKER (forbidden-path rejection) | STALE-DEMONSTRATED (D1) | fence controls, not theorem rows |
| INV-48-I-EVENT-SYNTAX | KILLED | KILLED | CHECKER | STALE-DEMONSTRATED (D1) | `Event` 14-ctor vocabulary |
| INV-48-I-INV-HYP-SYNTAX | OPEN | OPEN | CHECKER | STALE-DEMONSTRATED (D1) | 14 `step_*_inv` hypotheses |
| INV-48-I-STEP-ITE | KILLED | KILLED | CHECKER (no-`step`-unfold fails at `split`) | STALE-DEMONSTRATED (D1 checker; subject `Step.lean` also re-cut since — D4) | `step` unfolding discipline |
| INV-48-I-STEPEVENT-DELEGATE | KILLED | KILLED | CHECKER | STALE-DEMONSTRATED (D1) | `stepEvent` delegation |

File 31 (recut1): 6 rows, all killed (controls passed). Same checker lineage →
STALE-DEMONSTRATED (D1). Re-key: same targets as above.

Files 26/27 (+30, copy of 27): t54 vote rows. Rejected/submission base
`757dac98`; D3 shows all three subject files massively changed.

| row | file 26 | file 27 | kind | marking | re-key target |
|---|---|---|---|---|---|
| INV-54-PARTITION | OPEN | KILLED (silent-deletion mutant, full 40-hex source+log hashes) | PRODUCTION | STALE-DEMONSTRATED (D3; mutant `partition-silent-deletion-mutant.lean` archived and readable) | `KelGroups.Vote.questions_partition` |
| INV-54-DISJOINT | KILLED | KILLED | PRODUCTION | STALE-DEMONSTRATED (D3) | `KelGroups.Vote.ballots_nodup_disjoint` |
| INV-54-NOSTALE | KILLED | KILLED | PRODUCTION | STALE-DEMONSTRATED (D3) | sweep/`open_questions_are_open` rows |
| INV-54-FRANCHISE | OPEN | KILLED (unfranchised-recast, archived readable) | PRODUCTION | STALE-DEMONSTRATED (D3 + D5: mutant trace uses removed `.admitMember`/`.setRoles` constructors — the fixture itself cannot elaborate at this base) | `KelGroups.Vote.franchise_of_tallies`, `KelGroups.Vote.unfranchised_cast_noop` |
| INV-54-NOEXPIRY | OPEN (advisory) | OPEN (advisory) | PRODUCTION-gap instrument (member-event gap v2, archived) | STALE-DEMONSTRATED (D3 + D5) | `KelGroups.Vote.no_expiry` (single-question scope, not widened) |
| INV-54-POLICYFREE | KILLED | KILLED | PRODUCTION (legacyThreshold hard-code) | STALE-DEMONSTRATED (D3) | threshold/verdict rows |
| R-45 | — | carried | boundary violation record | STALE-DEMONSTRATED (vote machine re-cut) | R-45 disposition (outside S3) |

Files 24/25 (t54 auditor R-1..R-29 + R-2b + E-PRED; file 24 has R-11 FAIL):
gate-wiring/import/direction controls, subjects are pipeline properties, no
theorem identity, mutant sources not pinned per row → all rows
UNESTABLISHED-REUSE (file 24) / same (file 25); re-key target none. R-11 FAIL
is the ledger's own verdict, preserved, not re-labelled.

File 12 (t57): 10 rows, all KILLED, gate base `bb3ac41`, toolchain Lean 4.25.0
bound per receipt. D4 shows the whole vote machine + `Step.lean` re-cut since
→ all 10 STALE-DEMONSTRATED. Re-key: INV-57-BOUNDARY/NOOP/AUTH/EXHAUSTIVE →
vote admissibility/sweep rows; INV-57-NOEXPIRY → `KelGroups.Vote.no_expiry`;
INV-54 ×5 → same targets as files 26/27. Mutant sources not pinned per row
("fresh … mutant" prose) → per-mutant re-execution needs new instruments
(see §6).

File 22 (t62 main): 9 INV-62 table rows (F-01/F-04/E-DECISION appear only in
prose, not as table rows — no F-row states to transcribe; stated so none is
sought). KILLED (ONE-STORE/PAYLOAD-ONLY/ONE-KEY): audit ref `3a7b355a` is NOT
in the object DB and no candidate is recorded → provenance for re-verification
unrecoverable → UNESTABLISHED-REUSE (evidence text preserved in archive, nothing
disproved). OPEN (HISTORICAL/DIRECT-ONLY/ATOMIC-HOOK/V3-BASE/CLOSED-SUMS/
PROOF-TRUST, all "pending S62-B/C"): open obligations. For DIRECT-ONLY/
ATOMIC-HOOK/V3-BASE/CLOSED-SUMS the awaited S62-B vocabulary is demonstrably
present at this base (sealed `DirectCommand`/`BaseMutation`/`BaseChange` sums
read in `KelGroups/Event.lean`) → these four are STALE-DEMONSTRATED in the
narrow sense that their blocker is resolvable now (re-key to `KelGroups.*`
substrate rows owed); HISTORICAL/PROOF-TRUST stay UNESTABLISHED-REUSE.

Files 15–21, 23 (t62 archived auditor/owner): decision/SHA prose refs, no mutant
rows → UNESTABLISHED-REUSE as records (no theorem subject to re-key).

**1C. Non-Lean-mutant files (no production-definition kill; disposition per file).**
Files 3/6/7 (t48-owner-codex TSVs): 18 rows each, all OPEN, evidence NONE —
open obligations, UNESTABLISHED-REUSE (nothing recorded, nothing disproved).
Files 4/5 (docs M1–M8): doc-mutant receipts with full 64-hex receipt hashes and
byte-exact restore (e.g. M1 `eb1261e1…`) — design-record controls, wrong subject
for S3 D2 → UNUSABLE for D2 (retained as design evidence for #71). File 8
(I10-*, all OPEN "pending … mutant"): open obligations, UNESTABLISHED-REUSE.
File 9 (A1–A4, all OPEN): process/contract rows, UNUSABLE for D2. Files 10/11
(composition EVENT-*/VERDICT-*/PRODUCTION-*/ROUTE-THRESHOLD, all KILLED):
production-trace rows predating the `Composition.route` split — subjects
renamed since → STALE-DEMONSTRATED, re-key to
`Reactivegas.Composition.*` + `voteApply` rows owed. Files 13/14 (t59
I-FENCE-*/I-R1-*/M-*/E-*): fence/citation rows, some KILLED with hashes, subjects
are docs/debt, not theorems → UNUSABLE for D2. File 28 (t74 E-CLOSED/E-ONLY):
Haskell subject → UNUSABLE for D2. File 35 (release I001 KILLED-S001,
C002 DEFERRED-S002): release controls; C002's DEFERRED stays visible as
deferred, not dropped → UNUSABLE for D2. File 36 (batch1 12 INV-*, KILLED):
simulator subject → UNUSABLE for D2. Files 37–41 (fable GEO/IT/LINK/E-/NOTE-):
simulator subject → UNUSABLE for D2. Files 1 (R2-CORE/SUBJECTS/CLICK-PARAMS
KILLED with `receipts/mutants.log`): simulator subject → UNUSABLE for D2. File
2 (prose, 0 rows): no receipts → nothing to mark. Files 42/43 (INV-TC-01..04,
OPEN/NONE): toolchain rows, UNUSABLE for D2 (pin `v4.25.0` noted as context).

**REUSABLE-BOUNDED: 0.** No receipt meets the bar at this base.

**REUSABLE-BOUNDED: 0.** No receipt meets the bar at this base.

## §2. Staleness evidence (demonstrated, not asserted)

- D1 checker rewrite: `git diff 4898e55e..3590c00 -- scripts/check-reactivegas-inversion-coverage` = 467 insertions/138 deletions; `git diff a408e09..3590c00 -- <same>` = 247+/73−; `TraceTests.lean` also changed in both. Covers files 31–34, 29.
- D2 quota retirement: `grep -rn "163" scripts/` finds no quota (only `.lake` C artifacts); `grep -rn "expectedDeclarations" scripts/` finds nothing. The t48 REGRESSION instrument (163/163 lexical count) has no counterpart at this base. Covers INV-48-I-REGRESSION.
- D3 vote re-cut: `git diff 757dac98..3590c00 -- lean/KelGroups/Vote/Fold.lean lean/KelGroups/Vote/Invariants.lean lean/Reactivegas/Invariants.lean` = 2021 insertions/517 deletions. Covers files 26/27/30.
- D4 substrate re-cut: `git diff bb3ac41..3590c00 -- lean/KelGroups/Vote/ lean/Reactivegas/Step.lean` = 1164 insertions/736 deletions across 7 files. Covers file 12.
- D5 vocabulary removal: current `KelGroups/Vote/Event.lean` offers only `openQuestion`/`cast`/`renounce` (read); the archived FRANCHISE/NOEXPIRY instruments use `.admitMember`/`.setRoles` (read in `franchise-unfranchised-recast-mutant.lean`). Fixtures cannot elaborate at this base. Covers INV-54-FRANCHISE/NOEXPIRY.
- Toolchain pins as recorded: t57 Lean 4.25.0 (matches base pin — no toolchain delta claimed for file 12); t54 mixed 4.25/4.27 runs (4.27 legs differ from the base pin — noted, not relied on).
- Where no demonstration exists (unbound footprints, refs not in the object DB such as `3a7b355a`), rows are marked UNESTABLISHED-REUSE or UNRECOVERABLE as tabulated — never STALE.

## §3. Full finite row map (239 qualified rows; relation with per-row rationale)

D1a/D1b extents per R2 (239 qualified identities in `P1A-qualified-classified.txt` `ef93b9…`; 27 modules; guard/effect/error axis; rev-3 table verified with zero disagreement) are incorporated by reference and stand. The R2 ownership relation is corrected here:

- Family A (14 inversions): per-constructor ownership stands as tabulated in R2 (all 14 `hstep` literals verified by grep). The donate control (Build 2) establishes only `Reactivegas.step_donate_inv` ↔ G-donate.
- Family B cross-cutting: `conservation_preserved` + `step_authorized` own all 14 guard/effect pairs per-constructor (14 + 14 proof arms counted). CORRECTED: `solvent_preserved`, `reach_solvent`, `not_insolvent_of_reach`, `comune_not_a_member_of_reach` (+ 3 private step-lemmas) have no per-constructor split (0 match arms; hypotheses are `Reach` + arbitrary-`e`). Their ownership is DEFERRED with reason (per-atom fund-sensitivity needs kill evidence; fund-neutral constructors provisionally out of scope but the row stays on the map). All other Family B rows (guard lemmas, escrow/double-entry, pledge, governance) stand as tabulated — each conclusion names its guard/effect.
- Family C (30 vote rows): vote atoms only; `sweepClosures_idempotent` needs no guard atom (equation rationale); `no_expiry` stays single-question scope. Stands.
- Family D (substrate + 7 root aliases): aliases inherit their counterpart's atoms; both spellings stay listed (different signatures; a kill of one need not kill the other). Stands.
- Family E (composition/trace/witnesses): per-check ownership stands; `*_mutant_caught` rows keep the dual reading (same atoms + checker-sensitivity). `TraceTests` mirrors stay listed separately. Stands.
- 81 helper rows: owned by NO atom (satisfiable-hypotheses obligation, §4). Not tabulated as pairs — no manufactured pairings.
- DEFERRED identities (visible, never dropped): the 7 solvent/`Reach` rows above + C002 (release, file 35) + 7 root-alias second spellings where kill-independence is unproven. Each carries its reason here.

**Ownership annex (per-row; rationale read from each statement's hypotheses/conclusion, never the name).**

*Family A — 14 inversions (all 14 `hstep` constructor literals verified by grep):*

| row | owns | rationale |
|---|---|---|
| `step_grant_inv` (root) | G-grantPermission + E-permit-flag | `hstep` names `.grantPermission a c`; collection found + `isResponsabile` + permitted-flag equation (excerpt verified) |
| `step_deny_inv` (root) | G-denyPermission + E-refund-all | `hstep` names `.denyPermission a c`; refund-all + collection-removal equation |
| `step_pledge_inv` (root) | G-pledge + E-escrow-debit | `hstep` names `.pledge a u c v`; escrow-debit equation |
| `step_accept_inv` (root) | G-acceptPledge + E-pending-to-accepted | `hstep` names `.acceptPledge a u c`; move equation |
| `step_refuse_inv` (root) | G-refusePledge + E-refund-pending | `hstep` names `.refusePledge a u c`; refund equation |
| `step_correct_inv` (root) | G-correctPledge + E-difference | `hstep` names `.correctPledge a u c v'`; settlement equation |
| `step_close_inv` (root) | G-closePurchase + E-referente-spend | `hstep` names `.closePurchase a c`; spend equation |
| `step_fail_inv` (root) | G-failPurchase + E-refund-all | `hstep` names `.failPurchase a c`; refund equation |
| `Reactivegas.step_open_inv` | G-openPurchase + E-open-collection | `hstep` names `.openPurchase a c`; opening equation (excerpt verified) |
| `Reactivegas.step_deposit_inv` | G-deposit + E-double-entry | `hstep` names `.deposit a u v`; double-entry equation (excerpt verified) |
| `Reactivegas.step_withdraw_inv` | G-withdraw + E-double-entry + non-stalled | `hstep` names `.withdraw a u v`; equation with `stalled` conjunct (excerpt verified) |
| `Reactivegas.step_transferCassa_inv` | G-transferCassa + E-cassa-move | `hstep` names `.transferCassa a f v`; move equation (excerpt verified) |
| `Reactivegas.step_donate_inv` | G-donate + E-comune-credit | `hstep` names `.donate a v`; comune equation (excerpt + Build-2 RED verified) |
| `Reactivegas.step_backdonate_inv` | G-backdonate + E-equal-share + auth atom | `hstep` names `.backdonate a w`; equation with `auth s w` conjunct (excerpt verified); SOLE owner of the backdonate-auth atom |

*Family B — economic cross-cutting:*

| row | owns | rationale |
|---|---|---|
| `conservation_preserved` | all 14 G/E per-constructor | arbitrary-`e` + `conservation` hyps; 14 proof arms counted |
| `step_authorized` | all 14 G per-constructor | arbitrary-`e` success implies `authorizedStep`; 14 arms counted |
| `solvent_preserved`, `reach_solvent`, `not_insolvent_of_reach`, `comune_not_a_member_of_reach`, `credit_pledges_of_reach` (private), `comune_not_a_member_step` (private), `credit_pledges_step` (private) | DEFERRED (0 match arms; `Reach` + arbitrary-`e` hyps; per-atom fund-sensitivity needs kill evidence) | rows stay on the map |
| `pledge_guard_inv` | G-pledge | conclusion over pledge-guard conjuncts |
| `auth_referente_guard_inv` | G-acceptPledge + G-refusePledge | referente-equality conjuncts |
| `close_guard_inv` | G-closePurchase | closure-guard conjuncts |
| `fail_guard_inv` | G-failPurchase | failure-guard conjuncts |
| `pledge_escrow_debit` | G-pledge + E-escrow | escrow-debit equation |
| `deposit_double_entry` | G-deposit | double-entry equation |
| `withdraw_double_entry` | G-withdraw | double-entry equation |
| `close_spends_referente` | G-closePurchase | referente-spend equation |
| `close_permission_to_close` | G-closePurchase + G-grantPermission | permission predicate over close/grant |
| `pledge_rejected_when_member` | G-pledge | member-pledge refusal |
| `pledge_preserves_allUnique` | G-pledge | uniqueness preservation |
| `governance_enacts_windUpAdmin` | governance effect | `governanceEnacts u (windUpAdmin s u)` (excerpt verified) |
| `solvent_init` | none (boot proposition) | `solvent view State.empty`; constrains boot, no guard atom |
| `stepDetailed_erases` (root) | none (wrapper equation) | `eraseDiagnostic` after `stepDetailed` equals `stepEvent` (`cases h:` shape verified) |

*Family C — 37 vote rows (`KelGroups.Vote.*`; no `step`/`Event` in any statement — economic guards never own these):*

| row | owns | rationale |
|---|---|---|
| `emptyVoteState_sweepReady`, `emptyVoteState_wellFormed`, `sweepClosures_sweepReady` (private), `sweepClosures_wellFormed` (private), `effectedState_sweepReady` (private), `applyVoteEvent_preserves_wellFormed`, `foldFrom_preserves_wellFormed` (private), `foldVote_wellFormed` | V-sweep + V-open/V-cast/V-renounce | conclusions are `SweepReady`/`VoteWellFormed` over sweep/fold outputs (hypotheses name `sweepClosures`/`foldVote`/`effectedState`) |
| `ballots_nodup_disjoint`, `open_questions_are_open`, `questions_partition` (fold-output conjunctions verified), `sweepStep_of_open`, `sweep_filterMap_of_swept`, `filter_open_idem`, `sweepClosures_idempotent` (self-composition equation verified), `sweepDuplicating_duplicates` | V-sweep + V-tally | statements equate or partition sweep/fold outputs; `sweepClosures_idempotent` needs no guard atom — the equation is the rationale |
| `inadmissible_is_noop` (verified `VoteEvent` + `VoteError` args), `nonresponsabile_event_noop`, `unfranchised_cast_noop` | V-franchise + the named VoteEvent (open/cast/renounce per statement) | each hypothesis is the named inadmissibility condition over a `VoteEvent` |
| `franchise_of_tallies`, `tally_keys_franchised_from` (private), `tallyKeysOfState_erased_le` (private), `tallyKeysOfState_insert_cases` (private), `effectedState_tally_growth` (private), `verdictOf_threshold_congr` (threshold-congruence hypothesis verified), `sweepStep_key`, `sweepClosures_open_mem`/`closed_mem` (private), `sweepClosures_preserves_qid`/`effectedState_preserves_qid`/`applyVoteEvent_preserves_qid`/`foldFrom_preserves_qid` (private), `foldVote_append` (private), `placeBallot_clean`/`placeBallot_tally` (private), `sweepClosures_tallyKeys` (private) | V-tally / V-threshold / V-sweep as named in each conclusion | conclusions are about tallies, verdicts, or question-ids over vote state; private lemmas support the authored rows above |
| `no_expiry` | V-cast on one distinct question ONLY | statement covers a cast on one question (t54 C5 evidence); not member events, not the event list — scope stated, not widened |

*Family D — 31 substrate rows + 7 root aliases (`KelGroups.*`; base/validate/integration atoms only):*

| row | owns | rationale |
|---|---|---|
| `emptyState_wellFormed`, `approvePending_wellFormed` (private), `foldEvents_preserves_wellFormed` (private), `enact_preserves_wellFormed`, `finishEnact_preserves_wellFormed`, `tryEnact_preserves_wellFormed`, `applyPropose_preserves_wellFormed`, `applyApprove_preserves_wellFormed`, `applyEvent_preserves_wellFormed`, `foldGroup_wellFormed` | B-propose / B-approve / B-enact + W-coherence | each hypothesis names the fold/enact function whose output must satisfy `WellFormed` |
| `tryEnact_eq_of_enacts`, `tryEnactDetailed_enactment_threshold_met` (private), `enact_implies_threshold_met`, `members_change_implies_enacted`, `member_key_coherent`, `approvals_nodup`, `proposer_mem_approvals`, `majority_table`, `majority_not_strict_on_even` (+ 7 root aliases of the same short names, each calling its counterpart — `:881` pattern verified) | B-enact / B-approve / W-coherence as named | threshold/majority/membership statements over `GroupState`; aliases inherit counterpart atoms; both spellings stay listed (different signatures — a kill of one need not kill the other) |
| `validateDirectAdmission_ok`, `direct_admission_requires_admin`, `non_admin_admission_is_noop`, `membership_growth_is_direct_admission` | B-admit | conclusions name `validateDirectAdmission`/admission outcomes |
| `commitBaseChange_ok`, `commitBaseChange_members`, `tryEnactBase_runs_hook`, `tryEnactBase_preserves_absence`, `enactMutation_preserves_absence`, `base_change_runs_hook` | B-mutate / B-hook | conclusions name `commitBaseChange`/`tryEnactBase`/`enactMutation`/hook outputs |
| `app_event_preserves_members`, `app_event_has_no_base_change` | B-hook (negatively: no base change) | conclusions state app events leave membership/base-change untouched |

*Family E — composition, trace, witnesses (3 + 1 + 12 + 22):*

| row | owns | rationale |
|---|---|---|
| `Reactivegas.Composition.voteDerived_iff_not_direct` (verified `route`/`voteDerived` equations), `baseEnacted_threshold_met`, `appDecided_verdict_exhaustive` | route / vote-derived / threshold atoms | equations over `Event` routing, not guards |
| `Reactivegas.baseHook_votes` (verified `baseHook … = .ok s'` implies `s'.votes = sweepClosures …`), `base_change_recomputes_votes` | B-hook + V-sweep | hook-output equations |
| `Reactivegas.*_holds` witnesses (`base_departure_applies_cleanup`, `base_change_can_close_without_ballot`, `direct_admission_only_holds`, `base_recompute_reachable_holds`, `sweep_idempotent_witness`, `sweep_idempotent_mutant_caught`, `integrated_theorem_witness_holds`, `canonical_economy_holds`, `exhaustive_inventories_hold`, `i57_boundary/exhaustive/noop/auth/r45/partition/disjoint/nostale/franchise/policyfree/noexpiry_holds`, `i57_disjoint/franchise/policyfree_mutant_caught`, `admissionPreservation_holds`, `productionWellFormed_holds`, `comuneThresholdSanity_holds`, `comune_cannot_authorize`, `app_members_preservation_holds`, `app_members_preservation_mutant_caught`) | the atoms each named `check*` evaluates (sweep → V-sweep; admission → B-admit; etc.) | `checkX = true by decide` shape; `*_mutant_caught` rows add the checker-sensitivity reading (survival of the surrounding check = assurance gap, never a production kill) |
| `TraceTests.*` mirrors (7 check mirrors + `all_checks_pass` + `frozen_checks/inventory/corpus_faithful`) | same atoms as the `Reactivegas.*` counterpart with the same short name | same `check*` subject under the `TraceTests` namespace; listed separately, never merged |

## §4. Helper satisfiability — own static witnesses, separated from footprints (Gap 4)

The R2 double duty is withdrawn: P2-a-style footprint checks discharge nothing here. Each helper's hypotheses are exhibited with concrete values; definition bodies cited were read at this base (`Types.lean:46–63`: `assocLookup []=none`, `assocErase []=[]`, `assocAdjust []=[]`, `setInsert` contains-cons; `State.lean`: `bal []=0`, `splitUser []=none`, `pullCollection []=none`, `stripCollections []=([],[])`, `refundAll m []=m`; `Step.lean:24`: `demand`; `Predicates.lean:40`: `uniquePledges` vacuous on empties; `List.nodup_nil` used in-tree at `Vote/Invariants.lean:576`).

| group | witness (hypotheses → values) | rows covered (all 81; per-row preserved) |
|---|---|---|
| H-none (`assocLookup_*_of_none` ×3, `not_mem_users_of_splitUser_none`, `user_absent_of_any_false`, `closed_guard_absent`, `refundAll_bal_ge[_lemma]`, `stripCollections_*` ∀-rows, `sumBal_foldl_bump`, `bal_foldl_bump_ge`, `nodup_append_mem`, `assoc_entries_key_unique`, `filterMap_keys_nodup` with `f=fun _=>none` (hid vacuous), `uniquePledges_pend_cons` with empty accepted/pending (hu/hun vacuous), `stripCollections_referente[_ne]`, `sumPledges_append`, `refundAll_sum`, `bal_cons`, `bump_sum`, `sumBal_cons`, `escrowSum_cons`, `stripCollections_sum`, `option_bind_inv` is H-some below) | `entries/l/records = []` (any-condition holds vacuously; `setInsert`/`bump`/`foldl` nil-cases as cited) | `KelGroups.assocLookup_erase_of_none/adjust_of_none/insert_of_none/insert_self`, `KelGroups.Vote.assocErase_sublist'/keys_nodup'/key_absent'/assocAdjust_keys'/keys_nodup'/nodup_append_mem/nodup_erase/closed_guard_absent/assoc_entries_key_unique/filterMap_keys_nodup/setInsert_nodup'`, `KelGroups.assocErase_sublist/keys_nodup/key_absent`, `not_mem_users_of_splitUser_none`, `user_absent_of_any_false`, `refundAll_bal_ge[_lemma]`, `stripCollections_sublist[_lemma]/amount_lemma/sum/referente[_ne]`, `sumBal_foldl_bump`, `bal_foldl_bump_ge`, `sumPledges_append`, `refundAll_sum`, `sumBal_cons`, `bal_cons`, `bump_sum`, `escrowSum_cons`, `uniquePledges_pend_cons`, `option_bind_inv` excluded (see H-some) |
| H-some (`assocLookup_some_mem[_']`, `mem_assocLookup_some'`, `splitUser_*` (6), `pullCollection_*` (8: `mem_lemma/det/id[_lemma]/sum[_lemma]/sublist[_lemma]/mem`), `refundAll` n/a, `option_bind_inv`, `setInsert_mem[_cases]`, `mem_erase_inv`, `assocInsert_mem_cases`) | `entries=[(k,v)]` / `l=[⟨u,v⟩]` (Pledge `⟨user,amount⟩` shape read in `Step.lean`) / `cols=[x]` with `x=⟨c,"k",false,[],[]⟩` (Collection shape read in `Step.lean:80`); `o=some ()`, `values=[existing]`, `l=[b]` with `a≠b` literals | `KelGroups.assocLookup_some_mem`, `KelGroups.Vote.assocLookup_some_mem'/mem_assocLookup_some'`, `splitUser_sum[_lemma]/sublist[_lemma]/amount[_lemma]`, `pullCollection_mem[_lemma]/det/id[_lemma]/sum[_lemma]/sublist[_lemma]`, `option_bind_inv`, `setInsert_mem/mem_cases`, `mem_erase_inv`, `assocInsert_mem_cases` |
| H-neq (`assocErase_other_lookup`, `assocInsert_other_lookup`, `mem_map_fst_erase_of_ne`, `bal_bump_ne[_lemma]`) | distinct literals (`"a"≠"b"` by decide) + `entries=[]` | as listed |
| H-bool (`bool_not_true`:`b=false`; `bool_and_left/right`:`b₁=b₂=true`; `demand_eq_true_of_some`:`b=true`; `demand_none_of_ne_true`:`b=false`; `eq_nil_of_isEmpty`:`l=[]`) | literals as shown | as listed |
| H-prop (`assocErase/Insert/Adjust_property`) | `entries=[]`, `property=fun _ _ => True` (h/hnew/hold/hchange vacuous-or-True) | as listed |
| H-mem-cons (`unique_mem_cons_inv`: `acc=pend=[]`, `p=q=⟨u,v⟩`; `setInsert_nodup[_']`: `values=[]`; `mem_map_fst_insert`: `entries=[]`) | as shown | as listed |

Count check: H-none 40 + H-some 24 + H-neq 5 + H-bool 5 + H-prop 3 + H-mem-cons 4 = 81 = all helper rows. Zero execution used; the footprint operation is nowhere relied on above.

**Literal per-row roster (every helper qualified identity, grep-able; witness group in brackets):** `KelGroups.assocAdjust_keys` [H-none], `KelGroups.assocAdjust_property` [H-prop], `KelGroups.assocErase_key_absent` [H-none], `KelGroups.assocErase_keys_nodup` [H-none], `KelGroups.assocErase_property` [H-prop], `KelGroups.assocErase_sublist` [H-none], `KelGroups.assocInsert_keys_nodup` [H-none], `KelGroups.assocInsert_property` [H-prop], `KelGroups.assocLookup_adjust_of_none` [H-none], `KelGroups.assocLookup_erase_of_none` [H-none], `KelGroups.assocLookup_insert_of_none` [H-none], `KelGroups.assocLookup_insert_self` [H-none], `KelGroups.assocLookup_some_mem` [H-some], `KelGroups.setInsert_mem` [H-some], `KelGroups.setInsert_nodup` [H-mem-cons], `KelGroups.Vote.assocAdjust_keys'` [H-none], `KelGroups.Vote.assocAdjust_keys_nodup'` [H-none], `KelGroups.Vote.assocAdjust_property` [H-prop], `KelGroups.Vote.assocErase_key_absent'` [H-none], `KelGroups.Vote.assocErase_keys_nodup'` [H-none], `KelGroups.Vote.assocErase_other_lookup` [H-neq], `KelGroups.Vote.assocErase_property` [H-prop], `KelGroups.Vote.assocErase_sublist'` [H-none], `KelGroups.Vote.assocInsert_keys_nodup'` [H-none], `KelGroups.Vote.assocInsert_mem_cases` [H-some], `KelGroups.Vote.assocInsert_other_lookup` [H-neq], `KelGroups.Vote.assocInsert_property` [H-prop], `KelGroups.Vote.assocLookup_some_mem'` [H-some], `KelGroups.Vote.mem_assocLookup_some'` [H-some], `KelGroups.Vote.mem_map_fst_erase_of_ne` [H-neq], `KelGroups.Vote.mem_map_fst_insert` [H-mem-cons], `KelGroups.Vote.setInsert_mem_cases` [H-some], `KelGroups.Vote.setInsert_nodup'` [H-mem-cons], `KelGroups.Vote.nodup_append_mem` [H-none], `KelGroups.Vote.mem_erase_inv` [H-some], `KelGroups.Vote.nodup_erase` [H-none], `KelGroups.Vote.closed_guard_absent` [H-none], `KelGroups.Vote.assoc_entries_key_unique` [H-none], `KelGroups.Vote.filterMap_keys_nodup` [H-none], `bal_bump` [H-none], `bal_bump_ne_lemma` [H-neq], `bal_cons` [H-none], `bool_and_left` [H-bool], `bool_and_right` [H-bool], `bool_not_true` [H-bool], `bump_sum` [H-none], `demand_eq_true_of_some` [H-bool], `demand_none_of_ne_true` [H-bool], `eq_nil_of_isEmpty` [H-bool], `not_mem_users_of_splitUser_none` [H-none], `option_bind_inv` [H-some], `pullCollection_det` [H-some], `pullCollection_id` [H-some], `pullCollection_id_lemma` [H-some], `pullCollection_mem` [H-some], `pullCollection_mem_lemma` [H-some], `pullCollection_sublist` [H-some], `pullCollection_sublist_lemma` [H-some], `pullCollection_sum` [H-some], `pullCollection_sum_lemma` [H-some], `refundAll_bal_ge` [H-none], `refundAll_bal_ge_lemma` [H-none], `refundAll_sum` [H-none], `splitUser_amount` [H-some], `splitUser_amount_lemma` [H-some], `splitUser_sublist` [H-some], `splitUser_sublist_lemma` [H-some], `splitUser_sum` [H-some], `splitUser_sum_lemma` [H-some], `stripCollections_amount_lemma` [H-none], `stripCollections_referente` [H-none], `stripCollections_referente_ne` [H-none], `stripCollections_sublist` [H-none], `stripCollections_sublist_lemma` [H-none], `stripCollections_sum` [H-none], `sumBal_cons` [H-none], `sumPledges_append` [H-none], `unique_mem_cons_inv` [H-mem-cons], `uniquePledges_pend_cons` [H-none], `user_absent_of_any_false` [H-none], `bal_bump_ne` [H-neq], `escrowSum_cons` [H-none], `sumBal_foldl_bump` [H-none], `bal_foldl_bump_ge` [H-none]. (Each qualified identity appears exactly once in `P1A-qualified-classified.txt`, which governs.)

## §5. Cost model (unfunded arithmetic, not an authorized campaign)

Nothing here authorizes execution. Unit costs are the layer-corrected observations (cold 19 s observed-once/log-lost; incremental RED 10 s logged; incremental GREEN 3 s logged; check-elaboration 11 s; module-elaboration+`#eval` 2 s — each at its own layer, never averaged). Scaling over the repaired counts is shown as arithmetic:

- 158 authored rows ≈ 14 inversion + 8 solvent-deferred + 30 vote + 24 substrate + 18 composition/witness + 64 guard/preservation rows. At one admitted single-atom build per relevant mutant (≈12 vote + ≈10 substrate + 20 inversion-family mutants) plus final acceptance (2): ≈44 builds IF every mutant is needed — presented as an upper-bound envelope, not a plan, and not authorized.
- 81 helper rows: $0 execution — satisfiability exhibited statically in §4.
- STALE receipt re-keying: per-receipt elaboration checks are the request in §6, not a grant.
- Batching savings: unmeasured, not subtracted. The mandate is not narrowed to fit any number above.

## §6. Genuinely unavoidable execution (requests with exact argv, ungranted)

- G-B1 compiler-generated enumeration: argv `PATH=/nix/store/4gr3n4nrp0xxgykyyzdxi3xjj2ikn5x1-lean4-4.25.0/bin:$PATH lake build Reactivegas.TraceTests Reactivegas.CorpusExport Reactivegas.CorpusGate` (cwd `lean/`, driver: seat shell), then the gate's elaboration as shipped in `scripts/check-lean-axioms` (unmodified). Expected observable: 3 oleans + `axiom-theorems walkOcc/distinct/fold` lines + `axiom-gate: ok`. Classification: 1 substantive build + 1 elaboration. Cost: ≈19 s + elaboration. UNGRANTED. (The lost cold log is NOT re-run: G-B2 is withdrawn — an honest limit stated is worth more than a build spent erasing it.)
- G-B3 STALE re-verification elaborations: argv `PATH=<same> lake env lean <family-check-file>` per receipt family (12 families), driver: seat shell. Expected observable: per-family RED (kill reproduced) or GREEN-with-diff (survival = finding). Classification: elaborations (0 builds). Cost: ≈11 s each worst case. UNGRANTED.

AUTHORSHIP ROSTER (158 authored qualified identities; ownership: annex Families A-E above; helper roster in §4; machine-readable classification governs):
approvals_nodup auth_referente_guard_inv close_guard_inv close_permission_to_close close_spends_referente comune_not_a_member_of_reach comune_not_a_member_step conservation_preserved credit_pledges_of_reach credit_pledges_step deposit_double_entry enact_implies_threshold_met fail_guard_inv governance_enacts_windUpAdmin KelGroups.app_event_has_no_base_change KelGroups.app_event_preserves_members KelGroups.applyApprove_preserves_wellFormed KelGroups.applyEvent_preserves_wellFormed KelGroups.applyPropose_preserves_wellFormed KelGroups.approvals_nodup KelGroups.approvePending_wellFormed KelGroups.base_change_runs_hook KelGroups.commitBaseChange_members KelGroups.commitBaseChange_ok KelGroups.direct_admission_requires_admin KelGroups.emptyState_wellFormed KelGroups.enact_implies_threshold_met KelGroups.enactMutation_preserves_absence KelGroups.enact_preserves_wellFormed KelGroups.finishEnact_preserves_wellFormed KelGroups.foldEvents_preserves_wellFormed KelGroups.foldGroup_wellFormed KelGroups.majority_not_strict_on_even KelGroups.majority_table KelGroups.member_key_coherent KelGroups.members_change_implies_enacted KelGroups.membership_growth_is_direct_admission KelGroups.non_admin_admission_is_noop KelGroups.proposer_mem_approvals KelGroups.tryEnactBase_preserves_absence KelGroups.tryEnactBase_runs_hook KelGroups.tryEnactDetailed_enactment_threshold_met KelGroups.tryEnact_eq_of_enacts KelGroups.tryEnact_preserves_wellFormed KelGroups.validateDirectAdmission_ok KelGroups.Vote.applyVoteEvent_preserves_qid KelGroups.Vote.applyVoteEvent_preserves_wellFormed KelGroups.Vote.ballots_nodup_disjoint KelGroups.Vote.effectedState_preserves_qid KelGroups.Vote.effectedState_sweepReady KelGroups.Vote.effectedState_tally_growth KelGroups.Vote.emptyVoteState_sweepReady KelGroups.Vote.emptyVoteState_wellFormed KelGroups.Vote.filter_open_idem KelGroups.Vote.foldFrom_preserves_qid KelGroups.Vote.foldFrom_preserves_wellFormed KelGroups.Vote.foldVote_append KelGroups.Vote.foldVote_wellFormed KelGroups.Vote.franchise_of_tallies KelGroups.Vote.inadmissible_is_noop KelGroups.Vote.no_expiry KelGroups.Vote.nonresponsabile_event_noop KelGroups.Vote.open_questions_are_open KelGroups.Vote.placeBallot_clean KelGroups.Vote.placeBallot_tally KelGroups.Vote.questions_partition KelGroups.Vote.sweepClosures_closed_mem KelGroups.Vote.sweepClosures_idempotent KelGroups.Vote.sweepClosures_open_mem KelGroups.Vote.sweepClosures_preserves_qid KelGroups.Vote.sweepClosures_sweepReady KelGroups.Vote.sweepClosures_tallyKeys KelGroups.Vote.sweepClosures_wellFormed KelGroups.Vote.sweepDuplicating_duplicates KelGroups.Vote.sweep_filterMap_of_swept KelGroups.Vote.sweepStep_key KelGroups.Vote.sweepStep_of_open KelGroups.Vote.tally_keys_franchised_from KelGroups.Vote.tallyKeysOfState_erased_le KelGroups.Vote.tallyKeysOfState_insert_cases KelGroups.Vote.unfranchised_cast_noop KelGroups.Vote.verdictOf_threshold_congr majority_not_strict_on_even majority_table member_key_coherent members_change_implies_enacted not_insolvent_of_reach pledge_escrow_debit pledge_guard_inv pledge_preserves_allUnique pledge_rejected_when_member proposer_mem_approvals reach_solvent Reactivegas.admissionPreservation_holds Reactivegas.app_members_preservation_holds Reactivegas.app_members_preservation_mutant_caught Reactivegas.base_change_can_close_without_ballot Reactivegas.base_change_recomputes_votes Reactivegas.base_departure_applies_cleanup Reactivegas.baseHook_votes Reactivegas.base_recompute_reachable_holds Reactivegas.canonical_economy_holds Reactivegas.Composition.appDecided_verdict_exhaustive Reactivegas.Composition.baseEnacted_threshold_met Reactivegas.Composition.voteDerived_iff_not_direct Reactivegas.comune_cannot_authorize Reactivegas.comuneThresholdSanity_holds Reactivegas.direct_admission_only_holds Reactivegas.exhaustive_inventories_hold Reactivegas.i57_auth_holds Reactivegas.i57_boundary_holds Reactivegas.i57_disjoint_holds Reactivegas.i57_disjoint_mutant_caught Reactivegas.i57_exhaustive_holds Reactivegas.i57_franchise_holds Reactivegas.i57_franchise_mutant_caught Reactivegas.i57_noexpiry_holds Reactivegas.i57_noop_holds Reactivegas.i57_nostale_holds Reactivegas.i57_partition_holds Reactivegas.i57_policyfree_holds Reactivegas.i57_policyfree_mutant_caught Reactivegas.i57_r45_holds Reactivegas.integrated_theorem_witness_holds Reactivegas.productionWellFormed_holds Reactivegas.step_backdonate_inv Reactivegas.step_deposit_inv Reactivegas.step_donate_inv Reactivegas.step_open_inv Reactivegas.step_transferCassa_inv Reactivegas.step_withdraw_inv Reactivegas.sweep_idempotent_mutant_caught Reactivegas.sweep_idempotent_witness solvent_init solvent_preserved step_accept_inv step_authorized step_close_inv step_correct_inv step_deny_inv stepDetailed_erases step_fail_inv step_grant_inv step_pledge_inv step_refuse_inv TraceTests.all_checks_pass TraceTests.app_members_preservation_holds TraceTests.app_members_preservation_mutant_caught TraceTests.base_change_can_close_without_ballot TraceTests.base_departure_applies_cleanup TraceTests.base_recompute_reachable_holds TraceTests.direct_admission_only_holds TraceTests.frozen_checks_faithful TraceTests.frozen_corpus_faithful TraceTests.frozen_inventory_faithful TraceTests.sweep_idempotent_mutant_caught TraceTests.sweep_idempotent_witness withdraw_double_entry 
## Reconciliation (no coverage claimed)

- `DISCOVERED`: 239 qualified identities + 27 modules + axis (§3; R2 extents incorporated).
- `REQUIRED-INPUT`: same 239, zero exemptions; relevant pairs per §3 (no Cartesian product); DEFERRED rows listed visibly.
- `EXECUTED`/`KILLED`/`SURVIVED`/`BLOCKED`: not claimed. Build-2 RED stays costing evidence only.
- Every identity: kill pending (unfunded §5), helper-satisfiability exhibited (§4), or DEFERRED with reason (§3). Nothing absorbed, nothing covered, nothing dropped.

*End of R3. History preserved (original + R2). Packet: this file + `P1A-qualified-inventory.txt` (`efdeb3…`) + `P1A-qualified-classified.txt` (`ef93b9…`) + prior retained evidence. Auditor of another family inherits nothing else.*
