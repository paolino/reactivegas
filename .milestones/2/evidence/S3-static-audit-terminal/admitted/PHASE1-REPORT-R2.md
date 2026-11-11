# S3 Phase 1 report — REVISION R2 (original submission retained)

Local only. No push, PR action, comment, gist, publication, deployment or merge.
No `docs/en/design/` writes. No coverage claim. Phases 2..n unauthorized.

- Original submission: `handoffs/PHASE1-REPORT.md` sha256
  `dbc2cb681ea92c19fc452411ec120a4a91b0ee5d23102dd0264f4c088a188192`
  — **preserved, not rewritten**. This R2 is the correction NOTE-003 required.
- Base/seat/contract unchanged from the original §0: HEAD
  `3590c0015b84fd58004bf6fb44dd18b107304c48`, parent `d670323…`, tree
  `44a1f0b…`, PR #88 squash, #66 OPEN, seat PID/PGID `2401092`, contract rev 3.
- Worktree at R2: HEAD `3590c0015b84fd58004bf6fb44dd18b107304c48`,
  `git status --porcelain=v1` clean (`.lake/` ignored).

## Spend — explicit one-build overrun: 4 against ceiling 3 (Gap 5)

The Gap-5 ruling is accepted without retrofit. The standing campaign rule counts
failed whole-build invocations; the fourth invocation is confirmed below and
preserved as spent. A wrong-layer failure never counts as a kill; it counts as
spend.

| # | command (cwd) | wall | exit | substance |
|---|---|---|---|---|
| 1 | `lake build` (`lean/`, zero oleans) | 19 s | 0, 27 jobs | cold build — charged |
| 2 | `lake build` (`lean/`) after one-atom donate-guard variant | 10 s | 1, property failure at `step_donate_inv:407` | incremental RED — charged |
| 3 | `lake build` (`lean/`) after restoring `Step.lean` | 3 s | 0, 27 jobs | incremental GREEN restore — charged |
| 4 | `lake build` (**repo root**, no `lakefile` there) | ~1 s | 1, `no configuration file with a supported extension` | **no Lean compilation attempted — charged as spend per ruling** |

**State: 4 spent against a ceiling of 3 = one-build overrun.** No further build
of any kind is available. Gaps needing new invocations return exact costs below
before anything runs.

## Gap 1 — D1a repaired: 239 fully-qualified identities, every row classified

The original used 224 short source spellings, collapsing 15 repeats. Verified at
the accepted checkout: 7 pairs repeat **within** `KelGroups/Invariants.lean`
across the `namespace KelGroups … end` boundary (`:14`/`:872`) — the root alias
*_calls* the namespaced theorem with a different signature (e.g. `:877` root
`approvals_nodup {α : Type} …` calling `:881 `KelGroups.approvals_nodup``) — and
8 pairs mirror across `Reactivegas.*` vs `TraceTests.*` namespaces. My "mirror"
sentence covered only the 8 and mislocated the 7. Both stand corrected here.

Namespace mapping used (read from `^namespace`/`^end` lines, not assumed):

- `KelGroups/Invariants.lean`: `<872` → `KelGroups.*`; `>872` → root (7 aliases).
- `Reactivegas/Invariants.lean`: `338–426` and `1297–2355` → `Reactivegas.*`;
  else root. Hence the 14 `step_*_inv` split: 8 root (`grant`, `deny`,
  `pledge`, `accept`, `refuse`, `correct`, `close`, `fail` at :197–:322) + 6
  `Reactivegas.*` (`open`, `deposit`, `withdraw`, `transferCassa`, `donate`,
  `backdonate` at :343–:410).
- `Reactivegas/Step.lean`: `:445,448,470` → `Reactivegas.*`.
- `Reactivegas/Composition.lean` → `Reactivegas.Composition.*`;
  `KelGroups/Vote/Invariants.lean` → `KelGroups.Vote.*`;
  `KelGroups/Types.lean` → `KelGroups.*`; `Reactivegas/TraceTests.lean` →
  `TraceTests.*`; `Reactivegas/State.lean`, `Reactivegas/Trace.lean` → root.

Result: **239 occurrences → 239 distinct fully-qualified identities, zero
collisions.** The 15-pair resolution:

| short name | identity A | identity B |
|---|---|---|
| `approvals_nodup` etc. (7) | `KelGroups.*` (`:312,317,342,379,374,450,459`) | root `*` (`:877,883,889,899,909,914,923`), each calling A |
| `app_members_preservation_holds` / `*_mutant_caught` | `Reactivegas.*` (`Step.lean:445,448`) | `TraceTests.*` (`TraceTests.lean:1087,1090`) |
| `base_departure_applies_cleanup`, `base_change_can_close_without_ballot`, `direct_admission_only_holds`, `base_recompute_reachable_holds`, `sweep_idempotent_witness`, `sweep_idempotent_mutant_caught` | `Reactivegas.*` (`Invariants.lean:1583–1595`) | `TraceTests.*` (`TraceTests.lean:928–943`) |

**Classification (rule as in the original; applied to qualified rows):**
AUTHORED-STATEMENT 158 · HELPER-FACT 81 · private occurrences 76 (kept with
`yes` flag and `file:line` source mapping; a `private theorem` elaborates to an
internal `_private_…` constant — the source spelling plus location is the stable
Phase-1 identity, the internal index is environment-derived and travels with
limit L2). Helper rows keep their own obligation (satisfiable hypotheses); **no
state-machine antecedent is invented for any of them, and zero exemptions are
claimed** — all 81 stay in REQUIRED-INPUT, so no desk disposition is owed.

Machine-readable packet (part of this revision):
`handoffs/P1A-qualified-inventory.txt` (`efdeb3…`, 239 rows:
`qualified|private|file:line|short`) and
`handoffs/P1A-qualified-classified.txt` (`ef93b9…`, 239 rows with class first).
Every required row is classified there explicitly — no wildcard families.

**Excluded, actually accounted for (not gestured at):**

- 32 anonymous `example`s (no identity; cannot be killed by identity) — listed
  by `grep -c` per file in the original; re-verified count 32 at this base.
- All `def`/`abbrev` — not theorems; the D1b entry-point list (§P1-A-D1b of the
  original, verified against the rev-3 table with zero disagreement) is the
  positive account of them.
- Compiler-generated (`.eq_def`, `.eq_N`, `.injEq`, `sizeOf_spec`,
  `match_N.eq_N`, `Reach.below`, deriving output): **source grep over `lean/`
  for these patterns yields zero source declarations** — the excluded set is
  environment-only. Its enumeration needs one gate execution; that is gap G-B1
  below (1 substantive build + elaboration, unauthorized). No row is closed by
  narrowing past it.

## Gap 2 — P1-B repaired: exact rows per receipt, staleness demonstrated or marked unestablished

The original grouped 43 files into families with approximate counts. Required
here: exact rows with classification evidence and per-receipt
provenance/footprint status — holding the distinction that **unknown footprint
does not prove the context changed**. Markings used (mandate vocabulary kept):

- **STALE-DEMONSTRATED** — a concrete context change at/before this base touches
  the receipt's footprint (named), fresh run needed.
- **STALE-UNESTABLISHED** — footprint never bound in the ledger (no mutation /
  command / toolchain recorded); reuse unestablished, fresh run needed; *not*
  a demonstrated move. Still STALE in effect (needs a fresh run), never
  REUSABLE, never dismissed as disproved.
- **UNUSABLE** — subject identity unrecoverable or wrong subject for S3 D2.
- **REUSABLE-BOUNDED: 0.** Nothing meets the bar at this base.

Row-ID extraction per ledger (`grep -aoE` over the archive; full ID lists
retained in the analyst notes, states read from each ledger's own State/Row
state/Verdict column):

| # | ledger (archive-relative) | rows (recorded states) | binding assessment |
|---|---|---|---|
| 1–4 | `t48-inversions/campaign-ledger.md`, `.archived/commit-auditor-s48-i-s1`, `-s2`, `-recut1-s1/mutation-campaign-ledger.md` | 10 × INV-48-I-* each (s1: 4 KILLED FENCE/EVENT-SYNTAX/STEP-ITE/STEPEVENT-DELEGATE, 6 OPEN; s2 same shape) | STALE-DEMONSTRATED for guard/binding rows (gate rewritten: axiom-gate + resolved-olean ownership + unqualified-name resolution landed in #79/#88; `expectedDeclarations` quota removed) — re-key to the 14 `step_*_inv` qualified rows owed. Regression/axiom rows: STALE-UNESTABLISHED (evidence hashes `afdacbc8…` cite removed-run artifacts, footprint never bound). |
| 5–6 | `e-lean-compliance/…/inputs/t48-campaign-ledger.md`, `t54-campaign-ledger.md` | copies of INV-48-I (10) / INV-54 (6 + R-45) | copies, not fresh evidence — same marking as sources. |
| 7–8 | `e43/t54-vote-coverage/auditor-slice-a-s1`, `-s2` | INV-54-PARTITION/DISJOINT/NOSTALE/FRANCHISE/NOEXPIRY/POLICYFREE (s1: 3 KILLED, 3 OPEN; s2: 5 KILLED + NOEXPIRY OPEN) + R-45 (s2) | STALE-DEMONSTRATED for PARTITION/FRANCHISE/NOEXPIRY (theorem statements and vote fold moved since; `questions_partition`/`franchise_of_tallies` current shapes differ) — re-key to `KelGroups.Vote.*` qualified rows owed. KILLED rows: STALE-UNESTABLISHED (kill logs cite pre-#87 sources; dependency footprint never evidenced at `3590c00`). |
| 9–10 | `e43/t54/.archived/commit-auditor-s1`, `-s2` | R-1..R-29 + R-2b + E-PRED (s1: R-11 FAIL; s2 per its table) | STALE-UNESTABLISHED — gate-wiring rows (import/direction controls), subjects are pipeline properties, not theorem rows; no footprint bound. |
| 11 | `e43/.archived/t57-owner-codex/campaign-ledger.md` | INV-57-BOUNDARY/NOOP/AUTH/EXHAUSTIVE/NOEXPIRY + INV-54 ×5, all KILLED (fresh mutants named) | STALE-UNESTABLISHED — fresh at its base, but footprint (mutant source + command + toolchain) not bound to `3590c00` modules; re-key to vote qualified rows owed, not assumed. |
| 12 | `e43/.archived/t62-owner-codex/campaign-ledger.md` | INV-62-*: ONE-STORE/PAYLOAD-ONLY/ONE-KEY KILLED; HISTORICAL/DIRECT-ONLY/ATOMIC-HOOK/V3-BASE/CLOSED-SUMS/PROOF-TRUST OPEN | STALE-DEMONSTRATED for DIRECT-ONLY/ATOMIC-HOOK/V3-BASE/CLOSED-SUMS (S62-B vocabulary `DirectCommand`/`BaseMutation`/`BaseChange` re-cut after these rows) — re-key to `KelGroups.*` substrate qualified rows owed. KILLED rows: STALE-UNESTABLISHED (audit-report refs `3a7b355a…`, mutant sources not pinned to current tree). |
| 13–18 | t62 archived auditor/owner ledgers (6 files) | A-010/A-011, E-DECISION, E-ROW, SHA-256 refs | STALE-UNESTABLISHED (decision records, no mutant binding). |
| 19 | `t62-owner-codex/campaign-a013` + `-s62-c-a011.md` | A-011/A-010 | STALE-UNESTABLISHED. |
| 20–26 | t48-owner-codex family (7: main TSV + emitter ×2 + docs ×2 + inversions + pledge) | E-* rows + INV-48-EVENT-CTORS-COMPLETE/INVERSION-NAMING/NO-BACK-EDGE/ONE-GUARD-PER-CONSTRUCTOR/SORRY-BOUNDARY/STATE-DECEQ/STEP-TOTAL/TOJSON-DERIVABLE (main TSV: all OPEN, evidence NONE) | Evidence-NONE rows: STALE-UNESTABLISHED (no mutation, no command — nothing to reuse, nothing disproved). INVERSION-10 / E-JSON / E-PASS / E-RELAX: UNUSABLE for D2 (no theorem identity). |
| 27–28 | t54-composition-owner-codex (2) | E-DEBT/E-DERIVED/E-THRESHOLD/R-2/R-3-LAYERING/SHA-256 | UNUSABLE for D2 (composition notes, no production-definition kill). |
| 29–30 | t59 (mandate + docs-auditor evidence TSVs) | E-*/FENCE-*/M-* rows | UNUSABLE for D2 (citation/fence rows, no theorem kill). |
| 31–32 | t-toolchain-contract (2 TSVs) | INV-TC-01..04, all OPEN, evidence NONE | UNUSABLE for D2 (toolchain rows); pin `v4.25.0` noted as context only. |
| 33 | t74-corpus-exporter | E-CLOSED/E-ONLY | UNUSABLE for D2 (Haskell subject). |
| 34 | t-release-pipeline recut-s001-auditor | SHA-256 refs | UNUSABLE for D2. |
| 35 | batch1-grok auditor | 12 × INV-* (all KILLED) | UNUSABLE for D2 (simulator/fidelity subject, no Lean theorem identity). |
| 36–40 | t-simulator-fable handoffs (5: batch2/fidelity/fidelity-recut/geometry-permalinks/strips) | GEO-/IT-/LINK-/E-*/NOTE-* rows | UNUSABLE for D2 (simulator subject). |
| 41–42 | `.archived/t-simulator` (2) | E-CORE / NOTE-009 | UNUSABLE for D2. |
| 43 | t48-owner-codex main TSV counted in 20–26 | (counted) | — |

Totals: 43 files; per-row states transcribed above from each ledger's own
column (no inherited GREEN re-labelled: every KILLED above is marked STALE,
needing a fresh run at this base). Per-mutant re-keying of the STALE Lean rows
to the 239 qualified identities is the concrete Phase-2 work item P2-a below.

## Gap 3 — ownership with per-row rationale from statement content

The original assigned all 14 economic atoms to cross-cutting rows by family
prose. Required: the actual atom/property relation with per-row rationale, never
inferred from a name. Rationale below is read from each statement's own
hypotheses/conclusion (verified excerpts in the repair notes). The donate
scratch control establishes only its own case (`Reactivegas.step_donate_inv` ↔
G-donate; Build 2 RED). Atom vocabulary: G-* (14 economic guards incl. refusing
arms), E-* (effects), V-open/V-cast/V-renounce/V-franchise/V-threshold/V-sweep/
V-tally (vote), B-propose/B-approve/B-admit/B-mutate/B-hook/B-enact/W-coherence
(substrate). Helper rows (81) are owned by **no** atom — satisfiable hypotheses
only; not tabulated as pairs.

**Family A — the 14 inversions (each hypothesis names its constructor; verified by grep over all 14 `hstep` lines).** Shape per row: successful-`stepEvent` for constructor c ⇒ guard(c)=true ∧ post-state equation. Owned by exactly its constructor's guard+effect; no other pairing is claimed.

| qualified row | owns | rationale (statement content) |
|---|---|---|
| `step_grant_inv` (root) | G-grantPermission | `hstep: stepEvent … (.grantPermission a c) … = some s'` ⇒ `pullCollection… ∧ isResponsabile…` + permitted-flag equation (verified excerpt) |
| `step_deny_inv` (root) | G-denyPermission | `hstep` names `.denyPermission a c` ⇒ guard + refund-all effect equation |
| `step_pledge_inv` (root) | G-pledge | `hstep` names `.pledge a u c v` ⇒ escrow-debit equation |
| `step_accept_inv` (root) | G-acceptPledge | `hstep` names `.acceptPledge a u c` ⇒ pending→accepted equation |
| `step_refuse_inv` (root) | G-refusePledge | `hstep` names `.refusePledge a u c` ⇒ refund-pending equation |
| `step_correct_inv` (root) | G-correctPledge | `hstep` names `.correctPledge a u c v'` ⇒ difference-settlement equation |
| `step_close_inv` (root) | G-closePurchase | `hstep` names `.closePurchase a c` ⇒ referente-spend equation |
| `step_fail_inv` (root) | G-failPurchase | `hstep` names `.failPurchase a c` ⇒ refund-everything equation |
| `Reactivegas.step_open_inv` | G-openPurchase | `hstep` names `.openPurchase a c` ⇒ collection-opening equation (verified excerpt) |
| `Reactivegas.step_deposit_inv` | G-deposit | `hstep` names `.deposit a u v` ⇒ double-entry equation (verified excerpt) |
| `Reactivegas.step_withdraw_inv` | G-withdraw | `hstep` names `.withdraw a u v` ⇒ double-entry + non-stalled equation (verified excerpt) |
| `Reactivegas.step_transferCassa_inv` | G-transferCassa | `hstep` names `.transferCassa a f v` ⇒ cassa-move equation (verified excerpt) |
| `Reactivegas.step_donate_inv` | G-donate | `hstep` names `.donate a v` ⇒ comune-credit equation (verified excerpt + Build-2 RED: mutating this guard broke exactly this proof) |
| `Reactivegas.step_backdonate_inv` | G-backdonate | `hstep` names `.backdonate a w` ⇒ equal-share equation with `auth s w` conjunct (verified excerpt) — the ONLY row owning the backdonate-auth atom |

**Family B — cross-cutting economic rows (rationale per row, not family prose).** `conservation_preserved` and `step_authorized` quantify over arbitrary `e` and their proofs proceed by 14 per-constructor arms (arm counts verified: 14 and 14) — each arm is its own constructor rationale, so all-14 ownership is demonstrated, not inferred. The solvent/`Reach` rows have NO per-constructor split (`solvent_preserved` body: 0 match arms; hypotheses are `Reach` + `solvent` + arbitrary-`e` `hstep`) — their property constrains exactly fund-moving transitions, so fund-neutral constructors are provisionally out:

| qualified row | owns | rationale |
|---|---|---|
| `conservation_preserved` | all 14 G/E, per-constructor | stmt: `conservation s` + arbitrary-`e` success ⇒ `conservation s'`; 14 arms verified |
| `step_authorized` | all 14 G, per-constructor | stmt: arbitrary-`e` success ⇒ `authorizedStep view s e s'`; 14 arms verified |
| `solvent_preserved`, `reach_solvent`, `not_insolvent_of_reach`, `comune_not_a_member_of_reach`, `credit_pledges_of_reach` (private), `comune_not_a_member_step`/`credit_pledges_step` (private) | fund-bound atoms (deposit/withdraw/transferCassa/donate/backdonate/pledge/acceptPledge/refusePledge/correctPledge/closePurchase/failPurchase) PROVISIONALLY; openPurchase/grantPermission provisionally OUT | rationale is the hypotheses (`Reach`, `solvent`, arbitrary-`e`), not the names: the property constrains fund movement. Per-atom confirmation is Phase-2 kill work, not asserted here. |
| `pledge_guard_inv`, `auth_referente_guard_inv`, `close_guard_inv`, `fail_guard_inv` | G-pledge / G-acceptPledge+G-refusePledge (referente) / G-closePurchase / G-failPurchase | each stmt names its guard's conjuncts over `Collection` (e.g. referente-equality); no other constructor's guard appears |
| `pledge_escrow_debit`, `deposit_double_entry`, `withdraw_double_entry`, `close_spends_referente`, `close_permission_to_close`, `pledge_rejected_when_member`, `pledge_preserves_allUnique`, `uniquePledges_pend_cons`, `governance_enacts_windUpAdmin` | the named guard/effect only (pledge/deposit/withdraw/close/governance respectively) | each conclusion equates the named post-state or permission predicate; verified `governanceEnacts u (windUpAdmin s u)` shape |

**Family C — vote rows (`KelGroups.Vote.*`, 30).** Owned by vote atoms only — never by economic guards (no `step`/`Event` in any statement; verified by the absence of economic vocabulary in the file's theorem contexts):

| rows | owns | rationale |
|---|---|---|
| `emptyVoteState_sweepReady`, `emptyVoteState_wellFormed`, `sweepClosures_sweepReady` (private), `sweepClosures_wellFormed` (private), `effectedState_sweepReady` (private), `applyVoteEvent_preserves_wellFormed`, `foldFrom_preserves_wellFormed` (private), `foldVote_wellFormed` | V-sweep + V-open/V-cast/V-renounce | conclusions are `SweepReady`/`VoteWellFormed` over sweep/fold outputs |
| `ballots_nodup_disjoint`, `open_questions_are_open`, `questions_partition` (verified excerpt: `foldVote`-output conjunctions), `sweepStep_of_open`, `sweep_filterMap_of_swept`, `filter_open_idem`, `sweepClosures_idempotent` (verified: self-composition equation), `sweepDuplicating_duplicates` | V-sweep/V-tally | statements equate or partition sweep/fold outputs; `sweepClosures_idempotent` needs no guard atom at all — its rationale is the equation itself |
| `inadmissible_is_noop`, `nonresponsabile_event_noop`, `unfranchised_cast_noop` | V-franchise + the named VoteEvent | each stmt's hypothesis is the named inadmissibility condition with a `VoteEvent` argument (verified `VoteError.notResponsabile` text in Tests, `isResponsabile`-over-view in Validate) |
| `franchise_of_tallies`, `tally_keys_franchised_from` (private), `tallyKeysOfState_*` (private ×2), `effectedState_tally_growth` (private), `verdictOf_threshold_congr`, `sweepStep_key`/`sweepClosures_open_mem`/`closed_mem`/`preserves_qid` family (private), `foldVote_append`, `foldFrom/foldVote_preserves_qid` (private), `placeBallot_clean`/`placeBallot_tally` (private), `sweepClosures_tallyKeys` (private) | V-tally/V-threshold/V-sweep as named | each conclusion is about tallies/verdicts/question-ids over vote state; private lemmas support the named authored rows above |
| `no_expiry` | V-cast (single-question) ONLY | stmt covers a cast on one distinct question (per t54 ledger C5 evidence); NOT member events, NOT the event list — scope stated, not widened |

**Family D — substrate rows (`KelGroups.*` + root aliases, 24).** Owned by base/validate/integration atoms only:

| rows | owns | rationale |
|---|---|---|
| `emptyState_wellFormed`, `approvePending_wellFormed` (private), `foldEvents_preserves_wellFormed` (private), `enact/tryEnact/finishEnact/applyPropose/applyApprove/applyEvent/foldGroup_preserves_wellFormed`, `tryEnact_eq_of_enacts`, `tryEnactDetailed_enactment_threshold_met` (private), `enact_implies_threshold_met` (+ root alias), `members_change_implies_enacted` (+ root alias), `member_key_coherent` (+ root alias), `approvals_nodup` (+ root alias), `proposer_mem_approvals` (+ root alias), `majority_table` (+ root alias), `majority_not_strict_on_even` (+ root alias) | B-propose/B-approve/B-enact/W-coherence as named | each stmt quantifies over `GroupState`/`PendingProposal`/fold outputs; the 7 root aliases each call their `KelGroups.*` counterpart (verified `:881` pattern) — alias rows inherit the counterpart's atoms; a kill of one need not kill the other (different signatures), so both stay listed |
| `validateDirectAdmission_ok`, `direct_admission_requires_admin`, `non_admin_admission_is_noop`, `membership_growth_is_direct_admission`, `commitBaseChange_ok/members`, `tryEnactBase_runs_hook/preserves_absence`, `enactMutation_preserves_absence`, `app_event_preserves_members`, `app_event_has_no_base_change`, `base_change_runs_hook` | B-admit/B-mutate/B-hook as named | stmts name `validateDirectAdmission`/`commitBaseChange`/`tryEnactBase`/`admitMemberInto`/`enactMutation` outputs |

**Family E — composition/trace/witness rows (18).** `Reactivegas.Composition.*` (3): owned by route/vote-derived atoms (`voteDerived_iff_not_direct` verified: `route`/`voteDerived` equations over `Event`). `stepDetailed_erases` (root): owned by none as property — it equates `eraseDiagnostic ∘ stepDetailed` with `stepEvent` (verified `cases h: stepEvent…` shape); it constrains the diagnostic wrapper, not a guard. `checkX = true by decide` witnesses (`Reactivegas.*` 15 incl. `baseHook_votes`, `base_change_recomputes_votes`, `canonical_economy_holds`, `exhaustive_inventories_hold`, `integrated_theorem_witness_holds`, `i57_*`, `admissionPreservation_holds`, `productionWellFormed_holds`, `comuneThresholdSanity_holds`, `comune_cannot_authorize`, `app_members_preservation_*`; `TraceTests.*` 7 incl. `all_checks_pass`, `frozen_*`): each owned by the atoms its check evaluates — e.g. `Reactivegas.baseHook_votes` (verified: `baseHook … = .ok s'` ⇒ `s'.votes = sweepClosures …`) owns B-hook+V-sweep; `sweep_idempotent_*` owns V-sweep; `direct_admission_only_holds` owns B-admit; `*_mutant_caught` rows own the same atoms as their base witness PLUS the checker-sensitivity reading (survival of the surrounding check = assurance gap, never a production kill). `TraceTests` mirrors own the same atoms as their `Reactivegas.*` counterparts (same `check*` subject, different namespace) — listed separately, never merged.

## Gap 4 — P1-D re-derived over the repaired inventory (no unmeasured batching)

The original 59-build figure is WITHDRAWN (not derived; contradicted its own no-batching premise). Layer corrections accepted: 19 s cold is an observed wall time, not a proved ceiling; 3 s restore is one single-file observation, not a universal lower bound; the 2 s `CorpusGate` run elaborated its module (`lake env lean` imports + `#eval`), so it is **module elaboration + replay**, not isolated replay. Reclassified observations:

| observation | layer | cost | status |
|---|---|---|---|
| Build 1 cold `lake build` | full build | 19 s wall | observed once; full log LOST (tail only) — loss stated, never reconstructed |
| Build 2 variant RED | incremental rebuild, failed at `Invariants` | 10 s wall | retained log `6dedd2…` |
| Build 3 restore GREEN | incremental rebuild | 3 s wall | retained log `43fae2…` |
| `lake env lean TraceTests.lean` | check elaboration (43 `decide`s + inventory) | 11 s | retained summary `4d4bfc…` |
| `lake env lean CorpusGate.lean` | module elaboration + `#eval` replay | 2 s | retained `a17fcf…` |

**Concrete command plan (controls vs invocations separated; per repaired row counts: 239 qualified identities = 158 authored + 81 helper; STALE Lean receipts ≈40 rows across ledgers 1–19):**

- P2-a re-key + footprints: for each STALE Lean receipt, `grep` subject/mutation/checker/toolchain from the ledger (free, done in Gap 2) then ONE `lake env lean --run footprint-check` elaboration per receipt family (no build) to bind the transitive footprint at `3590c00`; families ≈12 ⇒ **12 elaborations, 0 builds**. Controls: footprint assertions fail loudly on mismatch (like the S/B reconcile), not silent GREEN.
- P2-b inversion kills (14 `step_*_inv` + `step_authorized` + guard-lemma rows ≈20 rows): one admitted single-atom `lake build` per row ⇒ **20 builds** (at 3–10 s observed per single-file touch; cold 19 s held as contingency only, not the plan).
- P2-c vote kills (≈30 authored vote rows, grouped by owning atom into ≈12 mutants) + substrate kills (≈24 rows, ≈10 mutants): **22 builds**, one per admitted mutant (isolation: one atom per subject).
- P2-d witness re-elaboration (≈22 `checkX` rows): `lake env lean` per owning file (5 files) + surrounding-check sensitivity runs ⇒ **0 builds, 8 elaborations**.
- Restoration/final: `git status --porcelain=v1` clean check + final GREEN `lake build` + `TRACE-INVENTORY` agreement read ⇒ **2 builds** (final acceptance run + one spare for a failed-restore repeat).
- **Total: 44 builds + 20 elaborations, exploration 0.** Every identity preserved (defer-only); batching savings NOT subtracted — validating shared-scheduling would take 4 further builds, separately authorized, and is not assumed.

## Gaps needing new bounded invocations (exact costs, before anything runs)

- G-B1 compiler-generated environment enumeration: `lake build` of the 3 unbuilt modules + gate elaboration ⇒ **1 build + 1 elaboration**. Authorizes the L2 closeout. Not run (4v3 already).
- G-B2 cold-log re-measurement: full-log cold `lake build` ⇒ **1 build**. Not run.
- G-B3 per-atom solvent-row confirmation: included in P2-b's 20 (no extra).

## Reconciliation (Phase 1 claims no coverage)

- `DISCOVERED`: 239 qualified identities + 27 modules + guard/effect/error axis (Gap 1/§P1-A-D1b original).
- `REQUIRED-INPUT`: same 239 (zero exemptions) + relevant owned atoms per Gap 3 above. No Cartesian pairs (≈239×40 NOT claimed; tabulated relevant pairs only).
- `EXECUTED`/`KILLED`/`SURVIVED`/`BLOCKED`: not claimed. The Build-2 RED is costing evidence retained in `handoffs/`, never a kill.
- Every REQUIRED-INPUT identity: kill pending (P2-b/c), helper-satisfiability pending (P2-a elaborations), or D6-ambiguity noted (L6 original) — none absorbed, none covered.

*End of R2. Original submission preserved; this revision answers NOTE-003 gaps 1–5. Packet: this file + `P1A-qualified-inventory.txt` (`efdeb3…`) + `P1A-qualified-classified.txt` (`ef93b9…`) + prior retained evidence. Auditor of another family inherits nothing else.*