# CORRECTIONS-017 — causal taxonomy (kill / observed / structural), generation fixed (no execution)

Static only: proof-body reads at base `3590c001` (unfold-vs-call audit across
every kill-op row), writing. No builds, queries, probes, mutations, Phase-2,
hidden workers, or audit. Prior artifacts preserved; this file +
`OPMAP-v5-requirement-verdict-input.txt` govern the taxonomy where they
conflict with earlier operative text.

## The rule encoded (NOTE-017's criterion, applied mechanically)

A predicted RED must be the declaration the compiler's diagnostic would name
for the row's OWN proof obligation failing: KILL = the proof unfolds/matches
the mutated definition (or decides over its computation) so the mutant breaks
elaboration AT this declaration. OBSERVED = the proof calls a sibling lemma
whose failure cascades (real protection, never this row's RED). ELAB-STATIC =
neither (tautological/parametric/structural/projection/checker-local content;
verified at elaboration). OPEN-KILL = a kill should exist but its mutant is
unidentified (bounded search stated).

## Resolutions (proof evidence per family)

- Four guard inversions + `close_permission_to_close`: pure-Bool/data bodies,
  zero `stepEvent` — ELAB-STATIC (were kill-mapped; the OP-12..17 arm mutants
  cannot reach them; their atoms' kills live on the consuming event
  inversions, which keep them).
- `majority_not_strict_on_even`: OP-67a weakening mutant leaves it green by
  arithmetic (`2*(n/2)<=n`) — conceded. CORRECTED mutant (strengthening
  `(adminCount+1)/2` → `adminCount+1`): table RED (1→2 vs 1) AND not_strict RED
  (4≤1 false), both verified arithmetically. OP-67a keeps both rows.
- Vote lemma tower flattened by proof reads: composed rows (ballots,
  open_questions, franchise, no_expiry, foldVote_wellFormed, sweepReady,
  sweepWellFormed, tally_keys_franchised_from, sweepClosures_tallyKeys via
  open_mem, sweep_filterMap, applyEvent_preserves, foldGroup, foldEvents,
  direct_admission_requires_admin, base_change_runs_hook, members_change-R,
  enact_implies-R, solvent chain, escrow/rejected/preserves/close_spends)
  → OBSERVED with the named upstream row each. Unfolding rows keep KILL
  (placeBallot_clean/tally definitional; open/closed_mem, idempotent,
  preserves_qid, sweepStep pair, effectedState trio, applyVoteEvent pair,
  inadmissible trio, foldVote_append, foldFrom pair via own induction,
  no_expiry? No — calls pair → OBSERVED as listed).
- Structural rows → ELAB-STATIC: 4 guards + close_permission, tryEnact_eq
  (rfl), approvals/proposer/member_key (+roots, projections),
  tallyKeys-erased/insert (checker-unfold), filter_open/duplicating
  (list/test-def), verdictOf_congr (parametric), stepDetailed/frozen/
  composition/check witnesses as filed.
- Threshold-VALUE mutant (OP-41) kills NOTHING filed — WITHDRAWN as a kill op
  (verdictOf parametric; foldVote/franchise/no_expiry all OBSERVED via other
  rows). POLICYFREE-family rows keep ELAB base + OPEN-KILL bounded searches.

## Retained map + envelope (final taxonomy)

`handoffs/OPMAP-v5-requirement-verdict-input.txt` (`77dd1258…`, 210 lines
`OP|requirement|verdict|mutated-atom-or-tag|required-input`): 85 KILL, 30
OBSERVED (upstream row named per line), 21 OPEN-KILL (bounded search stated),
62 ELAB-STATIC, 9 RECOVERED, 2 STATIC, 1 ACCEPT. Machine-audited: 158/158
authored present, 0 helpers, 0 family tokens in mutant column. Every KILL row
names the mutant its own proof obligation breaks (unfold/simp/decide over the
mutated definition verified per family); OBSERVED rows name the upstream row
whose failure cascades; ELAB rows verify at elaboration (tautological,
parametric, projection, checker-local, wrapper, witness); OPEN-KILL rows name
the bounded search. SHARED-COUNTERPART tag retired (aliases resolved to
OBSERVED/ELAB by counterpart verdict).
Envelope recomputed from verdicts (unfunded; batching unsubtracted; not a
grant): kills Step-closure 60 (OP-11..24:42 + OP-25/29/31:9 + OP-67G:3 +
OP-69K/L:6) + Fold-closure 30 (15 ops) + Validate-closure 12 (OP-49/58/59/60)
+ Integration-closure 8 (OP-61/62/62B/62C) + Step OP-63:3 + K-State 10
(OP-67a/b) + Vote-State 4 (OP-57K) = 127; re-runs 4 (OP-42..45); ELAB 7
(OP-66E/68/69/70/71/72/74); acceptance 2 (OP-73) → **targeted 127+4+7+2 = 140 + 1 build**. All prior
envelope totals WITHDRAWN as fitted artifacts of defective maps.

*End of CORRECTIONS-017. Genuine OPENs stand; the target is a correct
assessment, not a zero-OPEN table.*
