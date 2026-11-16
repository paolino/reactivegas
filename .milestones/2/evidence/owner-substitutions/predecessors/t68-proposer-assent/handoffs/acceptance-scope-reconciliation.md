# Acceptance scope reconciliation — #68 (owner-side, vs original requirements)

Mandate: spec 988b2ab4 (R68-01..10 incl. R68-07 as qualified at 9a549f2),
gate v3 29e49c9d, oracle 9448e889. Candidate 3ee5c12 on 4a6cd87,
final squash 4cdb6078 (tasks-stamp only over 3ee5c12).
Audit-2 PASS 0c7a16ab (binding: full original+corrected scope).

## Requirement → evidence at final bytes (3ee5c12 content)

Corroborating checks sharing one source tree (not independent proof
sources): the frozen gate legs, the fenced oracle, worker guards,
inversions, and failure-mode instruments all read the same candidate.
Independence across parties holds where stated (owner-executed vs
auditor-rerun of the same frozen legs). Freshness split is explicit:
HISTORICAL = RED-commit/base falsification (sensitivity proofs, kept as
record); FRESH = final-tree GREEN executions + rebound A/B campaign.

| Original requirement | Evidence (fresh unless marked HISTORICAL) | Executed by |
|---|---|---|
| R68-01 empty-open both paths | oracle h/iEmptyOpen GREEN (leg-5); worker t68 guards GREEN (leg-2) | owner gate + auditor rerun |
| R68-02 enactment from recorded assents only | threshold-met family proved (leg-2); killer/admin-change guards GREEN | owner gate + auditor rerun |
| R68-03 arithmetic unchanged | byte-identical tables + elaboration in leg-2/just-ci logs | owner gate + auditor rerun |
| R68-04 refusal at boundary, new variant | worker + oracle refusal guards GREEN; 12 failure-mode guards (exact keyed identity vs duplicate/non-admin/unknown); validation premises in inversions | owner gate + auditor |
| R68-05 n=1 two-step agency | oracle + worker n=1 guards GREEN; sole_admin theorem proved | owner gate + auditor |
| R68-06 enactment sets (+B-killer) | oracle 13 guards incl. killer (approvals==["c"]) + admin-change GREEN | owner gate + auditor |
| R68-07 qualified preservation (spec R68-07) | RawStructural core + fold inductions proved (leg-2); admissible/integrated inductions; 8 regression guards GREEN; doc-comments; spec qualification | owner gate + auditor re-derived |
| R68-08 dependent statements (spec R68-08) | ruled pair + threshold evidence + wrappers re-proved (leg-2); 486 constants axiom-checked; zero sorry | owner gate + auditor |
| R68-09 witnesses (spec R68-09) | 13 oracle + 14 worker + 8 regression guards elaborate-true | owner gate + auditor |
| R68-10 mutation control (spec R68-10) | owner full-gate REDs rebound to repaired bytes (attempts 10, 11, independently hashed); auditor reconstructed overlay REDs both paths (T9/T15 logs) with module-rebuild-before-run both sides | owner + auditor |
| WellFormed/invariants/proof trust | 486 constants axiom-checked; zero sorry; 163 pin; inversions 14/14 structural | auditor + gate |
| Corpus/sealed hook/hook atomicity | leg-3 exact-true; hook-error atomicity guards; corpus 7→10 stories | owner gate + auditor |
| just-ci on final bytes | owner gate-12 exit 0 + auditor INDEPENDENT rerun, byte-identical b1ee60d2 | owner + auditor |

Sensitivity record (HISTORICAL, not fresh-at-final): RED-commit worker
guards all-false (attempt-2), oracle 10/13 RED on base with clean
elaboration, tripwire 4 sites on base. These prove the instruments CAN
fail; final GREENs + rebound A/B executions supply the actual fresh
evidence tabulated above.

## Required-missing vs optional-additional (desk NOTE-011 question)

- Validate/arithmetic re-mutation: NOT performed by auditor-2 (limits,
  "repair fence"). Classification: OPTIONAL-ADDITIONAL, explicitly not
  demanded. Rationale: the mandate's mutation class (spec R68-10) is
  proposer-credit per path — executed fresh by both parties. Refusal
  sensitivity rests on corroborating same-source checks (HISTORICAL REDs
  above + fresh failure-mode guards + validation-premise inversions), not
  on independent proof sources. Arithmetic identity is proved by
  byte-diff, which mutation cannot strengthen. No requirement invented;
  no rerun demanded.
- Vote/Step machines: untouched by slice (fence-verified); no control
  applicable. Correctly excluded.
- No required-missing control found. No gap named. Acceptance may proceed.

## Small corrections carried (no impact)

- Audit worktree actual path: /code/reactivegas-issue-68-audit-s2
  (detached HEAD; "-detached" in the ticket AUDIT-START line was a state
  descriptor, not part of the path).
- Pre-repair "no reachable guarantee lost" + "14/14 exact" wordings remain
  SUPERSEDED (see auditor-s2 packet addendum + R68-07 text); current truth
  is the qualified statements + 11/14 exact-premise with 3 inherited
  omissions routed to #66 S5.
- Advisory (no action): wBarIntact mislabels bar survival (fails via
  empty-open conjunct under A); bar survival rests on module confinement
  (Fold-only diff, Validate untouched — verified). Worker prefix signer
  "stranger" vs auditor "a": shape matches.

Conclusion: every mandated control has fresh evidence at final bytes or
admitted historical sensitivity record as labeled; evidence bound
separately per artifact hash. No inference from PASS alone.
