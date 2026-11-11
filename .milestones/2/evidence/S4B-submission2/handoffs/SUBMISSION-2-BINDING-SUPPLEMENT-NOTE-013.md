# SUBMISSION-2 BINDING SUPPLEMENT — NOTE-013 defects resolved, rebinding requested

**From:** S4-B commit owner (`muse`, pid/pgid 1493708).
**State:** repair commit `0f3ad01a447f40a23eb282ff5b4a8adc2b303ca1` (clean tree;
parent `59309d6`, base `3590c001`). Prior handoff
(`SUBMISSION-2-BINDING-HANDOFF.md`) stands except where this supplement
corrects it. **No OT/O run executed** (NOTE-012 gate + NOTE-013 no-build rule
both respected). This supplement spends nothing.
**Spend at supplement:** submissions 1/2 (sub-2 open), substantive 8/14,
targeted 42/60.

## 1. Defect dispositions (all verified at source before conceding)

**Defect 1 (promotion harvest discarded hypotheses) — FIXED in `0f3ad01`.**
`forallTelescopeReducing … fun _ ty` harvested the conclusion only; the fix
harvests bound-variable types (hypotheses) plus the body via `inferType` over
the telescope. Comment now states the honest scope (mention = binding, not use
or sensitivity). Theorem untouched, as ordered. Omit-diff re-validated
(`git apply --check` clean) after the fix.

**Defect 2 (miniatures changed the environment) — completed, not contested.**
Miniatures re-scoped as SUPPLEMENTARY mechanism illustration; production
binding is carried by the O4/O5 real-file runs. Precise input differences:

- OT1 (`S2-chain-P07.lean`, `757bd4e6…`): no project imports. Local copies:
  Key/CollId/Pledge/Collection/State(**minus votes** — unobserved by this
  chain)/GroupView+Member+Role/Admin/isAdmin-chain/**assocLookup** (real
  bodies); `bal`/`bump`/`sumPledges`/`pullCollection`/`demand`/
  `isResponsabile` (production shape over local types); `BackdonateAuth`;
  `Event`/`AppEvent`/`step`/`stepEvent` (**closePurchase-only** — the tested
  statements quantify over closePurchase inputs exclusively; single-ctor
  inductives, exhaustive by construction); helpers `option_bind_inv`,
  `demand_eq_true_of_some`, `bool_and_left/right`, `eq_nil_of_isEmpty`,
  `permissionToClose` (copies); `variable {view} {auth}` (production shape).
  Target statements/proofs byte-identical (`close_guard_inv` proves,
  `step_close_inv` fails, `close_permission_to_close` elaborates only via the
  broken link, `permissionToClose_corr` proves). Differs from production:
  truncated event/step vocabulary, voteless State, local (not imported) types.
- OT2 (`S2-chain-P01.lean`, `f69a9003…`): no project imports. Local copies:
  Key/Member(`key` only)/GroupView/`assocLookup`/`lookupMember` (real bodies)
  + **mutated `isMember`** + private lookup helpers (copies) + local
  `comuneId`/`comune_not_a_member` (copies); byte-identical helper
  statements/proofs + proving P01-orig contrast. Differs: local types, mutant
  locus (local copy vs production `Types.lean`).
- Positives (NEW, each its own invocation — see §4):
  `S2-chain-P07-clean.lean` (`62322552…`, production atom restored, expects
  exit 0), `S2-chain-P01-clean.lean` (`807f3a2f…`, production body restored,
  expects exit 0). Each differs from its negative twin by exactly one line
  (verified by diff). A red twin without its green twin could fail from its
  own setup; the pair isolates the atom/body as the difference.

**Defect 3 (census quota + errors-as-nonpred + unconditional OK) — OT4
REWRITTEN** (`S2-census.lean`, `ab3dd269…`): prints BOTH sorted identity sets
(old-rule + new-rule) with per-kind table, named below-exclusion and named
sort-undecided bucket (never "non-predicate"); asserts set RELATIONS
(new ⊇ old, symmetric difference empty with every delta printed BY NAME,
unclassified == 0, opaque-pred == 0 as this-tree baseline, thm-excluded
sanity) — no hardcoded 24 anywhere; `S2-CENSUS-OK` only with zero errors
else `S2-CENSUS-FAILED`.

## 2. O4/O5 failure predictions (certain vs reported-as-observed)

Mechanism established by source reading (sorry-transitivity: failed theorems
keep intact statements, so dependents elaborate; only defeq-through-body
breaks):

- O4 CERTAIN: `step_close_inv` fails (guard-shape defeq vs mutated atom).
  `close_guard_inv` proves (pure); `close_permission_to_close` elaborates via
  the broken link (no new error); all other `step_close_inv` consumers
  (`conservation`/`auth`/`spends`/`solvency` close cases) elaborate via intact
  statements. EXPECTED-AS-OBSERVED (same defect family, reported from receipt,
  never pre-claimed): trace `closePurchaseNegative` flip via
  `all_checks_pass`, `frozenChecks` freeze mismatch, and the appended print
  (`true`). Print visibility basis: 50 `info:` lines in submission-1 build
  logs; actual presence reported honestly at execution.
- O5 CERTAIN: exactly the two promoted helpers fail (defeq through the
  constant-false body). Everything downstream (`keys_mem_coe`, solvent,
  insolvent, canCloseGroup, P01-orig, `productionWellFormed_proj`) elaborates
  via intact statements. EXPECTED-AS-OBSERVED: trace decide-flips over
  `isMember`-guarded steps (`all_checks_pass`) and `comune_cannot_authorize`
  (via `productionWellFormed` on a comune-member view).

## 3. Fit/gap (exact, before phase)

Sheet authorizes 6 sub (O1–O6, unchanged, exact) + 4 tgt (OT1–OT4). The two
positive twins add 2 targeted invocations (one per twin; two invocations never
counted as one): need 6 targeted total. **Gap: +2 targeted** (spend would reach
48/60 — within the hard 60 ceiling; this is a sheet-gap return, not a ceiling
raise request). Substantive unchanged (no gap there). If the +2 is refused, the
positives stay OPEN rows and the negatives stand as mechanism illustration
only (stated now, not later).

## 4. Rebinding requested

Please bind: repair commits `59309d6` + `0f3ad01` (defect-1 fix; omit context re-validated post-fix via `git apply --check`); prior instrument hashes otherwise stand (OT1/OT2/OT3 unchanged) except OT4 superseded by `ab3dd269…` above; the 3 new files above with OT1pos/OT2pos argv (same shape as OT1/OT2: cwd=`…/lean`, `nix develop --quiet -c lake env lean <ABS-PATH>`); the §2 failure taxonomy. No runs until bound.
