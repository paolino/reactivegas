# CORRECTIONS-016 — generation pattern fixed row-by-row (no execution)

Static only: conclusion re-reads for all 14 inversions + guard lemmas,
definition bodies for every mutant site, writing. No builds, queries, probes,
mutations, Phase-2, hidden workers, or audit. Prior artifacts preserved; this
file + the two files below govern where they conflict with earlier operative
text on these three points.

## 1. Dropped atoms restored (completeness on my own STATEMENT basis)

Re-reads established three conclusion-vs-arm omissions exactly as flagged plus
full verification of the rest: `pledge_guard_inv` now carries all six
conclusion conjuncts (G09b–G09g; G09a/G09h correctly absent — unnamed in the
statement); `step_pledge_inv`/`step_accept_inv`/`step_close_inv` drop the
stalled atom their conclusions omit (G09h/G10e/G13f); all other inversion and
guard-lemma conclusions match their arms conjunct-for-conjunct (verified
above). Stalled atoms remain ONLY on PROOF-DEP-ARM lines (proofs execute full
arms — basis explicit, not hidden).

## 2. Causal targets corrected throughout (no file co-location)

The grant-arm/pledge-helper misattachment is removed: every pledge-conjunction
row sits on the pledge-arm op with the pledge-arm mutant. Full pass applied:
auth rows span both referente arms (correct — the conjunction appears in both);
`close_permission_to_close` keeps close-arm primary with the grant-set `E02`
read explicitly noted as observed-in-OP-11; all other rows verified same-entry.
Retained: `handoffs/RELATION-v2-property-atom.txt` (`eeea2c2c…`, as corrected).

## 3. Operation column carries literal mutants or explicit tags (no families)

Retained: `handoffs/OPMAP-v4-requirement-mutant-input.txt` (`7a21d576…`, 176
lines `OP|requirement|mutated-atom|required-input`). Every kill op names one
literal mutant (`MUT:file:line:old→new` from read bodies — arm guards, effect
drops, erase/filter/threshold/comparator/field/constructor/arithmetic mutants);
tagged alternatives stay explicit (COLL-ALL14 collective with resolved arm-op
targets; RECOVERED archived instruments; ELAB owning-file verification;
SHARED/PREMISE/VACUOUS statics; OBSERVED-IN named runs). Machine-audited:
158/158 authored covered, 0 helpers, 0 family tokens in the mutant column.
Suffix-letter splits (OP-50a/b … OP-67a/b/67G) preserve traceability; structural
rows that no threshold/default mutant can falsify (parametric/empty/idempotent
lemmas) verify by owning-file elaboration (OP-74), stated not hidden.

## Envelope restated from the map (unfunded; batching unsubtracted; not a grant)

Per-op closure classes from the mechanical import-closure table (Step 3,
Fold/Validate-Integration 2–3–6 by mutated module, ELAB 1, acceptance 1+2):
kills 42+9+38+6+12+8+3+10+3 = 131; re-runs 4; witnesses 5; structural ELAB 1;
acceptance 2 → **143 targeted + 1 build**. Terms: OP-11..24:42, OP-25/29/31:9,
OP-39/40/46a/46b/48/50a/50b/52/53/54a/64a-c/65a-c/66a-b:38, OP-41:6,
OP-49/58/59/60:12, OP-61/62/62B/62C:8, OP-63/67G:6, OP-67a/b:10, OP-42..45:4,
OP-68..72:5, OP-74:1, OP-73:2. Prior 124+1/129+1/131+1 totals WITHDRAWN as
fitted artifacts of the defective map. Helpers $0 (exhibited).
Past-provenance permanents excluded (no execution closes them).

*End of CORRECTIONS-016. Genuine OPENs (solvent conjunct-level, t57 pins,
R-canAdd linkage, source-root separation, cold log) stand as recorded.*
