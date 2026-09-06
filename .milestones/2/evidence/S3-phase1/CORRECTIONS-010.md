# CORRECTIONS-010 — cost classes split by module; witness scope restored; 974 classified per identity (no execution)

Static only: graph lines and counts read at base `3590c001`. No build/probe/
elaboration run. No production, statement, or fence change. Worktree untouched.

## C1. Cost classes split by ACTUAL mutated module (consequence corrected)

Verified graph lines: `KelGroups/Integration.lean:1 import KelGroups.Validate`;
`KelGroups/Invariants.lean:1-3` imports Fold, Validate, Integration;
`Validate.lean:1`/`Fold.lean:1` import State; NOTHING outside Invariants (except
`Reactivegas/Composition.lean`) imports Fold — Integration does NOT import Fold.
So Integration is Validate's dependent; Fold and Validate are siblings. My
"reuse the siblings" consequence was inverted. Corrected classes (checker
`KelGroups.Invariants`, one-module-per-invocation method):

| mutated module | affected closure to the checker | invocations/op |
|---|---|---|
| Validate | Validate → Integration → Invariants | **4** (3 rebuilds + 1 check) |
| Fold | Fold → Invariants | 2 (1 rebuild + 1 check) |
| Integration | Integration → Invariants | 2 (1 rebuild + 1 check) |
| Step (via Predicates; `Predicates` imported only by `Invariants` + umbrella) | Step → Predicates → Invariants | 3 (2 rebuilds + 1 check) |

Corrected envelope (targeted; unfunded; batching unsubtracted): OP-11..24
inversion 14×3=42; OP-25..31 solvent 7×3=21; OP-32..38 aliases 0 (static);
OP-39/40/41 DISJOINT/NOSTALE/POLICYFREE (Fold-class) 3×2=6; OP-42..45 t57 four
atom-groups (class fixed at authoring by target module; envelope 3 each = 12
worst case, reducible to 2 for Fold/Integration-class); OP-46..55 vote rest
(Fold-class) 10×2=20; OP-56..58 B-admit 3 atoms (Validate-class) 3×4=12;
OP-59..61 hook 3 atoms (Integration-class) 3×2=6; OP-62..65 preservation
(Fold-class) 4×2=8; OP-66..70 witnesses 5×1=5; OP-71 final 1 build + 2
elaborations. Future targeted total 134 + 1 build. Coverage is never reduced to
fit any number above.

## C2. Witness scope restored (narrowing withdrawn)

A changed-definition witness NEED NOT quote the atom verbatim: verbatim
quotation (as Build-2's mismatch did) is SUFFICIENT, never necessary. The
standing rule is the original disjunction — "observable changed-definition
witness, OR equivalent actual loading evidence" — and no valid method is ruled
out for Lean's failure formatting. Unchanged: provenance alone (hash
replacement, LEAN_PATH order) never establishes loading.

## C3. All 974 classified per identity (exclusive partition + named families)

Exclusive partition of the 1213 retained compiled identities: SOURCE exact 163
+ SOURCE-PRIVATE via `_private.<Module>.<idx>.<name>` substring 76 +
GEN-RECURSOR 1 (`Reach.brecOn`, recursor over Reach — the mandate's
`Reach.below` family) + GEN-PROJECTION 12 + GEN-OTHER 961 = 1213. (163+76=239
source; 1+12+961=974 remainder.)

GEN-OTHER shape census (non-exclusive patterns; overlap stated, e.g. generated
`_private` match-equations carry two patterns): 95 `.inj`, 95 `.injEq`, 133
`sizeOf_spec`, 9 `ofNat_ctorIdx`, 9 `.eq_def`, 231 `.eq_N` (all N — the rule is
the suffix shape, so `.eq_2`-style names are covered, none called unexpected
for numbering), 101 `match_N.eq` (subset of `.eq_N`, counted once above), 373
`_proof_*`, 133 generated-`_private`, remainder `inst*`/deriving outputs. Rule
per family: machine-generated equation/injectivity/size/recursor/proof-term/
instance scaffold sharing its parent's module prefix; no declaration site in
`lean/` sources (verified by absence from the source grep inventory).

The 13 individually listed (no family left unnamed): 12 structure-field
projections — `SweepReady.openNodup/closedNodup/openClosedDisjoint/openClean/
closedClean/closedNotOpen`, `VoteWellFormed.opensOpen/toSweepReady`,
`WellFormed.memberKeys/pendingKeys/membersCoherent/pendingCoherent` — each
verified as a DECLARED FIELD of its structure (`Vote/Invariants.lean:46-61`,
`KelGroups/Invariants.lean:28-32`) with NO `theorem`/`lemma`/`def` declaration
site (grep verified); classified GENERATED-PROJECTION. Honest limit retained:
classification is name-shape + module-attribution + declaration-site absence
(static recognition); the mechanism of their `thmInfo` status is not further
established, and any auditor reading them differently has the exact list above
to work from.

*End of CORRECTIONS-010.*
