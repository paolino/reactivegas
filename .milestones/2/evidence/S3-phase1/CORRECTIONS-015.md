# CORRECTIONS-015 — literal relation, reconciled op map, three-way isolation split (no execution)

Static only: reads/greps/writes. No builds, queries, probes, mutations,
Phase-2, hidden workers, or second audit. Prior versions preserved; where this
file conflicts with them on the three points below, THIS file governs.

## F-03 — complete per-atom/per-property artifact (no families, no Cartesian)

Retained: `handoffs/ATOMS-ledger.txt` (`e67e5169…`, ~140 literal atoms with
source line refs: G/E arm conjuncts+effects `Step.lean:44-145`, V atoms,
B/R/W atoms, premise/non-atom tags) and `handoffs/RELATION-property-atom.txt`
(`89e5ac37…`, 559 lines `qualified-property|atom-id|basis`). Machine-audited:
all 158 authored identities carry ≥1 line; 0 helper rows present. Bases:
STATEMENT (atom named in statement — all 14 `hstep` literals + conclusion
equations read), PROOF-DEP(-ARM) (definition unfolded/used in proof as read —
counted arms), WITNESS (named check subject), SHARED (alias call-body
dependence, single-count), PREMISE/NONE (transport/vacuous, no atoms).
No Cartesian product: only listed pairs exist (559 lines, not 158×140).

Owned OPEN pairs (relevance unestablished statically — kill work, named):
solvent-row conjunct-level sensitivity beyond arm equations; t57-instrument→
theorem pins beyond recorded subjects; R-canAdd/R-canRemove theorem linkage
(no statement names them). `no_expiry` corrected scope stands (arbitrary event
+ prefix + hpres premise).

## F-06 — reconciled op map (premise boundary honored; ranges resolved)

Solvent reconciliation: OP-28 (`comune_not_a_member_of_reach`) and OP-30
(`comune_not_a_member_step`) are STATIC (premise-transport / vacuous `_hstep`)
— a fixed-parameter transport row holds no kill op. Kill ops OP-25,26,27,29,31
(5, Step-closure 3 each). Aliases static (OP-32..38, 0 ops).
Retained: `handoffs/OPMAP-v2-requirement-operation-target.txt` (`7445df5d…`,
174 lines `OP|requirement|atom|target-module|input-kind`) — every op names its
target module and input kind (admitted-mutant / recovered-instrument /
elaboration-only / static); zero placeholders. Coverage machine-audited
(158/158 rows, 0 helpers).
Envelope recomputed from the map (unfunded; batching unsubtracted; not a
grant/budget): kills 42+15+6+24+9+6+11 = 113; t57 re-runs 4; witnesses 5;
acceptance 2 → **124 targeted + 1 build**. Terms: OP-11..24:42, OP-25..31
(minus static):15, OP-39..41:6, OP-42..45:4, OP-46..57:24, OP-58..60:9,
OP-61..63:6, OP-64..67G:11, OP-68..72:5, OP-73:2.

## F-07 — three separate ledger entries (label ≠ attribution ≠ separation)

(i) SINGLE-CAUSE ATTRIBUTION (retained, bounded): Build-2 one-file one-line
diff + RED naming exactly the owning theorem with the mutated atom quoted
(`:407` mismatch) — establishes one cause, nothing about roots.
(ii) SOURCE-ROOT/FILESYSTEM SEPARATION: OPEN — the scratch variant was built
inside the candidate worktree (same `.lake` root); detached-root rebuild
unexecuted and ungranted.
(iii) HISTORICAL FENCE EVIDENCE (retained, scoped): t48 INV-48-I-FENCE rows
(forbidden `Step.lean`-path rejection, `e408a627…`/`2da5c149…`, gate logs) —
establishes forbidden-path rejection observance (a path control), NOT mutant
isolation. The S62 three-path fence was never cited in this packet; nothing is
claimed for it.

*End of CORRECTIONS-015. F-08 method kept: journal writes by append-to-EOF
shell command with tail readback only.*
