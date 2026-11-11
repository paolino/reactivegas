# CORRECTIONS-009 — false absence withdrawn; aliases without statement mutation; loading evidence by closure (no execution)

Static only: bodies quoted from source read at base `3590c00`. No build/probe/
elaboration run. No production, statement, or fence change. Worktree untouched.

## C1. Runtime-absence claim WITHDRAWN — production establishes the property

My CORRECTIONS-008 sentence "production never ESTABLISHES `comune_not_a_member
view` for a view" is FALSE and withdrawn in full. Bodies read (not relayed):

- `Step.lean:357`: `productionWellFormed gs := !isMember comuneId (groupView
  gs)` — the negated canonical membership check.
- `Step.lean:362` (`boot`): builds the aggregate, returns it ONLY `if
  productionWellFormed gs` — "Guarded founding aggregate. `none` when
  `comuneId` appears in the supplied member list."
- `Step.lean:377` (`apply`): checks `productionWellFormed gs` BEFORE the
  integrated fold AND `productionWellFormed result.state` after
  (`comuneReserved` otherwise) — "refused before the generic integrated fold
  runs, so that reserved key cannot become authorized by being smuggled in."
- `Step.lean:346` (`integration`): `reserved := comuneId` — the reserved key IS
  the comune account.
- `KelGroups/Validate.lean:142` (`validateDirectAdmission`): `if target =
  reserved then .error (.reservedKey target)` checked FIRST.

Corrected ownership for `comune_not_a_member_of_reach`: the theorem transports
the boot premise through Reach induction (fixed `view` parameter — that half of
the correction stands), AND the property IS established and maintained by
production (`validateDirectAdmission` reserved-refusal + `productionWellFormed`
boot/apply gates). No guard-mutant pairing is manufactured for the transport
theorem itself; the RUNTIME producers above are the established definitions for
any future kill work on this property. The proposed S5 finding is rewritten to
the only warranted question: whether, and how, the current theorem statements
connect to THOSE producers across the claimed scope. Absence of such a
connection is not established here and never inferred. The theorem is true; the
fence is unchanged; no production change is requested.

## C2. Aliases: shared dependency classified as-is; no quota language

- Each alias keeps its ORIGINAL statement (7 call-through bodies verified; no
  counterpart statement mutated). A counterpart-body kill propagating through
  the call counts ONCE (shared semantic dependency), never as a second
  independent mutation; an alias-surface break is checker-class, classified
  separately, never summed with production kills. Former OP-32..38 execution is
  replaced by this static classification: 0 execution ops for aliases.
- Transports and helper facts never owe unrelated runtime guards: the 81 helper
  rows keep satisfiability-of-hypotheses (R3 §4/R4), the comune transport keeps
  premise-carriage (C1). No row is filled by forced pairing.
- "14 economic atoms" as a blanket property is withdrawn everywhere it lacks
  per-row showing. What stands is only shown evidence: 14 `hstep` literals,
  14+14 counted proof arms, per-conclusion atoms, proof-dependency maps.

## C3. Loading evidence: closure rebuilds + observable mutant witness

R5's M-elab* (hash replacement + LEAN_PATH order) is demoted to PROVENANCE —
necessary, never sufficient for loading. Corrected method M-elab**: for a
mutated module M checked at module C, rebuild the ACTUAL affected dependency
closure in dependency order (computed from `^import` lines at base), count every
invocation, and require the RED output to QUOTE the mutated atom (as Build-2's
mismatch quoted `(0 < v + 1)` vs `(0 < v)`) — the observable changed-definition
witness. Measured closures at base:

- Inversion/solvent ops (mutate `Step.lean`, check `Reactivegas/Invariants.lean`
  via `Predicates`): closure Step → Predicates → Invariants = **2 rebuilds + 1
  proof check = 3 targeted invocations/op** (State/Integration/Vote deps reused
  from base lib, unchanged — stated per op).
- Vote ops (mutate `Vote/Fold.lean`, check `Vote/Invariants.lean`, sole direct
  importer): **1 rebuild + 1 check = 2/op**.
- Substrate ops (mutate Fold/Validate/Integration, check
  `KelGroups/Invariants.lean`): **1 rebuild + 1 check = 2/op** (siblings
  reused; `Integration` imports only `Validate` — verified).
- Witness re-elaborations: 1 elaboration, no rebuild, no mutation.
- Estimates marked ESTIMATE with basis (11 s check-elaboration reference only);
  Build-2's 10 s never cited for these. First executed op measures.

*End of CORRECTIONS-009. Theorem true; fence unchanged; statements unchanged.*
