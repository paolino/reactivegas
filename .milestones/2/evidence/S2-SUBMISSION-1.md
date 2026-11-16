# SUBMISSION-1 — S2: a real axiom gate, honest checks, no quota

Seat `muse`. Ticket/epic owner `claude-opus-5[1m]` pane `%503`.
Mandate hash `0a1db9887ccc9d8f`, gate hash `39d6aa4e2c0c0170`,
plus inbox `NOTE-001-amendment-1` (A2 → A2′, read in full, acknowledged in
STATUS.md with zero builds).
Base `4a6cd87`, branch `chore/66-s2-axiom-gate`, worktree
`/code/reactivegas-66-s2`.
**Candidate `5745a2c`** (local commit, nothing pushed, no PR, no issue edit,
no external artifact of any kind).
Cold provenance: worktree started with no `lean/.lake`; the receipt chain
runs from the first cold build to the final green without ever deleting
`.lake`.
Caps: submissions 1/2 used by this packet. Build budget 8/8 substantive
(enumerated below; every other invocation was a no-op rebuild or a warm
`lake env lean` probe, neither of which compiles). Ceiling raises 0/2.

Companion file in this directory: `AXIOM-IDENTITIES.txt` — the 26 swept
module identities and all 1214 swept theorem identities, extracted verbatim
from the final green run.

## Row A — total axiom gate (`scripts/check-lean-axioms`, new, +x, wired into `just lean`)

Three sets, three independent derivations, reconciled — never quotas:

| set | derived from | must not come from |
|---|---|---|
| **S** source modules | shell: `git ls-files lean/*.lean` cross-checked against a `find` walk (disagreement fails closed: truncation and untracked files both rejected) | any import list |
| **B** built modules | `env.header.moduleNames` of the gate's own elaborated environment, filtered to the project roots | the filesystem walk |
| **T** theorem identities | `thmInfo` constants attributed to **B** through each module's own `constNames` (header names/data zipped; a length mismatch fails rather than truncates) | any name list |

Reconciliation failures: `S \ B` non-empty (tracked module never reached the
environment); `B \ S` non-empty; shell discovery vs an independent Lean-side
filesystem walk disagreeing; zero S, zero B, or zero T. No expected-count
constant exists anywhere in gate or driver — counts are printed, the
reconciliation passes or fails. Allowed axioms are exactly `permittedAxioms`
at `scripts/check-reactivegas-inversion-coverage:101`
(`propext`, `Classical.choice`, `Quot.sound`); the driver's doc comment says
so and why (extension of the existing six-inversion policy to the whole
extent, no new policy chosen). `sorryAx` gets its own finding wording so a
sorry fails *as a sorry*. The driver uses no partial call (no indexing, no
`!`-accessors, no `getString!`); the wrapper asserts absence of `PANIC at` in
both output streams and requires every reconciliation row to be present —
results inspected, not just the exit code. No new tracked Lean module was
added (temp driver under `$TMPDIR`), so §5 is untriggered and #70 untouched.

Final receipt (`just lean`, exit 0, zero recompiles):
`axiom-sources tracked=26 walked=26`, `axiom-built count=26`,
`axiom-theorems count=1214`, `axiom-gate: ok`.

## Row A2′ — the 163 quota is removed (amendment scope, `scripts/check-reactivegas-inversion-coverage` only)

`def expectedDeclarations : Nat := 163` and both `== 163` assertions are
deleted. The per-location reconciliation is verbatim preserved (real Lean
parse per file vs elaborated theorems at source positions via declaration
ranges — already two independent derivations), plus per-file census rows
(`lean-census <path> declared=N backed=M`), reported totals with no quota,
and an explicit empty-census failure. `requiredInversions`, tightness,
coverage comparison, and negative control are byte-untouched (see diff).
Constraint map: (1) reconciliation kept; (2) no counter-vs-counter gate
remains — any declared/backed divergence already fails per location with its
identity; (3) a real added theorem passes (Run C below); (4) an omitted
declaration fails with its identity (omit probe below); (5) sorry/axiom fail
for dependency (Run B2 + sorry/axiom controls); (6) file identities and
counts reported; (7) six inversions independent of the census; (8) script-only
edit, no model or statement change. Zero quota strings remain in
`scripts/`, `justfile`, or `lean/` sources.

## Row B — the three checks stop lying (`lean/Reactivegas/Invariants.lean`, `TraceTests.lean`)

Behaviour preserved bit-for-bit under accurate names; same Bools, same `by decide` proofs:

| was (false claim) | now (what it computes) | real enforcer bound in the doc |
|---|---|---|
| `i57TrustNoSorry` → `checkI57Trust` → `i57_trust_holds` | `adminAdmissionAndPreservation` → `checkAdminAdmissionPreservation` → `admin_admission_preservation_holds` | Row A gate (`scripts/check-lean-axioms`) |
| `kelGroupsHasNoReactivegasImport` → `checkI57Direction` → `i57_direction_holds` | `mixedGroupComuneExcluded` → `checkMixedGroupComuneExcluded` → `mixed_group_comune_excluded_holds` | `nix/lean-dependency-direction.sh` in `just lean` |
| `leanToolchainMatchesPin` → `checkI57Toolchain` → `i57_toolchain_holds` | `fixtureComuneIdAndThreshold` → `checkFixtureComuneIdAndThreshold` → `fixture_comune_id_and_threshold_holds` | `scripts/check-lean-toolchain` via `lean-toolchain-contract` in `just ci` |

Docs state the exact computation, name the enforcer, and say the
import-direction/toolchain properties are *not inherently uncheckable in
Lean* — only that these Bools never computed them. No stale name remains in
any source (only in regenerable `.lake` artifacts). The three `TraceTests`
re-exports are deleted in the same diff; verified dead before deletion (no
`open TraceTests` anywhere, no qualified refs outside its own file, absent
from the `checks` table and every script/recipe). Nothing is left
unenforced, so no B6 residual finding is carried: trust→Row A gate,
direction→shell scanner, toolchain→pin contract, all in the mandatory path.

## Row C — doc path (`lean/Reactivegas/Predicates.lean`)

One comment: `` `docs/design/state-machine.md` `` →
`` `docs/en/design/state-machine.md` `` (target verified present;
`docs/design/` does not exist; nothing under `docs/` touched).

## RED bundle (unmodified base `4a6cd87`; probe file staged, then removed; base sources untouched)

- R1/R2/R3: `lean/Reactivegas/RedProbeS2.lean` (staged-tracked) with a
  sorry-bodied def, a def depending on a declared non-standard axiom, and a
  trust-shaped Bool. `scripts/check-reactivegas-inversion-coverage` exited **0**
  with `inversion-audit: ok`, `declared=163 elaborated-backed=163`, **zero**
  mentions of sorry/RedProbe. A `theorem` variant tripped only the 163 count,
  never a sorry/axiom finding. So: no gate rejected sorry outside the six
  inversions, no gate rejected a non-standard-axiom dependency, and a new
  tracked module was reconciled against nothing.
- R4: eval probe `(checkI57Trust, checkAdminAdmissionReachable,
  checkAppMembersPreservation, redProbeTrustShaped, decide-equality)` =
  `(true, true, true, true, true)` on the poisoned tree — the green "trust"
  check IS the admission-Bool conjunction, carrying no sorry/axiom signal.

## Controls (each demonstrated failing; all control files staged-tracked during their run, removed after)

- **sorry-theorem** (gate-direct): `theorem auditOnlySorry := by sorry` →
  exit 1, one finding: `AuditSorryS2.auditOnlySorry: depends on sorryAx (a sorry-bodied
  theorem in the extent)`; extent 27/27/27, 1215 theorems.
- **non-standard axiom, exact A6 shape** (gate-direct):
  `axiom auditOnlyAxiom : True` + `theorem auditOnlyUse := auditOnlyAxiom` →
  exit 1, one finding naming the USE and the axiom (dependency exercised, not
  declaration).
- **transitivity** (gate-direct, amendment follow-up i): poisoned def
  `auditTransDef` + using theorem `auditTransThm` → exit 1, one finding on
  the THEOREM naming `auditTransAxiom`; the def itself correctly unswept.
  The `thmInfo`-only sweep is sound for statement protection because
  `collectAxioms` is transitive — shown, not asserted.
- **removed module** (identical driver body, `import Reactivegas.TraceTests`
  dropped, full S in env): exit 1, `tracked source modules that never
  reached the environment: Reactivegas.TraceTests`. Refinement recorded, not
  hidden: dropping `Reactivegas.Trace` alone stays green because the
  `Reactivegas` umbrella re-imports it transitively — the check is over
  environment reachability (what is actually swept), the semantically correct
  target.
- **truncated inventory** (full imports, env with `KelGroups.Tests` dropped):
  exit 1, two findings (walk-vs-discovery AND built-vs-discovery).
- **PANIC assertion**: the script's `grep -q "PANIC at"` predicate passes a
  clean pair, fires on a dirty pair, and passes the 1244-line green receipt.
- **Run A, added clean module through `just lean`**: full green with
  `tracked=27 walked=27` and `axiom-module Reactivegas.AuditAddedS2` listed —
  additions are reconciled, not evaded.
- **Run B, def-shaped poison through `just lean`**: exit 0 recorded HONESTLY
  as the limit that motivated the transitivity probe (a def no theorem uses
  is inert; see above).
- **Run B2, proof-poisoning through `just lean` (amendment follow-up ii,
  the amendment's justification)**: existing non-inversion theorem made
  `by sorry`, count unchanged — direction OK, inversion
  `declared=163 elaborated-backed=163 expected=163` → `inversion-audit: ok`
  (**the quota blind**), negative control ok, then
  `axiom-gate: Reactivegas.admin_admission_preservation_holds: depends on
  sorryAx` → recipe fails AT `check-lean-axioms`. Filed reverted
  byte-identical after (verified by `diff`).
- **Run C, A2′ constraint 3 through `just lean`**: valid added theorem
  (`trivial`) → full green with `declared=164 elaborated-backed=164`, no
  quota, per-file census line for the new module. Under the old quota this
  exact shape failed for the wrong reason (see RED).
- **Omit probe, A2′ constraint 4 (zero builds)**: theorem appended to source
  without rebuilding, census driver run directly → exit 1,
  `declared=164 elaborated-backed=163` plus the identity-level finding
  `Step.lean:480: auditOmittedProbe is declared in source but no theorem
  elaborated there`. File restored; `git status` clean.

## Build spend (budget 8 substantive)

Substantive (`lake build` that compiled): (1) cold 29-job full build on fresh
`.lake`; (2) RED inversion run (probe module); (3) Row B/C rename recompile;
(4) Run A `just lean`; (5) Run B `just lean`; (6) transitivity gate run;
(7) Run B2 `just lean` (poison recompile); (8) Run C `just lean`. Zero-build:
feasibility probes, R4 eval, removed/truncated/PANIC/omit probes, all gate
re-runs after (3) (no-op `lake build`s), and the final receipt `just lean`
(verified zero `Built …` lines). The amendment ack cost no build.

## Honest limits

1. Def-shaped axiom/sorry dependencies with no theorem use are outside the
   mandated `thmInfo` sweep (brief A1/**T**); proven inert via the
   transitivity control, not assumed.
2. The `B \ S` (unaccounted built module) branch never fired in any run; it
   is strictness-in-depth, reported as such.
3. `specs/62-one-membership-model/functions-model.md` still names the old
   `checkI57*` identifiers in historical prose — outside the fence,
   non-normative, left untouched.
4. Stale `.lake` artifacts still contain old symbol names (build cache only;
   regenerated by any fresh build; cold provenance recorded at seat-take).
5. No separate green receipt was taken between the Step.lean restore and the
   final run because the restore was byte-identical to built content — the
   final `just lean` recompiled nothing, which is itself the evidence.
