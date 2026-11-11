# CORRECTIONS-008 — three static corrections per NOTE-008 (no execution)

Static only: source reads quoted verbatim. No build/probe/elaboration run. No
model, statement, or fence change. Worktree untouched.

## C1. `comune_not_a_member_of_reach` — premise transport, not runtime producer

R5's assignment to Validate's direct-admission guard and production boot is
WITHDRAWN. At source (`Reactivegas/Invariants.lean:1141`):

```lean
theorem comune_not_a_member_of_reach {s : State} (hr : Reach view auth s) :
    comune_not_a_member view := by
  induction hr with
  | boot h => exact h
  | trans _ _ ih => exact ih
```

with (`Predicates.lean:97+`) `| boot (h : comune_not_a_member view) : Reach
view auth State.empty`. `view` is a fixed parameter of `Reach`; the boot case
returns the premise (`exact h`); the step case propagates it (`exact ih`).
Statement and proof never touch `validateDirectAdmission` or `boot`. Corrected
ownership: NONE as a runtime property — it transports the boot premise. **No
guard-mutant pairing is manufactured for it** (none was executed; none is
planned).

**Statement-to-production gap recorded for S5:** production never ESTABLISHES
`comune_not_a_member view` for a view — it is assumed at boot and carried by a
fixed parameter. The theorem is TRUE and unchallenged; the implementation fence
is UNCHANGED (no production edit proposed or made). Owner of the gap: S5.

`comune_not_a_member_step` (private, `:883`) likewise ignores its step
hypothesis (`_hstep`, returns `h`) — owns no guard atom (already so mapped; now
with the body quoted). `solvent_preserved`'s comune limb therefore contributes
no atoms; its 14 economic atoms come solely from `credit_pledges_step`'s
per-constructor arms (R5 §2, unchanged).

## C2. Alias rows keep original statements; atoms without quota language

OP-32..38 as specified (breaking alias statements) is WITHDRAWN as production
kills: a changed theorem statement is not a production-definition mutation and
cannot test the original claim. Each alias KEEPS its original statement. Actual
semantic dependencies (call bodies verified): each root alias calls exactly its
`KelGroups.*` counterpart. Mapping corrected: alias rows inherit counterpart
atoms via shown dependence AND are not separately countable production kills —
a counterpart-body kill propagates through the call (one kill, not two); an
alias-surface break is a checker-class test, classified separately, never
summed with production kills.

"14 economic atoms": the phrase as a blanket property is withdrawn wherever it
lacks per-row showing. What stands, each with its shown evidence: the 14
inversions (one `hstep` literal each, grepped); `conservation_preserved` and
`step_authorized` (14 + 14 counted proof arms); every other row its own
conclusion-named atoms; solvent rows per proof-dependency map. No
one-per-constructor quota exists; no irrelevant pairing is kept.

## C3. M-elab corrected: rebuild, replace (hash-evidenced), then check

R5's M-elab is WITHDRAWN as specified: without rebuilding the mutated
dependency, the probe may load the baseline `.olean` while reporting for a
mutant. Corrected method M-elab* (per op): (1) copy baseline lib to run dir;
(2) apply the single-atom mutation to a scratch SOURCE copy; (3) REBUILD the
mutated dependency module(s) with `lean -o` into the run lib, REPLACING the
baseline artifact; (4) record sha256 of the replaced artifact before/after as
load evidence; (5) elaborate the dependent theorem module with
LEAN_PATH=run-first; (6) RED names the owning theorem or the op FAILS as a
kill (a GREEN is a finding, not coverage). Per op named and counted: 1 rebuild
+ 1 replacement (hash-evidenced) + 1 proof check = 2 targeted invocations.

Costs restated honestly: elaboration-layer per-op values are UNMEASURED —
marked ESTIMATE with basis (11 s TraceTests full-file elaboration is the only
measured elaboration of comparable scope, an upper REFERENCE, not a bound;
Build-2's 10 s was a different layer and is never cited for these). The first
executed op replaces estimates with measurements; ranged rows' "exact lines
re-read at execution time" is replaced by the filed targets (R5 §3 arm lines
for inversions; atom-grouped files for the rest) plus instrument authoring
counted INSIDE each op (static authoring, no extra op).

*End of CORRECTIONS-008. Fence unchanged; statements unchanged; proofs
unchallenged.*
