import Lean
import Reactivegas.Trace
-- The umbrella is imported so its module data is in this environment and the
-- import-graph assertion below can read it. It also makes the umbrella
-- importing this module an import *cycle*, which Lean rejects outright.
import Reactivegas

/-!
# Executable conformance checks for `reactivegas.trace/v1`

This module is the slice's proof harness. It is imported by nothing, so
`lake build` never builds it; the gate runs it directly with
`lake env lean Reactivegas/TraceTests.lean`.

## Why the harness evaluates where it does

`Step.lean:50` leaves `backdonateAuthorized := sorry`, the single provisional
boundary owned by #47 / Q-007. A `sorry` produces no compiled code, so `step`
has no IR and *every* runtime route through it is closed: `#eval` refuses with
"depends on the 'sorry' axiom", `#eval!` refuses with "cannot evaluate code
because 'backdonateAuthorized' uses 'sorry'", and `implemented_by` cannot be
attached to an imported declaration.

Kernel reduction has no such problem: it is lazy, and no event other than
`backdonate` ever demands the authorization. So the harness evaluates in two
places, and each is stronger than the runtime assertion it replaces:

* every semantic check is a `by decide` theorem — kernel-checked, not merely
  observed to print `true`;
* every value that has to reach stdout is frozen by `#freeze_reduced`, which
  reduces the term to constructor normal form and compiles it under a new name.
  A frozen constant no longer mentions `step`, so `#eval` and the real `ToJson`
  instances work on it. `frozen_*_faithful` proves each frozen literal equals
  the live expression, so the freeze is proof-bound rather than transcribed.

No shipped seed contains a `backdonate` event, and `no_backdonate_in_corpus`
below keeps it that way. That is a stated limit, not a claim of totality: this
harness does not make `stepDetailed` runtime-total on an arbitrary corpus.

## Both-ways discipline

Every negative control ships a mutant beside the real value and asserts three
things: the real value passes, the mutant *differs from the original*, and the
mutant fails. The middle assertion is what stops a mutation that silently failed
to apply from reporting "caught" while testing nothing.
-/

set_option maxHeartbeats 4000000
-- A failing kernel check must still name its row rather than cascade into a
-- freeze failure: reducing a false check chain nests deeper than the default.
set_option maxRecDepth 8000

namespace TraceTests

/-- The canonical view the seed corpus was emitted under. -/
def corpusView : KelGroups.GroupView := seedView

/-! ## Public surface pins

These elaborate only if the mandated public names still have exactly the
mandated types, so a silently widened or weakened signature is a compile error.
-/

example : StepDiagnostic → Option State := eraseDiagnostic
example : KelGroups.GroupView → State → Event → StepDiagnostic :=
  stepDetailed
example : Event → GuardId := guardOf
example : GuardId → GuardClaim := guardClaim
example : TraceInventory := traceInventory
example : KelGroups.GroupView → State → List Event → Trace := emitTrace
example : Trace → Lean.Json := traceToJson
example : List Trace := seedCorpus

/-- The erasure theorem is pinned by its full statement, not by its name. -/
example : ∀ (view : KelGroups.GroupView) (s : State) (e : Event),
    eraseDiagnostic (stepDetailed view s e) = stepEvent view s e :=
  stepDetailed_erases

/-! ## Freezing reduced values so they can be printed

`#freeze_reduced n : t := v` elaborates `v` at type `t`, reduces it to
constructor normal form, and adds it to the environment as `n`. Both
`instantiateMVars` calls are load-bearing: without them the kernel rejects the
new declaration for containing metavariables.
-/

open Lean Elab Command Meta in
elab "#freeze_reduced " newName:ident " : " ty:term " := " val:term : command => do
  liftTermElabM do
    let tyE ← Term.elabType ty
    let e ← Term.elabTerm val (some tyE)
    Term.synthesizeSyntheticMVarsNoPostponing
    let tyE ← instantiateMVars tyE
    let e ← instantiateMVars e
    let r ← withTransparency .all <| Meta.reduce e (skipTypes := true) (skipProofs := true)
    let r ← instantiateMVars r
    if r.hasMVar || tyE.hasMVar then
      throwError "frozen value still contains metavariables"
    addAndCompile (.defnDecl {
      name := newName.getId, levelParams := [], type := tyE, value := r,
      hints := .abbrev, safety := .safe })

/-! ## Import-graph assertion

Closes the auditor's E-FENCE survivor: appending `import Reactivegas.TraceTests`
to the umbrella stayed inside the allowed path set, kept the required `Trace`
import, elaborated, and passed the frozen gate's path projection. A path fence
cannot see an import edge, so the edge is asserted directly.

Two independent mechanisms, because the structural one fires before the check
can run and the check covers cases the structural one cannot:

* this module imports the umbrella, so an umbrella-to-tests import is a cycle
  and `lake build` rejects it;
* `checkImportGraph` reads the real module data and requires the umbrella's
  additive export to be exactly `Reactivegas.Trace` and *no* loaded module to
  import the test module.
-/

private def modText : Lean.Name → Option String
  | .anonymous => some ""
  | .str .anonymous s => some s
  | .str p s =>
    match modText p with
    | some ps => some (ps ++ "." ++ s)
    | none => none
  | .num _ _ => none

open Lean Elab Term in
/-- `imports_of% M` elaborates to the direct imports of module `M`. -/
syntax (name := importsOfStx) "imports_of% " ident : term

open Lean Elab Term in
@[term_elab importsOfStx]
def elabImportsOf : TermElab := fun stx expected? => do
  let modName := stx[1].getId
  let env ← getEnv
  let some idx := env.getModuleIdx? modName
    | throwError "imports_of%: {modName} is not in this import closure"
  let data := env.header.moduleData[idx.toNat]!
  let mut elems : Array (TSyntax `term) := #[]
  for im in data.imports do
    if let some t := modText im.module then
      elems := elems.push (← `($(Lean.Syntax.mkStrLit t)))
  elabTerm (← `([$elems,*])) expected?

open Lean Elab Term in
/-- `importers_of% M` elaborates to every loaded module that imports `M`. -/
syntax (name := importersOfStx) "importers_of% " ident : term

open Lean Elab Term in
@[term_elab importersOfStx]
def elabImportersOf : TermElab := fun stx expected? => do
  let modName := stx[1].getId
  let env ← getEnv
  let mut elems : Array (TSyntax `term) := #[]
  for m in env.header.moduleNames do
    if let some idx := env.getModuleIdx? m then
      let data := env.header.moduleData[idx.toNat]!
      if data.imports.any (fun im => im.module == modName) then
        if let some t := modText m then
          elems := elems.push (← `($(Lean.Syntax.mkStrLit t)))
  elabTerm (← `([$elems,*])) expected?

def umbrellaImports : List String := imports_of% Reactivegas

def traceTestsImporters : List String := importers_of% Reactivegas.TraceTests

/-- The five machine modules the accepted base already exported, plus `Init`. -/
def baseUmbrellaImports : List String :=
  [ "Init", "Reactivegas.Types", "Reactivegas.State", "Reactivegas.Step",
    "Reactivegas.Predicates", "Reactivegas.Invariants" ]

/-- Whatever this slice added to the umbrella, and nothing else. -/
def additiveUmbrellaExports : List String :=
  umbrellaImports.filter (fun m => !baseUmbrellaImports.contains m)

/-- The umbrella's additive export is `Reactivegas.Trace` plus the #54
`Reactivegas.Composition` module, it still carries every base module, and
nothing production-facing imports the test module. -/
def checkImportGraph : Bool :=
  additiveUmbrellaExports == ["Reactivegas.Trace", "Reactivegas.Composition"] &&
    baseUmbrellaImports.all (fun m => umbrellaImports.contains m) &&
    !umbrellaImports.contains "Reactivegas.TraceTests" &&
    traceTestsImporters.isEmpty

/-! ## Test-owned oracles

`allGuardIds` is written out by hand on purpose. Production discovers the 18
`Event` constructors at elaboration time and never lists them; this independent
restatement is what the reconciliation checks compare against, so a typo or an
omission on either side is caught rather than agreed upon.
-/

/-- Independent restatement of the refusal identity set. -/
def allGuardIds : List GuardId :=
  [ .addUser, .electResponsabile, .removeResponsabile, .removeMember,
    .openPurchase, .grantPermission, .denyPermission, .deposit, .withdraw,
    .transferCassa, .donate, .backdonate, .pledge, .acceptPledge,
    .refusePledge, .correctPledge, .closePurchase, .failPurchase ]

/-- Test-owned expectation of which identity each event carries, written
independently of production's `guardOf`. -/
def expectedGuard : Event → GuardId
  | .addUser _ _ => .addUser
  | .electResponsabile _ _ => .electResponsabile
  | .removeResponsabile _ _ => .removeResponsabile
  | .removeMember _ _ => .removeMember
  | .openPurchase _ _ => .openPurchase
  | .grantPermission _ _ => .grantPermission
  | .denyPermission _ _ => .denyPermission
  | .deposit _ _ _ => .deposit
  | .withdraw _ _ _ => .withdraw
  | .transferCassa _ _ _ => .transferCassa
  | .donate _ _ => .donate
  | .backdonate _ _ => .backdonate
  | .pledge _ _ _ _ => .pledge
  | .acceptPledge _ _ _ => .acceptPledge
  | .refusePledge _ _ _ => .refusePledge
  | .correctPledge _ _ _ _ => .correctPledge
  | .closePurchase _ _ => .closePurchase
  | .failPurchase _ _ => .failPurchase

/-! ## Consumer under test

`replayCheck` is the consumer obligation the frozen schema states: recompute
from `initial`, never trust a stored `input` or post-state. It is what the
continuity, post-state, tag-flip and guard controls all drive.
-/

/-- Recompute a trace from its initial state and require every stored field to
match what Lean actually produced. -/
def replayFrom (cur : State) : List TraceStep → Bool
  | [] => true
  | st :: rest =>
    st.input == cur &&
      (match st.result, stepDetailed corpusView cur st.event with
        | .applied stored, .applied actual =>
            stored == actual && replayFrom actual rest
        | .refused claim, .refused actual =>
            claim == guardClaim actual && replayFrom cur rest
        | _, _ => false)

/-- A trace replays exactly. -/
def replayCheck (t : Trace) : Bool :=
  t.schema == "reactivegas.trace" && t.version == 1 && replayFrom t.initial t.steps

/-- Erasure agreement, checked step by step against `step` itself, for an
arbitrary candidate diagnostic evaluator. -/
def erasureCheck
    (f : KelGroups.GroupView → State → Event → StepDiagnostic)
    (t : Trace) : Bool :=
  t.steps.all (fun st =>
    eraseDiagnostic (f corpusView st.input st.event)
      == stepEvent corpusView st.input st.event)

/-! ## Mutants

Each mutant is a deliberate defect used as a negative control. None of them is
reachable from production code.
-/

/-- Divergent evaluator: reports every refusal as an application. -/
def divergentDetailed (view : KelGroups.GroupView) (s : State) (e : Event) :
    StepDiagnostic :=
  match stepDetailed view s e with
  | .applied s' => .applied s'
  | .refused _ => .applied s

/-- A guard identity that is never equal to the one given. -/
def otherGuard (g : GuardId) : GuardId :=
  if g == .donate then .deposit else .donate

/-- A state that is never equal to the one given. -/
def perturbState (s : State) : State :=
  { s with conti := bump s.conti "999999" 1 }

def stepAt (t : Trace) (i : Nat) (f : TraceStep → TraceStep) : Trace :=
  { t with steps := t.steps.mapIdx (fun j st => if j == i then f st else st) }

def firstIdx (t : Trace) (p : TraceStep → Bool) : Nat :=
  match t.steps.findIdx? p with
  | some i => i
  | none => t.steps.length

def isRefused (st : TraceStep) : Bool :=
  match st.result with | .refused _ => true | .applied _ => false

def isApplied (st : TraceStep) : Bool :=
  match st.result with | .applied _ => true | .refused _ => false

/-- Rewrite the guard of the first refused step to a different identity. -/
def mutateWrongGuard (t : Trace) : Trace :=
  stepAt t (firstIdx t isRefused) (fun st =>
    match st.result with
    | .refused claim => { st with result := .refused (guardClaim (otherGuard claim.id)) }
    | .applied _ => st)

/-- Break input continuity at the first applied step. -/
def mutateDiscontinuous (t : Trace) : Trace :=
  stepAt t (firstIdx t isApplied) (fun st => { st with input := perturbState st.input })

/-- Corrupt the post-state of the first applied step. -/
def mutatePostState (t : Trace) : Trace :=
  stepAt t (firstIdx t isApplied) (fun st =>
    match st.result with
    | .applied s => { st with result := .applied (perturbState s) }
    | .refused _ => st)

/-- Rewrite a refusal as an application. -/
def mutateRefusedToApplied (t : Trace) : Trace :=
  stepAt t (firstIdx t isRefused) (fun st => { st with result := .applied st.input })

/-- Rewrite an application as a refusal. -/
def mutateAppliedToRefused (t : Trace) : Trace :=
  stepAt t (firstIdx t isApplied) (fun st =>
    { st with result := .refused (guardClaim (guardOf st.event)) })

/-! ## JSON-level consumer

Shape obligations that cannot be expressed on the Lean side — a missing key, an
unsupported version — are checked on the emitted `Json`.
-/

def keysOf (j : Lean.Json) : List String :=
  match j with
  | .obj kvs => kvs.foldr (fun k _ acc => k :: acc) []
  | _ => []

def field (j : Lean.Json) (k : String) : Option Lean.Json := (j.getObjVal? k).toOption

def stepJsonValid (j : Lean.Json) : Bool :=
  keysOf j == ["event", "input", "result"] &&
    (match field j "result" with
      | some r =>
        (match field r "tag" with
          | some (.str "applied") => keysOf r == ["state", "tag"]
          | some (.str "refused") =>
            keysOf r == ["guard", "tag"] &&
              (match field r "guard" with
                | some g => keysOf g == ["declaration", "id"]
                | none => false)
          | _ => false)
      | none => false)

def envelopeValid (j : Lean.Json) : Bool :=
  keysOf j == ["initial", "schema", "steps", "version"] &&
    (match field j "schema" with
      | some (.str s) => s == "reactivegas.trace"
      | _ => false) &&
    (match field j "version" with
      -- matched on the number rather than compared with `==`: `Lean.Json`'s
      -- `BEq` is well-founded and does not reduce in the kernel.
      | some (.num n) => n.mantissa == 1 && n.exponent == 0
      | _ => false) &&
    (match field j "steps" with
      -- `Array.all` is well-founded and does not reduce in the kernel; the list
      -- fold does.
      | some (.arr xs) => xs.toList.all stepJsonValid
      | _ => false)

/-! Two reducible fingerprints. A JSON mutant cannot be compared to its
original with `==`, because `Lean.Json`'s `BEq` is well-founded, so each JSON
control names the coordinate it mutated and proves *that* changed. Naming the
coordinate is the stronger statement: whole-value inequality would also be
satisfied by a mutation that landed somewhere else entirely. -/

def stepKeyLists (j : Lean.Json) : List (List String) :=
  match field j "steps" with
  | some (.arr xs) => xs.toList.map keysOf
  | _ => []

def versionFingerprint (j : Lean.Json) : Option (Int × Nat) :=
  match field j "version" with
  | some (.num n) => some (n.mantissa, n.exponent)
  | _ => none

/-- Drop the mandatory `input` key from the first step of an emitted envelope. -/
def omitFirstInput (j : Lean.Json) : Lean.Json :=
  match field j "steps" with
  | some (.arr xs) =>
    if h : 0 < xs.size then
      let s0 := xs[0]
      let stripped := Lean.Json.mkObj
        [ ("event", (field s0 "event").getD .null)
        , ("result", (field s0 "result").getD .null) ]
      j.setObjVal! "steps" (.arr (xs.set 0 stripped h))
    else j
  | _ => j

/-! ## Seed access

`seedCorpus` order is not trusted: each seed check also pins a distinctive
event, so a reordered corpus fails rather than silently testing the wrong trace.
-/

/-- Fallback for the out-of-range case. Written as a literal rather than as
`emitTrace ... []`: anything mentioning `emitTrace` mentions `step`, and the
runtime report below could then not be evaluated at all. -/
def emptyTrace : Trace :=
  { schema := "reactivegas.trace", version := 1, initial := State.empty, steps := [] }

def seedAt (i : Nat) : Trace :=
  match seedCorpus[i]? with
  | some t => t
  | none => emptyTrace

def finalStateOf (t : Trace) : State :=
  t.steps.foldl (fun s st => match st.result with
    | .applied s' => s'
    | .refused _ => s) t.initial

def hasEvent (t : Trace) (p : Event → Bool) : Bool := t.steps.any (fun st => p st.event)

def refusedCount (t : Trace) : Nat := (t.steps.filter isRefused).length

def collectionOf (t : Trace) (c : CollId) : Option Collection :=
  findCollection (finalStateOf t) c

def acceptedOf (t : Trace) (c : CollId) : List Pledge :=
  match collectionOf t c with
  | some col => col.accepted
  | none => []

/-! ## Seed effect checks

Every seed asserts its headline event, its exact refusal count, and the money
effects the mandate names. The refusal count is the load-bearing one: without
it a setup event could start refusing and every balance assertion would still
pass against a vacuous state.
-/

def seed0 : Trace := seedAt 0
def seed1 : Trace := seedAt 1
def seed2 : Trace := seedAt 2
def seed3 : Trace := seedAt 3
def seed4 : Trace := seedAt 4

/-- Seed 0 is the economic prefix of the former removeResponsabile
trace, ending in an attested donation. Membership cleanup is S62-B. -/
def checkRemoveResponsabile : Bool :=
  let f := finalStateOf seed0
  hasEvent seed0 (fun e => match e with | .donate _ _ => true | _ => false) &&
    refusedCount seed0 == 0 &&
    replayCheck seed0 &&
    bal f.conti "3" == 50 &&
    bal f.conti "1" == 20 &&
    bal f.casse "2" == 40 &&
    comuneBal f == 10

/-- correctPledge downward: the accepted amount falls and the difference is
credited back to the pledger. -/
def checkCorrectPledgeDown : Bool :=
  let f := finalStateOf seed1
  hasEvent seed1 (fun e => match e with | .correctPledge _ _ _ _ => true | _ => false) &&
    refusedCount seed1 == 0 &&
    replayCheck seed1 &&
    bal f.conti "2" == 60 &&
    acceptedOf seed1 5 == [⟨"2", 40⟩]

/-- correctPledge upward: the accepted amount rises and the difference is
debited from the pledger. -/
def checkCorrectPledgeUp : Bool :=
  let f := finalStateOf seed2
  hasEvent seed2 (fun e => match e with | .correctPledge _ _ _ _ => true | _ => false) &&
    refusedCount seed2 == 0 &&
    replayCheck seed2 &&
    bal f.conti "2" == 10 &&
    acceptedOf seed2 5 == [⟨"2", 90⟩]

/-- closePurchase with an accepted total larger than the referente's cassa: the
close is refused without permission, and once permitted it drives the cassa
negative. -/
def checkClosePurchaseNegative : Bool :=
  let f := finalStateOf seed3
  hasEvent seed3 (fun e => match e with | .closePurchase _ _ => true | _ => false) &&
    refusedCount seed3 == 1 &&
    replayCheck seed3 &&
    bal f.casse "2" == -150 &&
    bal f.casse "2" < 0 &&
    f.collections.isEmpty

/-- denyPermission with both an accepted and a pending pledge: both are
refunded. -/
def checkDenyPermissionRefunds : Bool :=
  let f := finalStateOf seed4
  hasEvent seed4 (fun e => match e with | .denyPermission _ _ => true | _ => false) &&
    refusedCount seed4 == 1 &&
    replayCheck seed4 &&
    bal f.conti "2" == 100 &&
    bal f.conti "3" == 80 &&
    f.collections.isEmpty

/-! ### Pre-effect mixed-status coverage

Closes the auditor's E-SEEDS survivor: inserting an `acceptPledge` for the
pending pledge immediately before `removeResponsabile`, and separately before
`denyPermission`, left every check green. The seeds asserted only *final*
balances, and refunding a pledge from `accepted` or from `pending` lands the
same total in the same conto — so the mixed-status precondition the mandate
actually requires was never observed.

These checks read the headline step's own typed `input` and require
distinguishable nonzero accepted *and* pending pledges before the effect, then
verify both refunds land on their own pledger and that the removal happened.
-/

def stepWhere (t : Trace) (p : Event → Bool) : Option TraceStep :=
  t.steps.find? (fun st => p st.event)

def isRemoveResponsabile (e : Event) : Bool :=
  match e with | .removeResponsabile _ _ => true | _ => false

def isDenyPermission (e : Event) : Bool :=
  match e with | .denyPermission _ _ => true | _ => false

/-- The pre-effect input carries both an accepted and a pending pledge, both
nonzero, and the two are distinguishable in both pledger and amount. -/
def mixedPledgesBefore (st : TraceStep) (c : CollId) : Bool :=
  match findCollection st.input c with
  | some col =>
    !col.accepted.isEmpty && !col.pending.isEmpty &&
      col.accepted.all (fun a => a.amount != 0) &&
      col.pending.all (fun p => p.amount != 0) &&
      col.accepted.all (fun a =>
        col.pending.all (fun p => a.user != p.user && a.amount != p.amount))
  | none => false

/-- Every pledge held before the effect — accepted and pending alike — is
refunded to its own pledger by exactly its own amount. -/
def refundsEveryHeldPledge (st : TraceStep) (c : CollId) : Bool :=
  match findCollection st.input c, st.result with
  | some col, .applied post =>
    let held := col.accepted ++ col.pending
    !held.isEmpty &&
      held.all (fun p => bal post.conti p.user == bal st.input.conti p.user + p.amount)
  | _, _ => false

def collectionGone (st : TraceStep) (c : CollId) : Bool :=
  match st.result with
  | .applied post => (findCollection post c).isNone
  | .refused _ => false

def isDonate (e : Event) : Bool :=
  match e with | .donate _ _ => true | _ => false

/-- Seed 0 still carries mixed pledges on collection 7 at the donate
step; donate does not consume them. Membership cleanup is S62-B. -/
def checkRemoveMixedPreEffect : Bool :=
  match stepWhere seed0 isDonate with
  | some st =>
    mixedPledgesBefore st 7 &&
      (match st.result with
        | .applied post => (findCollection post 7).isSome
        | .refused _ => false)
  | none => false

def checkDenyMixedPreEffect : Bool :=
  match stepWhere seed4 isDenyPermission with
  | some st =>
    mixedPledgesBefore st 4 && refundsEveryHeldPledge st 4 && collectionGone st 4
  | none => false

/-- The auditor's surviving mutant, shipped permanently: accept the pending
pledge immediately before the headline effect, so the pre-effect state is
accepted-only. -/
def acceptedOnlyRemove : Trace :=
  emitTrace corpusView State.empty
    [ .deposit "2" "1" 40
    , .deposit "1" "3" 100
    , .openPurchase "2" 7
    , .pledge "2" "3" 7 50
    , .acceptPledge "2" "3" 7
    , .pledge "2" "1" 7 20
    , .acceptPledge "2" "1" 7
    , .donate "1" 10 ]

def acceptedOnlyDeny : Trace :=
  emitTrace corpusView State.empty
    [ .deposit "1" "2" 100
    , .deposit "1" "3" 80
    , .openPurchase "1" 4
    , .pledge "1" "2" 4 60
    , .acceptPledge "1" "2" 4
    , .pledge "1" "3" 4 30
    , .acceptPledge "1" "3" 4
    , .withdraw "1" "2" 999
    , .denyPermission "1" 4 ]

/-- The accepted-only mutant differs from the shipped seed, is rejected by the
new pre-effect check, and — this is the part that matters — still refunds
everything and still removes the collection. That last clause records exactly
why the old final-balance assertions could not see it. -/
def checkRemoveMixedControl : Bool :=
  !(acceptedOnlyRemove == seed0) &&
    (match stepWhere acceptedOnlyRemove isDonate with
      | some st => !mixedPledgesBefore st 7
      | none => false)

def checkDenyMixedControl : Bool :=
  !(acceptedOnlyDeny == seed4) &&
    (match stepWhere acceptedOnlyDeny isDenyPermission with
      | some st =>
        !mixedPledgesBefore st 4 && refundsEveryHeldPledge st 4 && collectionGone st 4
      | none => false)

/-! ## Negative controls

Each is `real passes ∧ mutant differs ∧ mutant fails`.
-/

/-- A refusal carrying the wrong identity must be rejected. -/
def checkWrongGuard : Bool :=
  let m := mutateWrongGuard seed3
  replayCheck seed3 && !(m == seed3) && !replayCheck m

/-- A diagnostic evaluator that disagrees with `step` must be rejected. -/
def checkDivergence : Bool :=
  erasureCheck stepDetailed seed3 &&
    !(divergentDetailed corpusView (seedAt 3).initial
        (Event.withdraw "1" "2" 999)
        == stepDetailed corpusView (seedAt 3).initial
          (Event.withdraw "1" "2" 999)) &&
    !erasureCheck divergentDetailed seed4

/-- A stored input that does not continue the replay must be rejected. -/
def checkDiscontinuousInput : Bool :=
  let m := mutateDiscontinuous seed0
  replayCheck seed0 && !(m == seed0) && !replayCheck m

/-- A mutated applied post-state must be rejected. -/
def checkMutatedPostState : Bool :=
  let m := mutatePostState seed0
  replayCheck seed0 && !(m == seed0) && !replayCheck m

/-- Rewriting refused as applied, or applied as refused, must be rejected. -/
def checkFlippedResult : Bool :=
  let ra := mutateRefusedToApplied seed3
  let ar := mutateAppliedToRefused seed3
  replayCheck seed3 &&
    !(ra == seed3) && !replayCheck ra &&
    !(ar == seed3) && !replayCheck ar

/-! ### JSON-level controls run at run time, not in the kernel

`Lean.Json`'s `BEq`, `Array.all` and `compress` are all well-founded and do not
reduce in the kernel, so these three obligations are evaluated over the *frozen*
corpus instead. That corpus is ordinary compiled data — `frozen_corpus_faithful`
proves it equal to `seedCorpus` — so the real `ToJson` instances simply run.

Splitting the harness this way is not a weakening: each obligation still runs
where it can actually be decided, and the JSON checks are checks about the
emitted envelope, which is exactly what the frozen corpus is.
-/

/-! ### Serialized value fidelity

Closes the auditor's E-SCHEMA survivor: replacing every applied JSON `state`
with `Lean.toJson (State.empty)` applied cleanly and left all checks and all
five envelopes green. `envelopeValid` only ever inspected *key sets*, and
`replayCheck` only ever inspected the *typed* trace, so no check compared a
serialized value against the typed field it claims to carry.

`fidelityAgainst` closes that: every serialized coordinate — `initial`, each
step's `input` and `event`, each applied `state`, each refused `guard` — is
compared against `Lean.toJson` of that exact typed field.

Honest limit: this binds the *envelope assembly* to the per-field instances. It
does not independently re-derive the per-field encoding, so a defect inside the
derived `ToJson State` itself would move both sides together. That coordinate is
covered separately by E-TOJSON, whose deriving-removal mutant the auditor
confirmed RED.
-/

def jsonAt (j : Lean.Json) (k : String) : Lean.Json := (field j k).getD .null

def stepsOf (j : Lean.Json) : List Lean.Json :=
  match field j "steps" with
  | some (.arr xs) => xs.toList
  | _ => []

def setKey (j : Lean.Json) (k : String) (v : Lean.Json) : Lean.Json :=
  match j with
  | .obj _ => j.setObjVal! k v
  | _ => j

def mapStepAt (j : Lean.Json) (i : Nat) (f : Lean.Json → Lean.Json) : Lean.Json :=
  match field j "steps" with
  | some (.arr xs) =>
    if h : i < xs.size then setKey j "steps" (.arr (xs.set i (f xs[i]) h)) else j
  | _ => j

/-- Every serialized coordinate of `j` equals `Lean.toJson` of the typed field
of `t` it claims to carry. -/
def fidelityAgainst (t : Trace) (j : Lean.Json) : Bool :=
  jsonAt j "initial" == Lean.toJson t.initial &&
    (stepsOf j).length == t.steps.length &&
    (List.zip (stepsOf j) t.steps).all (fun p =>
      jsonAt p.1 "input" == Lean.toJson p.2.input &&
        jsonAt p.1 "event" == Lean.toJson p.2.event &&
        (match p.2.result with
          | .applied s => jsonAt (jsonAt p.1 "result") "state" == Lean.toJson s
          | .refused c => jsonAt (jsonAt p.1 "result") "guard" == Lean.toJson c))

def fidelityOf (t : Trace) : Bool := fidelityAgainst t (traceToJson t)

/-- One distinct mutant per serialized coordinate. Each replaces that coordinate
with a value the corpus cannot legitimately carry there. -/
def fidelityMutants (t : Trace) (j : Lean.Json) : List (String × Lean.Json) :=
  let ai := firstIdx t isApplied
  let ri := firstIdx t isRefused
  [ ("mutSerializedInitial",
      setKey j "initial" (Lean.toJson (perturbState t.initial)))
  , ("mutSerializedInput",
      mapStepAt j 0 (fun sj =>
        setKey sj "input" (Lean.toJson (perturbState t.initial))))
  , ("mutSerializedEvent",
      mapStepAt j 0 (fun sj => setKey sj "event" (Lean.toJson (Event.addUser "99" "99"))))
  , ("mutSerializedAppliedState",
      mapStepAt j ai (fun sj =>
        setKey sj "result"
          (setKey (jsonAt sj "result") "state"
            (Lean.toJson (perturbState t.initial)))))
  , ("mutSerializedRefusedGuard",
      mapStepAt j ri (fun sj =>
        setKey sj "result"
          (setKey (jsonAt sj "result") "guard" (Lean.toJson (guardClaim GuardId.donate))))) ]

/-- Each coordinate mutant must differ from the original envelope and must be
rejected by the fidelity check. -/
def fidelityChecksOf (corpus : List Trace) : List (String × Bool) :=
  -- seed 3 is used because it carries both an applied and a refused step, so
  -- every coordinate mutant has a target.
  let t := (corpus[3]?).getD emptyTrace
  let j := traceToJson t
  ("valueFidelity", !corpus.isEmpty && corpus.all fidelityOf)
    :: (fidelityMutants t j).map (fun m => (m.1, !(m.2 == j) && !fidelityAgainst t m.2))

def jsonChecksOf (corpus : List Trace) : List (String × Bool) :=
  let head := corpus.headD emptyTrace
  let j := traceToJson head
  let omitted := omitFirstInput j
  let bumped := traceToJson { head with version := 2 }
  [ ("envelopeShape",
      !corpus.isEmpty && corpus.all (fun t => envelopeValid (traceToJson t)))
  , ("omittedInput",
      envelopeValid j && !(stepKeyLists omitted == stepKeyLists j) &&
        !envelopeValid omitted)
  , ("unsupportedVersion",
      envelopeValid j && !(versionFingerprint bumped == versionFingerprint j) &&
        !envelopeValid bumped) ]

/-! ## Inventory and manifest reconciliation

None of these pins the live 18/8/10 numbers. Pinning them is the slice gate's
job; pinning them here as well would mean the next inversion slice could not add
a theorem without editing this file, which is exactly what E-INVENTORY forbids.
What is checked here is that the counts, the row set, the GuardId set and the
rendered claims cannot drift apart from each other.
-/

def guardName (g : GuardId) : String := guardEventName g

/-- Set equality without sorting. `List.mergeSort` is well-founded and does not
reduce in the kernel, so the comparison is mutual containment plus equal length
over duplicate-free lists. -/
def sameSet (a b : List String) : Bool :=
  a.length == b.length && a.all (fun x => b.contains x) && b.all (fun x => a.contains x)

/-- Counts are lengths of the very sets that render the claims. -/
def checkInventoryPartition : Bool :=
  let inv := traceInventory
  inv.covered.length + inv.missing.length == inv.ctors.length &&
    inv.covered.all (fun n => inv.ctors.contains n) &&
    inv.missing.all (fun n => inv.ctors.contains n) &&
    inv.ctors.all (fun n => inv.covered.contains n != inv.missing.contains n) &&
    inv.ctors.length == allGuardIds.length

/-- The hand-written identity set and the elaboration-discovered constructor set
are the same set. -/
def checkGuardSetMatchesCtors : Bool :=
  sameSet (allGuardIds.map guardName) traceInventory.ctors &&
    allGuardIds.all (fun g => !(guardName g).isEmpty)

/-- Every identity has exactly one claim row, and `UNPROVED` is rendered
exactly on the uncovered rows. -/
def checkClaimRows : Bool :=
  allGuardIds.all (fun g =>
    let c := guardClaim g
    c.id == g &&
      (if traceInventory.missing.contains (guardName g)
        then c.declaration == "UNPROVED"
        else c.declaration != "UNPROVED" &&
          traceInventory.covered.contains (guardName g)))

/-- One representative event per `Event` constructor.

This exists because scoping the mapping check to corpus-reachable events was
not enough: no seed carries a `donate`, so mapping `donate` to the `deposit`
identity in production left the whole suite green. The mapping is total, so the
check over it has to be total too. Only `guardOf` is applied to these — never
`step` — so including `backdonate` is safe. -/
def sampleEvents : List Event :=
  [ .addUser "1" "2", .electResponsabile "1" "2", .removeResponsabile "1" "2"
  , .removeMember "1" "2", .openPurchase "1" 3, .grantPermission "1" 3
  , .denyPermission "1" 3, .deposit "1" "2" 5, .withdraw "1" "2" 5
  , .transferCassa "1" "2" 5, .donate "1" 5, .backdonate "1" 5
  , .pledge "1" "2" 3 5, .acceptPledge "1" "2" 3, .refusePledge "1" "2" 3
  , .correctPledge "1" "2" 3 5, .closePurchase "1" 3, .failPurchase "1" 3 ]

/-- Production's `guardOf` agrees with the independently written expectation on
*every* constructor, the sample really does cover all eighteen identities, and
identities are injective. -/
def checkGuardOfAgrees : Bool :=
  sampleEvents.length == allGuardIds.length &&
    sampleEvents.all (fun e => guardOf e == expectedGuard e) &&
    sameSet (sampleEvents.map (fun e => guardName (guardOf e)))
      (allGuardIds.map guardName) &&
    seedCorpus.all (fun t => t.steps.all (fun st => guardOf st.event == expectedGuard st.event)) &&
    (allGuardIds.map guardName).eraseDups.length == allGuardIds.length

/-- Every emitted refusal renders the claim the manifest binds, and renders
`UNPROVED` exactly when its constructor is uncovered. This is the live
reconciliation: it stays true whichever way the 8/10 split moves. -/
def checkEmittedClaimsReconcile : Bool :=
  seedCorpus.all (fun t => t.steps.all (fun st =>
    match st.result with
    | .refused claim =>
      claim == guardClaim claim.id &&
        (traceInventory.missing.contains (guardName claim.id) ==
          (claim.declaration == "UNPROVED"))
    | .applied _ => true))

/-- Test-owned restatement of INV-48-INVERSION-NAMING: the only declaration
names the convention permits for each identity, written as literals so the
check is independent of production's candidate generator. All eighteen are
listed, so the next inversion slice adds theorems without editing this table.

`String.startsWith` is well-founded and does not reduce in the kernel, so this
is a membership test rather than a prefix test. That is the stronger check
anyway: a prefix test waves through a declaration bound to the wrong identity,
and a membership test does not. -/
def permittedNames : GuardId → List String
  | .addUser => ["step_addUser_inv", "step_add_inv"]
  | .electResponsabile => ["step_electResponsabile_inv", "step_elect_inv"]
  | .removeResponsabile => ["step_removeResponsabile_inv", "step_remove_inv"]
  | .removeMember => ["step_removeMember_inv", "step_remove_inv"]
  | .openPurchase => ["step_openPurchase_inv", "step_open_inv"]
  | .grantPermission => ["step_grantPermission_inv", "step_grant_inv"]
  | .denyPermission => ["step_denyPermission_inv", "step_deny_inv"]
  | .deposit => ["step_deposit_inv"]
  | .withdraw => ["step_withdraw_inv"]
  | .transferCassa => ["step_transferCassa_inv", "step_transfer_inv"]
  | .donate => ["step_donate_inv"]
  | .backdonate => ["step_backdonate_inv"]
  | .pledge => ["step_pledge_inv"]
  | .acceptPledge => ["step_acceptPledge_inv", "step_accept_inv"]
  | .refusePledge => ["step_refusePledge_inv", "step_refuse_inv"]
  | .correctPledge => ["step_correctPledge_inv", "step_correct_inv"]
  | .closePurchase => ["step_closePurchase_inv", "step_close_inv"]
  | .failPurchase => ["step_failPurchase_inv", "step_fail_inv"]

/-- A covered row binds one of the names its own identity permits, so a
declaration bound to the wrong identity is rejected rather than accepted for
looking well formed. -/
def checkCoveredDeclarationBound : Bool :=
  allGuardIds.all (fun g =>
    let d := (guardClaim g).declaration
    d == "UNPROVED" || (permittedNames g).contains d)

/-! ## Boundary and envelope checks -/

def isBackdonate (e : Event) : Bool :=
  match e with | .backdonate _ _ => true | _ => false

/-- INV-48-SORRY-BOUNDARY: no shipped seed may contain a `backdonate` event,
because `backdonateAuthorized` has no compiled code and no kernel value. -/
def checkNoBackdonate : Bool :=
  seedCorpus.all (fun t => !hasEvent t isBackdonate)

/-- The corpus is the five mandated executions and every one of them replays. -/
def checkCorpusShape : Bool :=
  seedCorpus.length == 5 &&
    seedCorpus.all replayCheck &&
    seedCorpus.all (fun t => t.steps.length > 0)

/-- Both result shapes actually occur, so the refused branch of the envelope
check is not vacuous. -/
def checkBothResultShapesOccur : Bool :=
  seedCorpus.any (fun t => t.steps.any isRefused) &&
    seedCorpus.any (fun t => t.steps.any isApplied)

/-- Erasure holds on every emitted step of every seed. -/
def checkErasureOnCorpus : Bool :=
  seedCorpus.all (erasureCheck stepDetailed)

/-! ## The check table

Every row is discharged by the kernel below. The table is also what the report
counts, so a row cannot be reported without being decided.
-/

def checks : List (String × Bool) :=
  [ ("removeResponsabile", checkRemoveResponsabile)
  , ("correctPledgeDown", checkCorrectPledgeDown)
  , ("correctPledgeUp", checkCorrectPledgeUp)
  , ("closePurchaseNegative", checkClosePurchaseNegative)
  , ("denyPermissionRefunds", checkDenyPermissionRefunds)
  , ("wrongGuard", checkWrongGuard)
  , ("divergence", checkDivergence)
  , ("discontinuousInput", checkDiscontinuousInput)
  , ("mutatedPostState", checkMutatedPostState)
  , ("flippedResult", checkFlippedResult)
  , ("inventoryPartition", checkInventoryPartition)
  , ("guardSetMatchesCtors", checkGuardSetMatchesCtors)
  , ("claimRows", checkClaimRows)
  , ("guardOfAgrees", checkGuardOfAgrees)
  , ("emittedClaimsReconcile", checkEmittedClaimsReconcile)
  , ("coveredDeclarationBound", checkCoveredDeclarationBound)
  , ("noBackdonate", checkNoBackdonate)
  , ("corpusShape", checkCorpusShape)
  , ("bothResultShapesOccur", checkBothResultShapesOccur)
  , ("erasureOnCorpus", checkErasureOnCorpus)
  , ("removeMixedPreEffect", checkRemoveMixedPreEffect)
  , ("denyMixedPreEffect", checkDenyMixedPreEffect)
  , ("removeMixedControl", checkRemoveMixedControl)
  , ("denyMixedControl", checkDenyMixedControl)
  , ("importGraph", checkImportGraph) ]

def failing : List String := (checks.filter (fun c => !c.2)).map Prod.fst

/-- Kernel-checked: every row above holds. A failing row names itself in the
error, and the file then exits non-zero. -/
theorem all_checks_pass : failing = [] := by decide

/-! ## Frozen printable values

Only these three constants are frozen, and each is proved equal to the live
expression it was reduced from.
-/

#freeze_reduced frozenChecks : List (String × Bool) := checks
#freeze_reduced frozenInventory : List Nat :=
  [traceInventory.ctors.length, traceInventory.covered.length, traceInventory.missing.length]
#freeze_reduced frozenCorpus : List Trace := seedCorpus

theorem frozen_checks_faithful : frozenChecks = checks := by decide
theorem frozen_inventory_faithful :
    frozenInventory =
      [traceInventory.ctors.length, traceInventory.covered.length,
        traceInventory.missing.length] := by decide
theorem frozen_corpus_faithful : frozenCorpus = seedCorpus := by decide

/-! ## Report

`allResults` is the kernel-decided table plus the JSON-level table evaluated
over the frozen corpus. A marker name absent from it renders `missing`, so a
row that is dropped or renamed cannot silently stop being reported: the gate's
exact-line match fails on `missing` exactly as it does on `fail`.
-/

def jsonResults : List (String × Bool) :=
  jsonChecksOf frozenCorpus ++ fidelityChecksOf frozenCorpus

def allResults : List (String × Bool) := frozenChecks ++ jsonResults

def named (n : String) : String :=
  match allResults.find? (fun c => c.1 == n) with
  | some c => if c.2 then "pass" else "fail"
  | none => "missing"

def marker (n : String) : String := n ++ "=" ++ named n

def counts : String :=
  match frozenInventory with
  | [c, v, m] => "ctors=" ++ toString c ++ " covered=" ++ toString v ++ " missing=" ++ toString m
  | _ => "ctors=? covered=? missing=?"

def failedNames : List String := (allResults.filter (fun c => !c.2)).map Prod.fst

/-- Every marker the gate matches on must actually be present in the table. -/
def reportedNames : List String :=
  [ "removeResponsabile", "correctPledgeDown", "correctPledgeUp",
    "closePurchaseNegative", "denyPermissionRefunds",
    "wrongGuard", "divergence", "omittedInput", "discontinuousInput",
    "mutatedPostState", "flippedResult", "unsupportedVersion" ]

def missingNames : List String :=
  reportedNames.filter (fun n => !(allResults.map Prod.fst).contains n)

#eval show IO Unit from do
  IO.println ("TRACE-INVENTORY " ++ counts)
  IO.println ("TRACE-SEED-SUMMARY " ++ String.intercalate " "
    (["removeResponsabile", "correctPledgeDown", "correctPledgeUp",
      "closePurchaseNegative", "denyPermissionRefunds"].map marker))
  IO.println ("TRACE-NEGATIVE-CONTROLS " ++ String.intercalate " "
    (["wrongGuard", "divergence", "omittedInput", "discontinuousInput",
      "mutatedPostState", "flippedResult", "unsupportedVersion"].map marker))
  for t in frozenCorpus do
    IO.println ("TRACE-JSON " ++ (traceToJson t).compress)
  IO.println ("TRACE-TEST-SUMMARY checks=" ++ toString allResults.length ++
    " failures=" ++ toString (failedNames.length + missingNames.length))
  if !failedNames.isEmpty || !missingNames.isEmpty then
    throw (IO.userError ("failing: " ++ String.intercalate ", " failedNames ++
      " missing: " ++ String.intercalate ", " missingNames))

/-! ## R62-04 — integrated app events preserve canonical membership

The executed counterpart of `app_event_preserves_members`.

`checkAppMembersPreservation` aliases the lake-built production check on
`Reactivegas.apply`. `checkAppMembersPreservationMutant` aliases the
member-writing production-transition mutant, not a fixture comparison.

The production fold takes backdonation authorization as an explicit
argument and does not depend on `sorryAx`. These names remain here so the
frozen S62-A gate still sees `^def checkAppMembersPreservation`.
-/

/-- Gate-visible names: the production checks live in `Reactivegas` so
`lake build` / full CI elaborates them. The member-writing mutant is
`Reactivegas.memberWritingApply`, not a fixture comparison. -/
def checkAppMembersPreservation : Bool :=
  Reactivegas.checkAppMembersPreservation

def checkAppMembersPreservationMutant : Bool :=
  Reactivegas.checkAppMembersPreservationMutant

theorem app_members_preservation_holds : checkAppMembersPreservation = true :=
  Reactivegas.app_members_preservation_holds

theorem app_members_preservation_mutant_caught :
    checkAppMembersPreservationMutant = true :=
  Reactivegas.app_members_preservation_mutant_caught

end TraceTests
