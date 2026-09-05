import Lean
import Reactivegas.Invariants

/-!
# `reactivegas.trace/v1` producer

The additive Lean end of the frozen cross-repository interchange format: a
typed diagnostic over the existing `step`, a stable refusal identity per event
constructor, a live reconciliation between those identities and the accepted
inversion theorems, and the exact v1 JSON envelope.

Nothing here is imported by the machine. The dependency direction is one-way:
this module imports `Reactivegas.Invariants` and therefore the whole machine,
and no existing module imports it back.

## What this module does not do

It does not decide anything economic. `stepDetailed` evaluates the existing
`step` and reads its `Option`; the guard and effect chain is never restated
here, and `stepDetailed_erases` proves the two agree.

It also does not explain *why* an event was refused. `step` collapses several
guard conditions per constructor into one `none`, so a refusal names the event
constructor and the declaration its claim binds — never a prose cause.

## JSON

`State`, `Event` and `GuardId` reach the wire through Lean's own `ToJson`
deriving handler, so there is no second payload generator to drift. The two
hand-written instances below exist only to place the frozen `tag`
discriminator; their payloads are `Lean.toJson` of the derived instances.
-/

/-! ### JSON for the machine types

Derived from the existing declarations, so `Types.lean` and `State.lean` stay
untouched. These have to precede every instance below that serialises a
`State` or an `Event`.
-/

-- ToJson/FromJson for Pledge/Collection/Vote/State/Event live in
-- `Reactivegas.Invariants` (imported above) so the corpus and this
-- emitter share one instance set.

/-! ### Stable refusal identity -/

/-- Stable typed refusal identity, total over the `Event` constructors.

The derived `ToJson` renders each identity as its own constructor name, which
*is* the `guard.id` string the frozen schema specifies. -/
inductive GuardId where
  | openPurchase | grantPermission | denyPermission | deposit | withdraw
  | transferCassa | donate | backdonate | pledge | acceptPledge
  | refusePledge | correctPledge | closePurchase | failPurchase
deriving DecidableEq, Lean.ToJson

/-- The refusal identity of an event. Exhaustive by construction: a new `Event`
constructor makes this fail to compile rather than fall through to a default. -/
def guardOf : Event → GuardId
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

/-- The `Event` constructor name an identity belongs to.

Read back out of the identity's own `ToJson` rendering, so the wire `guard.id`
and the manifest key are the same string by construction and cannot drift.
This reads structured `Json`, not a display rendering. -/
def guardEventName (g : GuardId) : String :=
  match Lean.toJson g with
  | .str s => s
  | _ => ""

/-! ### Typed diagnostic over the existing step -/

/-- Exactly an applied post-state or a refusal identity. -/
inductive StepDiagnostic where
  | applied (state : State)
  | refused (guard : GuardId)
deriving DecidableEq

/-- Forget the diagnostic and recover the plain `step` result. -/
def eraseDiagnostic : StepDiagnostic → Option State
  | .applied s => some s
  | .refused _ => none

/-- The diagnostic evaluator. It calls the existing `step` and classifies its
`Option`; it does not reimplement any guard or effect. -/
def stepDetailed (view : KelGroups.GroupView) (s : State) (e : Event)
    (auth : BackdonateAuth) : StepDiagnostic :=
  match stepEvent view s e auth with
  | some s' => .applied s'
  | none => .refused (guardOf e)

/-- The diagnostic carries no information `step` does not, and loses none:
erasing it returns exactly `step`. Without this a refusal explanation would be
a second implementation rather than provable Lean output. -/
theorem stepDetailed_erases (view : KelGroups.GroupView) (s : State)
    (e : Event) (auth : BackdonateAuth) :
    eraseDiagnostic (stepDetailed view s e auth) = stepEvent view s e auth := by
  unfold stepDetailed
  cases h : stepEvent view s e auth with
  | none => simp [eraseDiagnostic]
  | some s' => simp [eraseDiagnostic]

/-! ### Accepted-inversion reconciliation

The manifest is discovered from the environment at elaboration time. Neither
the constructors nor the uncovered ones are written down anywhere in this
file: adding a correctly named accepted inversion shrinks `missing` with no
edit here.

An inversion for constructor `c` is a *theorem* named `step_<c>_inv` or
`step_<stem>_inv`, where `stem` is the leading lowercase run of `c`, whose
statement mentions both `step` and `Event.c`. Requiring the statement to
mention the specific constructor is what makes the rule collision-safe:
`step_close_inv` can bind `closePurchase` and nothing else,
because it can only mention one constructor.

Candidates resolve by unqualified name, so a theorem binds whether it
elaborates bare or under a namespace; the bound declaration always renders
the unqualified candidate.

The limit is declared, not hidden: this establishes that an accepted
declaration of the right shape exists and is bound, not that its conclusion is
a genuine guard inversion. Proving that belongs to the inversion slice that
owns the new theorems.
-/

/-- Does an expression mention this constant? -/
private def mentionsConst (e : Lean.Expr) (n : Lean.Name) : Bool :=
  (e.find? (fun x => x.isConstOf n)).isSome

/-- The candidate accepted-inversion names for one constructor. -/
private def inversionCandidates (ctor : String) : List String :=
  let stem := ctor.takeWhile (fun ch => !ch.isUpper)
  ["step_" ++ ctor ++ "_inv", "step_" ++ stem ++ "_inv"]

open Lean Elab Command in
/-- `inversion_manifest% Event` elaborates to one row per constructor of the
named inductive, each carrying the accepted inversion declaration bound to it
or `none`. -/
syntax (name := inversionManifestStx) "inversion_manifest% " ident : term

open Lean Elab Term in
@[term_elab inversionManifestStx]
def elabInversionManifest : TermElab := fun stx expected? => do
  let indName ← resolveGlobalConstNoOverload stx[1]
  let env ← getEnv
  let some (.inductInfo iv) := env.find? indName
    | throwError "inversion_manifest%: {indName} is not an inductive type"
  -- Every accepted-inversion-shaped theorem in the environment, keyed by
  -- unqualified (last-component) name and swept once. A candidate binds
  -- whether its theorem elaborates bare or under any namespace; the bound
  -- declaration always renders the unqualified candidate. The match is
  -- total: non-string last components (e.g. numeric) cannot be candidates
  -- and are skipped, so no declaration name can panic the elaborator.
  let allThms : Array (String × TheoremVal) :=
    env.constants.toList.toArray.filterMap fun (n, ci) =>
      match ci with
      | .thmInfo ti =>
        match n with
        | .str _ s =>
          if "step_".isPrefixOf s && s.endsWith "_inv" then some (s, ti)
          else none
        | _ => none
      | _ => none
  let mut rows : Array (TSyntax `term) := #[]
  for ctor in iv.ctors do
    let short := ctor.getString!
    let mut bound : Option String := none
    for cand in inversionCandidates short do
      if bound.isNone
          && allThms.any fun (s, ti) =>
            s == cand
              && (mentionsConst ti.type (Lean.Name.mkSimple "stepEvent")
                || mentionsConst ti.type (Lean.Name.mkSimple "step"))
              && mentionsConst ti.type ctor then
        bound := some cand
    let key := Lean.Syntax.mkStrLit short
    let row ← match bound with
      | none => `(($key, (none : Option String)))
      | some d => `(($key, some $(Lean.Syntax.mkStrLit d)))
    rows := rows.push row
  elabTerm (← `([$rows,*])) expected?

/-- One row per `Event` constructor: its name, and the accepted inversion
declaration bound to it if there is one. Computed from the live environment. -/
def guardManifest : List (String × Option String) := inversion_manifest% Event

/-- The declaration a constructor's claim binds, or `UNPROVED`. -/
private def declarationOf (ctor : String) : String :=
  match guardManifest.find? (fun row => row.1 == ctor) with
  | some (_, some d) => d
  | _ => "UNPROVED"

/-- A claim row: the stable identity and the declaration it binds. `UNPROVED`
is the rendering for an identity with no accepted inversion. -/
structure GuardClaim where
  id : GuardId
  declaration : String
deriving DecidableEq

instance : Lean.ToJson GuardClaim where
  toJson c := Lean.Json.mkObj
    [ ("id", Lean.toJson c.id)
    , ("declaration", Lean.Json.str c.declaration) ]

/-- The claim row for an identity. Every identity has exactly one. -/
def guardClaim (id : GuardId) : GuardClaim :=
  { id := id, declaration := declarationOf (guardEventName id) }

/-- Coverage inventory. The counts are lengths of these very sets, and the sets
are the same rows that render the claims, so counts and rows cannot drift
independently. -/
structure TraceInventory where
  ctors : List String
  covered : List String
  missing : List String

/-- The live inventory, partitioned from the discovered manifest. -/
def traceInventory : TraceInventory :=
  { ctors := guardManifest.map Prod.fst
  , covered := (guardManifest.filter (fun r => r.2.isSome)).map Prod.fst
  , missing := (guardManifest.filter (fun r => r.2.isNone)).map Prod.fst }

/-! ### The frozen v1 envelope -/

/-- A step outcome: the exact post-state, or the refusal claim. -/
inductive TraceResult where
  | applied (state : State)
  | refused (guard : GuardClaim)
deriving DecidableEq

/-- The `tag` discriminator is the frozen schema's, so it is placed here rather
than derived. The payloads are the derived instances. -/
instance : Lean.ToJson TraceResult where
  toJson
    | .applied s => Lean.Json.mkObj [("tag", Lean.Json.str "applied"), ("state", Lean.toJson s)]
    | .refused g => Lean.Json.mkObj [("tag", Lean.Json.str "refused"), ("guard", Lean.toJson g)]

/-- One replayable step. `input` is retained even though it is redundant: the
schema requires a consumer to check it against the state it recomputes, and
never to trust it as authority. -/
structure TraceStep where
  input : State
  event : Event
  result : TraceResult
deriving DecidableEq

/-- The v1 envelope. -/
structure Trace where
  schema : String
  version : Nat
  initial : State
  steps : List TraceStep
deriving DecidableEq
deriving instance Lean.ToJson for TraceStep
deriving instance Lean.ToJson for Trace

/-- Evaluate the events in order, keeping the state unchanged across a refusal
so that the next step's `input` stays continuous. -/
private def emitSteps (view : KelGroups.GroupView) (s : State)
    (auth : BackdonateAuth) : List Event → List TraceStep
  | [] => []
  | e :: rest =>
    match stepDetailed view s e auth with
    | .applied s' => ⟨s, e, .applied s'⟩ :: emitSteps view s' auth rest
    | .refused g => ⟨s, e, .refused (guardClaim g)⟩ :: emitSteps view s auth rest

/-- Emit the v1 trace of running `events` from `initial` under a fixed
canonical view. Authorization is explicit; the seed corpus passes a
refusing probe and contains no backdonate event. -/
def emitTrace (view : KelGroups.GroupView) (initial : State)
    (events : List Event) (auth : BackdonateAuth) : Trace :=
  { schema := "reactivegas.trace"
  , version := 1
  , initial := initial
  , steps := emitSteps view initial auth events }

/-- The envelope as JSON. -/
def traceToJson (trace : Trace) : Lean.Json := Lean.toJson trace

/-! ### Seed corpus

The four high-risk classes the frozen format requires, in five executions.
`correctPledge` needs two because the downward and upward settlements move
money in opposite directions.

No seed contains a `backdonate` event. Authorization is an explicit
argument; the seed corpus passes a refusing probe so backdonation is
not evaluated here.
-/

/-- Canonical view for the seed corpus: two admins and one ordinary
member. Membership is not grown by vote-local or economic events. -/
def seedView : KelGroups.GroupView :=
  { members :=
      [ ("1", { key := "1", email := "1@trace",
                roles := [KelGroups.Role.adminRole KelGroups.Admin.publicAdmin] })
      , ("2", { key := "2", email := "2@trace",
                roles := [KelGroups.Role.adminRole KelGroups.Admin.publicAdmin] })
      , ("3", { key := "3", email := "3@trace", roles := [] }) ] }

/-- A mixed-status collection (one accepted pledge, one pending) ending in an
attested donation. Membership cleanup is not an economic event at all: it is
the sealed consequence of a base transition, exercised by
`Reactivegas.checkAdminDepartureCleanup`. -/
private def seedDonationPrefix : List Event :=
  [ .deposit "2" "1" 40
  , .deposit "1" "3" 100
  , .openPurchase "2" 7
  , .pledge "2" "3" 7 50
  , .acceptPledge "2" "3" 7
  , .pledge "2" "1" 7 20
  , .donate "1" 10 ]

/-- An accepted pledge corrected downward. -/
private def seedCorrectPledgeDown : List Event :=
  [ .deposit "1" "2" 100
  , .openPurchase "1" 5
  , .pledge "1" "2" 5 60
  , .acceptPledge "1" "2" 5
  , .correctPledge "1" "2" 5 40 ]

/-- The same collection corrected upward. -/
private def seedCorrectPledgeUp : List Event :=
  [ .deposit "1" "2" 100
  , .openPurchase "1" 5
  , .pledge "1" "2" 5 60
  , .acceptPledge "1" "2" 5
  , .correctPledge "1" "2" 5 90 ]

/-- A closure large enough to drive the referente's cassa negative. The close
is attempted once before permission is granted, so the corpus carries a refused
step whose identity has an accepted inversion. -/
private def seedClosePurchaseNegative : List Event :=
  [ .deposit "1" "3" 200
  , .openPurchase "2" 9
  , .pledge "2" "3" 9 150
  , .acceptPledge "2" "3" 9
  , .closePurchase "2" 9
  , .grantPermission "2" 9
  , .closePurchase "2" 9 ]

/-- A denial refunding both an accepted and a pending pledge. The refused
withdrawal carries an identity with no accepted inversion, so the corpus also
exercises an `UNPROVED` claim row. -/
private def seedDenyPermissionRefunds : List Event :=
  [ .deposit "1" "2" 100
  , .deposit "1" "3" 80
  , .openPurchase "1" 4
  , .pledge "1" "2" 4 60
  , .acceptPledge "1" "2" 4
  , .pledge "1" "3" 4 30
  , .withdraw "1" "2" 999
  , .denyPermission "1" 4 ]

/-- The five mandated executions, all from the empty payload under
`seedView`. -/
def seedAuth : BackdonateAuth := fun _ _ => false

def seedCorpus : List Trace :=
  [ emitTrace seedView State.empty seedDonationPrefix seedAuth
  , emitTrace seedView State.empty seedCorrectPledgeDown seedAuth
  , emitTrace seedView State.empty seedCorrectPledgeUp seedAuth
  , emitTrace seedView State.empty seedClosePurchaseNegative seedAuth
  , emitTrace seedView State.empty seedDenyPermissionRefunds seedAuth ]
